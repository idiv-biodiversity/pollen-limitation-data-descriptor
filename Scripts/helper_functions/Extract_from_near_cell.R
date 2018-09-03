# /////////////////////////////////////////////////////////////////////////
## Function to extract the closest non-NA cell values from a raster at given
## pairs of longitude-latitude.
## Specifically, for all points that the usual raster::extract() returns NA, it
## searches for the closest non-NA cell value within given buffer (meters). All
## other points returned by the usual raster::extract() are kept as such. It
## uses RANN::nn2 to fast compute neighbors based on Euclidean distances. The
## fact that Euclidean and not great circle distances
## (https://en.wikipedia.org/wiki/Great-circle_distance) are used does not alter
## the nearest neighbor search (check
## https://scicomp.stackexchange.com/questions/7622/fast-nearest-neighbor-search-latitude-longitude?newreg=0daf766b1e5446bcaed39aee02153f26
## for an interesting discusion).
## ___ Arguments
## rst        - raster object
## XY         - data.table with two columns: first column=longitude; second
##              column=latitude
## my.buffer  - same as in raster::extract(); "The radius of a buffer around 
##              each point from which to extract cell values. [...] If the data 
##              are not projected (latitude/longitude), the unit should be meters. 
##              Otherwise it should be in map-units (typically also meters)".
##              buffer needs to be at least the raster's resolution (one cell)
## simplified - Logical argument. TRUE means that the result is a data.table 
##              with only one column - the extracted raster values.
##              If FALSE, then 5 extra columns are added: 
##               the coordinates (longitude-latitue - two columns), 
##               the ID of the raster cells where the points fall, 
##               the raster cell values and 
##               a column that indicates for each point if a buffer was used 
##               (NA means NO, 1 means YES)
## ___ Returns
## data.table object (check "simplified" argument above)
# /////////////////////////////////////////////////////////////////////////

extract_FromNearCellRst <- function(rst, 
                                    XY, 
                                    my.buffer, 
                                    simplified = TRUE, 
                                    lib.check  = TRUE){
    start.time <- Sys.time() # will inform about the CPU time
    
    # Check the input format --------------------------------------------------
    
    # Check if packages exist
    # .........................................................................
    if (isTRUE(lib.check)){
        if (!requireNamespace("RANN", quietly = TRUE)) 
            stop("Please install package {RANN}")
        if (!requireNamespace("data.table", quietly = TRUE)) 
            stop("Please install package {data.table}")
        if (!requireNamespace("raster", quietly = TRUE)) 
            stop("Please install package {raster}")
    }
    
    # Inform about the CRS of the raster
    # .........................................................................
    message("Please always check if the raster and the points correspond to the same CRS \n",
            "The CRS of the given raster is: \n", rst@crs, "\n")
    
    # Some defensive testing of the input
    # .........................................................................
    
    # Test if XY is a two column data.table
    if (!is.data.table(XY) | dim(XY)[2] != 2)
        stop("Expecting XY to be a a) data.table, b) with 2 columns (long-lat). Please provide a 2-columns data.table object")
    
    # Check if the data.table’s columns are numeric
    # The user should take care of the numeric conversion because in this way 
    # it can discover possible unwanted values.
    is.col.num <- sapply(XY, is.numeric)
    if ( ! all(is.col.num) )
        stop("In XY, column(s): \n",
             paste(colnames(XY)[!is.col.num], collapse = ", "), 
             "\n not numeric! Please convert them to numeric.")
    
    # Check if the buffer is numeric
    if (!is.numeric(my.buffer))
        stop("Expecting numeric buffer (in meters)")
    
    # Extract -----------------------------------------------------------------
    
    # Extract cell value & ID at given XY point
    ext <- data.table(raster::extract(x = rst, 
                                      y = XY, 
                                      cellnumbers = TRUE, 
                                      method = 'simple'))
    
    # If there are NA cell values, then search for the nearest non-Na cell value
    # within buffer's range.
    NA.idx <- is.na(ext[[2]])
    if ( all(NA.idx) ) stop("All points returned NA! \n - most probably the given raster and points have different CRS")
    if ( any(NA.idx) ) {
        packageStartupMessage(sum(NA.idx), 
                              " points outside raster coverage encountered \n - applying buffer extraction ...")
        # Get records where the extraction above returned NA-s.
        ext.NA <- ext[NA.idx,]
        # Get corresponding point coordinates from XY.
        XY.NA <- XY[NA.idx,]
        
        # Extract now a buffer of cells around these "NA" points. Will get a
        # list of matrices with raster cell values and cell IDs around each "NA"
        # point for some points can also return a single logical NA.
        # NOTE: buffer extraction (line below) is the most time consuming
        # operation in all this process.
        ext.bf <- raster::extract(x = rst, 
                                  y = XY.NA, 
                                  cellnumbers = TRUE, 
                                  buffer = my.buffer, 
                                  method = 'simple')
        
        # Some of these neighboring cells might have as well NA values,
        # therefore, get the coordinates of the centers of the raster cells only
        # for non-NA cells in each buffer (matrix).
        ext.bf.noNA.XY <- 
            lapply( 
                ext.bf, 
                function(m) {
                    # If m is a single logical NA then consider it as empty matrix
                    if (is.null(dim(m))) m <- matrix(data = NA, 
                                                     nrow = 1, 
                                                     ncol = 2)
                    # For each buffer matrix in the list of matrices ext.bf
                    # exclude all rows with NA cell values (in 2nd column)
                    # The drop = FALSE option is used defensively, see more at: 
                    # https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-do-my-matrices-lose-dimensions_003f 
                    m  <- m[!is.na(m[,2]),, drop = FALSE]
                    # Get coordinates of the center of each raster cell
                    xy <- raster::xyFromCell(object = rst, cell = m[,1])
                    # Bind the cleaned matrix "m" with the matrix of coordinates "xy"
                    cbind(xy,m)} 
            )
        
        # Knowing coordinates of the center of non-Na buffer cells and knowing
        # coordinates of each original point, then find out the nearest non-NA
        # buffer cell.
        
        # Initiate a vector that will contain nearest neighbor values.
        # NOTE: initiate with NA (this is safe in case there is absolutely NO
        # nearest valid neighbor cell within given buffer).
        neighbors <- rep(NA, times = length(ext.bf.noNA.XY))
        
        # For each buffer (matrix):
        for (i in 1:length(ext.bf.noNA.XY)){
            my.buffer <- ext.bf.noNA.XY[[i]]
            # Avoid cases when some buffers are empty (check if buffer matrix
            # has no rows) (it happens when buffer radius is not big enough and
            # can't find any non-NA cell value).
            if (dim(my.buffer)[1] != 0) {
                # Use function RANN::nn2() to find nearest neighbor (and
                # corresponding distance). The drop = FALSE option is used
                # defensively, see more at:
                # https://cran.r-project.org/doc/FAQ/R-FAQ.html#Why-do-my-matrices-lose-dimensions_003f 
                NN <- RANN::nn2(data  = my.buffer[,1:2, drop = FALSE],
                                query = XY.NA[i,], 
                                k     = 1)
                # Get value of nearest neighbor cell
                neighbors[i] <- my.buffer[NN$nn.idx,4]
            }
        }
        
        # Replace NA cell values from the first extraction with nearest neighbor
        # cell values. If all neighbors are still NA, then no need to overwrite
        # with NA (as NA is already in place in the 2nd column of ext)
        if (!all(is.na(neighbors))) ext[NA.idx, 2 := neighbors]
        
        packageStartupMessage("...buffer extraction completed!")
    } else message("All points were inside raster coverage - extracting as usual, NO buffer extraction needed")
    # end of big IF
    
    # Return only extracted raster values or, optionally, add some extra columns
    if ( simplified ) {
        message("\n Elapsed/CPU time: ", 
                format(round(Sys.time() - start.time, digits = 2)), "\n\n")
        return(ext[,2])
    } else {
        # Adds column that indicates for each point if a buffer was used. NA
        # means NO, 1 means YES
        ext[NA.idx, buf := 1L]
        message("\n Elapsed/CPU time: ", 
                format(round(Sys.time() - start.time, digits = 2)), "\n\n")
        return(cbind(XY, ext))
    }
}
