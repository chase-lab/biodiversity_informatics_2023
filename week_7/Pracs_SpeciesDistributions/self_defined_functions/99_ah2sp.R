#Modified from a function written by Andrew Bevan and updated by Pascal Title (found in the github ptitle/ rangeBuilder)
# x: alpha hull object generated with ahull();
# isol.points.buff: logial; whether generated buffers for isolated points that are outside of convex hull
# buff: the size of the buffer for isolated points in kilometers. A cylindrical equal area projection is used for buffering.
# library(alphahull)
# library(rgeos)

ah2sp <- function(x, increment = 360, rnd = 10, isol.points.buff = TRUE, buff = 10, tol = 1e-4, proj4string = CRS(as.character(NA))){
  if (class(x) != "ahull"){
    stop("x needs to be an ahull class object")
  }
  
  # projection
  if(is.na(proj4string) & isol.points.buff) proj4string <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  # Extract the edges from the ahull object as a dataframe
  xdf <- as.data.frame(x$arcs)
  
	#correct for possible arc order strangeness (Pascal Title addition 29 Nov 2013)
	k <- 1
	xdf <- cbind(xdf, flip = rep(FALSE, nrow(xdf)))
	repeat{
		if (is.na(xdf[k+1, 'end1'])) {
			break
		}
    #cat(k, '\n')
		if (xdf[k,'end2'] == xdf[k+1,'end1']) {
      #cat('case 1\n')
			k <- k + 1
		} else if (xdf[k,'end2'] != xdf[k+1,'end1'] & !xdf[k,'end2'] %in% xdf$end1[k+1:nrow(xdf)] & !xdf[k,'end2'] %in% xdf$end2[k+1:nrow(xdf)]) {
			#cat('case 2\n')
      k <- k + 1
		} else if (xdf[k,'end2'] != xdf[k+1,'end1'] & xdf[k,'end2'] %in% xdf$end1[k+1:nrow(xdf)] & !xdf[k,'end2'] %in% xdf$end2[k+1:nrow(xdf)]) {
			#cat('case 3\n')
      m <- which(xdf$end1[k+1:nrow(xdf)] == xdf[k,'end2']) + k
			xdf <- rbind(xdf[1:k,],xdf[m,],xdf[setdiff((k+1):nrow(xdf),m),])
		} else if (xdf[k,'end2'] != xdf[k+1,'end1'] & !xdf[k,'end2'] %in% xdf$end1[k+1:nrow(xdf)] & xdf[k,'end2'] %in% xdf$end2[k+1:nrow(xdf)]) {
			#cat('case 4\n')
      m <- which(xdf$end2[k+1:nrow(xdf)] == xdf[k,'end2']) + k
			tmp1 <- xdf[m,'end1']
			tmp2 <- xdf[m,'end2']
			xdf[m,'end1'] <- tmp2
			xdf[m,'end2'] <- tmp1
			xdf[m,'flip'] <- TRUE
			xdf <- rbind(xdf[1:k,], xdf[m,], xdf[setdiff((k+1):nrow(xdf), m),])
		} else {
      k <- k + 1
    }
	}

	#For isolated points, create buffed area (Added by Wubing Xu)
	ppolys <- NULL
	if(isol.points.buff){
		xdfp <- subset(xdf,xdf$r == 0)
		if(nrow(xdfp) > 0){
			spps <- SpatialPoints(xdfp[,1:2],proj4string = proj4string)
			spps <- sp::spTransform(spps, CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=km +no_defs"))
			spps.buffer <- rgeos::gBuffer(spps,width=buff)
			# to assure the buffered areas are within the geographical coordinate range
			bound = SpatialPolygons(list(Polygons(list(Polygon(cbind(c(-180,180,180,-180),c(89.99,89.99,-89.99,-89.99)))),"p1")), 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
			bound <- sp::spTransform(bound,  CRS("+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=km +no_defs"))
			spps.buffer <- gIntersection(spps.buffer, bound, byid=FALSE)
			# transform to the raw projection
			spps.buffer <- sp::spTransform(spps.buffer, proj4string)
			ppolys <- slot(slot(spps.buffer,"polygons")[[1]],"Polygons")
		}
	}
	
	#The cases of arcs
	xdf <- subset(xdf,xdf$r > 0)
	lpolys <- NULL	
	res <- NULL

  if (nrow(xdf) > 0) {
    # Convert each arc to a line segment
    linesj <- list()
    prevx <- NULL
    prevy <- NULL
    j <- 1
    for(i in 1:nrow(xdf)) {
      rowi <- xdf[i,]
      v <- c(rowi$v.x, rowi$v.y)
      theta <- rowi$theta
      r <- rowi$r
      cc <- c(rowi$c1, rowi$c2)
      # Arcs need to be redefined as strings of points. Work out the number of points to allocate in this arc segment.
      ipoints <- 2 + round(increment * (rowi$theta / pi), 0)
      # Calculate coordinates from arc() description for ipoints along the arc.
      angles <- alphahull::anglesArc(v, theta)
      if (rowi['flip'] == TRUE){ angles <- rev(angles) }
      seqang <- seq(angles[1], angles[2], length = ipoints)
      x <- round(cc[1] + r * cos(seqang),rnd)
      y <- round(cc[2] + r * sin(seqang),rnd)
      # Check for line segments that should be joined up and combine their coordinates
      if (is.null(prevx)) {
        prevx <- x
        prevy <- y
      # added numerical precision fix (Pascal Title Dec 9 2013)
      } else if ((x[1] == round(prevx[length(prevx)],rnd) | abs(x[1] - prevx[length(prevx)]) < tol)  #remove or change tolerance? 
				&& (y[1] == round(prevy[length(prevy)],rnd) | abs(y[1] - prevy[length(prevy)]) < tol)) {
          if (i == nrow(xdf)){
          #We have got to the end of the dataset
            prevx <- append(prevx ,x[2:ipoints])
            prevy <- append(prevy, y[2:ipoints])
            prevx[length(prevx)] <- prevx[1]
            prevy[length(prevy)] <- prevy[1]
            coordsj <- cbind(prevx,prevy)
            colnames(coordsj) <- NULL
            # Build as Line and then Lines class
            linej <- Line(coordsj)
            linesj[[j]] <- Lines(linej, ID = as.character(j))
          } else {
            prevx <- append(prevx, x[2:ipoints])
            prevy <- append(prevy, y[2:ipoints])
          }
        } else {
      # We have got to the end of a set of lines, and there are several such sets, so convert the whole of this one to a line segment and reset.
          prevx[length(prevx)] <- prevx[1]
          prevy[length(prevy)] <- prevy[1]
          coordsj <- cbind(prevx,prevy)
          colnames(coordsj)<-NULL
      # Build as Line and then Lines class
          linej <- Line(coordsj)
          linesj[[j]] <- Lines(linej, ID = as.character(j))
          j <- j + 1
          prevx <- NULL
          prevy <- NULL
        }
    }
    
    #Drop lines that will not produce adequate polygons (Pascal Title addition 9 Dec 2013)
    badLines <- vector()
   for (i in 1:length(linesj)){
    	if (nrow(linesj[[i]]@Lines[[1]]@coords) < 4){
    		badLines <- c(badLines,i)
		}
    }
    if (length(badLines) > 0){linesj <- linesj[-badLines]}
    
    # Promote to SpatialLines
    lspl <- SpatialLines(linesj)
    # Convert lines to polygons
    # Pull out Lines slot and check which lines have start and end points that are the same
    lns <- slot(lspl, "lines")
    polys <- sapply(lns, function(x) { 
      crds <- slot(slot(x, "Lines")[[1]], "coords")
      identical(crds[1, ], crds[nrow(crds), ])
    }) 
    # Select those that do and convert to SpatialPolygons
    spls <- lspl[polys]
    list_of_Lines <- slot(spls, "lines")
    lpolys <- lapply(list_of_Lines, function(x) {Polygon(slot(slot(x, "Lines")[[1]], "coords")) }) #Modifed by Wubing Xu
	}
  
	#Generate spatial polygons (Modifed by Wubing Xu)
    if (!is.null(lpolys) | !is.null(ppolys)){ 
	    res <- SpatialPolygons(list(Polygons(append(lpolys,ppolys),"ID"=1)),proj4string=proj4string)
		# Create a dataframe, then promote to SpatialPolygonsDataFrame 
		id <- sapply(slot(res, "polygons"), function(x) slot(x, "ID")) 
		areas <- sapply(slot(res, "polygons"), function(x) slot(x, "area")) 
		df <- data.frame("ID"=id,"Area"=areas)
		res <- SpatialPolygonsDataFrame(res, data=df)
		res <- res[which(res@data$Area > 0),] 
	} 
  return(res)
}