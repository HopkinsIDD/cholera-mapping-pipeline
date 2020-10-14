#' @export
#' @name extract_country_from_raster
#' @title extract_country_from_raster
#' @description Extract a Raster* data only for locations in a shapefile
#' @param raster_layer The Raster* to extract from.
#' @param shapefile The shapefile to use for bounding
#' @return A Raster* of the same type as \code{raster_layer}
#' @param partial_cover whether or not to do partial matching
#' @param method Whether to use the raster method or the previous hard coded one
#' @param tol The distance in (degrees lat/long) rasters are permitted to shift
#' @param trim_small Remove values with less than this fraction of the median overlap
extract_country_from_raster <- function(raster_layer,shapefile,partial_cover = FALSE,method='raster',trim_small=0,tol = 1e-5){
  if("sf" %in% class(shapefile)){
    shapefile = shapefile$geometry
  }
  if("sfc" %in% class(shapefile)){
    #' @importFrom sf as_Spatial
    shapefile = as_Spatial(shapefile)
  }
  #' @importFrom raster extent
  lhs = extent(raster_layer)
  #' @importFrom raster extent
  ## This should maybe be imported from sf somehow?
  #' @importFrom raster extent
  rhs = extent(shapefile)
  if(!compare_extents(shapefile,raster_layer)){
    ## Check and make sure the extents have proper containment
    extents_ok = sign(rhs@xmin - lhs@xmin + tol) +
      sign(lhs@xmax - rhs@xmax + tol) +
      sign(rhs@ymin - lhs@ymin + tol) +
      sign(lhs@ymax - rhs@ymax + tol)
    if(extents_ok < 4){
      stop('Cannot crop raster to this shapefile.  The extent of the shapefile is not contained in the extent of the raster')
    }
    #' @importFrom raster crop
    raster_layer <- crop(
      raster_layer,
      #' @importFrom raster extent
      extent(shapefile),
      snap='out'
    )
  }
  if(partial_cover){
    if(method == 'raster'){
      if(trim_small <= 0.01){
        stop("For method raster, trim_small must be at least .01")
      }
      #' @importFrom raster rasterize
      tmp_raster <- rasterize(shapefile,raster_layer,getCover=partial_cover)/100
      tmp_raster[tmp_raster == 0] <- NA
      #' @importFrom raster values values<-
      values(tmp_raster)[values(tmp_raster) < quantile(values(tmp_raster),.5,na.rm=T)*trim_small] <- NA
      return(tmp_raster * raster_layer)
    } else {
      if(trim_small != 0){
        stop("Not written")
      }
      na_only = FALSE
      #' @importFrom raster values values<-
      if(all(is.na(unique(values(raster_layer))))){
        na_only = TRUE
        values(raster_layer) <- 0
      }
      #' @importFrom sf st_union
      #' @importFrom sf st_as_sf
      shapefile = st_union(st_as_sf(shapefile))
      #' @importFrom raster raster
      #' @importFrom sf st_as_sf
      all_polys = st_as_sf(rasterToPolygons(raster_layer,dissolve = FALSE))
      #' @importFrom sf st_crs
      st_crs(all_polys) <- st_crs(shapefile)
      #' @importFrom sf st_intersects
      intersects = sapply(st_intersects(all_polys,shapefile),length) > 0
      if(na_only){
        intersects = is.na(intersects)
      }
      #' @importFrom sf st_intersection
      cropped_polys = st_intersection(all_polys[intersects,],shapefile)
      #' @importFrom sf st_area
      adjustment_factors = st_area(cropped_polys)/st_area(all_polys[intersects,])
      if(class(raster_layer) == "RasterBrick"){
        indices_1 <- which(apply(raster_layer@data@values,1,function(x){any(!is.na(x))}))
        indices_2 = indices_1[intersects]
        indices_3 = indices_1[!(indices_1 %in% indices_2)]
        raster_layer@data@values[indices_2,] = raster_layer@data@values[indices_2,] * adjustment_factors
        raster_layer@data@values[indices_3,] = NA
      } else if(class(raster_layer) == "RasterStack"){
        #' @importFrom raster values
        indices_1 <- which(apply(values(raster_layer),1,function(x){any(!is.na(x))}))
        indices_2 = indices_1[intersects]
        indices_3 = indices_1[!(indices_1 %in% indices_2)]
        #' @importFrom raster values values<-
        values(raster_layer)[indices_2,] = values(raster_layer)[indices_2,] * adjustment_factors
        #' @importFrom raster values  values<-
        values(raster_layer)[indices_3,] = NA
      } else {
        indices_1 <- which(!is.na(raster_layer@data@values))
        indices_2 = indices_1[intersects]
        indices_3 = indices_1[!(indices_1 %in% indices_2)]
        raster_layer@data@values[indices_2] = raster_layer@data@values[indices_2] * adjustment_factors
        raster_layer@data@values[indices_3] = NA
      }
    }
  } else {
    #' @importFrom raster mask
    raster_layer <- mask(
      raster_layer,
      shapefile
    )
  }
  
  return(raster_layer)
  # #' @importFrom raster extract
  # extract(raster_layer,shapefiles)
}

#' @export
#' @name aggregate_raster_xlayers
#' @title aggregate_raster_xlayers
#' @description A mask of stackApply that always assumes indices is all 1
#' @param x Raster* object
#' @param fun function that returns a single value, e.g. mean or min, and that takes a na.rm argument (or can pass through arguments via ...)
#' @param ... additional arguments as for writeRaster
#' @return A RasterLayer

aggregate_raster_xlayers <- function(x,fun,...){
  #' @importFrom raster getValues
  if(is.null(dim(getValues(x)))){
    return(x)
  }
  rc <- raster(x)
  #' @importFrom raster setValues
  #' @importFrom raster getValues
  setValues(rc,apply(getValues(x),1,fun,...))
}

#' @export
#' @name aggregate_raster_xcells
#' @title aggregate_raster_xcells
#' @description Apply a function to each layer, aggregating the cells of the raster
#' @param x Raster* object
#' @param fun function that returns a single value, e.g. mean or min, and that takes a na.rm argument (or can pass through arguments via ...)
#' @param ... additional arguments as for writeRaster
#' @return A vector
aggregate_raster_xcells <- function(x,fun,...){
  if(is.null(dim(getValues(x)))){
    #' @importFrom raster getValues
    return(fun(getValues(x),...))
  }
  #' @importFrom raster getValues
  apply(getValues(x),2,fun,...)
}

#' @export
#' @name expand_extent
#' @title expand_extent
#' @description Make an extent larger from the center
#' @param extent An raster::extent The extent to expand.  Should be the result of raster::expand
#' @param expansion A numeric describing how much bigger to make the extent as a numeric.
expand_extent <- function(extent,expansion=2){
  xrange <- extent@xmax - extent@xmin
  yrange <- extent@ymax - extent@ymin
  xmed <- (extent@xmax + extent@xmin)/2
  ymed <- (extent@ymax + extent@ymin)/2
  extent@xmin <- xmed - (xrange*expansion)/2
  extent@xmax <- xmed + (xrange*expansion)/2
  extent@ymin <- ymed - (yrange*expansion)/2
  extent@ymax <- ymed + (yrange*expansion)/2
  return(extent)
}

#' @export
#' @name apply_to_all_sublevels
#' @title apply_to_all_sublevels
#' @description Apply a function to a raster with different answers for each subregion of a country at a particular ISO level
#' @param location_name The name of the country
#' @param raster The raster to apply the function to
#' @param ISO_level The level to get the shapefiles at
#' @param taxonomy_dir The taxonomy directory to store the shapefiles in
#' @param verbose Set to true for more warnings and messages
#' @param partial_cover Set to true to get accurate partial matches by subsampling the raster grid.
apply_to_all_sublevels <- function(location_name,
                                   raster_layer,
                                   ISO_level,
                                   fun,
                                   taxonomy_dir = 'taxonomy-verified',
                                   verbose=FALSE,
                                   fix_location_names = FALSE,
                                   partial_cover = FALSE,
                                   trim_small = 0,
                                   method = 'not raster',
                                   ...){
  
  all_shapefiles <- get_country_sublevels(location_name,ISO_level,taxonomy_dir,verbose)
  rc <- apply(
    all_shapefiles,
    1,
    function(shp){
      shp = st_sfc(shp$geometry)
      extracted_raster = extract_country_from_raster(
        raster_layer,
        shp,
        partial_cover = partial_cover,
        trim_small = trim_small,
        method = method
      )
      aggregate_raster_xcells(extracted_raster,fun,...)
    }
  )
  
  rc_names <- apply(
      as.data.frame(c(list(all_shapefiles[['who_region']]),list(all_shapefiles[['ISO_A1']]),lapply(
        1:ISO_level,
        function(level){
          all_shapefiles[[paste("ISO_A2_L",level,sep="")]]
        }
      ))),
      1,
      paste,
      collapse='_')
  if(fix_location_names){
    rc_names = fix_location_name(rc_names,taxonomy_dir)
  }
  # rc_names <- all_shapefiles[[paste("NAME",1:ISO_level,sep='_')]]
  if(is.null(dim(rc))){
    names(rc) <- rc_names
  } else {
    colnames(rc) <- rc_names
  }
  return(rc)
}


#' @export
#' @name regrid_raster
#' @title regrid_raster
#' @description using linear combinations, put one raster on the grid of another raster
#' @param x The Raster* object to regrid
#' @param y The Raster* object to obtain the extent,resolution etc from
#' @return A Raster* of the same type and layers as x with the same extent/resolution/etc as y
regrid_raster <- function(x,y,aggregate = TRUE, .fun = sum){
  ## Delete raster names for x and y so that default names (layer.1, etc) are used
  names(x) <- rep("", length(names(x)))
  names(y) <- rep("", length(names(y)))
  ## Crop x to the right size for speed reasons.
  ##   To ensure no data is lost, we expand the extent
  ##   of y by one cell, and use the snap='out' argument
  ##   go back and make sure we actually need the expand extent
  ##   i think I may have been using type='out' instead of snap
  ##   when i tested this
  #' @importFrom raster crop
  #' @importFrom raster extent
  x <- crop(x,extent(y),snap='out')
  # y_original <- y
  # #' @importFrom raster crop
  # y <- crop(y,extent(x),snap='out')
  if(aggregate){
    resol = min((dim(x)/dim(y))[1:2])
    if(resol > 2){
      resol = floor(resol)
      x = aggregate(x,resol,fun=.fun)
    } else if(resol < .5){
      resol = 1/resol
      resol = floor(resol)
      #' @importFrom raster aggregate
      y = aggregate(y,resol,fun=.fun)
    }
  }
  ## If the extents overlap, but the raster is all NA, the shape file creation will fail
  if(all(is.na(x[]))){
    ## This is a pretty hackish way to do this.  We should be able to skip the rest of the code.
    x[] <- 0
  }
  if(all(is.na(y[]))){
    ## This is a pretty hackish way to do this.  We should be able to skip the rest of the code.
    stop("y is all na")
  }
  # #' @importFrom raster crop
  # x <- crop(
  #   x,
  #   expand_extent(
  #     #' @importFrom raster extent
  #     extent(y),
  #     #' @importFrom raster extent
  #     1 + (extent(y)@ymax - extent(y)@ymin)/ncol(y)
  #   ),
  #   snap='out'
  # )
  ## Could possibly be sped up by also cropping y
  ## and expanding the extent at the end.
  ## Turn everything into polygons because the raster package has betrayed me
  #' @importFrom sf st_as_sf
  #' @importFrom raster rasterToPolygons
  xPoly <- st_as_sf(rasterToPolygons(x))
  #' @importFrom sf st_as_sf
  #' @importFrom raster rasterToPolygons
  yPoly <- st_as_sf(rasterToPolygons(y))
  
  ## Keep all of the variables around including area
  ## Probably not necessary to calculate area_x and idx_x
  xPoly$idx_x <- 1:nrow(xPoly)
  yPoly$idx_y <- 1:nrow(yPoly)
  #' @importFrom sf st_area
  xPoly$area_x <- st_area(xPoly)
  yPoly$area_y <- st_area(yPoly)
  
  ## copy all of layers over
  for(colname in names(xPoly)[startsWith(x=names(xPoly),prefix='layer')]){
    xPoly[[paste(colname,"x",sep='_')]] <- xPoly[[colname]]
  }
  
  ## again, probably not necessary to copy over the layers_y
  for(colname in names(yPoly)[startsWith(x=names(yPoly),prefix='layer')]){
    yPoly[[paste(colname,"y",sep='_')]] <- yPoly[[colname]]
  }
  
  #' @importFrom sf st_intersection
  ## This performs the pairwise intersection, so we'll get all overlaps of anything with anything else
  iPoly <- st_intersection(xPoly,yPoly)
  if(nrow(iPoly) == 0){
    rc <- raster(y)
    rc[] <- 0
    tmp <- raster(y)
    tmp[] <- 0
    if(nlayers(x) > 1){
      for(i in 2:nlayers(x)){
        #' @importFrom raster addLayer
        rc <- addLayer(rc,tmp)
      }
    }
    return(rc)
  }
  
  #' @importFrom sf st_area
  ## we need the new area here as well for normalizing
  iPoly$area <- st_area(iPoly)
  
  ## This is for dplyr magic to get the new layers
  layer_names <- names(iPoly)[startsWith(names(iPoly),'layer')]
  layer_names <- layer_names[endsWith(layer_names,'_x')]
  
  
  layer_names_nox <- substr(layer_names,1,nchar(layer_names)-2)
  #' @importFrom dplyr summarize_
  ## This is the dplyr magic
  ## The objective is to get one column for each layer in x
  ##   where we sum over all cells with the same idx_y
  #' @importFrom dplyr summarize_
  summarize_(
    #' @importFrom dplyr group_by
    group_by(
      iPoly,
      idx_y),
    .dots = setNames(
      paste("sum(",layer_names, " * area / area_x)"),
      layer_names_nox
    )
  ) -> rcval
  ##   Turn back into a raster.  Need different methods for the different
  ##   types of raster inputs.  Could combine brick and stack
  non_na_idx <- integer(0)
  if(raster::nlayers(y) > 1){
    non_na_idx = which(apply(y[],1,function(x){any(!is.na(x))}))
  } else {
    non_na_idx = which(!is.na(y[]))
  }
  if(class(x) == 'RasterLayer'){
    #' @importFrom raster raster
    rc <- raster(y)
    rc[] <- as.numeric(rcval$layer)
    return(rc)
  }
  if(class(x) == 'RasterStack'){
    #' @importFrom raster raster
    tmp <- raster(y)
    tmp[] <- 0
    rc <- tmp
    if(nlayers(x) > 1){
      for(i in 2:nlayers(x)){
        #' @importFrom raster addLayer
        rc <- addLayer(rc,tmp)
      }
    }
    # non_na_idx = which(apply(y[],1,function(x){any(!is.na(x))}))
    if(nlayers(x) > 0){
      for(idx in 1:nlayers(x)){
        rc[][non_na_idx,idx] = rcval[[layer_names_nox[idx] ]]
      }
    }
    names(rc) <- layer_names_nox
    return(rc)
  }
  if(class(x) == 'RasterBrick'){
    #' @importFrom raster raster
    tmp <- raster(y)
    tmp[] <- 0
    rc <- tmp
    if(nlayers(x) > 1){
      for(i in 2:nlayers(x)){
        #' @importFrom raster addLayer
        rc <- addLayer(rc,tmp)
      }
    }
    # non_na_idx = which(apply(y[],1,function(x){any(!is.na(x))}))
    if(nlayers(x) > 0){
      for(idx in 1:nlayers(x)){
        rc[][non_na_idx,idx] = rcval[[layer_names_nox[idx] ]]
      }
    }
    names(rc) <- layer_names_nox
    return(rc)
  }
  ## Each class has its own return, so if we get here then this is bad.
  stop("Cannot regrid an object of class", paste(class(x)))
}

#' @export
#' @name create_raster_from_shapefile
#' @title create_raster_from_shapefile
#' @description Create a RasterLayer that covers the same region as a shapefile
#' @param shapefile Either a location name, country name, or shapefile to use for the extent of the raster
#' @param region A worldpop region containing the shapefile (or NULL to lookup automatically)
#' @param resol Approximate resolution of the output raster in km
#' @param trim trim raster to shapefile (not just the extent)
#' @param partial_cover only used if trim==TRUE, passed to extract_country_from_raster.
#' @param trim_small only used if trim==TRUE, passed to extract_country_from_raster.
#' @param method only used if trim==TRUE, passed to extract_country_from_raster.
#' @return A RasterLayer
create_raster_from_shapefile <- function(shapefile,region=NULL,layers_dir = "Layers",resol=10,trim=TRUE,partial_cover=FALSE,trim_small=0, method = 'not raster'){
  if(is.null(region)){
    region = lookup_WorldPop_region(shapefile)
  }
  if("sf" %in% class(shapefile)){
    shapefile = shapefile$geometry
  }
  if("sfc" %in% class(shapefile)){
    #' @importFrom sf as_Spatial
    shapefile = as_Spatial(shapefile)
  }
  directory = paste(layers_dir,"pop",region,sep='/')
  files = list.files(directory)
  files = files[grepl('tif$',files)]
  files = files[grepl('adj',files)]
  file = files[1]
  #' @importFrom raster raster
  rl = raster(paste(layers_dir,"pop",region,file,sep='/'))
  #' @importFrom raster extent
  shp_extent = extent(shapefile)
  shp_extent@xmin = shp_extent@xmin - res(rl)[1]*resol[1]
  shp_extent@xmax = shp_extent@xmax + res(rl)[1]*resol[1]
  shp_extent@ymin = shp_extent@ymin - res(rl)[2]*resol[1]
  shp_extent@ymax = shp_extent@ymax + res(rl)[2]*resol[1]
  #' @importFrom raster crop
  rl = crop(rl,shp_extent,snap='out')
  #' @importFrom raster aggregate
  rl = aggregate(rl,resol)
  rl[!is.na(rl)] <- 1
  if(trim){
    rl[] <- 1
    rl = extract_country_from_raster(rl,shapefile,partial_cover=partial_cover,trim_small=trim_small,method=method)
  }
  return(rl)
}


compare_extents <- function(shapefile,raster){
  #' @importFrom raster extent
  lhs = extent(shapefile)
  #' @importFrom raster extent
  rhs = extent(raster)
  #' @importFrom raster res
  resol = res(raster)
  return(
    (abs(lhs@xmin - rhs@xmin) < resol[1]) &
      (abs(lhs@xmax - rhs@xmax) < resol[1]) &
      (abs(lhs@ymin - rhs@ymin) < resol[2]) &
      (abs(lhs@ymax - rhs@ymax) < resol[2])
  )
}

#' @export
#' @name extract_nearest_neighbor_matrix
#' @title extract_nearest_neighbor_matrix
#' @description Get the nearest neighbor matrix of the chosen cells in the raster
#' @param raster_layer The Raster* object to get the cells from
#' @param cell_mask The particular cells to extract
#' @param sparse Whether to return a sparse matrix or a dense one.  See Matrix for details
#' @return a Matrix object with the nearest neigbhors as 1, and everything else as 0.
extract_nearest_neighbor_matrix <- function(raster_layer,cell_mask,sparse=T){
  raster_dim = dim(raster_layer)[1:2]
  cell_mask_row = floor((cell_mask-1)/ncol(raster_layer)) + 1
  cell_mask_col = ((cell_mask - 1) %% ncol(raster_layer)) + 1
  nearest_neighbors = dplyr::data_frame(
    id = 1:length(cell_mask),
    row = cell_mask_row,
    col = cell_mask_col
  )
  
  nearest_neighbors <- dplyr::ungroup(
    dplyr::mutate(
      dplyr::group_by(
        dplyr::ungroup(
          dplyr::mutate(
            dplyr::group_by(
              nearest_neighbors,
              row
            ),
            n_col_l = sapply(col,function(x,y=col){y[which.min(abs(x - y) + nrow(nearest_neighbors)*(x>=y))]}),
            n_col_r = sapply(col,function(x,y=col){y[which.min(abs(x - y) + nrow(nearest_neighbors)*(x<=y))]}),
            d_col_l = n_col_l - col,
            d_col_r = col - n_col_r
          )
        ),
        col
      ),
      n_row_l = sapply(row,function(x,y=row){y[which.min(abs(x - y) + nrow(nearest_neighbors)*(x>=y))]}),
      n_row_r = sapply(row,function(x,y=row){y[which.min(abs(x - y) + nrow(nearest_neighbors)*(x<=y))]}),
      d_row_l = n_row_l - row,
      d_row_r = row - n_row_r
    )
  )
  nearest_neighbors$min_dist = mapply(
    cl = nearest_neighbors$d_col_l,
    rl = nearest_neighbors$d_row_l,
    cr = nearest_neighbors$d_col_r,
    rr = nearest_neighbors$d_row_r,
    function(rl,rr,cl,cr){
      tmp = c(rl,rr,cl,cr)
      tmp = tmp[tmp > 0]
      min(tmp)
    }
  )
  
  nearest_neighbors <- dplyr::mutate(
    nearest_neighbors,
    n_row_l = ifelse(d_row_l > 0, n_row_l,NA),
    n_row_r = ifelse(d_row_r > 0, n_row_r,NA),
    n_col_l = ifelse(d_col_l > 0, n_col_l,NA),
    n_col_r = ifelse(d_col_r > 0, n_col_r,NA),
    n_row_l = ifelse(d_row_l <= min_dist, n_row_l,NA),
    n_row_r = ifelse(d_row_r <= min_dist, n_row_r,NA),
    n_col_l = ifelse(d_col_l <= min_dist, n_col_l,NA),
    n_col_r = ifelse(d_col_r <= min_dist, n_col_r,NA)
  )
  
  nearest_neighbor_pairs <- dplyr::select(
    dplyr::bind_rows(
      dplyr::inner_join(nearest_neighbors,nearest_neighbors,by=c('row'='n_row_l','col'='col')),
      dplyr::inner_join(nearest_neighbors,nearest_neighbors,by=c('row'='n_row_r','col'='col')),
      dplyr::inner_join(nearest_neighbors,nearest_neighbors,by=c('row'='row','col'='n_col_l')),
      dplyr::inner_join(nearest_neighbors,nearest_neighbors,by=c('row'='row','col'='n_col_r'))
    ),
    id.x,
    id.y
  )
  ## Symmetrize the nearest neighbor matrix so we don't have sources and sinks...
  nearest_neighbor_pairs_t = data_frame(
    id.x = nearest_neighbor_pairs$id.y,
    id.y = nearest_neighbor_pairs$id.x
  )
  nearest_neighbor_pairs = unique(bind_rows(nearest_neighbor_pairs,nearest_neighbor_pairs_t))
  
  if(any(sort(unique(nearest_neighbor_pairs$id.x)) != 1:length(cell_mask))){
    stop("Investigate this")
  }
  if(any(sort(unique(nearest_neighbor_pairs$id.y)) != 1:length(cell_mask))){
    stop("Investigate this")
  }
  
  nearest_neighbor_matrix <- Matrix(0,nrow=nrow(nearest_neighbors),ncol=nrow(nearest_neighbors),sparse=T)
  for(neighbor_idx in 1:nrow(nearest_neighbor_pairs)){
    nearest_neighbor_matrix[nearest_neighbor_pairs$id.x[neighbor_idx],nearest_neighbor_pairs$id.y[neighbor_idx]] <- 1
  }
  return(nearest_neighbor_matrix)
}

#' @export
#' @name estimate_determinant
#' @title estimate_determinant
#' @description Estimate the determinant of a nearest neighbors matrix
#' @param rho_samples Number of times to sample rho
#' @param nn_mat nearest neighbor matrix (see extract_nearest_neighbors_matrix)
#' @return vector of estimates for the determinant of nn_mat
estimate_determinant = function(rho_samples,nn_mat){
  
  # Create a matrix with diagonal apply(D_mat,1,sum)
  #' @importFrom Matrix Matrix
  D_mat=Matrix(0,nrow=nrow(nn_mat),ncol=ncol(nn_mat),sparse=T)
  diag(D_mat)=apply(nn_mat,1,sum)
  
  ldet_DpA<-rep(0,rho_samples+1)
  # calculate the off diagonal of D - p * A for each value of p
  for (i in 1:(rho_samples + 1)) {
    pi<-(i-1)/(rho_samples)
    # store the sampled log(determinant(D - p * A))
    dpnn<-D_mat-pi*nn_mat
    #eigs<-eigen(dpnn,only.values=T)$values
    #ldet_DpA[i] <- sum(log(eigs))
    #' @importFrom Matrix determinant
    ldet_DpA[i] <-determinant(dpnn,logarithm=T)$modulus[[1]]
    if(ldet_DpA[i]==-Inf&pi==1){
      pi=0.9999
      dpnn<-D_mat-pi*nn_mat
      #eigs<-eigen(dpnn,only.values=T)$values
      #ldet_DpA[i] <- sum(log(eigs))
    #' @importFrom Matrix determinant
      ldet_DpA[i] <-determinant(dpnn,logarithm=T)$modulus[[1]]
    }
    print(pi)
  }
  
  return(ldet_DpA)
}

#' @export
#' @name create_distance_to_shapefile_raster
#' @title create_distance_to_shapefile_raster
#' @description Create a RasterLayer that is 0 on a shapefile and the distance to the nearest cell intersecting the shapefile otherwise
#' @param shapefile The shapefile of features
#' @param region A worldpop region containing the shapefile (or NULL to lookup automatically)
#' @param resol Approximate resolution of the output raster in km
#' @return A RasterLayer
create_distance_to_shapefile_raster <- function(shapefile,region=NULL,layers_dir = "Layers",resol=10){
  if(is.null(region)){
    region = lookup_WorldPop_region(shapefile)
  }
  if("sf" %in% class(shapefile)){
    shapefile = shapefile$geometry
  }
  if("sfc" %in% class(shapefile)){
    #' @importFrom sf as_Spatial
    shapefile = as_Spatial(shapefile)
  }
  directory = paste(layers_dir,"pop",region,sep='/')
  files = list.files(directory)
  files = files[grepl('tif$',files)]
  files = files[grepl('adj',files)]
  file = files[1]
  #' @importFrom raster raster
  rl = raster(paste(layers_dir,"pop",region,file,sep='/'))
  #' @importFrom raster extent
  shp_extent = extent(shapefile)
  shp_extent@xmin = shp_extent@xmin - res(rl)[1]*resol[1]
  shp_extent@xmax = shp_extent@xmax + res(rl)[1]*resol[1]
  shp_extent@ymin = shp_extent@ymin - res(rl)[2]*resol[1]
  shp_extent@ymax = shp_extent@ymax + res(rl)[2]*resol[1]
  #' @importFrom raster crop
  rl = crop(rl,shp_extent,snap='out')
  #' @importFrom raster aggregate
  rl = aggregate(rl,resol)
  rl[!is.na(rl)] <- 1
  if(trim){
    rl[] <- 1
    rl = extract_country_from_raster(rl,shapefile,partial_cover=partial_cover,trim_small=trim_small,method=method)
  }
  return(rl)
  #' @importFrom raster stack
  rl <- stack(riv.r,lake.r,)
  return()
  ### Check for cached files
  if(file.exists(
    paste(layers_dir,'water','HTI_water_all.tif',sep='/')
  )){
    return(raster(
      paste(layers_dir,'water','HTI_water_all.tif',sep='/')
    ))
  }
  ### Load the full map of 2D and 1D bodies of water.
  water_area <- st_union(st_read(paste(layers_dir,"water","HTI_water_areas_dcw.shp",sep='/'),quiet=TRUE))
  water_line <- st_union(st_read(paste(layers_dir,"water","HTI_water_lines_dcw.shp",sep='/'),quiet=TRUE))
  
  ### Make aggregate rasters
  lake.a <- projectExtent(raster,st_crs(water_area)$proj4string)
  river.a <- projectExtent(raster,st_crs(water_line)$proj4string)
  
  ### To compute the mean over a cell, we subdivide.
  #' @importFrom raster disaggregate
  lake.r <- disaggregate(lake.a,covariate_factor)
  #' @importFrom raster values values<-
  values(lake.r) <- NA
  #' @importFrom raster disaggregate
  river.r <- disaggregate(river.a,covariate_factor)
  #' @importFrom raster values values<-
  values(river.r) <- NA
  
  ### Set the cells that contain that type of water to 0
  #' @importFrom raster values values<-
  values(lake.r)[
    #' @importFrom raster extract
    extract(
      lake.r,
      #' @importFrom sf as_Spatial
      as_Spatial(water_area),
      weight=TRUE,
      cellnumbers=TRUE
    )[[1]][,1]] = 0
  #' @importFrom raster values values<-
  values(river.r)[
    #' @importFrom raster extract
    extract(
      river.r,
      #' @importFrom sf as_Spatial
      as_Spatial(water_line),
      weight=TRUE,
      cellnumbers=TRUE
    )[[1]][,1]] = 0
  
  ### Fill in the remaining cells based on the distance to the nearest cell
  #' @importFrom raster distance
  lake.r <- distance(lake.r)
  #' @importFrom raster distance
  river.r <- distance(river.r)
  
  ### Aggregate back to the right spatial scale
  #' @importFrom raster aggregate
  lake.a <- aggregate(lake.r,covariate_factor)
  #' @importFrom raster aggregate
  river.a <- aggregate(river.r,covariate_factor)
  
  ### For coastline, we get a map of all land
  #' @importFrom sf st_read
  land_shapes = st_read(paste(layers_dir,"land_shapefiles","land_polygons.shp",sep='/'),quiet=TRUE)
  
  ### Filter out things that are far away from the area of interest
  #' @importFrom sf st_union
  land_shapes = st_union(extract_shapefile_from_raster(land_shapes,raster,expansion_factor))
  new_extent = expand_extent(
    #' @importFrom raster extent
    #' @importFrom sf st_crs
    extent(projectExtent(raster,st_crs(land_shapes)$proj4string)),
    expansion_factor
  )
  
  ### Make an aggregate raster at the right gridsize on the new extent
  ###   We'll crop later
  #' @importFrom raster extend
  coast.a <- extend(raster,new_extent)
  
  ### Dissagregate it for better estimates
  #' @importFrom raster disaggregate
  coast.r <- disaggregate(coast.a,covariate_factor)
  
  ### Set anything not on land to 0
  values(coast.r) = 0
  #' @importFrom raster mask
  coast.r <- mask(coast.r,as_Spatial(land_shapes),inverse=TRUE)
  
  ### Fill in the rest with the appropriate distances
  #' @importFrom raster distance
  coast.r <- distance(coast.r)
  
  ### Aggregate back to the right spatial scale and crop back to the right extent.
  #' @importFrom raster aggregate
  coast.a <- aggregate(coast.r,covariate_factor)
  #' @importFrom raster crop
  coast.a <- crop(coast.a,raster)
  
  water.a <- min(river.a,lake.a,coast.a)
  return(water.a)
}

#' @export
#' @name crop_raster_to_shapefile
#' @title crop_raster_to_shapefile
#' @description Crop a raster to the extent of a shapefile
#' @param raster_layer The Raster* to extract from.
#' @param shapefile The shapefile to use for bounding
#' @return A Raster* of the same type as \code{raster_layer} with the extent determined by \code{shapefile}
crop_raster_to_shapefile <- function(raster_layer,shapefile){
  if("sf" %in% class(shapefile)){
    shapefile = shapefile$geometry
  }
  if("sfc" %in% class(shapefile)){
    #' @importFrom sf as_Spatial
    shapefile = as_Spatial(shapefile)
  }
  #' @importFrom raster extent
  lhs = extent(raster_layer)
  #' @importFrom raster extent
  ## This should maybe be imported from sf somehow?
  #' @importFrom raster extent
  rhs = extent(shapefile)
  if(!compare_extents(shapefile,raster_layer)){
    #' @importFrom raster crop
    raster_layer <- crop(
      raster_layer,
      #' @importFrom raster extent
      extent(shapefile),
      snap='out'
    )
  }
}
