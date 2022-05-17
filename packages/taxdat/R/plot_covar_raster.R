#' @include plot_cache_function.R

#' @export
#' @name plot_raster_covariates
#' @title plot_raster_covariates
#' @description add
#' @param covar_data_filename covariates rdata filename
#' @param render default is FALSE
#' @return ggplot object with covariate raster
plot_raster_covariates <- function(cache,cholera_directory,config) {
  get_covar(name="covar_cube_output", config = params$config,cache=cache,cholera_directory = params$cholera_directory)
  get_covar_cube (name="covar_cube", config = params$config,cache=cache,cholera_directory = params$cholera_directory)
  get_sf_grid(name="sf_grid", config = params$config,cache=cache,cholera_directory = params$cholera_directory)
  covar_cube_output<-cache[["covar_cube_output"]]
  covar_cube<-cache[["covar_cube"]]
  sf_grid<-cache[["sf_grid"]]
  
  covar_layers <- covar_cube[,,-1, drop = F]
  ncovar <- ifelse(length(dim(covar_layers))==2, 1, dim(covar_layers)[3])
  
  if(nrow(sf_grid) == prod(dim(covar_cube[,,1, drop = F]))){
    
    covar_df <- purrr::map_dfc(seq_len(ncovar), function(x){
      if(ncovar>1){
        covar_layer <- cbind(covar_layers[,,x])
      } else {
        covar_layer <- abind::adrop(covar_layers, 3)
      }
      unlist(lapply(1:ncol(covar_layers), function(x){
        covar_layer[,x]
      }))
    })
    covar_df <- purrr::set_names(covar_df, dimnames(covar_cube_output$covar_cube)[[3]][-1])
    
    pltdata <- dplyr::bind_cols(sf_grid, covar_df)
    
    ## plot first time point of all covariates for now
    pltdata_dummy <-
      tidyr::gather(
        dplyr::filter(
          pltdata,
          t == 1
        ),
        one_of(dimnames(covar_cube_output$covar_cube)[[3]]), key = "covars", value = "value"
      )

    plt<-pltdata_dummy%>%group_by(covars)%>%
      do(gg={
        ggplot(.,ggplot2::aes(fill = value, color = value)) + ggplot2::geom_sf()+
          ggplot2::facet_wrap(~covars)+
          ggplot2::scale_fill_viridis_c(aesthetics = c("colour", "fill"),
                                        guide = ggplot2::guide_colorbar(title = "Covariate at time 1"),
                                        option = "B", na.value = "white") + 
          ggplot2::theme_bw() + 
          ggplot2::theme(legend.position = "bottom",
                         legend.key.size = unit(0.5, 'cm'),
                         legend.title =element_text(size=10))})%>%
      .$gg%>%gridExtra::grid.arrange(grobs=.,nrow=2)
    return(plt)
  } else{
    warning("sf_grid has a different number of cells or timepoints than covar_cube")
  }
}
