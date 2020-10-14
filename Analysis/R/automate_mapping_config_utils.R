automate_mapping_config <- function(p, covariate_names){
  
  map_name <- paste0(p$countries_name, "_", lubridate::year(as.Date(p$start_time)), "_", lubridate::year(as.Date(p$end_time)), "_", p$res_space, "km")

  cat(paste0(
    "name: '", map_name, "'\n",
    "taxonomy: '", p$taxonomy_path, "'\n",
    "countries: ", p$countries, "\n",
    "countries_name: '", p$countries_name, "'\n",
    "res_space: ", p$res_space, "\n",
    "res_time: '", p$res_time, "'\n",
    "smoothing_period: ", p$smoothing_period, "\n",
    "case_definition: '", p$case_definition, "'\n",
    "start_time: '", as.Date(p$start_time), "'\n",
    "end_time: '", as.Date(p$end_time), "'\n",
    "covariate_choices: ['", paste(covariate_names, collapse="','"), "']\n",
    "data_source: '", p$data_source, "'\n",
    "stan:\n",
    "  ncores: ", p$ncores, "\n",
    "  model: '", p$model, ".stan'\n",
    "  niter: ", p$niter, "\n",
    "  recompile: ", p$recompile, "\n"
  ))

}


