library(Cairo)
library(dplyr)
library(sf)

### DEFINE TAXDAT FUNCTIONS
#### aggregate_raster_xlayers
aggregate_raster_xlayers <- function(x, fun, ...) {
  if (is.null(dim(raster::getValues(x)))) {
    return(x)
  }
  rc <- raster::raster(x)

  raster::setValues(rc, apply(raster::getValues(x), 1, fun, ...))
}
#### aggregate_raster_xcells
aggregate_raster_xcells <- function(x, fun, ...) {
  if (is.null(dim(raster::getValues(x)))) {
    return(fun(raster::getValues(x), ...))
  }
  apply(raster::getValues(x), 2, fun, ...)
}
#### non_na_cells
non_na_cells <- function(raster_layer) {
  if ("population" %in% names(raster_layer)) {
    non_na_gridcells <- which(
      (aggregate_raster_xlayers(raster_layer, function(x) {
        !any(is.na(x))
      })[]) &
        ((raster_layer[["population"]])[] >= 1)
    )
  } else {
    non_na_gridcells <- which(aggregate_raster_xlayers(raster_layer, function(x) {
      !any(is.na(x))
    })[])
  }
  if (length(non_na_gridcells) == 0) {
    stop("All gridcells are NA")
  }
  gridcell_converter <- setNames(1:length(non_na_gridcells), non_na_gridcells)
  names(non_na_gridcells) <- gridcell_converter
  return(list(non_na_gridcells, gridcell_converter))
}

column_container <- function(x, y = names(x)) {
  if (length(y) != length(x)) {
    warning("x and y should have the same length")
  }
  y <- gsub("_", " ", y)
  y <- gsub(".stan", "", y)
  y <- gsub(".", " ", y, fixed = TRUE)
  for (i in seq_len(length(y))) {
    x[[i]] <- column(4, strong(y[[i]]), x[[i]])
  }
  return(
    do.call(fluidRow, x[seq_len(length(y))])
  )
}

filename_to_stubs <- function(x) {
  if (length(x) == 0) {
    return(x)
  }
  print(x)
  x <- strsplit(x, "/")
  x <- sapply(
    x,
    function(y) {
      y[[length(y)]]
    }
  )
  x <- strsplit(x, ".", fixed = TRUE)
  x <- sapply(
    x,
    function(y) {
      if (y[[1]] == "testing") {
        y[[1]] <- paste(y[1:2], collapse = ".")
        y <- y[-2]
      }
      y <- y[-4]
      y <- y[-2]
      if (length(y) > 3) {
        y <- y[-length(y)]
      }
      return(y)
    }
  )
  return(x)
}


read_file_of_type <- function(filename, variable) {
  if (grepl("output.\\d+.csv$", filename)) { # Stan output csv
    model.rand <- rstan::read_stan_csv(filename)
  }
  if (grepl("json$", filename)) { # stan input json
    stan_data <- jsonlite::read_json(filename, simplifyVector = TRUE)
  }
  if (grepl("rdata$", filename)) { # some kind of rdata file
    load(filename)
  }
  if (!exists(variable)) {
    stop(paste0("The variable (", variable, ") isn't present in the file", filename, ")"))
  }
  return(eval(parse(text = variable)))
}

server <- function(input, output, session) {
  data_directory <- "/home/Users/jkaminsky/git/cholera-taxonomy/branches/postgis-covar/Analysis/data/"

  all_data <- list.files(data_directory)
  # all_data <- all_data[grepl(".rdata$", all_data)]
  all_data <- c(
    all_data[grepl("^testing", all_data)],
    all_data[!grepl("^testing", all_data)]
  )
  all_stubs <- filename_to_stubs(all_data)
  data_stubs <- unique(sapply(all_stubs, function(x) {
    x[[1]]
  }))

  output$data_stubs <- renderUI({
    radioButtons(
      "data",
      strong("Display Which Data?"),
      choices = data_stubs,
      inline = FALSE
    )
  })

  model_stubs <- reactive({
    indices <- which(
      sapply(
        all_stubs,
        function(x) {
          (x[[1]] == input$data) & (length(x) >= 3)
        }
      )
    )
    unique(sapply(all_stubs[indices], function(x) {
      x[[2]]
    }))
  })

  output$model_stubs <- renderUI({
    checkboxGroupInput(
      "models",
      strong("Display Which Models?"),
      choices = model_stubs()
    )
  })

  iterations_stubs <- reactive({
    indices <- which(
      sapply(
        all_stubs,
        function(x) {
          if (length(x) < 3) {
            return(FALSE)
          }
          x[[1]] == input$data &
            x[[2]] %in% input$models
        }
      )
    )

    rc <- sapply(
      all_data[indices],
      function(x) {
        paste(filename_to_stubs(x)[2:3], collapse = " ")
      }
    )

    return(unique(rc))
  })

  output$iterations_stubs <- renderUI({
    checkboxGroupInput(
      "iterations",
      strong("Display Which Number of Iterations?"),
      choices = iterations_stubs()
    )
  })

  preprocessed_data_filename <- reactive({
    indices <- which(
      grepl(paste0(input$data, "."), all_data, fixed = TRUE) &
        (grepl("preprocess", all_data))
    )
    rc <- paste(data_directory, all_data[indices], sep = "/")
    return(rc)
  })

  covar_data_filename <- reactive({
    indices <- which(
      grepl(paste0(input$data, "."), all_data, fixed = TRUE) &
        (grepl("covar", all_data))
    )
    rc <- paste(data_directory, all_data[indices], sep = "/")
    return(rc)
  })

  model_input_filenames <- reactive({
    indices <- which(
      sapply(
        input$iterations,
        function(x) {
          grepl(x, sapply(all_stubs, paste, collapse = " "))
        }
      ) &
        sapply(all_stubs, function(x) {
          x[[1]] == input$data
        }) &
        grepl("input", all_data, fixed = TRUE)
    )
    rc <- paste(data_directory, all_data[indices], sep = "/")
    return(rc)
  })

  model_output_filenames <- reactive({
    indices <- which(
      sapply(
        input$iterations,
        function(x) {
          grepl(x, sapply(all_stubs, paste, collapse = " "))
        }
      ) &
        sapply(all_stubs, function(x) {
          x[[1]] == input$data
        }) &
        grepl("output", all_data, fixed = TRUE)
    )
    rc <- paste(data_directory, all_data[indices], sep = "/")
    if (length(indices) == 0) {
      return(NULL)
    }
    print(rc)
    return(rc)
  })

  disjoint_set_sf_cases <- reactive({
    sf_cases <- read_file_of_type(preprocessed_data_filename(), "sf_cases")
    my_names <- names(sf_cases)[
      c(grep("location", names(sf_cases)), grep("name_", names(sf_cases)))
    ]
    sf_cases$attributes.location_period_id <- sf_cases[[my_names[[1]]]]
    for (i in (1 + seq_len(length(my_names) - 1))) {
      sf_cases$attributes.location_period_id <- paste(
        sf_cases$attributes.location_period_id,
        sf_cases[[my_names[i]]]
      )
    }

    aggregate_sf_cases <- dplyr::summarize(
      dplyr::group_by(
        sf_cases,
        attributes.location_period_id
      ),
      cases = mean(attributes.fields.suspected_cases / as.numeric(TR - TL + 1) * 365),
      variance = var(attributes.fields.suspected_cases / as.numeric(TR - TL + 1) * 365),
      observations = length(attributes.fields.suspected_cases)
    )
    aggregate_sf_cases <- sf::st_as_sf(aggregate_sf_cases)

    aggregate_sf_cases$area <-
      as.numeric(sf::st_area(aggregate_sf_cases)) / 1000 / 1000
    aggregate_sf_cases$area_adjusted_cases <-
      aggregate_sf_cases$cases / aggregate_sf_cases$area

    aggregate_sf_cases <- dplyr::arrange(aggregate_sf_cases, -area)
    overlaps <-
      sf::st_relate(aggregate_sf_cases, aggregate_sf_cases, "2********")
    non_overlapping_sets <- list()

    aggregate_sf_cases$not_included <- TRUE
    index <- 0
    while (any(aggregate_sf_cases$not_included)) {
      leftovers <- which(aggregate_sf_cases$not_included)
      index <- index + 1
      non_overlapping_sets[[index]] <- NA
      not_allowed <- NA
      for (idx in leftovers) {
        if (idx %in% not_allowed) {
        } else {
          aggregate_sf_cases$not_included[idx] <- FALSE
          non_overlapping_sets[[index]] <- c(non_overlapping_sets[[index]], idx)
          not_allowed <- sort(unique(c(not_allowed, overlaps[[idx]])))
        }
      }
      non_overlapping_sets[[index]] <-
        non_overlapping_sets[[index]][!is.na(non_overlapping_sets[[index]])]
    }

    aggregate_sf_cases$set <- as.integer(NA)

    for (set in seq_len(length(non_overlapping_sets))) {
      aggregate_sf_cases$set[non_overlapping_sets[[set]]] <- set
    }
    return(aggregate_sf_cases)
  })

  output$raw_area_adjusted_cases <- renderPlot({
    plt <- ggplot2::ggplot()

    if (length(preprocessed_data_filename()) == 1) {
      plt <- plt +
        ggplot2::geom_sf(
          data = disjoint_set_sf_cases(),
          ggplot2::aes(fill = case_scale_fun()(area_adjusted_cases))
        ) +
        ggplot2::scale_fill_continuous("Area-adjusted cases") +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::facet_wrap(~set)
    }
    pdf("~/area_adjusted_observations.pdf")
    print(plt)
    dev.off()
    plt
  })

  output$raw_observations <- renderPlot({
    plt <- ggplot2::ggplot()

    if (length(preprocessed_data_filename()) == 1) {
      plt <- plt +
        ggplot2::geom_sf(
          data = disjoint_set_sf_cases(),
          ggplot2::aes(fill = observations)
        ) +
        ggplot2::scale_fill_continuous("Observations") +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::facet_wrap(~set)
    }
    pdf("~/raw_observations.pdf")
    print(plt)
    dev.off()
    plt
  })

  output$raw_variance <- renderPlot({
    plt <- ggplot2::ggplot()

    if (length(preprocessed_data_filename()) == 1) {
      plt <- plt +
        ggplot2::geom_sf(
          data = disjoint_set_sf_cases(),
          ggplot2::aes(fill = variance)
        ) +
        ggplot2::scale_fill_continuous("Variance") +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::facet_wrap(~set)
    }
    plt
  })

  output$raw_cases <- renderPlot({
    plt <- ggplot2::ggplot()

    if (length(preprocessed_data_filename()) == 1) {
      plt <- plt +
        ggplot2::geom_sf(
          data = disjoint_set_sf_cases(),
          ggplot2::aes(fill = case_scale_fun()(cases))
        ) +
        ggplot2::scale_fill_continuous("Cases") +
        ggplot2::theme(legend.position = "bottom") +
        ggplot2::facet_wrap(~set)
    }
    pdf("~/raw_cases.pdf")
    print(plt)
    dev.off()
    plt
  })

  output$raster_population <- renderPlot({
    plt <- ggplot2::ggplot()
    if (length(covar_data_filename()) == 1) {
      covar_cube_output <- read_file_of_type(covar_data_filename(), "covar_cube_output")
      covar_cube <- covar_cube_output$covar_cube
      sf_grid <- covar_cube_output$sf_grid
      pop_layer <- covar_cube[, , 1] ## population is always the first layer

      if (nrow(sf_grid) == prod(dim(pop_layer))) {
        covar <- data.frame(covar = unlist(lapply(1:ncol(pop_layer), function(x) {
          pop_layer[, x]
        })))
        pltdata <- dplyr::bind_cols(sf_grid, covar)

        ## plots population for all time points
        plt <- plt +
          ggplot2::geom_sf(
            data = pltdata,
            ggplot2::aes(
              fill = case_scale_fun()(covar),
              color = case_scale_fun()(covar)
            )
          ) +
          # ggplot2::scale_fill_continuous("Population") +
          ggplot2::scale_fill_viridis_c(
            trans = rate_trans_f(),
            aesthetics = c("colour", "fill"),
            guide = ggplot2::guide_colorbar(title = "Population density [per grid cell]"),
            option = "E"
          ) +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::facet_wrap(~t)
      } else {
        warning("sf_grid has a different number of cells or timepoints than covar_cube")
      }
    } else {
      warning("There were multiple possible covariate Rdata files")
    }
    plt
  })

  output$raster_covariates <- renderPlot({
    plt <- ggplot2::ggplot()
    if (length(covar_data_filename()) == 1) {
      covar_cube_output <- read_file_of_type(covar_data_filename(), "covar_cube_output")
      covar_cube <- covar_cube_output$covar_cube
      sf_grid <- covar_cube_output$sf_grid
      covar_layers <- covar_cube[, , -1]
      ncovar <- ifelse(length(dim(covar_layers)) == 2, 1, dim(covar_layers)[3])

      if (nrow(sf_grid) == prod(dim(covar_cube[, , 1]))) {
        covar_df <- purrr::map_dfc(seq_len(ncovar), function(x) {
          if (ncovar > 1) {
            covar_layer <- covar_layers[, , x]
          } else {
            covar_layer <- covar_layers
          }
          unlist(lapply(1:ncol(covar_layers), function(x) {
            covar_layer[, x]
          }))
        })
        covar_df <- purrr::set_names(covar_df, paste0("covar.", seq_len(ncovar)))

        pltdata <- dplyr::bind_cols(sf_grid, covar_df)

        ## plot first time point of all covariates for now
        pltdata_dummy <-
          tidyr::gather(
            dplyr::filter(
              pltdata,
              t == 1
            ),
            contains("covar."),
            key = "covars", value = "value"
          )

        plt <- plt +
          ggplot2::geom_sf(
            data = pltdata_dummy,
            ggplot2::aes(fill = value, color = value)
          ) +
          ggplot2::scale_fill_viridis_c(
            trans = rate_trans_f(),
            aesthetics = c("colour", "fill"),
            guide = ggplot2::guide_colorbar(title = "Covariate at time 1"),
            option = "B"
          ) +
          # ggplot2::scale_fill_continuous("Covariate at time 1") +
          ggplot2::theme(legend.position = "bottom") +
          ggplot2::facet_wrap(~covars)
      } else {
        warning("sf_grid has a different number of cells or timepoints than covar_cube")
      }
    } else {
      warning("There were multiple possible covariate Rdata files")
    }
    plt
  })

  non_na_gridcells <- reactive({
    covar_cube_output <- read_file_of_type(covar_data_filename(), "covar_cube_output")
    non_na_gridcells <- covar_cube_output$non_na_gridcells
    non_na_gridcells
  })

  case_raster <- reactive({
    sf_cases <- read_file_of_type(preprocessed_data_filename(), "sf_cases")
    # case_raster <- basic_raster ## one t only?
    # layer_index <- 1
    if (grepl("^testing", filename_to_stubs(preprocessed_data_filename())[1])) {
      basic_raster <- read_file_of_type(preprocessed_data_filename(), "test_data")[["raster"]] ## template sf object
      test_data <- read_file_of_type(preprocessed_data_filename(), "test_data")
      case_raster <- test_data$underlying_distribution_mean[non_na_gridcells(), ]
      names(case_raster)[names(case_raster) == "cases"] <- "true_cases"
      names(case_raster)[names(case_raster) == "rate"] <- "true_rates"
    } else {
      covar_cube_output <- read_file_of_type(covar_data_filename(), "covar_cube_output")
      sf_grid <- covar_cube_output$sf_grid
      basic_raster <- sf_grid
      case_raster <- sf_grid
      test_data <- NULL
    }

    nchains <- 0
    for (filename in model_output_filenames()) {
      nchains <- nchains + 1
      model.rand <- read_file_of_type(filename, "model.rand")
      modeled_cases <- as.array(model.rand)[, , grepl("grid_case", names(model.rand)), drop = FALSE]
      modeled_cases_mean <- apply(modeled_cases, 3, mean)
      modeled_rates <- exp(as.array(model.rand)[, , grepl("log_lambda", names(model.rand)), drop = FALSE])
      modeled_rates_mean <- apply(modeled_rates, 3, mean)

      case_raster <- case_raster %>%
        dplyr::mutate(
          modeled_cases_mean = NA,
          modeled_rates_mean = NA
        )
      case_raster[non_na_gridcells(), ]$modeled_cases_mean <- modeled_cases_mean
      names(case_raster)[which(names(case_raster) == "modeled_cases_mean")] <- paste(
        "modeled cases\n",
        paste(filename_to_stubs(filename)[2:3], collapse = " "),
        "\niterations: Chain", filename_to_stubs(filename)[5]
      )
      case_raster[non_na_gridcells(), ]$modeled_rates_mean <- modeled_rates_mean
      names(case_raster)[which(names(case_raster) == "modeled_rates_mean")] <- paste(
        "modeled rates\n",
        paste(filename_to_stubs(filename)[2:3], collapse = " "),
        "\niterations: Chain", filename_to_stubs(filename)[5]
      )
    }

    case_raster
  })

  output$modeled_cases <- renderPlot({
    case_raster <- case_raster() %>%
      dplyr::select(contains("modeled cases"), id, t) %>%
      tidyr::gather(contains("iterations: Chain"), key = "chain", value = "value") %>%
      # tidyr::pivot_longer(contains("iterations: Chain"), names_to = "chain", values_to = "value") %>%
      dplyr::mutate(chain = stringr::str_replace(chain, "modeled cases", ""))

    plt <- ggplot2::ggplot()
    plt <- plt +
      ggplot2::geom_sf(
        data = case_raster,
        ggplot2::aes(fill = value, color = value)
      ) +
      # ggplot2::scale_fill_vidris_c("modeled cases", limits = uniform_scale_fun()) +
      ggplot2::scale_fill_viridis_c(
        trans = case_trans_f(),
        breaks = c(1, 10, 100, 1000),
        aesthetics = c("colour", "fill"),
        guide = ggplot2::guide_colorbar(title = "Incidence\n [cases/year]"),
        limits = uniform_scale_fun()
      ) +
      ggplot2::theme(legend.position = "bottom") +
      plot_facet_wrap() +
      ggplot2::theme(legend.text = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
    pdf("~/modeled_cases.pdf")
    print(plt)
    dev.off()
    plt
  })

  output$modeled_rates <- renderPlot({
    case_raster <- case_raster() %>%
      dplyr::select(contains("modeled rates"), id, t) %>%
      tidyr::gather(contains("iterations: Chain"), key = "chain", value = "value") %>%
      dplyr::mutate(chain = stringr::str_replace(chain, "modeled rates", ""))

    rate_rescaling <- 1e4 # rescale to have incidence per 10'000 people
    plt <- ggplot2::ggplot()
    plt <- plt +
      ggplot2::geom_sf(
        data = case_raster,
        ggplot2::aes(fill = value * rate_rescaling, color = value * rate_rescaling)
      ) +
      # ggplot2::scale_fill_continuous("modeled rates", limits = uniform_scale_fun()) +
      ggplot2::scale_fill_viridis_c(
        trans = rate_trans_f(),
        breaks = c(0.01, 0.1, 1, 10, 100, 1000),
        aesthetics = c("colour", "fill"),
        guide = ggplot2::guide_colorbar(title = "Incidence rate\n [cases/10'000/year]"),
        limits = uniform_scale_fun()
      ) +
      ggplot2::theme(legend.position = "bottom") +
      plot_facet_wrap() +
      ggplot2::theme(legend.text = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1))
    pdf("~/modeled_rates.pdf")
    print(plt)
    dev.off()
    plt
  })


  data_fidelity <- reactive({
    rc <- list()
    layer_index <- 1
    for (filename in model_output_filenames()) {
      corresponding_input_filename <- gsub("\\d+.csv", "json", gsub("stan_output", "stan_input", filename))
      print(c(filename, corresponding_input_filename))
      model.rand <- read_file_of_type(filename, "model.rand")
      stan_data <- read_file_of_type(corresponding_input_filename, "stan_input")$stan_data
      modeled_cases <- as.array(model.rand)[, , grepl("modeled_cases", names(model.rand)), drop = FALSE]
      modeled_cases_chain_mean <- apply(modeled_cases, c(2, 3), mean)
      actual_cases <- matrix(stan_data$y, nrow(modeled_cases_chain_mean), ncol(modeled_cases_chain_mean), byrow = TRUE)
      dimnames(actual_cases) <- dimnames(modeled_cases_chain_mean)
      modeled_cases_chain_mean <- reshape2::melt(modeled_cases_chain_mean)
      actual_cases <- reshape2::melt(actual_cases)
      comparison <- dplyr::left_join(modeled_cases_chain_mean, actual_cases, by = c(chains = "chains", parameters = "parameters"))
      names(comparison)[3:4] <- c("modeled cases", "actual cases")
      rc[[filename]] <- comparison
      names(rc)[[layer_index]] <- paste(
        paste(filename_to_stubs(filename)[2:3], collapse = " "),
        "\niterations: Chain", filename_to_stubs(filename)[5]
      )
      layer_index <- layer_index + 1
    }
    return(rc)
    # return((names(rc),function(x){renderPlot({rc[[x]]})}))
  })


  output$model_fidelity <- renderUI({
    comparison <- data_fidelity()
    rate_raster <- case_raster()
    column_container(
      lapply(
        seq_len(length(comparison)),
        function(x) {
          renderPlot({
            plt <- ggplot2::ggplot(comparison[[x]]) +
              ggplot2::geom_point(ggplot2::aes(x = `modeled cases`, y = `actual cases`, col = chains)) +
              ggplot2::geom_abline(intercept = 0, slope = 1) +
              ggplot2::coord_fixed(ratio = 1, xlim = c(1, max(comparison[[x]][, 3:4])), ylim = c(1, max(comparison[[x]][, 3:4])))
            pdf("~/model_fidelity.pdf")
            print(plt)
            dev.off()
            plt
          })
        }
      ),
      names(comparison)
    )
  })

  case_scale_fun <- reactive({
    if (input$cases_log) {
      return(function(x) {
        return(log(x + 1) / log(10))
      })
    }
    return(function(x) {
      return(x)
    })
  })

  case_trans_f <- reactive({
    if (input$cases_log) {
      return("log")
    }
    return("identity")
  })

  rate_scale_fun <- reactive({
    rate_rescaling <- 1e4 # rescale to have incidence per 10'000 people
    if (input$rates_log) {
      return(function(x) {
        return(log(((x * rate_rescaling) + 1)) / log(10))
      })
    }
    return(function(x) {
      return(x * rate_rescaling)
    })
  })

  rate_trans_f <- reactive({
    if (input$rates_log) {
      return("log")
    }
    return("identity")
  })

  uniform_scale_fun <- reactive({
    if (input$uniform_scale) {
      return(function(x) {
        c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
      })
    }
    return(function(x) {
      NULL
    })
  })

  plot_facet_wrap <- reactive({
    if (as.logical(input$break_chains) & as.logical(input$break_times)) {
      return(ggplot2::facet_wrap(~ t + chain, ncol = 4))
    }
    if (as.logical(input$break_times)) {
      return(ggplot2::facet_wrap(~t, ncol = 4))
    }
    if (as.logical(input$break_chains)) {
      return(ggplot2::facet_wrap(~chain, ncol = 4))
    }
  })

  output$rates_log <- renderUI({
    radioButtons(
      "rates_log",
      strong("Should I plot rates on the log scale?"),
      choices = c(TRUE, FALSE),
      inline = TRUE
    )
  })

  output$cases_log <- renderUI({
    radioButtons(
      "cases_log",
      strong("Should I plot cases on the log scale?"),
      choices = c(TRUE, FALSE),
      inline = TRUE
    )
  })

  output$uniform_scale <- renderUI({
    radioButtons(
      "uniform_scale",
      strong("Should rasters be plotted on a uniform color scale?"),
      choices = c(TRUE, FALSE),
      inline = TRUE
    )
  })

  output$break_chains <- renderUI({
    radioButtons(
      "break_chains",
      strong("Should I plot chains individually?"),
      choices = c(TRUE, FALSE),
      inline = TRUE
    )
  })

  output$break_times <- renderUI({
    radioButtons(
      "break_times",
      strong("Should I plot times individually?"),
      choices = c(TRUE, FALSE),
      inline = TRUE
    )
  })
}
