#' @name run_test_case
#' @title run_test_case
#' @description Help with test cases
#' @param polygon_type polygon types for test data (real polygon or fake/test polygon) ("real_polygon","std_grid"(default),"coarse_grid","overlapping_grid")
#' @param data_type to use raw data or gridded cases as the test data ("grid_case"(default),"real_case")
#' @param oc_type observation file types for test data ("std_oc"(default),"single_oc","nonzero_oc","linelist","outbreak_sitrep")
#' @param grid_coverage_type if the observed data is assumed to be completely or partially reported at the grid level ("std_coverage"(default),"full_coverage","partial_coverage")
#' @param polygon_coverate_type ("partial_coverage")
#' @param observation_type the types of the observations we observe (from one true distribution or multiple true distributions) ("single_test_observation"(default),"multiple_test_observations")
#' @param randomize if the boundaries of the polygon align perfectly with the polygon (TRUE(default), FALSE)
#' @param ncols number of grids per column
#' @param nrows number of grids per row
#' @param grid_proportion_observed proportion of gridcells that are observed.
#' @param time_scale the temporal resolution of the data ("year"(default),"month")
#' @param model model that is applied here ("dagar_seasonal.stan" (default))
#' @parm niter number of iterations for the model (10000(default))
#' @param ncovariates the number of covariates used here (2 (default, including population))
#' @param temporally_smooth_nonpop non population covariates are temporally smoothed or not(TRUE, FALSE (default))
#' @param spatially_smooth_nonpop non population covariates are spatially smoothed or not(TRUE, FALSE (default))
#' @param nonspatial Whether the independent layer varies in space for each covariate (TRUE,FALSE (default))
#' @param nontemporal Whether the independent layer varies in time for each covariate (TRUE, FALSE (default))
#' @param polygonal Whether to include the polygonal layer for each covariate (FALSE,TRUE (default))
#' @param radiating  Whether to include the radiating layer for each covariate (TRUE,FALSE (default))
#' @return country data report (html)

run_test_case <- function(data_type = "grid_case",
                          oc_type = "std_oc",
                          polygon_type = "std_grid",
                          grid_coverage_type = "std_coverage",
                          observation_type = "single_test_observation",
                          randomize = TRUE,
                          ncols = 10,
                          nrows = 10,
                          grid_proportion_observed = 0.9,
                          polygon_proportion_observed = 1,
                          time_scale = "year",
                          model = "dagar_seasonal.stan",
                          niter = 10000,
                          ncovariates = 2,
                          temporally_smooth_nonpop = FALSE,
                          spatially_smooth_nonpop = FALSE,
                          nonspatial = FALSE,
                          nontemporal = FALSE,
                          polygonal = TRUE,
                          radiating = FALSE) {
  library(sf)
  library(tidyverse)
  library(taxdat)

  ## ------------------------------------------------------------------------------------------------------------------------
  ## based on the type of test case, update the arguments of functions
  # polyton types
  if (polygon_type %in% "std_grid") {
    nrows <- 10
    ncols <- 10
    print("Generate 10*10 grid cells for this test case.")
  } else if (polygon_type %in% "coarse_grid") {
    if (is.null(ncols) == F & is.null(nrows) == F) {
      if (ncols * nrows < 100) {
        ncols <- ncols
        nrows <- nrows
        print(paste(nrows, "*", ncols, "gridcells are applied here."))
      } else {
        stop("Need more coarse grid cell numbers. (fewer than 100 grid cells)")
      }
    } else {
      stop("Number of grid cells per row and per column must be specified for test case with coarse grid cells.")
    }
  }

  if (polygon_type %in% "overlapping_grid") {
    randomize <- FALSE
    print("Generate polygons uniformly for multiple layers.Polygon boundaries are aligned with the grid cell boundaries.")
  } else {
    print("Polygon boundaries may not be aligned with the grid cell boundaries (randomly).")
  }

  # grid coverage
  if (grid_coverage_type %in% "full_coverage") {
    grid_proportion_observed <- 1
    print("100% of the grid cells are observed.")
  } else if (grid_coverage_type %in% "partial_coverage") {
    grid_proportion_observed <- 0.4
    print("40% of the grid cells are observed.")
  } else if (grid_coverage_type %in% "std_coverage") {
    print("90% of the grid cells are observed.")
  }

  # polygon coverage
  if (polygon_coverate_type %in% "partial_coverage") {
    polygon_proportion_observed <- 0.4
    print("60% of regions are missing data.")
  }

  ## ------------------------------------------------------------------------------------------------------------------------
  ## load test dataset

  dbuser <- Sys.getenv("USER", "app")
  dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

  conn_pg <- taxdat::connect_to_db(dbuser, dbname)
  DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))

  my_seed <- c(
    10403, 624, 105045778, 1207077739, 2042172336, -219892751, -768060162,
    -2006256281, -1585201540, -978856627, -1568163926, -1028934365, 1356190728, 1795633769,
    1153151766, 1165788831, 2116870228, 833087301, 829928258, 1681319387, -277008544,
    1376178145, -672930770, 1480724183, 377343276, 861177917, -304533030, -1337000557,
    800359864, -468620199, -254982074, -1990955505, -1337858812, 1651087413, -1015114894,
    -1924473781, -831703280, 273737681, 341851998, 1940423751, -1326413860, 1868494637,
    -121227254, 2141020675, -1861967000, 7021129, -388945546, 940519807, -617802316,
    -488078043, 135954338, 1431662267, 449063616, -1878918207, 1730176654, -1598728777,
    1011281548, -847896035, -1452160454, 1100866163, 1918971160, -1271704519, 1016747686,
    -1264332561, -910593436, 1723051541, 198080466, 1748868395, 930941552, -753333327,
    1549228478, -1153465561, -1364459204, -1660352243, 1052698730, -652771613, -2010832184,
    189334057, -1061433386, -1345762209, 1232206612, -1793453307, -874677246, -72718437,
    -1772956128, 1800738721, 1559763182, 618531991, -653606932, 424061949, -2142203238,
    1787901779, -311720840, 424356889, 1049138438, -1745443889, -626937916, -114641931,
    1716161586, 1057148427, 1740672464, 1781866385, -558521314, 887616007, 386804380,
    1495141101, -292960054, -867038269, -737193432, -375860727, -1542216138, 157472575,
    1616089204, -239824667, 1238935650, -881995653, 1258369408, 1358439297, -1811743922,
    -1468779657, -14597812, 1061037533, -225604870, -161416141, 883657688, 1867576313,
    875561830, 1040508591, -799892188, -1589204523, 1711984786, 332558059, -32521936,
    10298225, -1676468610, 307876071, 309265404, 1779825869, 238999850, 1921321123,
    2124099976, 1968345577, -1234129770, 2106839583, -483797548, -567279931, 1529087170,
    -724063909, 229063904, -1444733087, -1294398034, 1261423191, 1826779820, 601132989,
    205566810, -867880685, 1207518008, -1908626471, -1490509370, -1920558705, -1004258684,
    578785205, -1440267022, -2065399861, -2059230064, -1119283375, 1627900126, -515039289,
    1965460828, 1382604461, 804657546, 15234435, -39796504, 74334665, 1735058166,
    554974463, -1051931852, 2080660645, -455634654, -1004539333, -1625491392, -619836607,
    739397646, -2000105161, -1935256564, 1736567197, 1703021498, -277490189, -1825789288,
    -905555015, 1829400614, 1228127343, -76642332, 2085447061, -575149742, -865009493,
    1949382640, -1020200143, -970218690, -1954947417, -1081293124, 1351528589, 1908902378,
    -927706525, 855113800, 1759764905, 1922713942, -459532321, 370438292, 682207877,
    -670347902, -511792357, -1405459552, 1165027105, 1044824686, 955729943, 1959060844,
    1723741053, 577344538, 2133519059, -300039688, -224007271, -1486000506, 264523599,
    -410417852, -366466187, -1210794574, -1062211189, 766737232, 827117329, 806792606,
    1452805511, 502902812, 1593799277, 2110466634, 924884803, 1819865000, -257199735,
    -543760458, -1797037377, 685295092, 2107107429, 836188642, 288084987, -841515264,
    1174647553, -200383282, -1888276745, -409590068, 973172061, -217101190, -1231261773,
    -1666938536, 1410560889, -1008100122, 1542476335, 634204836, -477634219, -74071534,
    -740732309, 10579632, 580162289, -815732738, -463491993, 1757759868, 875766861,
    -1978074454, -714777565, 1677430536, 1706881385, 33072662, -626909793, 1827785556,
    1346836037, -78424510, -748720933, -1999877536, 901672673, 604059438, 469009879,
    1399986220, -1245472963, 327890138, -318610285, 1318650040, -1236866215, 2081880902,
    -2073049841, 2140246020, 467961653, 2014508658, 337978187, 668144144, -1375713583,
    -1599974818, 847821639, 608670428, 1117898285, 1489566474, 1220184323, 1062723176,
    235954505, 2035784822, 982534271, -2118680396, -1305529307, 1234674338, 1146189243,
    1307624896, 1771641537, 1891056014, -902607689, -760170100, 1786556701, 1274367290,
    -1785411213, 533932056, 1563972409, -712144474, -1141200913, -366043804, -2101994219,
    140794578, 734749739, -903846544, -427300175, 1819471038, -1697957337, -2133291972,
    -1195157491, 938683242, 1381508579, -1872372280, -1415724759, 827048662, 358998879,
    847373844, -255311355, 1035738882, 473558683, 2048717088, 848017057, 1221094382,
    -504921193, 732564204, -1532708099, -261778022, 1066114643, -1742012552, 148876057,
    568674310, 289995471, -2030282044, 1131717365, -1818104014, 1627657483, -94279472,
    -618430831, -949307618, -757126905, 1523594652, -2129667603, 28281802, -814337341,
    1425902888, 2086473993, 1634539830, -1478824385, 1990785908, -1786101787, 81316706,
    -1353656453, 1445514368, -577879423, -422785458, 113846903, -793375668, 1823716573,
    -520876550, -1776360653, -1418164520, -181079303, -21612954, 1863499183, -1554855900,
    -979887915, 216249234, -1777912341, -1217520592, -1765080463, 304198014, -327383065,
    981896956, 1107250125, 471211050, -1186156637, -151589752, 977686761, -1923899498,
    178743583, 1904525524, 1710042565, -670718014, -305254309, 384587744, -1281835423,
    1365008558, 948978007, -533938772, -2027839811, -1984383398, 1619433491, -1181499848,
    -634377511, 1436046534, -1676676977, -1198922364, -1399292235, 1933682674, 1225574091,
    -333545584, 544634449, 2033179614, 1432049351, 1414764636, 1577692589, 2034162826,
    622042243, 1277819880, 458326217, -2070456842, 847467519, 1919759924, 1667828645,
    -61164510, 1676278075, -583036096, -106747327, 1527718670, -755709897, 610672396,
    1952872605, -489976134, -2114274061, -1907055208, -339127623, 1545839398, 989369199,
    1811526372, -672279403, -774648750, -1806907477, 1867158256, 2065553969, -181586370,
    -684485209, -2003873348, -207159411, -1757295382, 1141045603, -1771127992, -780754775,
    1657814102, -265900321, -189113452, -881469051, -1024462718, 970732059, -1241874784,
    -257587679, -1568860818, -1917927657, 330969196, 1895449213, 1883543322, -105688621,
    1637039352, -638031207, -1812979322, -897276337, -1896206268, 1661999733, 1068638386,
    771700875, 63592016, -1514590703, 1253163166, -1749203833, 1670495004, -582008467,
    -2143883958, 1666369091, -2086851928, -1968328567, -1885973834, -263620161, -1614532364,
    395024229, -1998758686, 870111995, -1711990272, -711470591, -1841416242, 2088117751,
    -796855860, 1968503901, 1111215994, -488089933, -685276072, -793415047, -896151578,
    -1519638225, 518217124, -1518907307, -537615086, 1750866283, 1484114352, -1690275343,
    173374206, 414212967, 498259068, -1317375155, -902414934, -331676893, 764234248,
    -252792727, -1098802922, -1146899297, 1189262932, -48085691, -1220871870, -1306266661,
    1845978464, -512619039, 1625983534, 251854039, 689070892, 904832573, -188139558,
    1959906195, -18252872, 2012768857, 430747206, 40311823, 1158494980, 849049141,
    -508339854, 832268875, 136637712, 1386980817, -1867487906, 935653959, -1689608740,
    -1029663443, -1755857398, 1676914691, 437721448, 707895369, -282456202, -1225957505,
    -378672204, 1840374565, -1021262430, -1326875461, 50946240, 1227635137, 286919822,
    286081975, -1748597620, 591347741, -1341861830, 44544115, 1420895000, -205958599,
    -422789978, -198344977, 1456457828, -638284779, -2147074606, 334834475, -1716535184,
    -1091384911, 106594238, 783479079, 1823545148, 523872013, -1783402902, 1807011043,
    991074504, -663846871, 1831318998, 585005663, -1296183020, -1766690555, 765903362,
    -932874853, 366152736, 1035883937, 1902427886, -1437795689, 1123374572, 1774111229,
    671520922, -73267885, -1068074376, -470799847, -477234426, 1769427407, -712833596,
    -1493528075, 33903154, 899767307, 2146321360, -821174895, 1609288222, -1825415161,
    -834781028, -1849841427, -10400054, -1061791293, -296237016, 429740041, -804090826,
    472843583, -97884556, -509874459
  ) %>%
    as.integer()

  query_time_left <- lubridate::ymd("2000-01-01")
  query_time_right <- lubridate::ymd("2001-12-31")
  ## Pull data frames needed to create testing database from the api This doesn't
  ## pull covariates, but does pull everything else tryCatch({ all_dfs <-
  ## taxdat::create_testing_dfs_from_api( username
  ## =Sys.getenv('CHOLERA_API_USERNAME'), api_key = Sys.getenv('CHOLERA_API_KEY'),
  ## locations = 'AFR::KEN', time_left = query_time_left, time_right =
  ## query_time_right, uids = NULL, website =
  ## 'https://api.cholera-taxonomy.middle-distance.com/' ) }, error = function(e) {
  ## })
  load(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "all_dfs_object.rdata"))

  if (data_type %in% "real_case") {
    if (oc_type %in% "single_oc") {
      all_dfs$observations_df <- all_dfs$observations_df %>% mutate(attributes.source_documents = as.character(attributes.source_documents))
      observations_df <- data.frame(
        OC = all_dfs$observations_df$relationships.observation_collection.data.id[1],
        number_observations = nrow(all_dfs$observations_df[all_dfs$observations_df$relationships.observation_collection.data.id == all_dfs$observations_df$relationships.observation_collection.data.id[1], ])
      )
      selected_OC <- observations_df$number_observations[1]
      selected_geom <- all_dfs$shapes_df$geom[1:selected_OC]
      all_dfs <- list(
        location_df = all_dfs$location_df[1:selected_OC, ],
        location_period_df = all_dfs$location_period_df[1:selected_OC, ],
        shapes_df = all_dfs$shapes_df[1:selected_OC, ],
        observations_df = all_dfs$observations_df[1:selected_OC, ]
      )
      print(paste("Here, we're testing the scenario with only one observation collection file using real data. Test dataset only include OC", selected_OC))
    } else if (oc_type %in% "nonzero_oc") {
      # select nonZero observations
      nonZero_rowname <- rownames(all_dfs$observations_df[which(all_dfs$observations_df$attributes.fields.suspected_cases > 0 & is.na(all_dfs$observations_df$attributes.fields.suspected_cases) == F & is.na(all_dfs$observations_df$attributes.location_period_id) == F), ])
      all_dfs <- list(
        location_df = all_dfs$location_df[nonZero_rowname, ],
        location_period_df = all_dfs$location_period_df[nonZero_rowname, ],
        shapes_df = all_dfs$shapes_df[nonZero_rowname, ],
        observations_df = all_dfs$observations_df[nonZero_rowname, ]
      )
      print("Here, we're testing the scenario with only positive. Test dataset only include positive observations.")
    } else {
      print("Multiple observations collections including zero case observations are applied here.")
    }
    cases <- all_dfs$observations_df$attributes.fields.suspected_cases
  } else {
    print("Using grid cases for this test case")
  }

  ## ------------------------------------------------------------------------------------------------------------------------
  ## Change polygons
  test_extent <- sf::st_bbox(all_dfs$shapes_df)
  test_raster <- create_test_raster(nrows = nrows, ncols = ncols, nlayers = 2, test_extent = test_extent)

  if (!polygon_type %in% "real_polygon") {
    test_polygons <- sf::st_make_valid(create_test_layered_polygons(
      test_raster = test_raster,
      base_number = 1, n_layers = 2, factor = nrows * ncols, snap = FALSE, randomize = FALSE,
      seed = my_seed
    ))
    my_seed <- .GlobalEnv$.Random.seed

    all_dfs$shapes_df <- test_polygons %>%
      dplyr::mutate(
        qualified_name = location, start_date = min(all_dfs$shapes_df$start_date),
        end_date = max(all_dfs$shapes_df$end_date)
      )
    names(all_dfs$shapes_df)[names(all_dfs$shapes_df) == "geometry"] <- "geom"
    sf::st_geometry(all_dfs$shapes_df) <- "geom"

    all_dfs$location_period_df <- all_dfs$shapes_df %>%
      sf::st_drop_geometry()
    all_dfs$location_df <- all_dfs$shapes_df %>%
      sf::st_drop_geometry() %>%
      dplyr::group_by(qualified_name) %>%
      dplyr::summarize()
  } else {
    print("Using real polygon for this test case")
  }

  ## ------------------------------------------------------------------------------------------------------------------------
  ## Change covariates
  test_extent <- sf::st_bbox(all_dfs$shapes_df)
  test_raster <- create_test_raster(nrows = nrows, ncols = ncols, nlayers = 2, test_extent = test_extent)

  # create covarates parameters based on the number of covariates
  spatially_smooth <- c(TRUE, rep(spatially_smooth_nonpop, ncovariates - 1))
  temporally_smooth <- c(FALSE, rep(temporally_smooth_nonpop, ncovariates - 1))
  nonspatial <- rep(nonspatial, ncovariates)
  nontemporal <- rep(nontemporal, ncovariates)
  polygonal <- rep(polygonal, ncovariates)
  radiating <- rep(radiating, ncovariates)

  test_covariates <- create_multiple_test_covariates(
    test_raster = test_raster,
    ncovariates = ncovariates,
    nonspatial = nonspatial,
    nontemporal = nontemporal,
    spatially_smooth = spatially_smooth,
    temporally_smooth = temporally_smooth,
    polygonal = polygonal,
    radiating = radiating,
    seed = my_seed
  )
  my_seed <- .GlobalEnv$.Random.seed
  min_time_left <- query_time_left
  max_time_right <- query_time_right
  covariate_raster_funs <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(
    test_covariates,
    min_time_left,
    max_time_right
  )

  ## ------------------------------------------------------------------------------------------------------------------------
  ## Change observations
  raster_df <- taxdat::convert_test_covariate_funs_to_simulation_covariates(covariate_raster_funs)

  test_underlying_distribution <- create_underlying_distribution(covariates = raster_df, seed = my_seed)
  my_seed <- .GlobalEnv$.Random.seed

  test_observations <- observe_polygons(
    test_polygons = dplyr::mutate(all_dfs$shapes_df, location = qualified_name, geometry = geom), test_covariates = raster_df, underlying_distribution = test_underlying_distribution,
    noise = FALSE, number_draws = 1, grid_proportion_observed = grid_proportion_observed, polygon_proportion_observed = polygon_proportion_observed,
    min_time_left = query_time_left, max_time_right = query_time_right, seed = my_seed
  )
  my_seed <- .GlobalEnv$.Random.seed

  ## ------------------------------------------------------------------------------------------------------------------------
  ## test two observations from different underlying distributions

  if (observation_type %in% "two_tested_observations") {
    ###
    test_covariates2 <- create_multiple_test_covariates(
      test_raster = test_raster,
      ncovariates = ncovariates,
      nonspatial = !nonspatial,
      nontemporal = !nontemporal,
      spatially_smooth = !spatially_smooth,
      temporally_smooth = !temporally_smooth,
      polygonal = !polygonal,
      radiating = !radiating,
      seed = my_seed
    )
    my_seed <- .GlobalEnv$.Random.seed
    covariate_raster_funs2 <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(
      test_covariates2,
      min_time_left,
      max_time_right
    )

    raster_df2 <- taxdat::convert_test_covariate_funs_to_simulation_covariates(covariate_raster_funs2)
    test_underlying_distribution2 <- create_underlying_distribution(covariates = raster_df2, seed = my_seed)
    my_seed <- .GlobalEnv$.Random.seed
    test_observations2 <- observe_polygons(
      test_polygons = dplyr::mutate(all_dfs$shapes_df,
        location = qualified_name, geometry = geom
      ), test_covariates = raster_df2, underlying_distribution = test_underlying_distribution2,
      noise = FALSE, number_draws = 1, grid_proportion_observed = grid_proportion_observed, polygon_proportion_observed = polygon_proportion_observed,
      min_time_left = query_time_left, max_time_right = query_time_right, seed = my_seed
    )
    my_seed <- .GlobalEnv$.Random.seed
    ###
    test_covariates3 <- create_multiple_test_covariates(
      test_raster = test_raster,
      ncovariates = ncovariates,
      nonspatial = nonspatial,
      nontemporal = nontemporal,
      spatially_smooth = !spatially_smooth,
      temporally_smooth = !temporally_smooth,
      polygonal = polygonal,
      radiating = radiating,
      seed = my_seed
    )
    my_seed <- .GlobalEnv$.Random.seed
    covariate_raster_funs3 <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(
      test_covariates3,
      min_time_left,
      max_time_right
    )

    raster_df3 <- taxdat::convert_test_covariate_funs_to_simulation_covariates(covariate_raster_funs3)
    test_underlying_distribution3 <- create_underlying_distribution(covariates = raster_df3, seed = my_seed)
    my_seed <- .GlobalEnv$.Random.seed

    test_observations3 <- observe_polygons(
      test_polygons = dplyr::mutate(all_dfs$shapes_df,
        location = qualified_name, geometry = geom
      ), test_covariates = raster_df3, underlying_distribution = test_underlying_distribution3,
      noise = FALSE, number_draws = 1, grid_proportion_observed = grid_proportion_observed, polygon_proportion_observed = polygon_proportion_observed,
      min_time_left = query_time_left, max_time_right = query_time_right, seed = my_seed
    )
    my_seed <- .GlobalEnv$.Random.seed

    test_observations1 <- test_observations
    test_observations <- dplyr::bind_rows(test_observations1, test_observations2, test_observations3)

    print("Three test observations from three different underlying distributions are applied here (different test covariates are applied to create the underlying distributions).")
  } else {
    print("One test observation from one true underlying distribution is applied here.")
  }

  if (data_type %in% "real_case") {
    if (!length(cases) == nrow(all_dfs_observations_df)) {
      stop("The number of raw observations don't match the number of test observations.")
    }
  }

  all_dfs$observations_df <- test_observations %>%
    dplyr::mutate(
      observation_collection_id = draw,
      time_left = time_left,
      time_right = time_right,
      qualified_name = location,
      primary = TRUE,
      phantom = FALSE,
      suspected_cases = cases,
      deaths = NA, confirmed_cases = NA
    )

  ## ------------------------------------------------------------------------------------------------------------------------
  ## Create Database
  setup_testing_database(conn_pg, drop = TRUE)
  taxdat::setup_testing_database_from_dataframes(conn_pg, all_dfs, covariate_raster_funs)

  ## NOTE: Change me if you want to run the report locally config_filename <-
  ## paste(tempfile(), 'yml', sep = '.')
  config_filename <- "/home/app/cmp/Analysis/R/test_config.yml"

  ## Put your config stuff in here
  config <- list(
    general = list(
      location_name = all_dfs$location_df$qualified_name[[1]],
      start_date = as.character(min_time_left),
      end_date = as.character(max_time_right),
      width_in_km = 1,
      height_in_km = 1,
      time_scale = time_scale
    ),
    stan = list(directory = rprojroot::find_root_file(criterion = ".choldir", "Analysis", "Stan"), ncores = 1, model = model, niter = niter, recompile = TRUE),
    name = "test_???",
    taxonomy = "taxonomy-working/working-entry1",
    smoothing_period = 1, case_definition = "suspected", covariate_choices = raster_df$name,
    data_source = "sql", file_names = list(
      stan_output = rprojroot::find_root_file(criterion = ".choldir", "Analysis", "output", "test.stan_output.rdata"),
      stan_input = rprojroot::find_root_file(criterion = ".choldir", "Analysis", "output", "test.stan_input.rdata")
    )
  )

  yaml::write_yaml(x = config, file = config_filename)

  ## Run the mapping pipeline
  Sys.setenv(CHOLERA_CONFIG = config_filename)
  source(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "R", "execute_pipeline.R"))
  rmarkdown::render(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "output", "country_data_report.Rmd"),
    params = list(config_filename = config_filename, cholera_directory = "~/cmp/", drop_nodata_years = TRUE)
  )
}

run_test_case(temporally_smooth_nonpop = TRUE, spatially_smooth_nonpop = TRUE)
