## Basic test setup starting from real data
library(taxdat)
library(sf)

dbuser <- Sys.getenv("USER", "app")
dbname <- Sys.getenv("CHOLERA_COVAR_DBNAME", "cholera_covariates")

conn_pg <- taxdat::connect_to_db(dbuser, dbname)
DBI::dbClearResult(DBI::dbSendQuery(conn = conn_pg, "SET client_min_messages TO WARNING;"))

my_seed <- c(
  10404, 625, 105045778, 1207077739, 2042172336, -219892751, -768060162,
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
cov3_seed <- c(
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
  578785205, -1440267022, -2065399861, 2059230064, -1119283375, 1627900126, -515039289,
  1965460828, 1382604461, 804657546, -15234435, -39796504, 74334665, 1735058166,
  554974463, -1051931852, 2080660645, 455634654, -1004539333, -1625491392, -619836607,
  739397646, -2000105161, -1935256564, -1736567197, 1703021498, -277490189, -1825789288,
  -905555015, 1829400614, 1228127343, 76642332, 2085447061, -575149742, -865009493,
  1949382640, -1020200143, -970218690, 1954947417, -1081293124, 1351528589, 1908902378,
  -927706525, 855113800, 1759764905, -1922713942, -459532321, 370438292, 682207877,
  -670347902, -511792357, -1405459552, -1165027105, 1044824686, 955729943, 1959060844,
  1723741053, 577344538, 2133519059, 300039688, -224007271, -1486000506, 264523599,
  -410417852, -366466187, -1210794574, 1062211189, 766737232, 827117329, 806792606,
  1452805511, 502902812, 1593799277, -2110466634, 924884803, 1819865000, -257199735,
  -543760458, -1797037377, 685295092, -2107107429, 836188642, 288084987, -841515264,
  1174647553, -200383282, -1888276745, 409590068, 973172061, -217101190, -1231261773,
  -1666938536, 1410560889, -1008100122, -1542476335, 634204836, -477634219, -74071534,
  -740732309, 10579632, 580162289, -815732738, -463491993, 1757759868, 875766861,
  -1978074454, -714777565, 1677430536, -1706881385, 33072662, -626909793, 1827785556,
  1346836037, -78424510, -748720933, 1999877536, 901672673, 604059438, 469009879,
  1399986220, -1245472963, 327890138, 318610285, 1318650040, -1236866215, 2081880902,
  -2073049841, 2140246020, 467961653, 2014508658, 337978187, 668144144, -1375713583,
  -1599974818, 847821639, 608670428, -1117898285, 1489566474, 1220184323, 1062723176,
  235954505, 2035784822, 982534271, 2118680396, 1305529307, 1234674338, 1146189243,
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

sim_time_left <- lubridate::ymd("2000-01-01")
sim_time_right <- lubridate::ymd("2001-12-31")
query_time_left <- c(lubridate::ymd("2000-01-01"), lubridate::ymd("2000-01-01"), lubridate::ymd("2000-01-01"))
query_time_right <- c(lubridate::ymd("2000-05-30"), lubridate::ymd("2001-05-30"), lubridate::ymd("2001-12-31"))
## Pull data frames needed to create testing database from the api This doesn't
## pull covariates, but does pull everything else tryCatch({ all_dfs <-
## taxdat::create_testing_dfs_from_api( username
## =Sys.getenv('CHOLERA_API_USERNAME'), api_key =
## Sys.getenv('CHOLERA_API_KEY'), locations = 'AFR::KEN', time_left =
## query_time_left, time_right = query_time_right, uids = NULL, website =
## 'https://api.cholera-taxonomy.middle-distance.com/' ) }, error = function(e)
## { })
load(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "all_dfs_object.rdata"))

## ------------------------------------------------------------------------------------------------------------------------
## Change polygons
test_extent <- sf::st_bbox(all_dfs$shapes_df)
test_raster <- create_test_raster(nrows = 10, ncols = 10, nlayers = 8, test_extent = test_extent)
test_polygons <- sf::st_make_valid(create_test_layered_polygons(
  test_raster = test_raster,
  base_number = 1, n_layers = 2, factor = 10 * 10, snap = FALSE, randomize = TRUE,
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

## ------------------------------------------------------------------------------------------------------------------------
## Change covariates
covariates_table <- data.frame(
  nonspatial = c(FALSE, FALSE, TRUE), nontemporal = c(
    FALSE,
    FALSE, FALSE
  ), spatially_smooth = c(TRUE, TRUE, TRUE), temporally_smooth = c(
    FALSE,
    FALSE, FALSE
  ), polygonal = c(TRUE, TRUE, TRUE), radiating = c(FALSE, FALSE, FALSE),
  constant = c(TRUE, FALSE, FALSE), Data_simulation_covariates = c(
    TRUE, TRUE,
    TRUE
  ), Model_covariates = c(TRUE, TRUE, FALSE)
)

test_covariates <- create_multiple_test_covariates(
  test_raster = test_raster, ncovariates = 2,
  seed = my_seed
)
my_seed <- .GlobalEnv$.Random.seed

test_extent <- sf::st_bbox(all_dfs$shapes_df)
test_raster <- create_test_raster(nrows = 10, ncols = 10, nlayers = 8, test_extent = test_extent)
test_covariates <- create_multiple_test_covariates(
  test_raster = test_raster, ncovariates = 2,
  nonspatial = covariates_table$nonspatial[1:2], nontemporal = covariates_table$nontemporal[1:2],
  spatially_smooth = covariates_table$spatially_smooth[1:2], temporally_smooth = covariates_table$temporally_smooth[1:2],
  polygonal = covariates_table$polygonal[1:2], radiating = covariates_table$radiating[1:2],
  constant = covariates_table$constant[1:2], seed = my_seed
)
my_seed <- .GlobalEnv$.Random.seed

min_time_left <- sim_time_left
max_time_right <- sim_time_right
covariate_raster_funs <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(
  test_covariates,
  min_time_left, max_time_right
)

# covariates that make the observations
test_raster_observation <- create_test_raster(
  nrows = 10, ncols = 10, nlayers = 8,
  test_extent = test_extent
)

test_covariates_observation <- create_multiple_test_covariates(
  test_raster = test_raster_observation,
  ncovariates = 2, nonspatial = covariates_table$nonspatial[1:2], nontemporal = covariates_table$nontemporal[1:2],
  spatially_smooth = covariates_table$spatially_smooth[1:2], temporally_smooth = covariates_table$temporally_smooth[1:2],
  polygonal = covariates_table$polygonal[1:2], radiating = covariates_table$radiating[1:2],
  constant = covariates_table$constant[1:2], seed = my_seed
)
my_seed <- .GlobalEnv$.Random.seed

test_covariates3_observation <- create_multiple_test_covariates(
  test_raster = test_raster_observation,
  ncovariates = 2, nonspatial = covariates_table$nonspatial[c(1, 3)], nontemporal = covariates_table$nontemporal[c(
    1,
    3
  )], spatially_smooth = covariates_table$spatially_smooth[c(1, 3)], temporally_smooth = covariates_table$temporally_smooth[c(
    1,
    3
  )], polygonal = covariates_table$polygonal[c(1, 3)], radiating = covariates_table$radiating[c(
    1,
    3
  )], constant = covariates_table$constant[c(1, 3)], seed = cov3_seed
)

test_covariates_observation_final <- test_covariates_observation
test_covariates_observation_final[[3]] <- test_covariates3_observation[[2]]

min_time_left <- sim_time_left
max_time_right <- sim_time_right
covariate_raster_funs_observation <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(
  test_covariates_observation,
  min_time_left, max_time_right
)
covariate3_raster_funs_observation <- taxdat:::convert_simulated_covariates_to_test_covariate_funs(
  test_covariates3_observation,
  min_time_left, max_time_right
)

for (layer in seq_len(8)) {
  covariate3_raster_funs_observation[[layer + 8]]$name <- "covariate3"
  covariate_raster_funs_observation[[layer + 2 * 8]] <- covariate3_raster_funs_observation[[layer + 8]]
}

## save additional covariates in the data generation process for country data
## report
saveRDS(test_covariates_observation_final, "/home/app/cmp/Analysis/output/test_case_42_data_simulation_covariates.rdata")

## ------------------------------------------------------------------------------------------------------------------------
## Change observations
raster_df <- taxdat::convert_test_covariate_funs_to_simulation_covariates(covariate_raster_funs_observation)

test_underlying_distribution <- create_underlying_distribution(
  covariates = raster_df,
  seed = my_seed
)
my_seed <- .GlobalEnv$.Random.seed

test_observations <- list()
for (time_index in seq_len(length(query_time_left))) {
  test_observations[[time_index]] <- observe_polygons(
    test_polygons = dplyr::mutate(all_dfs$shapes_df,
      location = qualified_name, geometry = geom
    ), test_covariates = raster_df, underlying_distribution = test_underlying_distribution,
    noise = FALSE, number_draws = 1, grid_proportion_observed = 1, polygon_proportion_observed = 1,
    min_time_left = sim_time_left, max_time_right = sim_time_right, observation_time_left = query_time_left[[time_index]], observation_time_right = query_time_right[[time_index]], seed = my_seed
  )
}
test_observations <- do.call(what = dplyr::bind_rows, test_observations)
my_seed <- .GlobalEnv$.Random.seed

all_dfs$observations_df <- test_observations %>%
  dplyr::mutate(
    observation_collection_id = draw, time_left = time_left, time_right = time_right,
    qualified_name = location, primary = TRUE, phantom = FALSE, suspected_cases = cases,
    deaths = NA, confirmed_cases = NA
  )

# overlapping observations with consistent case counts
all_dfs$observations_df[which(all_dfs$observations_df$qualified_name == "1"), ]$cases <- all_dfs$observations_df[which(all_dfs$observations_df$qualified_name == "1"), ]$cases
# partially covered for certain polygons
all_dfs$observations_df <- all_dfs$observations_df %>%
  subset(!qualified_name %in% c("1::2", "1::10", "1::20", "1::100", "1::78", "1::5", "1::35", "1::98", "1::92", "1::87"))

test_true_grid_cases <- test_underlying_distribution$mean
# label grids that is observed
observed_polygon_id <- c(unique(data.frame(sf::st_join(st_centroid(test_true_grid_cases), sf::st_as_sf(all_dfs$observations_df))) %>% subset(is.na(location) == F) %>% subset(!qualified_name == "1") %>% dplyr::select(id)))
observed_test_true_grid_cases <- test_true_grid_cases %>% subset(id %in% observed_polygon_id$id)
test_true_grid_cases <- test_true_grid_cases %>% mutate(observed = ifelse(id %in% observed_polygon_id$id, "Observed grid cells", "Unobserved grid cells"))

saveRDS(test_true_grid_cases, "/home/app/cmp/Analysis/output/test_case_42_true_grid_cases.rdata")

## ------------------------------------------------------------------------------------------------------------------------
## Create Database
setup_testing_database(conn_pg, drop = TRUE)
taxdat::setup_testing_database_from_dataframes(conn_pg, all_dfs, covariate_raster_funs)

## NOTE: Change me if you want to run the report locally config_filename <-
## paste(tempfile(), 'yml', sep = '.')
config_filename <- "/home/app/cmp/Analysis/R/config_test_case_42.yml"

## Put your config stuff in here
config <- list(general = list(
  location_name = all_dfs$location_df$qualified_name[[1]],
  start_date = as.character(min_time_left), end_date = as.character(max_time_right),
  width_in_km = 1, height_in_km = 1, time_scale = "year", covariates = unique(sapply(
    covariate_raster_funs,
    function(x) {
      x$name
    }
  ))
), stan = list(
  directory = rprojroot::find_root_file(
    criterion = ".choldir",
    "Analysis", "Stan"
  ), ncores = 4, model = "dagar_seasonal_flexible.stan", niter = 4000,
  recompile = TRUE
), file_names = list(stan_input = rprojroot::find_root_file(
  criterion = ".choldir",
  "Analysis", "output", "test42.stan_input.rdata"
), stan_output = rprojroot::find_root_file(
  criterion = ".choldir",
  "Analysis", "output", "test42.stan_output.rds"
)), test_metadata = list(
  name = "test_42",
  nrows = 10, ncols = 10, data_type = "Grid data", oc_type = "-", polygon_type = "Fake polygon",
  polygon_coverage = "90%", randomize = TRUE, ncovariates = nrow(covariates_table),
  single_year_run = ifelse(lubridate::year(query_time_right) - lubridate::year(query_time_left) ==
    0, "yes", "no"), nonspatial = covariates_table$nonspatial, nontemporal = covariates_table$nontemporal,
  spatially_smooth = covariates_table$spatially_smooth, temporally_smooth = covariates_table$temporally_smooth,
  polygonal = covariates_table$polygonal, radiating = covariates_table$radiating,
  constant = covariates_table$constant, Data_simulation_covariates = covariates_table$Data_simulation_covariates,
  Model_covariates = covariates_table$Model_covariates, Observations_with_inconsistent_data = paste0(
    "Nationally reported data is 3 times of the cases reported at the subnational level."
  ),
  Loc_with_inconsistent_data = "-", Cov_data_simulation_filename = "/home/app/cmp/Analysis/output/test_case_42_data_simulation_covariates.rdata", test_true_grid_case_filename = "/home/app/cmp/Analysis/output/test_case_42_true_grid_cases.rdata"
))

yaml::write_yaml(x = config, file = config_filename)

Sys.setenv(CHOLERA_CONFIG = config_filename)
source(rprojroot::find_root_file(criterion = ".choldir", "Analysis", "R", "execute_pipeline.R"))
rmarkdown::render(
  rprojroot::find_root_file(
    criterion = ".choldir", "Analysis", "output",
    "country_data_report.Rmd"
  ),
  params = list(
    cholera_directory = "~/cmp/",
    config = config_filename,
    drop_nodata_years = TRUE
  ),
  output_file = "test_case_42_country_data_report"
)
