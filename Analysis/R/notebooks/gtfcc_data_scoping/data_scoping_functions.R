# --------------------------------------------------------------------------------
# Purpose: Functions for plotting data in database scoping Rmd file
# --------------------------------------------------------------------------------


# Get region the data is coming from
get_region <- function(loc_name) {
  reg <- substr(loc_name, 1, 4)
  reg <- gsub("[^[:alnum:]=\\.]", "", reg)
  return(reg)
}

# get geography group name
get_geo_name <- function(lvl) {
  # some checks
  if (!is.numeric(lvl)) stop("Geography must be numeric")
  if (lvl < 0 | lvl > 99) stop("Geography must be a number between 0 and 99")
  # return appropriate level
  if (lvl == 99) {
    return("Level ?")
  }
  if (lvl == 0) {
    return("Country-level")
  }
  if (lvl == 1) {
    return("Level 1")
  }
  if (lvl >= 2 & lvl < 99) {
    return("Level 2+")
  }
}

# make a pretty table
pretty_table <- function(data, num_row = 4) {
  kable(data) %>%
    # simple striped, bordered table
    kable_styling(
      bootstrap_options = c("bordered"),
      position = "left", full_width = FALSE
    ) %>%
    # merge the regions
    collapse_rows(columns = 1) %>%
    # rows are black
    row_spec(1:num_row, color = "black") %>%
    # return table
    return()
}

# make a prettier table
prettier_table <- function(data) {
  data %>%
    # set up conditional formatting
    mutate_all(~ cell_spec(.x, background = ifelse(is.na(.x), "#FFFF66",
      ifelse(.x == 0, "#CC99CC",
        ifelse(is.character(.x), "white", "#80CDC1")
      )
    ))) %>%
    # make table with informative title and headers
    kable(escape = F, col.names = gsub("National_|Subnational_", "", names(data))) %>%
    # striped, bordered table
    kable_styling(
      bootstrap_options = c("bordered", "condensed"), fixed_thead = TRUE,
      position = "left", full_width = TRUE, font = 10
    ) %>%
    # rows are black
    row_spec(1:nrow(data), color = "black") %>%
    # add header
    add_header_above(c(
      " " = 1, " " = 1,
      "National" = length(grep("National", names(data))),
      "Subnational" = length(grep("Subnational", names(data)))
    )) %>%
    # add some helpful borders
    column_spec(2, border_right = TRUE) %>%
    column_spec(max(grep("National", names(data))), border_right = TRUE) %>%
    # group by regions
    pack_rows(index = table(data$region)) %>%
    # make it scroll
    scroll_box(width = "100%", height = "500px")
}

# make a pretty graph
pretty_graph <- function(data, colors, color_names) {
  # increase font size
  theme_set(theme_gray(base_size = 14))
  # stacked bar chart
  ggplot(data = data, aes(x = region, y = proportion, fill = data_type)) +
    scale_fill_manual("Data type", values = colors, labels = color_names) +
    geom_bar(stat = "identity") +
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size = 14)
    ) +
    facet_grid(~group, scales = "free_x", space = "free_x") +
    xlab("Region") +
    ylab("Proportion of observations")
}

# national data scoping plots
data_map <- function(dt_plot, ad, indicator, indicator_name) {
  # subset to gegraphic level
  dt_plot <- dt_plot[geo_group == ad]
  # add to shapefile
  dt_shp <- merge(shp, dt_plot, by.x = "ISO_A3", by.y = "country", all = T)
  # plot
  myplot <-
    ggplot(data = dt_shp) +
    geom_sf(colour = "grey20", aes(fill = get(indicator)), show.legend = T) +
    coord_sf(xlim = c(-112, 135), ylim = c(-35, 62)) +
    ggtitle(unique(dt_plot$group)) +
    theme(
      text = element_text(size = 14),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      rect = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
    ) +
    if (grepl("prop", indicator)) {
      scale_fill_viridis_c(indicator_name, na.value = "grey80", limits = c(0, 1))
    } else {
      scale_fill_viridis_c(indicator_name,
        na.value = "grey80",
        option = "plasma", trans = "sqrt"
      )
    }
  return(myplot)
}
