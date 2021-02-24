
# To avoid any issues with version control, data and functions from glptools are saved here.
# Additionally, some functions developed for the A Path Forward report series are included
#   here to make them easy to share across projects.

# Sae data files
if (FALSE) {
  library(glptools)
  save(map_zip, FIPS_df, MSA_df, MSA_FIPS, file = "raw_data/glptools_exports.RData")
}

`%cols_in%` <- function (df, columns) columns[columns %in% names(df)]
`%not_in%`  <- function (x, table) match(x, table, nomatch = 0L) == 0L

assign_row_join <- function(df_1, df_2){
  tryCatch({
    bind_rows(df_1, df_2)
  },
  error = function(cond){
    df_2
  })
}

assign_col_join <- function(df_1, df_2, by){
  tryCatch({
    full_join(df_1, df_2, by = by)
  },
  error = function(cond){
    df_2
  })
}

pull_peers <- function(df, add_info = F, subset_to_peers = T, geog = "", additional_geogs = "", FIPS_df){

  # If no geography provided, use MSA column. If no MSA column, use FIPS column.
  if (geog == ""){
    if ("MSA" %in% names(df)) geog <- "MSA"
    else if ("FIPS" %in% names(df)) geog <- "FIPS"
  }
  if(geog == "") stop("MSA and FIPS columns are missing from the data frame.")

  # Ensure the Brimingham FIPS code is five digits and that the MSA column is of type character
  if ("MSA" %in% names(df))  df %<>% mutate(MSA = MSA %>% as.character())
  if ("FIPS" %in% names(df)) df %<>% mutate(FIPS = FIPS %>% as.character %>% replace(. == "1073", "01073"))

  # Add information columns
  if (add_info) {
    if      ("MSA" %in% names(df))  df %<>% left_join(MSA_df,  by = "MSA")
    else if ("FIPS" %in% names(df)) df %<>% left_join(FIPS_df, by = "FIPS")
  }

  # subset to peers based on geog
  if(subset_to_peers) {
    if (geog == "FIPS") df %<>% filter(FIPS %in% c(FIPS_df$FIPS, additional_geogs))
    if (geog == "MSA") {
      if ("MSA" %in% names(df)) {
        df %<>% filter(MSA %in% c(MSA_FIPS$MSA, additional_geogs))
      } else {
        df %<>% filter(FIPS %in% c(MSA_FIPS$FIPS, additional_geogs))
      }
    }
    if (geog == "MSA_2012") {
      if ("MSA" %in% names(df)) {
        df %<>% filter(MSA %in% c(MSA_FIPS_2012$MSA, additional_geogs))
      } else {
        df %<>% filter(FIPS %in% c(MSA_FIPS_2012$FIPS, additional_geogs))
      }
    }
  }

  df
}

stl_merge <- function(df, ..., weight_var = "", method = "",
                      other_grouping_vars = "", just_replace_FIPS = F, keep_counties = F){

  weight_var <- as.character(substitute(weight_var))
  variables <- dplyr:::tbl_at_vars(df, vars(...))
  grouping_vars <- c("FIPS", "year", "sex", "race", other_grouping_vars)

  if (just_replace_FIPS) return(df %>% mutate(FIPS = replace(FIPS, FIPS %in% c("29189", "29510"), "MERGED")))

  if (keep_counties) {
    df_stl <- df %>%
      select(any_of(c(grouping_vars, variables))) %>%
      filter(FIPS %in% c("29189", "29510"))
  }

  # For each variable to be weighted, create a new df of the applicable variables
  for(v in variables){

    temp_df <- df %>%
      mutate(FIPS = replace(FIPS, FIPS %in% c("29189", "29510"), "MERGED")) %>%
      group_by(across(any_of(c("FIPS", "year", "sex", "race", other_grouping_vars))))

    if      (method == "mean") temp_df %<>% summarise(!!v := weighted.mean(.data[[v]], .data[[weight_var]]), .groups = "drop")
    else if (method == "max")  temp_df %<>% summarise(!!v := max(.data[[v]], na.rm = TRUE), .groups = "drop")
    else if (method == "min")  temp_df %<>% summarise(!!v := min(.data[[v]], na.rm = TRUE), .groups = "drop")
    else if (method == "sum")  temp_df %<>% summarise(across(v, sum), .groups = "drop")

    output <- assign_col_join(output, temp_df, by = c("FIPS", "year"))

  }

  if (keep_counties) output %<>% bind_rows(df_stl)

  output
}

# Map function
make_map <- function(indicator, title = "", legend = "", caption = "", no_legend = FALSE, vir_dir = 1){
  plt <- ggplot(jfco_sf) +
    geom_sf(aes(fill={{ indicator }} )) +
    # scale_fill_gradient(low = "#323844", high = "#d63631", name = "Percent") +
    scale_fill_viridis(na.value = "grey", name = legend, direction = vir_dir) +
    theme_bw(base_size = 22) +
    theme(plot.caption = element_text(lineheight = .5)) +
    theme(text = element_text(family = "Montserrat"),
          panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          panel.border = element_blank()) +
    labs(title = title,
         caption = caption)

  if (no_legend == TRUE){
    plt <- plt + theme(legend.position = "none")
  }

  plt <- plt +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
      legend.key = element_rect(fill = "transparent",colour = NA))

  return(plt)
}

# Ranking function
ranking <- function(df, var, plot_title = "",
                    year = NULL, sex = "total", race = "total",
                    order = "Descending",
                    y_title = "Percent", caption_text = "", subtitle_text = "",
                    bar_label = TRUE, sigfig = 3, accuracy = 0.1,
                    label_function, alternate_text = NULL,
                    ranking_colors = TRUE, text_size, FIPS_df){

  # Copy variable var to a new column for use with the '$' operator
  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))
  df$var <- df[[var]]
  # Filter to sex, race, and year
  if ("sex" %in% names(df)) df <- df[df$sex == sex,]
  if ("race" %in% names(df)) df <- df[df$race == race,]
  if("year" %in% names(df)) {
    df <- df[df$year %in% year,]
    if (length(year) > 1) {
      df %<>%
        group_by_at(df %cols_in% c("MSA", "FIPS")) %>%
        summarise(var = mean(var, na.rm = TRUE)) %>%
        ungroup()
    }
  }
  # Add peer data if not already present
   if ("city" %not_in% names(df)) {
     df %<>%
       pull_peers(add_info = T, FIPS_df = FIPS_df) %>%
       filter(current == 1)
   }

  # Sort according to order parameter
  if (order %in% c("descending", "Descending")) df %<>% arrange(desc(var))
  if (order %in% c("ascending", "Ascending"))   df %<>% arrange(var)
  df %<>% filter(!is.na(var))
  # Create numbered city labels for left side of graph
  df %<>%
    mutate(
      rank = row_number(),
      names = paste0(rank, ". ", city))
  # Set bar colors
  if (ranking_colors) {
    # color_values <- c("#96ca4f", "#ffd600", "#db2834")
    # color_names <- c("green", "yellow", "red")
    # if (order %in% c("descending", "Descending")) {color_names  = rev(color_names)}
    #
    # breaks <- classInt::classIntervals(na.omit(df$var), 3, style = "jenks")
    # df$color <- NA
    # df$color[df$var <= breaks$brks[2]] <- color_names[1]
    # df$color[df$var > breaks$brks[2] & df$var <= breaks$brks[3]] <- color_names[2]
    # df$color[df$var > breaks$brks[3]] <- color_names[3]

    color_values <- c("#323844", "#d63631")
    color_names <- c("gray", "red")

    df$color <- "red"
    df$color[df$city == "Louisville"] <- "gray"
  } else {
    df$color <- "blue"
    color_values <- "#f58021"
    color_names <- "blue"
  }
  #if (order %in% c("descending", "Descending")) color_values = rev(color_values)
  color_values = rev(color_values)
  # Create numeric labels
  # Create numeric labels
  if (!missing(label_function)) {
    label_text <- df$var %>% signif(sigfig) %>% label_function()
  } else if (y_title == "Dollars") {
    if(mean(df$var, na.rm = TRUE) > 10000) {
      label_text <- df$var %>% signif(sigfig) %>% scales::dollar(accuracy = accuracy, scale = .001, suffix = "k")
    } else {
      label_text <- df$var %>% signif(sigfig) %>% scales::dollar(accuracy = .01)
    }
  } else if (stringr::str_detect(y_title, "Percent")) {
    label_text <- df$var %>% signif(sigfig) %>% scales::percent(accuracy = accuracy, scale = 1, suffix = "%")
  } else {
    label_text <- df$var %>% signif(sigfig) %>% scales::comma(accuracy = accuracy)
  }

  # Set text format, highlight and italicise Louisville text, highlight Louisville bar
  df$textcolor <- "#000000"
  df$textcolor[df$city == "Louisville"] <- "#000000"

  df$textfont <- "Montserrat"
  df$textfont[df$city == "Louisville"] <- "Montserrat Bold"

  label_color_names <- c("white", "black")
  label_color_values <- c("#000000", "#ffffff")

  df$label_color <- "white"
  df$label_color[df$city == "Louisville"] <- "black"
  #df$linecolor <- "#ffffff"
  #df$linecolor[df$city == "Louisville"] <- "#00a9b7"
  df$lou <- if_else(df$city == "Louisville", 1, 0)
  df$text_alignment <- 1.1
  if (!is.null(alternate_text)) df$text_alignment[df$rank %in% alternate_text] <- -0.1
  ### PLOT GRAPH

  # Initial plot
  p <- ggplot(data = df,
              aes(x = factor(names, levels = rev(names)),
                  y = var,
                  fill  = factor(color, levels = color_names, ordered = TRUE)))
  p <- p + guides(fill = FALSE, color = FALSE)
  # Add bars
  p <- p +
    geom_bar(stat = "identity",
             size = text_size) +
    coord_flip() +
    ggthemes::theme_tufte()
  p <- p + scale_fill_manual(values = color_values)
  #p <- p + scale_color_manual(values = c("#ffffff", "#00a9b7"))
  # Add features
  title_scale <- min(1, 48 / nchar(plot_title))
  p <- p + theme(text = element_text(family = "Montserrat"),
                 plot.title = element_text(size = 14 * title_scale * text_size, hjust = 0.5, margin = margin(b = 10, unit = "pt")),
                 axis.text.y = element_text(hjust = 0,
                                            size = 10 * text_size,
                                            color = rev(df$textcolor),
                                            family = rev(df$textfont)),
                 axis.title.x = element_text(size = 10 * text_size),
                 axis.ticks = element_blank(),
                 axis.text.x = element_blank(),
                 plot.caption = element_text(size = 5 * text_size, lineheight = 0.5))
  if(subtitle_text != ""){
    p <- p +
      labs(subtitle = subtitle_text) +
      theme(plot.subtitle = element_text(hjust = 0.5, size = 10 * text_size))

  }
  # Add numeric labels to bars based on bar_label parameter
  if (y_title != "" & bar_label) {
    p <- p + geom_text(aes(label = label_text,
                           hjust = text_alignment,
                           color = factor(label_color),
                           family = textfont),
                       size = 4.5 * text_size) +
      scale_colour_manual(values=c("#000000", "#ffffff"))
  }
  # Add vertical line to the left side of the bars based on the h_line parameter
  if (min(df$var, na.rm = TRUE) < 0) p <- p + geom_hline(yintercept = 0, linetype = "longdash", size = 2)
  # Add remaining text
  p <- p + labs(title = plot_title,
                y = y_title,
                x = "",
                caption = caption_text)

  p <- p +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
      legend.key = element_rect(fill = "transparent",colour = NA))

  p
}

plt_by <- function(df, group_var, var, title_text = "Home Mortgages",
                   y_axis = "Percent", y_min = NA, units = "number",
                   remove_legend_title = F) {

  var <- enquo(var)
  group_var <- enquo(group_var)

  text_scale <- 1

  plt <- ggplot(data = df, aes(x = year, y = !!var, group = !!group_var, colour = !!group_var)) +
    geom_point(size = 2) +
    geom_line(size = .65) +
    theme_bw() +
    labs(title = title_text, x = "Year", y = y_axis) +
    theme(legend.position = "bottom")

  color_pal <- c("#d63631", "#323844", "#eaab21", "#a7bfd7", "#7CE3B6")[1:length(unique(pull(df, !!group_var)))]

  plt <- plt  +
    scale_colour_manual(values = color_pal) +
    #scale_x_continuous(breaks = seq(from = 2007, to = 2019, by = 2)) +
    theme(text = element_text(family = "Montserrat"),

          legend.title     = element_text(size = 30 * text_scale),
          legend.text      = element_text(size = 24 * text_scale,
                                          margin = margin(b = 0.2 * text_scale, t = 0.2 * text_scale, unit = "cm")),

          axis.text    = element_text(size = 24 * text_scale),
          axis.title   = element_text(size = 30 * text_scale),
          axis.title.x = element_text(margin = margin(t = 0.3 * text_scale, unit = "cm")),
          axis.title.y = element_text(margin = margin(r = 0.3 * text_scale, unit = "cm")),

          plot.title = element_text(size = 42 * text_scale,
                                    hjust = .5,
                                    margin = margin(b = 0.4 * text_scale, unit = "cm")))

  if (!is.na(y_min)) {
    plt <- plt + ylim(y_min, NA)
  }

  if (remove_legend_title) plt <- plt + theme(legend.title = element_blank())

  # if(length(unique(pull(df, !!group_var))) >= 4){
  #   p <- p + guides(colour = guide_legend(label.position = "top",
  #                                         keywidth = unit(6 * text_scale, "lines")))
  # }

  if(units == "Dollars") plt <- plt + scale_y_continuous(labels = scales::dollar)
  if(units == "Percent") plt <- plt + scale_y_continuous(labels = function(x) scales::percent(x, accuracy = 1, scale = 1))

  plt <- plt +
    theme(
      panel.background = element_rect(fill = "transparent", color = NA), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      legend.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent", color = "transparent"), # get rid of legend panel bg
      legend.key = element_rect(fill = "transparent",colour = NA)
    )

  plt
}

trend_cc <- function(df, var,
                     plot_title = "", y_title = "", caption_text = "", subtitle_text = "",
                     ylimits = "", pctiles = T, shading = F, xmin = 2010, xmax = 2019,
                     label_function = NULL, axis_function = NULL, year_breaks = NULL){

  var <- dplyr:::tbl_at_vars(df, vars(!!enquo(var)))

  if (length(var) == 1) df$var <- df[[var]]
  geog <- if_else("MSA" %in% names(df), "MSA", "FIPS")

  #reduce size of dataframe
  df <- df %>%
    select(any_of(c("year", geog, "var")))

  df_wol <- df %>% filter(across(geog, ~ . %not_in% c("31140", "21111")))
  lville <- df %>% filter(across(geog, ~ . %in% c("31140", "21111")))

  #  Group the data set by year, category, if avilable.
  #  Calculate the 25th percentile, 75th percentile,
  #   and mean of the peer data set.
  df_wol %<>%
    group_by_at(df %cols_in% c("year")) %>%
    summarise(q1 = quantile(var, prob = 0.25, na.rm = TRUE),
              mean = mean(var, na.rm = TRUE),
              q3 = quantile(var, prob = 0.75, na.rm = TRUE))

  # Lville data frame contains three columns
  lville %<>% rename(lou = var)

  #join 25th percentile, 75th percentile, and Louisville values
  df <- full_join(lville, df_wol, by = df %cols_in% c("year", "category"))

  #Reshape the data to long format
  df %<>% gather(lou, q1, mean, q3, key = "variable", value = "value")

  #No year breaks in this case

  #Line settings for the graph
  df$style_group <- df$variable

  df %<>% mutate(line_group = factor(style_group))

  # Label and factor style_group with the order based on cat_names.
  cat_levels <- ""
  cat_labels <- ""

  cat_levels <- rep(cat_levels, each = 2)

  var_levels <- c(paste0(cat_levels, c("lou", "mean")),
                  paste0(cat_levels, c("q1", "q3")))

  peer_group <- c("Louisville", "Peer Mean")

  peer_group <- rep(peer_group, length(cat_labels))
  cat_labels  <- rep(cat_labels, each = 2)

  var_labels <- c(paste0(cat_labels, peer_group),
                  rep("25th and 75th Percentiles", length(cat_labels)))

  df$style_group <- factor(df$style_group,
                           levels = var_levels,
                           labels = var_labels,
                           ordered = TRUE)

  #Initial plot
  p <- ggplot(data = df,
              aes(x = year, y = value,
                  group = line_group,
                  colour = style_group,
                  linetype = style_group,
                  alpha = style_group,
                  label = value))

  txt_scale <- 1

  p <- p +
    geom_point(size = 2 * txt_scale) +
    geom_line(size = 1  * txt_scale)

  border_space = (max(df$value, na.rm = TRUE) - min(df$value, na.rm = TRUE)) * 0.1

  ylimits <- c(min(df$value, na.rm = TRUE) - border_space,
               max(df$value, na.rm = TRUE) + border_space)

  y_axis_height <- ylimits[2] - ylimits[1]

  # Create data endpoint labels
  df_label <- df %>% filter(year == xmax)

  if (y_title == "Dollars") {
    label_text <- df_label$value %>% dollar(accuracy = 0.01,)
    axis_format <-            dollar_format(accuracy = 0.01)
  } else if (y_title == "Percent") {
    label_text <- df_label$value %>% percent(accuracy = 0.1, scale = 1, suffix = "%")
    axis_format <-            percent_format(accuracy = 1,   scale = 1, suffix = "%")
  }

  label_length <- max(nchar(label_text))

  xmax_adjustment <- 0.01 + 0.0125 * label_length

  p <- p +
    scale_x_continuous(
      limits = c(xmin, xmax + (xmax - xmin) * xmax_adjustment),
      breaks = seq(xmin + 1, xmax, 2),
      minor_breaks = waiver()) +
    scale_y_continuous(
      limits = ylimits,
      breaks = pretty_breaks(),
      labels = axis_format)

  p <- p +
    geom_text_repel(
      data = df_label,
      aes(label = label_text),
      xlim = c(xmax + (xmax - xmin) * .01, xmax + (xmax - xmin) * xmax_adjustment),
      size = 8,
      hjust = 0,
      alpha = 1,
      segment.alpha = 0,
      family = "Museo Sans 300",
      show.legend = FALSE)

  txt_scale <- 2
  title_scale <- min(1, 48 / nchar(plot_title))

  #adjust theme
  p <- p + theme_bw(
    base_size = 5 * txt_scale)

  p <- p + theme(
    text = element_text(family = "Montserrat"),
    legend.title     = element_blank(),
    legend.position  = "top",
    legend.margin    = margin(t = 0.2 * txt_scale, unit = "cm"),
    legend.spacing.x = unit(0.4 * txt_scale, "cm"),
    legend.text      = element_text(size = 12 * txt_scale,
                                    margin = margin(b = 0.1 * txt_scale, t = 0.2 * txt_scale, unit = "cm")),

    axis.text    = element_text(size = 10 * txt_scale),
    axis.title   = element_text(size = 12 * txt_scale),
    axis.title.x = element_text(margin = margin(t = 0.3 * txt_scale, unit = "cm")),
    axis.title.y = element_text(margin = margin(r = 0.3 * txt_scale, unit = "cm")),

    plot.title = element_text(size = 18 * txt_scale * title_scale,
                              hjust = .5),

    plot.caption = element_text(size = 5 * txt_scale,
                                lineheight = 0.5))


  #add labels
  p <- p + labs(
    title   = plot_title,
    x       = "Year",
    y       = "Dollars",
    caption = "Data from Bureau of Labor Statistics")

  p <- p + guides(colour = guide_legend(label.position = "top"))

  #Extract stlyle labels to match setting with legend
  line_types <- levels(df$style_group)

  line_col   <- c("#00a9b7", "black", "grey50")

  #set line type
  line_type  <- rep(c("solid", "dashed"))
  line_type <- c(line_type, "dashed")

  #set line alphas. If shading is used, remove percentile lines
  line_alpha <- rep(c(1, 0.7))
  line_alpha <- c(line_alpha, 0.7)

  p <- p +
    scale_colour_manual(
      values = line_col,
      labels = line_types) +
    scale_linetype_manual(
      values = line_type,
      labels = line_types) +
    scale_alpha_manual(
      values = line_alpha,
      labels = line_types)

  p
}
