# plot_agebreakdown(pne6, plot_type = "pie")
# plot_agebreakdown(pne6, plot_type = "waffle")
# plot_agebreakdown(pne6, plot_type = "bar", include_legend = FALSE)
# plot_agebreakdown(pne6, plot_type = "tree", include_legend = FALSE)


#' Category charts from computed tables
#'
#' @param x A table with a group column 'text' and a count column 'numeric' or 'integer' in that order.
#' @param plot_type One of four optins. Pie Chart 'pie', Bar Chart 'bar', Treemap 'tree', Waffle 'waffle'
#' @param legend_location Can be one of "none", "bottom", "right", "left". Defaults to none.
#' @param chart_colors A vector of colors to use with the category chart.  Defaults to NULL. If NULL a six color pallete is used.
#' @param include_pie_numbers Whether to include the numbers within the slices of the pie chart. Defaults to FALSE.
#' @param include_bar_text Whether to include the category text within the bar. Defaults to TRUE
#' @param waffle_cols The number of columns of the waffle chart.
#' @param percent Whether to the categories display counts or percent.
#' @import ggplot2
#' @importFrom forcats fct_reorder
#' @import treemapify
#' @import ggfittext
#' @import dplyr
#' @export
plot_category <- function(x, plot_type = "pie", legend_location = "none", chart_colors = NULL,
                          include_pie_numbers = FALSE, include_bar_text = TRUE,
                          waffle_cols = 12, percent = FALSE) {

  if (is.null(chart_colors)) chart_colors <- c("#4c98a4","#938595","#405b9f", "#5494c8", "#1e73cb", "#73849e")

  colnames(x) <- c("group", "n")

  d1 <- dplyr::ungroup(x)

  if(nrow(d1) > length(chart_colors)) stop("Including more groups than colors. If chart_colors is NULL then 6 is the limit")

  if (!any(plot_type %in% c("pie", "bar", "tree", "waffle"))) stop("Expecting one of 'pie', 'bar', 'treemap' or 'waffle'")

  if (plot_type == "pie") {

    p_out <- plot_pie(d1, chart_colors = chart_colors, legend_location = legend_location,
                      include_pie_numbers = include_pie_numbers, percent = percent)

  } else if (plot_type == "bar" ) {

    p_out <- plot_bar(d1, chart_colors = chart_colors, legend_location = legend_location,
                      include_bar_text = include_bar_text, percent = percent)

  } else if (plot_type == "tree") {

    p_out <- plot_tree(d1, chart_colors = chart_colors,
                       legend_location = legend_location, percent = percent)

  } else if (plot_type == "waffle") {

    p_out <- plot_waffle(d1, chart_colors = chart_colors, legend_location = legend_location,
                         waffle_cols = waffle_cols, percent = percent)

  }

  p_out

}


#' Pie charts from computed tables
#' @param x A table with a group column 'text' and a count column 'numeric' or 'integer' in that order.
#' @param legend_location Can be one of "none", "bottom", "right", "left". Defaults to none.
#' @param chart_colors A vector of colors to use with the category chart.
#' @param include_pie_numbers Whether to include the numbers within the slices of the pie chart. Defaults to FALSE.
#' @param percent Whether to the categories display counts or percent.
#' @export
plot_pie <- function(x, chart_colors, legend_location,
                     include_pie_numbers = FALSE, percent = FALSE) {

  x <- x %>%
    dplyr::ungroup(x) %>%
    dplyr::mutate(percent = round(100*n/sum(n),0),
                  percent_label = dplyr::case_when(percent <= 2 ~ "" ,
                                                   percent <= 5 ~ paste0(percent),
                                                   TRUE ~ paste0(percent, "%")),
                  group = forcats::fct_reorder(group, n, .desc = TRUE))

  if (!percent) {
    p_out <-  ggplot2::ggplot(x, ggplot2::aes(x = "", y = n, fill = group))

  } else {
    p_out <-  ggplot2::ggplot(x, ggplot2::aes(x = "", y = percent, fill = group))
  }


  p_out <- p_out +
    ggplot2::geom_col(position = "fill") +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::scale_fill_manual(values = chart_colors) +
    ggplot2::labs(fill = "") +
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size=14, face="bold"),
      legend.position = legend_location)

  if (include_pie_numbers) {

    if (!percent) {
      p_out <- p_out +
        ggplot2::geom_text(ggplot2::aes(label = n), color = "white",
                           position = ggplot2::position_fill(vjust = 0.5))
    } else {
      p_out <- p_out +
        ggplot2::geom_text(ggplot2::aes(label = percent_label), color = "white",
                           position = ggplot2::position_fill(vjust = 0.5))
    }
  } # end include_pie_number if

  p_out

}

#' Bar chart from a computed tables
#' @param x A table with a group column 'text' and a count column 'numeric' or 'integer' in that order.
#' @param legend_location Can be one of "none", "bottom", "right", "left". Defaults to none.
#' @param chart_colors A vector of colors to use with the category chart.
#' @param include_bar_text Whether to include the category text within the bar. Defaults to TRUE
#' @param percent Whether to the categories display counts or percent.
#' @export
plot_bar <- function(x, chart_colors, legend_location,
                     include_bar_text = FALSE, percent = FALSE) {

  x <- x %>%
    dplyr::ungroup(x) %>%
    dplyr::mutate(percent = round(100*n/sum(n),0),
                  percent_label = dplyr::case_when(percent <= 2 ~ "" ,
                                                   percent <= 5 ~ paste0(percent),
                                                   TRUE ~ paste0(percent, "%")),
                  group = forcats::fct_reorder(group, n, .desc = TRUE))

  # https://github.com/wilkox/ggfittext
  if (!percent) {
    p_out <-  ggplot2::ggplot(x, ggplot2::aes(x = group, y = n,
                                              fill = group, label = group)) +
      ggplot2::labs(fill = "", y = "Count")

  } else {
    p_out <-  ggplot2::ggplot(x, ggplot2::aes(x = group, y = percent,
                                              fill = group, label = group)) +
      ggplot2::labs(fill = "", y = "Percentage")
  }


  p_out <- p_out +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = chart_colors) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow=2,byrow=TRUE)) +
    ggplot2::theme_minimal()+
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x =  ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(size=14, face="bold"),
      legend.position = legend_location)

  if (include_bar_text) {
    p_out <- p_out +
      ggfittext::geom_bar_text(reflow = TRUE, contrast = TRUE)

  }

  p_out

}


#' Treemap chart from a computed tables
#' @param x A table with a group column 'text' and a count column 'numeric' or 'integer' in that order.
#' @param legend_location Can be one of "none", "bottom", "right", "left". Defaults to none.
#' @param chart_colors A vector of colors to use with the category chart.  Defaults to NULL. If NULL a six color pallete is used.
#' @param percent Whether to the categories display counts or percent.
#' @export
plot_tree <- function(x, chart_colors, legend_location, percent = FALSE) {

  x <- x %>%
    dplyr::ungroup(x) %>%
    dplyr::mutate(percent = round(100*n/sum(n),0),
                  percent_label = dplyr::case_when(percent <= 2 ~ "" ,
                                                   percent <= 5 ~ paste0(percent),
                                                   TRUE ~ paste0(percent, "%")),
                  group = forcats::fct_reorder(group, n, .desc = TRUE))


  # https://github.com/wilkox/treemapify
  # https://github.com/wilkox/ggfittext
  if (!percent) {
    p_out <-  ggplot2::ggplot(x, ggplot2::aes(area = percent, fill = factor(percent),
                                              label = paste0(group,"\n", n)))

  } else {
    p_out <-  ggplot2::ggplot(x, ggplot2::aes(area = percent, fill = factor(percent),
                                              label = paste0(group,"\n", percent, "%")))
  }


  p_out <- p_out +
    treemapify::geom_treemap(color = "black") +
    treemapify::geom_treemap_text(fontface = "italic", place = "centre", reflow = TRUE) +
    ggplot2::scale_fill_manual(values = piecolors) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow=2,byrow=TRUE)) +
    ggplot2::labs(fill = "Count") +
    ggplot2::theme(plot.title = ggplot2::element_text(size=14, face="bold"),
                   legend.position = legend_location) +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 1))

  p_out

}

#' Treemap chart from a computed tables
#' @param x A table with a group column 'text' and a count column 'numeric' or 'integer' in that order.
#' @param legend_location Can be one of "none", "bottom", "right", "left". Defaults to none.
#' @param chart_colors A vector of colors to use with the category chart.  Defaults to NULL. If NULL a six color pallete is used.
#' @param waffle_cols The number of columns of the waffle chart.
#' @param percent Whether to the categories display counts or percent.
#' @export
plot_waffle <- function(x, chart_colors, legend_location, waffle_cols, percent = FALSE) {

  # https://github.com/liamgilbey/ggwaffle
  x <- x %>%
    dplyr::ungroup(x) %>%
    dplyr::mutate(percent = round(100*n/sum(n),0),
                  percent_label = dplyr::case_when(percent <= 2 ~ "" ,
                                                   percent <= 5 ~ paste0(percent),
                                                   TRUE ~ paste0(percent, "%")))


  total <- sum(x$n)
  col_use <- waffle_cols
  rows_use <- ceiling(total / col_use)

  p_out <- x %>%
    dplyr::group_by(group) %>%
    tidyr::expand(count = seq(1:n)) %>%
    dplyr::mutate(n = c(max(count), rep(NA, dplyr::n()-1)),
                  sort = max(count)) %>%
    dplyr::arrange(desc(sort)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = rep(1:col_use, each = rows_use)[1:dplyr::n()],
                  y = rep(1:rows_use, col_use)[1:dplyr::n()],
                  group = forcats::fct_reorder(group, sort, .desc = TRUE)) %>%
    ggplot2::ggplot(ggplot2::aes(x, y, fill = group)) +
    ggwaffle::geom_waffle() +
    ggplot2::coord_equal() +
    ggplot2::scale_fill_manual(values = chart_colors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.grid =  ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(size=14, face="bold"),
                   legend.position = legend_location) +
    ggplot2::labs(fill = "", x = "", y = "" )

  p_out

}
