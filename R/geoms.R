
#' @importFrom ggplot2 ggproto Geom draw_key_point
#' @importFrom grid gpar pointsGrob polyLineGrob unit
#'
GeomTimeline <-
    ggplot2::ggproto(
        `_class` = "GeomTimeline",
        `_inherit` = ggplot2::Geom,
        required_aes = c("x"),
        default_aes = ggplot2::aes(
            y = 0.20,
            colour = "skyblue",
            fill = "skyblue",
            size = 1,
            alpha = 0.25,
            shape = 21,
            stroke = 0.5
        ),
        draw_key = ggplot2::draw_key_point,
        draw_panel = function(data, panel_scales, coord) {
            coords <- coord$transform(data, panel_scales)

            points <- grid::pointsGrob(
                x = coords$x,
                y = coords$y,
                pch = coords$shape,
                size = grid::unit(coords$size * 0.5, "char"),
                gp = grid::gpar(
                    fill = coords$fill,
                    colour = coords$fill,
                    alpha = coords$alpha
                )
            )

            y_lines <- unique(coords$y)

            line <- grid::polylineGrob(
                x = grid::unit(rep(c(0, 1), each = length(y_lines)), "npc"),
                y = grid::unit(c(y_lines, y_lines), "npc"),
                id = rep(seq_along(y_lines), 2),
                gp = grid::gpar(
                    col = "grey",
                    lwd = 1.5
                )
            )

            grid::gList(line, points)

        }
    )


#' Timeline charts
#'
#' This geom creates a timeline of events and adds it to a plot.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes} or \code{aes_}.
#' @param data The data to be displayed.  If specified and \code{inherit.aes = TRUE}
#' (the default), it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#' If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA,
#' the default, includes if any aesthetics are mapped. FALSE never includes,
#' and TRUE always includes.
#' @param ... other arguments passed on to layer.
#'
#' @return A GeomTimeline layer
#' @export
#'
#' @importFrom ggplot2 layer
#' @examples
#' eq_data %>% eq_clean_data() %>% filter(INTENSITY > 6, DATE > "2000-01-01") %>%
#' ggplot(aes(x = DATE, size = INTENSITY, color = DEATHS, y = COUNTRY)) +
#' geom_timeline()
geom_timeline <- function(mapping = NULL, data = NULL, na.rm = TRUE,
    stat = "identity", position = "identity", show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
            geom = GeomTimeline,
            mapping = mapping,
            data = data,
            stat = stat,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            position = position,
            params = list(na.rm = na.rm, ...)

        )
}

#' @importFrom ggplot2 ggproto Geom draw_key_blank aes
#' @importFrom grid gpar textGrob polyLineGrob unit
#' @importFrom dplyr sample_n top_n
GeomTimelineLabel <-
    ggplot2::ggproto(
        "GeomTimelineLabel",
        ggplot2::Geom,
        required_aes = c("x", "label"),
        draw_key = ggplot2::draw_key_blank,
        setup_data = function(data, params) {
            print(params$n_max)
            if (!is.null(params$n_max)) {
                if (is.null(data$size)) {
                    data <- dplyr::sample_n(data,params$n_max)
                }
                else {
                    data <- dplyr::top_n(data, params$n_max, size) %>%
                        arrange(desc(size)) %>% slice(1:params$n_max)
                }
            }

            data
        },
        draw_panel = function(data, panel_scales, coord, n_max) {

            if (is.null(data$y))  data$y <- 0.2
            coords <- coord$transform(data, panel_scales)
            offset <- 0.25 / length(unique(data$group))

            text <- grid::textGrob(
                label = coords$label,
                x = coords$x,
                y = coords$y + offset,
                rot = 45,
                just = c("left", "bottom")
            )

            line <- grid::polylineGrob(
                x = grid::unit(c(coords$x, coords$x), "npc"),
                y = grid::unit(c(coords$y, coords$y + offset), "npc"),
                id = rep(1:NROW(coords), 2),
                gp = grid::gpar(
                    col = "grey",
                    lwd = 1.5,
                    linetype = "dashed"
                )
            )

            grid::gList(text, line)

        }
    )

#' Timeline labels
#'
#' This geom adds labels to the points in a timeline chart, up to \code{n_max} points.
#'
#' @param mapping Set of aesthetic mappings created by \code{aes} or \code{aes_}.
#' @param data The data to be displayed.  If specified and \code{inherit.aes = TRUE}
#' (the default), it is combined with the default mapping at the top level of
#' the plot. You must supply mapping if there is no plot mapping.
#' @param n_max The maximum number of points to be labelled. If a size aesthetic
#' is passed, the top n values based on the size aes will be selected. If not,
#' n random values are selected.
#' @param na.rm If FALSE, the default, missing values are removed with a warning.
#' If TRUE, missing values are silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA,
#' the default, includes if any aesthetics are mapped. FALSE never includes,
#' and TRUE always includes.
#' @param ... other arguments passed on to layer.
#'
#' @return A GeomTimelineLabel layer
#' @export
#'
#' @importFrom ggplot2 layer
#' @examples
#' eq_data %>% eq_clean_data() %>% filter(INTENSITY > 6, DATE > "2000-01-01") %>%
#' ggplot(aes(x = DATE, size = INTENSITY, color = DEATHS, y = COUNTRY)) +
#' geom_timeline()
geom_timeline_label <- function(
        mapping = NULL, data = NULL, stat = "identity",
        position = "identity", ..., na.rm = FALSE,
        n_max = NULL, show.legend = NA,
        inherit.aes = TRUE
    ) {
    ggplot2::layer(
        geom = GeomTimelineLabel, mapping = mapping,
        data = data, stat = stat, position = position,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, n_max = n_max, ...)
    )

}


#' Theme for geom_timeline plot
#'
#' @description  This theme helps visualize the information from a geom_timeline
#' plot better.
#'
#' @examples
#' \dontrun{
#' data %>% eq_clean_data() %>%
#'    filter(COUNTRY %in% c("GREECE", "ITALY"), YEAR > 2000) %>%
#'    ggplot(aes(x = DATE, y = COUNTRY")) +
#'    geom_timeline() +
#'    theme_timeline()
#' }
#'
#' @importFrom ggplot2 theme element_blank element_line element_text
#' @importFrom grid arrow unit
#'
#' @export
#'
theme_eq_timeline <- ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    legend.position = "bottom",
    legend.key = ggplot2::element_blank(),
    axis.line.x =
        ggplot2::element_line(
            colour = "black",
            arrow = grid::arrow(
                type = "closed",
                length = grid::unit(1, "npc")
            ),
            size = 0.9
        ),
    axis.text.x = ggplot2::element_text(colour = "black"),
    axis.ticks = ggplot2::element_blank(),
    axis.title = ggplot2::element_blank()
    )
