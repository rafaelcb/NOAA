GeomTimeline <-
    ggplot2::ggproto(
        `_class` = "GeomTimeline",
        `_inherit` = ggplot2::Geom,
        required_aes = c("x"),
        default_aes = ggplot2::aes(
            y = 0.5,
            xmin = NULL,
            xmax = NULL,
            color = "skyblue",
            size = 1,
            alpha = 0.8
        ),
        reparametrise = function(data) {
            if (!is.null(data$xmin)) {
                data$xmin <- as.Date(data$xmin)
                data <- data %>% dplyr::filter(.data$x >= .data$xmin)
            }
            if (!is.null(data$xmax)) {
                data$xmax <- as.Date(data$xmax)
                data <- data %>% dplyr::filter(.data$x <= .data$xmax)
            }

            return(data)
        },
        draw_key = ggplot2::draw_key_blank,
        draw_group = function(data, panel_scales, coord) {
            data <- data[complete.cases(data), ]
            coords <- coord$transform(data, panel_scales)
            print(coords)

            line <- grid::segmentsGrob(
                x0 = min(coords$x, na.rm = TRUE),
                x1 = max(coords$x, na.rm = TRUE),
                y0 = coords$y,
                y1 = coords$y
            )

            points <- grid::pointsGrob(
                x = coords$x,
                y = coords$y,
                pch = 21,
                gp = grid::gpar(
                    fill = coords$colour,
                    cex = coords$size * 0.5,
                    alpha = coords$alpha
                )
            )

            grid::gTree(children = grid::gList(line, points))

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
    show.legend = NA, inherit.aes = TRUE, ...) {
        ggplot2::layer(
            geom = GeomTimeline,
            mapping = mapping,
            data = data,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(na.rm = na.rm, ...)

        )
}

