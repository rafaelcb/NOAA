#' Draw earthquake locations and annotate them.
#'
#' @param df Dataframe with NOAA data, including  at least earthquake
#' longitude and latitude.
#' @param annot_col Unquoted column names with information to annotate the popups of the map.
#'
#' @importFrom rlang enquo
#' @importFrom leaflet leaflet addTiles addCircleMarkers
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' library(dplyr)
#' eq_data %>% eq_clean_data() %>% eq_location_clean() %>%
#' dplyr::filter(COUNTRY == "CHINA" & lubridate::year(DATE) >= 2000) %>%
#' eq_map(annot_col = DATE)
#'
eq_map <- function(df, annot_col = NULL) {
    annot_col <- rlang::enquo(annot_col)
    leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addCircleMarkers(
                data = df,
                lng = ~LONGITUDE,
                lat = ~LATITUDE,
                radius = 8,
                weight = 0.5,
                popup = annot_col
            )
}


#' Create labels for an earthquake location map
#'
#' @param df An NOAA earthquake dataframe, with information on locations, magnitude
#' and death toll of earthquakes.
#'
#' @return An HTML-like column vector with information for the map popups.
#' @export
#' @importFrom magrittr "%>%"
#'
#' @examples
#' library(dplyr)
#' eq_data %>% eq_clean_data() %>% eq_location_clean() %>%
#'    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'    mutate(popup_text = eq_create_label(.)) %>% eq_map(annot_col = popup_text)
#'
eq_create_label <- function(df) {

    if (!"LOCATION_NAME" %in% colnames(df)) stop("LOCATION_NAME column necessary to create label.")
    if (!"EQ_PRIMARY" %in% colnames(df)) stop("EQ_PRIMARY column necessary to create label.")
    if (!"TOTAL_DEATHS" %in% colnames(df)) stop("TOTAL_DEATHS column necessary to create label.")
    location <- ifelse(
        !is.na(df$LOCATION_NAME),
        paste("<b>Location:</b>", df$LOCATION_NAME, "<br />"),
        ""
        )
    magnitude <- ifelse(
        !is.na(df$EQ_PRIMARY),
        paste("<b>Magnitude:</b>", df$EQ_PRIMARY, "<br />"),
        ""
        )
    deaths <- ifelse(
        !is.na(df$TOTAL_DEATHS),
        paste("<b>Total deaths:</b>", df$TOTAL_DEATHS, "<br />"),
        ""
        )

    paste(location, magnitude, deaths)

}
