#' Clean NOAA data
#'
#' This function takes in raw NOAA earthquake data as a data.frame
#' (using the original names) and converts the \code{LONGITUDE} and
#' \code{LATITUDE} columns to
#' numeric class. It also creates a DATE variable that combines the
#' \code{YEAR}, \code{MONTH} and \code{DAY} variables to a a Date class object.
#'
#' @param dataframe A data.frame including raw NOAA earthquake data, as
#' read from the original .txt file.
#'
#' @return A tibble with the new column and modified class.
#' @export
#'
#' @importFrom lubridate make_date
#' @importFrom dplyr mutate as_tibble
#' @importFrom magrittr "%>%"
#' @examples
#' eq_clean_data(eq_data)
eq_clean_data <- function(dataframe) {
    dataframe %>%
        dplyr::mutate(
            LONGITUDE = as.numeric(.data$LONGITUDE),
            LATITUDE = as.numeric(.data$LATITUDE),
            EQ_PRIMARY = as.numeric(.data$EQ_PRIMARY),
            TOTAL_DEATHS = as.numeric(.data$TOTAL_DEATHS),
            DATE = lubridate::make_date(
                year = .data$YEAR,
                month = .data$MONTH,
                day = .data$DAY
            )
        ) %>% dplyr::as_tibble()
}


#' Clean NOAA earthquake location
#'
#' This function takes a raw NOAA earthquake data.frame and cleans the
#' \code{LOCATION_NAME} column, by removing the country name and converting the
#' string to title case.
#'
#' @param dataframe A data.frame including raw NOAA earthquake data, as
#' read from the original .txt file.
#'
#' @return A tibble with the cleaned \code{LOCATION_NAME} column.
#' @export
#'
#' @importFrom dplyr mutate as_tibble
#' @importFrom stringr str_to_title str_replace
#' @importFrom magrittr "%>%"
#' @examples
#' eq_location_clean(eq_data)
eq_location_clean <- function(dataframe) {
    dataframe %>%
        dplyr::mutate(
            LOCATION_NAME =
                stringr::str_to_title(
                    stringr::str_replace(
                        .data$LOCATION_NAME,
                        "[A-Z]+:",
                        ""
                    )
                )

        ) %>% dplyr::as_tibble()
}
