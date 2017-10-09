library(testthat)
library(NOAA)
library(dplyr)
library(leaflet)

context("Leaflet map and label creation")

plot <- eq_data %>% eq_clean_data() %>% eq_location_clean() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    mutate(popup_text = eq_create_label(.)) %>% eq_map(annot_col = popup_text)

test_that(
    "Leaflet map is rendered",
    {
        expect_is(plot, "leaflet")
        expect_equal(plot$x$calls[[2]]$method, "addCircleMarkers")
    }
)

popup <- eq_data %>% eq_clean_data() %>% eq_location_clean() %>%
    dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
    slice(1) %>%
    mutate(popup_text = eq_create_label(.)) %>% select(popup_text)
popup_sample <- "<b>Location:</b>  Veracruz:  San Andres Tuxtla, Tuxtepec <br /> <b>Magnitude:</b> 5.9 <br /> "
test_that(
    "Popup labels are correct",
    {
        expect_is(popup$popup_text, "character")
        expect_equal(as.character(popup), popup_sample)
    }
)
