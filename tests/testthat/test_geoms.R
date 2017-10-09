library(testthat)
library(NOAA)
library(dplyr)
library(ggplot2)

context("Timeline geoms")

plot <- eq_data %>%  eq_clean_data() %>% eq_location_clean() %>%
    filter(lubridate::year(DATE) >= 2000, COUNTRY == "CHINA") %>%
    ggplot(aes(x = DATE, size = EQ_PRIMARY, label = LOCATION_NAME)) +
    geom_timeline() + geom_timeline_label(n_max = 10) + theme_eq_timeline

test_that(
    "Timeline geoms work",
    {
        expect_is(plot, "ggplot")
        expect_equal(plot$theme$axis.text.x$colour, "black")
        expect_equal(length(plot$layers), 2)

    }
    )
