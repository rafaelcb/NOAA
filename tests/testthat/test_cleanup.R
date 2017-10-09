library(testthat)
library(NOAA)
library(dplyr)

context("Cleaning dataset")

clean_dataset <- eq_clean_data(eq_data)
test_that(
    "eq_clean_data cleans the dataframe",
    {
        expect_is(clean_dataset, "data.frame")
        expect_is(clean_dataset$LATITUDE, "numeric")
        expect_is(clean_dataset$LONGITUDE, "numeric")
        expect_is(clean_dataset$DATE, "Date")
        expect_is(clean_dataset$TOTAL_DEATHS, "numeric")
        expect_is(clean_dataset$EQ_PRIMARY, "numeric")
    }

)

dummy_loc <- tibble(LOCATION_NAME = c("COSTA RICA: SAN JOSE", "KANSAS CITY"))
clean_location <- eq_location_clean(dummy_loc)

test_that(
    "eq_location_clean cleans the LOCATION_NAME variables",
    {
        expect_is(clean_location, "data.frame")
        expect_is(clean_location$LOCATION_NAME, "character")
        expect_equal(clean_location$LOCATION_NAME, c("San Jose", "Kansas City"))
    }

)
