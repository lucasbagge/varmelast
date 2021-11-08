# SETUP ----

library(tidyverse)
library(lubridate)


# DATA DESCRIPTIONS ----
description_tbl <-
    tribble(
        ~ dataset_id, ~ desc,
        "walmart_sales_weekly", "A sample of 7 Weekly data sets from the Kaggle Walmart Recruiting Store Sales Forecasting competition.",
        "airpassengers", "The classic Box & Jenkins airline data. Monthly totals of international airline passengers, 1949 to 1960."
    )

# COMPOSE DATASETS TIBBLE----
datasets_tbl <- list(
    walmart_sales_weekly    = walmart_sales_weekly %>% select(id, Date, Weekly_Sales),
    airpassengers           = AirPassengers %>% tk_tbl() %>% mutate(index = as_date(index))
) %>%
    enframe(
        name  = "dataset_id",
        value = "data"
    ) %>%
    left_join(description_tbl, by = "dataset_id")

# COMPOSE CHOICES ----
choices_vec <- datasets_tbl$dataset_id

