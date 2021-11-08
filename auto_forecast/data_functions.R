# DATA FUNCTIONS ----

library(timetk)
library(tidyverse)

# FUNCTIONS -----

select_dataset <- function(data, choice = "m4_daily", show_message = TRUE) {

    data_filtered <- data %>%
        filter(dataset_id == choice)

    if (show_message) {
        message(stringr::str_glue(
        "Selected: {data_filtered$dataset_id}
        Description: {data_filtered$desc}"
        ))
    }

    return(data_filtered$data[[1]])

}

get_dataset_description <- function(data, choice = "m4_daily") {
    data_filtered <- data %>%
        filter(dataset_id == choice)

    return(data_filtered$desc[[1]])
}

get_dataset_column_names <- function(data) {
    data %>% colnames()
}

contains_time_series_groups <- function(data) {
    idx <- timetk::tk_index(data)
    idx_unique <- unique(idx)

    groups_detected <- FALSE
    if (length(idx) > length(idx_unique)) {
        groups_detected <- TRUE
    }
    return(groups_detected)
}


