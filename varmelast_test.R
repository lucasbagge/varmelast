from <- today() - 10
to <- today()
# https://www.varmelast.dk/api/v1/heatdata?contextSite=varmelast_dk
# Do the GET data dynamic so the user can choose the date range
resp <- 
  GET(
  paste0('https://www.varmelast.dk/api/v1/heatdata/historical?from=',
         from,
         '&to=',
         to,
         '&intervalMinutes=60&contextSite=varmelast_dk')
)

# Get content of the site
content <- 
  fromJSON(content(resp, 'text'))

date <- content$times$timestamp

df_list <- content$times$values

date_tibble <- map_df(date, as_tibble)

df_tibble <- map_df(df_list, as_tibble)

df_wide <-
  df_tibble %>%
  select(-valueError) %>%
  pivot_wider(names_from =  key,
              values_from = value) %>%
  unnest()

df <-
  df_wide %>% 
  cbind(date_tibble) %>%
  mutate(date = ymd_hms(value)) %>%
  janitor::clean_names() %>%
  select(-value) %>%
  rename(
    Affaldsenergianlaeg = be_vl_affald_ef,
    Kraftvarmeanlaeg = be_vl_kraftv_ef,
    'Spidslast gas' = be_vl_spids_gas_ef,
    'Spidslast olie' = be_vl_spids_olie_ef,
    'Spidslast træpiller' = be_vl_bio_ef,
    'CO2 - Udledning' = be_vl_total_fak,
    'Lokal produktion' = local
  )
df %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  pivot_longer(cols = -c(date),
               names_to = "metric",
               values_to = "value") %>%
  group_by(date = date, metric) %>%
  summarise(value = sum(value, na.rm = TRUE)) %>%
  ungroup()