Further Cleaning Air Quality Data
================
Gustavo Garcia-Franceschini
2023-11-30

# Air Quality Data Time frames

``` r
df_pm25 = read_csv("raw_data/pm_2020.csv") %>%
  bind_rows(read_csv("raw_data/pm_2021.csv")) %>%
  bind_rows(read_csv("raw_data/pm_2022.csv")) %>%
  janitor::clean_names() %>%
  filter(state == "New York") %>%
  mutate(pollutant = "PM2.5",
         measurement = "daily_mean_pm2_5_concentration",
         value = daily_mean_pm2_5_concentration,
         scaled_value = scale(value)[,1])

df_ozone = read_csv("raw_data/ozone_2020.csv") %>%
  bind_rows(read_csv("raw_data/ozone_2021.csv")) %>%
  bind_rows(read_csv("raw_data/ozone_2022.csv")) %>%
  janitor::clean_names() %>%
  filter(state == "New York") %>%
  mutate(pollutant = "Ozone",
         measurement = "daily_max_8_hour_ozone_concentration",
         value = daily_max_8_hour_ozone_concentration,
         scaled_value = scale(value)[,1])

df_co = read_csv("raw_data/co_2020.csv") %>%
  bind_rows(read_csv("raw_data/co_2021.csv")) %>%
  bind_rows(read_csv("raw_data/co_2022.csv")) %>%
  janitor::clean_names() %>%
  filter(state == "New York") %>%
  mutate(pollutant = "CO",
         measurement = "daily_max_8_hour_co_concentration",
         value = daily_max_8_hour_co_concentration,
         scaled_value = scale(value)[,1])

df_no2 = read_csv("raw_data/no2_2020.csv") %>%
  bind_rows(read_csv("raw_data/no2_2021.csv")) %>%
  bind_rows(read_csv("raw_data/no2_2022.csv")) %>%
  janitor::clean_names() %>%
  filter(state == "New York") %>%
  mutate(pollutant = "NO2",
         measurement = "daily_max_1_hour_no2_concentration",
         value = daily_max_1_hour_no2_concentration,
         scaled_value = scale(value)[,1])

df_air = df_pm25 %>%
  bind_rows(df_ozone) %>%
  bind_rows(df_co) %>%
  bind_rows(df_no2)
```

# Loading Zip Code Data

``` r
library(sf)
```

    ## Linking to GEOS 3.11.2, GDAL 3.6.2, PROJ 9.2.0; sf_use_s2() is TRUE

``` r
library(sfheaders)
#source: https://github.com/erikgregorywebb/nyc-housing/blob/master/Data/nyc-zip-codes.csv
zip_codes = read_csv("raw_data/nyc-zip-codes.csv") %>%
  mutate(zip = as.character(ZipCode)) %>%
  select(-ZipCode) %>%
  janitor::clean_names()
```

    ## Rows: 178 Columns: 3

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Borough, Neighborhood
    ## dbl (1): ZipCode
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
#source: https://catalog.data.gov/dataset/tiger-line-shapefile-2019-2010-nation-u-s-2010-census-5-digit-zip-code-tabulation-area-zcta5-na 
zip_shapes = read_sf(dsn = "raw_shapes/", layer = 'tl_2019_us_zcta510') %>% 
  rename(zip = ZCTA5CE10) %>%
  inner_join(zip_codes) %>%
  janitor::clean_names()
```

    ## Joining with `by = join_by(zip)`

# Matching air quality sites with zip codes

``` r
df_air = df_air %>% st_as_sf(coords = c("site_longitude", "site_latitude"),
                             crs = st_crs(zip_shapes)) %>%
  select(date, source, site_id, poc, units, daily_aqi_value, site_name, 
         daily_obs_count, percent_complete, aqs_parameter_code, 
         aqs_parameter_desc, cbsa_code, cbsa_name, state_code, state, 
         county_code, county, pollutant, measurement, value, scaled_value, 
         geometry)
```

``` r
zip_contains_site = st_join(df_air, zip_shapes, join = st_within) %>%
  sf_to_df(fill = T)  %>%
  select(zip, site_id) %>%
  filter(!is.na(zip)) %>%
  group_by(site_id, zip) %>%
  summarize(count = n())
```

    ## `summarise()` has grouped output by 'site_id'. You can override using the
    ## `.groups` argument.

``` r
df_air = df_air %>%
  left_join(zip_contains_site) %>%
  select(-count) %>%
  rename(zip_code = zip) %>% 
  sf_to_df(fill = T)
```

    ## Joining with `by = join_by(site_id)`

# Uploading Data

``` r
write_csv(df_air, "cleaned_data/alt_air_data.csv")
```
