Joining Air Quality and Zip Code
================
Gustavo Garcia-Franceschini
2023-11-19

``` r
library(sf)
```

    ## Linking to GEOS 3.11.2, GDAL 3.6.2, PROJ 9.2.0; sf_use_s2() is TRUE

``` r
library(sfheaders)
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# Doing some shape math

What’s going on here is I take the center of each zip code area, and
figure out which UHF neighborhood the that point in inside of. This will
then tell me which zip codes belong to which UHF neighborhoods, and will
allow us to join the air quality and disease datasets.

One shortcoming of this method is that we don’t actually know if the
entire zip code area is inside the UHF neighborhood; just the center.

``` r
air_shapes = read_sf(dsn = "raw_shapes/", layer = "UHF42") %>%
  filter(id != "0")

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
zip_shapes = read_sf(dsn = "raw_shapes/", layer = 'tl_2019_us_zcta510') %>% 
  rename(zip = ZCTA5CE10) %>%
  inner_join(zip_codes) %>%
  st_transform(crs= st_crs(air_shapes)) %>%
  st_point_on_surface()
```

    ## Joining with `by = join_by(zip)`

    ## Warning: st_point_on_surface assumes attributes are constant over geometries

``` r
#source: https://dewey.dunnington.ca/post/2022/profiling-point-in-polygon-joins-in-r/
uhf_contains_zips = st_join(zip_shapes, air_shapes, join = st_within) %>%
  sf_to_df(fill = T)  %>%
  select(zip, id)
```

# Loading data

``` r
df_air = read_csv("raw_data/air_quality.csv") %>%
  janitor::clean_names() %>%
  select(-message) %>%
  mutate(id = as.character(geo_join_id)) %>%
  filter(geo_type_name == "UHF42")
```

    ## Rows: 16218 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (7): Name, Measure, Measure Info, Geo Type Name, Geo Place Name, Time Pe...
    ## dbl (4): Unique ID, Indicator ID, Geo Join ID, Data Value
    ## lgl (1): Message
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
joined_datasets = df_air %>%
  full_join(uhf_contains_zips) %>%
  full_join(zip_codes)
```

    ## Joining with `by = join_by(id)`

    ## Warning in full_join(., uhf_contains_zips): Detected an unexpected many-to-many relationship between `x` and `y`.
    ## ℹ Row 1 of `x` matches multiple rows in `y`.
    ## ℹ Row 85 of `y` matches multiple rows in `x`.
    ## ℹ If a many-to-many relationship is expected, set `relationship =
    ##   "many-to-many"` to silence this warning.

    ## Joining with `by = join_by(zip)`

``` r
write_csv(joined_datasets, "cleaned_data/air_quality_zips.csv")
```
