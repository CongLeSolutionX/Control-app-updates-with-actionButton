library("WDI")
library("tidyverse")
library("gapminder")
library("countrycode")

countries_tib <- gapminder %>%
  select(country, continent) %>%
  unique() %>%
  rename(name = country) %>%
  mutate_if(is.factor, as.character) %>%
  mutate(iso2c = countrycode(name,
                             "country.name",
                             "iso2c"))

indicators_tib <- tibble(
  indicator_code = c("IT.NET.USER.ZS", "FB.CBK.BRCH.P5", "SE.SEC.ENRL"),
  indicator_name = c("Individuals using the Internet (% of population)", "Commercial bank branches (per 100,000 adults)", "Secondary education, pupils")
)

wdi_data <- WDI(country = countries_tib$iso2c, indicator = indicators_tib$indicator_code)

wdi_data %>%
  as_tibble() %>%
  gather(indicator, value, 4:ncol(.)) %>%
  write_csv("data/wdi_data.csv")

countries_tib %>%
  write_csv("data/countries.csv")

indicators_tib %>%
  write_csv("data/indicators.csv")
