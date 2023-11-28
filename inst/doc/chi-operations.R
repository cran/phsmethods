## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
chi_numbers <- c(
  "0211165794",
  "9999999999",
  "402070763",
  "00402070763",
  "0101010000",
  "Missing CHI",
  NA,
  ""
)

library(phsmethods)

chi_check(chi_numbers)

## ----message=FALSE------------------------------------------------------------
library(dplyr)

data <- tibble(chi = c(
  "0211165794",
  "9999999999",
  "402070763",
  "00402070763",
  "0101010000",
  "Missing CHI",
  NA,
  ""
))

## -----------------------------------------------------------------------------
fixed_data <- data %>%
  mutate(chi = chi_pad(chi))

checked_data <- fixed_data %>%
  mutate(valid_chi = chi_check(chi))

checked_data

## -----------------------------------------------------------------------------
fixed_data %>%
  count(valid_chi = chi_check(chi), sort = TRUE)

## -----------------------------------------------------------------------------
fixed_data %>%
  mutate(chi = if_else(chi_check(chi) != "Valid CHI", NA_character_, chi))

fixed_data %>%
  filter(chi_check(chi) == "Valid CHI")

## -----------------------------------------------------------------------------
data <- tibble(
  chi = c("0101011237", "0211165794", "0402070763", "0101336489", "1904851231", "2902960018")
)

# Confirm all of the CHIs are valid
count(data, chi_check(chi))

data_sex <- data %>%
  mutate(sex = sex_from_chi(chi, chi_check = FALSE))
data_sex

## -----------------------------------------------------------------------------
data_sex <- data_sex %>%
  mutate(sex_factor = sex_from_chi(chi, as_factor = TRUE))

data_sex

## -----------------------------------------------------------------------------
library(ggplot2)

data_sex %>%
  ggplot(aes(y = "", fill = sex_factor)) +
  geom_bar() +
  coord_polar() +
  labs(title = "Count of Male vs Female", x = "", y = "") +
  scale_fill_brewer("Sex (from CHI)", type = "qual") +
  theme_minimal()

## -----------------------------------------------------------------------------
data_dob <- data %>%
  mutate(dob = dob_from_chi(chi))

data_dob

## -----------------------------------------------------------------------------
# Expect no one born after 2015-12-31
data %>%
  mutate(dob = dob_from_chi(chi, max_date = as.Date("2015-12-31")))

# Expect no one born before 1999-12-31 i.e. 16 years before our data started.
data %>%
  mutate(dob = dob_from_chi(
    chi,
    max_date = as.Date("2015-12-31"),
    min_date = as.Date("2015-12-31") - lubridate::years(16)
  ))

## -----------------------------------------------------------------------------
data <- data %>%
  mutate(event_date = as.Date(c(
    "2015-01-01",
    "2014-01-01",
    "2013-01-01",
    "2012-01-01",
    "2011-01-01",
    "2010-01-01"
  )))

# Using the event date as the maximum date
data %>%
  mutate(dob = dob_from_chi(chi, max_date = event_date))

# Setting a 'fixed' minimum date as well as using the event date
data_dob <- data %>%
  mutate(dob = dob_from_chi(
    chi,
    max_date = event_date,
    min_date = as.Date("1915-01-01")
  ))

data_dob

## -----------------------------------------------------------------------------
data %>%
  mutate(age = age_from_chi(chi))

# Work out age at a fixed date
data %>%
  mutate(age = age_from_chi(chi, ref_date = as.Date("2016-01-01")))

# Work out age at a relative date
data %>%
  mutate(age = age_from_chi(chi, ref_date = event_date))

## -----------------------------------------------------------------------------
data %>%
  mutate(age = age_from_chi(chi, ref_date = event_date, max_age = 18))

data %>%
  mutate(age = age_from_chi(
    chi,
    ref_date = event_date,
    min_age = 60,
    max_age = 120
  ))

data %>%
  mutate(age = age_from_chi(
    chi,
    min_age = 60,
    max_age = 120
  ))

