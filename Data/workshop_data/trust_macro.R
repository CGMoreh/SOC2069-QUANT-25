### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
### ----------------------------------------------------------------------- ###  
### ----           Macro-correlates of social trust                -----    ### 
### ----------------------------------------------------------------------- ###  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  

##### Packages #####

# if (!require("pak")) install.packages("pak")

# pak::pkg_install("xfun")

# xfun::pkg_attach("tidyverse",
#                  "ggrepel",
#                  "sjlabelled",
#                  "sjmisc",
#                  "easystats",
#                  "archive",
#                  "fs",
#                  install = "pak" # requires the {pak} packages to run pak::pkg_install
#                  )

library(tidyverse)
library(ggrepel)
library(easystats)
library(sjlabelled)
library(sjmisc)
library(archive)
library(fs)

##### Get "social trust" variable from the Integrated Values Surveys (IVS) #####

# Downloaded the folowing Integrated Values Surveys (IVS) datasets in SPSS (.sav) format from https://www.worldvaluessurvey.org/WVSEVStrend.jsp on 05/11/2025:
##   - EVS Trend File 1981-2017 (3.0.0)
##   - WVS Trend File 1981-2022 (4.0.0) 

# Applied the SPSS Merge Syntax file (EVS_WVS_MergeSyntax_SPSS.zip) provided on https://www.worldvaluessurvey.org/WVSEVStrend.jsp
# Saved output IVS dataset as "ivs_81-22_v4-0.sav"


# Import complete dataset
# ivs <- sjlabelled::read_spss("./Data/raw/ivs/ivs_81-22_v4-0.sav", encoding="latin1")
ivs <- data_read("./Data/raw/ivs/ivs_81-22_v4-0.sav", encoding="latin1")

nrow(ivs) ## [1] 666907

# Helper function to clean up labels after "latin1" encoding
clean_text <- function(x) {
  if (is.null(x)) return(x)
  # preserve NA
  if (all(is.na(x))) return(x)
  x_chr <- as.character(x)
  x_chr <- stringi::stri_replace_all_regex(
    x_chr,
    pattern = c("Â´", "Â", "Ã¢", "Ã©", "â€™", "â€˜", "â€œ", "â€�", "\u0092"),
    replacement = c("'",  "'",  "a",   "e",  "'",    "'",    "\"",   "\"",   "'"),
    vectorize_all = FALSE
  )
  # transliterate remaining diacritics / smart quotes to ASCII
  x_chr <- stringi::stri_trans_general(x_chr, "Latin-ASCII")
  # normalize a few remaining Unicode quotes to plain ASCII
  x_chr <- gsub("[\u2018\u2019\u201A\u201B\u2032]", "'", x_chr)
  x_chr <- gsub("[\u201C\u201D\u201E\u201F\u2033]", "\"", x_chr)
  trimws(x_chr)
}

clean_labels <- function(df) {
  for (nm in names(df)) {
    var <- df[[nm]]
    # variable label (haven/sjlabelled use "label" attribute)
    vl <- attr(var, "label", exact = TRUE)
    if (!is.null(vl)) attr(df[[nm]], "label") <- clean_text(vl)

    # value labels (haven uses "labels" attribute: a named numeric vector)
    labs <- attr(var, "labels", exact = TRUE)
    if (!is.null(labs)) {
      names(labs) <- clean_text(names(labs))
      attr(df[[nm]], "labels") <- labs
    }

    # factor levels
    if (is.factor(var)) levels(df[[nm]]) <- clean_text(levels(var))
  }

  # clean var.labels attribute
  if (!is.null(attr(df, "var.labels"))) {
    attr(df, "var.labels") <- vapply(attr(df, "var.labels"), clean_text, FUN.VALUE = "")
  }

  df
}

ivs_trust <- ivs |> 
  select(S001, s002, S002EVS, S003, COW_NUM, S020, COUNTRY_ALPHA, COW_ALPHA, A165) |> 
  drop_labels() |> 
  clean_labels() |> 
  drop_na(A165) |> 
  mutate(
    source = paste0(S001, ifelse(is.na(s002), S002EVS, s002)),
    country = case_match(S003, 
                    "Great Britain" ~ "United Kingdom",
                    "Northern Ireland" ~ "United Kingdom",
                    .default = S003),
    S020 = to_numeric(S020, preserve_levels = TRUE) |> as.integer()
  )

    # country_year = paste0(country, "_", S020),

    # trust_d = reverse(A165)

    # trust_d = 
    #   rec(A165, 
    #     rec = "
    #       2 = 0 [Can't be too careful];               
    #       1 = 1 [Most people can be trusted]; 
    #       else = NA
    #     "
    # )

    # trust_d = recode_values(
    #   A165,
    #     recode = list(
    #       `0` = 2,
    #       `1` = 1
    #     )
    #   ),

    # trust_d = case_match(
    #   A165,
    #   "2" ~ 0,
    #   "1" ~ 1
    #   )
  # assign_labels(
  #     select = "trust_d",
  #     # variable = "Trust in people",
  #     values = c(
  #       `0` = "Can't be too careful", 
  #       `1` = "Most people can be trusted"
  #     )
  #   )
 

options(warn=-1)
options(warn=0)


data_write(ivs_trust, "Data/workshop_data/ivs_trust.sav")
    
ivs_trust_macro <- ivs_trust |> 
  mutate(
    trust = A165 |> 
      datawizard::to_numeric(lowest = 0) |> 
      reverse(),
    trust_pct = round(mean(trust, na.rm = TRUE) * 100, 2),
    n_trusts = sum(trust == 1, na.rm = TRUE),
    n_valid = sum(!is.na(trust)),
    .by = c(country, source)
  ) |> 
  var_labels(
    trust_pct = "% people who agree that “most people can be trusted”",
    n_trusts = "Number of people who agree that “most people can be trusted”",
    n_valid = "Total sample size",
  ) |> 
  select(source, year = S020, country, trust_pct, n_trusts, n_valid) |>
  distinct(.keep_all = TRUE)

#### Delhey and Newton 2005 ####

# From Delhey and Newton: "Two waves of the WVS are used: wave II for 1990, and wave III for 1995-7. Trust scores are available in the WVS III survey for 55 countries; an additional 11 countries are available from the previous WVS II, giving a total of 66 countries."

delhey_newton_2005_ivs <- ivs_trust_macro |> 
  filter(source %in% c("WVS3", "WVS2", "EVS2")) |> 
  arrange(country, desc(source)) |> 
  distinct(country, .keep_all = TRUE)



#### Wilkinson and Pickett 2009 ####

# From: Wilkinson RG, Pickett K. 2009. The Spirit Level: Why Greater Equality Makes Societies Stronger. New York: Bloomsbury Press
#   - p. 271, endnote 5: 5. European Values Study Group and World Values Survey Association, European and World Values Survey Integrated Data File, 1999-2001, Release r. Ann Arbor, MI: Inter-university Consortium for Political and Social Research, 2005.
#   - p. 267: ...23 rich countries:  Australia Greece Portugal Austria Ireland Singapore Belgium Israel Spain Canada Italy Sweden Denmark Japan Switzerland Finland Netherlands United Kingdom France New Zealand United States of America Germany Norway

# From Pickett K. 2024. The Spirit Level at 15 – Technical Appendix. The Equality Trust, London:
#   - In The Spirit Level we used the inter-decile 80:20 ratio for income inequality;
#   - In The Spirit Level, we used the average reported between 2003-2006 (measured between 1992-2001) and correlated that with the most up to date outcomes data we could access (1999-2004).

# data available from the Equality Trust website: 
# - https://equalitytrust.org.uk/news/blog/spirit-level-data-antidote-alternative-facts-0/
# - International: https://media.equality-trust.out.re/uploads/2025/09/international-inequality.xls

data_read("Data/raw/macro/pickett-2009-international-inequality.xls") |> 
  select(1, S80S20 = 2, 3, 4, 5, 10, 11) |> 
  data_write("Data/workshop_data/w2/pickett2009.csv")


#### Pickett et al. 2024 ####

### TRUST ###

# From Pickett K, Gauhar A, Wilkinson R. 2024. The Spirit Level at 15: The Enduring Impact of Inequality. The Equality Trust, London: 
#   - footnote 38: World Values Survey Trend File (1981-2022) CrossNational Data-Set. Data File Version 3.0.0. 
#   - footnote 39: EVS Trend File 1981-2017. ZA7503, Data Version 3.0.0.
# From Pickett K. 2024. The Spirit Level at 15 – Technical Appendix. The Equality Trust, London:
#   - p2: Our dataset includes 22 countries:  Australia, Austria, Belgium, Canada, Denmark, Finland, France, Germany, Greece, Ireland, Israel, Italy, Japan, Netherlands, New Zealand, Norway, Portugal, Spain, Sweden, Switzerland, UK, USA
#   - p2: As Singapore is not in the OECD it is not included in this report

pickett_countries <- c("Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Greece", "Ireland", "Israel", "Italy", "Japan", "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States")

pickett_etal_2024_ivs <- ivs_trust_macro |> 
  # filter(country %in% pickett_countries) |> 
  arrange(country, desc(year)) |> 
  distinct(country, .keep_all = TRUE) |> 
  mutate(country = case_match(country, 
    "Turkey" ~ "Türkiye",
    .default = country)
  )

### INEQUALITY ###

# From Pickett K. 2024. The Spirit Level at 15 – Technical Appendix. The Equality Trust, London:
#   - In this report we use the Gini coefficient, but for direct comparison with the original analyses, we also give the correlations for the 80:20 ratio; Both are available from the OECD.
#   - Income inequality data from 2013 is used for this update.  ... precedes the outcome data with sufficient lag time to have had an impact

oecd_inequality <- data_read("Data/raw/macro/OECD/oecd-inequality.csv") |> 
  select(country_code = REF_AREA, country = "Reference area", year = TIME_PERIOD, measure = Measure, value = OBS_VALUE)

pickett_etal_2024_inequality <- oecd_inequality |> 
  filter(!is.na(value) & ifelse(country != "Brazil", year <= 2013, year <= 2016)) |> 
  # filter(country %in% pickett_countries) |> 
  group_by(country, measure) |> 
  slice_max(year, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  pivot_wider(
    names_from = measure,
    values_from = value
  ) |> 
  mutate(country = case_match(country, 
  "China (People’s Republic of)" ~ "China",
  "Korea" ~ "South Korea",
  "Slovak Republic" ~ "Slovakia",
  .default = country
))

pickett_etal_2024 <- data_join(pickett_etal_2024_inequality, pickett_etal_2024_ivs, by = "country", join = "inner") |> 
  select(country, trust_pct, gini = "Gini (disposable income)", S80S20 = "Quintile share ratio (disposable income)", trust_source = source, trust_n_agrees = n_trusts, trust_n_total = n_valid, trust_year = year.y, inequality_year = year.x) |> 
  var_labels(
    gini = "Gini (disposable income)", 
    S80S20 = "Quintile share ratio (disposable income)"
  )

data_write(pickett_etal_2024, "Data/workshop_data/w2/pickett&al2024.sav")


## OECD IDD ##

wilkinson <- wdi |>
  rename (
    series = `Series Name`,
    country = `Country Name`,
    country_code = `Country Code`,
    year = Time,
  ) |> 
  filter(!is.na(Value), year %in% 1999:2001) |>   
  mutate(
    series = case_match(
      series,
      "Income share held by highest 20%" ~ "h20",
      "Income share held by lowest 20%" ~ "l20"
    )
  ) |>
  filter(!is.na(series)) |> 
  select(country, country_code, year, series, Value) |>
  pivot_wider(names_from = series, values_from = Value) |> 
  group_by(country, country_code) |>
  summarize(
    h20_m = mean(h20, na.rm = TRUE),
    l20_m = mean(l20, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  mutate(
    s80s20 = if_else(!is.na(h20_m) & !is.na(l20_m), h20_m / l20_m, NA_real_)
  ) |> 
  filter(country %in% c(
    "Australia", "Greece", "Portugal", "Austria", "Ireland",
    "Singapore", "Belgium", "Israel", "Spain", "Canada",
    "Italy", "Sweden", "Denmark", "Japan", "Switzerland",
    "Finland", "Netherlands", "United Kingdom", "France", "New Zealand",
    "United States", "Germany", "Norway")
  )








#### Macroeconomic variables ####

## WB WDI ##

wdi <- data_read("Data/raw/macro/WDI/WDI_Data.csv")

delhey_newton_2005_wdi <- wdi |>
  select (
    series = `Series Name`,
    country = `Country Name`,
    country_code = `Country Code`,
    year = Time,
    Value
  ) |> 
  filter(!is.na(Value) & year %in% 1995:1996) |>   
  mutate(
    series = case_match(
      series,
      "GDP per capita, PPP (current international $)" ~ "gdp-pc-ppp",
      "Gini index" ~ "gini"
    ),
    dist = abs(year - 1996)
  ) |>
  filter(!is.na(series)) |>
  group_by(country, series) |>
  arrange(dist, desc(year), .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |>
  select(!c(year, dist)) |> 
  pivot_wider(names_from = series, values_from = Value) |> 
  drop_na()








### Importing "GDP per capita, PPP (constant 2017 international $) [World Bank, 2019]" data from https://data.worldbank.org/indicator
### WVS variable name: "GDPpercap2"

fs::dir_ls("Data/raw/macro_contextual")

# a <- archive("Data/macro_contextual/P_Data_Extract_From_World_Development_Indicators.zip")


WB <- readxl::read_xlsx("Data/raw/macro_contextual/P_Data_Extract_From_World_Development_Indicators.xlsx", sheet = 1) |> 
  select(3, 5:10) |> 
  rename(country = 1,
         GDPpercap2 = 2,
         pop = 3,
         urban_pop_pct = 4,
         inc_top20 = 5,
         inc_bottom20 = 6,
         s80s20 = 7) |> 
  mutate(across(-c(country), as.numeric),
         country = case_match(country, 
                              "Russian Federation" ~ "Russia", 
                              "Slovak Republic" ~ "Slovakia", 
                              "Egypt, Arab Rep." ~ "Egypt",
                              "Hong Kong SAR, China" ~ "Hong Kong SAR",
                              "Iran, Islamic Rep." ~ "Iran",
                              "Kyrgyz Republic" ~ "Kyrgyzstan",
                              "Macao SAR, China" ~ "Macau SAR",
                              "Korea, Rep." ~ "South Korea",
                              "Turkiye" ~ "Turkey",
                              .default = country)) |> 
  var_labels(GDPpercap2 = "GDP per capita, PPP (constant 2017 international $)",
             s80s20 = "Ratio of the average income of the 20% richest to the 20% poorest")


WB_CLASS <- readxl::read_xlsx("Data/raw/macro_contextual/CLASS.xlsx", sheet = 1, n_max = 220) |> 
  rename(country = "Economy") |> 
  select(1, 3) |> 
  mutate(country = case_match(country, 
                              "Russian Federation" ~ "Russia", 
                              "Slovak Republic" ~ "Slovakia", 
                              "Egypt, Arab Rep." ~ "Egypt",
                              "Hong Kong SAR, China" ~ "Hong Kong SAR",
                              "Iran, Islamic Rep." ~ "Iran",
                              "Kyrgyz Republic" ~ "Kyrgyzstan",
                              "Macao SAR, China" ~ "Macau SAR",
                              "Korea, Rep." ~ "South Korea",
                              "Türkiye" ~ "Turkey",
                              .default = country))
  

### Match and merge the datasets



l <- list(joint_cntry_averages, 
     WB, 
     WB_CLASS)


joint_merged <- datawizard::data_merge(l, join = "left", by = "country")

# write_rds(joint_merged, "Data/data_for_logo.rds")

saveRDS(joint_merged, "Data/for_analysis/lab3macro.rds", compress = "bzip2")

## Name changes

ineq <- readRDS("D:/OneDrive - Newcastle University/GitHub_priv/SOC2069-QUANT/Data/trust_inequalities_macro.rds")

ineq <- ineq |> 
  rename(country_code = cntry_AN,
         GDP_pc = GDPpercap2,
         pop_n = pop,
         income_top20 = inc_top20,
         income_bottom20 = inc_bottom20,
         inequality_s80s20 = s80s20)

ineq <- ineq |> 
  sjlabelled::var_labels(country = "Country name",
                         pop_n = "Population size",
                         urban_pop_pct = "% of population living in urban areas",
                         income_top20 = "Share of population in the top 20% of the income distribution",
                         income_bottom20 = "Share of population in the bottom 20% of the income distribution",
                         Region = "Geographical region (World Bank classification)")

data_write(ineq, "D:/OneDrive - Newcastle University/GitHub_priv/SOC2069-QUANT/Data/workshop_data/trust_inequality.dta")


     