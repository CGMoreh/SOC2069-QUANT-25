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

xfun::pkg_attach("tidyverse",
                 "sjlabelled",
                 "sjmisc",
                 "easystats",
                 "archive",
                 "fs",
                 install = "pak" # requires the {pak} packages to run pak::pkg_install
                 )

# library(tidyverse)
# library(sjlabelled)
# library(sjmisc)
# library(easystats)
# library(archive)
# library(fs)

#### Individual-level "social trust" ####

# Downloaded the folowing Integrated Values Surveys (IVS) datasets in SPSS (.sav) format from https://www.worldvaluessurvey.org/WVSEVStrend.jsp on 05/11/2025:
##   - EVS Trend File 1981-2017 (3.0.0)
##   - WVS Trend File 1981-2022 (4.0.0) 

# Applied the SPSS Merge Syntax file (EVS_WVS_MergeSyntax_SPSS.zip) provided on https://www.worldvaluessurvey.org/WVSEVStrend.jsp
# Saved output IVS dataset as "ivs_81-22_v4-0.sav"


# Import complete dataset
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
  select(S001, s002, S002EVS, S003, S020, 
    # COW_NUM, COUNTRY_ALPHA, COW_ALPHA, 
    A165) |> 
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
  ) |> select(
    source, year = S020, country, A165
  )
data_write(ivs_trust, "Data/workshop_data/ivs_trust.sav")

#### Country-level "social trust" ####

ivs_trust <- data_read("Data/workshop_data/ivs_trust.sav")

ivs_trust_macro <- ivs_trust |> 
  mutate(
    trust = A165 |> 
      datawizard::to_numeric(lowest = 0) |> 
      reverse(),
    trusts_pct = round(mean(trust, na.rm = TRUE) * 100, 2),
    trusts_n = sum(trust == 1, na.rm = TRUE),
    total_n = sum(!is.na(trust)),
    .by = c(country, source)
  ) |> 
  mutate(country = case_match(country, 
    "Turkey" ~ "Türkiye",
    "Taiwan ROC" ~ "Taiwan",
    .default = country)
  ) |> 
  var_labels(
    trusts_pct = "% people who agree that “most people can be trusted”",
    trusts_n = "Number of people who agree that “most people can be trusted”",
    total_n = "Total sample size",
  ) |> 
  select(source, year, country, trusts_pct, trusts_n, total_n) |>
  distinct(.keep_all = TRUE)

data_write(ivs_trust_macro, "Data/workshop_data/ivs_trust_macro.sav")

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
  select(1, 2, 3, 4, 5, 10, 11) |> 
  data_write("Data/workshop_data/w2/pickett2009.csv")


#### Pickett et al. 2024 ####

### TRUST ###

# From Pickett K, Gauhar A, Wilkinson R. 2024. The Spirit Level at 15: The Enduring Impact of Inequality. The Equality Trust, London: 
#   - footnote 38: World Values Survey Trend File (1981-2022) CrossNational Data-Set. Data File Version 3.0.0. 
#   - footnote 39: EVS Trend File 1981-2017. ZA7503, Data Version 3.0.0.
# From Pickett K. 2024. The Spirit Level at 15 – Technical Appendix. The Equality Trust, London:
#   - p2: Our dataset includes 22 countries:  Australia, Austria, Belgium, Canada, Denmark, Finland, France, Germany, Greece, Ireland, Israel, Italy, Japan, Netherlands, New Zealand, Norway, Portugal, Spain, Sweden, Switzerland, UK, USA
#   - p2: As Singapore is not in the OECD it is not included in this report

ivs_trust_macro <- data_read("Data/workshop_data/ivs_trust_macro.sav")

pickett_countries <- c("Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", "France", "Germany", "Greece", "Ireland", "Israel", "Italy", "Japan", "Netherlands", "New Zealand", "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom", "United States")

pickett_etal_2024_ivs <- ivs_trust_macro |> 
  # filter(country %in% pickett_countries) |> 
  arrange(country, desc(year)) |> 
  distinct(country, .keep_all = TRUE)

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
  mutate(country = case_match(
    country, 
    "China (People’s Republic of)" ~ "China",
    "Korea" ~ "South Korea",
    "Slovak Republic" ~ "Slovakia",
    .default = country
  )
)

pickett_etal_2024 <- data_join(
  pickett_etal_2024_inequality, pickett_etal_2024_ivs, 
  by = "country", 
  join = "inner"
) |> 
  select(Country = country, Trust = trusts_pct, "Income_inequality_Gini" = "Gini (disposable income)", "Income_inequality_S80S20" = "Quintile share ratio (disposable income)") |> 
  var_labels(
    "Income_inequality_Gini" = "Gini coefficient (disposable income)", 
    "Income_inequality_S80S20" = "Quintile share ratio (disposable income)"
  )

data_write(pickett_etal_2024, "Data/workshop_data/w2/pickett&al2024.sav")

#### Delhey and Newton 2005 ####

# From Delhey and Newton: 
#   - "Two waves of the WVS are used: wave II for 1990, and wave III for 1995-7. 
#   - Trust scores are available in the WVS III survey for 55 countries; 
#   - an additional 11 countries are available from the previous WVS II, giving a total of 66 countries."
# From Delhy and Newton (working paper):
#   - "However, six of the 66 nations for which we have trust data had to be excluded from the analysis because other data was missing, leaving a total of 60 nations for this research."
#   - "The excluded countries are Bosnia, Montenegro, Northern Ireland, Puerto Rico, Serbia, and Taiwan."

delhey_newton_2005_countries <- c(
        "Canada", "United States",
        "Mexico", "Domenica", "Uruguay", "Chile", "Argentina", "Venezuela", "Colombia", "Peru", "Brazil",
        "Norway", "Sweden", "Denmark", "Netherlands", "Finland", "Ireland", "Iceland", "Germany", "Switzerland",
        "Italy", "Belgium", "Austria", "United Kingdom", "Spain", "France", "Portugal",
        "Ukraine", "Bulgaria", "Czechia", "Albania", "Slovakia", "Latvia", "Croatia", "Belarus", "Russia", "Hungary",
        "Estonia", "Moldova", "Lithuania", "Romania", "Poland", "Slovenia", "Macedonia",
        "China", "Japan", "India", "South Korea", "Armenia", "Azerbaijan", "Bangladesh", "Georgia", "Pakistan", "Philippines", "Türkiye",
        "Nigeria","South Africa", "Ghana", # But Ghana is missing in WVS3 or WVS2
        "Australia", "New Zealand"    
      )

# Function to recode country names
rename_country <- function(df, country = country) {
  df |> 
    mutate(country = str_squish(as_character(country))) |> 
    mutate(country = case_match(country,
      "Dominican Republic"               ~ "Domenica",
      "Dominican Rep."                   ~ "Domenica",
      "Czech Republic"                   ~ "Czechia",
      "Russian Federation"               ~ "Russia",
      "Korea, South"                     ~ "South Korea",
      "Korea, Rep."                      ~ "South Korea",
      "Korea, Rep. of"                   ~ "South Korea", 
      "Korea, North"                     ~ "North Korea",
      "Macedonia (Former Yug. Rep)"      ~ "Macedonia",
      "Macedonia, FYR"                   ~ "Macedonia",
      "North Macedonia"                  ~ "Macedonia",
      "Macedonia, TFYR"                  ~ "Macedonia",
      "Moldova, Rep. of"                 ~ "Moldova",
      "Slovak Republic"                  ~ "Slovakia",
      "United States of America"         ~ "United States",
      "Viet Nam"                         ~ "Vietnam",
      "Turkey"                           ~ "Türkiye",
      "Turkiye"                          ~ "Türkiye",
      "Venezuela, RB"                    ~ "Venezuela",
      .default = country
    ))
}

### TRUST ###

ivs_trust_macro <- data_read("Data/workshop_data/ivs_trust_macro.sav")

delhey_newton_2005_ivs <- ivs_trust_macro |> 
  filter(
    source %in% c("WVS3", "WVS2", "EVS2"),
  ) |> 
  arrange(country, desc(source)) |> 
  distinct(country, .keep_all = TRUE)

nrow(delhey_newton_2005_ivs)  # [1] 66

# Add world regions as in Table 1 in Delhey&Newton (2005):
delhey_newton_2005_ivs <- delhey_newton_2005_ivs |> 
  mutate(country = str_squish(country)) |>
  mutate(
    Region = case_when(
      country %in% c(
        "Argentina","Brazil","Chile","Colombia","Mexico","Peru",
        "Uruguay","Venezuela","Canada","United States",
        "Puerto Rico","El Salvador","Dominican Rep."
      ) ~ "Americas",
      country %in% c(
        "Austria","Belgium","Denmark","Finland","France","Germany",
        "Ireland","Italy","Netherlands","Norway","Portugal","Spain",
        "Sweden","Switzerland","United Kingdom","Iceland"
      ) ~ "Western Europe",
      country %in% c(
        "Albania","Armenia","Azerbaijan","Belarus","Bosnia and Herzegovina",
        "Bulgaria","Croatia","Czechia","Estonia","Georgia","Hungary",
        "Latvia","Lithuania","Moldova","Montenegro","North Macedonia","Malta",
        "Poland","Romania","Russia","Serbia","Slovakia","Slovenia","Ukraine"
      ) ~ "Eastern Europe",
      country %in% c(
        "China","India","Japan","Pakistan","Philippines","South Korea",
        "Taiwan","Bangladesh","Türkiye"
      ) ~ "Asia",
      country %in% c("Nigeria","South Africa","Ghana") ~ "Africa",
      country %in% c("Australia","New Zealand") ~ "Oceania",
      TRUE ~ "Other"
    )
  ) |> 
  select(country, Social_trust = trusts_pct, Region)

### ETHNIC FRACTIONALISATION ###

alesina_fract <- data_read(
  "Data/raw/macro/alesina_et_al-2003-fractionalization/2003_fractionalization.xls",
  sheet = "Fractionalization Measures",
  skip = 1
) |> 
  select(
    country = Country,
    Ethnic_fractionalisation = Ethnic
  ) |>
  data_filter(2:216) |> 
  datawizard::convert_to_na(na = ".") |> 
  tidyr::drop_na() |>
  mutate(
    Ethnic_fractionalisation = round(coerce_to_numeric(Ethnic_fractionalisation), 2)
  )

### INCOME INEQUALITY (Gini) and NATIONAL WEALTH (GDP per capita in PPP) ###

gini_a <- data_read("Data/raw/macro/WDI/WB_WDI_SI_POV_GINI.csv") |> 
  select(country = REF_AREA_LABEL, year = TIME_PERIOD, gini = OBS_VALUE) |> 
  filter(year %in% 1994:1996) |>
  mutate(dist = abs(year - 1996)) |> 
  group_by(country) |>
  arrange(dist, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |> 
  select(country, gini_a = gini)

gini_b <- data_read("Data/raw/macro/OECD/oecd_gini_94-97.csv") |> 
  select(country = "Reference area", year = TIME_PERIOD, gini = OBS_VALUE) |> 
  mutate(
    dist = abs(year - 1996),
    gini = gini*100
  ) |> 
  group_by(country) |>
  arrange(dist, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |> 
  select(country, gini_b = gini)

gini_c <- data_read("Data/raw/macro/WDI/Gini.xlsx", range = "A7:AL174") |> 
  select(country = Country, "1989":"1996") |> 
  data_to_long(select = "1989":"1996", names_to = "year", values_to = "gini") |> 
    mutate(
    year = to_numeric(year),
    dist = abs(year - 1996)
  ) |> 
  group_by(country) |>
  arrange(dist, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |> 
  select(country, gini_c = gini) |> 
  drop_na()

gini_d <- data_read("Data/raw/macro/WDI/WB_WDI_SI_POV_GINI.csv") |> 
  select(country = REF_AREA_LABEL, year = TIME_PERIOD, gini = OBS_VALUE) |> 
  # filter(year %in% 1994:1996) |>
  mutate(dist = abs(year - 1996)) |> 
  group_by(country) |>
  arrange(dist, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |> 
  select(country, gini_d = gini)

for (i in c("gini_a", "gini_b", "gini_c", "gini_d")) {
  cleaned <- rename_country(get(i, envir = .GlobalEnv))
  assign(i, cleaned, envir = .GlobalEnv)
}

gini <- reduce(list(gini_a, gini_b, gini_c, gini_d), full_join, by = "country") |> 
  mutate(
    gini = coalesce(gini_a, gini_c, gini_b, gini_d),
    gini = round(gini, 2)
  ) |> 
  select(country, gini)

gdp_pc_ppp <- data_read("Data/raw/macro/WDI/WB_WDI_NY_GDP_PCAP_PP_CD.csv") |> 
  select(country = REF_AREA_LABEL, year = TIME_PERIOD, gdp_pc_ppp = OBS_VALUE) |> 
  # filter(year %in% 1995:1996) |>
  mutate(dist = abs(year - 1995)) |> 
  group_by(country) |>
  arrange(dist, .by_group = TRUE) |>
  slice_head(n = 1) |>
  ungroup() |> 
  mutate(gdp_pc_ppp = round(gdp_pc_ppp, 0)) |> 
  select(country, gdp_pc_ppp)

### QUALITY OF GOVERNMENT and POLITICAL FREEDOM ###

# From Delhey and Newton (working paper): 
#   - The factor quality of government consists of (factor loadings in brackets): 
#      - political stability index (.93)
#      - law and order index (.82)
#      - rule of law index (.98)
#      - government effectiveness index (.97) 
#      - cumulated freedom score (.84)
#   - The explained variance is 83%, and the KMO value .83.
#   - The Human Development Report (2002) provides a collection of subjective indicators of the quality of government, including:
#     - An index political stability and lack of violence is taken from the World Bank
#     - An index of law and order is taken from the International Country Risk Guide (ICRG)
#     - The index of the rule of law is taken from the World Bank
#     - The index of government effectiveness is taken from the World Bank
#   - Political freedom: Freedom House ratings for political rights and civil liberties which, 
#     - for the purposes of this work, were combined into a single political freedom score;
#     - Because it may be some time before political freedom can create a climate of trust, a measure for the mean level of freedom over 20 years (1976-1996) was also constructed (Freedom House score, cumulated 20 years, averaged)
#     - 7 = free, 1 = not free

hdr2002 <- data_read("Data/raw/macro/UN Human Development Reports/un-hdr-2002.csv") |> 
  select(
    country = Country, 
    pol_stability = "Political stability and lack of violence",
    law_order = "Law and order",
    rule_law = "Rule of law",
    gov_effectiveness = "Government effectiveness"
  )

freedom_house <- data_read(
  "Data/raw/macro/Freedom House/Country_and_Territory_Ratings_and_Statuses_FIW_1973-2025_0.xlsx",
  sheet = "PR_CL_76-96",
  skip = 1
) |> 
  # drop any columns whose name starts with "Status"
  select(-starts_with("Status")) |> 
  # convert remaining non-country columns to numeric, treating "-" as NA
  mutate(
    across(-country, ~ na_if(., "-")),
    across(-country, ~ as.numeric(.))
  )

freedom_house |> select(!country) |> ncol() # [1] 40 columns without 'country', two measures, so 20 years

freedom_house  <- freedom_house |> 
  mutate(
    pol_freedom = row_means(freedom_house, exclude = c(country), remove_na = TRUE)  |> 
    # turn NaN values to NA
    replace_nan_inf() |> 
    # reverse code so that 7 = free, 1 = not free
    reverse() |> 
    round(2)
  ) |> 
  select(country, pol_freedom)

### RELIGION ###

religion <- data_read("Data/raw/macro/World Religion Data (v1.1)/WRP_national.csv") |> 
  select(year, state, name, chrstprotpct:sumreligpct
  )

religion <- religion |> 
  mutate(
    country = countrycode::countrycode(
      sourcevar = state,
      origin = "cown",                 # cown: Correlates of War numeric
      destination = "country.name.en",  # country.name.en: country name (English)
    ),
    Protestantism_pct = ifelse(chrstprotpct >= 0.25, chrstprotpct + chrstcatpct + chrstangpct, 0),
    Protestantism_pct2 = ifelse(chrstprotpct >= 0.3, chrstprotpct + chrstcatpct + chrstangpct, 0),
    Protestantism_pct3 = ifelse(chrstprotpct >= 0.4, chrstprotpct + chrstcatpct + chrstangpct, 0),
    Protestantism = ifelse(Protestantism_pct >= 0.5, 1, 0),
    Protestantism_2 = ifelse(Protestantism_pct2 >= 0.5, 1, 0),
    Protestantism_3 = ifelse(Protestantism_pct3 >= 0.5, 1, 0)
) |> 
  filter(year == 1995) |> 
  select(country, Protestantism)

### COMBINE ALL CORRELATES ###

for (i in c("delhey_newton_2005_ivs", "alesina_fract", "gini", "gdp_pc_ppp", "hdr2002", "freedom_house", "religion")) {
  cleaned <- rename_country(get(i, envir = .GlobalEnv))
  assign(i, cleaned, envir = .GlobalEnv)
}

delhey_newton_2005 <- datawizard::data_merge(
  list(
    delhey_newton_2005_ivs, 
    alesina_fract,
    gini,
    gdp_pc_ppp,
    hdr2002,
    freedom_house,
    religion
  ),
  join = "left", 
  by = "country"
) |>
  drop_na(pol_stability) |>
  # filter(country %in% delhey_newton_2005_countries)
  filter(country != "El Salvador")

delhey_newton_2005 <- delhey_newton_2005 |> 
  mutate(
    qual_gov = row_means(
      delhey_newton_2005, 
      select = c("pol_stability":"pol_freedom"), 
      remove_na = TRUE
    ) |> 
      round(2)
  )

delhey_newton_2005 <- delhey_newton_2005 |> 
  rename(
    Country = country,
    Income_inequality = gini,
    National_wealth = gdp_pc_ppp,
    Political_stability = pol_stability,
    Law_Order = law_order,
    Rule_law = rule_law,
    Gov_effectiveness = gov_effectiveness,
    Political_freedom = pol_freedom,
    Quality_of_government = qual_gov
  ) |> 
  var_labels(
    Country = "Country",
    Region = "World region",
    Ethnic_fractionalisation = "Ethnic fractionalisation (Alesina et al. 2003)",
    Income_inequality = "Gini coefficient, 0% (perfect equality) to 100% (perfect inequality)",
    National_wealth = "GDP per capita, PPP (current international $)",
    Political_stability = "Index of political stability and lack of violence (Human Development Report 2002)",
    Law_Order = "Index of law and order (Human Development Report 2002)",
    Rule_law = "Index of the rule of law (Human Development Report 2002)",
    Gov_effectiveness = "Index of government effectiveness (Human Development Report 2002)",
    Political_freedom = "Freedom House scores for civil liberties and political rights, 1976-1996 cumulated average",
    Protestantism = "Significant protestant population (>=25% Protestant and >=50% Protestand and Catholic combined)",
    # Protestantism_2 = "Significant protestant population (>40% Protestant)",
    Quality_of_government = "Quality of government (Factor)"
)

## Checks

lm(Social_trust ~ 0 + Protestantism + Ethnic_fractionalisation, delhey_newton_2005) |> standardize()  |> model_parameters()

# check_kmo(select(delhey_newton_2005, "pol_stability":"pol_freedom"))

principal_components(select(delhey_newton_2005, "Political_stability":"Political_freedom"), 
  # rotation = "varimax",
  sparse = FALSE,
  standardize = TRUE,
  sort = TRUE
)
  
setdiff(delhey_newton_2005_countries, delhey_newton_2005$country) # Ghana
setdiff(delhey_newton_2005$country, delhey_newton_2005_countries) # "El Salvador", "Malta"
  
  
data_write(delhey_newton_2005, "Data/workshop_data/w3/delhey&newton2005.sav")
data_write(delhey_newton_2005, "Data/workshop_data/w3/delhey&newton2005.rds")
