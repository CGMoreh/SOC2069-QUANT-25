### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
### ----------------------------------------------------------------------- ###  
### ---- Preparing World Values Study / European Values Study data -----    ### 
### ----------------------------------------------------------------------- ###  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  
### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###  

##### Packages #####

# if (!require("pak")) install.packages("pak")
# 
# pak::pkg_install("xfun")
# 
# xfun::pkg_attach("tidyverse",
#                  "ggrepel",
#                  "strengejacke",
#                  "easystats",
#                  "archive",
#                  "fs",
#                  install = "pak" # requires the {pak} packages to run pak::pkg_install
#                  )

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, ggrepel, easystats, sjlabelled, archive, fs
)

##### Wave 7 only #####


#### Get country-level macro variables for the latest available date ##### 


## Raw datafiles are large, so I store them outside the project in compressed format
## and load them from there for different projects

## Copy the original folder to clipboard and read the the clipboard content in:
# D:\OneDrive - Newcastle University\DATA\EVS_WVS\WVS_7

# datafolder <- readClipboard()

## File name and create readable connection to the compressed file

fs::dir_ls("Data/raw/wvs7")

datafile <- "Data/raw/wvs7/F00010733-WVS_Cross-National_Wave_7_spss_v5_0.zip"

# d <- fs::path(datafolder, datafile)

d <- archive::archive_read(datafile)


# Complete dataset
wvs7 <- sjlabelled::read_spss(d)


# Trust and country-level variables
wvs7small <- wvs7 |>  
  select(B_COUNTRY, B_COUNTRY_ALPHA, C_COW_NUM, C_COW_ALPHA,
         Q57, 
         GDPpercap1, militaryexp, educationexp, healthexp, clfhscore, polity, 
         incrichest10p, giniWB, lifeexpect, urbanpop, easeofbusiness, Trade,
         medageun, meanschooling) |> 
  rename(trust = Q57)


# w$trust_d <- sjmisc::rec(w$trust, rec = "2=0; 1=1; else=NA")

# w$country <- (as_factor(as_character(as_numeric(w$B_COUNTRY))))

## remove individual-level variables

nrow(wvs7small)
nrow(distinct(wvs7small))



ctry_averages <- wvs7small |> 
  mutate(country = as_numeric(B_COUNTRY) |> 
           as_label(),
         country = case_match(country, 
                              "Great Britain" ~ "United Kingdom",
                              "Northern Ireland" ~ "United Kingdom",
                              .default = country),                              
         trust_d = sjmisc::rec(trust, 
                               rec = "2=0; 1=1; else=NA")) |> 
  mutate(trust_pct = round(mean(trust_d, na.rm = T) * 100, 2), 
         .by = country) |>                                                      # instead of `group_by` (https://dplyr.tidyverse.org/reference/dplyr_by.html)
  select(-c(trust, trust_d)) |> 
  distinct(country, .keep_all = TRUE)

nrow(distinct(ctry_averages))

identical(nrow(ctry_averages), 
          nrow(distinct(ctry_averages))
          )






##### Joint EVS/WVS 2017-2022 #####

fs::dir_ls(datafolder)

datafile <- "EVS_WVS_Joint_Spss_v4_0.7z"

d <- fs::path(datafolder, datafile)

d <- archive::archive_read(d)

# Whole dataset
joint <- sjlabelled::read_spss(d)

joint_small <- joint |> 
  select(cntry, cntry_AN, A165) |> 
  rename(trust = A165) |> 
  mutate(country = as_numeric(cntry) |> 
            as_label(),
         country = case_match(country, 
                              "Great Britain" ~ "United Kingdom",
                              "Northern Ireland" ~ "United Kingdom",
                              .default = country),  
         trust_d = sjmisc::rec(trust, 
                               rec = "2=0; 1=1; else=NA")) |> 
  mutate(trust_pct = round(mean(trust_d, na.rm = T) * 100, 2), 
         .by = country)                                                     # instead of `group_by` (https://dplyr.tidyverse.org/reference/dplyr_by.html)


nrow(joint_small)
nrow(distinct(joint_small)) # 257


joint_cntry_averages <- joint_small |> 
  select(-c(trust, trust_d, cntry)) |> 
  distinct(country, .keep_all = TRUE) |> 
  var_labels(trust_pct = "% people who agree that “most people can be trusted”")
             

nrow(distinct(joint_cntry_averages)) # 89

identical(nrow(joint_cntry_averages), 
          nrow(distinct(joint_cntry_averages))
)

### Problem: the country-level variables in the ctry-averages dataset are not for all the countries on the joint... dataset
### Need to get those macros variables from elsewhere
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


     