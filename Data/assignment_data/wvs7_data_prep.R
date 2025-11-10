###############################################################################################
#####
#####  Data preparation script for SOC2069 
#####  
#####   SOC2069 is an introductory quantitative methods module (course) for 2nd-year sociology 
#####   students at Newcastle University (United Kingdom)
#####
#####   The aim of this script is to prepare original raw data from the World Values Survey, Wave 7
#####   for use in workshops and assignments by students
#####
#####   The script provides a complete data management pipeline from the raw data files that are
#####   freely available (upon registration) from the survey project's website
#####
#####   The datasets produced are for single countries and contain only a limited number of variables 
#####   selected for answering the specific toy research questions set as an assignment task.
#####   They are therefore not suitable for real research purposes. 
#####   
#####  Prepared by: Dr. Chris Moreh                           
#####
#####  Date: October 2024                                   
#####
###############################################################################################


## Install and load packages ##################################################################

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, easystats, sjlabelled, sjmisc, archive, fs, DT
)

### WVS7 data preparation #####################################################################

## Select variables for each assignment question

wvs_vars <- c(paste0("Q", c(
          ### Are religious people more satisfied with life?
            6, 165, 171, 172, 173, 289,   # religion
            49,                           # life satisfaction
            
          ### Are older people more likely to see the death penalty as justifiable?
            195,                          # death penalty
            262,                          # age
            
          ### What factors are associated with opinions about future European Union enlargement among Europeans?
               # N/A
            
          ### Is higher internet/social media use associated with stronger anti-immigrant sentiments?
            21, 121:130,                  # attitudes to immigration
            201:212,                      # information source (incl. internet, social media, email, mobile phone, etc.)
            
          ### How does victimisation relate to trust in the police?
            69,                           # confidence in the police
            144, 145,                     # personal and family victimization
            
          ### What factors are associated with belief in life after death?
            166,                          # belief in life after death
            
          ### Are government/public sector employees more inclined to perceive higher levels of corruption than those working in the private sector?
            112:117,                      # perceptions of corruption overall and in several institutions
            284,                          # sector of employment
            
          ### Various covariates
            47, 50, 52, 56,
            64:89,                        # confidence in institutions
            106:111,                      # political/economic values
            131:138,                      # neighbourhood safety
            260, 263:265, 273:275, 
            277, 278, 279, 287, 288
            )
          ),
          ### Administrative
          "H_URBRURAL", "country", "country_iso2", "country_iso3" # country to be created
)


## Download the final (6.0) version of the World Values Survey (WVS) wave 7 data in SPSS format: 
## https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp

## Name of the downloaded raw .zip folder and the SPSS dataset name
zipfile <- "F00010733-WVS_Cross-National_Wave_7_spss_v6_0.zip"
datafile <- "WVS_Cross-National_Wave_7_spss_v6_0.sav"

## Find the path to the raw file on the PC
zipfile_path <- fs::dir_ls(glob = paste0("*", zipfile), 
                           recurse = TRUE, 
                           fail = FALSE)

## Check contents of the .zip file
archive::archive(zipfile_path)                                         

## Create connection link with the data file in the .zip
filelink <- archive::archive_read(zipfile_path,                        
                                  file = datafile)

## Import the SPSS file to R
wvs7 <- sjlabelled::read_spss(filelink)

## Checks
dim(wvs7)  # [1] 97220   613

## Recode country
country_names <- get_labels(wvs7$B_COUNTRY,       drop.unused = TRUE)
country_codes <- get_labels(wvs7$B_COUNTRY_ALPHA, drop.unused = TRUE)

wvs7 <- wvs7 |> 
        mutate(country = labels_to_levels(B_COUNTRY),
               country = case_match(country, 
                                    "Great Britain" ~ "United Kingdom",
                                    "Northern Ireland" ~ "United Kingdom",
                                    .default = country),
               country_iso3 = case_match(B_COUNTRY_ALPHA, 
                                    "GBR" ~ "GBR",
                                    "NIR" ~ "GBR",
                                    .default = B_COUNTRY_ALPHA),
               country_iso2 = countrycode::countrycode(country_iso3, origin = "iso3c", destination = "iso2c")
               ) 

## Select out a limited number of variables
wvs7_small <- wvs7 |> 
  select(all_of(wvs_vars)) |> 
  relocate(starts_with("country"))

country_codes <- get_labels(wvs7_small$country_iso3, drop.unused = TRUE)
country_names <- get_labels(wvs7_small$country,      drop.unused = TRUE)

## Save the dataset
wvs7_small |> 
  sjlabelled::write_spss("Data/assignment_data/WVS7/wvs7.sav")

## Break down and export dataset by country
for (c in country_codes) {
  wvs7_small |> 
    filter(country_iso3 == c) |> 
    sjlabelled::write_spss(paste0("Data/assignment_data/WVS7/wvs7_", c, ".sav"))
}

