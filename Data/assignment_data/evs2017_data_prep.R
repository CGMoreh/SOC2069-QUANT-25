###############################################################################################
#####
#####  Data preparation script for SOC2069 
#####  
#####   SOC2069 is an introductory quantitative methods module (course) for 2nd-year sociology 
#####   students at Newcastle University (United Kingdom)
#####
#####   The aim of this script is to prepare original raw data from the European Values Study 2017
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

### EVS2017 data preparation ##################################################################

## Select variables for each assignment question

evs_vars <- c(paste0("v", c(
          ### Are religious people more satisfied with life?
            6, 9, 51:56, 57, 63, 64,       # religion
            39,                            # life satisfaction
          
          ### Are older people more likely to see the death penalty as justifiable?
            163,                           # death penalty
          
          ### What factors are associated with opinions about future European Union enlargement among Europeans?
            198,                          # Opinion on EU enlargement
          
          ### Is higher internet/social media use associated with stronger anti-immigrant sentiments?
            24, 184:188,                  # attitudes to immigration
            211,                          # NOT internet use per se, but specifically "social media"
          
          ### How does victimisation relate to trust in the police?
            # N/A Missing variables on "victimisation"       
          
          ### What factors are associated with belief in life after death?
            58,                            # Belief in life after death
          
          ### Are government/public sector employees more inclined to perceive higher levels of corruption than those working in the private sector?
            # N/A Missing variables on "corruption"        
          
          ### Various covariates 
            8, 102:107, 142, 143, 144, 164:168, 169, 170, 
            225, 227, 230, 232, 234, 237, "239_r", "243_r", 244, 249, 259, 260, 261, "261_ppp", "261_r"
            )
          ),
          ### Administrative and text-coded
          "age", "age_r", 
          "country", "country_iso2", "country_iso3" # country to be created
)


## Download Version 5.0.0 of the European Values Study 2017: Integrated Dataset (EVS 2017): 
## https://doi.org/10.4232/1.13897

## Name of the downloaded raw .zip folder and the SPSS dataset name
zipfile <- "ZA7500_v5-0-0.sav.zip"
datafile <- "ZA7500_v5-0-0.sav"

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
evs2017 <- sjlabelled::read_spss(filelink)

## Checks
dim(evs2017)  ## [1] 59438   474

## Recode country
#country_names <- get_labels(evs2017$country,       drop.unused = TRUE)
#country_codes <- get_labels(evs2017$c_abrv,        drop.unused = TRUE)

evs2017 <- evs2017 |> 
  mutate(country = labels_to_levels(country),
         country_iso2 = c_abrv,
         country_iso3 = countrycode::countrycode(country_iso2, origin = "iso2c", destination = "iso3c")
  ) 

## Select out a limited number of variables
evs2017_small <- evs2017 |> 
  select(all_of(evs_vars)) |> 
  relocate(starts_with("country"))

country_names <- get_labels(evs2017_small$country,      drop.unused = TRUE)
country_codes <- get_labels(evs2017_small$country_iso3, drop.unused = TRUE)

## Save the dataset
evs2017_small |> 
  sjlabelled::write_spss("Data/assignment_data/EVS2017/evs2017.sav")

## Break down and export dataset by country
for (c in country_codes) {
  evs2017_small |> 
    filter(country_iso3 == c) |> 
    sjlabelled::write_spss(paste0("Data/assignment_data/EVS2017/evs2017_", c, ".sav"))
}
