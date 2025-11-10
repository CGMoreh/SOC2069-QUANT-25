if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, ggrepel, easystats, sjlabelled, sjmisc, archive, fs, DT
)


##############################################################################
##########          JOINT EVS/WVS 2017-2022 DATA            ##################
##############################################################################

## Download Joint EVS/WVS 2017-2022 data-set (v5.0; Jun 24, 2024) in SPSS format: https://www.worldvaluessurvey.org/WVSEVSjoint2017.jsp

## Name of the downloaded raw dataset
raw_filename <- "F00012153-EVS_WVS_Joint_Spss_v5_0.zip"

## Find the path to the raw file on the PC
raw_filepath <- fs::dir_ls(path = "\\",                           # set root to "\" on Windows
                           glob = paste0("*", raw_filename), 
                           recurse = TRUE, 
                           fail = FALSE)

## Check contents of the .zip file
archive::archive(raw_filepath)                                         

## Create connection with the data file
filelink <- archive::archive_read(raw_filepath,                        
                                  file = "EVS_WVS_Joint_Spss_v5_0.sav")

## Import the SPSS file to R; name it jvs as in "Joint EVS/WVS" 
jvs2017 <- sjlabelled::read_spss(filelink)

dim(jvs2017)  # [1] 156658    231
levels(jvs2017$cntry)
sjmisc::frq(jvs2017$cntry)




##############################################################################
########## UK-only datasets for slides/worksheet demo       ##################
##############################################################################

#  ESS10 #####################################################################


ess10 <- data_read("Data/raw/ess10/ESS10/ESS10.sav")

ess10_uk <- data_filter(ess10, "cntry == 'GB'")

sjlabelled::write_stata(ess10_uk, "D:/OneDrive - Newcastle University/GitHub_priv/SOC2069-QUANT/Data/workshop_data/ess10_uk.dta")

##############################################################################
### Select small data for data transformation demo with JASP
##############################################################################

data_transformation_all <- data_read("D:/OneDrive - Newcastle University/GitHub_priv/SOC2069-QUANT/Data/workshop_data/ess10_uk.dta", convert_factors = F) |> 
  select(idno, nwspol, netusoft, netustm, ppltrst, pplfair, pplhlp, polintr, lrscale, health, rlgdnm, rlgatnd, wrclmch) |> 
  mutate(ppltrst = as.numeric(ppltrst) - 1)



data_read("D:/OneDrive - Newcastle University/GitHub_priv/SOC2069-QUANT/Data/workshop_data/ess10_uk.dta") |> 
  select(idno, nwspol, netusoft, netustm, ppltrst, pplfair, pplhlp, polintr, lrscale, health, rlgdnm, rlgatnd, wrclmch) |> 
  mutate(ppltrst = as.numeric(ppltrst) - 1,
         pplfair = as.numeric(pplfair) - 1,
         pplhlp = as.numeric(pplhlp) - 1,
         lrscale = as.numeric(lrscale) - 1,
         rlgdnm = as.character(rlgdnm)
         ) |>
  filter(idno %in% c(50378, 60555, 92713, 92730, 55709, 51138, 57305, 71215, 80670, 72206, 89194, 91883, 67396, 87715, 84528)) |> 
  data_write("D:/OneDrive - Newcastle University/GitHub_priv/SOC2069-QUANT/Data/workshop_data/data_transformation.dta")


### Further transformations for renaming and labelling exercises

data_read("D:/OneDrive - Newcastle University/GitHub_priv/SOC2069-QUANT/Data/workshop_data/ess10_uk.dta", convert_factors = F) |> 
  select(idno, nwspol, netusoft, netustm, ppltrst, pplfair, pplhlp, polintr, lrscale, health, rlgdnm, rlgatnd,  wrclmch) |> 
  mutate(ppltrst = as.numeric(ppltrst) - 1,
         pplfair = as.numeric(pplfair) - 1,
         pplhlp = as.numeric(pplhlp) - 1,
         lrscale = as.numeric(lrscale) - 1) |>
  filter(idno %in% c(50378, 60555, 92713, 92730, 55709, 51138, 57305, 71215, 80670, 72206, 89194, 91883, 67396, 87715, 84528)) |> 
  mutate(across(nwspol:wrclmch, ~ as.numeric(.x)),
         A1 = nwspol, 
         A2 = netusoft, 
         A3 = netustm, 
         A4 = ppltrst, 
         A5 = pplfair, 
         A6 = pplhlp, 
         B1 = polintr, 
         B26 = lrscale, 
         C7 = health, 
         C12 = rlgdnm, 
         C16 = rlgatnd, 
         C32 = wrclmch,
         A3 = case_when(idno == 71215 & is.na(A3) ~ 7777,
                        idno == 80670 & is.na(A3) ~ 8888, 
                        TRUE ~ A3),
         A5 = case_when(idno == 80670 & is.na(A5) ~ 88, TRUE ~ A5),
         A6 = case_when(idno == 72206 & is.na(A6) ~ 77, TRUE ~ A6),
         C12 = case_when(idno == 80670 & is.na(C12) ~ 77, TRUE ~ C12)
         ) |> 
  select(idno, A1:C32) |> 
  data_write("D:/OneDrive - Newcastle University/GitHub_priv/SOC2069-QUANT/Data/workshop_data/data_labelling.dta")





#  WVS7 #######################################################################

wvs7 <- data_read("Data/raw/wvs7/F00010733-WVS_Cross-National_Wave_7_spss_v5_0/WVS7_70vars.dta")

wvs7_uk <- wvs7 |> 
  mutate(country = as_numeric(B_COUNTRY) |> 
           as_label(),
         country = case_match(country, 
                              "Great Britain" ~ "United Kingdom",
                              "Northern Ireland" ~ "United Kingdom",
                              .default = country)
  ) |> 
  data_filter("country == 'United Kingdom'")

## Saving but some labelling error when saving to .dta; investigate;
## saving instead to .sav

sjlabelled::write_spss(wvs7_uk, "D:/OneDrive - Newcastle University/GitHub_priv/SOC2069-QUANT/Data/workshop_data/wvs7_uk.sav")






#  Osterman 2021 #####################################################################

raw_osterman <- data_read("Data/raw/Replication_data_ESS1-9_20201113.dta")

model_vars <- c("trustindex3", "reform1_7", "reform_id_num", "female", "agea", "fbrneur", "mbrneur", "fnotbrneur", "mnotbrneur", "blgetmg_d", "yrbrn", "essround")

# Select out cases
osterman_t3 <- raw_osterman  |> 
  filter(agea >= 25 &
         agea <=80 &
         brncntr == 1 &
         reform_years<=7 & 
         reform_years>=-7
         ) |> 
  select("cntry", "dweight", "ppltrst", "pplfair", "pplhlp", 
         "eduyrs25", "paredu_a_high", model_vars) |> 
  drop_na(any_of(model_vars)) |> 
  sjlabelled::val_labels(female = c("Male" = 0, "Female" = 1),
                         blgetmg_d = c("No" = 0, "Yes" = 1),
                         fbrneur = c("No" = 0, "Yes" = 1),
                         mbrneur = c("No" = 0, "Yes" = 1),
                         fnotbrneur = c("No" = 0, "Yes" = 1),
                         mnotbrneur = c("No" = 0, "Yes" = 1),
                         paredu_a_high = c("No" = 0, "Yes" = 1),
                         ) |> 
  sjlabelled::var_labels(trustindex3 = "Social trust scale",
                         reform1_7 = "General reform indicator",
                         reform_id_num = "Reform ID number",
                         female = "Sex",
                         fnotbrneur = "Foreign-born father, outside Europe",
                         mnotbrneur = "Foreign-born mother, outside Europe",
                         blgetmg_d = "Belongs to ethnic minority",
                         eduyrs25 = "Years of full-time education",
                         paredu_a_high = "High parental education"
                         )

osterman_t3 |> data_write("Data/workshop_data/osterman_t3.dta")






