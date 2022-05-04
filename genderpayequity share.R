#####
# Code for: Ganguli et al. (2022). How the primary care physician gender wage gap differs by compensation model: findings from a microsimulation study. 
#####

# INSTRUCTIONS: Open this file in R, see https://www.datacamp.com/community/tutorials/installing-R-windows-mac-ubuntu

##### SECTION 1: Install libraries used in this analysis ##### 
# for information about each library, type ?<packagename>, e.g., ?readr
# for any package that does not load/is not already loaded, type install.packages('<packagename>') to install, e.g., install.packages('readr')
rm(list=ls()) # clear environment of other data
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(icd)
library(tableone)
library(mice)
library(tidyr)
library(MatchIt)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(quantreg)

##### SECTION 2: Download input database data ##### 
# Table25 refers to a patient-encounter-level table in the PRIME database, see access guidelines here:  https://redivis.com/datasets/93g0-4eydgybx5
# CMSCompare refers to the NPI-level provider database, available for download here: https://data.cms.gov/provider-data/dataset/mj5m-pzi6  
# The data here are downloaded to a Mac 'downloads' directory
setwd("~/Downloads") # adjust to your working directory for wherever the data are stored
df <- read_csv("Table_25.csv") # PRIME data

cms <- read_csv("CMSCompare_DOC_Data_NationalDownloadableFile.csv", # national NPI data set
                col_types = cols_only(NPI = col_guess(),
                                      Grd_yr = col_guess()))

# join the two datasets by unique provider NPI number
df = df %>%
  left_join(cms, by = c("npi" = "NPI")) %>%
  mutate(Grd_yr= ifelse(is.na(Grd_yr), mean(Grd_yr, na.rm=TRUE), Grd_yr)) 

##### SECTION 3: Payments by patient encounters ##### 
# estimate payment by type of encounter
# encountertypcode = CPT code for the given visit
# insmcare = Medicare
# insmedcaid = Medicaid
# insself = self-pay
# inspriv = private commercial insurance
df = df %>%
  mutate(payment = insmcare*((encountertypecode=="99201")*46.49+
                               (encountertypecode=="99202")*77.48+
                               (encountertypecode=="99203")*109.92+
                               (encountertypecode=="99204")*166.86+
                               (encountertypecode=="99205")*209.75+
                               (encountertypecode=="99211")*23.07+
                               (encountertypecode=="99212")*45.77+
                               (encountertypecode=="99213")*73.32+
                               (encountertypecode=="99214")*110.28+
                               (encountertypecode=="99215")*147.76+
                               (encountertypecode=="G0402")*169.02+
                               (encountertypecode=="G0438")*174.43+
                               (encountertypecode=="G0439")*118.21)+
           (insmcaid|insself)*((encountertypecode=="99201")*33.85+
                                 (encountertypecode=="99202")*56.42+
                                 (encountertypecode=="99203")*80.04+
                                 (encountertypecode=="99204")*121.51+
                                 (encountertypecode=="99205")*152.74+
                                 (encountertypecode=="99211")*16.80+
                                 (encountertypecode=="99212")*33.33+
                                 (encountertypecode=="99213")*53.39+
                                 (encountertypecode=="99214")*80.31+
                                 (encountertypecode=="99215")*107.60+
                                 (encountertypecode=="G0402")*123.08+
                                 (encountertypecode=="G0438")*127.02+
                                 (encountertypecode=="G0439")*86.08)+
           inspriv*((encountertypecode=="99201")*62.56+
                      (encountertypecode=="99202")*91.16+
                      (encountertypecode=="99203")*132.8+
                      (encountertypecode=="99204")*202.17+
                      (encountertypecode=="99205")*273.69+
                      (encountertypecode=="99211")*45.74+
                      (encountertypecode=="99212")*58.8+
                      (encountertypecode=="99213")*88.51+
                      (encountertypecode=="99214")*131.71+
                      (encountertypecode=="99215")*196.13))
df$payment[(df$payment)==0 & df$inspriv==1] =  131.71
df$payment[(df$payment)==0 & df$insself==1] =  80.31
df$payment[(df$payment)==0 & df$insmcare==1] =  110.28
df$payment[(df$payment)==0 & df$insmcaid==1] =  80.31

# tabulate the number of providers by gender and by practice ID
length(table(df$npi[df$Provider_Gender_Code=="F"]))
length(table(df$npi[df$Provider_Gender_Code=="M"]))
length(table(df$practiceid))

##### SECTION 4: Organize the dataset in terms of provider characteristics ##### 
# this code first creates several variables: prov_gdr = provider gender; gdr = patient gender; enc_newpt = encounter is for new patient visit; enc_estpt = encounter is for established patient; gradyear = year of med school graduation
# group_by clusters the data by provider npi to get the data as summaries of these variables, and max_age, min_age, mean_age is to determine max/min/mean of their patient panel as of calendar year 2019, spec = national specialty code taxonomy: https://taxonomy.nucc.org 
df_prov = df %>%
  mutate(prov_gdr = (Provider_Gender_Code=="F"),
         gdr = 1*(gender=="F" |
                    gender=="female" |
                    gender=="Female" |
                    gender=="FEMALE")+
           0*(gender=="M" |
                gender=="male" |
                gender=="Male" |
                gender=="MALE"),
         enc_newpt = (encountertypecode=="99201") |
           (encountertypecode=="99202")|
           (encountertypecode=="99203")|
           (encountertypecode=="99204")|
           (encountertypecode=="99205"),
         enc_estpt = (encountertypecode=="99211") |
           (encountertypecode=="99212")|
           (encountertypecode=="99213")|
           (encountertypecode=="99214")|
           (encountertypecode=="99215")|
           (encountertypecode=="G0402")|
           (encountertypecode=="G0438")|
           (encountertypecode=="G0439"),
         insmcaid = 1*(insmcaid==1)+0*(insmcare==1),
         gradyear = as.numeric(Grd_yr)) %>%
  group_by(npi) %>%
  summarise(encounters = n(),
            panel = encounters/3.19,
            prov_gdr = max(prov_gdr),
            max_age = 2019-min(birthyear),
            min_age = 2019-max(birthyear),
            mean_age = 2019-mean(birthyear),
            gdr = mean(gdr),
            new_pts = mean(enc_newpt),
            est_pts = mean(enc_estpt),
            insmcaid = mean(insmcaid),
            insmcare = mean(insmcare),
            inspriv = mean(inspriv),
            insself = mean(insself),
            yrs_prac = 2019-max(gradyear),
            spec = Healthcare_Provider_Taxonomy_Code_1) %>%
  distinct()

##### SECTION 5: Create initial table of unmatched patient characteristics by provider gender ##### 
# uses the CreateTableOne package to define variables in table, specify which variables are factor variables, and prints the table stratified by provider gender
vars = c("min_age", 
         "max_age",
         "mean_age",
         "gdr", 
         "new_pts", 
         "est_pts",
         "insmcaid", 
         "insmcare", 
         "insself", 
         "inspriv",
         "encounters",
         "yrs_prac")
varsToFactor <- c("Provider_Gender_Code",
                  "gender",
                  "insmcaid",
                  "insmcare",
                  "insself",
                  "inspriv")
tableOne <- CreateTableOne(vars = vars, 
                           strata = "prov_gdr", 
                           data = df_prov)
print(tableOne, quote = TRUE, noSpaces = TRUE)

##### SECTION 6: Organize the dataset in terms of annual revenue by provider ##### 
# Groups by both NPI and encounter year to get annual revenue
# creates variables encounters  = total visits, patients = distinct patient IDs during that period, gross_rev = gross revenue
df_rev = df %>%
  mutate(prov_gdr = (Provider_Gender_Code=="F"),
         enc_year = year(encounterdate)) %>%
  group_by(npi,enc_year) %>%
  summarise(encounters = n(),
            patients = n_distinct(patientuid),
            prov_gdr = max(prov_gdr),
            gross_rev = sum(payment)) %>%
  filter(encounters>limit) %>%
  ungroup() %>%
  group_by(npi) %>%
  summarise(encounters = mean(encounters,na.rm=T),
            gross_rev = mean(gross_rev,na.rm=T),
            prov_gdr = max(prov_gdr)) %>%
  ungroup() %>%
  select(npi, gross_rev, prov_gdr)

##### SECTION 7: Create initial table of unmatched gross revenue by provider gender ##### 
# uses the CreateTableOne package to define variables in table, and prints the table
vars = c("encounters","gross_rev")
tableTwo <- CreateTableOne(vars = vars, 
                           strata = c("prov_gdr"), 
                           data = df_rev)
print(tableTwo, quote = TRUE, noSpaces = TRUE)


##### SECTION 8: Calculate Charleson and HCC risk scores by patient ##### 
# creates separate datasets for each of the two risk scores
# the encounter diagnosis codes are organized and translated into icd10 diagnoses and then charlson is computed using the icd library
df_charl = df %>%
  select(patientuid,encounterdiagnosiscode) %>%
  mutate(icd10 = encounterdiagnosiscode) %>%
  select(patientuid,icd10)
df_charl_sub = df_charl %>%
  select(patientuid) %>%
  distinct()
df_charl = cbind(df_charl_sub,charlson(df_charl))
df_charl$charl = df_charl$`charlson(df_charl)`

# the encounter diagnoses are turned into icd_name and date and then icd10_comorbid_hcc is used from the icd library to compute the hcc score
df_hcc = df %>%
  select(patientuid,encounterdiagnosiscode,encounterdate) %>%
  mutate(icd_name = encounterdiagnosiscode,
         date = as.Date(encounterdate)) %>%
  select(patientuid,icd_name, date)
df_hcc$visit_name = df_hcc %>% group_indices(patientuid)
df_hcc$visit_name = as.character(df_hcc$visit_name)
df_hcc_sub = df_hcc %>%
  select(patientuid, visit_name) %>%
  distinct()
df_hcc = df_hcc %>% select(-patientuid)
df_hcc = icd10_comorbid_hcc(df_hcc)
df_hcc = df_hcc %>%
  full_join(df_hcc_sub, by = c("visit_name" = "visit_name"))
df_hcc$hcc[is.na(df_hcc$hcc)] = 1

##### SECTION 9: organize dataset of practice revenue by practice id ##### 
# this data organization method counts how many providers are associated with each practice
df_rev = df_rev %>%
  select(npi, gross_rev)
df_prov = df_prov %>%
  select(npi, gdr, new_pts, est_pts, yrs_prac, spec, prov_gdr)
pracsize = df %>%
  select(practiceid,npi) %>%
  distinct() %>%
  group_by(practiceid) %>%
  summarise(count = n()) %>%
  mutate(grp_size = count) %>%
  select(practiceid, grp_size) %>%
  distinct()

##### SECTION 10: add risk scores, revenue, and practice size variables to main dataset ##### 
# this transformation first adds the charleson and hcc risk scores, then the practice-level revenues and practice size data to the main dataset
# we then define the above-mentioned variables and summarize them by provider NPI
df_full = df %>%
  left_join(df_charl, by = c("patientuid" = "patientuid")) %>%
  left_join(df_hcc, by = c("patientuid" = "patientuid")) %>%
  left_join(df_rev, by = c("npi" = "npi")) %>%
  left_join(pracsize, by = c("practiceid" = "practiceid")) %>%
  mutate(prov_gdr = (Provider_Gender_Code=="F"), 
         gdr = 1*(gender=="F" |
                    gender=="female" |
                    gender=="Female" |
                    gender=="FEMALE")+
           0*(gender=="M" |
                gender=="male" |
                gender=="Male" |
                gender=="MALE"),
         enc_newpt = (encountertypecode=="99201") |
           (encountertypecode=="99202")|
           (encountertypecode=="99203")|
           (encountertypecode=="99204")|
           (encountertypecode=="99205"),
         enc_estpt = (encountertypecode=="99211") |
           (encountertypecode=="99212")|
           (encountertypecode=="99213")|
           (encountertypecode=="99214")|
           (encountertypecode=="99215")|
           (encountertypecode=="G0402")|
           (encountertypecode=="G0438")|
           (encountertypecode=="G0439"),
         insmcaid = 1*(insmcaid==1)+0*(insmcare==1),
         gradyear = as.numeric(Grd_yr)) %>%
  group_by(npi) %>%
  summarise(prov_gdr = max(prov_gdr),
            charl = mean(charl,na.omit=T),
            hcc = mean(hcc,na.omit=T),
            encounters = n(),
            prov_gdr = max(prov_gdr),
            max_age = 2019-min(birthyear),
            min_age = 2019-max(birthyear),
            mean_age = 2019-mean(birthyear),
            gdr = mean(gdr),
            new_pts = mean(enc_newpt),
            est_pts = mean(enc_estpt),
            insmcaid = mean(insmcaid),
            insmcare = mean(insmcare),
            inspriv = mean(inspriv),
            insself = mean(insself),
            yrs_prac = 2019-max(gradyear),
            spec = Healthcare_Provider_Taxonomy_Code_1,
            practiceid = practiceid,
            gross_rev = mean(gross_rev),
            Grd_yr = mean(Grd_yr),
            grp_size = mean(grp_size)) %>%
  select(mean_age, practiceid, npi, charl, hcc, gross_rev, prov_gdr, Grd_yr, 
         spec,encounters,min_age,max_age, yrs_prac, grp_size,
         gdr, new_pts, est_pts, insmcaid, insmcare, insself, inspriv) %>%
  distinct()


##### SECTION 11: imputation ##### 
# using multiple imputation with chained equations, using the classification and regression tree method in the mice package
df_full_imp <- mice(df_full,m=1,maxit=50,seed=123,method="cart")
summary(df_full_imp)

# create the complete dataset
df_full_comp <- complete(df_full_imp,1)

# create a table to describe the provider-level patient panel characteristics
# yrs_prac = years in practice; mean_age = mean patient age; max_age = max age in panel; min_age = min patient age in panel; gross_rev = gross revenues from provider; sessions = # of half day sessions per week; encounters = annual encounters; panel = unique patients in panel; spec = specialty; grp_size = # of providers in practice id
vars = c("yrs_prac", "mean_age","min_age", "max_age", "gross_rev","sessions", "encounters", "panel", "spec", "grp_size",
         "gdr", "new_pts", "est_pts", "insmcaid", "insmcare", "insself", "inspriv", "charl","hcc")
tableFour <- CreateTableOne(vars = vars, 
                             strata = c("prov_gdr"), 
                             data = df_full_comp)
print(tableFour, quote = TRUE, noSpaces = TRUE) # main text table 1, pre-match


##### SECTION 12: matching ##### 
# uses the coarsened exact matching (cem) in the matchit package to match providers by years in practice, specialty, practice id [location]
match.it <- matchit(prov_gdr ~ yrs_prac + spec + practiceid + encounters, data = df_full_comp, method="cem")
# create the matched dataset
df.match <- match.data(match.it)[1:ncol(df_full_comp)]
summary(match.it)
# summarize the matched data in a table by 
tableFive <- CreateTableOne(vars = vars, 
                            strata = c("prov_gdr"), 
                            data = df.match)
print(tableFive, quote = TRUE, noSpaces = TRUE) # main text table 1, post-match


##### SECTION 13: alternative compensation models ##### 
# summarize the panel size, age, patient gender distribution and risk scores at the practice level for computing the alternative compensation models
df_prac = df_full_comp %>%
  select(practiceid, npi, gross_rev, panel, charl, hcc, mean_age, gdr) %>%
  distinct() %>%
  group_by(practiceid) %>%
  summarise(pool = sum(gross_rev)* (1-0.34), # overhead rate
            prac_panel = sum(panel),
            prac_age = mean(mean_age),
            prac_gdr = mean(gdr),
            prac_charl = mean(charl),
            prac_hcc = mean(hcc)) %>%
  distinct()

# compute the alternative payments: ffs_ = fee for service; cap_ = capitated; ra_ = risk adjusted, followed by variables risk adjusted against, _o = only, hybrid_ = hybrid payment model
df_sal = df_full_comp %>%
  select(practiceid, npi, panel, mean_age, gdr, charl,hcc, gross_rev, prov_gdr, yrs_prac, spec, grp_size, encounters) %>%
  distinct() %>%
  left_join(df_prac, by = c("practiceid" = "practiceid")) %>%
  mutate(ffs_salary = gross_rev * (1-0.34) ,
         cap_salary = pool * panel/prac_panel ,
         cap_salary_ra_age = pool * panel/prac_panel * mean_age/prac_age ,
         cap_salary_ra_age_sex = pool * panel/prac_panel * mean_age/prac_age * gdr/prac_gdr ,
         cap_salary_ra_charl = pool * panel/prac_panel * mean_age/prac_age * gdr/prac_gdr * charl/prac_charl ,
         cap_salary_ra_hcc_o = pool * panel/prac_panel * hcc/prac_hcc ,
         cap_salary_ra_charl_o = pool * panel/prac_panel * charl/prac_charl ,
         cap_salary_ra_hcc = pool * panel/prac_panel * mean_age/prac_age * gdr/prac_gdr * hcc/prac_hcc ,
         hybrid_charl = ffs_salary * 0.8 + cap_salary_ra_charl * 0.2, # hybrid models have 80% base ffs and 20% capitated
         hybrid_hcc = ffs_salary * 0.8 + cap_salary_ra_hcc * 0.2) %>%
  select(practiceid, prov_gdr, hybrid_charl, hybrid_hcc, ffs_salary, cap_salary, encounters,
         cap_salary_ra_age,cap_salary_ra_age_sex, cap_salary_ra_charl, cap_salary_ra_hcc, cap_salary_ra_charl_o, cap_salary_ra_hcc_o, 
         yrs_prac, spec, grp_size) %>%
  distinct()

##### SECTION 14: summarize results  ##### 
# compute the difference in results by provider gender across each compensation model
# here we do a quantile regression of salary vs provider gender and show the resulting tabulation of summary statistics around the estimate
# tau refers to the quantile estimated, e.g., tau = 0.5 corresponds to the median
# alternative payments: ffs_ = fee for service; cap_ = capitated; ra_ = risk adjusted, followed by variables risk adjusted against, _o = only, hybrid_ = hybrid payment model
match.it2 <- matchit(prov_gdr ~ yrs_prac + spec + practiceid + encounters, data = df_sal, method="cem")
df.match2 <- match.data(match.it2)[1:ncol(df_sal)]
vars = c("encounters", "ffs_salary", "cap_salary", "cap_salary_ra_age","cap_salary_ra_age_sex", "cap_salary_ra_charl", "cap_salary_ra_hcc", "cap_salary_ra_charl_o", "cap_salary_ra_hcc_o", "hybrid_charl", "hybrid_hcc")
tableSix <- CreateTableOne(vars = vars, 
                           strata = c("prov_gdr"), 
                           data = df.match2)
print(tableSix, quote = TRUE, noSpaces = TRUE) # main text table 2

fit1 <- rq(ffs_salary ~ prov_gdr, tau = 0.5, data = df.match2)
tab_model(fit1) #median
fit1a <- rq(ffs_salary ~ prov_gdr, tau = 0.25, data = df.match2)
tab_model(fit1a) #25th centile
fit1b <- rq(ffs_salary ~ prov_gdr, tau = 0.75, data = df.match2)
tab_model(fit1b) #75% centile

fit2 <- rq(cap_salary ~ prov_gdr, tau = 0.5, data = df.match2)
tab_model(fit2)
fit2a <- rq(cap_salary ~ prov_gdr, tau = 0.25, data = df.match2)
tab_model(fit2a)
fit2b <- rq(cap_salary ~ prov_gdr, tau = 0.75, data = df.match2)
tab_model(fit2b)

fit3 <- rq(cap_salary_ra_age ~ prov_gdr, tau = 0.5, data = df.match2)
tab_model(fit3)
fit3a <- rq(cap_salary_ra_age ~ prov_gdr, tau = 0.25, data = df.match2)
tab_model(fit3a)
fit3b <- rq(cap_salary_ra_age ~ prov_gdr, tau = 0.75, data = df.match2)
tab_model(fit3b)

fit4 <- rq(cap_salary_ra_age_sex ~ prov_gdr, tau = 0.5, data = df.match2)
tab_model(fit4)
fit4a <- rq(cap_salary_ra_age_sex ~ prov_gdr, tau = 0.25, data = df.match2)
tab_model(fit4a)
fit4b <- rq(cap_salary_ra_age_sex ~ prov_gdr, tau = 0.75, data = df.match2)
tab_model(fit4b)

fit5 <- rq(cap_salary_ra_charl ~ prov_gdr, tau = 0.5, data = df.match2)
tab_model(fit5)
fit5a <- rq(cap_salary_ra_charl ~ prov_gdr, tau = 0.25, data = df.match2)
tab_model(fit5a)
fit5b <- rq(cap_salary_ra_charl ~ prov_gdr, tau = 0.75, data = df.match2)
tab_model(fit5b)

fit6 <- rq(cap_salary_ra_hcc ~ prov_gdr, tau = 0.5, data = df.match2)
tab_model(fit6)
fit6a <- rq(cap_salary_ra_hcc ~ prov_gdr, tau = 0.25, data = df.match2)
tab_model(fit6a)
fit6b <- rq(cap_salary_ra_hcc ~ prov_gdr, tau = 0.75, data = df.match2)
tab_model(fit6b)

fit7 <- rq(cap_salary_ra_charl_o ~ prov_gdr, tau = 0.5, data = df.match2)
tab_model(fit7)
fit7a <- rq(cap_salary_ra_charl_o ~ prov_gdr, tau = 0.25, data = df.match2)
tab_model(fit7a)
fit7b <- rq(cap_salary_ra_charl_o ~ prov_gdr, tau = 0.75, data = df.match2)
tab_model(fit7b)

fit8 <- rq(cap_salary_ra_hcc_o ~ prov_gdr, tau = 0.5, data = df.match2)
tab_model(fit8)
fit8a <- rq(cap_salary_ra_hcc_o ~ prov_gdr, tau = 0.25, data = df.match2)
tab_model(fit8a)
fit8b <- rq(cap_salary_ra_hcc_o ~ prov_gdr, tau = 0.75, data = df.match2)
tab_model(fit8b)

fit9 <- rq(hybrid_charl ~ prov_gdr, tau = 0.5, data = df.match2)
tab_model(fit9)
fit9a <- rq(hybrid_charl ~ prov_gdr, tau = 0.25, data = df.match2)
tab_model(fit9a)
fit9b <- rq(hybrid_charl ~ prov_gdr, tau = 0.75, data = df.match2)
tab_model(fit9b)

fit10 <- rq(hybrid_hcc ~ prov_gdr, tau = 0.5, data = df.match2)
tab_model(fit10)
fit10a <- rq(hybrid_hcc ~ prov_gdr, tau = 0.25, data = df.match2)
tab_model(fit10a)
fit10b <- rq(hybrid_hcc ~ prov_gdr, tau = 0.75, data = df.match2)
tab_model(fit10b)

