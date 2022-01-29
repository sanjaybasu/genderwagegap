rm(list=ls())
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

setwd("~/Downloads")
df <- read_csv("Table_25.csv")

cms <- read_csv("CMSCompare_DOC_Data_NationalDownloadableFile.csv",
                col_types = cols_only(NPI = col_guess(),
                                      Grd_yr = col_guess()))


df = df %>%
  left_join(cms, by = c("npi" = "NPI")) %>%
  mutate(Grd_yr= ifelse(is.na(Grd_yr), mean(Grd_yr, na.rm=TRUE), Grd_yr)) 


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


length(table(df$npi[df$Provider_Gender_Code=="F"]))
length(table(df$npi[df$Provider_Gender_Code=="M"]))
length(table(df$practiceid))

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


vars = c("encounters","gross_rev")
tableTwo <- CreateTableOne(vars = vars, 
                           strata = c("prov_gdr"), 
                           data = df_rev)

print(tableTwo, quote = TRUE, noSpaces = TRUE)



df_charl = df %>%
  select(patientuid,encounterdiagnosiscode) %>%
  mutate(icd10 = encounterdiagnosiscode) %>%
  select(patientuid,icd10)
df_charl_sub = df_charl %>%
  select(patientuid) %>%
  distinct()
df_charl = cbind(df_charl_sub,charlson(df_charl))
df_charl$charl = df_charl$`charlson(df_charl)`


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

df_full_imp <- mice(df_full,m=1,maxit=50,seed=123,method="cart")
summary(df_full_imp)

df_full_comp <- complete(df_full_imp,1)

vars = c("yrs_prac", "mean_age","min_age", "max_age", "gross_rev","sessions", "encounters", "panel", "spec", "grp_size",
         "gdr", "new_pts", "est_pts", "insmcaid", "insmcare", "insself", "inspriv", "charl","hcc")
tableFour <- CreateTableOne(vars = vars, 
                             strata = c("prov_gdr"), 
                             data = df_full_comp)

print(tableFour, quote = TRUE, noSpaces = TRUE)



match.it <- matchit(prov_gdr ~ yrs_prac + spec + practiceid + grp_size + encounters , data = df_full_comp, method="cem")
df.match <- match.data(match.it)[1:ncol(df_full_comp)]

summary(match.it)

tableFive <- CreateTableOne(vars = vars, 
                            strata = c("prov_gdr"), 
                            data = df.match)

print(tableFive, quote = TRUE, noSpaces = TRUE)

df_prac = df_full_comp %>%
  select(practiceid, npi, gross_rev, panel, charl, hcc, mean_age, gdr) %>%
  distinct() %>%
  group_by(practiceid) %>%
  summarise(pool = sum(gross_rev)* (1-0.34),
            prac_panel = sum(panel),
            prac_age = mean(mean_age),
            prac_gdr = mean(gdr),
            prac_charl = mean(charl),
            prac_hcc = mean(hcc)) %>%
  distinct()

calibrate = 269868/mean(df_full_comp$gross_rev*(1-0.34))

df_sal = df_full_comp %>%
  select(practiceid, npi, panel, mean_age, gdr, charl,hcc, gross_rev, prov_gdr, yrs_prac, spec, grp_size, encounters) %>%
  distinct() %>%
  left_join(df_prac, by = c("practiceid" = "practiceid")) %>%
  mutate(ffs_salary = gross_rev * (1-0.34) * calibrate,
         cap_salary = pool * panel/prac_panel * calibrate,
         cap_salary_ra_age = pool * panel/prac_panel * mean_age/prac_age * calibrate,
         cap_salary_ra_age_sex = pool * panel/prac_panel * mean_age/prac_age * gdr/prac_gdr * calibrate,
         cap_salary_ra_charl = pool * panel/prac_panel * mean_age/prac_age * gdr/prac_gdr * charl/prac_charl * calibrate,
         cap_salary_ra_hcc_o = pool * panel/prac_panel * hcc/prac_hcc * calibrate,
         cap_salary_ra_charl_o = pool * panel/prac_panel * charl/prac_charl * calibrate,
         cap_salary_ra_hcc = pool * panel/prac_panel * mean_age/prac_age * gdr/prac_gdr * hcc/prac_hcc * calibrate,
         hybrid_charl = ffs_salary * 0.8 + cap_salary_ra_charl * 0.2,
         hybrid_hcc = ffs_salary * 0.8 + cap_salary_ra_hcc * 0.2) %>%
  select(practiceid, prov_gdr, hybrid_charl, hybrid_hcc, ffs_salary, cap_salary, encounters,
         cap_salary_ra_age,cap_salary_ra_age_sex, cap_salary_ra_charl, cap_salary_ra_hcc, cap_salary_ra_charl_o, cap_salary_ra_hcc_o, 
         yrs_prac, spec, grp_size) %>%
  distinct()



match.it2 <- matchit(prov_gdr ~ yrs_prac + spec + practiceid + grp_size + encounters , data = df_sal, method="cem")
df.match2 <- match.data(match.it2)[1:ncol(df_sal)]

vars = c("encounters", "ffs_salary", "cap_salary", "cap_salary_ra_age","cap_salary_ra_age_sex", "cap_salary_ra_charl", "cap_salary_ra_hcc", "cap_salary_ra_charl_o", "cap_salary_ra_hcc_o", "hybrid_charl", "hybrid_hcc")
tableSix <- CreateTableOne(vars = vars, 
                            strata = c("prov_gdr"), 
                            data = df.match2)

print(tableSix, quote = TRUE, noSpaces = TRUE)


fit1 <- lm(ffs_salary ~ prov_gdr, data = df.match2)
tab_model(fit1)


fit2 <- lm(cap_salary ~ prov_gdr, data = df.match2)
tab_model(fit2)


fit3 <- lm(cap_salary_ra_age ~ prov_gdr, data = df.match2)
tab_model(fit3)


fit4 <- lm(cap_salary_ra_age_sex ~ prov_gdr, data = df.match2)
tab_model(fit4)


fit5 <- lm(cap_salary_ra_charl ~ prov_gdr, data = df.match2)
tab_model(fit5)


fit6 <- lm(cap_salary_ra_hcc ~ prov_gdr, data = df.match2)
tab_model(fit6)


fit7 <- lm(cap_salary_ra_charl_o ~ prov_gdr, data = df.match2)
tab_model(fit7)


fit8 <- lm(cap_salary_ra_hcc_o ~ prov_gdr, data = df.match2)
tab_model(fit8)


fit9 <- lm(hybrid_charl ~ prov_gdr, data = df.match2)
tab_model(fit9)


fit10 <- lm(hybrid_hcc ~ prov_gdr, data = df.match2)
tab_model(fit10)

