
# Male -------------
fm_male <- read_csv(paste0(path_pbc_folder,"/working/Prediction dataset - males.csv")) %>% 
  ungroup() %>% 
  dplyr::filter(iaea_ia_age <= 26) %>% 
  dplyr::select(iaea_country,record_id,iaea_ia_age,iaea_visit,iaea_ia_lgt,iaea_ia_wgt,
                iaea_ia_ts,iaea_ia_ss,reg_asia,iaea_deut_fm,iaea_deut_ffm) %>% 
  dplyr::filter(complete.cases(.)) %>% 
  mutate(iaea_ia_lgt = iaea_ia_lgt/100) %>% 
  mutate(iaea_ia_age9 = case_when(iaea_ia_age >= 9 ~ iaea_ia_age - 9,
                                  TRUE ~ 0),
         iaea_ia_age18 = case_when(iaea_ia_age >= 18 ~ iaea_ia_age - 18,
                                   TRUE ~ 0),
         weight_by_height = iaea_ia_wgt/(iaea_ia_lgt),
         quetelet = iaea_ia_wgt/((iaea_ia_lgt)^2),
         ponderal = iaea_ia_wgt/((iaea_ia_lgt)^3)
         ) 

unique_ids_male <- fm_male %>% 
  distinct(iaea_country,record_id)

set.seed(2021)
train_ids_male <- unique_ids_male %>% 
  group_by(iaea_country) %>% 
  sample_frac(size=2/3) %>% 
  ungroup()

validation_ids_male <- unique_ids_male %>% 
  anti_join(train_ids_male,
            by=c("iaea_country","record_id"))

test_male <- read_csv(paste0(path_pbc_folder,"/working/External validation sample.csv")) %>% 
  dplyr::select(iaea_country,iaea_sex,record_id,iaea_ia_age,iaea_visit,iaea_ia_lgt,iaea_ia_wgt,
                iaea_ia_ts,iaea_ia_ss,iaea_deut_fm,iaea_deut_ffm) %>% 
  dplyr::filter(iaea_sex == "male",iaea_ia_age <= 26) %>% 
  dplyr::filter(complete.cases(.)) %>% 
  mutate(iaea_ia_lgt = iaea_ia_lgt/100) %>% 
  mutate(iaea_ia_age9 = case_when(iaea_ia_age >= 9 ~ iaea_ia_age - 9,
                                  TRUE ~ 0),
         reg_asia = case_when(iaea_country == "AUS" ~ "Other",
                              iaea_country == "IND" ~ "Asia",
                              iaea_country == "SAF" ~ "Other",
                              TRUE ~ NA_character_),
         iaea_ia_age18 = case_when(iaea_ia_age >= 18 ~ iaea_ia_age - 18,
                                   TRUE ~ 0),
         weight_by_height = iaea_ia_wgt/(iaea_ia_lgt),
         quetelet = iaea_ia_wgt/((iaea_ia_lgt)^2),
         ponderal = iaea_ia_wgt/((iaea_ia_lgt)^3)
  )

train_male <- train_ids_male %>% 
  left_join(fm_male,
            by=c("iaea_country","record_id"))

validation_male <- validation_ids_male %>% 
  left_join(fm_male,
            by=c("iaea_country","record_id"))

# Female -------------
fm_female <- read_csv(paste0(path_pbc_folder,"/working/Prediction dataset - females.csv")) %>% 
  ungroup() %>% 
  dplyr::filter(iaea_ia_age <= 26) %>% 
  dplyr::select(iaea_country,record_id,iaea_ia_age,iaea_visit,iaea_ia_lgt,iaea_ia_wgt,
                iaea_ia_ts,iaea_ia_ss,reg_asia,iaea_deut_fm,iaea_deut_ffm) %>% 
  dplyr::filter(complete.cases(.)) %>% 
  mutate(iaea_ia_lgt = iaea_ia_lgt/100) %>% 
  mutate(iaea_ia_age9 = case_when(iaea_ia_age >= 9 ~ iaea_ia_age - 9,
                                  TRUE ~ 0),
         iaea_ia_age18 = case_when(iaea_ia_age >= 18 ~ iaea_ia_age - 18,
                                   TRUE ~ 0),
         weight_by_height = iaea_ia_wgt/(iaea_ia_lgt),
         quetelet = iaea_ia_wgt/((iaea_ia_lgt)^2),
         ponderal = iaea_ia_wgt/((iaea_ia_lgt)^3)
  ) 

unique_ids_female <- fm_female %>% 
  distinct(iaea_country,record_id)

set.seed(2021)
train_ids_female <- unique_ids_female %>% 
  group_by(iaea_country) %>% 
  sample_frac(size=2/3) %>% 
  ungroup()

validation_ids_female <- unique_ids_female %>% 
  anti_join(train_ids_female,
            by=c("iaea_country","record_id"))

test_female <- read_csv(paste0(path_pbc_folder,"/working/External validation sample.csv")) %>% 
  dplyr::select(iaea_country,iaea_sex,record_id,iaea_ia_age,iaea_visit,iaea_ia_lgt,iaea_ia_wgt,
                iaea_ia_ts,iaea_ia_ss,iaea_deut_fm,iaea_deut_ffm) %>% 
  dplyr::filter(iaea_sex == "female",iaea_ia_age <= 26) %>% 
  dplyr::filter(complete.cases(.)) %>% 
  mutate(iaea_ia_lgt = iaea_ia_lgt/100) %>% 
  mutate(iaea_ia_age9 = case_when(iaea_ia_age >= 9 ~ iaea_ia_age - 9,
                                  TRUE ~ 0),
         reg_asia = case_when(iaea_country == "AUS" ~ "Other",
                              iaea_country == "IND" ~ "Asia",
                              iaea_country == "SAF" ~ "Other",
                              TRUE ~ NA_character_),
         iaea_ia_age18 = case_when(iaea_ia_age >= 18 ~ iaea_ia_age - 18,
                                   TRUE ~ 0),
         weight_by_height = iaea_ia_wgt/(iaea_ia_lgt),
         quetelet = iaea_ia_wgt/((iaea_ia_lgt)^2),
         ponderal = iaea_ia_wgt/((iaea_ia_lgt)^3)
  ) 

train_female <- train_ids_female %>% 
  left_join(fm_female,
            by=c("iaea_country","record_id"))

validation_female <- validation_ids_female %>% 
  left_join(fm_female,
            by=c("iaea_country","record_id"))
