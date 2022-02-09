source("analysis/pbc01_train and validation split.R")


library(compareGroups)

# BY COUNTRY, SEX ------------

bind_rows(train_ids_male %>% mutate(dataset = paste0("Training -",iaea_country)),
          validation_ids_male %>% mutate(dataset = paste0("Validation -",iaea_country)),
          test_male %>% mutate(dataset =paste0("Test -",iaea_country))) %>% 
  group_by(dataset) %>% 
  tally()

bind_rows(train_male %>% mutate(dataset = paste0("Training -",iaea_country)),
          validation_male %>% mutate(dataset = paste0("Validation -",iaea_country)),
          test_male %>% mutate(dataset =paste0("Test -",iaea_country))) %>% 
  compareGroups(dataset ~ iaea_visit + reg_asia + 
                  # iaea_ia_age9 + iaea_ia_age18 + 
                  iaea_ia_lgt + iaea_ia_wgt +
                  iaea_ia_ts + iaea_ia_ss +
                  iaea_deut_fm + iaea_deut_ffm,
              data=.,
                method = c(3,3,
                           1,1,
                           1,1,
                           1,1),max.ylev = 12) %>% 
  createTable(.,digits=1,type = 1,q.type = c(2,2),sd.type = 2,show.all = TRUE) %>% 
  export2xls(.,paste0(path_pbc_folder,"/working/descriptives for males.xlsx"))


bind_rows(train_ids_female %>% mutate(dataset = paste0("Training -",iaea_country)),
          validation_ids_female %>% mutate(dataset = paste0("Validation -",iaea_country)),
          test_female %>% mutate(dataset =paste0("Test -",iaea_country))) %>% 
  group_by(dataset) %>% 
  tally()
bind_rows(train_female %>% mutate(dataset = paste0("Training -",iaea_country)),
          validation_female %>% mutate(dataset = paste0("Validation -",iaea_country)),
          test_female %>% mutate(dataset =paste0("Test -",iaea_country))) %>% 
  compareGroups(dataset ~ iaea_visit + reg_asia + 
                  # iaea_ia_age9 + iaea_ia_age18 + 
                  iaea_ia_lgt + iaea_ia_wgt +
                  iaea_ia_ts + iaea_ia_ss +
                  iaea_deut_fm + iaea_deut_ffm,
                data=.,
                method = c(3,3,
                           1,1,
                           1,1,
                           1,1),max.ylev = 12) %>% 
  createTable(.,digits=1,type = 1,q.type = c(2,2),sd.type = 2,show.all = TRUE) %>% 
  export2xls(.,paste0(path_pbc_folder,"/working/descriptives for females.xlsx"))
# BY VISIT -----------
fm_male %>% 
  compareGroups(iaea_visit ~ iaea_country + reg_asia + 
                  # iaea_ia_age9 + iaea_ia_age18 + 
                  iaea_ia_lgt + iaea_ia_wgt +
                  iaea_ia_ts + iaea_ia_ss +
                  iaea_deut_fm + iaea_deut_ffm,data=.,
                method = c(3,3,
                           1,1,
                           1,1,
                           1,1),max.ylev = 10) %>% 
  createTable(.,digits=1,type = 1,q.type = c(2,2),sd.type = 2,show.all = TRUE)
