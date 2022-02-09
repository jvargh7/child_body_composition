source("analysis/pbc01_train and validation split.R")

bind_rows(train_male %>% mutate(dataset = "Training"),
          validation_male %>% mutate(dataset = "Validation")) %>% 
  dplyr::select(iaea_country,record_id,iaea_ia_age,iaea_visit,
                iaea_ia_wgt,iaea_ia_lgt,
                iaea_ia_ts,iaea_ia_ss,dataset) %>% 
  left_join(read_csv(paste0(path_pbc_folder,"/working/Prediction dataset - males.csv")) %>% 
              dplyr::select(iaea_country,record_id,iaea_visit,
                            iaea_ia_hc,iaea_ia_ac),
            by=c("iaea_country","record_id","iaea_visit")) %>% View()

