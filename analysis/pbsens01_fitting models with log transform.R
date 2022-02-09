
source("analysis/pbcaux03_predictor equations with WFH.R")
source("analysis/pbc01_train and validation split.R")
source("functions/fitting models.R")
# https://stats.stackexchange.com/questions/147836/prediction-interval-for-lmer-mixed-effects-model-in-r#

# Male - FM ------------
m1_fm <- fitting_models(paste0("log(iaea_deut_fm) ~",x_1),
                        train_male,validation_male,test_male,model = "lmer",outcome_transform = "log")

m2_fm <- fitting_models(paste0("log(iaea_deut_fm) ~",x_2),
                        train_male,validation_male,test_male,model = "lmer",outcome_transform = "log")

m3_fm <- fitting_models(paste0("log(iaea_deut_fm) ~",x_3),
                        train_male,validation_male,test_male,model = "lmer",outcome_transform = "log")
m4_fm <- fitting_models(paste0("log(iaea_deut_fm) ~",x_4),
                        train_male,validation_male,test_male,model = "lmer",outcome_transform = "log")

saveRDS(m1_fm,paste0(path_pbc_folder,"/working/sensitivity/m1_fm_log.RDS"))
saveRDS(m2_fm,paste0(path_pbc_folder,"/working/sensitivity/m2_fm_log.RDS"))
saveRDS(m3_fm,paste0(path_pbc_folder,"/working/sensitivity/m3_fm_log.RDS"))
saveRDS(m4_fm,paste0(path_pbc_folder,"/working/sensitivity/m4_fm_log.RDS"))

# Male - FFM ----------------
m1_ffm <- fitting_models(paste0("log(iaea_deut_ffm) ~",x_1),
                         train_male,validation_male,test_male,model = "lmer",outcome_transform = "log")

m2_ffm <- fitting_models(paste0("log(iaea_deut_ffm) ~",x_2),
                         train_male,validation_male,test_male,model = "lmer",outcome_transform = "log")

m3_ffm <- fitting_models(paste0("log(iaea_deut_ffm) ~",x_3),
                         train_male,validation_male,test_male,model = "lmer",outcome_transform = "log")
m4_ffm <- fitting_models(paste0("log(iaea_deut_ffm) ~",x_4),
                         train_male,validation_male,test_male,model = "lmer",outcome_transform = "log")

saveRDS(m1_ffm,paste0(path_pbc_folder,"/working/sensitivity/m1_ffm_log.RDS"))
saveRDS(m2_ffm,paste0(path_pbc_folder,"/working/sensitivity/m2_ffm_log.RDS"))
saveRDS(m3_ffm,paste0(path_pbc_folder,"/working/sensitivity/m3_ffm_log.RDS"))
saveRDS(m4_ffm,paste0(path_pbc_folder,"/working/sensitivity/m4_ffm_log.RDS"))


# Female - FM ------------
f1_fm <- fitting_models(paste0("log(iaea_deut_fm) ~",x_1),
                        train_female,validation_female,test_female,model = "lmer",outcome_transform = "log")

f2_fm <- fitting_models(paste0("log(iaea_deut_fm) ~",x_2),
                        train_female,validation_female,test_female,model = "lmer",outcome_transform = "log")

f3_fm <- fitting_models(paste0("log(iaea_deut_fm) ~",x_3),
                        train_female,validation_female,test_female,model = "lmer",outcome_transform = "log")
f4_fm <- fitting_models(paste0("log(iaea_deut_fm) ~",x_4),
                        train_female,validation_female,test_female,model = "lmer",outcome_transform = "log")

saveRDS(f1_fm,paste0(path_pbc_folder,"/working/sensitivity/f1_fm_log.RDS"))
saveRDS(f2_fm,paste0(path_pbc_folder,"/working/sensitivity/f2_fm_log.RDS"))
saveRDS(f3_fm,paste0(path_pbc_folder,"/working/sensitivity/f3_fm_log.RDS"))
saveRDS(f4_fm,paste0(path_pbc_folder,"/working/sensitivity/f4_fm_log.RDS"))

# Female - FFM ----------------
f1_ffm <- fitting_models(paste0("log(iaea_deut_ffm) ~",x_1),
                         train_female,validation_female,test_female,model = "lmer",outcome_transform = "log")

f2_ffm <- fitting_models(paste0("log(iaea_deut_ffm) ~",x_2),
                         train_female,validation_female,test_female,model = "lmer",outcome_transform = "log")

f3_ffm <- fitting_models(paste0("log(iaea_deut_ffm) ~",x_3),
                         train_female,validation_female,test_female,model = "lmer",outcome_transform = "log")
f4_ffm <- fitting_models(paste0("log(iaea_deut_ffm) ~",x_4),
                         train_female,validation_female,test_female,model = "lmer",outcome_transform = "log")

saveRDS(f1_ffm,paste0(path_pbc_folder,"/working/sensitivity/f1_ffm_log.RDS"))
saveRDS(f2_ffm,paste0(path_pbc_folder,"/working/sensitivity/f2_ffm_log.RDS"))
saveRDS(f3_ffm,paste0(path_pbc_folder,"/working/sensitivity/f3_ffm_log.RDS"))
saveRDS(f4_ffm,paste0(path_pbc_folder,"/working/sensitivity/f4_ffm_log.RDS"))








