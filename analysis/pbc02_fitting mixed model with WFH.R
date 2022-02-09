
source("analysis/pbc01_train and validation split.R")
source("functions/fitting models.R")
# https://stats.stackexchange.com/questions/147836/prediction-interval-for-lmer-mixed-effects-model-in-r#
source("analysis/pbcaux03_predictor equations with WFH.R")



# Male - FM ------------
m1_fm <- fitting_models(paste0("iaea_deut_fm ~",x_1),
                      train_male,validation_male,test_male,model = "lmer")

m2_fm <- fitting_models(paste0("iaea_deut_fm ~",x_2),
                        train_male,validation_male,test_male,model = "lmer")

m3_fm <- fitting_models(paste0("iaea_deut_fm ~",x_3),
                        train_male,validation_male,test_male,model = "lmer")
m4_fm <- fitting_models(paste0("iaea_deut_fm ~",x_4),
                        train_male,validation_male,test_male,model = "lmer")

saveRDS(m1_fm,paste0(path_pbc_folder,"/working/m1_fm.RDS"))
saveRDS(m2_fm,paste0(path_pbc_folder,"/working/m2_fm.RDS"))
saveRDS(m3_fm,paste0(path_pbc_folder,"/working/m3_fm.RDS"))
saveRDS(m4_fm,paste0(path_pbc_folder,"/working/m4_fm.RDS"))

# Male - FFM ----------------
m1_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_1),
                         train_male,validation_male,test_male,model = "lmer")

m2_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_2),
                         train_male,validation_male,test_male,model = "lmer")

m3_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_3),
                         train_male,validation_male,test_male,model = "lmer")
m4_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_4),
                         train_male,validation_male,test_male,model = "lmer")

saveRDS(m1_ffm,paste0(path_pbc_folder,"/working/m1_ffm.RDS"))
saveRDS(m2_ffm,paste0(path_pbc_folder,"/working/m2_ffm.RDS"))
saveRDS(m3_ffm,paste0(path_pbc_folder,"/working/m3_ffm.RDS"))
saveRDS(m4_ffm,paste0(path_pbc_folder,"/working/m4_ffm.RDS"))


# Female - FM ------------
f1_fm <- fitting_models(paste0("iaea_deut_fm ~",x_1),
                        train_female,validation_female,test_female,model = "lmer")

f2_fm <- fitting_models(paste0("iaea_deut_fm ~",x_2),
                        train_female,validation_female,test_female,model = "lmer")

f3_fm <- fitting_models(paste0("iaea_deut_fm ~",x_3),
                        train_female,validation_female,test_female,model = "lmer")
f4_fm <- fitting_models(paste0("iaea_deut_fm ~",x_4),
                        train_female,validation_female,test_female,model = "lmer")

saveRDS(f1_fm,paste0(path_pbc_folder,"/working/f1_fm.RDS"))
saveRDS(f2_fm,paste0(path_pbc_folder,"/working/f2_fm.RDS"))
saveRDS(f3_fm,paste0(path_pbc_folder,"/working/f3_fm.RDS"))
saveRDS(f4_fm,paste0(path_pbc_folder,"/working/f4_fm.RDS"))

# Female - FFM ----------------
f1_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_1),
                         train_female,validation_female,test_female,model = "lmer")

f2_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_2),
                         train_female,validation_female,test_female,model = "lmer")

f3_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_3),
                         train_female,validation_female,test_female,model = "lmer")
f4_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_4),
                         train_female,validation_female,test_female,model = "lmer")

saveRDS(f1_ffm,paste0(path_pbc_folder,"/working/f1_ffm.RDS"))
saveRDS(f2_ffm,paste0(path_pbc_folder,"/working/f2_ffm.RDS"))
saveRDS(f3_ffm,paste0(path_pbc_folder,"/working/f3_ffm.RDS"))
saveRDS(f4_ffm,paste0(path_pbc_folder,"/working/f4_ffm.RDS"))







          
