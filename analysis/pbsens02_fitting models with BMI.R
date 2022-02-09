
source("analysis/pbc01_train and validation split.R")
source("functions/fitting models.R")
# https://stats.stackexchange.com/questions/147836/prediction-interval-for-lmer-mixed-effects-model-in-r#
source("analysis/pbcaux04_predictor equations with BMI.R")



# Male - FM ------------
m1_fm <- fitting_models(paste0("iaea_deut_fm ~",x_1),
                        train_male,validation_male,test_male,model = "lmer")

m2_fm <- fitting_models(paste0("iaea_deut_fm ~",x_2),
                        train_male,validation_male,test_male,model = "lmer")

m3_fm <- fitting_models(paste0("iaea_deut_fm ~",x_3),
                        train_male,validation_male,test_male,model = "lmer")
m4_fm <- fitting_models(paste0("iaea_deut_fm ~",x_4),
                        train_male,validation_male,test_male,model = "lmer")

saveRDS(m1_fm,paste0(path_pbc_folder,"/working/sensitivity/m1_fm_bmi.RDS"))
saveRDS(m2_fm,paste0(path_pbc_folder,"/working/sensitivity/m2_fm_bmi.RDS"))
saveRDS(m3_fm,paste0(path_pbc_folder,"/working/sensitivity/m3_fm_bmi.RDS"))
saveRDS(m4_fm,paste0(path_pbc_folder,"/working/sensitivity/m4_fm_bmi.RDS"))

# Male - FFM ----------------
m1_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_1),
                         train_male,validation_male,test_male,model = "lmer")

m2_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_2),
                         train_male,validation_male,test_male,model = "lmer")

m3_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_3),
                         train_male,validation_male,test_male,model = "lmer")
m4_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_4),
                         train_male,validation_male,test_male,model = "lmer")

saveRDS(m1_ffm,paste0(path_pbc_folder,"/working/sensitivity/m1_ffm_bmi.RDS"))
saveRDS(m2_ffm,paste0(path_pbc_folder,"/working/sensitivity/m2_ffm_bmi.RDS"))
saveRDS(m3_ffm,paste0(path_pbc_folder,"/working/sensitivity/m3_ffm_bmi.RDS"))
saveRDS(m4_ffm,paste0(path_pbc_folder,"/working/sensitivity/m4_ffm_bmi.RDS"))


# Female - FM ------------
f1_fm <- fitting_models(paste0("iaea_deut_fm ~",x_1),
                        train_female,validation_female,test_female,model = "lmer")

f2_fm <- fitting_models(paste0("iaea_deut_fm ~",x_2),
                        train_female,validation_female,test_female,model = "lmer")

f3_fm <- fitting_models(paste0("iaea_deut_fm ~",x_3),
                        train_female,validation_female,test_female,model = "lmer")
f4_fm <- fitting_models(paste0("iaea_deut_fm ~",x_4),
                        train_female,validation_female,test_female,model = "lmer")

saveRDS(f1_fm,paste0(path_pbc_folder,"/working/sensitivity/f1_fm_bmi.RDS"))
saveRDS(f2_fm,paste0(path_pbc_folder,"/working/sensitivity/f2_fm_bmi.RDS"))
saveRDS(f3_fm,paste0(path_pbc_folder,"/working/sensitivity/f3_fm_bmi.RDS"))
saveRDS(f4_fm,paste0(path_pbc_folder,"/working/sensitivity/f4_fm_bmi.RDS"))

# Female - FFM ----------------
f1_ffm <- fitting_models(paste0("iaea_deut_fm ~",x_1),
                         train_female,validation_female,test_female,model = "lmer")

f2_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_2),
                         train_female,validation_female,test_female,model = "lmer")

f3_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_3),
                         train_female,validation_female,test_female,model = "lmer")
f4_ffm <- fitting_models(paste0("iaea_deut_ffm ~",x_4),
                         train_female,validation_female,test_female,model = "lmer")

saveRDS(f1_ffm,paste0(path_pbc_folder,"/working/sensitivity/f1_ffm_bmi.RDS"))
saveRDS(f2_ffm,paste0(path_pbc_folder,"/working/sensitivity/f2_ffm_bmi.RDS"))
saveRDS(f3_ffm,paste0(path_pbc_folder,"/working/sensitivity/f3_ffm_bmi.RDS"))
saveRDS(f4_ffm,paste0(path_pbc_folder,"/working/sensitivity/f4_ffm_bmi.RDS"))








