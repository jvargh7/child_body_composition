m2_fm <- readRDS(paste0(path_pbc_folder,"/working/m2_fm.RDS"))
f2_fm <- readRDS(paste0(path_pbc_folder,"/working/f2_fm.RDS"))
m2_ffm <- readRDS(paste0(path_pbc_folder,"/working/m2_ffm.RDS"))
f2_ffm <- readRDS(paste0(path_pbc_folder,"/working/f2_ffm.RDS"))


bind_rows(
  broom.mixed::tidy(m2_fm$model) %>% 
    mutate(model = "fm_boys"),
  broom.mixed::tidy(f2_fm$model) %>% 
    mutate(model = "fm_girls"),
  broom.mixed::tidy(m2_ffm$model) %>% 
    mutate(model = "ffm_boys"),
  broom.mixed::tidy(f2_ffm$model) %>% 
    mutate(model = "ffm_girls")
) %>% 
  dplyr::filter(!is.na(std.error)) %>% 
  mutate(coef_ci = paste0(estimate %>% round(.,2)," (",
                          (estimate - 1.96*std.error) %>% round(.,2),", ",
                          (estimate + 1.96*std.error) %>% round(.,2),")")) %>% 
  dplyr::select(term,model,coef_ci) %>% 
  pivot_wider(names_from=model,values_from=coef_ci) %>% 
  write_csv(.,paste0(path_pbc_folder,"/working/coefficients for linear spline.csv"))


# SENSITIVITY - LOG --------------

m2_fm_log <- readRDS(paste0(path_pbc_folder,"/working/sensitivity/m2_fm_log.RDS"))
f2_fm_log <- readRDS(paste0(path_pbc_folder,"/working/sensitivity/f2_fm_log.RDS"))
m2_ffm_log <- readRDS(paste0(path_pbc_folder,"/working/sensitivity/m2_ffm_log.RDS"))
f2_ffm_log <- readRDS(paste0(path_pbc_folder,"/working/sensitivity/f2_ffm_log.RDS"))


bind_rows(
  broom.mixed::tidy(m2_fm_log$model) %>% 
    mutate(model = "fm_boys"),
  broom.mixed::tidy(f2_fm_log$model) %>% 
    mutate(model = "fm_girls"),
  broom.mixed::tidy(m2_ffm_log$model) %>% 
    mutate(model = "ffm_boys"),
  broom.mixed::tidy(f2_ffm_log$model) %>% 
    mutate(model = "ffm_girls")
) %>% 
  dplyr::filter(!is.na(std.error)) %>% 
  mutate(coef_ci = paste0(estimate %>% round(.,2)," (",
                          (estimate - 1.96*std.error) %>% round(.,2),", ",
                          (estimate + 1.96*std.error) %>% round(.,2),")")) %>% 
  dplyr::select(term,model,coef_ci) %>% 
  pivot_wider(names_from=model,values_from=coef_ci) %>% 
  write_csv(.,paste0(path_pbc_folder,"/working/coefficients for linear spline_log.csv"))