source("analysis/pbc01_train and validation split.R")


m1_fm <- readRDS(paste0(path_pbc_folder,"/working/m1_fm.RDS"))
m2_fm <- readRDS(paste0(path_pbc_folder,"/working/m2_fm.RDS"))
m3_fm <- readRDS(paste0(path_pbc_folder,"/working/m3_fm.RDS"))
m4_fm <- readRDS(paste0(path_pbc_folder,"/working/m4_fm.RDS"))

# Table 1 Summary of models -----------

bind_cols(
  m2_fm$summary %>% 
    dplyr::select(dataset,group,nobs,rmse,mae,mape,outside) %>% 
    rename(m2_rmse = rmse,
           m2_mae = mae,
           m2_mape = mape,
           m2_outside = outside),
  m3_fm$summary %>% 
    dplyr::select(rmse,mae,mape,outside) %>%
    rename(m3_rmse = rmse,
           m3_mae = mae,
           m3_mape = mape,
           m3_outside = outside)
  )

# Fig 2 Examples of Fitted and Actual Trajectories ----------

set.seed(400)
bind_rows(
  bind_cols(train,
            PI_m2_train %>% rename(linear_fit = fit,
                                   linear_lwr = lwr,
                                   linear_upr = upr),
            PI_m3_train %>% rename(cubic_fit = fit,
                                   cubic_lwr = lwr,
                                   cubic_upr = upr)) %>% 
    mutate(dataset = "Training"),
  
  bind_cols(validation,
            PI_m2_validation %>% rename(linear_fit = fit,
                                   linear_lwr = lwr,
                                   linear_upr = upr),
            PI_m3_validation %>% rename(cubic_fit = fit,
                                   cubic_lwr = lwr,
                                   cubic_upr = upr)) %>% 
    mutate(dataset = "Validation")
  
) %>% 

  right_join(
    fm_male %>% 
      dplyr::distinct(record_id,iaea_country) %>% 
      group_by(iaea_country) %>% 
      sample_n(1),
    by = c("record_id","iaea_country")
  )  %>% 
  ungroup() %>% 
  arrange(record_id,iaea_visit) %>% 
  dplyr::select(record_id,iaea_country,iaea_ia_age,
                dataset,
                iaea_deut_fm,linear_fit,cubic_fit) %>% 
  ggplot(data=.,aes(x=iaea_ia_age,group=record_id)) +
  geom_point(aes(y = iaea_deut_fm,shape = dataset)) +
  geom_path(aes(y=iaea_deut_fm,col="Actual")) +
  geom_path(aes(y=linear_fit,col="Linear spline")) +
  geom_path(aes(y=cubic_fit,col="Cubic spline")) +
  facet_wrap(~iaea_country) + 
  # scale_linetype_manual(name ="Country") +
  scale_color_manual(name = "",values=c("Actual"="black",
                                 "Linear spline" = "red",
                                 "Cubic spline" = "blue")) + 
  scale_shape_discrete(name = "Data Split") +
  theme_bw() + 
  xlab("Visit in months") +
  ylab("Fat mass (kg)") +
  scale_y_continuous(limits=c(0,5)) +
  theme(legend.position = "bottom")



# Supplementary Fig 1 Residual plot ----------

bind_rows(
  bind_cols(validation, PI_m2_validation) %>% mutate(dataset="Validation"),
  bind_cols(train,PI_m2_train) %>% mutate(dataset="Training")
  # bind_cols(test,PI_m2_test) %>% mutate(dataset="Testing"),
  ) %>% 
  mutate(residual = iaea_deut_fm - fit) %>% 
  ggplot(data=.,aes(x=iaea_visit,y=residual)) +
  geom_point() +
  # geom_path(aes(group = record_id),alpha=0.2) + 
  geom_smooth(col="blue") +
  facet_grid(dataset~iaea_country) +
  theme_bw() +
  xlab("Visit in months") +
  ylab("Fat mass (kg)")

