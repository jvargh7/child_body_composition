
# Fat mass in boys ------------
(bind_rows(m1_fm$train %>% mutate(data = "Training"),
           m1_fm$validation %>% mutate(data="Validation")) %>% 
  ggplot(data=.,aes(x=iaea_ia_age)) +
  geom_smooth(aes(y=c(m1_fm$train$predicted,
                      m1_fm$validation$predicted),
                  col="Linear spline without covariates"), se=FALSE) +
  geom_smooth(aes(y=c(m2_fm$train$predicted,
                      m2_fm$validation$predicted),
                  col="Linear spline"), se=FALSE) +
  geom_smooth(aes(y=c(m3_fm$train$predicted,
                      m3_fm$validation$predicted),
                  col="Cubic spline"),se=FALSE) +
  geom_smooth(aes(y=c(m4_fm$train$predicted,
                      m4_fm$validation$predicted),
                  col="All cubic spline"),se=FALSE) +
  theme_bw() +
  facet_grid(~data) +
  xlab("Age in months") +
  ylab("Predicted Fat mass (kg)") +
  scale_y_continuous(limits=c(0,8)) +
  scale_color_manual("",values=c("Linear spline without covariates" = "orange",
                                 "Linear spline" = "blue",
                                 "Cubic spline" = "red",
                                 "All cubic spline" = "darkgreen")) +
  theme(legend.position="bottom")) %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/lines of best fit for male FM.png"),width = 2000,height=800,units="px")
