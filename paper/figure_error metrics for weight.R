
source("functions/errors plot.R")

errors_plot_weight(sex = "m",var = "fm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for male fm_weight exposure.png"),width = 10,height = 6,units="in")

errors_plot_weight(sex = "m",var = "ffm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for male ffm_weight exposure.png"),width = 10,height = 6,units="in")


errors_plot_weight(sex = "f",var = "fm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for female fm_weight exposure.png"),width = 10,height = 6,units="in")


errors_plot_weight(sex = "f",var = "ffm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for female ffm_weight exposure.png"),width = 10,height = 6,units="in")
