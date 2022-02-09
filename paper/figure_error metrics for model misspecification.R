
source("functions/errors plot.R")

errors_plot(sex = "m",var = "fm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for male fm.png"),width = 10,height = 6,units="in")

errors_plot(sex = "m",var = "ffm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for male ffm.png"),width = 10,height = 6,units="in")


errors_plot(sex = "f",var = "fm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for female fm.png"),width = 10,height = 6,units="in")


errors_plot(sex = "f",var = "ffm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for female ffm.png"),width = 10,height = 6,units="in")


# Sensitivity: LOG -----------

errors_plot(sex = "m",var = "fm",suffix="_log") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for male fm_log.png"),
         width = 10,height = 6,units="in")

errors_plot(sex = "m",var = "ffm",suffix="_log") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for male ffm_log.png"),
         width = 10,height = 6,units="in")


errors_plot(sex = "f",var = "fm",suffix="_log") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for female fm_log.png"),
         width = 10,height = 6,units="in")


errors_plot(sex = "f",var = "ffm",suffix="_log") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for female ffm_log.png"),
         width = 10,height = 6,units="in")


# Combined LOG and Identity ------

errors_plot_combined(sex = "m",var = "fm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for male fm_combined.png"),width = 10,height = 6,units="in")

errors_plot_combined(sex = "m",var = "ffm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for male ffm_combined.png"),width = 10,height = 6,units="in")


errors_plot_combined(sex = "f",var = "fm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for female fm_combined.png"),width = 10,height = 6,units="in")


errors_plot_combined(sex = "f",var = "ffm") %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/error metrics for female ffm_combined.png"),width = 10,height = 6,units="in")
