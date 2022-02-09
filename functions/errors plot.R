require(ggpubr)

errors_plot <- function(sex = "m",var = "fm",suffix=""){
  
  sub_folder = ifelse(suffix == "","/working/","/working/sensitivity/")

  
  model1 <- readRDS(paste0(path_pbc_folder,sub_folder,sex,"1_",var,suffix,".RDS"))
  model2 <- readRDS(paste0(path_pbc_folder,sub_folder,sex,"2_",var,suffix,".RDS"))
  model3 <- readRDS(paste0(path_pbc_folder,sub_folder,sex,"3_",var,suffix,".RDS"))
  model4 <- readRDS(paste0(path_pbc_folder,sub_folder,sex,"4_",var,suffix,".RDS"))
  
  
  df = bind_rows(
    model1$summary %>% mutate(model = "Model 1"),
    model2$summary %>% mutate(model = "Model 2"),
    model3$summary %>% mutate(model = "Model 3"),
    model4$summary %>% mutate(model = "Model 4")
  ) %>% 
    mutate(group = factor(group,levels=c("Overall","AUS","BRA","IND","PAK","SAF","SRI")),
           dataset = factor(dataset,levels=c("train","validation","test"),labels=c("Training","Validation","Test")),
           model = factor(model,levels=c("Model 2","Model 1","Model 3","Model 4"),
                          labels=c("Linear spline","Quadratic term for age","Natural spline for age","Natural spline for all predictors"))) %>% 
    dplyr::filter(!str_detect(group,"Overall [0-9]+")) 
  
  figA = df %>% 
    ggplot(data=.,aes(x=rmse,y=group,col=model)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("RMSE (in kg)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,1))
  
  figB = df %>% 
    ggplot(data=.,aes(x=rmspe,y=group,col=model)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("RMSPE (%)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,100))
  
  figC = df %>% 
    ggplot(data=.,aes(x=mae,y=group,col=model)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("MAE (in kg)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,1))
  
  figD = df %>% 
    ggplot(data=.,aes(x=mape,y=group,col=model)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("MAPE (%)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,100))
  
  ggarrange(figA,
            figB,
            figC,
            figD,labels=LETTERS[1:4],nrow=2,ncol=2,
            legend = "bottom",
            common.legend = TRUE)
  
}



errors_plot_combined <- function(sex = "m",var = "fm"){
  
  sub_folder1 = "/working/"
  sub_folder2 = "/working/sensitivity/"
  
  
  model1 <- readRDS(paste0(path_pbc_folder,sub_folder1,sex,"1_",var,".RDS"))
  model2 <- readRDS(paste0(path_pbc_folder,sub_folder1,sex,"2_",var,".RDS"))
  model3 <- readRDS(paste0(path_pbc_folder,sub_folder1,sex,"3_",var,".RDS"))
  model4 <- readRDS(paste0(path_pbc_folder,sub_folder1,sex,"4_",var,".RDS"))
  
  model1L <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"1_",var,"_log.RDS"))
  model2L <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"2_",var,"_log.RDS"))
  model3L <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"3_",var,"_log.RDS"))
  model4L <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"4_",var,"_log.RDS"))
  
  df = bind_rows(
    model1$summary %>% mutate(model = "Model 1",outcome = "Identity"),
    model2$summary %>% mutate(model = "Model 2",outcome = "Identity"),
    model3$summary %>% mutate(model = "Model 3",outcome = "Identity"),
    model4$summary %>% mutate(model = "Model 4",outcome = "Identity"),
    
    model1L$summary %>% mutate(model = "Model 1",outcome = "Log"),
    model2L$summary %>% mutate(model = "Model 2",outcome = "Log"),
    model3L$summary %>% mutate(model = "Model 3",outcome = "Log"),
    model4L$summary %>% mutate(model = "Model 4",outcome = "Log")
    
  ) %>% 
    mutate(group = factor(group,levels=c("Overall","AUS","BRA","IND","PAK","SAF","SRI")),
           dataset = factor(dataset,levels=c("train","validation","test"),labels=c("Training","Validation","Test")),
           model = factor(model,levels=c("Model 2","Model 1","Model 3","Model 4"),
                          labels=c("Linear spline","Quadratic term for age","Natural spline for age","Natural spline for all predictors"))) %>% 
    dplyr::filter(!str_detect(group,"Overall [0-9]+")) 
  
  figA = df %>% 
    ggplot(data=.,aes(x=rmse,y=group,col=model,shape = outcome)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("RMSE (in kg)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,1))
  
  figB = df %>% 
    ggplot(data=.,aes(x=rmspe,y=group,col=model,shape = outcome)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("RMSPE (%)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,100))
  
  figC = df %>% 
    ggplot(data=.,aes(x=mae,y=group,col=model,shape = outcome)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("MAE (in kg)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,1))
  
  figD = df %>% 
    ggplot(data=.,aes(x=mape,y=group,col=model,shape = outcome)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("MAPE (%)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,100))
  
  ggarrange(figA,
            figB,
            figC,
            figD,labels=LETTERS[1:4],nrow=2,ncol=2,
            legend = "bottom",
            common.legend = TRUE)
  
}


errors_plot_weight <- function(sex = "m",var = "fm"){
  
  sub_folder1 = "/working/"
  sub_folder2 = "/working/sensitivity/"
  
  
  # model1 <- readRDS(paste0(path_pbc_folder,sub_folder1,sex,"1_",var,".RDS"))
  model2 <- readRDS(paste0(path_pbc_folder,sub_folder1,sex,"2_",var,".RDS"))
  # model3 <- readRDS(paste0(path_pbc_folder,sub_folder1,sex,"3_",var,".RDS"))
  # model4 <- readRDS(paste0(path_pbc_folder,sub_folder1,sex,"4_",var,".RDS"))
  
  # model1B <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"1_",var,"_bmi.RDS"))
  model2B <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"2_",var,"_bmi.RDS"))
  # model3B <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"3_",var,"_bmi.RDS"))
  # model4B <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"4_",var,"_bmi.RDS"))
  
  # model1P <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"1_",var,"_ponderal.RDS"))
  model2P <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"2_",var,"_ponderal.RDS"))
  # model3P <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"3_",var,"_ponderal.RDS"))
  # model4P <- readRDS(paste0(path_pbc_folder,sub_folder2,sex,"4_",var,"_ponderal.RDS"))
  
  df = bind_rows(
    # model1$summary %>% mutate(model = "Model 1",exposure = "Weight by Height"),
    model2$summary %>% mutate(model = "Model 2",exposure = "Weight by Height"),
    # model3$summary %>% mutate(model = "Model 3",exposure = "Weight by Height"),
    # model4$summary %>% mutate(model = "Model 4",exposure = "Weight by Height"),
    
    # model1B$summary %>% mutate(model = "Model 1",exposure = "Body mass index"),
    model2B$summary %>% mutate(model = "Model 2",exposure = "Body mass index"),
    # model3B$summary %>% mutate(model = "Model 3",exposure = "Body mass index"),
    # model4B$summary %>% mutate(model = "Model 4",exposure = "Body mass index"),
    
    # model1P$summary %>% mutate(model = "Model 1",exposure = "Ponderal index"),
    model2P$summary %>% mutate(model = "Model 2",exposure = "Ponderal index"),
    # model3P$summary %>% mutate(model = "Model 3",exposure = "Ponderal index"),
    # model4P$summary %>% mutate(model = "Model 4",exposure = "Ponderal index")
    
  ) %>% 
    mutate(group = factor(group,levels=c("Overall","AUS","BRA","IND","PAK","SAF","SRI")),
           dataset = factor(dataset,levels=c("train","validation","test"),labels=c("Training","Validation","Test")),
           model = factor(model,levels=c("Model 2","Model 1","Model 3","Model 4"),
                          labels=c("Linear spline","Quadratic term for age","Natural spline for age","Natural spline for all predictors"))) %>% 
    dplyr::filter(!str_detect(group,"Overall [0-9]+")) 
  
  figA = df %>% 
    ggplot(data=.,aes(x=rmse,y=group,col=model,shape = exposure)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("RMSE (in kg)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,1))
  
  figB = df %>% 
    ggplot(data=.,aes(x=rmspe,y=group,col=model,shape = exposure)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("RMSPE (%)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,100))
  
  figC = df %>% 
    ggplot(data=.,aes(x=mae,y=group,col=model,shape = exposure)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("MAE (in kg)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,1))
  
  figD = df %>% 
    ggplot(data=.,aes(x=mape,y=group,col=model,shape = exposure)) +
    geom_point() +
    facet_grid(dataset~.,scales="free_y") +
    theme_bw() +
    xlab("MAPE (%)") +
    ylab("") +
    scale_color_discrete(name="") +
    scale_x_continuous(limits=c(0,100))
  
  ggarrange(figA,
            figB,
            figC,
            figD,labels=LETTERS[1:4],nrow=2,ncol=2,
            legend = "bottom",
            common.legend = TRUE)
  
}
