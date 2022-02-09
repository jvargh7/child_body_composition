source("functions/bland altman.R")


require(ggpubr)
map2(.x= c("m","m","f","f"),
         .y=c("fm","ffm","fm","ffm"),
         .f=function(sex,var){
           readRDS(paste0(path_pbc_folder,"/working/",sex,"2_",var,".RDS")) %>% 
             {ba_plot(actual = .$test$outcome,predicted=.$test$predicted,se_bands = TRUE)}
           
           
         }) %>% 
  ggarrange(plotlist = .,labels=LETTERS[1:4],nrow=2,ncol=2) %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/bland altman for test.png"),width=9,height=5.24)

map2(.x= c("m","m","f","f"),
     .y=c("fm","ffm","fm","ffm"),
     .f=function(sex,var){
       readRDS(paste0(path_pbc_folder,"/working/",sex,"2_",var,".RDS")) %>% 
         {ba_prop_plot(actual = .$test$outcome,predicted=.$test$predicted,se_bands = TRUE)}
       
       
     }) %>% 
  ggarrange(plotlist = .,labels=LETTERS[1:4],nrow=2,ncol=2) %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/bland altman proportional for test.png"),width=9,height=5.24)

# SENSITIVITY - LOG --------

map2(.x= c("m","m","f","f"),
     .y=c("fm","ffm","fm","ffm"),
     .f=function(sex,var){
       readRDS(paste0(path_pbc_folder,"/working/sensitivity/",sex,"2_",var,"_log",".RDS")) %>% 
         {ba_plot(actual = .$test$outcome,predicted=.$test$predicted)}
       
       
     }) %>% 
  ggarrange(plotlist = .,labels=LETTERS[1:4],nrow=2,ncol=2) %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/bland altman for test_log.png"),width=9,height=5.24)
