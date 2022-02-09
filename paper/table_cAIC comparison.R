
require(cAIC4)
map2_dfr(.x= c("m","m","f","f"),
        .y=c("fm","ffm","fm","ffm"),
        .f=function(sex,var){
          model1 <- cAIC(readRDS(paste0(path_pbc_folder,"/working/",sex,"1_",var,".RDS"))$model);
          model2 <- cAIC(readRDS(paste0(path_pbc_folder,"/working/",sex,"2_",var,".RDS"))$model);
          model3 <- cAIC(readRDS(paste0(path_pbc_folder,"/working/",sex,"3_",var,".RDS"))$model);
          model4 <- cAIC(readRDS(paste0(path_pbc_folder,"/working/",sex,"4_",var,".RDS"))$model);
          
          
          
          
          data.frame(model = paste0(var," - ",sex),
                     caic2 = model2$caic,
                     caic1 = model1$caic,
                     caic3 = model3$caic,
                     caic4 = model4$caic,
                     df2 = model2$df,
                     df1 = model1$df,
                     df3 = model3$df,
                     df4 = model4$df
                     
                     ) %>% 
            return(.)
          
          
        }) %>% 
  write_csv(.,paste0(path_pbc_folder,"/working/table_conditional aic comparison.csv"))
