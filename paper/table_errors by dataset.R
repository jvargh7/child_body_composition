map2_dfr(.x= c("m","m","f","f"),
         .y=c("fm","ffm","fm","ffm"),
         .f=function(sex,var){
           readRDS(paste0(path_pbc_folder,"/working/",sex,"2_",var,".RDS"))$summary %>% 
             dplyr::filter(group %in% c("Overall")) %>% 
             dplyr::select(dataset,nobs,outside,rmse,mae,rmspe,mape,r) %>% 
             mutate(sex = sex,
                    var = var) %>% 
             mutate_at(vars(rmse,mae,r),function(x) round(x,2)) %>% 
             mutate_at(vars(rmspe,mape),function(x) round(x,1))
           
           
         }) %>% 
  write_csv(.,paste0(path_pbc_folder,"/working/table_errors by dataset.csv"))
