left_join(
  map2_dfr(.x= c("m","m","f","f"),
     .y=c("fm","ffm","fm","ffm"),
     .f=function(sex,var){
       readRDS(paste0(path_pbc_folder,"/working/",sex,"2_",var,".RDS"))$summary %>% 
         dplyr::filter(group %in% c("Overall","[0,9]","(9,18]","(18,32]")) %>% 
         dplyr::select(dataset,group,rmse) %>% 
         mutate(sex = sex,
                var = var,
                rmse = round(rmse,2))
       
       
     }) %>% 
  pivot_wider(names_from=dataset,values_from=rmse),
  map2_dfr(.x= c("m","m","f","f"),
           .y=c("fm","ffm","fm","ffm"),
           .f=function(sex,var){
             readRDS(paste0(path_pbc_folder,"/working/",sex,"2_",var,".RDS"))$summary %>% 
               dplyr::filter(group %in% c("Overall","[0,9]","(9,18]","(18,32]")) %>% 
               dplyr::select(dataset,group,r) %>% 
               mutate(sex = sex,
                      var = var,
                      r = round(r,2))
             
             
           }) %>% 
    pivot_wider(names_from=dataset,values_from=r),
  by = c("sex","var","group")) %>% 
  dplyr::select(group,sex,var,contains("train"),contains("validation"),
                contains("test")) %>% 
  write_csv(.,paste0(path_pbc_folder,"/working/table_rmse by age category.csv"))
