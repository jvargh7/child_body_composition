
source("analysis/pbc01_train and validation split.R")
# source("analysis/pbc02_fitting mixed model.R")

# By Age (precise) -----------
fm_male %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_fm)) +
  geom_smooth(aes(col=iaea_country,group=iaea_country),se=FALSE) +
  geom_smooth(col="black",se=FALSE) +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)")

fm_male %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_fm)) +
  geom_point() +
  geom_path(aes(group = record_id),alpha=0.2) + 
  geom_smooth(col="blue") +
  facet_wrap(~iaea_country) +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)")

fm_male %>% 
  lm(iaea_deut_fm ~ reg_asia + iaea_ia_age,data=.) %>%
  residuals(.) %>% 
  matrix(.,ncol=length(unique(fm_male$iaea_visit)),
         byrow=TRUE) %>% 
  cor(.) %>% 
  corrplot::corrplot(.,method = "number")


# RESIDUAL CORRELATION MATRIX by country adjusting for age ---------

map(unique(fm_male$iaea_country),
    .f = function(c){
      
      country_df <- fm_male %>% 
        dplyr::filter(iaea_country == c);
      print(paste0(c," ",paste0(unique(country_df$iaea_visit),collapse="; ")));
      country_df %>% 
        lm(iaea_deut_fm ~ iaea_ia_age,data=.) %>%
        residuals(.) %>% 
        matrix(.,ncol=length(unique(country_df$iaea_visit)),
               byrow=TRUE) %>% 
        cor(.) %>% 
        corrplot::corrplot(.,method = "number")
      
      
    })


# RESIDUAL CORRELATION MATRIX by country adjusting for Visit -----------
fm_male %>% 
  ggplot(data=.,aes(x=iaea_visit,y=iaea_deut_fm)) +
  geom_point() +
  geom_path(aes(group = record_id),alpha=0.2) + 
  geom_smooth(col="blue") +
  facet_wrap(~iaea_country) +
  theme_bw() +
  xlab("Visit in months") +
  ylab("Fat mass (kg)")

map(unique(fm_male$iaea_country),
    .f = function(c){
      
      country_df <- fm_male %>% 
        dplyr::filter(iaea_country == c);
      print(paste0(c," ",paste0(unique(country_df$iaea_visit),collapse="; ")));
      country_df %>% 
        lm(iaea_deut_fm ~ factor(iaea_visit),data=.) %>%
        residuals(.) %>% 
        matrix(.,ncol=length(unique(country_df$iaea_visit)),
               byrow=TRUE) %>% 
        cor(.) %>% 
        corrplot::corrplot(.,method = "number")
      
      
    })


fm_male %>% 
  mutate(wt_for_ht = (iaea_ia_wgt/(iaea_ia_lgt/100))) %>% 
  dplyr::select(iaea_country,record_id,iaea_ia_age,iaea_visit,wt_for_ht,
                iaea_ia_lgt,iaea_ia_wgt,iaea_ia_ts,iaea_ia_ss,iaea_deut_fm,iaea_deut_ffm) %>% 
  pivot_longer(cols=-one_of("iaea_country","record_id","iaea_visit","iaea_ia_age","iaea_deut_fm","iaea_deut_ffm"),
               names_to="x_var",values_to="x_value") %>% 
  ggplot(data=.,aes(x=x_value,y=iaea_deut_fm)) +
  geom_point() +
  geom_smooth(method="lm")+
  facet_grid(x_var ~ iaea_visit)

