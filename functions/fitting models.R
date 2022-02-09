
require(splines)
require(lme4)
require(glmmLasso)
require(merTools)

# train_df <- train_male
# validation_df <- validation_male
# test_df <- test_male
# model = "lmer"
# model = "glmmLasso"
# formula_chr = "iaea_deut_fm ~ reg_asia + iaea_ia_age + iaea_ia_age9 + iaea_ia_age18"

# https://stats.stackexchange.com/questions/147836/prediction-interval-for-lmer-mixed-effects-model-in-r


bootMer_pred <- function(lmer_model,newdata){
  
  predFun <- function(fit) {
    predict(fit,newdata,allow.new.levels=TRUE)
  }
  bb <- bootMer(lmer_model,nsim=250,FUN=predFun, use.u = FALSE)
  predMat <- bb$t
  # quantile(predMat[,1], c(0.50,0.05,0.95))
  apply(predMat,2,function(x) quantile(x,probs=c(0.50,0.05,0.95))) %>% 
    t() %>% 
  return(.)
}

fitting_models <- function(formula_chr, train_df,validation_df,test_df,model = "lmer",
                           outcome_transform = "identity",boot_ci = FALSE){
  
  # random effects is specific to this dataset
  
  outcome_var = str_extract(formula_chr,"^[\\(a-z0-9_\\)]+\\s*~") %>% str_replace_all(.,"[\\s|~]+","")
  print(outcome_var)
  if(outcome_transform == "log"){
    outcome_var = str_replace_all(outcome_var,"(\\(|log|\\))","")
    print(outcome_var)
  }
  # print(outcome)
  train_df$outcome <- train_df %>% dplyr::select(one_of(outcome_var)) %>% pull()
  validation_df$outcome <- validation_df %>% dplyr::select(one_of(outcome_var)) %>% pull()
  test_df$outcome <- test_df %>% dplyr::select(one_of(outcome_var)) %>% pull()
  
  head(train_df$outcome)
  
  if(model == "lmer"){
    model_fit <- lmer(as.formula(paste0(formula_chr,"+ (1|record_id)")),
                      data = train_df)
    # bic_model <- BIC(model_fit)
    
    
    train_df[,c("predicted","upr","lwr")] <- predictInterval(merMod = model_fit, newdata = train_df,
                                                    level = 0.95, n.sims = 1000,
                                                    stat = "median", type="linear.prediction",
                                                    include.resid.var = TRUE)
    validation_df[,c("predicted","upr","lwr")] <- predictInterval(merMod = model_fit, newdata = validation_df,
                                                    level = 0.95, n.sims = 1000,
                                                    stat = "median", type="linear.prediction",
                                                    include.resid.var = TRUE)
    test_df[,c("predicted","upr","lwr")] <- predictInterval(merMod = model_fit, newdata = test_df,
                                                    level = 0.95, n.sims = 1000,
                                                    stat = "median", type="linear.prediction",
                                                    include.resid.var = TRUE)
  }
  
  if(model == "glmmLasso"){
    
    model_fit <- glmmLasso(as.formula(formula_chr),
                      data = train_df,lambda = 10,
                      rnd = list(record_id=~1),
                      control=glmmLassoControl(method="REML"))
    # bic_model <- model_fit$bic
    train_df$predicted <- predict(model_fit) 
    validation_df$predicted <- predict(model_fit,newdata=validation_df,allow.new.levels=TRUE)
    test_df$predicted <- predict(model_fit,newdata=test_df,allow.new.levels=TRUE)
    
    train_df$outside_predint = NA
    validation_df$outside_predint = NA
    test_df$outside_predint = NA
  }
  
  if(outcome_transform == "log"){
    train_df <- train_df %>% 
      mutate_at(vars(one_of(c("predicted","upr","lwr",
                              "predicted_boot","upr_boot","lwr_boot"))), function(x) exp(x))
    validation_df <- validation_df %>% 
      mutate_at(vars(one_of(c("predicted","upr","lwr",
                              "predicted_boot","upr_boot","lwr_boot"))), function(x) exp(x))
    test_df <- test_df %>% 
      mutate_at(vars(one_of(c("predicted","upr","lwr",
                              "predicted_boot","upr_boot","lwr_boot"))), function(x) exp(x))
    
    
  }
  
  if(model != "glmmLasso"){
    train_df <- train_df %>% 
      mutate(outside_predint = case_when(outcome < lwr | outcome > upr ~ 1,
                                         outcome >= lwr & outcome <= upr ~ 0,
                                         TRUE ~ NA_real_))
    validation_df <- validation_df %>% 
      mutate(outside_predint = case_when(outcome < lwr | outcome > upr ~ 1,
                                         outcome >= lwr & outcome <= upr ~ 0,
                                         TRUE ~ 0))
    test_df <- test_df %>% 
      mutate(outside_predint = case_when(outcome < lwr | outcome > upr ~ 1,
                                         outcome >= lwr & outcome <= upr ~ 0,
                                         TRUE ~ 0))
  }
  
  if(boot_ci == FALSE){
    train_df$outside_confint_boot = -1
    validation_df$outside_confint_boot = -1
    test_df$outside_confint_boot = -1
  }
  
  if(boot_ci == TRUE){
    
    train_df[,c("predicted_boot","upr_boot","lwr_boot")] <- bootMer_pred(model_fit,newdata = train_df)
    validation_df[,c("predicted_boot","upr_boot","lwr_boot")] <- bootMer_pred(model_fit,newdata = validation_df)
    test_df[,c("predicted_boot","upr_boot","lwr_boot")] <- bootMer_pred(model_fit,newdata = test_df)
    
    train_df <- train_df %>% 
      mutate(outside_confint_boot = case_when(outcome < lwr_boot | outcome > upr_boot ~ 1,
                                              outcome >= lwr_boot & outcome <= upr_boot ~ 0,
                                              TRUE ~ NA_real_)
             
      )
    validation_df <- validation_df %>% 
      mutate(outside_confint_boot = case_when(outcome < lwr_boot | outcome > upr_boot ~ 1,
                                              outcome >= lwr_boot & outcome <= upr_boot ~ 0,
                                              TRUE ~ NA_real_))
    test_df <- test_df %>% 
      mutate(outside_confint_boot = case_when(outcome < lwr_boot | outcome > upr_boot ~ 1,
                                              outcome >= lwr_boot & outcome <= upr_boot ~ 0,
                                              TRUE ~ NA_real_))
    
  }
  
  
  # group is specific to this dataset
  
  train_summary <- gof_summary(predicted = train_df$predicted,
                               actual = train_df$outcome,
                               age = train_df$iaea_ia_age,
                               visit = train_df$iaea_visit,
                               outside = train_df$outside_predint,
                               boot = train_df$outside_confint_boot,
                               group_var = train_df$iaea_country,
                               id_var = train_df$record_id)
  validation_summary <- gof_summary(predicted = validation_df$predicted,
                               actual = validation_df$outcome,
                               age = validation_df$iaea_ia_age,
                               visit = validation_df$iaea_visit,
                               outside = validation_df$outside_predint,
                               boot = validation_df$outside_confint_boot,
                               group_var = validation_df$iaea_country,
                               id_var = validation_df$record_id)
  test_summary <- gof_summary(predicted = test_df$predicted,
                              actual = test_df$outcome,
                              age = test_df$iaea_ia_age,
                              visit = test_df$iaea_visit,
                              outside = test_df$outside_predint,
                              boot = test_df$outside_confint_boot,
                              group_var = test_df$iaea_country,
                              id_var = test_df$record_id)
  
  model_summary <- bind_rows(
    train_summary %>% mutate(dataset = "train"),
    validation_summary %>% mutate(dataset = "validation"),
    test_summary %>% mutate(dataset = "test"),
    
  )
  
  
  return(
    list(train = train_df,
         validation = validation_df,
         test = test_df,
         model = model_fit,
         summary = model_summary)
  )
  
  
}


gof_summary <- function(predicted,
                        actual,
                        age,
                        visit,
                        outside,
                        boot,
                        group_var = NULL,
                        id_var = NULL){
  
  df = data.frame(predicted = predicted,
                  outcome = actual,
                  age = age,
                  visit = visit,
                  outside = outside,
                  boot = boot,
                  group = group_var,
                  id = id_var) %>% 
    mutate(age_category = cut(age,breaks=c(0,9,18,32),right=TRUE,include.lowest=TRUE))
  
  summary_overall <- df %>% 
    dplyr::summarize(
      nchildren = length(unique(id)),
      n = n(),
      mean_pred = mean(predicted,na.rm=TRUE),
      sd_pred = sd(predicted,na.rm=TRUE),
      mean_outcome = mean(outcome,na.rm=TRUE),
      sd_outcome = sd(outcome,na.rm=TRUE),
      nobs = sum(!is.na(predicted)),
      rmse = sqrt(mean((predicted-outcome)^2,na.rm=TRUE)),
      rmspe = 100*sqrt(mean(((predicted/outcome) - 1)^2,na.rm=TRUE)),
      
      r = cor(predicted,outcome,use = "complete.obs",method="pearson"),
      mae = mean(abs(predicted-outcome),na.rm=TRUE),
      mape = 100*mean(abs((predicted/outcome) -1 ),na.rm=TRUE),
      outside = sum(outside,na.rm=TRUE),
      boot = sum(boot,na.rm=TRUE)
    ) %>% 
    mutate(group = "Overall")
  
  summary_grouped <- df %>% 
    group_by(group) %>% 
    dplyr::summarize(
      nchildren = length(unique(id)),
      n = n(),
      nobs = sum(!is.na(predicted)),
      mean_pred = mean(predicted,na.rm=TRUE),
      sd_pred = sd(predicted,na.rm=TRUE),
      mean_outcome = mean(outcome,na.rm=TRUE),
      sd_outcome = sd(outcome,na.rm=TRUE),
      rmse = sqrt(mean((predicted-outcome)^2,na.rm=TRUE)),
      rmspe = 100*sqrt(mean(((predicted/outcome) - 1)^2,na.rm=TRUE)),
      r = cor(predicted,outcome,use = "complete.obs",method="pearson"),
      mae = mean(abs(predicted-outcome),na.rm=TRUE),
      mape = 100*mean(abs((predicted/outcome) -1 ),na.rm=TRUE),
      outside = sum(outside,na.rm=TRUE),
      boot = sum(boot,na.rm=TRUE)
    )
  
  summary_visit <- df %>% 
    group_by(visit) %>% 
    dplyr::summarize(
      nchildren = length(unique(id)),
      n = n(),
      nobs = sum(!is.na(predicted)),
      mean_pred = mean(predicted,na.rm=TRUE),
      sd_pred = sd(predicted,na.rm=TRUE),
      mean_outcome = mean(outcome,na.rm=TRUE),
      sd_outcome = sd(outcome,na.rm=TRUE),
      rmse = sqrt(mean((predicted-outcome)^2,na.rm=TRUE)),
      rmspe = 100*sqrt(mean(((predicted/outcome) - 1)^2,na.rm=TRUE)),
      r = cor(predicted,outcome,use = "complete.obs",method="pearson"),
      mae = mean(abs(predicted-outcome),na.rm=TRUE),
      mape = 100*mean(abs((predicted/outcome) -1 ),na.rm=TRUE),
      outside = sum(outside,na.rm=TRUE),
      boot = sum(boot,na.rm=TRUE)
    ) %>% 
    mutate(visit = paste0("Overall ",visit,"mo")) %>% 
    rename(group = visit )
  
  summary_agecat <- df %>% 
    group_by(age_category) %>% 
    dplyr::summarize(
      nchildren = length(unique(id)),
      n = n(),
      nobs = sum(!is.na(predicted)),
      mean_pred = mean(predicted,na.rm=TRUE),
      sd_pred = sd(predicted,na.rm=TRUE),
      mean_outcome = mean(outcome,na.rm=TRUE),
      sd_outcome = sd(outcome,na.rm=TRUE),
      rmse = sqrt(mean((predicted-outcome)^2,na.rm=TRUE)),
      rmspe = 100*sqrt(mean(((predicted/outcome) - 1)^2,na.rm=TRUE)),
      r = cor(predicted,outcome,use = "complete.obs",method="pearson"),
      mae = mean(abs(predicted-outcome),na.rm=TRUE),
      mape = 100*mean(abs((predicted/outcome) -1 ),na.rm=TRUE),
      outside = sum(outside,na.rm=TRUE),
      boot = sum(boot,na.rm=TRUE)
    )  %>% 
    rename(group = age_category)
  
  
  return(bind_rows(summary_overall,
                   summary_grouped,
                   summary_visit,
                   summary_agecat))
  
  
  
}