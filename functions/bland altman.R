ba_plot <- function(actual,predicted,se_bands = FALSE){
  
  df = data.frame(a = actual,
             p = predicted) %>% 
    mutate(x = (a + p)/2,
           y = (a-p)) 

  mean_y = mean(df$y,na.rm=TRUE)
  sd_y = sd(df$y,na.rm=TRUE)
  n_y = length(na.omit(df$y))
  se_mean = sqrt((sd_y^2)/n_y)
  se_limits = sqrt((3*sd_y^2)/n_y)
  cat("Mean: ",mean_y)
  print(n_y)
  print(cor.test(df$y,df$x,use="complete.obs"))
  limits = c(mean_y - 1.96*sd_y,mean_y + 1.96*sd_y)
  
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4470095/
  mean_ci = c(mean_y - 1.96*se_mean,mean_y + 1.96*se_mean)
  lowerlimit_ci = c(mean_y - 1.96*sd_y - 1.96*se_limits, mean_y - 1.96*sd_y + 1.96*se_limits)
  upperlimit_ci = c(mean_y + 1.96*sd_y - 1.96*se_limits, mean_y + 1.96*sd_y + 1.96*se_limits)
  
  
  # print(limits)
  b_a <- df %>% 
    ggplot(data=.,aes(x=x,y=y)) +
    geom_point() +
    geom_smooth(method = "lm",se=FALSE,col="darkblue") + 
    geom_hline(yintercept=limits,linetype = 2,col="black") +
    geom_hline(yintercept=mean_y,linetype = 1,col="black") +
    xlab("Average of Observed and Predicted") +
      scale_x_continuous(limits=c(1,6)) +
      scale_y_continuous(limits=c(-2,2)) +
    ylab("Observed - Predicted") +
    theme_bw() 
  
  if(se_bands){
   b_a <- b_a + 
     geom_ribbon(aes(ymin = mean_ci[1],ymax=mean_ci[2]),fill="grey80",alpha=0.3) +
     geom_ribbon(aes(ymin = lowerlimit_ci[1],ymax=lowerlimit_ci[2]),fill="grey80",alpha=0.3) +
     geom_ribbon(aes(ymin = upperlimit_ci[1],ymax=upperlimit_ci[2]),fill="grey80",alpha=0.3)
  }
  
  b_a %>% 
    return(.)
  
  
}


ba_prop_plot <- function(actual,predicted,se_bands){
  
  df = data.frame(a = actual,
                  p = predicted) %>% 
    mutate(x = (a + p)/2,
           y = ((a-p)*100)/x) 
  
  mean_y = mean(df$y,na.rm=TRUE)
  sd_y = sd(df$y,na.rm=TRUE)
  n_y = length(na.omit(df$y))
  se_mean = sqrt((sd_y^2)/n_y)
  se_limits = sqrt((3*sd_y^2)/n_y)
  
  print(n_y)
  cat("Mean: ",mean_y)
  print(cor.test(df$y,df$x,use="complete.obs"))
  limits = c(mean_y - 1.96*sd_y,mean_y + 1.96*sd_y)
  
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4470095/
  mean_ci = c(mean_y - 1.96*se_mean,mean_y + 1.96*se_mean)
  lowerlimit_ci = c(mean_y - 1.96*sd_y - 1.96*se_limits, mean_y - 1.96*sd_y + 1.96*se_limits)
  upperlimit_ci = c(mean_y + 1.96*sd_y - 1.96*se_limits, mean_y + 1.96*sd_y + 1.96*se_limits)
  
  
  # print(limits)
  b_a <- df %>% 
    ggplot(data=.,aes(x=x,y=y)) +
    geom_point() +
    geom_smooth(method = "lm",se=FALSE,col="darkblue") + 
    geom_hline(yintercept=limits,linetype = 2,col="black") +
    geom_hline(yintercept=mean_y,linetype = 1,col="black") +
    xlab("Average of Observed and Predicted") +
    scale_x_continuous(limits=c(1,6)) +
    scale_y_continuous(limits=c(-100,100)) +
    ylab("Observed - Predicted") +
    theme_bw() 
  
  if(se_bands){
    b_a <- b_a + 
      geom_ribbon(aes(ymin = mean_ci[1],ymax=mean_ci[2]),fill="grey80",alpha=0.3) +
      geom_ribbon(aes(ymin = lowerlimit_ci[1],ymax=lowerlimit_ci[2]),fill="grey80",alpha=0.3) +
      geom_ribbon(aes(ymin = upperlimit_ci[1],ymax=upperlimit_ci[2]),fill="grey80",alpha=0.3)
  }
  
  b_a %>% 
    return(.)
  
  
}
