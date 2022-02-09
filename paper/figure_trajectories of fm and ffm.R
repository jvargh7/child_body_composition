source("analysis/pbc01_train and validation split.R")

require(ggpubr)
# Trajectories Pooled ------------

pooled_male = bind_rows(train_male %>% mutate(df = "Training"),
                    validation_male %>% mutate(df = "Validation")) %>% 
  mutate(df = factor(df,levels=c("Training","Validation")))
pooled_female = bind_rows(train_female %>% mutate(df = "Training"),
                    validation_female %>% mutate(df = "Validation")) %>% 
  mutate(df = factor(df,levels=c("Training","Validation")))

fig1A = pooled_male %>% 
  
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_fm)) +
  geom_point(aes(shape = df),col="grey40") +
  geom_path(aes(group = record_id),alpha=0.2,col="grey80") + 
  geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)") +
  scale_shape_discrete(name="") +
  scale_x_continuous(breaks=seq(0,30,by=6))

fig1B = pooled_male %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_ffm)) +
  geom_point(aes(shape = df),col="grey40") +
  geom_path(aes(group = record_id),alpha=0.2,col="grey80") + 
  geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat free mass (kg)")+
  scale_shape_discrete(name="") +
  scale_x_continuous(breaks=seq(0,30,by=6))

fig1C = pooled_female %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_fm)) +
  geom_point(aes(shape = df),col="grey40") +
  geom_path(aes(group = record_id),alpha=0.2,col="grey80") + 
  geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)")+
  scale_shape_discrete(name="") +
  scale_x_continuous(breaks=seq(0,30,by=6))

fig1D = pooled_female %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_ffm)) +
  geom_point(aes(shape = df),col="grey40") +
  geom_path(aes(group = record_id),alpha=0.2,col="grey80") + 
  geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat free mass (kg)")+
  scale_shape_discrete(name="") +
  scale_x_continuous(breaks=seq(0,30,by=6))


ggarrange(fig1A,
          fig1B,
          fig1C,
          fig1D,labels=LETTERS[1:4],nrow=2,ncol=2,
          legend = "bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/individual trajectories of body comp pooled.png"),width = 12,height = 5.5,units="in")




# Trajectories by Country --------------------
# TRAIN --------
figA = fm_male %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_fm)) +
  geom_point() +
  geom_path(aes(group = record_id),alpha=0.2) + 
  geom_smooth(col="blue") +
  facet_wrap(~iaea_country) +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)")

figB = fm_male %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_ffm)) +
  geom_point() +
  geom_path(aes(group = record_id),alpha=0.2) + 
  geom_smooth(col="blue") +
  facet_wrap(~iaea_country) +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat free mass (kg)")

figC = fm_female %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_fm)) +
  geom_point() +
  geom_path(aes(group = record_id),alpha=0.2) + 
  geom_smooth(col="blue") +
  facet_wrap(~iaea_country) +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)")

figD = fm_female %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_ffm)) +
  geom_point() +
  geom_path(aes(group = record_id),alpha=0.2) + 
  geom_smooth(col="blue") +
  facet_wrap(~iaea_country) +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat free mass (kg)")


ggarrange(figA,
          figB,
          figC,
          figD,labels=LETTERS[1:4],nrow=2,ncol=2,
          legend = "bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/individual trajectories of body comp in training.png"),width = 12,height = 5.5,units="in")

# TEST ---------
figE = test_male %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_fm)) +
  geom_point() +
  geom_path(aes(group = record_id),alpha=0.2) + 
  # geom_smooth(col="blue") +
  facet_wrap(~iaea_country) +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)")

figF = test_male %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_ffm)) +
  geom_point() +
  geom_path(aes(group = record_id),alpha=0.2) + 
  # geom_smooth(col="blue") +
  facet_wrap(~iaea_country) +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat free mass (kg)")

figG = test_female %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_fm)) +
  geom_point() +
  geom_path(aes(group = record_id),alpha=0.2) + 
  # geom_smooth(col="blue") +
  facet_wrap(~iaea_country) +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)")

figH = test_female %>% 
  ggplot(data=.,aes(x=iaea_ia_age,y=iaea_deut_ffm)) +
  geom_point() +
  geom_path(aes(group = record_id),alpha=0.2) + 
  # geom_smooth(col="blue") +
  facet_wrap(~iaea_country) +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat free mass (kg)")


ggarrange(figE,
          figF,
          figG,
          figH,labels=LETTERS[1:4],nrow=2,ncol=2,
          legend = "bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/individual trajectories of body comp in test.png"),width = 12,height = 5.5,units="in")



# Boxplots of body composition distribution by visit -----------

figI = fm_male %>% 
  ggplot(data=.,aes(x=factor(iaea_visit),y=iaea_deut_fm)) +
  geom_boxplot(aes(fill=iaea_country)) +
  # geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)") +
  scale_fill_discrete(name = "")

figJ = fm_male %>% 
  ggplot(data=.,aes(x=factor(iaea_visit),y=iaea_deut_ffm)) +
  geom_boxplot(aes(fill=iaea_country)) +
  # geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat free mass (kg)") +
  scale_fill_discrete(name = "")

figK = fm_female %>% 
  ggplot(data=.,aes(x=factor(iaea_visit),y=iaea_deut_fm)) +
  geom_boxplot(aes(fill=iaea_country)) +
  # geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)") +
  scale_fill_discrete(name = "")

figL = fm_female %>% 
  ggplot(data=.,aes(x=factor(iaea_visit),y=iaea_deut_ffm)) +
  geom_boxplot(aes(fill=iaea_country)) +
  # geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat free mass (kg)") +
  scale_fill_discrete(name = "")


ggarrange(figI,
          figJ,
          figK,
          figL,labels=LETTERS[1:4],nrow=2,ncol=2,
          legend = "bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/distribution of body comp in training.png"),width = 12,height = 5.5,units="in")

figM = test_male %>% 
  ggplot(data=.,aes(x=factor(iaea_visit),y=iaea_deut_fm)) +
  geom_boxplot(aes(fill=iaea_country)) +
  # geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)") +
  scale_fill_discrete(name = "")

figN = test_male %>% 
  ggplot(data=.,aes(x=factor(iaea_visit),y=iaea_deut_ffm)) +
  geom_boxplot(aes(fill=iaea_country)) +
  # geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat free mass (kg)") +
  scale_fill_discrete(name = "")

figO = test_female %>% 
  ggplot(data=.,aes(x=factor(iaea_visit),y=iaea_deut_fm)) +
  geom_boxplot(aes(fill=iaea_country)) +
  # geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat mass (kg)") +
  scale_fill_discrete(name = "")

figP = test_female %>% 
  ggplot(data=.,aes(x=factor(iaea_visit),y=iaea_deut_ffm)) +
  geom_boxplot(aes(fill=iaea_country)) +
  # geom_smooth(col="blue") +
  theme_bw() +
  xlab("Age in months") +
  ylab("Fat free mass (kg)") +
  scale_fill_discrete(name = "")


ggarrange(figM,
          figN,
          figO,
          figP,labels=LETTERS[1:4],nrow=2,ncol=2,
          legend = "bottom",
          common.legend = TRUE) %>% 
  ggsave(.,filename = paste0(path_pbc_folder,"/figures/distribution of body comp in test.png"),width = 12,height = 5.5,units="in")
