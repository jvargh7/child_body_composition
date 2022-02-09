source("analysis/pbc01_train and validation split.R")


library(GGally)
# conflict_prefer("wrap", "GGally")
path_incap_repo = "C:/code/incap"
source(paste0(path_incap_repo,"/2015-18/cor_func for GGAlly.R"))

ggpairs(train_male %>% 
                    dplyr::select(iaea_ia_age,iaea_ia_lgt,
                                  iaea_ia_wgt,
                                  weight_by_height, 
                                  iaea_ia_ts,iaea_ia_ss,
                                  iaea_deut_fm,iaea_deut_ffm),
                  columnLabels = c("Age",
                                   "Length (cm)",
                                   "Weight (kg)",
                                   "Weight for \nHeight (kg/m)",
                                   "Triceps (cm)",
                                   "Sub-scapular (cm)",
                                   "Fat mass (kg)",
                                   "Fat free mass (kg)"),
                  upper = list(continuous = wrap(cor_func,
                                                 method = 'pearson', symbol = expression('\u03C1 ='))),
                  lower = list(continuous = function(data, mapping, ...) {
                    # ggally_smooth(data = data, mapping = mapping) +
                    #   geom_point(size=0.2) +
                    #   theme(panel.background = element_blank())
                    ggplot(data=data,mapping=mapping) +
                      geom_point(color="grey",alpha=0.3,size=0.5) +
                      geom_smooth(color="black",method="loess",size=0.8) +
                      theme(panel.background = element_blank())
                    
                  }),
                  diag = list(continuous = function(data, mapping, ...) {
                    ggally_barDiag(data = data, mapping = mapping) + 
                      theme(panel.background = element_blank())}
                  )) %>% 
  ggsave(.,filename=paste0(path_pbc_folder,"/figures/male joint distribution in training.png"),width=10,height=10)

ggpairs(train_female %>% 
          dplyr::select(iaea_ia_age,iaea_ia_lgt,
                        iaea_ia_wgt,
                        weight_by_height, 
                        iaea_ia_ts,iaea_ia_ss,
                        iaea_deut_fm,iaea_deut_ffm),
        columnLabels = c("Age",
                         "Length (cm)",
                         "Weight (kg)",
                         "Weight for \nHeight (kg/m)",
                         "Triceps (cm)",
                         "Sub-scapular (cm)",
                         "Fat mass (kg)",
                         "Fat free mass (kg)"),
        upper = list(continuous = wrap(cor_func,
                                       method = 'pearson', symbol = expression('\u03C1 ='))),
        lower = list(continuous = function(data, mapping, ...) {
          # ggally_smooth(data = data, mapping = mapping) +
          #   geom_point(size=0.2) +
          #   theme(panel.background = element_blank())
          ggplot(data=data,mapping=mapping) +
            geom_point(color="grey",alpha=0.3,size=0.5) +
            geom_smooth(color="black",method="loess",size=0.8) +
            theme(panel.background = element_blank())
          
        }),
        diag = list(continuous = function(data, mapping, ...) {
          ggally_barDiag(data = data, mapping = mapping) + 
            theme(panel.background = element_blank())}
        )) %>% 
  ggsave(.,filename=paste0(path_pbc_folder,"/figures/female joint distribution in training.png"),width=10,height=10)
