rm(list=ls())
#setwd("C:\\rotation\\steve\\hgf_monkey\\analysis\\data")
setwd("C:\\rotation\\steve\\data\\behavior\\cleaned\\all_stable")

source("C:/Pandemic_2020/revisions/code/theme_publication.R")

library(data.table)
library(dplyr)
library(rstatix)

# read multiple csv files
temp = list.files(pattern = "*.csv")
monkeys_dat = lapply(temp, read.csv)

# store monkey ids
# monkey_id <- c("4elg_pre","4gor_pre","4pre_pre","4sho_pre",
#                "control_elgpre","control_gorpre","control_Hugpre","control_Jampre","control_Jarpre",
#                "control_Jippre","control_karpre","control_Kenpre","control_kerpre","control_norpre",
#                "control_parpre","control_perpre","control_pigpre","control_poopre","control_prepre",
#                "control_regpre","control_rhopre","control_robpre","control_Shipre","control_shopre",
#                "lofc_Jampos","lofc_Jippos","lofc_Shipos",
#                "md_kerpos","md_perpos","md_regpos",
#                "mofc_Hugpos","mofc_Jarpos","mofc_Kenpos","mofc_Kurpos",
#                "ofc_fedpre","ofc_henpre","ofc_nadpre","ofc_slipre",
#                "vlpfc_elgpos","vlpfc_gorpos","vlpfc_prepos","vlpfc_shopos")

monkey_id <- c("control_elg","control_gor","control_Hug","control_Jam","control_Jar",
               "control_Jip","control_kar","control_Ken","control_ker","control_nor",
               "control_par","control_per","control_pig","control_poo","control_pre",
               "control_reg","control_rho","control_rob","control_Shi","control_sho",
               "lofc_Jam","lofc_Jip","lofc_Shi",
               "md_ker","md_per","md_reg",
               "mofc_Hug","mofc_Jar","mofc_Ken","mofc_Kur",
               "ofc_fed","ofc_hen","ofc_nad","ofc_sli",
               "vlpfc_elg","vlpfc_gor","vlpfc_pre","vlpfc_sho")

# monkey_id <- c("4elg_pre","4gor_pre","4pre_pre","4sho_pre",
#                "elgpos","elgpre","fedpre","gorpos",
#                "gorpre","henpre","karpre","kerpos","nadpre",
#                "parpre","perpos","pigpre","poopre","prepos",
#                "prepre","regpos","shopos","shopre","slipre")

# store lesion groups
lesion_group <- c("control","control","control","control","control",
                  "control","control","control","control","control",
                  "control","control","control","control","control",
                  "control","control","control","control","control",
                  "lofc","lofc","lofc",
                  "md","md","md",
                  "mofc","mofc","mofc","mofc",
                  "ofc","ofc","ofc","ofc",
                  "vlpfc","vlpfc","vlpfc","vlpfc")

# lesion_group <- c("vlpfc_pre","vlpfc_pre","vlpfc_pre","vlpfc_pre",
#                   "vlpfc_post","c","ofc","vlpfc_post",
#                   "c","ofc","c","md_post","ofc",
#                   "c","md_post","c","c","vlpfc_post",
#                   "c","md_post","vlpfc_post","c","ofc")


monkeys_stable_dat <- list()
#monkeys_variable_dat <- list()

monkeys_stable_behavior_session <- list()
#monkeys_variable_behavior_session <- list()

monkeys_stable_behavior_all_sessions <- list()
#monkeys_variable_behavior_all_sessions <- list()

monkeys_stable_behavior_avg_df <- list()
#monkeys_variable_behavior_avg_df <- list()

monkeys_stable_sessions <- list()
#monkeys_variable_sessions <- list()

monkeys_stable_ma_sessions <- list()
#monkeys_variable_ma_sessions <- list()

wsr_stable_binned <- c()
lsr_stable_binned <- c()
#wsr_variable_binned <- c()

monkeys_stable_lsr_wsr_timecourse <- list()
#monkeys_variable_wsr_timecourse <- list()

avg_across_stable_session <- list()
#avg_across_variable_session <- list()

monkeys_stable_behavior_rate_sessions <- list()

monkeys_stable_behavior_rates <- list()

for (m in 1:length(monkey_id)){
  
  #for (m in 1:38){
  
  #m = 6
  
  # remove "rt" column header
  if("rt" %in% colnames(monkeys_dat[[m]])){
    
    monkeys_dat[[m]] <- monkeys_dat[[m]][,-c(5)]
    
  }
  
  # split monkey data by reward schedule
  monkeys_stable_dat[[m]] <- monkeys_dat[[m]][which(monkeys_dat[[m]]$schedule == 3),]
  #monkeys_variable_dat[[m]] <- monkeys_dat[[m]][which(monkeys_dat[[m]]$schedule == 4),]
  
  
  
  
  
  monkeys_stable_behavior_session[[m]] <- list()
  #monkeys_variable_behavior_session[[m]] <- list()
  
  monkeys_stable_behavior_rate_sessions[[m]] <- list()
  
  monkeys_stable_ma_sessions[[m]] <- list()
  #monkeys_variable_ma_sessions[[m]] <- list()
  
  
  for (i in 1:(nrow(monkeys_stable_dat[[m]]))){
    
    # stable reward schedule
    if (monkeys_stable_dat[[m]]$choices[i] == 1 | monkeys_stable_dat[[m]]$choices[i] == 2 | monkeys_stable_dat[[m]]$choices[i] == 3){
      
      
      monkeys_stable_dat[[m]]$win_switch[i] <- ifelse(monkeys_stable_dat[[m]]$choices[i-1] == -2,NA,
                                                      ifelse(monkeys_stable_dat[[m]]$outcomes[i-1] == 1 && (monkeys_stable_dat[[m]]$choices[i] != monkeys_stable_dat[[m]]$choices[i-1]),1,0))
      monkeys_stable_dat[[m]]$win_stay[i] <- ifelse(monkeys_stable_dat[[m]]$choices[i-1] == -2,NA,
                                                    ifelse(monkeys_stable_dat[[m]]$outcomes[i-1] == 1 && (monkeys_stable_dat[[m]]$choices[i] == monkeys_stable_dat[[m]]$choices[i-1]),1,0))
      monkeys_stable_dat[[m]]$lose_switch[i] <- ifelse(monkeys_stable_dat[[m]]$choices[i-1] == -2,NA,
                                                       ifelse(monkeys_stable_dat[[m]]$outcomes[i-1] == 0 && (monkeys_stable_dat[[m]]$choices[i] != monkeys_stable_dat[[m]]$choices[i-1]),1,0))
      monkeys_stable_dat[[m]]$lose_stay[i] <- ifelse(monkeys_stable_dat[[m]]$choices[i-1] == -2,NA,
                                                     ifelse(monkeys_stable_dat[[m]]$outcomes[i-1] == 0 && (monkeys_stable_dat[[m]]$choices[i] == monkeys_stable_dat[[m]]$choices[i-1]),1,0))
      
      # monkey_elg[[m]]$win_switch[i] <- ifelse(monkey_elg[[m]]$outcomes[i-1] == 1 && (monkey_elg[[m]]$choices[i] != monkey_elg[[m]]$choices[i-1]),1,0)
      # monkey_elg[[m]]$win_stay[i] <- ifelse(monkey_elg[[m]]$outcomes[i-1] == 1 && (monkey_elg[[m]]$choices[i] == monkey_elg[[m]]$choices[i-1]),1,0)
      # monkey_elg[[m]]$lose_switch[i] <- ifelse(monkey_elg[[m]]$outcomes[i-1] == 0 && (monkey_elg[[m]]$choices[i] != monkey_elg[[m]]$choices[i-1]),1,0)
      # monkey_elg[[m]]$lose_stay[i] <- ifelse(monkey_elg[[m]]$outcomes[i-1] == 0 && (monkey_elg[[m]]$choices[i] == monkey_elg[[m]]$choices[i-1]),1,0)
      
    } else {
      
      monkeys_stable_dat[[m]]$win_switch[i] <- NA
      monkeys_stable_dat[[m]]$win_stay[i] <- NA
      monkeys_stable_dat[[m]]$lose_switch[i] <- NA
      monkeys_stable_dat[[m]]$lose_stay[i] <- NA
      
    }
    
    # variable reward schedule
    # if (monkeys_variable_dat[[m]]$choices[i] == 1 | monkeys_variable_dat[[m]]$choices[i] == 2 | monkeys_variable_dat[[m]]$choices[i] == 3){
    #   
    #   
    #   monkeys_variable_dat[[m]]$win_switch[i] <- ifelse(monkeys_variable_dat[[m]]$choices[i-1] == -2,NA,
    #                                                     ifelse(monkeys_variable_dat[[m]]$outcomes[i-1] == 1 && (monkeys_variable_dat[[m]]$choices[i] != monkeys_variable_dat[[m]]$choices[i-1]),1,0))
    #   monkeys_variable_dat[[m]]$win_stay[i] <- ifelse(monkeys_variable_dat[[m]]$choices[i-1] == -2,NA,
    #                                                   ifelse(monkeys_variable_dat[[m]]$outcomes[i-1] == 1 && (monkeys_variable_dat[[m]]$choices[i] == monkeys_variable_dat[[m]]$choices[i-1]),1,0))
    #   monkeys_variable_dat[[m]]$lose_switch[i] <- ifelse(monkeys_variable_dat[[m]]$choices[i-1] == -2,NA,
    #                                                      ifelse(monkeys_variable_dat[[m]]$outcomes[i-1] == 0 && (monkeys_variable_dat[[m]]$choices[i] != monkeys_variable_dat[[m]]$choices[i-1]),1,0))
    #   monkeys_variable_dat[[m]]$lose_stay[i] <- ifelse(monkeys_variable_dat[[m]]$choices[i-1] == -2,NA,
    #                                                    ifelse(monkeys_variable_dat[[m]]$outcomes[i-1] == 0 && (monkeys_variable_dat[[m]]$choices[i] == monkeys_variable_dat[[m]]$choices[i-1]),1,0))
    #   
    #   # monkey_elg[[m]]$win_switch[i] <- ifelse(monkey_elg[[m]]$outcomes[i-1] == 1 && (monkey_elg[[m]]$choices[i] != monkey_elg[[m]]$choices[i-1]),1,0)
    #   # monkey_elg[[m]]$win_stay[i] <- ifelse(monkey_elg[[m]]$outcomes[i-1] == 1 && (monkey_elg[[m]]$choices[i] == monkey_elg[[m]]$choices[i-1]),1,0)
    #   # monkey_elg[[m]]$lose_switch[i] <- ifelse(monkey_elg[[m]]$outcomes[i-1] == 0 && (monkey_elg[[m]]$choices[i] != monkey_elg[[m]]$choices[i-1]),1,0)
    #   # monkey_elg[[m]]$lose_stay[i] <- ifelse(monkey_elg[[m]]$outcomes[i-1] == 0 && (monkey_elg[[m]]$choices[i] == monkey_elg[[m]]$choices[i-1]),1,0)
    #   
    # } else {
    #   
    #   monkeys_variable_dat[[m]]$win_switch[i] <- NA
    #   monkeys_variable_dat[[m]]$win_stay[i] <- NA
    #   monkeys_variable_dat[[m]]$lose_switch[i] <- NA
    #   monkeys_variable_dat[[m]]$lose_stay[i] <- NA
    #   
    # }
    
  }
  
  
  # binned WSR (bin_size = 10)
  
  bin_size = 20
  
  if (nrow(monkeys_stable_dat[[m]] == 3000)){
    
    monkeys_stable_dat[[m]] <- monkeys_stable_dat[[m]][1:1500,]
  }
  # if (nrow(monkeys_variable_dat[[m]] == 3000)){
  #   
  #   monkeys_variable_dat[[m]] <- monkeys_variable_dat[[m]][1:1500,]
  # }
  
  monkeys_stable_sessions[[m]] <- unique(monkeys_stable_dat[[m]]$session)
  #monkeys_variable_sessions[[m]] <- unique(monkeys_variable_dat[[m]]$session)
  
  # stable schedule
  for (s in 1:length(monkeys_stable_sessions[[m]])){
    
    for (b in 1:(nrow(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]) - (bin_size-1))){
      
      wsr_stable_binned[b] <- sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_switch[b:(b+(bin_size-1))], na.rm = TRUE)/(sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_switch[b:(b+(bin_size-1))], na.rm = TRUE) + sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_stay[b:(b+(bin_size-1))], na.rm = TRUE))
      lsr_stable_binned[b] <- sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_stay[b:(b+(bin_size-1))], na.rm = TRUE)/(sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_stay[b:(b+(bin_size-1))], na.rm = TRUE) + sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_switch[b:(b+(bin_size-1))], na.rm = TRUE))
      
      
    }
    
    win_switch_rate_stable <- sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_switch, na.rm = TRUE)/(sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_switch, na.rm = TRUE) + sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_stay, na.rm = TRUE))
    win_switch_rate_stable_pre = sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_switch[1:150], na.rm = TRUE)/(sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_switch[1:150], na.rm = TRUE) + sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_stay[1:150], na.rm = TRUE))
    win_switch_rate_stable_post = sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_switch[151:300], na.rm = TRUE)/(sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_switch[151:300], na.rm = TRUE) + sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_stay[151:300], na.rm = TRUE))
    
    lose_stay_rate_stable <- sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_stay, na.rm = TRUE)/(sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_stay, na.rm = TRUE) + sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_switch, na.rm = TRUE))
    lose_stay_rate_stable_pre <- sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_stay[1:150], na.rm = TRUE)/(sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_stay[1:150], na.rm = TRUE) + sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_switch[1:150], na.rm = TRUE))
    lose_stay_rate_stable_post <- sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_stay[151:300], na.rm = TRUE)/(sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_stay[151:300], na.rm = TRUE) + sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_switch[151:300], na.rm = TRUE))
    
    # win_switch_rate_stable <- sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_switch, na.rm = TRUE)/(sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_switch, na.rm = TRUE) + sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$win_stay, na.rm = TRUE))
    # lose_stay_rate_stable <- sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_stay, na.rm = TRUE)/(sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_stay, na.rm = TRUE) + sum(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]$lose_switch, na.rm = TRUE))
    # 
    # 
    # monkeys_stable_behavior_rate_sessions[[m]][[s]] <- data.frame(id = monkey_id[m],
    #                                                               group = lesion_group[m],
    #                                                               wsr_stable = win_switch_rate_stable,
    #                                                               lsr_stable = lose_stay_rate_stable)
    
    monkeys_stable_behavior_rate_sessions[[m]][[s]] <- data.frame(id = monkey_id[m],
                                                                  group = lesion_group[m],
                                                                  wsr_stable = win_switch_rate_stable,
                                                                  wsr_stable_pre = win_switch_rate_stable_pre,
                                                                  wsr_stable_post = win_switch_rate_stable_post,
                                                                  lsr_stable = lose_stay_rate_stable,
                                                                  lsr_stable_pre = lose_stay_rate_stable_pre,
                                                                  lsr_stable_post = lose_stay_rate_stable_post)
    
    
    monkeys_stable_ma_sessions[[m]][[s]] = data.frame(#bins = 1:nrow(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]) - (bin_size-1),
      #wsr_stable = win_switch_rate_stable,
      #lsr_stable = lose_stay_rate_stable,
      wsr_stable_ma = wsr_stable_binned,
      lsr_stable_ma = lsr_stable_binned)
    
  }
  
  # variable schedule
  # for (s in 1:length(monkeys_variable_sessions[[m]])){
  #   
  #   for (b in 1:(nrow(monkeys_variable_dat[[m]][which(monkeys_variable_dat[[m]]$session == monkeys_variable_sessions[[m]][s]),]) - (bin_size-1))){
  #     
  #     wsr_variable_binned[b] <- sum(monkeys_variable_dat[[m]][which(monkeys_variable_dat[[m]]$session == monkeys_variable_sessions[[m]][s]),]$win_switch[b:(b+9)], na.rm = TRUE)/(sum(monkeys_variable_dat[[m]][which(monkeys_variable_dat[[m]]$session == monkeys_variable_sessions[[m]][s]),]$win_switch[b:(b+9)], na.rm = TRUE) + sum(monkeys_variable_dat[[m]][which(monkeys_variable_dat[[m]]$session == monkeys_variable_sessions[[m]][s]),]$win_stay[b:(b+9)], na.rm = TRUE))
  #     
  #     
  #   }
  #   
  #   
  #   monkeys_variable_ma_sessions[[m]][[s]] = data.frame(#bins = 1:nrow(monkeys_stable_dat[[m]][which(monkeys_stable_dat[[m]]$session == monkeys_stable_sessions[[m]][s]),]) - (bin_size-1),
  #     wsr_variable_ma = wsr_variable_binned)
  #   
  # }
  
  
  sep_list_stable <- unlist(monkeys_stable_ma_sessions[[m]], recursive = FALSE)
  monkeys_stable_lsr_wsr_timecourse[[m]] <- lapply(split(sep_list_stable, names(sep_list_stable)), do.call, what = cbind)
  
  avg_across_stable_session[[m]] <- data.frame(id = monkey_id[m],
                                               bins = 1:(300-(bin_size-1)),
                                               group = lesion_group[m],
                                               lsr_avg = rowMeans(monkeys_stable_lsr_wsr_timecourse[[m]][[1]], na.rm = TRUE),
                                               wsr_avg = rowMeans(monkeys_stable_lsr_wsr_timecourse[[m]][[2]], na.rm = TRUE))
  
  
  # sep_list_variable <- unlist(monkeys_variable_ma_sessions[[m]], recursive = FALSE)
  # monkeys_variable_wsr_timecourse[[m]] <- lapply(split(sep_list_variable, names(sep_list_variable)), do.call, what = cbind)
  # 
  # avg_across_variable_session[[m]] <- data.frame(bins = 1:291,
  #                                                group = lesion_group[m],
  #                                                wsr_avg = rowMeans(monkeys_variable_wsr_timecourse[[m]][[1]]))
  
  monkeys_stable_behavior_rates[[m]] <- rbindlist(monkeys_stable_behavior_rate_sessions[[m]])
  
}


# plot behavior boxplot for OFC and MDmc
monkeys_stable_rates_dat <- rbindlist(monkeys_stable_behavior_rates)

subjects_to_not_use <- c("control_Jam","control_Jip","control_ker",
                         "control_per","control_reg","control_Shi")

monkeys_stable_rates_dat <- monkeys_stable_rates_dat[-which(monkeys_stable_rates_dat$id %in% subjects_to_not_use),]

monkeys_stable_rates_dat1 <- monkeys_stable_rates_dat[-which(monkeys_stable_rates_dat$id == "ofc_sli"),]


# average across sessions - wsr
dat_wsr_sessions_df <- monkeys_stable_rates_dat1 %>%
  dplyr::group_by(id,group) %>%
  dplyr::summarise(avg_wsr_pre = mean(wsr_stable_pre, na.rm = TRUE),
                   #avg_mu03_1 = mean(mu03_1, na.rm = TRUE),
                   avg_wsr_post = mean(wsr_stable_post, na.rm = TRUE)#,
                   #avg_mu03_2 = mean(mu03_2, na.rm = TRUE),
                   )

# average across sessions - lsr
dat_lsr_sessions_df <- monkeys_stable_rates_dat1 %>%
  dplyr::group_by(id,group) %>%
  dplyr::summarise(avg_lsr_pre = mean(lsr_stable_pre, na.rm = TRUE),
                   #avg_mu03_1 = mean(mu03_1, na.rm = TRUE),
                   avg_lsr_post = mean(lsr_stable_post, na.rm = TRUE)#,
                   #avg_mu03_2 = mean(mu03_2, na.rm = TRUE),
  )

dat_wsr_rates_long_df <- dat_wsr_sessions_df %>%
  gather(key = "wsr_time", value = "value", avg_wsr_pre,avg_wsr_post) %>%
  convert_as_factor(id,group)

dat_wsr_rates_long_df$time<- rep(c("pre","post"), each=31)
dat_wsr_rates_long_df$time <- factor(dat_wsr_rates_long_df$time, levels = c("pre","post"))


dat_lsr_rates_long_df <- dat_lsr_sessions_df %>%
  gather(key = "lsr_time", value = "value", avg_lsr_pre,avg_lsr_post) %>%
  convert_as_factor(id,group)

dat_lsr_rates_long_df$time<- rep(c("pre","post"), each=31)
dat_lsr_rates_long_df$time <- factor(dat_lsr_rates_long_df$time, levels = c("pre","post"))

# boxplot
library(ggplot2)
# ggplot(data = subset(dat_rates_long_df, group %in% c("control", "ofc", "md")), 
#        aes(time, value, fill = group)) +
#   geom_boxplot(position = position_dodge2(width = 0.2))+ 
#   
#   # linetype parameter is used to customize the joining line
#   #geom_line(aes(group = paired), linetype=2, size=0.5, position = position_dodge2(0.5))+
#   
#   # geom_point() is used to plot data points on boxplot
#   geom_point(aes(fill=group),size=2,shape=21, position = position_dodge2(width = 0.2)) + 
#   #facet_wrap(~session, ncol = 5) + #, #labeller = labeller(lesion = c("control" = "control",
#   # "lesion." = "LOFC lesion"))) +
#   scale_fill_manual("Group",
#                     labels = c('control' = 'control',
#                                'md' = 'MDmc lesion',
#                                'ofc' = 'OFC lesion'),
#                     values = c('gray','#a74ac3','#F58231') #993232 #a74ac3 #999932 #f79b5a
#   ) + 
#   ylab("win-switch rate") + xlab("reversal") + theme_Publication() +
#   theme(#axis.title.y = element_blank(),
#     #axis.title.x = element_blank(),
#     #axis.text = element_blank(),
#     #axis.line = element_line(colour="black", size = 1.5),
#     axis.ticks.x = element_blank(),
#     #axis.ticks = element_blank(),
#     #legend.text = element_blank(),
#     legend.position = "none"
#   )


p_width <- 0.8

ggplot(data = subset(dat_wsr_rates_long_df, group %in% c("control", "ofc", "md")), 
       aes(x=time, y=value, fill=group)) +
  
  # Explicitly set group aesthetic and width for geom_boxplot
  geom_boxplot(aes(group=interaction(time, group)), width=p_width, 
               position=position_dodge(width=p_width + 0.1), size=1.2, alpha=0.8) +
  
  # Use position_jitterdodge() for geom_point
  geom_point(aes(group=interaction(time, group)), 
             position=position_jitterdodge(jitter.width=0.5, dodge.width=p_width), 
             size=2, shape=21) +
  
  scale_fill_manual("Group",
                    labels = c('control' = 'control',
                               'md' = 'MDmc lesion',
                               'ofc' = 'OFC lesion'),
                    values = c('gray', '#a74ac3', '#F58231')
  ) + ylim(0,1) + #xlim(1,281) +
  #scale_x_continuous(breaks = c(1,70,140,210,280)) +
  #ggtitle("Win-switching in Stable schedule") + 
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              legend.text = element_blank(),
                              legend.position = "none"
  )




p_width <- 0.8  # Decreased width for more separation

ggplot(data = subset(dat_lsr_rates_long_df, group %in% c("control", "ofc", "md")), 
       aes(x=time, y=value, fill=group)) +
  
  # Explicitly set group aesthetic and width for geom_boxplot
  geom_boxplot(aes(group=interaction(time, group)), width=p_width, 
               position=position_dodge(width=p_width + 0.1), size=1.2, alpha=0.8) +  # Increase dodge width slightly more than boxplot width
  
  # Use position_jitterdodge() for geom_point
  geom_point(aes(group=interaction(time, group)), 
             position=position_jitterdodge(jitter.width=0.5, dodge.width=p_width + 0.1), 
             size=2, shape=21) +
  
  scale_fill_manual("Group",
                    labels = c('control' = 'control',
                               'md' = 'MDmc lesion',
                               'ofc' = 'OFC lesion'),
                    values = c('gray', '#a74ac3', '#F58231')) + 
  ylim(0,1) +
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              legend.text = element_blank(),
                              legend.position = "none"
  )















# plot behavior trajectories for OFC and MDmc
monkeys_stable_timecourse_dat <- rbindlist(avg_across_stable_session)

subjects_to_not_use <- c("control_Jam","control_Jip","control_ker",
                         "control_per","control_reg","control_Shi")

monkeys_stable_timecourse_dat <- monkeys_stable_timecourse_dat[-which(monkeys_stable_timecourse_dat$id %in% subjects_to_not_use),]

monkeys_stable_timecourse_dat1 <- monkeys_stable_timecourse_dat[-which(monkeys_stable_timecourse_dat$id == "ofc_sli"),]
monkeys_stable_timecourse_dat1$time <- ifelse(monkeys_stable_timecourse_dat1$bins <= 150, "pre","post")

stats <- monkeys_stable_timecourse_dat1 %>%
  dplyr::group_by(group,bins,time) %>%
  dplyr::mutate(mean_wsr = mean(wsr_avg, na.rm = TRUE),
                   sem_wsr = std.error(wsr_avg, na.rm = TRUE),
                   mean_lsr = mean(lsr_avg, na.rm = TRUE),
                   sem_lsr = plotrix::std.error(lsr_avg, na.rm = TRUE)) %>% ungroup()

# monkeys_stable_timecourse_dat$time <- ifelse(monkeys_stable_timecourse_dat$bins <= 150, "pre","post")
# stats <- monkeys_stable_timecourse_dat %>%
#   dplyr::group_by(group,bins,time) %>%
#   dplyr::mutate(mean_wsr = mean(wsr_avg, na.rm = TRUE),
#                 sem_wsr = std.error(wsr_avg, na.rm = TRUE),
#                 mean_lsr = mean(lsr_avg, na.rm = TRUE),
#                 sem_lsr = std.error(lsr_avg, na.rm = TRUE)) %>% ungroup()


# win-switch behavior between control and ofc
fig_ofc_md_wsr <- ggplot(data = subset(stats, group == "control" | group == "ofc" | group == "md")) + geom_line(aes(x= bins, y = mean_wsr, 
                                                                                                                                            color = group#, 
                                                                                                                                            #linetype = group
                                                                                                                                            #size= 0.1
), size=2) +
  geom_ribbon(aes(x = bins, ymin = (mean_wsr - sem_wsr), ymax = (mean_wsr + sem_wsr), fill =group),alpha = 0.3, show.legend = F) +
  geom_vline(xintercept = 150, linetype = "longdash", size=1) +
  # scale_color_manual(values = c("darkred", "darkgreen", "darkblue","darkorange","black"),
  #                    labels = c('control' = 'control (n=20)',
  #                               'md' = 'MD (n=3)',
  #                               'mofc' = 'MOFC (n=4)',
  #                               'ofc' = 'OFC (n=7; n=3[LOFC] + n=4[OFC])',
  #                               'vlpfc' = 'VLPFC (n=4)')) + 
  scale_color_manual(values = c("black", "#911EB4","#F58231"),
                     labels = c('control' = 'control (n=14)',
                                'ofc' = 'OFC (n=3)',
                                'md' = 'MD (n=3)')) + 
  scale_fill_manual(values = c("black", "#911EB4","#F58231")) + ylim(0,1) + xlim(1,300) +
  scale_x_continuous(breaks = c(1,75,150,225,300)) +
  #ggtitle("Win-switching in Stable schedule") + 
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(
    aspect.ratio = 1, # Ensure a square aspect ratio
    axis.title.y = element_blank(),#element_text(face="bold", size=12),
    axis.title.x = element_blank(),#element_text(face="bold", size=12),
    axis.text.x = element_blank(),#element_text(size=10, color="black"),
    axis.text.y = element_blank(),#element_text(size=10, color="black"),
    axis.line = element_line(colour="black", size = 1.5),
    axis.ticks = element_line(colour="black", size = 1.5),
    #legend.text = element_text(size=12),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill=NA, size=1.5) # Add border around plot
  )



# win-switch behavior between control and ofc
fig_ofc_md_lsr <- ggplot(data = subset(stats, group == "control" | group == "ofc" | group == "md")) + geom_line(aes(x= bins, y = mean_lsr, 
                                                                                                                    color = group#, 
                                                                                                                    #linetype = group
                                                                                                                    #size= 0.1
), size=2) +
  geom_ribbon(aes(x = bins, ymin = (mean_lsr - sem_lsr), ymax = (mean_lsr + sem_lsr), fill =group),alpha = 0.3, show.legend = F) +
  geom_vline(xintercept = 150, linetype = "longdash", size=1) +
  # scale_color_manual(values = c("darkred", "darkgreen", "darkblue","darkorange","black"),
  #                    labels = c('control' = 'control (n=20)',
  #                               'md' = 'MD (n=3)',
  #                               'mofc' = 'MOFC (n=4)',
  #                               'ofc' = 'OFC (n=7; n=3[LOFC] + n=4[OFC])',
  #                               'vlpfc' = 'VLPFC (n=4)')) + 
  scale_color_manual(values = c("black", "#911EB4","#F58231"),
                     labels = c('control' = 'control (n=14)',
                                'ofc' = 'OFC (n=3)',
                                'md' = 'MD (n=3)')) + 
  scale_fill_manual(values = c("black", "#911EB4","#F58231")) + ylim(0,1) + xlim(1,300) +
  scale_x_continuous(breaks = c(1,75,150,225,300)) +
  #ggtitle("Win-switching in Stable schedule") + 
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(
    aspect.ratio = 1, # Ensure a square aspect ratio
    axis.title.y = element_blank(),#element_text(face="bold", size=12),
    axis.title.x = element_blank(),#element_text(face="bold", size=12),
    axis.text.x = element_blank(),#element_text(size=10, color="black"),
    axis.text.y = element_blank(),#element_text(size=10, color="black"),
    axis.line = element_line(colour="black", size = 1.5),
    axis.ticks = element_line(colour="black", size = 1.5),
    #legend.text = element_text(size=12),
    legend.position = "none",
    panel.border = element_rect(colour = "black", fill=NA, size=1.5) # Add border around plot
  )



### PERMUTATION TEST
## Is the jump in wsr in MDmc monkeys significantly higher than the jump in controls? - Yes

# subset relevant groups
control_mdmc_dat <- subset(stats, group == "control" | group == "md")

# Permutation-test for win-switching between control and mdmc
# Set a seed for reproducibility
set.seed(123)

# Create a function to calculate the test statistic (difference in means)
calculate_test_statistic <- function(data) {
  mean_mdmc_post <- data %>% filter(group == "md", time == "post") %>% pull(mean_wsr) %>% mean() 
  mean_mdmc_pre <- data %>% filter(group == "md", time == "pre") %>% pull(mean_wsr) %>% mean() 
  mean_control_post <- data %>% filter(group == "control", time == "post") %>% pull(mean_wsr) %>% mean() 
  mean_control_pre <- data %>% filter(group == "control", time == "pre") %>% pull(mean_wsr) %>% mean() 
  
  diff_in_means <- (mean_mdmc_post - mean_mdmc_pre) - (mean_control_post - mean_control_pre)
  return(diff_in_means)
}


# Observed test statistic
observed_statistic <- calculate_test_statistic(control_mdmc_dat)

# Number of permutations 
num_permutations <- 100

# Initialize an empty vector to store permuted test statistics
permuted_statistics <- numeric(num_permutations)

# Permutation test
for (i in 1:num_permutations) {
  # Shuffle the 'group' column to create a random permutation
  control_mdmc_dat$group <- sample(control_mdmc_dat$group)
  
  # Calculate the test statistic for the permuted data
  permuted_statistics[i] <- calculate_test_statistic(control_mdmc_dat)
}

# Calculate the p-value as the proportion of permuted statistics greater than or equal to the observed statistic
#p_value <- mean(permuted_statistics >= observed_statistic)

#p_value <- mean(observed_statistic < permuted_statistics)
p_value <- sum(observed_statistic < permuted_statistics)/100

# Display the results
cat("Observed test statistic:", observed_statistic, "\n")
cat("p-value:", sprintf("%.100f", p_value), "\n")