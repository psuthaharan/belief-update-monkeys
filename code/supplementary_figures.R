# belief volatility trajectory
rm(list=ls())
# read file
traj_dat <- read.csv("C:\\rotation\\steve\\data\\belief\\hgfToolBox\\output\\traj3_mdmc_sessions_dat.csv")


traj_ofc_dat <- read.csv("C:\\rotation\\steve\\data\\belief\\hgfToolBox\\output\\traj3_ofc_sessions_dat.csv")


# mdmc subjects
mdmc_subjects <- c("kerpos","perpos","regpos")
ofc_subjects <- c("nad","fed","hen","sli")


traj_dat_grouped <- traj_dat
traj_dat_grouped$group <- ifelse(traj_dat_grouped$id %in% mdmc_subjects,"mdmc","control")



traj_ofc_dat$group <- ifelse(traj_ofc_dat$id %in% ofc_subjects,"ofc","control")


traj_bind <- rbind(traj_dat_grouped,traj_ofc_dat)

traj_bind <- traj_bind[-which(traj_bind$id == "sli"),]



library(dplyr)
library(zoo)
# Add a row number for each subject
traj_subset_trials_grouped <- traj_bind %>%
  group_by(id,session,group) %>%
  mutate(row = row_number())


# normalization
traj_subset_trials_grouped_normalized <- traj_subset_trials_grouped %>%
  group_by(id,session,group) %>%
  mutate(normalized_belief3 = (belief3 - min(belief3)) / (max(belief3) - min(belief3))) %>%
  ungroup()

library(plotrix)
traj_subset_trials_grouped_avg_across_session <- traj_subset_trials_grouped_normalized %>%
  dplyr::group_by(id,row,group) %>%
  dplyr::summarise(mean_belief3 = mean(normalized_belief3, na.rm = TRUE))

# Compute the rolling mean with a window size of 10 and add bin number
traj_subset_trials_binned_grouped <- traj_subset_trials_grouped_avg_across_session %>%
  group_by(id,group) %>%
  mutate(rolling_mean = rollapply(mean_belief3, width=1,FUN=mean,align="right",fill=NA),
         bin = if_else(row > 0, row-0, NA_integer_))

# remove rows with NA in rolling_mean
traj_subset_trials_binned_grouped <- traj_subset_trials_binned_grouped %>%
  tidyr::drop_na(rolling_mean)

traj3_binned_grouped_dat <- traj_subset_trials_binned_grouped[,c("id","group","bin","rolling_mean")]


stats <- traj3_binned_grouped_dat %>%
  dplyr::group_by(group,bin) %>%
  dplyr::summarise(mean_mu3 = mean(rolling_mean, na.rm = TRUE),
                   sem_mu3 = std.error(rolling_mean, na.rm = TRUE))


library(ggplot2)
source("C:/Pandemic_2020/revisions/code/theme_publication.R")
# Plot of impact oflesion on belief volatility
ggplot(data = stats) + geom_line(aes(x= bin, y = mean_mu3, color = group), size=2) +
  geom_ribbon(aes(x = bin, ymin = (mean_mu3 - sem_mu3), ymax = (mean_mu3 + sem_mu3), fill =group),alpha = 0.3, show.legend = F) + 
  geom_point(data = subset(stats, bin == 1),
             aes(x = bin, y = mean_mu3, color = group), shape = 1, size = 3, stroke = 2) +
  scale_color_manual(values = c("black", "#911EB4","#F58231"),
                     labels = c('control' = 'control',
                                'md' = 'MDmc',
                                'ofc' = 'OFC')) + 
  scale_fill_manual(values = c("black","#911EB4","#F58231")) + ylim(0,1) +
  scale_x_continuous(breaks = c(1,75,150,225,300)) +
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              legend.text = element_blank(),
                              legend.position = "none"
  )


# sub-plot that zooms into the trajectories in control and OFC-lesioned macaques
ggplot(data = stats) + geom_line(aes(x= bin, y = mean_mu3, color = group), size=2) +
  geom_ribbon(aes(x = bin, ymin = (mean_mu3 - sem_mu3), ymax = (mean_mu3 + sem_mu3), fill =group),alpha = 0.3, show.legend = F) + 
  #geom_vline(xintercept = 150) +
  geom_point(data = subset(stats, bin == 1),
             aes(x = bin, y = mean_mu3, color = group), shape = 1, size = 3, stroke = 2) +
  scale_color_manual(values = c("black", "#911EB4","#F58231"),
                     labels = c('control' = 'control',
                                'md' = 'MDmc',
                                'ofc' = 'OFC')) + 
  scale_fill_manual(values = c("black","#911EB4","#F58231")) + ylim(0,0.1) + # zoom into changes in control and OFC
  scale_x_continuous(breaks = c(1,75,150,225,300)) +
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              legend.text = element_blank(),
                              legend.position = "none"
  )




mdmc_sessions_dat <- read.csv("C:\\rotation\\steve\\data\\belief\\hgfToolBox\\output\\mdmc_sessions_alltrials_est.csv")
ofc_sessions_dat <- read.csv("C:\\rotation\\steve\\data\\belief\\hgfToolBox\\output\\ofc_sessions_alltrials_est.csv")


control_mdmc_ofc_bind <- rbind(mdmc_sessions_dat,
                               ofc_sessions_dat)

merge_hgf_dat <- merge(control_mdmc_ofc_bind, traj_bind[,c(1,4)], by=c("id"))

dat_sessions_df <-  merge_hgf_dat %>%
  dplyr::group_by(id,group) %>%
  dplyr::summarise(avg_mu02 = mean(mu02, na.rm = TRUE),
                   avg_mu03 = mean(mu03, na.rm = TRUE),
                   avg_m3 = mean(m3, na.rm = TRUE),
                   avg_kappa2 = mean(kappa2, na.rm = TRUE),
                   avg_omega2 = mean(omega2, na.rm = TRUE),
                   avg_omega3 = mean(omega3, na.rm = TRUE),
                   avg_bic = mean(BIC, na.rm = TRUE),
                   avg_lme = mean(LME, na.rm = TRUE))


dat_sessions_m3_df <- dat_sessions_df[,c("id","group","avg_m3")]

dat_sessions_m3_df$norm_m3 <- (dat_sessions_m3_df$avg_m3 - min(dat_sessions_m3_df$avg_m3)) / (max(dat_sessions_m3_df$avg_m3) - min(dat_sessions_m3_df$avg_m3))


stats <-  dat_sessions_m3_df %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(mean_m3 = mean(norm_m3,na.rm = TRUE),
                   sem_m3 = std.error(norm_m3,na.rm=TRUE))


stats$group <- factor(stats$group, levels = c("control","ofc","mdmc"))


ggplot(stats, aes(x = group, y= mean_m3, fill = as.factor(group))) +
  geom_bar(stat="identity", color = "black", lwd=1.2,
           position = position_dodge(0.9)) +
  geom_errorbar(data=stats, aes(x=group, ymin=mean_m3-sem_m3,
                                ymax=mean_m3+sem_m3), width=0.4, colour="black", alpha=0.9,
                position = position_dodge(0.9),
                size=2) + ylim(0,1) +
  scale_fill_manual("",values = c('gray','#F58231','#a74ac3')) +
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              axis.ticks.x = element_blank(), 
                              legend.text = element_blank(),
                              legend.position = "right"#,
                              #legend.direction = "horizontal"
  )


#######################################################################
#######################################################################
#######################################################################

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


# average across sessions - wsr
dat_wsr_sessions_df <- monkeys_stable_rates_dat %>%
  dplyr::group_by(id,group) %>%
  dplyr::summarise(avg_wsr = mean(wsr_stable, na.rm = TRUE)
  )

# average across sessions - lsr
dat_lsr_sessions_df <- monkeys_stable_rates_dat %>%
  dplyr::group_by(id,group) %>%
  dplyr::summarise(avg_lsr = mean(lsr_stable, na.rm = TRUE)
  )

# boxplot - win-switch rate
library(ggplot2)
ggplot(data = subset(dat_wsr_sessions_df, group %in% c("control", "ofc")),
       aes(x= as.factor(1), y=avg_wsr)) +
  geom_boxplot(fill = "darkgray", width=0.6, size=1.2, alpha=0.8) +
  
  # geom_point() is used to plot data points on boxplot
  geom_point(aes(fill=group), color="black", size=3, shape=21, position = position_dodge2(width = 0.4)) +
  ylim(0,1) +
  scale_fill_manual("Group",
                    labels = c('control' = 'control',
                               'ofc' = 'OFC lesion'),
                    values = c('control' = 'gray',
                               'ofc' = '#F58231')) +
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks.y = element_line(colour="black", size = 1.5),
                              axis.ticks.x = element_blank(),
                              legend.text = element_blank(),
                              legend.position = "right"
  )

# boxplot - lose-stay rate
library(ggplot2)
ggplot(data = subset(dat_lsr_sessions_df, group %in% c("control", "ofc")),
       aes(x= as.factor(1), y=avg_lsr)) +
  geom_boxplot(fill = "darkgray", width=0.6, size=1.2, alpha=0.8) +
  
  # geom_point() is used to plot data points on boxplot
  geom_point(aes(fill=group), color="black", size=3, shape=21, position = position_dodge2(width = 0.4)) +
  ylim(0,1) +
  scale_fill_manual("Group",
                    labels = c('control' = 'control',
                               'ofc' = 'OFC lesion'),
                    values = c('control' = 'gray',
                               'ofc' = '#F58231')) +
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks.y = element_line(colour="black", size = 1.5),
                              axis.ticks.x = element_blank(),
                              legend.text = element_blank(),
                              legend.position = "right"
  )



#######################################################################
#######################################################################
#######################################################################


