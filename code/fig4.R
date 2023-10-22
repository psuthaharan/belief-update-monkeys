split8080_prelockdown_dat <- read.csv("C:\\Users\\prave\\Documents\\Praveen\\research\\side_projects\\XAI_psychiatry\\hgfToolBox\\output\\est_prelockdown_split8080_M6.csv")
split8080_prelockdown_subset  <- split8080_prelockdown_dat [,c("id","m3_1","m3_2","omega2_1","omega2_2")]
colnames(split8080_prelockdown_subset) <- c("id","m3_1","m3_2","omega2_1","omega2_2")

split8080_lockdown_dat <- read.csv("C:\\Users\\prave\\Documents\\Praveen\\research\\side_projects\\XAI_psychiatry\\hgfToolBox\\output\\est_lockdown_split8080_M6.csv")
split8080_lockdown_subset  <- split8080_lockdown_dat [,c("id","m3_1","m3_2","omega2_1","omega2_2")]
colnames(split8080_lockdown_subset) <- c("id","m3_1","m3_2","omega2_1","omega2_2") 

split8080_postlockdown_dat <- read.csv("C:\\Users\\prave\\Documents\\Praveen\\research\\side_projects\\XAI_psychiatry\\hgfToolBox\\output\\est_postlockdown_split8080_M6.csv")
split8080_postlockdown_subset  <- split8080_postlockdown_dat [,c("id","m3_1","m3_2","omega2_1","omega2_2")]
colnames(split8080_postlockdown_subset) <- c("id","m3_1","m3_2","omega2_1","omega2_2")

split8080_subset <- rbind(split8080_prelockdown_subset,
                          split8080_lockdown_subset,
                          split8080_postlockdown_subset)


pandemic_dat <- read.csv("C:\\Users\\prave\\Documents\\Praveen\\research\\side_projects\\XAI_psychiatry\\pandemicPRL.csv")
pandemic_subset <- pandemic_dat[,c("study_id","id","dataset","period")]
colnames(pandemic_subset) <- c("id","mturk_id","dataset","period")

pandemic_subset <- pandemic_subset[which(pandemic_subset$dataset == "pandemic"),]


split8080_pandemic <- merge(split8080_subset,pandemic_subset,by=c("id"))
split8080_pandemic_subset <- split8080_pandemic[,c("mturk_id","m3_1","m3_2","omega2_1","omega2_2")]
colnames(split8080_pandemic_subset) <- c("id","m3_1","m3_2","omega2_1","omega2_2")


paranoia_dat <- read.csv("C:\\Users\\prave\\Documents\\Praveen\\research\\side_projects\\XAI_psychiatry\\paranoia_pandemicPRL.csv")
split8080_paranoia <- merge(split8080_pandemic_subset,paranoia_dat,by=c("id"))

split8080_paranoia_dat <- split8080_paranoia[,c("id","m3_1","m3_2","omega2_1","omega2_2","paranoia_group_binary")]

# split8080_psychopathology_dat <- merge(split8080_paranoia_dat,merge_docs, by="id")
# split8080_psychopathology_subset <- split8080_psychopathology_dat[,c("id","m3_1","m3_2","omega2_1","omega2_2",
#                                                                      "paranoia_mean_score","anxiety_mean_score","depression_mean_score","ocd_mean")]
# 
# split8080_psychopathology_subset$preRevm3_group <- ifelse(split8080_psychopathology_subset$m3_1 > mean(split8080_psychopathology_subset$m3_1),1,0)
# split8080_psychopathology_subset$preRevw2_group <- ifelse(split8080_psychopathology_subset$omega2_1 > mean(split8080_psychopathology_subset$omega2_1),1,0)
# split8080_psychopathology_subset$postRevm3_group <- ifelse(split8080_psychopathology_subset$m3_2 > mean(split8080_psychopathology_subset$m3_2),1,0)
# split8080_psychopathology_subset$postRevw2_group <- ifelse(split8080_psychopathology_subset$omega2_2 > mean(split8080_psychopathology_subset$omega2_2),1,0)  
# 
# 
# split8080_psychopathology_subset$m3w2Rev_group <- ifelse(split8080_psychopathology_subset$postRevm3_group == 0 & split8080_psychopathology_subset$preRevw2_group == 0 ,"LL",
#                                                          ifelse(split8080_psychopathology_subset$postRevm3_group == 0 & split8080_psychopathology_subset$preRevw2_group == 1,"LH",
#                                                                 ifelse(split8080_psychopathology_subset$postRevm3_group == 1 & split8080_psychopathology_subset$preRevw2_group == 0,"HL",
#                                                                        ifelse(split8080_psychopathology_subset$postRevm3_group == 1 & split8080_psychopathology_subset$preRevw2_group == 1,"HH",""))))


wsr_lsr_prepost <- read.csv("C:\\Users\\prave\\Documents\\Praveen\\research\\side_projects\\XAI_psychiatry\\wsr_lsr_prepost_dat.csv")

split8080_behavior <- merge(split8080_paranoia_dat,wsr_lsr_prepost,by="id")

split8080_paranoia_m3_dat <- split8080_paranoia_dat %>%
  rstatix::gather(key = "m3", value = "value", m3_1,m3_2) %>%
  rstatix::convert_as_factor(id,paranoia_group_binary)

split8080_paranoia_m3_dat$reversal <- rep(c("pre","post"), each=535)
split8080_paranoia_m3_dat$reversal <- factor(split8080_paranoia_m3_dat$reversal, levels = c("pre","post"))

p_width <- 0.8

ggplot(split8080_paranoia_m3_dat, aes(reversal,value, fill=paranoia_group_binary)) +
  # Explicitly set group aesthetic and width for geom_boxplot
  geom_boxplot(aes(group=interaction(reversal, paranoia_group_binary)), width=p_width, 
               position=position_dodge(width=p_width + 0.1), size=1.2, alpha=0.8) +  # Increase dodge width slightly more than boxplot width
  
  
  # linetype parameter is used to customize the joining line
  #geom_line(aes(group = paired), linetype=2, size=0.5, position = position_dodge2(0.5))+
  
  # geom_point() is used to plot data points on boxplot
  #geom_point(aes(fill=group),size=2,shape=21, position = position_dodge2(0.7)) + #facet_wrap(~lesion, labeller = labeller(lesion = c("control" = "control",
  # "lesion." = "LOFC lesion"))) +
  scale_fill_manual("Group",
                    labels = c('0' = 'low paranoia',
                               '1' = 'high paranoia'),
                    values = c("#5B8FAF","#800000") #993232 #a74ac3 #999932 #f79b5a
  ) + 
  ylab(expression(m[3])) + scale_x_discrete(labels=c("pre" = "pre-contingency shift",
                                                     "post" = "post-contingency shift")) +
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              legend.text = element_blank(),
                              legend.position = "none"
  )




gaussian_pandemic_humans_m3 <- glmmTMB(value ~ paranoia_group_binary*reversal + (1|id), data = split8080_paranoia_m3_dat,
                                       family = gaussian)
summary(gaussian_pandemic_humans_m3)
Anova(gaussian_pandemic_humans_m3,type = "III")

gaussian_pandemic_humans_m3_pre <- glmmTMB(value ~ paranoia_group_binary, data = subset(split8080_paranoia_m3_dat, reversal == "pre"),
                                       family = gaussian)
summary(gaussian_pandemic_humans_m3_pre)
Anova(gaussian_pandemic_humans_m3_pre,type = "III")

gaussian_pandemic_humans_m3_post <- glmmTMB(value ~ paranoia_group_binary, data = subset(split8080_paranoia_m3_dat, reversal == "post"),
                                           family = gaussian)
summary(gaussian_pandemic_humans_m3_post)
Anova(gaussian_pandemic_humans_m3_post,type = "III")


gaussian_pandemic_humans_m3_low <- glmmTMB(value ~ reversal, data = subset(split8080_paranoia_m3_dat, paranoia_group_binary == 0),
                                           family = gaussian)
summary(gaussian_pandemic_humans_m3_low)
Anova(gaussian_pandemic_humans_m3_low,type = "III")

gaussian_pandemic_humans_m3_high <- glmmTMB(value ~ reversal, data = subset(split8080_paranoia_m3_dat, paranoia_group_binary == 1),
                                            family = gaussian)
summary(gaussian_pandemic_humans_m3_high)
Anova(gaussian_pandemic_humans_m3_high,type = "III")


split8080_paranoia_w2_dat <- split8080_paranoia_dat %>%
  rstatix::gather(key = "w2", value = "value", omega2_1,omega2_2) %>%
  rstatix::convert_as_factor(id,paranoia_group_binary)

split8080_paranoia_w2_dat$reversal <- rep(c("pre","post"), each=535)
split8080_paranoia_w2_dat$reversal <- factor(split8080_paranoia_w2_dat$reversal, levels = c("pre","post"))


ggplot(split8080_paranoia_w2_dat, aes(reversal,value, fill=paranoia_group_binary)) +
  # Explicitly set group aesthetic and width for geom_boxplot
  geom_boxplot(aes(group=interaction(reversal, paranoia_group_binary)), width=p_width, 
               position=position_dodge(width=p_width + 0.1), size=1.2, alpha=0.8) +  # Increase dodge width slightly more than boxplot width
  
  
  
  # linetype parameter is used to customize the joining line
  #geom_line(aes(group = paired), linetype=2, size=0.5, position = position_dodge2(0.5))+
  
  # geom_point() is used to plot data points on boxplot
  #geom_point(aes(fill=group),size=2,shape=21, position = position_dodge2(0.7)) + #facet_wrap(~lesion, labeller = labeller(lesion = c("control" = "control",
  # "lesion." = "LOFC lesion"))) +
  scale_fill_manual("Group",
                    labels = c('0' = 'low paranoia',
                               '1' = 'high paranoia'),
                    values = c("#5B8FAF","#800000") #993232 #a74ac3 #999932 #f79b5a
  ) + 
  ylab(expression(omega[2])) + scale_x_discrete(labels=c("pre" = "pre-contingency shift",
                                                     "post" = "post-contingency shift")) +
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              legend.text = element_blank(),
                              legend.position = "none"
  )



gaussian_pandemic_humans_w2 <- glmmTMB(value ~ paranoia_group_binary*reversal + (1|id), data = split8080_paranoia_w2_dat,
                                       family = gaussian)
summary(gaussian_pandemic_humans_w2)
Anova(gaussian_pandemic_humans_w2,type = "III")

gaussian_pandemic_humans_w2_pre <- glmmTMB(value ~ paranoia_group_binary, data = subset(split8080_paranoia_w2_dat, reversal == "pre"),
                                           family = gaussian)
summary(gaussian_pandemic_humans_w2_pre)
Anova(gaussian_pandemic_humans_w2_pre,type = "III")

gaussian_pandemic_humans_w2_post <- glmmTMB(value ~ paranoia_group_binary, data = subset(split8080_paranoia_w2_dat, reversal == "post"),
                                            family = gaussian)
summary(gaussian_pandemic_humans_w2_post)
Anova(gaussian_pandemic_humans_w2_post,type = "III")


gaussian_pandemic_humans_w2_low <- glmmTMB(value ~ reversal, data = subset(split8080_paranoia_w2_dat, paranoia_group_binary == 0),
                                           family = gaussian)
summary(gaussian_pandemic_humans_w2_low)
Anova(gaussian_pandemic_humans_w2_low,type = "III")

gaussian_pandemic_humans_w2_high <- glmmTMB(value ~ reversal, data = subset(split8080_paranoia_w2_dat, paranoia_group_binary == 1),
                                            family = gaussian)
summary(gaussian_pandemic_humans_w2_high)
Anova(gaussian_pandemic_humans_w2_high,type = "III")


split8080_behavior_wsr <- split8080_behavior%>%
  gather(key = "wsr", value = "value", wsr_stable_pre,wsr_stable_post) %>%
  convert_as_factor(id,paranoia_group_binary)

split8080_behavior_wsr$reversal <- rep(c("pre","post"), each=535)
split8080_behavior_wsr$reversal <- factor(split8080_behavior_wsr$reversal, levels = c("pre","post"))


ggplot(split8080_behavior_wsr, aes(reversal,value, fill=paranoia_group_binary)) +
  geom_boxplot()+
  
  # linetype parameter is used to customize the joining line
  #geom_line(aes(group = paired), linetype=2, size=0.5, position = position_dodge2(0.5))+
  
  # geom_point() is used to plot data points on boxplot
  #geom_point(aes(fill=group),size=2,shape=21, position = position_dodge2(0.7)) + #facet_wrap(~lesion, labeller = labeller(lesion = c("control" = "control",
  # "lesion." = "LOFC lesion"))) +
  scale_fill_manual("Group",
                    labels = c('0' = 'low paranoia',
                               '1' = 'high paranoia'),
                    values = c("#5B8FAF","#800000") #993232 #a74ac3 #999932 #f79b5a
  ) + 
  ylab("win-switch rate") + scale_x_discrete(labels=c("pre" = "pre-contingency shift",
                                                         "post" = "post-contingency shift")) +
  theme_Publication() + theme(strip.background = element_rect(colour="white", fill="white"))





split8080_behavior_lsr <- split8080_behavior %>%
  gather(key = "lsr", value = "value", lsr_stable_pre,lsr_stable_post) %>%
  convert_as_factor(id,paranoia_group_binary)

split8080_behavior_lsr$reversal <- rep(c("pre","post"), each=130)
split8080_behavior_lsr$reversal <- factor(split8080_behavior_lsr$reversal, levels = c("pre","post"))


ggplot(split8080_behavior_lsr, aes(reversal,value, fill=paranoia_group_binary)) +
  geom_boxplot()+
  
  # linetype parameter is used to customize the joining line
  #geom_line(aes(group = paired), linetype=2, size=0.5, position = position_dodge2(0.5))+
  
  # geom_point() is used to plot data points on boxplot
  #geom_point(aes(fill=group),size=2,shape=21, position = position_dodge2(0.7)) + #facet_wrap(~lesion, labeller = labeller(lesion = c("control" = "control",
  # "lesion." = "LOFC lesion"))) +
  scale_fill_manual("Group",
                    labels = c('0' = 'low paranoia',
                               '1' = 'high paranoia'),
                    values = c("#5B8FAF","#800000") #993232 #a74ac3 #999932 #f79b5a
  ) + 
  ylab("lose-stay rate") + scale_x_discrete(labels=c("pre" = "pre-contingency shift",
                                                         "post" = "post-contingency shift")) +
  theme_Publication() + theme(strip.background = element_rect(colour="white", fill="white"))






stats <- split8080_psychopathology_subset %>%
  dplyr::group_by(m3w2Rev_group) %>%
  dplyr::summarise(mean_paranoia = mean(paranoia_mean_score,na.rm = TRUE),
                   sem_paranoia = std.error(paranoia_mean_score,na.rm=TRUE))

g1 <- ggplot(stats, aes(x = m3w2Rev_group, y= mean_paranoia, fill = as.factor(m3w2Rev_group))) +
  geom_bar(stat="identity", color = "black", lwd=1.2,
           position = position_dodge(0.9)) +
  geom_errorbar(data=stats, aes(x=m3w2Rev_group, ymin=mean_paranoia-sem_paranoia,
                                ymax=mean_paranoia+sem_paranoia), width=0.4, colour="black", alpha=0.9,
                position = position_dodge(0.9),
                size=2) +
  scale_fill_manual("",values = c("#4A6D7A",
                                  "#0063A5",
                                  "#DA1F23",
                                  "purple")) + ylim(0,2)


stats <- split8080_psychopathology_subset %>%
  dplyr::group_by(m3w2Rev_group) %>%
  dplyr::summarise(mean_anxiety = mean(anxiety_mean_score,na.rm = TRUE),
                   sem_anxiety = std.error(anxiety_mean_score,na.rm=TRUE))

g2 <- ggplot(stats, aes(x = m3w2Rev_group, y= mean_anxiety, fill = as.factor(m3w2Rev_group))) +
  geom_bar(stat="identity", color = "black", lwd=1.2,
           position = position_dodge(0.9)) +
  geom_errorbar(data=stats, aes(x=m3w2Rev_group, ymin=mean_anxiety-sem_anxiety,
                                ymax=mean_anxiety+sem_anxiety), width=0.4, colour="black", alpha=0.9,
                position = position_dodge(0.9),
                size=2) +
  scale_fill_manual("",values = c("#4A6D7A",
                                  "#0063A5",
                                  "#DA1F23",
                                  "purple")) + ylim(0,2)


stats <- split8080_psychopathology_subset %>%
  dplyr::group_by(m3w2Rev_group) %>%
  dplyr::summarise(mean_depression = mean(depression_mean_score,na.rm = TRUE),
                   sem_depression = std.error(depression_mean_score,na.rm=TRUE))

g3 <- ggplot(stats, aes(x = m3w2Rev_group, y= mean_depression, fill = as.factor(m3w2Rev_group))) +
  geom_bar(stat="identity", color = "black", lwd=1.2,
           position = position_dodge(0.9)) +
  geom_errorbar(data=stats, aes(x=m3w2Rev_group, ymin=mean_depression-sem_depression,
                                ymax=mean_depression+sem_depression), width=0.4, colour="black", alpha=0.9,
                position = position_dodge(0.9),
                size=2) +
  scale_fill_manual("",values = c("#4A6D7A",
                                  "#0063A5",
                                  "#DA1F23",
                                  "purple")) + ylim(0,2)


stats <- split8080_psychopathology_subset %>%
  dplyr::group_by(m3w2Rev_group) %>%
  dplyr::summarise(mean_ocd = mean(ocd_mean,na.rm = TRUE),
                   sem_ocd = std.error(ocd_mean,na.rm=TRUE))

g4 <- ggplot(stats, aes(x = m3w2Rev_group, y= mean_ocd, fill = as.factor(m3w2Rev_group))) +
  geom_bar(stat="identity", color = "black", lwd=1.2,
           position = position_dodge(0.9)) +
  geom_errorbar(data=stats, aes(x=m3w2Rev_group, ymin=mean_ocd-sem_ocd,
                                ymax=mean_ocd+sem_ocd), width=0.4, colour="black", alpha=0.9,
                position = position_dodge(0.9),
                size=2) +
  scale_fill_manual("",values = c("#4A6D7A",
                                  "#0063A5",
                                  "#DA1F23",
                                  "purple")) + ylim(0,2)

library(ggpubr)
ggarrange(g1,g2,g3,g4, ncol=4)



############ BARNBY RESULTS ######

barnby_paranoia_dat <- read.csv("C:\\rotation\\steve\\human\\ProbabilisticCleaned.csv")
barnby_paranoia_subset <- barnby_paranoia_dat[,c("ID","Persec")]
colnames(barnby_paranoia_subset) <- c("id","Persec")

df_deduped <- barnby_paranoia_subset  %>% distinct(id, Persec, .keep_all = TRUE)

barnby_hgf_dat <- read.csv("C:\\rotation\\steve\\human\\est_barnby_split8080_M6.csv")
barnby_hgf_subset <- barnby_hgf_dat[,c("id","m3_1","m3_2","omega2_1","omega2_2","mu03_1","mu03_2","kappa2_1","kappa2_2","omega3_1","omega3_2")]


barnby_hgf_paranoia <- merge(barnby_hgf_subset,df_deduped,by="id")
barnby_hgf_paranoia$paranoia_group <- ifelse(barnby_hgf_paranoia$Persec >= 11, 1,0)

barnby_hgf_paranoia_long  <- barnby_hgf_paranoia  %>%
  rstatix::gather(key = "m3", value = "value", m3_1, m3_2) %>%
  rstatix::convert_as_factor(id,paranoia_group)

barnby_hgf_paranoia_long$reversal <- rep(c("pre","post"), each=692)
barnby_hgf_paranoia_long$reversal <- factor(barnby_hgf_paranoia_long$reversal, levels = c("pre","post"))



p_width <- 0.8

ggplot(barnby_hgf_paranoia_long, aes(reversal,value, fill=as.factor(paranoia_group))) +
  
  # Explicitly set group aesthetic and width for geom_boxplot
  geom_boxplot(aes(group=interaction(reversal, paranoia_group)), width=p_width, 
               position=position_dodge(width=p_width + 0.1), size=1.2, alpha=0.8) +  # Increase dodge width slightly more than boxplot width
  
  
  # linetype parameter is used to customize the joining line
  #geom_line(aes(group = paired), linetype=2, size=0.5, position = position_dodge2(0.5))+
  
  # geom_point() is used to plot data points on boxplot
  #geom_point(aes(fill=group),size=2,shape=21, position = position_dodge2(0.7)) + #facet_wrap(~lesion, labeller = labeller(lesion = c("control" = "control",
  # "lesion." = "LOFC lesion"))) +
  scale_fill_manual("Group",
                    labels = c('0' = 'low paranoia',
                               '1' = 'high paranoia'),
                    values = c("#5B8FAF","#800000") #993232 #a74ac3 #999932 #f79b5a
  ) + 
  ylab(expression(m[3])) + scale_x_discrete(labels=c("pre" = "pre-reversal",
                                                     "post" = "post-reversal")) +
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              legend.text = element_blank(),
                              legend.position = "none"
  )




barnby_hgf_paranoia_long  <- barnby_hgf_paranoia  %>%
  rstatix::gather(key = "w2", value = "value", omega2_1, omega2_2) %>%
  rstatix::convert_as_factor(id,paranoia_group)

barnby_hgf_paranoia_long$reversal <- rep(c("pre","post"), each=692)
barnby_hgf_paranoia_long$reversal <- factor(barnby_hgf_paranoia_long$reversal, levels = c("pre","post"))




ggplot(barnby_hgf_paranoia_long, aes(reversal,value, fill=as.factor(paranoia_group))) +
  # Explicitly set group aesthetic and width for geom_boxplot
  geom_boxplot(aes(group=interaction(reversal, paranoia_group)), width=p_width, 
               position=position_dodge(width=p_width + 0.1), size=1.2, alpha=0.8) +  # Increase dodge width slightly more than boxplot width
  
  
  # linetype parameter is used to customize the joining line
  #geom_line(aes(group = paired), linetype=2, size=0.5, position = position_dodge2(0.5))+
  
  # geom_point() is used to plot data points on boxplot
  #geom_point(aes(fill=group),size=2,shape=21, position = position_dodge2(0.7)) + #facet_wrap(~lesion, labeller = labeller(lesion = c("control" = "control",
  # "lesion." = "LOFC lesion"))) +
  scale_fill_manual("Group",
                    labels = c('0' = 'low paranoia',
                               '1' = 'high paranoia'),
                    values = c("#5B8FAF","#800000") #993232 #a74ac3 #999932 #f79b5a
  ) + 
  ylab(expression(omega[2])) + scale_x_discrete(labels=c("pre" = "pre-reversal",
                                                     "post" = "post-reversal")) +
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              legend.text = element_blank(),
                              legend.position = "none"
  )



barnby_m3 <- glmmTMB(value ~ paranoia_group*reversal + (1|id), data = barnby_hgf_paranoia_long,
                               family = gaussian)
summary(barnby_m3)
Anova(barnby_m3,type = "III")
# 


barnby_pre_m3 <- glmmTMB(value ~ paranoia_group, data = subset(barnby_hgf_paranoia_long, reversal == "pre"),
                     family = gaussian)
summary(barnby_pre_m3)
Anova(barnby_pre_m3,type = "III")

barnby_post_m3 <- glmmTMB(value ~ paranoia_group, data = subset(barnby_hgf_paranoia_long, reversal == "post"),
                     family = gaussian)
summary(barnby_post_m3)
Anova(barnby_post_m3,type = "III")


# effect of reversal on m3 within each group
barnby_m3_low <- glmmTMB(value ~ reversal + (1|id), data = subset(barnby_hgf_paranoia_long, paranoia_group == 0),
                         family = gaussian)
summary(barnby_m3_low)
Anova(barnby_m3_low,type = "III")


barnby_m3_high <- glmmTMB(value ~ reversal + (1|id), data = subset(barnby_hgf_paranoia_long, paranoia_group == 1),
                         family = gaussian)
summary(barnby_m3_high)
Anova(barnby_m3_high,type = "III")


##########################

barnby_w2 <- glmmTMB(value ~ paranoia_group*reversal + (1|id), data = barnby_hgf_paranoia_long,
                     family = gaussian)
summary(barnby_w2)
Anova(barnby_w2,type = "III")


barnby_pre_w2 <- glmmTMB(value ~ paranoia_group, data = subset(barnby_hgf_paranoia_long, reversal == "pre"),
                          family = gaussian)
summary(barnby_pre_w2)
Anova(barnby_pre_w2,type = "III")

barnby_post_w2 <- glmmTMB(value ~ paranoia_group, data = subset(barnby_hgf_paranoia_long, reversal == "post"),
                     family = gaussian)
summary(barnby_post_w2)
Anova(barnby_post_w2,type = "III")


# effect of reversal on m3 within each group
barnby_w2_low <- glmmTMB(value ~ reversal + (1|id), data = subset(barnby_hgf_paranoia_long, paranoia_group == 0),
                         family = gaussian)
summary(barnby_w2_low)
Anova(barnby_w2_low,type = "III")


barnby_w2_high <- glmmTMB(value ~ reversal + (1|id), data = subset(barnby_hgf_paranoia_long, paranoia_group == 1),
                          family = gaussian)
summary(barnby_w2_high)
Anova(barnby_w2_high,type = "III")



barnby_hgf_paranoia_long  <- barnby_hgf_paranoia  %>%
  gather(key = "kappa2", value = "value", omega3_1, omega3_2) %>%
  convert_as_factor(id,paranoia_group)

barnby_hgf_paranoia_long$reversal <- rep(c("pre","post"), each=692)
barnby_hgf_paranoia_long$reversal <- factor(barnby_hgf_paranoia_long$reversal, levels = c("pre","post"))




ggplot(barnby_hgf_paranoia_long, aes(reversal,value, fill=as.factor(paranoia_group))) +
  geom_boxplot()+
  
  # linetype parameter is used to customize the joining line
  #geom_line(aes(group = paired), linetype=2, size=0.5, position = position_dodge2(0.5))+
  
  # geom_point() is used to plot data points on boxplot
  #geom_point(aes(fill=group),size=2,shape=21, position = position_dodge2(0.7)) + #facet_wrap(~lesion, labeller = labeller(lesion = c("control" = "control",
  # "lesion." = "LOFC lesion"))) +
  scale_fill_manual("Group",
                    labels = c('0' = 'low paranoia',
                               '1' = 'high paranoia'),
                    values = c("#5B8FAF","#800000") #993232 #a74ac3 #999932 #f79b5a
  ) + 
  ylab(expression(omega[3])) + scale_x_discrete(labels=c("pre" = "pre-reversal",
                                                     "post" = "post-reversal")) +
  theme_Publication() + theme(strip.background = element_rect(colour="white", fill="white"))



### WSR and LSR from barnby dataset


barnby_paranoia_dat <- read.csv("C:\\rotation\\steve\\human\\ProbabilisticCleaned.csv")
barnby_paranoia_subset <- barnby_paranoia_dat[,c("ID","Persec")]
colnames(barnby_paranoia_subset) <- c("id","Persec")

df_deduped <- barnby_paranoia_subset  %>% distinct(id, Persec, .keep_all = TRUE)

barnby_hgf_dat <- read.csv("C:\\rotation\\steve\\human\\est_barnby_split8080_M6.csv")
barnby_hgf_subset <- barnby_hgf_dat[,c("id","m3_1","m3_2","omega2_1","omega2_2","mu03_1","mu03_2","kappa2_1","kappa2_2","omega3_1","omega3_2")]


barnby_hgf_paranoia <- merge(barnby_hgf_subset,df_deduped,by="id")
barnby_hgf_paranoia$paranoia_group <- ifelse(barnby_hgf_paranoia$Persec >= 11, 1,0)


barnby_dat <- read.csv("C:\\rotation\\steve\\human\\ProbabilisticCleaned.csv")

barnby_subset <- barnby_dat[,c("ID","Trial","Block","Selection","Correct","Persec")]
colnames(barnby_subset) <- c("id","Trial","Block","Selection","Correct","Persec")
barnby_subset$Selection <- ifelse(barnby_subset$Selection == "urn1.png",1,
                                  ifelse(barnby_subset$Selection == "urn2.png",2,
                                         ifelse(barnby_subset$Selection == "urn3.png",3,"")))

barnby_subset$Selection <- as.numeric(barnby_subset$Selection)



barnby_merged <- merge(barnby_subset,barnby_hgf_paranoia,by="id")

barnby_task_dat <- barnby_merged[,c("id","Trial","Block","Selection","Correct")]


barnby_task_ordered_dat <- barnby_task_dat[order(barnby_task_dat$id, barnby_task_dat$Block, barnby_task_dat$Trial),]
colnames(barnby_task_ordered_dat) <- c("id","trial","block","response_color","reward_type")


data_list <- list()

unique_subject_id <- unique(barnby_task_ordered_dat$id)

for (i in 1:length(unique_subject_id)){
  
  data_list[[i]] <- subset(barnby_task_ordered_dat, id == unique_subject_id[i])
}

bin_size = 20
matrix_df <- list()
response_color_timeseries <- list()
wsr_lsr_dat <- list()
wsr_binned <- c()
lsr_binned <- c()
wsr_timeseries_df <- list()
lsr_timeseries_df <- list()
timeseries_ma_df <- list()
timeseries_count_df <- list()
timeseries_ma_combined_df <- list()
avg_timeseries <- list()
timeseries_wsr_ma_transposed <- list()

for (i in 1:length(data_list)){
  
  for (t in 1:length(data_list[[i]]$trial)){
    data_list[[i]]$win_switch[t] <- ifelse(data_list[[i]]$reward_type[t-1] == 1 && (data_list[[i]]$response_color[t] != data_list[[i]]$response_color[t-1]),1,0)
    data_list[[i]]$win_stay[t] <- ifelse(data_list[[i]]$reward_type[t-1] == 1 && (data_list[[i]]$response_color[t] == data_list[[i]]$response_color[t-1]),1,0)
    data_list[[i]]$lose_switch[t] <- ifelse(data_list[[i]]$reward_type[t-1] == 0 && (data_list[[i]]$response_color[t] != data_list[[i]]$response_color[t-1]),1,0)
    data_list[[i]]$lose_stay[t] <- ifelse(data_list[[i]]$reward_type[t-1] == 0 && (data_list[[i]]$response_color[t] == data_list[[i]]$response_color[t-1]),1,0)
    }

  win_switch_rate_stable <- sum(data_list[[i]]$win_switch, na.rm = TRUE)/(sum(data_list[[i]]$win_switch, na.rm = TRUE) + sum(data_list[[i]]$win_stay, na.rm = TRUE))
  lose_stay_rate_stable <- sum(data_list[[i]]$lose_stay, na.rm = TRUE)/(sum(data_list[[i]]$lose_stay, na.rm = TRUE) + sum(data_list[[i]]$lose_switch, na.rm = TRUE))
  
  win_switch_rate_stable_pre <- sum(data_list[[i]]$win_switch[1:30], na.rm = TRUE)/(sum(data_list[[i]]$win_switch[1:30], na.rm = TRUE) + sum(data_list[[i]]$win_stay[1:30], na.rm = TRUE))
  win_switch_rate_stable_post <- sum(data_list[[i]]$win_switch[31:60], na.rm = TRUE)/(sum(data_list[[i]]$win_switch[31:60], na.rm = TRUE) + sum(data_list[[i]]$win_stay[31:60], na.rm = TRUE))
  
  lose_stay_rate_stable_pre <- sum(data_list[[i]]$lose_stay[1:30], na.rm = TRUE)/(sum(data_list[[i]]$lose_stay[1:30], na.rm = TRUE) + sum(data_list[[i]]$lose_switch[1:30], na.rm = TRUE))
  lose_stay_rate_stable_post <- sum(data_list[[i]]$lose_stay[31:60], na.rm = TRUE)/(sum(data_list[[i]]$lose_stay[31:60], na.rm = TRUE) + sum(data_list[[i]]$lose_switch[31:60], na.rm = TRUE))
  
  win_stay_count <- sum(data_list[[i]]$win_stay, na.rm = TRUE)
  win_switch_count <- sum(data_list[[i]]$win_switch, na.rm = TRUE)
  
  lose_stay_count <- sum(data_list[[i]]$lose_stay, na.rm = TRUE)
  lose_switch_count <- sum(data_list[[i]]$lose_switch, na.rm = TRUE)
  
  
  win_stay_count_pre <- sum(data_list[[i]]$win_stay[1:30], na.rm = TRUE)
  win_switch_count_pre <- sum(data_list[[i]]$win_switch[1:30], na.rm = TRUE)
  
  lose_stay_count_pre <- sum(data_list[[i]]$lose_stay[1:30], na.rm = TRUE)
  lose_switch_count_pre <- sum(data_list[[i]]$lose_switch[1:30], na.rm = TRUE)
  
  win_stay_count_post <- sum(data_list[[i]]$win_stay[31:60], na.rm = TRUE)
  win_switch_count_post <- sum(data_list[[i]]$win_switch[31:60], na.rm = TRUE)
  
  lose_stay_count_post <- sum(data_list[[i]]$lose_stay[31:60], na.rm = TRUE)
  lose_switch_count_post <- sum(data_list[[i]]$lose_switch[31:60], na.rm = TRUE)
  
  wsr_lsr_dat[[i]] <- data.frame(id = unique(data_list[[i]]$id),
                                 wsr_stable = win_switch_rate_stable,
                                 lsr_stable = lose_stay_rate_stable,
                                 wsr_stable_pre = win_switch_rate_stable_pre,
                                 wsr_stable_post = win_switch_rate_stable_post,
                                 lsr_stable_pre = lose_stay_rate_stable_pre,
                                 lsr_stable_post = lose_stay_rate_stable_post,
                                 wsc_stay = win_stay_count,
                                 wsc_stay_pre = win_stay_count_pre,
                                 wsc_stay_post = win_stay_count_post,
                                 wsc_switch = win_switch_count,
                                 wsc_switch_pre = win_switch_count_pre,
                                 wsc_switch_post = win_switch_count_post,
                                 # wsr_switch = win_switch_rate_stable,
                                 # wsr_switch_pre = win_switch_rate_stable_pre,
                                 # wsr_switch_post = win_switch_rate_stable_post,
                                 lsc_stay = lose_stay_count,
                                 lsc_stay_pre = lose_stay_count_pre,
                                 lsc_stay_post = lose_stay_count_post,
                                 lsc_switch = lose_switch_count,
                                 lsc_switch_pre = lose_switch_count_pre,
                                 lsc_switch_post = lose_switch_count_post)
  
  for (b in 1:(nrow(data_list[[i]]) - (bin_size-1))){
    
    wsr_binned[b] <- sum(data_list[[i]]$win_switch[b:(b+(bin_size-1))], na.rm = TRUE)/(sum(data_list[[i]]$win_switch[b:(b+(bin_size-1))], na.rm = TRUE) + sum(data_list[[i]]$win_stay[b:(b+(bin_size-1))], na.rm = TRUE))
    lsr_binned[b] <- sum(data_list[[i]]$lose_stay[b:(b+(bin_size-1))], na.rm = TRUE)/(sum(data_list[[i]]$lose_stay[b:(b+(bin_size-1))], na.rm = TRUE) + sum(data_list[[i]]$lose_switch[b:(b+(bin_size-1))], na.rm = TRUE))
    
    
  }
  

  
  
  timeseries_ma_df[[i]] = data.frame(id = unique(data_list[[i]]$id),
                                     bins = 1:(60-(bin_size-1)),
                                     wsr_ma = wsr_binned,
                                     lsr_ma = lsr_binned)
  
 
  
  timeseries_wsr_ma_transposed[[i]] <- data.frame(id = unique(data_list[[i]]$id),
                                                  wsr_moving_average = t(wsr_binned),
                                                  lsr_moving_average = t(lsr_binned))
  
  
  
}


rates_timeseries_df <- rbindlist(timeseries_ma_df)
wsr_lsr_df <- rbindlist(wsr_lsr_dat)


paranoia_dat <- barnby_hgf_paranoia[,c("id","paranoia_group")]

counts_timeseries_merged <- merge(paranoia_dat,wsr_lsr_df)

t.test(counts_timeseries_merged$wsr_stable_pre ~ counts_timeseries_merged$paranoia_group,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)


counts_sum_timeseries_merged <- counts_timeseries_merged %>%
  dplyr::group_by(id,paranoia_group) %>%
  dplyr::summarise(sum_wsc_stay_pre = sum(wsc_stay_pre,na.rm = TRUE),
                   sum_wsc_stay_post = sum(wsc_stay_post,na.rm = TRUE),
                   sum_wsc_switch_pre = sum(wsc_switch_pre,na.rm = TRUE),
                   sum_wsc_switch_post = sum(wsc_switch_post,na.rm = TRUE),
                   sum_lsc_stay_pre = sum(lsc_stay_pre,na.rm = TRUE),
                   sum_lsc_stay_post = sum(lsc_stay_post,na.rm = TRUE),
                   sum_lsc_switch_pre = sum(lsc_switch_pre,na.rm = TRUE),
                   sum_lsc_switch_post = sum(lsc_switch_post,na.rm = TRUE))


barnby_count_rates_win_stay_long <- counts_sum_timeseries_merged %>%
  gather(key = "reversal", value = "win_stay_count", sum_wsc_stay_pre, sum_wsc_stay_post) %>%
  convert_as_factor(id,paranoia_group)
barnby_count_rates_win_stay_long$reversal <- ifelse(barnby_count_rates_win_stay_long$reversal == "sum_wsc_stay_pre","pre","post")

barnby_count_rates_lose_stay_long <- counts_sum_timeseries_merged %>%
  gather(key = "reversal", value = "lose_stay_count", sum_lsc_stay_pre, sum_lsc_stay_post) %>%
  convert_as_factor(id,paranoia_group)
barnby_count_rates_lose_stay_long$reversal <- ifelse(barnby_count_rates_lose_stay_long$reversal == "sum_lsc_stay_pre","pre","post")

merge_stay_count <- merge(barnby_count_rates_win_stay_long[,c("id","paranoia_group","reversal","win_stay_count")], 
                          barnby_count_rates_lose_stay_long[,c("id","reversal","lose_stay_count")], by=c("id","reversal"))

barnby_count_rates_win_switch_long <- counts_sum_timeseries_merged %>%
  gather(key = "reversal", value = "win_switch_count", sum_wsc_switch_pre, sum_wsc_switch_post) %>%
  convert_as_factor(id,paranoia_group)
barnby_count_rates_win_switch_long$reversal <- ifelse(barnby_count_rates_win_switch_long$reversal == "sum_wsc_switch_pre","pre","post")

barnby_count_rates_lose_switch_long<- counts_sum_timeseries_merged %>%
  gather(key = "reversal", value = "lose_switch_count", sum_lsc_switch_pre, sum_lsc_switch_post) %>%
  convert_as_factor(id,paranoia_group)
barnby_count_rates_lose_switch_long$reversal <- ifelse(barnby_count_rates_lose_switch_long$reversal == "sum_lsc_switch_pre","pre","post")

merge_switch_count <- merge(barnby_count_rates_win_switch_long[,c("id","paranoia_group","reversal","win_switch_count")], 
                            barnby_count_rates_lose_switch_long[,c("id","reversal","lose_switch_count")], by=c("id","reversal"))

merged_dat <- merge(merge_stay_count,
                    merge_switch_count, by=c("id","paranoia_group","reversal"))


merged_dat$reversal <- as.factor(merged_dat$reversal)
merged_dat$reversal <- factor(merged_dat$reversal, levels = c("pre","post"))
binom_barnby_wsc <- glmmTMB(cbind(win_switch_count,win_stay_count) ~ paranoia_group*reversal + (1|id), data = merged_dat,
                             family = binomial)
summary(binom_barnby_wsc)
Anova(binom_barnby_wsc,type = "III")


binom_barnby_lsc <- glmmTMB(cbind(lose_stay_count,lose_switch_count) ~ paranoia_group*reversal + (1|id), data = merged_dat,
                            family = binomial)
summary(binom_barnby_lsc)
Anova(binom_barnby_lsc,type = "III")

# win-switch
binom_barnby_wsc_pre <- glmmTMB(cbind(win_switch_count,win_stay_count) ~ paranoia_group + (1|id), data = subset(merged_dat, reversal == "pre"),
                            family = binomial)
summary(binom_barnby_wsc_pre)
Anova(binom_barnby_wsc_pre,type = "III")

binom_barnby_wsc_post <- glmmTMB(cbind(win_switch_count,win_stay_count) ~ paranoia_group + (1|id), data = subset(merged_dat, reversal == "post"),
                                family = binomial)
summary(binom_barnby_wsc_post)
Anova(binom_barnby_wsc_post,type = "III")

# lose-stay
binom_barnby_lsc_pre <- glmmTMB(cbind(lose_stay_count,lose_switch_count) ~ paranoia_group + (1|id), data = subset(merged_dat, reversal == "pre"),
                                family = binomial)
summary(binom_barnby_lsc_pre)
Anova(binom_barnby_lsc_pre,type = "III")

binom_barnby_lsc_post <- glmmTMB(cbind(lose_stay_count,lose_switch_count) ~ paranoia_group + (1|id), data = subset(merged_dat, reversal == "post"),
                                 family = binomial)
summary(binom_barnby_lsc_post)
Anova(binom_barnby_lsc_post,type = "III")

rates_timeseries_merged <- merge(paranoia_dat,rates_timeseries_df)

library(plotrix)
stats <- rates_timeseries_merged %>%
  dplyr::group_by(paranoia_group,bins) %>%
  dplyr::summarise(mean_wsr = mean(wsr_ma, na.rm = TRUE),
                   sem_wsr = std.error(wsr_ma, na.rm = TRUE),
                   mean_lsr = mean(lsr_ma, na.rm = TRUE),
                   sem_lsr = std.error(lsr_ma, na.rm = TRUE))


wsr_plot <- ggplot(data = stats) + geom_line(aes(x= bins, y = mean_wsr, 
                                                 color = as.factor(paranoia_group)#, 
                                                 #linetype = group
                                                 #size= 0.3
), size=2) +
  geom_ribbon(aes(x = bins, ymin = (mean_wsr - sem_wsr), ymax = (mean_wsr + sem_wsr), fill =as.factor(paranoia_group)),alpha = 0.3, show.legend = F) +
  #geom_vline(xintercept = c(30), linetype = "longdash") + # bin = 71 => trials 71-80 (last bin before some post-reversal trials are averaged)
  # scale_color_manual(values = c("darkred", "darkgreen", "darkblue","darkorange","black"),
  #                    labels = c('control' = 'control (n=20)',
  #                               'md' = 'MD (n=3)',
  #                               'mofc' = 'MOFC (n=4)',
  #                               'ofc' = 'OFC (n=7; n=3[LOFC] + n=4[OFC])',
  #                               'vlpfc' = 'VLPFC (n=4)')) + 
  scale_color_manual(name = "Paranoia group",
                     values = c("#5B8FAF", "#772e25"),
                     labels = c('0' = 'non-clinical paranoia',
                                '1' = 'clinical paranoia')) +
  scale_fill_manual(values = c("black", "black")) +
  scale_x_continuous(breaks = c(1,10,20,30,40)) +
  #ggtitle("Win-switching in Stable schedule") + 
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              #legend.text = element_line(colour="black", size = 1.5),
                              legend.position = "none")



lsr_plot <- ggplot(data = stats) + geom_line(aes(x= bins, y = mean_lsr, 
                                                 color = as.factor(paranoia_group)#, 
                                                 #linetype = group
                                                 #size= 0.3
), size=2) +
  geom_ribbon(aes(x = bins, ymin = (mean_lsr - sem_lsr), ymax = (mean_lsr + sem_lsr), fill =as.factor(paranoia_group)),alpha = 0.3, show.legend = F) +
  #geom_vline(xintercept = c(30), linetype = "longdash") + # bin = 71 => trials 71-80 (last bin before some post-reversal trials are averaged)
  # scale_color_manual(values = c("darkred", "darkgreen", "darkblue","darkorange","black"),
  #                    labels = c('control' = 'control (n=20)',
  #                               'md' = 'MD (n=3)',
  #                               'mofc' = 'MOFC (n=4)',
  #                               'ofc' = 'OFC (n=7; n=3[LOFC] + n=4[OFC])',
  #                               'vlpfc' = 'VLPFC (n=4)')) + 
  scale_color_manual(name = "Paranoia group",
                     values = c("#5B8FAF", "#772e25"),
                     labels = c('0' = 'non-clinical paranoia',
                                '1' = 'clinical paranoia')) +
  scale_fill_manual(values = c("black", "black")) + 
  #ggtitle("Win-switching in Stable schedule") + 
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              #legend.text = element_line(colour="black", size = 1.5),
                              legend.position = "none")


















library(readxl)
library(openxlsx)
library(data.table)

folder_path1 <- "C:/Pandemic_2020/raw_pandemic_prl_data/all"

csv_files <- list.files(path = folder_path1, pattern = ".csv$", full.names = TRUE)

data_list <- list()  # Initialize an empty list to store the extracted data



for (file_csv in csv_files) {
  data_csv <- read.csv(file_csv, header = FALSE)
  data_list[[file_csv]] <- data_csv
}


header <- c("src_subject_id",
            "start_time",	
            "end_time",	
            "browser",
            "index",
            "first_half_probabilities",
            "second_half_probabilities",
            "deck_color",
            "deck_position",
            "deck_probabilities",
            "deck_contingencies",
            "response_color",
            "key_press",
            "response_position",
            "response_probability",
            "reward_type",
            "trial_type",
            "rt",
            "reward_tally",
            "exclusion_reason")  # Replace with your desired header names

for (file_csv in csv_files) {
  data_csv <- data_list[[file_csv]]
  colnames(data_csv) <- header
  data_list[[file_csv]] <- data_csv
}



bin_size = 20
matrix_df <- list()
response_color_timeseries <- list()
wsr_lsr_dat <- list()
wsr_binned <- c()
lsr_binned <- c()
wsr_timeseries_df <- list()
lsr_timeseries_df <- list()
timeseries_ma_df <- list()
timeseries_ma_combined_df <- list()
avg_timeseries <- list()
timeseries_wsr_ma_transposed <- list()
#reward_type_matrix_df <- list()

for (i in 1:length(data_list)){
  
  data_list[[i]]$response_color <- ifelse(data_list[[i]]$response_color == "blue" | data_list[[i]]$response_color == "green",1,
                                          ifelse(data_list[[i]]$response_color == "black" | data_list[[i]]$response_color == "orange",2,
                                                 ifelse(data_list[[i]]$response_color == "red" | data_list[[i]]$response_color == "pink" | data_list[[i]]$response_color == "purple",3,"")))
  data_list[[i]]$reward_type <- ifelse(data_list[[i]]$reward_type == "False" | data_list[[i]]$reward_type == "FALSE",0,
                                       ifelse(data_list[[i]]$reward_type == "True" | data_list[[i]]$reward_type == "TRUE",1,""))
  
  response_color_timeseries[[i]] <- data.frame(id = unique(data_list[[i]]$src_subject_id),
                                               t(c(data_list[[i]]$response_color)))
  
  for (t in 1:length(data_list[[i]]$index)){
    data_list[[i]]$win_switch[t] <- ifelse(data_list[[i]]$reward_type[t-1] == 1 && (data_list[[i]]$response_color[t] != data_list[[i]]$response_color[t-1]),1,0)
    data_list[[i]]$win_stay[t] <- ifelse(data_list[[i]]$reward_type[t-1] == 1 && (data_list[[i]]$response_color[t] == data_list[[i]]$response_color[t-1]),1,0)
    data_list[[i]]$lose_switch[t] <- ifelse(data_list[[i]]$reward_type[t-1] == 0 && (data_list[[i]]$response_color[t] != data_list[[i]]$response_color[t-1]),1,0)
    data_list[[i]]$lose_stay[t] <- ifelse(data_list[[i]]$reward_type[t-1] == 0 && (data_list[[i]]$response_color[t] == data_list[[i]]$response_color[t-1]),1,0)
  }
  
  win_switch_rate_stable <- sum(data_list[[i]]$win_switch, na.rm = TRUE)/(sum(data_list[[i]]$win_switch, na.rm = TRUE) + sum(data_list[[i]]$win_stay, na.rm = TRUE))
  lose_stay_rate_stable <- sum(data_list[[i]]$lose_stay, na.rm = TRUE)/(sum(data_list[[i]]$lose_stay, na.rm = TRUE) + sum(data_list[[i]]$lose_switch, na.rm = TRUE))
  
  win_switch_rate_stable_pre <- sum(data_list[[i]]$win_switch[1:80], na.rm = TRUE)/(sum(data_list[[i]]$win_switch[1:80], na.rm = TRUE) + sum(data_list[[i]]$win_stay[1:80], na.rm = TRUE))
  win_switch_rate_stable_post <- sum(data_list[[i]]$win_switch[81:160], na.rm = TRUE)/(sum(data_list[[i]]$win_switch[81:160], na.rm = TRUE) + sum(data_list[[i]]$win_stay[81:160], na.rm = TRUE))
  
  lose_stay_rate_stable_pre <- sum(data_list[[i]]$lose_stay[1:80], na.rm = TRUE)/(sum(data_list[[i]]$lose_stay[1:80], na.rm = TRUE) + sum(data_list[[i]]$lose_switch[1:80], na.rm = TRUE))
  lose_stay_rate_stable_post <- sum(data_list[[i]]$lose_stay[81:160], na.rm = TRUE)/(sum(data_list[[i]]$lose_stay[81:160], na.rm = TRUE) + sum(data_list[[i]]$lose_switch[81:160], na.rm = TRUE))
  
  win_stay_count <- sum(data_list[[i]]$win_stay, na.rm = TRUE)
  win_switch_count <- sum(data_list[[i]]$win_switch, na.rm = TRUE)
  
  lose_stay_count <- sum(data_list[[i]]$lose_stay, na.rm = TRUE)
  lose_switch_count <- sum(data_list[[i]]$lose_switch, na.rm = TRUE)
  
  
  win_stay_count_pre <- sum(data_list[[i]]$win_stay[1:80], na.rm = TRUE)
  win_switch_count_pre <- sum(data_list[[i]]$win_switch[1:80], na.rm = TRUE)
  
  lose_stay_count_pre <- sum(data_list[[i]]$lose_stay[1:80], na.rm = TRUE)
  lose_switch_count_pre <- sum(data_list[[i]]$lose_switch[1:80], na.rm = TRUE)
  
  win_stay_count_post <- sum(data_list[[i]]$win_stay[81:160], na.rm = TRUE)
  win_switch_count_post <- sum(data_list[[i]]$win_switch[81:160], na.rm = TRUE)
  
  lose_stay_count_post <- sum(data_list[[i]]$lose_stay[81:160], na.rm = TRUE)
  lose_switch_count_post <- sum(data_list[[i]]$lose_switch[81:160], na.rm = TRUE)
  
  wsr_lsr_dat[[i]] <- data.frame(id = unique(data_list[[i]]$src_subject_id),
                                 wsr_stable = win_switch_rate_stable,
                                 lsr_stable = lose_stay_rate_stable,
                                 wsr_stable_pre = win_switch_rate_stable_pre,
                                 wsr_stable_post = win_switch_rate_stable_post,
                                 lsr_stable_pre = lose_stay_rate_stable_pre,
                                 lsr_stable_post = lose_stay_rate_stable_post,
                                 wsc_stay = win_stay_count,
                                 wsc_stay_pre = win_stay_count_pre,
                                 wsc_stay_post = win_stay_count_post,
                                 wsc_switch = win_switch_count,
                                 wsc_switch_pre = win_switch_count_pre,
                                 wsc_switch_post = win_switch_count_post,
                                 # wsr_switch = win_switch_rate_stable,
                                 # wsr_switch_pre = win_switch_rate_stable_pre,
                                 # wsr_switch_post = win_switch_rate_stable_post,
                                 lsc_stay = lose_stay_count,
                                 lsc_stay_pre = lose_stay_count_pre,
                                 lsc_stay_post = lose_stay_count_post,
                                 lsc_switch = lose_switch_count,
                                 lsc_switch_pre = lose_switch_count_pre,
                                 lsc_switch_post = lose_switch_count_post)
  
  for (b in 1:(nrow(data_list[[i]]) - (bin_size-1))){
    
    wsr_binned[b] <- sum(data_list[[i]]$win_switch[b:(b+(bin_size-1))], na.rm = TRUE)/(sum(data_list[[i]]$win_switch[b:(b+(bin_size-1))], na.rm = TRUE) + sum(data_list[[i]]$win_stay[b:(b+(bin_size-1))], na.rm = TRUE))
    lsr_binned[b] <- sum(data_list[[i]]$lose_stay[b:(b+(bin_size-1))], na.rm = TRUE)/(sum(data_list[[i]]$lose_stay[b:(b+(bin_size-1))], na.rm = TRUE) + sum(data_list[[i]]$lose_switch[b:(b+(bin_size-1))], na.rm = TRUE))
    
    
  }
  
  timeseries_ma_df[[i]] = data.frame(id = unique(data_list[[i]]$src_subject_id),
                                     bins = 1:(160-(bin_size-1)),
                                     wsr_ma = wsr_binned,
                                     lsr_ma = lsr_binned)
  
  timeseries_wsr_ma_transposed[[i]] <- data.frame(id = unique(data_list[[i]]$src_subject_id),
                                                  wsr_moving_average = t(wsr_binned),
                                                  lsr_moving_average = t(lsr_binned))
  
  
  
}




rates_timeseries_df <- rbindlist(timeseries_ma_df)







dat <- read.csv("C:\\Users\\prave\\Documents\\Praveen\\research\\side_projects\\XAI_psychiatry\\data\\feature_dat.csv")
paranoia_dat <- dat[,c("id","paranoia_group_binary")]

rates_timeseries_merged <- merge(paranoia_dat,rates_timeseries_df)






library(plotrix)
library(dplyr)
stats <- rates_timeseries_merged %>%
  dplyr::group_by(paranoia_group_binary,bins) %>%
  dplyr::summarise(mean_wsr = mean(wsr_ma, na.rm = TRUE),
                   sem_wsr = std.error(wsr_ma, na.rm = TRUE),
                   mean_lsr = mean(lsr_ma, na.rm = TRUE),
                   sem_lsr = std.error(lsr_ma, na.rm = TRUE))

source("C:/Pandemic_2020/revisions/code/theme_publication.R") # source: https://rpubs.com/Koundy/71792

library(ggplot2)
wsr_plot <- ggplot(data = stats) + geom_line(aes(x= bins, y = mean_wsr, 
                                                 color = as.factor(paranoia_group_binary)#, 
                                                 #linetype = group
                                                 #size= 0.3
), size=2) +
  geom_ribbon(aes(x = bins, ymin = (mean_wsr - sem_wsr), ymax = (mean_wsr + sem_wsr), fill =as.factor(paranoia_group_binary)),alpha = 0.3, show.legend = F) +
  #geom_vline(xintercept = c(80), linetype = "longdash") + # bin = 71 => trials 71-80 (last bin before some post-reversal trials are averaged)
  # scale_color_manual(values = c("darkred", "darkgreen", "darkblue","darkorange","black"),
  #                    labels = c('control' = 'control (n=20)',
  #                               'md' = 'MD (n=3)',
  #                               'mofc' = 'MOFC (n=4)',
  #                               'ofc' = 'OFC (n=7; n=3[LOFC] + n=4[OFC])',
  #                               'vlpfc' = 'VLPFC (n=4)')) + 
  scale_color_manual(name = "Paranoia group",
                     values = c("#5B8FAF", "#772e25"),
                     labels = c('0' = 'non-clinical paranoia',
                                '1' = 'clinical paranoia')) +
  scale_fill_manual(values = c("black", "black")) + #ylim(0,0.5) + #xlim(61,101) + #ylim(0.05,0.1) #ylim(0.16,0.21) + #xlim(60,100) +
  scale_x_continuous(breaks = c(1,40,80,120,160)) +
  #ggtitle("Win-switching in Stable schedule") + 
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              #legend.text = element_line(colour="black", size = 1.5),
                              legend.position = "none")


# generate zoomed-in panel
ggplot(data = subset(stats, bins >=1 & bins <=40)) + geom_line(aes(x= bins, y = mean_wsr, 
                                                                   color = as.factor(paranoia_group_binary)#, 
                                                                   #linetype = group
                                                                   #size= 0.3
), size=2) +
  geom_ribbon(aes(x = bins, ymin = (mean_wsr - sem_wsr), ymax = (mean_wsr + sem_wsr), fill =as.factor(paranoia_group_binary)),alpha = 0.3, show.legend = F) +
  #geom_vline(xintercept = c(71), linetype = "longdash") + # bin = 71 => trials 71-80 (last bin before some post-reversal trials are averaged)
  # scale_color_manual(values = c("darkred", "darkgreen", "darkblue","darkorange","black"),
  #                    labels = c('control' = 'control (n=20)',
  #                               'md' = 'MD (n=3)',
  #                               'mofc' = 'MOFC (n=4)',
  #                               'ofc' = 'OFC (n=7; n=3[LOFC] + n=4[OFC])',
  #                               'vlpfc' = 'VLPFC (n=4)')) + 
  scale_color_manual(name = "Paranoia group",
                     values = c("#5B8FAF", "#772e25"),
                     labels = c('0' = 'non-clinical paranoia',
                                '1' = 'clinical paranoia')) +
  scale_fill_manual(values = c("black", "black")) +
  #scale_y_continuous(limits = c(0, 0.35), breaks = seq(from = 0, to = 0.45, by = 0.005))
#ggtitle("Win-switching in Stable schedule") + 
guides(colour = guide_legend(override.aes = list(size=3))) +
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              #legend.text = element_line(colour="black", size = 1.5),
                              legend.position = "none")


lsr_plot <- ggplot(data = stats) + geom_line(aes(x= bins, y = mean_lsr, 
                                                 color = as.factor(paranoia_group_binary)#, 
                                                 #linetype = group
                                                 #size= 0.3
), size=2) +
  geom_ribbon(aes(x = bins, ymin = (mean_lsr - sem_lsr), ymax = (mean_lsr + sem_lsr), fill =as.factor(paranoia_group_binary)),alpha = 0.3, show.legend = F) +
  #geom_vline(xintercept = 81, linetype = "longdash") +
  # scale_color_manual(values = c("darkred", "darkgreen", "darkblue","darkorange","black"),
  #                    labels = c('control' = 'control (n=20)',
  #                               'md' = 'MD (n=3)',
  #                               'mofc' = 'MOFC (n=4)',
  #                               'ofc' = 'OFC (n=7; n=3[LOFC] + n=4[OFC])',
  #                               'vlpfc' = 'VLPFC (n=4)')) + 
  scale_color_manual(name = "Paranoia group",
                     values = c("#5B8FAF", "#772e25"),
                     labels = c('0' = 'non-clinical paranoia',
                                '1' = 'clinical paranoia')) +
  scale_fill_manual(values = c("black", "black")) +
  scale_x_continuous(breaks = c(1,40,80,120,160)) +
  #ggtitle("Win-switching in Stable schedule") + 
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              #legend.text = element_line(colour="black", size = 1.5),
                              legend.position = "none")


# generate zoomed-in panel
ggplot(data = subset(stats, bins >=1 & bins <=40)) + geom_line(aes(x= bins, y = mean_lsr, 
                                                                   color = as.factor(paranoia_group_binary)#, 
                                                                   #linetype = group
                                                                   #size= 0.3
), size=2) +
  geom_ribbon(aes(x = bins, ymin = (mean_lsr - sem_lsr), ymax = (mean_lsr + sem_lsr), fill =as.factor(paranoia_group_binary)),alpha = 0.3, show.legend = F) +
  #geom_vline(xintercept = c(71), linetype = "longdash") + # bin = 71 => trials 71-80 (last bin before some post-reversal trials are averaged)
  # scale_color_manual(values = c("darkred", "darkgreen", "darkblue","darkorange","black"),
  #                    labels = c('control' = 'control (n=20)',
  #                               'md' = 'MD (n=3)',
  #                               'mofc' = 'MOFC (n=4)',
  #                               'ofc' = 'OFC (n=7; n=3[LOFC] + n=4[OFC])',
  #                               'vlpfc' = 'VLPFC (n=4)')) + 
  scale_color_manual(name = "Paranoia group",
                     values = c("#5B8FAF", "#772e25"),
                     labels = c('0' = 'non-clinical paranoia',
                                '1' = 'clinical paranoia')) +
  scale_fill_manual(values = c("black", "black")) + 
  #ggtitle("Win-switching in Stable schedule") + 
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              #legend.text = element_line(colour="black", size = 1.5),
                              legend.position = "none")