library(dplyr)

###### Using cole's model - OFC hgf estimates
monkey_model2_ofc_dat <- read.csv("C:\\rotation\\steve\\data\\belief\\monkeys_ofc_stable_sessions_Est1_M6.csv")
monkey_model2_ofc_dat <- monkey_model2_ofc_dat[,-c(3)]
monkey_model2_ofc_dat$group <- ifelse(monkey_model2_ofc_dat$group == "lesion","ofc_lesion","control")

###### Using cole's model - MDmc hgf estimates
monkey_model2_mdmc_dat <- read.csv("C:\\rotation\\steve\\data\\belief\\monkeys_mdmc_stable_sessions_Est1_M6.csv")
monkey_model2_mdmc_dat <- monkey_model2_mdmc_dat[-which(monkey_model2_mdmc_dat$time == "pre"),]
monkey_model2_mdmc_dat <- monkey_model2_mdmc_dat[,-c(3:4)]
monkey_model2_mdmc_dat$group <- ifelse(monkey_model2_mdmc_dat$group == "lesion","mdmc_lesion","control")


bind_ofc_mdmc_dat <- rbind(monkey_model2_ofc_dat,
                           monkey_model2_mdmc_dat)

dat_sessions_df <- bind_ofc_mdmc_dat %>%
  dplyr::group_by(monkey_id,group) %>%
  dplyr::summarise(avg_mu02_1 = mean(mu02_1, na.rm = TRUE),
                   avg_mu03_1 = mean(mu03_1, na.rm = TRUE),
                   avg_m3_1 = mean(m3_1, na.rm = TRUE),
                   avg_kappa2_1 = mean(kappa2_1, na.rm = TRUE),
                   avg_omega2_1 = mean(omega2_1, na.rm = TRUE),
                   avg_omega3_1 = mean(omega3_1, na.rm = TRUE),
                   avg_lme_1 = mean(LME_1, na.rm = TRUE),
                   avg_mu02_2 = mean(mu02_2, na.rm = TRUE),
                   avg_mu03_2 = mean(mu03_2, na.rm = TRUE),
                   avg_m3_2 = mean(m3_2, na.rm = TRUE),
                   avg_kappa2_2 = mean(kappa2_2, na.rm = TRUE),
                   avg_omega2_2 = mean(omega2_2, na.rm = TRUE),
                   avg_omega3_2 = mean(omega3_2, na.rm = TRUE),
                   avg_lme_2 = mean(LME_2, na.rm = TRUE))

dat_ofc <- subset(dat_sessions_df, group == "control" | group == "ofc_lesion")

dat_ofc_long_df <- dat_ofc %>%
  gather(key = "omega3_time", value = "value", avg_omega3_1,avg_omega3_2) %>%
  convert_as_factor(monkey_id,group)

dat_ofc_long_df$reversal <- rep(c("pre","post"), each=18)
dat_ofc_long_df$reversal <- factor(dat_ofc_long_df$reversal, levels = c("pre","post"))


dat_ofc_long_df1 <- dat_ofc_long_df[-which(dat_ofc_long_df$monkey_id == "slipre"),]


#Belief - OFC
ggplot(dat_ofc_long_df1, aes(reversal,value, fill=group)) +
  geom_boxplot()+
  
  # linetype parameter is used to customize the joining line
  #geom_line(aes(group = paired), linetype=2, size=0.5, position = position_dodge2(0.5))+
  
  # geom_point() is used to plot data points on boxplot
  geom_point(aes(fill=group),size=2,shape=21, position = position_dodge2(0.7)) + #facet_wrap(~lesion, labeller = labeller(lesion = c("control" = "control",
  # "lesion." = "LOFC lesion"))) +
  scale_fill_manual("Group",
                    labels = c('control' = 'control (n=14)',
                               'lesion' = 'OFC lesion (n=4)'),
                    values = c('gray','#F58231') #993232 #a74ac3 #999932 #f79b5a
  ) + labs(title = "") +
  ylab(expression(omega[3])) + scale_x_discrete(labels=c("pre" = "pre-reversal",
                                                         "post" = "post-reversal")) +
  theme_Publication() + theme(strip.background = element_rect(colour="white", fill="white"))




dat_mdmc <- subset(dat_sessions_df, group == "control" | group == "mdmc_lesion")

dat_mdmc_long_df <- dat_mdmc %>%
  gather(key = "omega3_time", value = "value", avg_omega3_1,avg_omega3_2) %>%
  convert_as_factor(monkey_id,group)

dat_mdmc_long_df$reversal <- rep(c("pre","post"), each=17)
dat_mdmc_long_df$reversal <- factor(dat_mdmc_long_df$reversal, levels = c("pre","post"))

#Belief - MDmc
ggplot(dat_mdmc_long_df, aes(reversal,value, fill=group)) +
  geom_boxplot()+
  
  # linetype parameter is used to customize the joining line
  #geom_line(aes(group = paired), linetype=2, size=0.5, position = position_dodge2(0.5))+
  
  # geom_point() is used to plot data points on boxplot
  geom_point(aes(fill=group),size=2,shape=21, position = position_dodge2(0.7)) + #facet_wrap(~lesion, labeller = labeller(lesion = c("control" = "control",
  # "lesion." = "LOFC lesion"))) +
  scale_fill_manual("Group",
                    labels = c('control' = 'control',
                               'lesion' = 'MDmc lesion'),
                    values = c('gray','#a74ac3') #993232 #a74ac3 #999932 #f79b5a
  ) + labs(title = "") +
  ylab(expression(omega[3])) + scale_x_discrete(labels=c("pre" = "pre-reversal",
                                                      "post" = "post-reversal")) +
  theme_Publication() + theme(strip.background = element_rect(colour="white", fill="white"))



library(rstatix)

View(dat_sessions_df)
dat_sessions_subset <- dat_sessions_df[,c(1:2,5,12)]
dat_ofc_md_long_df <- dat_sessions_subset %>%
  gather(key = "m3_time", value = "value", avg_m3_1,avg_m3_2) %>%
  convert_as_factor(monkey_id,group)

dat_ofc_md_long_df$time<- rep(c("pre","post"), each=21)
dat_ofc_md_long_df$time <- factor(dat_ofc_md_long_df$time, levels = c("pre","post"))


dat_ofc_md_long_df1 <- dat_ofc_md_long_df[-which(dat_ofc_md_long_df$monkey_id == "slipre"),]


bxp1 <- ggboxplot(
  dat_ofc_md_long_df1, x = "time", y = "value",
  color = "group", palette = "jco"
)
bxp1

dat_sessions_subset_w2 <- dat_sessions_df[,c(1:2,7,14)]
dat_ofc_md_long_w2_df <- dat_sessions_subset_w2 %>%
  gather(key = "omega2_time", value = "value", avg_omega2_1,avg_omega2_2) %>%
  convert_as_factor(monkey_id,group)

dat_ofc_md_long_w2_df$time<- rep(c("pre","post"), each=21)
dat_ofc_md_long_w2_df$time <- factor(dat_ofc_md_long_w2_df$time, levels = c("pre","post"))


dat_ofc_md_long_w2_df1 <- dat_ofc_md_long_w2_df[-which(dat_ofc_md_long_w2_df$monkey_id == "slipre"),]


bxp2 <- ggboxplot(
  dat_ofc_md_long_w2_df1, x = "time", y = "value",
  color = "group", palette = "jco"
)
bxp2




library(glmmTMB)

# regression model
#merged_dat$reversal <- as.factor(merged_dat$reversal)
#merged_dat$session_num <- as.numeric(merged_dat$session)
gaussian_monkeys_factorial <- glmmTMB(value ~ group*time + (1|monkey_id), 
                                      family = gaussian,
                                      data = dat_ofc_md_long_df1)
summary(gaussian_monkeys_factorial)
Anova(gaussian_monkeys_factorial,type = "III")
# three-way interaction; tendency over time or was it that certain sessions were diff from each other? <- motivation for looking at time as numeric 

# regression model with time as numeric
gaussian_monkeys_numeric <- glmmTMB(value ~ group*session_num*reversal + (1|monkey_id),family = gaussian,
                                    data = merged_dat)
summary(gaussian_monkeys_numeric)
Anova(gaussian_monkeys_numeric,type = "III")
# three-way interaction; there is a tendency over time, suggesting a change in relationship over group and reversal over time

# regression model with time as numeric - pre reversal
gaussian_monkeys_pre <- glmmTMB(value ~ group*session_num+ (1|monkey_id), family = gaussian,
                                data = subset(merged_dat, reversal == "pre"))
summary(gaussian_monkeys_pre)
Anova(gaussian_monkeys_pre,type = "III")
# two-way interaction between group and session before reversal, and a main effect of group

# regression model with time as numeric - post reversal
gaussian_monkeys_post <- glmmTMB(value ~ group*session_num+ (1|monkey_id), family = gaussian,
                                 data = subset(merged_dat, reversal == "post"))
summary(gaussian_monkeys_post)
Anova(gaussian_monkeys_post,type = "III")
# strong trend in the two-way interaction between group and session after reversal, and a main effect of group

# regression model with time as numeric - control
gaussian_monkeys_control <- glmmTMB(value ~ reversal*session_num+ (1|monkey_id),
                                    family = gaussian,
                                    data = subset(merged_dat, group == "control"))
summary(gaussian_monkeys_control)
Anova(gaussian_monkeys_control,type = "III")
# observed a main effect of reversal in the control monkeys

# binomial regression model with time as numeric - mdmc
gaussian_monkeys_mdmc <- glmmTMB(value ~ reversal*session_num+ (1|monkey_id),
                                 family = gaussian,
                                 data = subset(merged_dat, group == "lesion"))
summary(gaussian_monkeys_mdmc)
Anova(gaussian_monkeys_mdmc,type = "III")
# two-way interaction between reversal and time in the lesioned monkeys, and main effect of reversal
# A greater perception of task uncertainty in the lesioned monkeys across multiple sessions between pre-
# and post-reversal

source("C:/Pandemic_2020/revisions/code/theme_publication.R")

dat_ofc_md_long_df1$group <- factor(dat_ofc_md_long_df1$group,levels = c("control","mdmc_lesion","ofc_lesion"))

p_width <- 0.8  # Decreased width for more separation

ggplot(dat_ofc_md_long_df1, aes(time,value, fill=group)) +
  # Explicitly set group aesthetic and width for geom_boxplot
  geom_boxplot(aes(group=interaction(time, group)), width=p_width, 
               position=position_dodge(width=p_width + 0.1), size=1.2, alpha=0.8) +  # Increase dodge width slightly more than boxplot width
  
  # Use position_jitterdodge() for geom_point
  geom_point(aes(group=interaction(time, group)), 
             position=position_jitterdodge(jitter.width=0.5, dodge.width=p_width + 0.1), 
             size=2, shape=21) +
  scale_fill_manual("Group",
                    labels = c('control' = 'control',
                               'mdmc_lesion' = 'MDmc lesion',
                               'ofc_lesion' = 'OFC lesion'),
                    values = c('gray','#a74ac3','#F58231') #993232 #a74ac3 #999932 #f79b5a
  ) + 
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              legend.text = element_blank(),
                              legend.position = "none"
  )



dat_sessions_subset$avg_m3 <- rowMeans(dat_sessions_subset$avg_m3_1,dat_sessions_subset$avg_m3_2)

stats <-  dat_sessions_subset %>%
  dplyr::group_by(group) %>%
  dplyr::summarise(mean_wsr = mean(avg_m3_2,na.rm = TRUE),
                   sem_wsr = std.error(avg_m3_2,na.rm=TRUE))


subset_dat <- subset(prl_clean_na, phenotype == "hc" | phenotype == "chr")
t.test(subset_dat$wsr ~ subset_dat$phenotype,
       mu=0,
       alt="two.sided",
       conf=0.95,
       var.eq=F,
       paired=F)

stats$group <- factor(stats$group, levels = c("control","ofc_lesion","mdmc_lesion"))


dat_ofc_md_long_w2_df1$group <- factor(dat_ofc_md_long_w2_df1$group,levels = c("control","mdmc_lesion","ofc_lesion"))


ggplot(dat_ofc_md_long_w2_df1, aes(time,value, fill=group)) +
  # Explicitly set group aesthetic and width for geom_boxplot
  geom_boxplot(aes(group=interaction(time, group)), width=p_width, 
               position=position_dodge(width=p_width + 0.1), size=1.2, alpha=0.8) +  # Increase dodge width slightly more than boxplot width
  
  # Use position_jitterdodge() for geom_point
  geom_point(aes(group=interaction(time, group)), 
             position=position_jitterdodge(jitter.width=0.5, dodge.width=p_width + 0.1), 
             size=2, shape=21) +
  scale_fill_manual("Group",
                    labels = c('control' = 'control',
                               'mdmc_lesion' = 'MDmc lesion',
                               'ofc_lesion' = 'OFC lesion'),
                    values = c('gray','#a74ac3','#F58231') #993232 #a74ac3 #999932 #f79b5a
  ) + 
  guides(colour = guide_legend(override.aes = list(size=3))) + 
  theme_Publication() + theme(axis.title.y = element_blank(),
                              axis.title.x = element_blank(),
                              axis.text = element_blank(), 
                              axis.line = element_line(colour="black", size = 1.5),
                              axis.ticks = element_line(colour="black", size = 1.5),
                              #legend.text = element_blank(),
                              legend.position = "bottom",
                              legend.direction = "horizontal"
  )



### Bootstrapping
control_mdmc_dat <- subset(dat_sessions_df, group == "control" | group == "mdmc_lesion")


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
num_permutations <- 101

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
p_value <- sum(observed_statistic < permuted_statistics, na.rm = TRUE)/100

# Display the results
cat("Observed test statistic:", observed_statistic, "\n")
cat("p-value:", sprintf("%.100f", p_value), "\n")

