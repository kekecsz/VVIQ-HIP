
#### Packages

library(psych)
library(tidyverse) # for tidy coding and ggplot
library(boot)
library(ltm) # for cronbach.alpha


#### Custom functions
# bootstrap CI for correlation

boot_r = function(var1, var2){
  data_for_boot = data.frame(var1,var2)
  
  boot_r <- boot(data_for_boot, 
             statistic = function(data, i) {
               cor(data[i, "var1"], data[i, "var2"], method='pearson')
             },
             R = 10000
  )
  return(boot_r)
}


#### Load data

data_raw = read.csv("https://raw.githubusercontent.com/kekecsz/VVIQ-HIP/main/Data_VVIQ_HIP.csv")

#### Data wrangling

data_processed = data_raw %>% 
  rowwise() %>%
  mutate(HGSHS_observer_total = sum(c(HGSHS_observer_01, HGSHS_observer_02, HGSHS_observer_03, HGSHS_observer_04, HGSHS_observer_05, 
                            HGSHS_observer_06, HGSHS_observer_07, HGSHS_observer_08, HGSHS_observer_09, HGSHS_observer_10, 
                            HGSHS_observer_11, HGSHS_observer_12)),
         HGSHS_subject_total = sum(c(HGSHS_subject_01, HGSHS_subject_02, HGSHS_subject_03, HGSHS_subject_04, HGSHS_subject_05, 
                                      HGSHS_subject_06, HGSHS_subject_07, HGSHS_subject_08, HGSHS_subject_09, HGSHS_subject_10, 
                                      HGSHS_subject_11, HGSHS_subject_12)),
         VVIQ_total = sum(c(VVIQ1, VVIQ2, VVIQ3, VVIQ4, VVIQ5, VVIQ6, VVIQ7, VVIQ8, VVIQ9,
                            VVIQ10, VVIQ11, VVIQ12, VVIQ13, VVIQ14, VVIQ15, VVIQ16))
  ) %>% 
  ungroup()

#### Data for analysis

data_for_analysis_with_outliers = data_processed %>% 
  drop_na(VVIQ_total)

### drop two outliers
### there were only two people with below 30 VVIQ scores
### they seem out of distribution for correlation between VVIQ and hypnotizability
 data_for_analysis = data_processed %>% 
  drop_na(VVIQ_total) %>% 
  filter(VVIQ_total >30)

#### Analyse research questions

### Demographics and descriptives, and normality checks
 
table(data_for_analysis$gender)

### Internal consistency
## Chronbach's alpha


# HGSHS
HGSHS_subject_items = as.data.frame(cbind(data_for_analysis$HGSHS_subject_01, data_for_analysis$HGSHS_subject_02, data_for_analysis$HGSHS_subject_03, 
data_for_analysis$HGSHS_subject_04, data_for_analysis$HGSHS_subject_05, data_for_analysis$HGSHS_subject_06, 
data_for_analysis$HGSHS_subject_07, data_for_analysis$HGSHS_subject_08, data_for_analysis$HGSHS_subject_09, 
data_for_analysis$HGSHS_subject_10, data_for_analysis$HGSHS_subject_11, data_for_analysis$HGSHS_subject_12))
cronbach.alpha(HGSHS_subject_items, CI=TRUE, standardized=TRUE)

# VVIQ
VVIQ_items = as.data.frame(cbind(data_for_analysis$VVIQ1, data_for_analysis$VVIQ2, data_for_analysis$VVIQ3, data_for_analysis$VVIQ4, 
      data_for_analysis$VVIQ5, data_for_analysis$VVIQ6, data_for_analysis$VVIQ7, data_for_analysis$VVIQ8, 
      data_for_analysis$VVIQ9, data_for_analysis$VVIQ10, data_for_analysis$VVIQ11, data_for_analysis$VVIQ12, 
      data_for_analysis$VVIQ13, data_for_analysis$VVIQ14, data_for_analysis$VVIQ15, data_for_analysis$VVIQ16))
cronbach.alpha(VVIQ_items, CI=TRUE, standardized=TRUE)


# Imagery - Main dimension
Imagery_main = data.frame(PCI12_rev = 6-data_for_analysis$PCI12, PCI44 = data_for_analysis$PCI44, PCI18_rev = 6-data_for_analysis$PCI18, PCI48 = data_for_analysis$PCI48)
cronbach.alpha(Imagery_main, CI=TRUE, standardized=TRUE)

# Imagery amount
Imagery_amount = data.frame(PCI12_rev = 6-data_for_analysis$PCI12, PCI44 = data_for_analysis$PCI44)
cronbach.alpha(Imagery_amount, CI=TRUE, standardized=TRUE)

# Imagery vividness
Imagery_vividness = data.frame(PCI18_rev = 6-data_for_analysis$PCI18, PCI48 = data_for_analysis$PCI48)
cronbach.alpha(Imagery_vividness, CI=TRUE, standardized=TRUE)


### Assumption test for Pearson correlation

## VVIQ

data_for_analysis %>% 
  summarize(mean = mean(VVIQ_total),
            sd = sd(VVIQ_total))

describe(data_for_analysis$VVIQ_total)[,c("skew", "kurtosis")]
data_for_analysis %>% 
  ggplot() +
  aes(x = VVIQ_total) +
  geom_histogram()
data_for_analysis %>% 
  ggplot() +
  aes(sample = VVIQ_total) +
  stat_qq() +
  stat_qq_line()

## HGSHS


data_for_analysis %>% 
  summarize(mean = mean(HGSHS_subject_total),
            sd = sd(HGSHS_subject_total))

describe(data_for_analysis$HGSHS_subject_total)[,c("skew", "kurtosis")]
data_for_analysis %>% 
  ggplot() +
  aes(x = HGSHS_subject_total) +
  geom_histogram()
data_for_analysis %>% 
  ggplot() +
  aes(sample = HGSHS_subject_total) +
  stat_qq() +
  stat_qq_line()

## PCI imagery variables

data_for_analysis %>% 
  summarize(mean = mean(PCI_Main_05_Imagery), 
            sd = sd(PCI_Main_05_Imagery))

data_for_analysis %>% 
  summarize(mean = mean(PCI_Sub_13_Amount), 
            sd = sd(PCI_Sub_13_Amount))

data_for_analysis %>% 
  summarize(mean = mean(PCI_Sub_14_Vividness), 
            sd = sd(PCI_Sub_14_Vividness))

describe(data_for_analysis$PCI_Main_05_Imagery)[,c("skew", "kurtosis")]
data_for_analysis %>% 
  ggplot() +
  aes(x = PCI_Main_05_Imagery) +
  geom_histogram()
data_for_analysis %>% 
  ggplot() +
  aes(sample = PCI_Main_05_Imagery) +
  stat_qq() +
  stat_qq_line()


describe(data_for_analysis$PCI_Sub_13_Amount)[,c("skew", "kurtosis")]
data_for_analysis %>% 
  ggplot() +
  aes(x = PCI_Sub_13_Amount) +
  geom_histogram()
data_for_analysis %>% 
  ggplot() +
  aes(sample = PCI_Sub_13_Amount) +
  stat_qq() +
  stat_qq_line()


describe(data_for_analysis$PCI_Sub_14_Vividness)[,c("skew", "kurtosis")]
data_for_analysis %>% 
  ggplot() +
  aes(x = PCI_Sub_14_Vividness) +
  geom_histogram()
data_for_analysis %>% 
  ggplot() +
  aes(sample = PCI_Sub_14_Vividness) +
  stat_qq() +
  stat_qq_line()

## Assumption of normality seems to be violated in many instances, so we will use Spearman correlation instead.

### H1: There is a positive correlation between VVIQ and hypnotizability

## Testing H1 without outliers
cor.test(data_for_analysis$HGSHS_subject_total, data_for_analysis$VVIQ_total, alternative = "greater")

data_for_analysis %>% 
ggplot()+
  aes(x = HGSHS_subject_total, y = VVIQ_total)+
  geom_jitter()+
  geom_smooth(method = "lm")+
  scale_y_continuous(breaks = seq(0,80,10))+
  scale_x_continuous(breaks = c(0:12))+
  theme_bw()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  labs(
    x = "HGSHS total score",
    y = "VVIQ total score"
  )

### Gender differences in correlation

data_for_analysis %>% 
  filter(gender == "male") %>% 
  select(HGSHS_subject_total, VVIQ_total) %>% 
  cor()

data_for_analysis %>% 
  filter(gender == "female") %>% 
  select(HGSHS_subject_total, VVIQ_total) %>% 
  cor()

## Testing H1 with outliers included
set.seed(1)
boot_model = boot_r(var1 = data_for_analysis_with_outliers$HGSHS_subject_total, var2 = data_for_analysis_with_outliers$VVIQ_total)
boot_model$t0
boot.ci(boot_model, type = c("bca"), conf = 0.90) 

data_for_analysis_with_outliers %>% 
  ggplot()+
  aes(x = HGSHS_subject_total, y = VVIQ_total)+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_continuous(breaks = seq(0,80,10))+
  scale_x_continuous(breaks = c(0:12))+
  theme_bw()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))+
  labs(
    x = "HGSHS total score",
    y = "VVIQ total score"
  )


### Q2: There is a positive correlation between VVIQ and the Imagery factors of PCI: Imagery Main Dimension, Imagery amount subdimension, and Imagery Vividness subdimension.


set.seed(1)
boot_model = boot_r(var1 = data_for_analysis_with_outliers$PCI_Main_05_Imagery, var2 = data_for_analysis_with_outliers$VVIQ_total)
boot_model$t0
boot.ci(boot_model, type = c("bca"), conf = 0.95) 

data_for_analysis %>% 
  ggplot()+
  aes(x = VVIQ_total, y = PCI_Main_05_Imagery)+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_continuous(breaks = c(0:12))+
  scale_x_continuous(breaks = seq(0,80,10))+
  theme_bw()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))

set.seed(1)
boot_model = boot_r(var1 = data_for_analysis_with_outliers$PCI_Sub_13_Amount, var2 = data_for_analysis_with_outliers$VVIQ_total)
boot_model$t0
boot.ci(boot_model, type = c("bca"), conf = 0.95) 

data_for_analysis %>% 
  ggplot()+
  aes(x = VVIQ_total, y = PCI_Sub_13_Amount)+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_continuous(breaks = c(0:12))+
  scale_x_continuous(breaks = seq(0,80,10))+
  theme_bw()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))

set.seed(1)
boot_model = boot_r(var1 = data_for_analysis_with_outliers$PCI_Sub_14_Vividness, var2 = data_for_analysis_with_outliers$VVIQ_total)
boot_model$t0
boot.ci(boot_model, type = c("bca"), conf = 0.95) 

data_for_analysis %>% 
  ggplot()+
  aes(x = VVIQ_total, y = PCI_Sub_14_Vividness)+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_continuous(breaks = c(0:12))+
  scale_x_continuous(breaks = seq(0,80,10))+
  theme_bw()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))

### Q3: There is a positive correlation between hypnotizability and Imagery factors of PCI: Imagery Main Dimension, Imagery amount subdimension, and Imagery Vividness subdimension.


set.seed(1)
boot_model = boot_r(var1 = data_for_analysis_with_outliers$PCI_Main_05_Imagery, var2 = data_for_analysis_with_outliers$HGSHS_subject_total)
boot_model$t0
boot.ci(boot_model, type = c("bca"), conf = 0.95) 

data_for_analysis %>% 
  ggplot()+
  aes(x = HGSHS_subject_total, y = PCI_Main_05_Imagery)+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_continuous(breaks = c(0:12))+
  scale_x_continuous(breaks = seq(0,12,1))+
  theme_bw()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))

set.seed(1)
boot_model = boot_r(var1 = data_for_analysis_with_outliers$PCI_Sub_13_Amount, var2 = data_for_analysis_with_outliers$HGSHS_subject_total)
boot_model$t0
boot.ci(boot_model, type = c("bca"), conf = 0.95) 

data_for_analysis %>% 
  ggplot()+
  aes(x = HGSHS_subject_total, y = PCI_Sub_13_Amount)+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_continuous(breaks = c(0:12))+
  scale_x_continuous(breaks = seq(0,12,1))+
  theme_bw()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))

set.seed(1)
boot_model = boot_r(var1 = data_for_analysis_with_outliers$PCI_Sub_14_Vividness, var2 = data_for_analysis_with_outliers$HGSHS_subject_total)
boot_model$t0
boot.ci(boot_model, type = c("bca"), conf = 0.95) 

data_for_analysis %>% 
  ggplot()+
  aes(x = HGSHS_subject_total, y = PCI_Sub_14_Vividness)+
  geom_point()+
  geom_smooth(method = "lm")+
  scale_y_continuous(breaks = c(0:12))+
  scale_x_continuous(breaks = seq(0,12,1))+
  theme_bw()+
  theme(axis.text=element_text(size=20), axis.title=element_text(size=20,face="bold"))

data_for_analysis


# write.csv(data_processed, paste0(data_directory, "Data_processed.csv"), row.names = F)

