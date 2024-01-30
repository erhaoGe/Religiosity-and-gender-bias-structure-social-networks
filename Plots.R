
## ----------------------------------------------------------------------------------
library(tidyverse)
library(viridis)
library(ggpubr)
library(sna)
library(intergraph)
library(ergm)
library(magrittr)
library(texreg)
library(gtsummary)
library(ggeffects)
library(hrbrthemes)
library(broom)
library(AICcmodavg)
library(stargazer)

##### Figure 1
## ----------------------------------------------------------------------------------
### Proportion of male nominees
Edgelist_socialsupport %>% 
  distinct(Ego,Alter,Support) %>% 
  left_join(networkcapital2 %>% 
              select(ID,Gender) %>% 
              rename(Ego_Gender = Gender),
            by = c("Ego" = "ID")) %>% 
  left_join(networkcapital2 %>% 
              select(ID,Gender) %>% 
              rename(Alter_Gender = Gender),
            by = c("Alter" = "ID")) %>% 
  group_by(Ego,Alter_Gender) %>%    
  count(Ego_Gender, name = 'n_Alter') %>% 
  ungroup(Ego,Alter_Gender) %>%
  right_join(.,tidyr::expand(.,Ego,Alter_Gender)) %>% 
  select(-Ego_Gender) %>% 
  left_join(networkcapital2 %>% 
              select(ID,Gender) %>% 
              rename(Ego_Gender = Gender),
            by = c("Ego" = "ID")) %>% 
  replace_na(list(n_Alter = 0)) %>% 
  add_count(Ego,wt = n_Alter,name = 'n_Ego') %>% 
  group_by(Ego_Gender,Alter_Gender) %>%
  mutate(Ego_Gender = if_else(Ego_Gender =="Male",
                              "Male nominators (N = 147)",
                              "Female nominators (N = 140)")) %>% 
  ggplot(aes(x = Alter_Gender, y = n_Alter/n_Ego, 
             fill = Alter_Gender)) +
  geom_boxplot(alpha=.6) +
  stat_summary(fun.y = mean,
               color = "red", 
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE) +
  facet_wrap(vars(Ego_Gender),scales = "free") +
  scale_color_manual(values = c("#db7093", "#6ca6cd"))+
  scale_fill_manual(values = c("#db7093","#6ca6cd"))+
  xlab("Gender of nominees") +
  ylab("Proportion of nominees") +
  ggtitle("(a)") +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.line = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))


## male/female nominees receive how many proportion of male/female nominations
Edgelist_socialsupport %>% 
  distinct(Ego,Alter,Support) %>% 
  left_join(networkcapital2 %>% 
              select(ID,Gender) %>% 
              rename(Ego_Gender = Gender),
            by = c("Ego" = "ID")) %>% 
  left_join(networkcapital2 %>% 
              select(ID,Gender) %>% 
              rename(Alter_Gender = Gender),
            by = c("Alter" = "ID")) %>%
  group_by(Alter,Ego_Gender) %>% 
  count(Alter_Gender, name = 'n_Ego') %>% 
  ungroup(Alter,Ego_Gender) %>% 
  right_join(.,tidyr::expand(.,Alter,Ego_Gender)) %>% 
  select(-Alter_Gender) %>% 
  left_join(networkcapital2 %>% 
              select(ID,Gender) %>% 
              rename(Alter_Gender = Gender),
            by = c("Alter" = "ID")) %>% 
  replace_na(list(n_Ego = 0)) %>% 
  add_count(Alter,wt = n_Ego,name = 'n_Alter') %>% 
  group_by(Alter_Gender,Ego_Gender) %>% 
  mutate(Alter_Gender = if_else(Alter_Gender =="Male",
                                "Male nominees (N = 142)",
                                "Female nominees (N = 130)")) %>% 
  ggplot(aes(x = Ego_Gender, 
             y = n_Ego/n_Alter, 
             fill = Ego_Gender)) +
  geom_boxplot(alpha=.6) +
  stat_summary(fun.y = mean,
               color = "red", 
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE) +
  facet_wrap(vars(Alter_Gender),scales = "free") +
  scale_color_manual(values = c("#db7093", "#6ca6cd"))+
  scale_fill_manual(values = c("#db7093","#6ca6cd"))+
  xlab("Gender of nominators") +
  ylab("Proportion of nominators") +
  ggtitle("(b)") +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.line = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))


##### Figure 3
## (a) ----------------------------------------------------------------------------------
  ml <- list()
  ml[[1]] <- glm.nb( All ~ Age_cohort + Gender +
                       Rank1 + Siblinginvillage + 
                       Offspringinvillage,
                     data = Personal_network)
  
  ml[[2]] <- glm.nb( All ~ Age_cohort + Gender +
                       Rank1 + Siblinginvillage + 
                       Offspringinvillage + 
                       Fiveyearscore2 +
                       DailyRegularscore2,
                     data = Personal_network)
  
  ml[[3]] <- glm.nb( All ~ Age_cohort + Gender +
                       Rank1 + Siblinginvillage + 
                       Offspringinvillage +
                       Fiveyearscore2+
                       Gender*DailyRegularscore2,
                     data = Personal_network)
  
  ml[[4]] <- glm.nb( All ~ Age_cohort + Gender +
                       Rank1 + Siblinginvillage + 
                       Offspringinvillage +
                       Fiveyearscore2*Gender +
                       DailyRegularscore2,
                     data = Personal_network)

raw <- ggpredict(ml[[4]],
                 terms = c("Fiveyearscore2","Gender")) %>% 
  attr(., "rawdata")

std_dev <-  18.54487
mean_value <- 12.02431 


original_values <- c(0,50,100,150)
z_scores <- (original_values - mean_value) / std_dev

ggeffect(ml[[4]],
         terms = c("Fiveyearscore2 [-1:8,by = 0.1]","Gender")) %>% 
  plot(ci.style = "ribbon",
       limit.range = T) +
  scale_x_continuous(
    trans = scales::trans_new(
      "z_score_adjusted",
      transform = function(x) (x * std_dev) + mean_value,
      inverse = function(x) (x - mean_value)/std_dev),
    breaks = z_scores,  
    labels = original_values) + 
  
  scale_y_continuous(breaks = c(0,25,50,75),  
                     labels = c(0,25,50,75))+
  geom_point(aes(x = x,
                 y = response, 
                 color = group,
                 fill = group ),data = raw) +
  
  scale_color_manual(values = c( "#db7093","#6ca6cd"))+
  scale_fill_manual(values = c( "#db7093","#6ca6cd"))+
  
  xlab("Pilgrimage score") +
  ylab("Predicted In-degree value") + 
  theme(
    title = element_blank(),
    legend.position="bottom",
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)) +
  theme(panel.grid.minor = element_blank())

ggsave("Figure_3(a).tiff",width = 8, height = 6, dpi = 300)

# (b) ------------------------------------------------

ggeffects::ggeffect(ml[[3]],
                    terms = c("DailyRegularscore2","Gender")) %>%
  plot(ci.style = "errorbar",
       dodge = 0) +
  geom_line() +
  scale_color_manual(values = c( "#db7093","#6ca6cd"))+
  scale_fill_manual(values = c( "#db7093","#6ca6cd"))+
  xlab("Daily practice") +
  ylab("Predicted In-degree value") +
  theme(
    title = element_blank(),
    legend.position="bottom",
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5))

ggsave("Figure_3(b).tiff",width = 8, height = 6, dpi = 300)

##### Figure S1
## ----------------------------------------------------------------------------------
# Pilgrimage
networkcapital %>% 
  mutate(Gender =  factor(Gender,
                          levels = c("Female","Male"))) %>% 
  ggplot(aes(x=Gender , 
             y=Fiveyearscore, fill=Gender)) +
  geom_boxplot( alpha=.6) +
  scale_color_manual(values = c("#db7093", "#6ca6cd"))+
  scale_fill_manual(values = c("#db7093","#6ca6cd"))+
  xlab("Gender") +
  ylab("Pilgrimage score") +
  stat_summary(fun.y = mean,
               color = "red", 
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE) +
  stat_compare_means(aes(group = Gender),
                     method = "wilcox.test",
                     label = "p.format",
                     label.x = 1.4,
                     label.y = 150,
                     size = 3) +
  theme(
    legend.position = 'None',
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14,hjust = 0.5),
    axis.title.y = element_text(size = 14,hjust = 0.5),
    strip.text = element_text(size = 14,hjust = 0.5),
    strip.background = element_rect(colour = "black",size = .1))


## ----------------------------------------------------------------------------------
networkcapital2 %>% 
  ggplot(aes(x = factor(DailyRegularscore2),
             group = factor(Gender))) + 
  geom_bar(aes(y = ..prop..,fill = Gender),
           stat="count",
           width = 0.6, 
           position = position_dodge(width = 0.8)) +
  facet_wrap(~ Gender) +
  scale_x_discrete(labels = c("No","Yes")) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c( "#db7093","#6ca6cd")) +
  geom_text(aes(label = scales::percent(..prop..),
                y= ..prop.. ), 
            stat= "count", 
            hjust = .5,
            vjust=1,
            size = 3,
            position = position_dodge(0.9)) +
  xlab("Daily practices") +
  ylab("Proportion") +
  theme(
    legend.position = 'None',
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14,hjust = 0.5),
    axis.title.y = element_text(size = 14,hjust = 0.5),
    strip.text = element_text(size = 14,hjust = 0.5),
    strip.background = element_rect(colour = "black",size = .1))


##### Figure S7
## ----------------------------------------------------------------------------------
networkcapital2 %>% 
  rowwise() %>% 
  mutate(All = sum(c_across(Emotionally:Suggestion))) %>% 
  ungroup() %>% 
  pivot_longer(c(Emotionally:Suggestion,All),
               names_to = "Type") %>% 
  mutate(Type = case_when(
    Type == "Emotionally" ~  "Emotional",
    Type == "Financially" ~  "Financial",
    Type == "Guaranty"    ~  "Guarantee",
    Type == "Physically"  ~  "Behavioural",
    Type == "Suggestion"  ~   "Guidance",
    TRUE ~ as.character(Type))) %>% 
  group_by(Type) %>%
  mutate(max_value = max(value, na.rm = TRUE)) %>% 
  ungroup(Type) %>% 
  ggplot(aes(x=Gender, y = value, 
             fill=Gender)) +
  geom_boxplot(alpha=.6) +
  facet_wrap(vars(Type),scales = "free") +
  stat_summary(fun.y = mean,
               color = "red", 
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE) +
  stat_compare_means(aes(group = Gender),
                     method = "wilcox.test",
                     label = "p.format",
                     label.x = 1.3,
                     label.y = -2,
                     size = 2) +
  scale_color_manual(values = c("#db7093", "#6ca6cd"))+
  scale_fill_manual(values = c("#db7093","#6ca6cd"))+
  xlab("Gender") +
  ylab("In-degree value") +
  theme(
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.line = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15))

##### Figure S8
## ----------------------------------------------------------------------------------
networkcapital %>% 
  pivot_longer(c(Emotionally:Suggestion),
               names_to = "Type") %>% 
  mutate(Type = case_when(
    Type == "Emotionally" ~  "Emotional",
    Type == "Financially" ~  "Financial",
    Type == "Guaranty"    ~  "Guarantee",
    Type == "Physically"  ~  "Behavioural",
    Type == "Suggestion"  ~   "Guidance",
    TRUE ~ as.character(Type))) %>% 
  mutate(Gender = as.factor(Gender)) %>% 
  mutate(Gender =  factor(Gender,
                          levels = c("Male","Female"))) %>%
  ggplot(aes(x = Fiveyearscore,
             y = value, 
             color = Gender,
             fill = Gender ))+
  geom_point()+
  scale_color_manual(values = c( "#6ca6cd","#db7093"))+
  scale_fill_manual(values = c( "#6ca6cd","#db7093"))+
  geom_smooth(colour ="black",method = "loess",span =0.7,
              alpha = 0.2) +
  facet_wrap(vars(Type),scales = "free") +
  stat_cor(aes(color = Gender), 
           cor.coef.name = "rho",
           method = "spearman", 
           label.x = c(0,75), 
           label.y = -2,
           size = 1.5) +
  xlab("Pilgrimage score") +
  ylab("In-degree value") +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14,hjust = 0.5),
    axis.title.y = element_text(size = 14,hjust = 0.5),
    strip.text = element_text(size = 10,hjust = 0.5),
    strip.background = element_rect(colour = "black",size = .1))

##### Figure S9
## ----------------------------------------------------------------------------------
networkcapital2 %>% 
  pivot_longer(c(Emotionally:Suggestion),
               names_to = "Type") %>% 
  mutate(Type = case_when(
    Type == "Emotionally" ~  "Emotional",
    Type == "Financially" ~  "Financial",
    Type == "Guaranty"    ~  "Guarantee",
    Type == "Physically"  ~  "Behavioural",
    Type == "Suggestion"  ~   "Guidance",
    TRUE ~ as.character(Type))) %>% 
  mutate(Gender = as.factor(Gender)) %>% 
  mutate(Gender =  factor(Gender,levels = c("Male","Female"))) %>%
  ggplot( aes(x=as.factor(DailyRegularscore2), 
              y=value)) +
  geom_boxplot() +
  ggh4x::facet_grid2(Gender ~ Type, scales = "free_y", independent = "y")+
  scale_x_discrete(labels = c("No","Yes")) +
  xlab("Daily practice") +
  ylab("In-degree value") +
  stat_summary(fun.y = mean,
               color = "red", 
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE) +
  stat_compare_means(aes(group = DailyRegularscore2),
                     method = "wilcox.test",
                     label = "p.format",
                     label.x = 1.3,
                     label.y = -0.9,
                     size = 2) +
  
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14,hjust = 0.5),
    axis.title.y = element_text(size = 14,hjust = 0.5),
    strip.text = element_text(size = 10,hjust = 0.5),
    strip.background = element_rect(colour = "black",size = .1))

##### Figure S10
## ----------------------------------------------------------------------------------
networkcapital %>% 
  rowwise() %>% 
  mutate(All = sum(c_across(Emotionally:Suggestion))) %>% 
  ungroup() %>%
  mutate(Gender = as.factor(Gender)) %>% 
  mutate(Gender =  factor(Gender,
                          levels = c("Male","Female"))) %>%
  mutate(Fiveyearscore_log = log(Fiveyearscore + 1))  %>%
  ggplot(aes(x = Fiveyearscore_log,
             y = All, 
             color = Gender,
             fill = Gender ))+
  geom_point()+
  scale_color_manual(values = c( "#6ca6cd","#db7093"))+
  scale_fill_manual(values = c( "#6ca6cd","#db7093"))+
  xlab("Log-transformed pilgrimage score") +
  ylab("In-degree value") +
  theme(
    legend.position="bottom",
    axis.title.x = element_text(hjust = 0.5),
    axis.title.y = element_text(hjust = 0.5)) +
stat_cor(aes(color = Gender), 
         cor.coef.name = "rho",
         method = "spearman", 
         label.x = c(0,3), 
         label.y = 86,
         size = 3,
         show.legend = F) 

ggsave("Figure_S10(a).tiff",width = 8, height = 6, dpi = 300)

## ----------------------------------------------------------------------------------
networkcapital2 %>% 
  rowwise() %>% 
  mutate(All = sum(c_across(Emotionally:Suggestion))) %>% 
  ungroup() %>%
  mutate(Gender = as.factor(Gender)) %>% 
  mutate(Gender =  factor(Gender,levels = c("Male","Female"))) %>%
  ggplot( aes(x= as.factor(DailyRegularscore2), 
              y=All)) +
  geom_boxplot() +
  scale_x_discrete(labels = c("No","Yes")) +
  facet_wrap(. ~ Gender,  scales = "free" ) +
  xlab("Daily practice") +
  ylab("In-degree value") +
  stat_summary(fun.y = mean,
               color = "red", 
               geom = "point", shape = 18, size = 2,
               show.legend = FALSE) +
  stat_compare_means(aes(group = DailyRegularscore2),
                     method = "wilcox.test",
                     label = "p.format",
                     label.x = 1.4,
                     label.y = -0.9,
                     size = 3) +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14,hjust = 0.5),
    axis.title.y = element_text(size = 14,hjust = 0.5),
    strip.text = element_text(size = 14,hjust = 0.5),
    strip.background = element_rect(colour = "black",size = .1))

ggsave("Figure_S10(b).tiff",width = 8, height = 6, dpi = 300)