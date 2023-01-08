## Code to accompany ``Religiosity and gender bias structure social networks in a Tibetan population'' 
## Includes code for: 
## (1) The exponential graph models predicting supportive relationships
## (2) The Calculation of cohesiveness of the networks of kin of celibate monks
## (3) The Bayesian models predicting ego network characteristics


##########################################
library(sna)
library(ergm)
library(tidyverse)
library(texreg)
library(gtsummary)

##########################################
##########################################
## Table 1  ##
##########################################
##########################################
socialsupport_network <- igraph::graph.data.frame(d= Edgelist_socialsupport,
                                                  vertices = Suheri_demo_repu_reli %>% 
                                                    filter(Invillagenow == 1 & !Age_cohort == "0-15") %>%
                                                    filter(!is.na(Totalscore) & !is.na(DailyRegularscore)),
                                                  directed = T) %>% 
  intergraph::asNetwork() 

Full_model_scale_Monk2_1_4 <- ergm(socialsupport_network ~ edges  + 
                                     nodeicov("Age")+
                                     nodeicov("Relatives")+
                                     nodeifactor("Gender")+
                                     nodeifactor("Rank1")+
                                     nodeofactor("Rank1")+
                                     nodematch("WhichTribe")+
                                     nodematch("Gender")+
                                     edgecov(kinship_matrix, attr = "r")+
                                     edgecov(Kinship_affinal_all_matrix,attrname ="Affinal_r_rank")+
                                     edgecov(gps_matrix, attr = "distance"),
                                   control = control.ergm(seed=10,MCMC.burnin=1000,
                                                          MCMC.samplesize=10000,MCMC.interval=1000),
                                   verbose = F)

Full_model_scale_Monk2_1_2 <- ergm(socialsupport_network ~ edges  + 
                                     nodeicov("Age")+
                                     nodeicov("ScaleFiveyearscore")+
                                     nodeicov("DailyRegularscore")+
                                     nodeicov("Relatives")+
                                     nodeifactor("Gender")+
                                     nodeifactor("Rank1")+
                                     nodeofactor("Rank1")+
                                     nodeicov("mean_r_monk")+
                                     nodematch("WhichTribe")+
                                     nodematch("Gender")+
                                     edgecov(kinship_matrix, attr = "r")+
                                     edgecov(gps_matrix, attr = "distance")+
                                     edgecov(Kinship_affinal_all_matrix,attrname ="Affinal_r_rank"),
                                   control = control.ergm(seed=10,MCMC.burnin=1000,
                                                          MCMC.samplesize=10000,MCMC.interval=1000),
                                   verbose = F)

Full_model_scale_Monk2_1 <- ergm(socialsupport_network ~ edges  + 
                                   nodeicov("Age")+
                                   nodeicov("ScaleFiveyearscore")+
                                   nodeicov("DailyRegularscore")+
                                   nodeicov("Relatives")+
                                   nodeifactor("Gender")+
                                   nodeifactor("Rank1")+
                                   nodeofactor("Rank1")+
                                   nodeicov("mean_r_monk")+
                                   nodematch("WhichTribe")+
                                   nodematch("Gender")+
                                   edgecov(kinship_matrix, attr = "r")+
                                   edgecov(gps_matrix, attr = "distance")+
                                   edgecov(Kinship_affinal_all_matrix,attrname ="Affinal_r_rank")+
                                   mutual("Gender", diff = T),
                                 control = control.ergm(seed=10,MCMC.burnin=1000,
                                                        MCMC.samplesize=10000,MCMC.interval=1000),
                                 verbose = F)

tbl_merge(map(list(Full_model_scale_Monk2_1_4,
                   Full_model_scale_Monk2_1_2,
                   Full_model_scale_Monk2_1), ~ tbl_regression(.x)))

##########################################
##########################################
## Table 2  ##
##########################################
##########################################
#####
# Five supportive networks
##### 

Emotionally_support_network <- get.inducedSubgraph(socialsupport_network,
                                                   eid = which( socialsupport_network %e% "Support" =="Emotionally")) 

Financially_support_network <- get.inducedSubgraph(socialsupport_network,
                                                   eid = which( socialsupport_network %e% "Support" =="Financially")) 

Guaranty_support_network    <- get.inducedSubgraph(socialsupport_network,
                                                   eid = which( socialsupport_network %e% "Support" =="Guaranty")) 

Physically_support_network  <- get.inducedSubgraph(socialsupport_network,
                                                   eid = which( socialsupport_network %e% "Support" =="Physically")) 

Suggestion_support_network  <- get.inducedSubgraph(socialsupport_network,
                                                   eid = which( socialsupport_network %e% "Support" =="Suggestion")) 

###########
Separate_network <- list( Emotionally_support_network,
                          
                          Financially_support_network, 
                          
                          Guaranty_support_network,   
                          
                          Physically_support_network,  
                          
                          Suggestion_support_network )
# network summary
map(Separate_network,~ network.size(.))
map(Separate_network,~ network.edgecount(.))

# model summary
Separate_model_monks_r <- map( Separate_network, ~ ergm(. ~ edges  + 
                                                          nodeicov("Age")+
                                                          nodeicov("ScaleFiveyearscore")+
                                                          nodeicov("DailyRegularscore")+
                                                          nodeicov("Relatives")+
                                                          nodeifactor("Gender")+
                                                          nodeifactor("Rank1")+
                                                          nodeofactor("Rank1")+
                                                          nodematch("WhichTribe")+
                                                          nodematch("Gender")+
                                                          edgecov(kinship_matrix, attr = "r")+
                                                          edgecov(gps_matrix, attr = "distance")+
                                                          edgecov(Kinship_affinal_all_matrix,attrname ="Affinal_r_rank")+
                                                          nodeicov("mean_r_monk")+
                                                          mutual("Gender", diff = T),
                                                        control = control.ergm(seed=10,MCMC.burnin=1000,
                                                                               MCMC.samplesize=10000,MCMC.interval=1000),
                                                        verbose = F))


# output the results
screenreg(Separate_model_monks_r)


##########################################
##########################################
## Table 3  ##
##########################################
##########################################
### All kin of monks
library(igraph)
socialsupport_network <- intergraph::asIgraph(socialsupport_network) 
socialsupport_network_monk <- delete.vertices(socialsupport_network,
                                              V(socialsupport_network)[get.vertex.attribute(socialsupport_network,name="sum_r_monk")==0])
graph.density(socialsupport_network)
transitivity(socialsupport_network)
reciprocity(socialsupport_network) 

denTibetan  <-rep(0,10000)
transTibetan<-rep(0,10000)
recipTibetan<-rep(0,10000)

for (i in 1:10000){
  denTibetan  [i] = graph.density(induced.subgraph(socialsupport_network,vids=sample(1:288,55)))
  transTibetan[i] = transitivity(induced.subgraph(socialsupport_network,vids=sample(1:288,55)))
  recipTibetan[i] = reciprocity(induced.subgraph(socialsupport_network,vids=sample(1:288,55)))
}

TibetanSim<-cbind(denTibetan,transTibetan,recipTibetan)
TibetanSim<-as.data.frame(TibetanSim)

1-ecdf(TibetanSim$denTibetan)(graph.density(socialsupport_network_monk))
1-ecdf(TibetanSim$transTibetan)(transitivity(socialsupport_network_monk))
1-ecdf(TibetanSim$recipTibetan)(reciprocity(socialsupport_network_monk))


### Male kin of monks
socialsupport_network_male <- delete.vertices(socialsupport_network,
                                              V(socialsupport_network)[get.vertex.attribute(socialsupport_network,

                                                                                                                                                                                       name="Gender")=="Female"])
socialsupport_network_male_monk<- delete.vertices(socialsupport_network_male,
                                                  V(socialsupport_network_male)[get.vertex.attribute(socialsupport_network_male,
                                                                                                     name="sum_r_monk")==0])
for (i in 1:10000){
  denTibetan  [i] = graph.density(induced.subgraph(socialsupport_network_male,vids=sample(1:147,35)))
  transTibetan[i] = transitivity(induced.subgraph(socialsupport_network_male,vids=sample(1:147,35)))
  recipTibetan[i] = reciprocity(induced.subgraph(socialsupport_network_male,vids=sample(1:147,35)))
}

TibetanSim<-cbind(denTibetan,transTibetan,recipTibetan)
TibetanSim<-as.data.frame(TibetanSim)

1-ecdf(TibetanSim$denTibetan)(graph.density(socialsupport_network_male_monk))
1-ecdf(TibetanSim$transTibetan)(transitivity(socialsupport_network_male_monk))
1-ecdf(TibetanSim$recipTibetan)(reciprocity(socialsupport_network_male_monk))


### Female kin of monks

socialsupport_network_female<- delete.vertices(socialsupport_network,
                                               V(socialsupport_network)[get.vertex.attribute(socialsupport_network,
                                                                                             name="Gender")=="Male"])


socialsupport_network_female_monk<- delete.vertices(socialsupport_network_female,
                                                    V(socialsupport_network_female)[get.vertex.attribute(socialsupport_network_female,
                                                                                                         name="sum_r_monk")==0])

for (i in 1:10000){
  denTibetan  [i] = graph.density(induced.subgraph(socialsupport_network_female,vids=sample(1:142,20)))
  transTibetan[i] = transitivity(induced.subgraph(socialsupport_network_female,vids=sample(1:142,25)))
  # The significance changed when the value exceed 20.
  recipTibetan[i] = reciprocity(induced.subgraph(socialsupport_network_female,vids=sample(1:142,20)))
}

TibetanSim<-cbind(denTibetan,transTibetan,recipTibetan)
TibetanSim<-as.data.frame(TibetanSim)

1-ecdf(TibetanSim$denTibetan)(graph.density(socialsupport_network_female_monk))
1-ecdf(TibetanSim$transTibetan)(transitivity(socialsupport_network_female_monk))
1-ecdf(TibetanSim$recipTibetan)(reciprocity(socialsupport_network_female_monk))

##########################################
##########################################
## Figure 2 ##
##########################################
##########################################
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(patchwork)

(Plot1_1 <- networkcapital2 %>% 
    mutate(Gender = as.factor(Gender)) %>% 
    mutate(Gender =  factor(Gender,levels = c("Male","Female"))) %>% 
    ggplot(aes(x = Fiveyearscore,
               y = WPRrescaled, 
               color = Gender,
               fill = Gender ))+
    geom_point()+
    scale_color_manual(values = c( "#6ca6cd","#db7093"))+
    scale_fill_manual(values = c( "#6ca6cd","#db7093"))+
    geom_smooth(colour ="black",method = "loess",span =0.7,size = 1,
                alpha = 0.2) +
    xlab("Pilgrimage score") +
    ylab("Weighted page rank score") +
    ggtitle("(a)")+
    theme_ipsum() +
    theme(
      legend.position="bottom",
      axis.title.x = element_text(hjust = 0.5),
      axis.title.y = element_text(hjust = 0.5)))
ggsave("Plot1_1_gender_sociocentric3.tiff", 
       units="in", width=11, height=7, dpi=300, compression = 'lzw')  

(Plot1_2 <-
    networkcapital2 %>% 
    mutate(Gender = as.factor(Gender)) %>% 
    mutate(Gender =  factor(Gender,levels = c("Male","Female"))) %>%
    mutate(DailyRegularscore2 = factor(DailyRegularscore2,level = c(0,1))) %>% 
    ggplot( aes(x=DailyRegularscore2, y=WPRrescaled)) +
    geom_boxplot() +
    scale_x_discrete(labels = c("No","Yes"))+
    geom_jitter(aes(x=DailyRegularscore2, y=WPRrescaled),
                color="black", size=0.5, alpha=0.9) +
    stat_summary(fun.y = mean,
                 color = "red", position = position_dodge(0.75),
                 geom = "point", shape = 18, size = 3,
                 show.legend = FALSE) +
    facet_grid(. ~ Gender ) +
    xlab("Daily practice") +
    ylab("Weighted page rank score") + 
    ggtitle("(b)")+
    theme_ipsum() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14,hjust = 0.5),
      axis.title.y = element_text(size = 14,hjust = 0.5),
      strip.text = element_text(size = 14,hjust = 0.5),
      strip.background = element_rect(colour = "black",size = .1)))

ggsave("Plot1_2_gender_sociocentric3.tiff", 
       units="in", width=11, height=7, dpi=300, compression = 'lzw')  

(Plot1_3 <-
    networkcapital2 %>% 
    mutate(Gender = as.factor(Gender)) %>% 
    mutate(Gender =  factor(Gender,levels = c("Male","Female"))) %>%
    mutate(Kin_monk = if_else(mean_r > 0,"Yes","No")) %>% 
    mutate(Kin_monk = factor(Kin_monk, levels = c("No","Yes"))) %>% 
    ggplot(aes(x = Kin_monk,
               y = WPRrescaled))+
    geom_boxplot() +
    scale_x_discrete(labels = c("No","Yes"))+
    stat_summary(fun.y = mean,
                 color = "red", position = position_dodge(0.75),
                 geom = "point", shape = 18, size = 3,
                 show.legend = FALSE) +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    facet_grid(. ~ Gender ) +
    theme_ipsum() +
    xlab("Consanguineous kin of monks") +
    ylab("Weighted page rank score") +
    ggtitle("(c)")+
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14,hjust = 0.5),
      axis.title.y = element_text(size = 14,hjust = 0.5),
      strip.text = element_text(size = 14,hjust = 0.5),
      strip.background = element_rect(colour = "black",size = .1)))

ggsave("Plot1_3_gender_sociocentric3.tiff", 
       units="in", width=11, height=7, dpi=300, compression = 'lzw')  

Plot1_1 | (Plot1_2 / Plot1_3) +
  plot_annotation(tag_levels = "A")
ggsave("Plot1_gender_sociocentric3.tiff", 
       units="in", width=11, height=7, dpi=300, compression = 'lzw')  



##########################################
##########################################
## Figure 3 ##
##########################################
##########################################
library(rstan)
require(rethinking)

Reciprocity_Ego_data_whole_for_model3$Age <- (Reciprocity_Ego_data_whole_for_model2$Age-
                                                mean(Reciprocity_Ego_data_whole_for_model2$Age))/sd(Reciprocity_Ego_data_whole_for_model2$Age)
Reciprocity_Ego_data_whole_for_model3$Age2 <- Reciprocity_Ego_data_whole_for_model2$Age^2
Reciprocity_Ego_data_whole_for_model3$Age2 <- (Reciprocity_Ego_data_whole_for_model3$Age2-
                                                 mean(Reciprocity_Ego_data_whole_for_model3$Age2))/sd(Reciprocity_Ego_data_whole_for_model3$Age2)

Reciprocity_Ego_data_whole_for_model3$is_male<-Reciprocity_Ego_data_whole_for_model2$Gender=="Male"

Reciprocity_Ego_data_whole_for_model3$is_M<-Reciprocity_Ego_data_whole_for_model2$Rank1=="M"

Reciprocity_Ego_data_whole_for_model3$is_H<-Reciprocity_Ego_data_whole_for_model2$Rank1=="H"

Reciprocity_Ego_data_whole_for_model3$Siblinginvillage <- (Reciprocity_Ego_data_whole_for_model2$Siblinginvillage-
                                                             mean(Reciprocity_Ego_data_whole_for_model2$Siblinginvillage,na.rm = T))/sd(Reciprocity_Ego_data_whole_for_model2$Siblinginvillage,na.rm = T)
Reciprocity_Ego_data_whole_for_model3$Offspringinvillage <- (Reciprocity_Ego_data_whole_for_model2$Offspringinvillage-
                                                               mean(Reciprocity_Ego_data_whole_for_model2$Offspringinvillage,na.rm = T))/sd(Reciprocity_Ego_data_whole_for_model2$Offspringinvillage,na.rm = T)
Reciprocity_Ego_data_whole_for_model3$Fiveyearscore <- (Reciprocity_Ego_data_whole_for_model2$Fiveyearscore-
                                                          mean(Reciprocity_Ego_data_whole_for_model2$Fiveyearscore))/sd(Reciprocity_Ego_data_whole_for_model2$Fiveyearscore)
Reciprocity_Ego_data_whole_for_model3$mean_r <- (Reciprocity_Ego_data_whole_for_model2$mean_r-
                                                   mean(Reciprocity_Ego_data_whole_for_model2$mean_r))/sd(Reciprocity_Ego_data_whole_for_model2$mean_r)
Reciprocity_Ego_data_whole_for_model3$is_Daily <- Reciprocity_Ego_data_whole_for_model2$DailyRegularscore2=="1"

Reciprocity_Ego_data_whole_for_model3$Size <- Ego_Alter_data %>% count(Ego_ID) %$% n

Reciprocity_Ego_data_whole_for_model3$Reciprocity_tie <- Reciprocity_Ego_data_whole_for_model2$Reciprocity *
  (Ego_Alter_data %>% count(Ego_ID) %$% n)

Reciprocity_Ego_data_whole_for_model3$possibleties <- (Reciprocity_Ego_data_whole_for_model2$network_size*
                                                         (Reciprocity_Ego_data_whole_for_model2$network_size-1))

Reciprocity_Ego_data_whole_for_model3$density_tie <- Reciprocity_Ego_data_whole_for_model2$density*(Reciprocity_Ego_data_whole_for_model2$network_size*
                                                                                                      (Reciprocity_Ego_data_whole_for_model2$network_size-1))

Reciprocity_Ego_data_whole_for_model3$density_tie <- as.numeric(Reciprocity_Ego_data_whole_for_model3$density_tie)

Reciprocity_Ego_data_whole_for_model3$density <- Reciprocity_Ego_data_whole_for_model2$density 

Reciprocity_Ego_data_whole_for_model3$density <- as.numeric(Reciprocity_Ego_data_whole_for_model3$density)

Reciprocity_Ego_data_whole_for_model3 <- 
  Reciprocity_Ego_data_whole_for_model3 %>% 
  select(Age,Age2,is_male,is_M,is_H,Siblinginvillage,Offspringinvillage,
         Fiveyearscore,mean_r,is_Daily,Size,Reciprocity_tie,
         possibleties,density_tie,density)

Reciprocity_Ego_data_whole_for_model3_male <- Reciprocity_Ego_data_whole_for_model3 %>% 
  filter(is_male == T)

Reciprocity_Ego_data_whole_for_model3_female <- Reciprocity_Ego_data_whole_for_model3 %>% 
  filter(is_male == F)

############################################################
## Binomial model predicting proportion of reciprocity
Reciprosity_model <-map2stan(
  alist(
    Reciprocity_tie ~ dbinom(Size, p),
    logit(p) <- alpha + 
      beta_age*Age + 
      beta_age2*Age2 +
      beta_male*is_male + 
      beta_eco_M*is_M +
      beta_eco_H*is_H +
      beta_sibling*Siblinginvillage + 
      beta_offspring*Offspringinvillage + 
      beta_pilgrimage*Fiveyearscore + 
      beta_monk*mean_r + 
      beta_Daily*is_Daily,
    alpha ~ dnorm(0, 1),
    beta_age ~ dnorm(0, 1),
    beta_age2 ~ dnorm(0, 1),
    beta_male ~ dnorm(0, 1),
    beta_eco_M ~ dnorm(0, 1),
    beta_eco_H ~ dnorm(0, 1),
    beta_sibling   ~ dnorm(0, 1),
    beta_offspring ~ dnorm(0, 1),
    beta_pilgrimage~ dnorm(0, 1),
    beta_monk      ~ dnorm(0, 1),
    beta_Daily     ~ dnorm(0, 1),
    Siblinginvillage ~ dnorm(mu_N,sigma_N),
    mu_N ~ dnorm(0,1),
    sigma_N ~ dcauchy(0,1)),
  data=Reciprocity_Ego_data_whole_for_model3, 
  chains=4, iter=2000, warmup=1000, 
  control=list(max_treedepth=20))

Reciprosity_male_model <-map2stan(
  alist(
    Reciprocity_tie ~ dbinom(Size, p),
    logit(p) <- alpha + 
      beta_age*Age + 
      beta_age2*Age2 +
      beta_eco_M*is_M +
      beta_eco_H*is_H +
      beta_sibling*Siblinginvillage + 
      beta_offspring*Offspringinvillage + 
      beta_pilgrimage*Fiveyearscore + 
      beta_monk*mean_r + 
      beta_Daily*is_Daily,
    alpha ~ dnorm(0, 1),
    beta_age ~ dnorm(0, 1),
    beta_age2 ~ dnorm(0, 1),
    beta_eco_M ~ dnorm(0, 1),
    beta_eco_H ~ dnorm(0, 1),
    beta_sibling   ~ dnorm(0, 1),
    beta_offspring ~ dnorm(0, 1),
    beta_pilgrimage~ dnorm(0, 1),
    beta_monk      ~ dnorm(0, 1),
    beta_Daily     ~ dnorm(0, 1),
    Siblinginvillage ~ dnorm(mu_N,sigma_N),
    mu_N ~ dnorm(0,1),
    sigma_N ~ dcauchy(0,1)),
  data=Reciprocity_Ego_data_whole_for_model3_male, 
  chains=4, iter=2000, warmup=1000, 
  control=list(max_treedepth=20))

summary(Reciprosity_male_model)


Reciprosity_female_model <-map2stan(
  alist(
    Reciprocity_tie ~ dbinom(Size, p),
    logit(p) <- alpha + 
      beta_age*Age + 
      beta_age2*Age2 +
      beta_eco_M*is_M +
      beta_eco_H*is_H +
      beta_sibling*Siblinginvillage + 
      beta_offspring*Offspringinvillage + 
      beta_pilgrimage*Fiveyearscore + 
      beta_monk*mean_r + 
      beta_Daily*is_Daily,
    alpha ~ dnorm(0, 1),
    beta_age ~ dnorm(0, 1),
    beta_age2 ~ dnorm(0, 1),
    beta_eco_M ~ dnorm(0, 1),
    beta_eco_H ~ dnorm(0, 1),
    beta_sibling   ~ dnorm(0, 1),
    beta_offspring ~ dnorm(0, 1),
    beta_pilgrimage~ dnorm(0, 1),
    beta_monk      ~ dnorm(0, 1),
    beta_Daily     ~ dnorm(0, 1),
    Siblinginvillage ~ dnorm(mu_N,sigma_N),
    mu_N ~ dnorm(0,1),
    sigma_N ~ dcauchy(0,1)),
  data=Reciprocity_Ego_data_whole_for_model3_female, 
  chains=4, iter=2000, warmup=1000, 
  control=list(max_treedepth=20))

summary(Reciprosity_female_model)

############################################################
## Poisson model predicting ties between alters (density)

Density_model2 <- map2stan(
  alist(
    density_tie ~ dpois(lambda),
    log(lambda) <- alpha + 
      beta_age*Age + 
      beta_age2*Age2 +
      beta_male*is_male + 
      beta_eco_M*is_M +
      beta_eco_H*is_H +
      beta_sibling*Siblinginvillage + 
      beta_offspring*Offspringinvillage + 
      beta_pilgrimage*Fiveyearscore + 
      beta_monk*mean_r + 
      beta_Daily*is_Daily,
    alpha ~ dnorm(0, 10),
    beta_age ~ dnorm(0, 1),
    beta_age2 ~ dnorm(0, 1),
    beta_male ~ dnorm(0, 1),
    beta_eco_M ~ dnorm(0, 1),
    beta_eco_H ~ dnorm(0, 1),
    beta_sibling   ~ dnorm(0, 1),
    beta_offspring ~ dnorm(0, 1),
    beta_pilgrimage~ dnorm(0, 1),
    beta_monk      ~ dnorm(0, 1),
    beta_Daily     ~ dnorm(0, 1),
    Siblinginvillage ~ dnorm(mu_N,sigma_N),
    mu_N ~ dnorm(0,1),
    sigma_N ~ dcauchy(0,1)),
  data=Reciprocity_Ego_data_whole_for_model3, 
  chains=4, iter=2000, warmup=1000, 
  control=list(max_treedepth=20))

summary(Density_model2)

Density_male_model2 <-map2stan(
  alist(
    density_tie ~ dpois(lambda),
    log(lambda) <- alpha +  
      beta_age*Age + 
      beta_age2*Age2 +
      beta_eco_M*is_M +
      beta_eco_H*is_H +
      beta_sibling*Siblinginvillage + 
      beta_offspring*Offspringinvillage + 
      beta_pilgrimage*Fiveyearscore + 
      beta_monk*mean_r + 
      beta_Daily*is_Daily,
    alpha ~ dnorm(0, 10),
    beta_age ~ dnorm(0, 1),
    beta_age2 ~ dnorm(0, 1),
    beta_eco_M ~ dnorm(0, 1),
    beta_eco_H ~ dnorm(0, 1),
    beta_sibling   ~ dnorm(0, 1),
    beta_offspring ~ dnorm(0, 1),
    beta_pilgrimage~ dnorm(0, 1),
    beta_monk      ~ dnorm(0, 1),
    beta_Daily     ~ dnorm(0, 1),
    Siblinginvillage ~ dnorm(mu_N,sigma_N),
    mu_N ~ dnorm(0,1),
    sigma_N ~ dcauchy(0,1)),
  data=Reciprocity_Ego_data_whole_for_model3_male, 
  chains=4, iter=2000, warmup=1000, 
  control=list(max_treedepth=20))

summary(Density_male_model2)

Density_female_model2 <-map2stan(
  alist(
    density_tie ~ dpois(lambda),
    log(lambda) <- alpha + 
      beta_age*Age + 
      beta_age2*Age2 +
      beta_eco_M*is_M +
      beta_eco_H*is_H +
      beta_sibling*Siblinginvillage + 
      beta_offspring*Offspringinvillage + 
      beta_pilgrimage*Fiveyearscore + 
      beta_monk*mean_r + 
      beta_Daily*is_Daily,
    alpha ~ dnorm(0, 10),
    beta_age ~ dnorm(0, 1),
    beta_age2 ~ dnorm(0, 1),
    beta_eco_M ~ dnorm(0, 1),
    beta_eco_H ~ dnorm(0, 1),
    beta_sibling   ~ dnorm(0, 1),
    beta_offspring ~ dnorm(0, 1),
    beta_pilgrimage~ dnorm(0, 1),
    beta_monk      ~ dnorm(0, 1),
    beta_Daily     ~ dnorm(0, 1),
    Siblinginvillage ~ dnorm(mu_N,sigma_N),
    mu_N ~ dnorm(0,1),
    sigma_N ~ dcauchy(0,1)),
  data=Reciprocity_Ego_data_whole_for_model3_female, 
  chains=4, iter=2000, warmup=1000, 
  control=list(max_treedepth=20))

summary(Density_female_model2)
############################################################
## Linear model predicting network size
Size_model <-map2stan(
  alist(
    Size ~ dnorm(mu, sigma),
    mu <- alpha + 
      beta_age*Age + 
      beta_age2*Age2 +
      beta_male*is_male + 
      beta_eco_M*is_M +
      beta_eco_H*is_H +
      beta_sibling*Siblinginvillage + 
      beta_offspring*Offspringinvillage + 
      beta_pilgrimage*Fiveyearscore + 
      beta_monk*mean_r + 
      beta_Daily*is_Daily,
    alpha ~ dnorm(10, 4),
    beta_age ~ dnorm(0, 1),
    beta_age2 ~ dnorm(0, 1),
    beta_male ~ dnorm(0, 1),
    beta_eco_M ~ dnorm(0, 1),
    beta_eco_H ~ dnorm(0, 1),
    beta_sibling   ~ dnorm(0, 1),
    beta_offspring ~ dnorm(0, 1),
    beta_pilgrimage~ dnorm(0, 1),
    beta_monk      ~ dnorm(0, 1),
    beta_Daily     ~ dnorm(0, 1),
    sigma ~ dcauchy(0,2.5),
    Siblinginvillage ~ dnorm(mu_N,sigma_N),
    mu_N ~ dnorm(0,1),
    sigma_N ~ dcauchy(0,1)),
  data=Reciprocity_Ego_data_whole_for_model3, 
  chains=4, iter=2000, warmup=1000, 
  control=list(max_treedepth=20))

summary(Size_model)

Size_male_model <-map2stan(
  alist(
    Size ~ dnorm(mu, sigma),
    mu <- alpha + 
      beta_age*Age + 
      beta_age2*Age2 +
      beta_eco_M*is_M +
      beta_eco_H*is_H +
      beta_sibling*Siblinginvillage + 
      beta_offspring*Offspringinvillage + 
      beta_pilgrimage*Fiveyearscore + 
      beta_monk*mean_r + 
      beta_Daily*is_Daily,
    alpha ~ dnorm(10, 4),
    beta_age ~ dnorm(0, 1),
    beta_age2 ~ dnorm(0, 1),
    beta_eco_M ~ dnorm(0, 1),
    beta_eco_H ~ dnorm(0, 1),
    beta_sibling   ~ dnorm(0, 1),
    beta_offspring ~ dnorm(0, 1),
    beta_pilgrimage~ dnorm(0, 1),
    beta_monk      ~ dnorm(0, 1),
    beta_Daily     ~ dnorm(0, 1),
    sigma ~ dcauchy(0,2.5),
    Siblinginvillage ~ dnorm(mu_N,sigma_N),
    mu_N ~ dnorm(0,1),
    sigma_N ~ dcauchy(0,1)),
  data=Reciprocity_Ego_data_whole_for_model3_male, 
  chains=4, iter=2000, warmup=1000, 
  control=list(max_treedepth=20))

summary(Size_male_model)

Size_female_model <-map2stan(
  alist(
    Size ~ dnorm(mu, sigma),
    mu <- alpha + 
      beta_age*Age + 
      beta_age2*Age2 +
      beta_eco_M*is_M +
      beta_eco_H*is_H +
      beta_sibling*Siblinginvillage + 
      beta_offspring*Offspringinvillage + 
      beta_pilgrimage*Fiveyearscore + 
      beta_monk*mean_r + 
      beta_Daily*is_Daily,
    alpha ~ dnorm(10, 4),
    beta_age ~ dnorm(0, 1),
    beta_age2 ~ dnorm(0, 1),
    beta_eco_M ~ dnorm(0, 1),
    beta_eco_H ~ dnorm(0, 1),
    beta_sibling   ~ dnorm(0, 1),
    beta_offspring ~ dnorm(0, 1),
    beta_pilgrimage~ dnorm(0, 1),
    beta_monk      ~ dnorm(0, 1),
    beta_Daily     ~ dnorm(0, 1),
    sigma ~ dcauchy(0,2.5),
    Siblinginvillage ~ dnorm(mu_N,sigma_N),
    mu_N ~ dnorm(0,1),
    sigma_N ~ dcauchy(0,1)),
  data=Reciprocity_Ego_data_whole_for_model3_female, 
  chains=4, iter=2000, warmup=1000, 
  control=list(max_treedepth=20))

summary(Size_female_model)

#
precis(Reciprosity_male_model,prob=0.95,digits=3)
precis(Reciprosity_female_model,prob=0.95,digits=3)

#
precis(Size_male_model,prob=0.95,digits=3)
precis(Size_female_model,prob=0.95,digits=3)

#
precis(Density_male_model2,prob=0.95,digits=3)
precis(Density_female_model2,prob=0.95,digits=3) %>%  
  as.tibble(rownames = "Variable")

Out_Reciprocity_model_rethinkings <- 
  bind_rows(
    precis(Reciprosity_male_model,prob=0.95,digits=3) %>%
      as_tibble(rownames = "Variable")  %>% 
      rename(posterior_mean = mean,posterior_sd = sd) %>% 
      select(1:5) %>% 
      slice(1:10) %>% 
      mutate(Variable = c("Intercept","Age","Age sq",  
                          "Economic Rank (Middle)",   
                          "Economic Rank (High)", 
                          "Number of siblings",     
                          "Number of offsprings",  
                          "Pilgrimage Score", "Relatedness to monks",
                          "Daily Practice (Yes)")) %>% 
      mutate(Gender = "Male"),
    precis(Reciprosity_female_model,prob=0.95,digits=3) %>%
      as_tibble(rownames = "Variable")  %>% 
      rename(posterior_mean = mean,posterior_sd = sd) %>% 
      select(1:5) %>% 
      slice(1:10) %>% 
      mutate(Variable = c("Intercept","Age","Age sq",  
                          "Economic Rank (Middle)",   
                          "Economic Rank (High)", 
                          "Number of siblings",     
                          "Number of offsprings",  
                          "Pilgrimage Score", "Relatedness to monks",
                          "Daily Practice (Yes)")) %>% 
      mutate(Gender = "Female"))


Out_Size_model_rethinkings <- 
  bind_rows(
    precis(Size_male_model,prob=0.95,digits=3) %>%
      as_tibble(rownames = "Variable")  %>% 
      rename(posterior_mean = mean,posterior_sd = sd) %>% 
      select(1:5) %>% 
      slice(1:10) %>% 
      mutate(Variable = c("Intercept","Age","Age sq",  
                          "Economic Rank (Middle)",   
                          "Economic Rank (High)", 
                          "Number of siblings",     
                          "Number of offsprings",  
                          "Pilgrimage Score", "Relatedness to monks",
                          "Daily Practice (Yes)")) %>% 
      mutate(Gender = "Male"),
    precis(Size_female_model,prob=0.95,digits=3) %>%
      as_tibble(rownames = "Variable")  %>% 
      rename(posterior_mean = mean,posterior_sd = sd) %>% 
      select(1:5) %>% 
      slice(1:10) %>% 
      mutate(Variable = c("Intercept","Age","Age sq",  
                          "Economic Rank (Middle)",   
                          "Economic Rank (High)", 
                          "Number of siblings",     
                          "Number of offsprings",  
                          "Pilgrimage Score", "Relatedness to monks",
                          "Daily Practice (Yes)")) %>% 
      mutate(Gender = "Female"))


Out_Density_model_rethinkings <- 
  bind_rows(
    precis(Density_male_model2,prob=0.95,digits=3) %>%
      as_tibble(rownames = "Variable")  %>% 
      rename(posterior_mean = mean,posterior_sd = sd) %>% 
      select(1:5) %>% 
      slice(1:10) %>% 
      mutate(Variable = c("Intercept","Age","Age sq",  
                          "Economic Rank (Middle)",   
                          "Economic Rank (High)", 
                          "Number of siblings",     
                          "Number of offsprings",  
                          "Pilgrimage Score", "Relatedness to monks",
                          "Daily Practice (Yes)")) %>% 
      mutate(Gender = "Male"),
    precis(Density_female_model2,prob=0.95,digits=3) %>%
      as_tibble(rownames = "Variable")  %>% 
      rename(posterior_mean = mean,posterior_sd = sd) %>% 
      select(1:5) %>% 
      slice(1:10) %>% 
      mutate(Variable = c("Intercept","Age","Age sq",  
                          "Economic Rank (Middle)",   
                          "Economic Rank (High)", 
                          "Number of siblings",     
                          "Number of offsprings",  
                          "Pilgrimage Score", "Relatedness to monks",
                          "Daily Practice (Yes)")) %>% 
      mutate(Gender = "Female"))

Out_Reciprocity_model_rethinkings %>% 
  write.table(.,"Out_Reciprocity_model_rethinkings.csv",
              sep = ",",
              row.names = F)

Out_Size_model_rethinkings %>% 
  write.table(.,"Out_Size_model_rethinkings.csv",
              sep = ",",
              row.names = F)

Out_Density_model_rethinkings %>% 
  write.table(.,"Out_Density_model_rethinkings.csv",
              sep = ",",
              row.names = F)

(Plot_Ego_Size_rethinkings_model2 <- Out_Size_model_rethinkings %>% 
   filter(Variable %in% c("Pilgrimage Score" ,"Daily Practice (Yes)" ,"Relatedness to monks" ) ) %>% 
   ggplot(aes(x = Variable, y = posterior_mean, colour = Gender)) + 
   geom_pointrange(
     aes(ymin = `2.5%`, ymax = `97.5%`), 
     position = position_dodge(width = 0.5),
     alpha = 0.6, size = 0.5
   ) +
   coord_flip(ylim = c(-1,3)) +
   geom_hline(yintercept = 0, size = 0.1, alpha = 0.5) +
   scale_y_continuous(breaks = seq(-1,3,1))+
   theme_classic() +
   scale_colour_manual(
     values = c( "#db7093","#6ca6cd"),
     name = "Gender", 
     labels = c("Female", "Male"), 
     guide = guide_legend(reverse = T) ) +
   ylab("Size") +
   theme(
     axis.text = element_text(size = 12),
     axis.title.y = element_blank(),
     axis.title.x = element_text(size = 14),
     axis.line.x = element_line(),
     axis.line.y = element_blank(),
     axis.ticks.y = element_blank(),
     legend.text = element_text(size = 12),
     legend.title = element_text(size = 14),
     legend.position = "none"
   ) )

(Plot_Ego_Density_rethinkings_model2 <- Out_Density_model_rethinkings %>% 
    filter(Variable %in% c("Pilgrimage Score" ,"Daily Practice (Yes)" ,"Relatedness to monks" ) ) %>%  
    ggplot(aes(x = Variable, y = posterior_mean, colour = Gender)) + 
    geom_pointrange(
      aes(ymin = `2.5%`, ymax = `97.5%`), 
      position = position_dodge(width = 0.5),
      alpha = 0.6, size = 0.5
    ) +
    coord_flip(ylim = c(-0.5,1)) +
    geom_hline(yintercept = 0, size = 0.1, alpha = 0.5) +
    scale_y_continuous(breaks = seq(-0.5,1,0.5))+
    theme_classic() +
    scale_colour_manual(
      values = c( "#db7093","#6ca6cd"),
      name = "Gender", 
      labels = c("Female", "Male"), 
      guide = guide_legend(reverse = T) ) +
    ylab("Transitivity") +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 14),
      axis.line.x = element_line(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      legend.position = "none"
    ) )

(Plot_Ego_Reciprocity_rethinkings_model2 <- Out_Reciprocity_model_rethinkings %>% 
    filter(Variable %in% c("Pilgrimage Score" ,"Daily Practice (Yes)" ,"Relatedness to monks" ) ) %>% 
    ggplot(aes(x = Variable, y = posterior_mean, colour = Gender)) + 
    geom_pointrange(
      aes(ymin = `2.5%`, ymax = `97.5%`), 
      position = position_dodge(width = 0.5),
      alpha = 0.6, size = 0.5
    ) +
    coord_flip(ylim = c(-0.5,1)) +
    geom_hline(yintercept = 0, size = 0.1, alpha = 0.5) +
    scale_y_continuous(breaks = seq(-0.5,1,0.5))+
    theme_classic() +
    scale_colour_manual(
      values = c( "#db7093","#6ca6cd"),
      name = "Gender", 
      labels = c("Female", "Male"), 
      guide = guide_legend(reverse = T) ) +
    ylab("Reciprocity") +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_text(size = 14),
      axis.line.x = element_line(),
      axis.line.y = element_blank(),
      axis.ticks.y = element_blank(),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14)
    ) )


(Plot_Ego_Size_rethinkings_model2| Plot_Ego_Density_rethinkings_model2|Plot_Ego_Reciprocity_rethinkings_model2) 

ggsave("Plot_Model_Bayes_rethinking_4.tiff", units="in", width=12, height=7, 
       dpi=300, compression = 'lzw') 

