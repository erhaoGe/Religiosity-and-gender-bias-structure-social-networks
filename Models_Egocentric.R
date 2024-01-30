library(tidyverse)
library(MASS)
library(performance)

##### Table S13
## -----------------------------------------------------------------------

ml <- list()
ml[[1]] <- glm( All ~ Age_cohort + Gender +
                  Rank1 + Siblinginvillage + 
                  Offspringinvillage,
                family = "poisson",
                data = Personal_network)

ml[[2]] <- glm( All ~ Age_cohort + Gender +
                  Rank1 + Siblinginvillage + 
                  Offspringinvillage + 
                  Fiveyearscore2 +
                  DailyRegularscore2,
                family = "poisson",
                data = Personal_network)

ml[[3]] <- glm( All ~ Age_cohort + Gender +
                  Rank1 + Siblinginvillage + 
                  Offspringinvillage +
                  Fiveyearscore2+
                  Gender*DailyRegularscore2,
                family = "poisson",
                data = Personal_network)

ml[[4]] <- glm( All ~ Age_cohort + Gender +
                  Rank1 + Siblinginvillage + 
                  Offspringinvillage +
                  Fiveyearscore2*Gender +
                  DailyRegularscore2,
                family = "poisson",
                data = Personal_network)

### Checking if it's over-dispersed
check_overdispersion(ml[[1]])
check_overdispersion(ml[[2]])
check_overdispersion(ml[[3]])
check_overdispersion(ml[[4]])


# Updating the models to negative binomial models


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

stargazer(ml[[1]],
          ml[[2]],
          ml[[3]],
          ml[[4]],
          title = "Model determinants",
          align = T, 
          type = "html",
          single.row = F,
          no.space = TRUE,
          keep.stat=c("n","aic"), 
          star.cutoffs = c(0.1,0.05,0.01,0.001),
          ci = T,
          out = "Personal_network_model_interaction_details.html")



##### Table S14

model.names=c("control",
              "control+reli",
              "control+interaction1",
              "control+interaction2")

final.aics = aictab(ml, 
                    model.names,
                    second.ord = T)

print(final.aics)


final.aics %>% 
  as_tibble() %>% 
  write_csv("Personal_network_Model_comparison_tables.csv")

##### Model Fitting Check

model_poisson <-  glm( All ~ Age_cohort + Gender +
                         Rank1 + Siblinginvillage + 
                         Offspringinvillage +
                         Fiveyearscore2*Gender +
                         DailyRegularscore2,
                       family = "poisson",
                       data = Personal_network)

### Likelihood Ratio Test Comparing the Poisson Model 
### with the Negative binomial model
library(lmtest)
lrtest(model_poisson, ml[[4]])


### Creating simulated data based on the fitted model and 
### compares these simulations to the observed data
library(DHARMa)
simulated <- simulateResiduals(fittedModel = ml[[4]], n = 250)
plot(simulated)

### Cross-Validation to examine how the model performs on unseen data.
library(sjPlot)
plot_kfold_cv(Personal_network,k = 5, ml[[4]])


