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
          star.cutoffs = c(0.05,0.01,0.001),
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

