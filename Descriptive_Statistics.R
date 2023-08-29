### Descriptive statistics in the main text.

# Daily practice
chisq.test(networkcapital$Gender,
           networkcapital$DailyRegularscore2)

chisq.test(networkcapital$Age_cohort,
           networkcapital$DailyRegularscore2)

chisq.test(networkcapital$Rank1,
           networkcapital$DailyRegularscore2)

# Pilgrimage score
wilcox.test(Fiveyearscore ~ Gender, 
            data = networkcapital )

kruskal.test( Fiveyearscore ~ Age_cohort, 
              data = networkcapital)

kruskal.test( Fiveyearscore ~ Rank1, 
              data = networkcapital)

cor.test(networkcapital$Fiveyearscore, 
         networkcapital %>% 
           mutate(Rank1 = case_when(Rank1 == "H" ~ 3,
                                    Rank1 == "M" ~ 2,
                                    Rank1 == "L" ~ 1,) ) %$% Rank1, 
         method = "spearman")


### Table S3

Daily_score %>% 
  select(2:4,6,8,10,12,22,24,26,28) %>% 
  tableone::CreateTableOne(var = colnames(.)[c(1,3:7)] ,
                           strata = c("Gender"),
                           factorVars = c("LocalPilgrimageRegOrUnreg",
                                          "Kowtow",
                                          "TurnBeads",
                                          "BurningLastMonth",
                                          "FastingLY",
                                          "PerambulationLY"),data = .)

### Table S4







### Table S5
# Full network Summary

socialsupport_network <- igraph::graph.data.frame(
  d= Edgelist_socialsupport,
  vertices = networkcapital,
  directed = T) %>% 
  intergraph::asNetwork() 

network.size(socialsupport_network)

network.edgecount(socialsupport_network)

intergraph::asIgraph(socialsupport_network) %>% 
  degree(.,mode = "all") %>% 
  mean(.)

intergraph::asIgraph(socialsupport_network) %>% 
  degree(.,mode = "in") %>% 
  mean(.)

intergraph::asIgraph(socialsupport_network) %>% 
  edge_density()

intergraph::asIgraph(socialsupport_network) %>% 
  reciprocity(.)

intergraph::asIgraph(socialsupport_network) %>% 
  transitivity(., type="global")

intergraph::asIgraph(socialsupport_network) %>% 
  diameter(.)

# Separated networks Summary

Emotionally_support_network <- get.inducedSubgraph(socialsupport_network,
                                                   eid = which( socialsupport_network %e% 
                                                                  "Support" =="Emotionally")) 

Financially_support_network <- get.inducedSubgraph(socialsupport_network,
                                                   eid = which( socialsupport_network %e% 
                                                                  "Support" =="Financially")) 

Guaranty_support_network    <- get.inducedSubgraph(socialsupport_network,
                                                   eid = which( socialsupport_network %e% 
                                                                  "Support" =="Guaranty")) 

Physically_support_network  <- get.inducedSubgraph(socialsupport_network,
                                                   eid = which( socialsupport_network %e% 
                                                                  "Support" =="Physically")) 

Suggestion_support_network  <- get.inducedSubgraph(socialsupport_network,
                                                   eid = which( socialsupport_network %e% 
                                                                  "Support" =="Suggestion")) 
Separate_network <- list( Emotionally_support_network,
                          Physically_support_network, 
                          Suggestion_support_network,
                          Financially_support_network, 
                          Guaranty_support_network)

map(Separate_network,~ network.size(.))

map(Separate_network,~ network.edgecount(.))

map(Separate_network,~ intergraph::asIgraph(.) %>% 
      degree(.,mode = "all") %>% 
      mean(.))

map(Separate_network,~ intergraph::asIgraph(.) %>% 
      degree(.,mode = "in") %>% 
      mean(.))

map(Separate_network,~ intergraph::asIgraph(.)) %>% 
  map(., ~ ecount(.)/(vcount(.)*(vcount(.)-1)))

map(Separate_network,~ intergraph::asIgraph(.) %>% 
      reciprocity(.))

map(Separate_network,~ intergraph::asIgraph(.) %>% 
      transitivity(., type="global"))

map(Separate_network,~ intergraph::asIgraph(.) %>% 
      diameter(.))