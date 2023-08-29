socialsupport_network <- igraph::graph.data.frame(
  d= Edgelist_socialsupport,
  vertices = networkcapital3,
  directed = T) %>% 
  intergraph::asNetwork() 

### Full network
Full_model_scale_Monk2_1_4 <- ergm(socialsupport_network ~ edges  + 
                                     nodeicov("Age")+
                                     nodeifactor("Gender")+
                                     nodeifactor("Rank1")+
                                     nodematch("WhichTribe")+
                                     nodematch("Gender")+
                                     edgecov(kinship_matrix, attr = "r")+
                                     edgecov(Kinship_affinal_all_matrix,
                                             attrname ="Affinal_r_rank")+
                                     edgecov(gps_matrix, attr = "distance"),
                                   verbose = F) 

Full_model_scale_Monk2_1_2 <- ergm(socialsupport_network ~ edges  + 
                                     nodeicov("Age")+
                                     nodeifactor("Gender")+
                                     nodeifactor("Rank1")+
                                     nodematch("WhichTribe")+
                                     nodematch("Gender")+
                                     edgecov(kinship_matrix, attr = "r")+
                                     edgecov(Kinship_affinal_all_matrix,
                                             attrname ="Affinal_r_rank") +
                                     edgecov(gps_matrix, attr = "distance")+
                                     nodeicov("ScaleFiveyearscore")+
                                     nodeifactor("DailyRegularscore2"),
                                   verbose = F) 

Full_model_scale_Monk2_1_6 <- ergm(socialsupport_network ~ edges  + 
                                     nodeicov("Age")+
                                     nodeifactor("Gender")+
                                     nodeifactor("Rank1")+
                                     nodematch("WhichTribe")+
                                     nodematch("Gender")+
                                     edgecov(kinship_matrix, attr = "r")+
                                     edgecov(Kinship_affinal_all_matrix,
                                             attrname ="Affinal_r_rank") +
                                     edgecov(gps_matrix, attr = "distance")+
                                     nodeicov("ScaleFiveyearscore")+
                                     nodeifactor("DailyRegularscore2")+
                                     mutual() +
                                     gwdsp(0.5,fixed = T),
                                   verbose = F) 

# Table 1
Full_model_scale_Monk2_1_6 %>% 
  tbl_regression(exponentiate = F,
                 estimate_fun = 
                   purrr::partial(style_sigfig, 
                                  digits = 4)) %>% 
  as_gt() %>% 
  gt::gtsave(., 
             "Table1.html")

# Table S6
map(list(Full_model_scale_Monk2_1_4,
         Full_model_scale_Monk2_1_2,
         Full_model_scale_Monk2_1_6), 
    ~ tbl_regression(.x,estimate_fun =
                       purrr::partial(
                         style_sigfig, digits = 3))) %>% 
  tbl_merge()



### Separated networks
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

Separate_model3 <- map( Separate_network, 
                        ~ ergm(. ~ edges  + 
                                 nodeicov("Age")+
                                 nodeifactor("Gender")+
                                 nodeifactor("Rank1")+
                                 nodematch("WhichTribe")+
                                 nodematch("Gender")+
                                 edgecov(kinship_matrix, attr = "r")+
                                 edgecov(Kinship_affinal_all_matrix,
                                         attrname ="Affinal_r_rank") +
                                 edgecov(gps_matrix, attr = "distance")+
                                 nodeicov("ScaleFiveyearscore")+
                                 nodeifactor("DailyRegularscore2")+
                                 mutual()+
                                 idegree(0)+
                                 odegree(0)+
                                 gwdsp(0.5,fixed = T),
                               verbose = F))

# Table 2 & Table S12
map(Separate_model3,
    ~ tbl_regression(.x,
                     estimate_fun =
                       purrr::partial(
                         style_sigfig, digits = 3))) %>% 
  tbl_merge()

# Table S7 - Table S11
control_formula <- ". ~ edges  + 
  nodeicov('Age')+
  nodeifactor('Gender')+
  nodeifactor('Rank1')+
  nodematch('WhichTribe')+
  nodematch('Gender')+
  edgecov(kinship_matrix, attr = 'r')+
  edgecov(Kinship_affinal_all_matrix,
          attrname ='Affinal_r_rank') +
  edgecov(gps_matrix, attr = 'distance') "   

religion_formula <- paste0(control_formula,
                           "+ nodeicov('ScaleFiveyearscore')+
                     nodeifactor('DailyRegularscore2')") 

structure_formula <- paste0(religion_formula,"+
                     mutual()+
                     idegree(0)+
                     odegree(0)+
                     gwdsp(0.5,fixed = T)")

Separate_model_control <- map(Separate_network, 
                              ~ ergm(as.formula(control_formula)))
Separate_model_religion <- map(Separate_network, 
                               ~ ergm(as.formula(religion_formula)))
Separate_model_structure <- Separate_model3

results_control <- map(Separate_model_control, 
                       ~ tbl_regression(.x, 
                                        estimate_fun = 
                                          purrr::partial(style_sigfig, 
                                                         digits = 3)))

results_religion <- map(Separate_model_religion, 
                        ~ tbl_regression(.x, 
                                         estimate_fun = 
                                           purrr::partial(style_sigfig, 
                                                          digits = 3)))
results_structure <- map(Separate_model_structure, 
                         ~ tbl_regression(.x, 
                                          estimate_fun = 
                                            purrr::partial(style_sigfig, 
                                                           digits = 3)))

merged_results <- pmap(
  list(results_control, 
       results_religion, 
       results_structure), 
  ~ tbl_merge(list(..1, ..2, ..3)))


walk(seq_along(merged_results), 
     ~ {
       gt_table <- merged_results[[.x]] %>% 
         gtsummary::as_gt()
       gtsave(gt_table, 
              paste0("Separated_models_stepwise", 
                     .x, ".html"))
     })

AIC_results <- pmap(
  list(Separate_model_control, 
       Separate_model_religion, 
       Separate_model_structure), AIC)

AIC_results <- set_names(AIC_results,
                         c("Emotionally_support_network",
                           "Physically_support_network " , 
                           "Suggestion_support_network ",
                           "Financially_support_network" , 
                           "Guaranty_support_network   "))
print(AIC_results)




  