# Religiosity-and-gender-bias-structure-social-networks
Code & data Accompanying the Paper Titled 'Religiosity and Gender Bias in Social Network Structures' by Erhao Ge et al.

This repository contains the R code and Python code required to replicate the tables and figures presented in the paper, and rds files required to run the R codes.

File Overview
Preparation:
Loading_data.R: Imports the essential data and packages required for executing the R code.
Analysis:
Descriptive_Statistics.R: R code for generating the descriptive statistics mentioned in the main text.
Plots.R: R code to reproduce all figures in the main text and supplementary information (SI), except for Figure 2 (created with Gephi) and Figures S2-S6 (created with Python).
Plots.Rmd: Rmd file for replicating and visualizing all figures in the main text and SI.
Figure_S2_S6.py: Python code to replicate Figures S2-S6.
Models_sociocentric.R: R code to replicate sociocentric network models, as seen in Tables 1, 2, and S6-S12.
Models_Egocentric.R: R code for reproducing the egocentric network models presented in Tables S13 and S14.
Data:
Sociocentric Analysis:
Edgelist_socialsupport.rds: Data on social relationships.
Other_covariate_matrix.rds: Edge-level attribute data, including consanguineous and affinal kinship between residents, as well as geographic distances between households.
networkcapital.rds, networkcapital2.rds, networkcapital3.rds: Node-level attribute data, featuring variables such as religiosity and socio-economic status.
Egocentric Analysis:
Personal_network.rds: Data used for personal network analysis.

For any inquiries, please reach out to ucsaege@ucl.ac.uk.
