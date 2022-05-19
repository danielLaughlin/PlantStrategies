#############################
### Plant Strategies      ###
### Daniel C. Laughlin    ###
#############################

### Chapter 12 ###

### Estimates of establishment probability in published IPMS
library(dplyr)
library(Rpadrino)

pdb <- pdb_download(save = FALSE)
#est_p_pars <- filter(pdb$ParameterValues, grepl("est", parameter_name))
est_p_pars <- filter(pdb$ParameterValues, grepl("est", parameter_name) | parameter_name %in% c("Pe", "Ep"))
est_p_pars <- est_p_pars[-c(19:20),] # remove coral

ids <- unique(est_p_pars$ipm_id) 
est_p_pdb <- pdb_subset(pdb, ids) # subset the database
pdb_report(est_p_pdb) # generate an rmd report w/ citations and some other summary info for each study in there

range(est_p_pars$parameter_value)
hist(est_p_pars$parameter_value, breaks=50)
median(est_p_pars$parameter_value)
round(est_p_pars$parameter_value, 5)
