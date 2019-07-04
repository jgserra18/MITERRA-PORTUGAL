source('./Gaseous_N_losses/N-N2O/Function/irrigation_N2O_functions.R')
source('./Irrigation_module/Functions/compute_irrigatioN.R')

#get EFs for the main irrig systems
#optional
efs <- get_irrig_ef()

#get EFs for all irrig systems
all_efs <- irrig_ef_conditions()

#compute N2O from irrigationN
#efficiency is turned off
#tier 2
compute_n2o_irrig_sys99 <- compute_n2o_irrig_sys(FALSE, 1999, export_activity = T, 2) #if error, it is due to gaseous_functions source
compute_n2o_irrig_sys09 <- compute_n2o_irrig_sys(FALSE, 2009, export_activity = T, 2)

#tier 1
compute_n2o_irrig_sys99 <- compute_n2o_irrig_sys(FALSE, 1999, TRUE, 1)
compute_n2o_irrig_sys09 <- compute_n2o_irrig_sys(FALSE, 2009, TRUE, 1)