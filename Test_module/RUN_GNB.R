source('./Test_module/RUN_GNB_activity_data.R')
source('./GNB_module/Function/GNB_computation.R')


####################################################################
#########################   NOT MANDATORY   ########################
#======================================================================#
#subfunction that is then called in gnb_sum_input and gnb_sum_output

#id_in <- gnb_element_id(db_file99)[1]
#id_out  <- gnb_element_id(db_file99)[2]

#calculate total input and outputs, then merge each into a DF
#this increates an individual DF for the sum of each gnb parameter
#sub-function of gnb_compute

#inp_sum99 <- gnb_sum_input(db_file99, F)
#out_sum99 <- gnb_sum_output(db_file99)
#======================================================================#

#calculate GNBs wo/ irrigationN
gnb99 <- gnb_compute(db_file99)
gnb09 <- gnb_compute(db_file09)

#calculate GNBs w/ irrigationN
gnb09_irrig <- gnb_compute(irrig_db_file09, TRUE)
gnb99_irrig <- gnb_compute(irrig_db_file99, TRUE)

#optional: calculate NUEs
nue99 <- nue_compute(gnb99)
nue09 <- nue_compute(gnb09)

nue99_irrig <- nue_compute(gnb99_irrig)
nue09_irrig <- nue_compute(gnb09_irrig)

irrig_impact99 <- compute_irrig_influence(nue99, nue99_irrig)
irrig_impact09 <- compute_irrig_influence(nue09, nue09_irrig)

#write to GNB_module\Output
write_output('GNB', gnb99, 'gnb99_default')
write_output('GNB', gnb09, 'gnb09_default')

write_output('GNB', gnb99_irrig, 'gnb99_irrig')
write_output('GNB', gnb09_irrig, 'gnb09_irrig')

write_output('GNB', irrig_impact99, 'irrig_impact_InputGnbNue99')
write_output('GNB', irrig_impact09, 'irrig_impact_InputGnbNue09')

