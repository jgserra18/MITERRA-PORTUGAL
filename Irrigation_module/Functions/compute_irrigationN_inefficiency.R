source('./Runoff_module/Function/Compute_irrigation_runoff.R')

#computes irrigation N lost through inefficiencies
#if year is missing it computes for both years
irrig_ineffiency <- compute_irrigation_sys_inefficiency(write=TRUE)

