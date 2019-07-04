source('./Indirect_emissions_module/Function/Generic_indirectN2O_functions.R')

#computes individual indirect N2O sources (i.e. leaching AND runoff) for each YEAR and tier (i.e. ipcc and not_ipcc)
individual_ind_n2o_files <- compile_ind_n2o_all()

#aggregates runoff and leaching sources in the same dataset for each YEAR and tier
aggregated_ind_n2o <- populate_aggregated_ind_n2o()
