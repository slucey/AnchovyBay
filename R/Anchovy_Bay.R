#To download Rpath
# #This only needs to be done the first time you run the script
#library(devtools)
#devtools::install_github('NOAA-EDAB/Rpath', build_vignettes = TRUE)

library(Rpath); library(data.table)

groups <- c('whales', 'seals', 'cod', 'whiting', 'mackerel', 'anchovy', 'shrimp',
            'benthos', 'zooplankton', 'phytoplankton', 'detritus', 'sealers', 
            'trawlers', 'seiners', 'bait boats', 'shrimpers')

types <- c(rep(0, 9), 1, 2, rep(3, 5))

AB.params <- create.rpath.params(groups, types)

#Remember that data tables work like a sql statement
#sql: Select x where y = z from table
#data.table: table[y == z, x]

#Can also assign values using ':=' operator [this is an example]
AB.params$model[Group == 'cod', Biomass := 3]

# Static parameters for this example
#Biomass accumulation and unassimilated production
AB.params$model[Type < 3, BioAcc  := 0]
AB.params$model[Type < 2, Unassim := 0.2]
AB.params$model[Type == 2, Unassim := 0]
#Detrital Fate
AB.params$model[Type < 2, detritus := 1]
AB.params$model[Type > 1, detritus := 0]

#Check for issues in your parameter file with
check.rpath.params(AB.params)

#Once parameter file is built use this to run ecopath
AB <- rpath(AB.params, 'Anchovy Bay')
AB
print(AB, morts = T)
webplot(AB)

#Running Rsim
# 3 step process 
#Set the scene with rsim.scenario
AB.scene <- rsim.scenario(AB, AB.params, 1:25)

#Make any adjustments
AB.scene <- adjust.fishing(AB.scene, 'ForcedEffort', group = 'trawlers',
                           sim.year = 10:25, value = 0.5)

#Run the scenario
AB.run <- rsim.run(AB.scene, years = 1:25)
