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

#Can also assign values using ':=' operator
AB.params$model[Group == 'cod', Biomass := 3]

#Check for issues in your parameter file with
check.rpath.params(AB.params)

#Once parameter file is built use this to run ecopath
AB <- rpath(AB.params, 'Anchovy Bay')
AB
print(AB, morts = T)
webplot(AB)
