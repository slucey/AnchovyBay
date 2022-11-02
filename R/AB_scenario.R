#How to run a scenario
library(Rpath); library(data.table); library(here); library(ggplot2)

#Use balanced Anchovy Bay model
load(here('data', 'AB.params.rda'))
load(here('data', 'AB.rda'))

# create a scenario
AB.base <- rsim.scenario(AB, AB.params, 1:25)

#Adjust fishing effort
AB.base <- adjust.fishing(AB.base, parameter = 'ForcedEffort', group = 'bait boats',
                          sim.year = 10:25, value = 2.5)

#Adjust vulnerabilities
#Can be somewhat confusing - the group argument is the prey while the groupto
#argument is the predator - can set these for a predator/prey pair or as the
#example below for a predator on all of its prey
AB.base <- adjust.scenario(AB.base, parameter = 'VV', group = 'all', 
                           groupto = 'whiting', value = 3)

#Run scenario
AB.run <- rsim.run(AB.base, years = 1:25)

#Output results
AB.run
#To capture results as an object use the write.Rsim function
AB.output <- write.Rsim(AB.run)

#plot results
rsim.plot(AB.run)

#Individual results
anchovy <- extract.node(AB.run, 'anchovy')
plot(anchovy$Biomass, xlab = 'Month', ylab = 'Biomass')





