#Testing migration on Achovy Bay
library(Rpath); library(data.table); library(here)

#Load AB
load(here('data', 'AB.params.rda'))
load(here('data', 'AB.rda'))

#Baseline run
AB.scene <- rsim.scenario(AB, AB.params, 1980:2000)
AB.baserun <- rsim.run(AB.scene, years = 1980:2000)

rsim.plot(AB.baserun)

#Add migration
AB.mig <- copy(AB.scene)
AB.mig <- adjust.forcing(AB.mig, 'ForcedMigrate', 'mackerel', 
                         sim.year = 1980, sim.month = c(4, 9), 
                         value = c(5, -1.0))
AB.migrun <- rsim.run(AB.mig, years = 1980:2000)

rsim.plot(AB.migrun)

#Test other forcing functions
AB.bio <- copy(AB.scene)
AB.bio <- adjust.forcing(AB.bio, 'ForcedBio', 'mackerel', 
                         sim.year = 1980, sim.month = c(4, 9),
                         value = c(0.6, 1.2))
AB.biorun <- rsim.run(AB.bio, years = 1980:2000, method = 'AB')

rsim.plot(AB.biorun)

#Migrate every year
AB.mig2 <- copy(AB.scene)
AB.mig2 <- adjust.forcing(AB.mig2, 'ForcedMigrate', 'mackerel', 
                         sim.year = 1980:2000, sim.month = c(4, 9), 
                         value = c(5, -2))
AB.migrun2 <- rsim.run(AB.mig2, years = 1980:2000)

rsim.plot(AB.migrun2)
