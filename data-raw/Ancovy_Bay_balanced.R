#Anchovy Bay
#SML

#Required packages
library(data.table); library(Rpath); library(here)

#Anchovy Bay----
groups <- c('whales', 'seals', 'cod', 'whiting', 'mackerel', 'anchovy', 'shrimp',
            'benthos', 'zooplankton', 'phytoplankton', 'detritus', 'sealers', 
            'trawlers', 'seiners', 'bait boats', 'shrimpers')

types <- c(rep(0, 9), 1, 2, rep(3, 5))

AB.params <- create.rpath.params(groups, types)

#Biomass, Production, consumption
biomass <- c(0.08, 0.0609, 3, 1.8, 1.2, 7, 0.8, NA, 14.8, 9, 10, rep(NA, 5))

pb <- c(0.05, 0.164, 0.340, 0.581, 0.723, 1.140, 3, 3, 35, 240, rep(NA, 6))

qb <- c(9, 15, 2.58, 3.3, 4.4, 9.13, rep(NA, 10))

AB.params$model[, Biomass := biomass]
AB.params$model[, PB      := pb]
AB.params$model[, QB      := qb]

AB.params$model[Group == 'shrimp',      ProdCons := 0.25]
AB.params$model[Group == 'benthos',     ProdCons := 0.25]
AB.params$model[Group == 'zooplankton', ProdCons := 0.25]

#Add EE's for unknown biomasses
AB.params$model[Group == 'benthos', EE := 0.6]
#Biomass accumulation and unassimilated production
AB.params$model[, BioAcc  := c(rep(0, 11), rep(NA, 5))]
AB.params$model[, Unassim := c(rep(0.2, 9), rep(0, 2), rep(NA, 5))]

#Detrital fate
AB.params$model[, detritus := c(rep(1, 10), rep(0, 6))]

#Landings/Discards
AB.params$model[Group == 'seals',    sealers      := .0045]
AB.params$model[Group == 'cod',      trawlers     := 0.45]
AB.params$model[Group == 'whiting',  trawlers     := 0.2]
AB.params$model[Group == 'mackerel', seiners      := 0.4]
AB.params$model[Group == 'anchovy',  seiners      := 1.2]
AB.params$model[Group == 'anchovy',  "bait boats" := 0.2]
AB.params$model[Group == 'shrimp',   shrimpers    := 0.05]

#Diet
AB.params$diet[, whales      := c(rep(NA, 2), 0.1, 0.1, 0.2, 0.5, NA, 0.1, rep(NA, 4))]
AB.params$diet[, seals       := c(NA, NA, 0.04, 0.05, NA, NA, 0.01, 0.9, rep(NA, 4))]
AB.params$diet[, cod         := c(NA, NA, NA, 0.05, NA, 0.1, 0.01, 0.84, rep(NA, 4))]
AB.params$diet[, whiting     := c(NA, NA, 0.05, 0.05, NA, 0.45, 0.01, 0.44, rep(NA, 4))]
AB.params$diet[, mackerel    := c(rep(NA, 4), 0.05, 0.5, NA, NA, 0.45, rep(NA, 3))]
AB.params$diet[, anchovy     := c(rep(NA, 8), 1, rep(NA, 3))]
AB.params$diet[, shrimp      := c(rep(NA, 7), 1, rep(NA, 4))]
AB.params$diet[, benthos     := c(rep(NA, 7), 0.1, 0.1, 0.1, 0.7, NA)]
AB.params$diet[, zooplankton := c(rep(NA, 9), 0.9, 0.1, NA)]

#Ecopath
AB <- rpath(AB.params, 'Anchovy Bay')

usethis::use_data(DATASET, overwrite = TRUE)
usethis::use_data(DATASET, overwrite = TRUE)

# create a scenario
AB.base <- rsim.scenario(AB, AB.params, 1:100)

#Determine reference point for cod----
#Turn off fishing to get a proxy for b0
gear <- AB.params$model[Type == 3, Group]
for(i in 1:length(gear)){
  AB.base <- adjust.fishing(AB.base, parameter = 'ForcedEffort', group = gear[i], 
                            value = 0, sim.year = 0:100)
}
AB.b0 <- rsim.run(AB.base, method = 'RK4', 1:25)

#Extract cod data and find b0
cod <- extract.node(AB.b0, 'cod')
cod.b0  <- max(cod$Biomass)

#Set reference point of 1/2 b0
cod.ref <- .5 * cod.b0

#control rule function----
#This modifies the effort matrix of the Rsim scenario object
bio.rule <- function(Rsim.scenario, Rsim.run, group, gear, ref.point, year){
  group.num <- which(Rsim.scenario$params$spname == group)
  gear.num  <- which(Rsim.scenario$params$spname == gear) - Rsim.scenario$params$NUM_BIO
  current.effort <- Rsim.scenario$fishing$ForcedEffort[(year - 1)*12 + 1, gear.num]
  if(Rsim.run$end_state$Biomass[group.num] > ref.point){
    Rsim.scenario <- adjust.fishing(Rsim.scenario, 'ForcedEffort', group = gear, 
                                    sim.year = year + 1, value = current.effort * 1.05)
  }
  if(Rsim.run$end_state$Biomass[group.num] < ref.point){
    Rsim.scenario <- adjust.fishing(Rsim.scenario, 'ForcedEffort', group = gear, 
                                    sim.year = year + 1, value = current.effort * .75)
  }
return(Rsim.scenario)
}

#Run multistep scenario----
AB.base <- rsim.scenario(AB, AB.params, 1:100)
AB.init <- rsim.run(AB.base, method = 'AB', 1:25)
AB.full <- copy(AB.init)
for(i in 25:99){
  AB.base <- bio.rule(AB.base, AB.full, 'cod', 'trawlers', cod.ref, i)
  AB.full <- rsim.step(AB.base, AB.full, method = 'AB', i + 1)
}

#Visualize results
rsim.plot(AB.full, groups[1:11])
cod <- extract.node(AB.full, 'cod')
plot(cod$Biomass, xlab = 'Month', ylab = 'Biomass')
abline(h = cod.ref)






