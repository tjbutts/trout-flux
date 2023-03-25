## FluxWeb Calculation - Trout Food Web ## 
library(tidyverse)
library(lubridate)
library(fluxweb)

# Load in biomass datasets # 
fish = read_csv('trout_fish_biomass_selectpelagic.csv')
pelagic.miv = read_csv('trout_pmiv_biomass.reduced.csv')
zoop.grouped = read_csv('trout_pzoop_grouped.alldates.csv')

# fluxweb set up # 

# REQUIRES 
  # A matrix defining the set of trophic interactions between each species (binary for now) 
  # A vector with the average body mass of species (in g) 
  # A vector with the total biomass of each population 
  # A vector with the organism type of each species (plant, animal, detritus, etc.) 
  # A vector of metabolic types rate estimations (provided in paper) 

# Losses 
  # Losses defined as metabolic rates and are calculated using the species body mass 
    # losses = 0.71 * bodymass^(-0.25) - general 
  # More precise estimate 
    # losses = rep(NA, nb.species) - nb.species = number of taxa in the food web 
    # ecto.vert = met.types = 'Ectotherm vertebrates' (fish) 
    # endo.vert = met.types = 'Endotherm vertebrates' 
    # inv = met.types = 'Invertebrates' 

# Efficiencies 
  # The last parameter needed to estimate fluxes is the vector of feeding efficiencies. 
    # Since we estimated losses using metabolic rates - assimilation efficiencies should be used 
      # assimilation efficiency defines the proportion of eaten biomass that can be used for biomass production plus metabolism # 
      # Define efficiency based on organism type 
        # - efficiency with which a predator will assimilate energy from a prey defined by type of prey eaten 

# Trout Fluxing - Base #==============================

# Set up fluxweb dataframe (long format + wide matrix) # 
fish.select = fish %>% 
  select(year, spp, biomass.g_perhec, biomass_g) %>% 
  rename(bodymass_g = biomass_g) %>% 
  mutate(met.type = 'Ectothermic.vertebrate', 
         org.type = 'animal') %>% 
  mutate(losses = 18.18 * bodymass_g^(-0.29)) %>% # Calculate losses using metabolic theory of ecology 
  mutate(efficiencies = 0.906) # If consumed - would be animal(ef level = prey) 
fish.select

pelagic.miv.select = pelagic.miv %>%
  select(year4, taxon, biomass.g_perhec, weight.avg.g) %>% 
  rename(year = year4, 
         spp = taxon, 
         bodymass_g = weight.avg.g) %>%
  mutate(met.type = 'Invertebrate', 
         org.type = 'animal') %>% 
  mutate(losses = 18.18 * bodymass_g^(-0.29)) %>% # Calculate losses using metabolic theory of ecology 
  mutate(efficiencies = 0.906) %>% # If consumed - would be animal (ef.level = prey) 
  mutate(spp = tolower(spp)) 
pelagic.miv.select$spp = gsub('chaoborus larvae', 'chaoborus.larvae', pelagic.miv.select$spp)
pelagic.miv.select 

zoop.grouped.select = zoop.grouped %>% 
  select(year4, sample_date, larger_group, biomass_g.perhec, avg.bodymass_g) %>% 
  mutate(sample_date = mdy(sample_date)) %>% 
  mutate(month = month(sample_date)) %>% 
  filter(month == 7 | month == 8) %>%
  rename(year = year4, 
         spp = larger_group, 
         bodymass_g = avg.bodymass_g, 
         biomass.g_perhec = biomass_g.perhec) %>% 
  group_by(year, spp) %>% 
  summarize(biomass.g_perhec = mean(biomass.g_perhec),
            bodymass_g = mean(bodymass_g)) %>% 
  mutate(met.type = 'Invertebrate', 
         org.type = 'animal') %>% 
  mutate(spp = tolower(spp)) %>% 
  mutate(losses = 18.18 * bodymass_g^(-0.29)) %>% 
  mutate(efficiencies = 0.906) 
zoop.grouped.select

# Now combine the food web data and arrange by year # 
fish.select[is.na(fish.select)] <- 0
pelagic.miv.select
zoop.grouped.select

join1 = rbind(fish.select, pelagic.miv.select)
base = rbind(join1, zoop.grouped.select) %>% arrange(year)
base

# Trout Fluxing - Base.wae_corrected #==============================

# Set up fluxweb dataframe (long format + wide matrix) # 
fish.select = fish %>% 
  select(year, spp, biomass.g_perhec.corrected, biomass_g) %>% 
  rename(bodymass_g = biomass_g) %>% 
  rename(biomass.g_perhec = biomass.g_perhec.corrected) %>% # change to base name for ease of coding 
  mutate(met.type = 'Ectothermic.vertebrate', 
         org.type = 'animal') %>% 
  mutate(losses = 18.18 * bodymass_g^(-0.29)) %>% # Calculate losses using metabolic theory of ecology 
  mutate(efficiencies = 0.906) # If consumed - would be animal(ef level = prey) 
fish.select

pelagic.miv.select = pelagic.miv %>%
  select(year4, taxon, biomass.g_perhec, weight.avg.g) %>% 
  rename(year = year4, 
         spp = taxon, 
         bodymass_g = weight.avg.g) %>%
  mutate(met.type = 'Invertebrate', 
         org.type = 'animal') %>% 
  mutate(losses = 18.18 * bodymass_g^(-0.29)) %>% # Calculate losses using metabolic theory of ecology 
  mutate(efficiencies = 0.906) %>% # If consumed - would be animal (ef.level = prey) 
  mutate(spp = tolower(spp)) 
pelagic.miv.select$spp = gsub('chaoborus larvae', 'chaoborus.larvae', pelagic.miv.select$spp)
pelagic.miv.select 

zoop.grouped.select = zoop.grouped %>% 
  select(year4, sample_date, larger_group, biomass_g.perhec, avg.bodymass_g) %>% 
  mutate(sample_date = mdy(sample_date)) %>% 
  mutate(month = month(sample_date)) %>% 
  filter(month == 7 | month == 8) %>%
  rename(year = year4, 
         spp = larger_group, 
         bodymass_g = avg.bodymass_g, 
         biomass.g_perhec = biomass_g.perhec) %>% 
  group_by(year, spp) %>% 
  summarize(biomass.g_perhec = mean(biomass.g_perhec),
            bodymass_g = mean(bodymass_g)) %>% 
  mutate(met.type = 'Invertebrate', 
         org.type = 'animal') %>% 
  mutate(spp = tolower(spp)) %>% 
  mutate(losses = 18.18 * bodymass_g^(-0.29)) %>% 
  mutate(efficiencies = 0.906) 
zoop.grouped.select

# Now combine the food web data and arrange by year # 
fish.select[is.na(fish.select)] <- 0
pelagic.miv.select
zoop.grouped.select

join1 = rbind(fish.select, pelagic.miv.select)
base.wae_corrected = rbind(join1, zoop.grouped.select) %>% arrange(year)
base.wae_corrected


# Choose which dataset to flux #===========================
# trout_pelagic.web is the dataset the fluxing code comes from 
# Datasets # 
  # Base = no manipulation 
  # Base.wae_corrected = take 20% of walleye biomass to correct for littoral feeding

### Select food web to flux  ### ==============================
trout_pelagic.web = base.wae_corrected


# Select interaction matrix #===========================
### Binary matrices ####============================ 
early.mat = read_csv('matrix_01.14.csv')
early.mat = as.matrix(early.mat)
# Post Spiny water Flea
late.mat = read_csv('matrix_15.20.csv')
late.mat = as.matrix(late.mat)

# Trout Flux Run #==============================
##2001
d01 =trout_pelagic.web %>% filter(year == 2001) %>% filter(spp != 'lake.trout' & spp != 'walleye')
d01

mat.01 = early.mat[-c(2:3),-c(2:3)]
mat.01

biom01 = c(d01$biomass.g_perhec) # biomass # 
loss01 = c(d01$losses) # metabolic losses 
effic01 = c(d01$efficiencies) # efficiencies 

flux01 = fluxing(mat.01, biom01, loss01, effic01, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux01.stability = flux01
flux01 = as_tibble(flux01)

colnames = colnames(flux01)
colnames

flux01$prey = colnames
flux01
flux01 = column_to_rownames(flux01, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux01)) 
fluxsums$fluxfrom = rownames(flux01)
fluxfrom01 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom01 = mutate(fluxfrom01, year = 2001)
outgoing01 = fluxfrom01 %>% 
  mutate(lake.trout = NA, 
         walleye = NA) %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing01

# Make growth rate for zoops (acting as basal in this web) 
growth.01 = d01 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.01
growth.01[growth.01$spp == 'cisco' | growth.01$spp == 'lake.trout' | growth.01$spp == 'walleye' | 
            growth.01$spp == 'chaoborus.larvae' | 
            growth.01$spp == 'leptodora' | growth.01$spp == 'mysis', 'growth.rate'] <- NA 
growth.01

basal.gr = growth.01$growth.rate
basal.gr

# Calculate stability # 
stab01 = stability.value(flux01.stability, biom01, loss01, effic01, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab01

stab01 = data.frame(stab01) 
stab01 = stab01 %>% 
  rename(stability = stab01) %>% 
  mutate(year = 2001)
stab01

## 2002 
d02 = trout_pelagic.web %>% filter(year == 2002) %>% filter(spp != 'lake.trout' & spp != 'walleye')
d02

mat.02 = early.mat[-c(2:3),-c(2:3)]
mat.02

biom02 = c(d02$biomass.g_perhec) # biomass # 
loss02 = c(d02$losses) # metabolic losses 
effic02 = c(d02$efficiencies) # efficiencies 

flux02 = fluxing(mat.02, biom02, loss02, effic02, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux02.stability = flux02
flux02 = as_tibble(flux02)

colnames = colnames(flux02)
colnames

flux02$prey = colnames
flux02
flux02 = column_to_rownames(flux02, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux02)) 
fluxsums$fluxfrom = rownames(flux02)
fluxfrom02 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom02 = mutate(fluxfrom02, year = 2002)
outgoing02 = fluxfrom02 %>% 
  mutate(lake.trout = NA, 
         walleye = NA) %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing02

# Make growth rate for zoops (acting as basal in this web) 
growth.02 = d02 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.02
growth.02[growth.02$spp == 'cisco' | growth.02$spp == 'lake.trout' | growth.02$spp == 'walleye' | 
            growth.02$spp == 'chaoborus.larvae' | 
            growth.02$spp == 'leptodora' | growth.02$spp == 'mysis', 'growth.rate'] <- NA 
growth.02

basal.gr = growth.02$growth.rate
basal.gr

# Calculate stability # 
stab02 = stability.value(flux02.stability, biom02, loss02, effic02, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab02

stab02 = data.frame(stab02) 
stab02 = stab02 %>% 
  rename(stability = stab02) %>% 
  mutate(year = 2002)
stab02

##2003
d03 = trout_pelagic.web %>% filter(year == 2003) %>% filter(spp != 'lake.trout' & spp != 'walleye')
d03

mat.03 = early.mat[-c(2:3),-c(2:3)]
mat.03

biom03 = c(d03$biomass.g_perhec) # biomass # 
loss03 = c(d03$losses) # metabolic losses 
effic03 = c(d03$efficiencies) # efficiencies 

flux03 = fluxing(mat.03, biom03, loss03, effic03, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux03.stability = flux03
flux03 = as_tibble(flux03)

colnames = colnames(flux03)
colnames

flux03$prey = colnames
flux03
flux03 = column_to_rownames(flux03, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux03)) 
fluxsums$fluxfrom = rownames(flux03)
fluxfrom03 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom03 = mutate(fluxfrom03, year = 2003)
outgoing03 = fluxfrom03 %>% 
  mutate(lake.trout = NA, 
         walleye = NA) %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing03

# Make growth rate for zoops (acting as basal in this web) 
growth.03 = d03 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.03
growth.03[growth.03$spp == 'cisco' | growth.03$spp == 'lake.trout' | growth.03$spp == 'walleye' | 
            growth.03$spp == 'chaoborus.larvae' | 
            growth.03$spp == 'leptodora' | growth.03$spp == 'mysis', 'growth.rate'] <- NA 
growth.03

basal.gr = growth.03$growth.rate
basal.gr

# Calculate stability # 
stab03 = stability.value(flux03.stability, biom03, loss03, effic03, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab03

stab03 = data.frame(stab03) 
stab03 = stab03 %>% 
  rename(stability = stab03) %>% 
  mutate(year = 2003)
stab03

##2004
d04 = trout_pelagic.web %>% filter(year == 2004) %>% filter(spp != 'lake.trout' & spp != 'walleye')
d04

mat.04 = early.mat[-c(2:3),-c(2:3)]
mat.04

biom04 = c(d04$biomass.g_perhec) # biomass # 
loss04 = c(d04$losses) # metabolic losses 
effic04 = c(d04$efficiencies) # efficiencies 

flux04 = fluxing(mat.04, biom04, loss04, effic04, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux04.stability = flux04
flux04 = as_tibble(flux04)

colnames = colnames(flux04)
colnames

flux04$prey = colnames
flux04
flux04 = column_to_rownames(flux04, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux04)) 
fluxsums$fluxfrom = rownames(flux04)
fluxfrom04 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom04 = mutate(fluxfrom04, year = 2004)
outgoing04 = fluxfrom04 %>% 
  mutate(lake.trout = NA, 
         walleye = NA) %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing04

# Make growth rate for zoops (acting as basal in this web) 
growth.04 = d04 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.04
growth.04[growth.04$spp == 'cisco' | growth.04$spp == 'lake.trout' | growth.04$spp == 'walleye' | 
            growth.04$spp == 'chaoborus.larvae' | 
            growth.04$spp == 'leptodora' | growth.04$spp == 'mysis', 'growth.rate'] <- NA 
growth.04

basal.gr = growth.04$growth.rate
basal.gr

# Calculate stability # 
stab04 = stability.value(flux04.stability, biom04, loss04, effic04, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab04

stab04 = data.frame(stab04) 
stab04 = stab04 %>% 
  rename(stability = stab04) %>% 
  mutate(year = 2004)
stab04

##2005
d05 = trout_pelagic.web %>% filter(year == 2005) %>% filter(spp != 'lake.trout' & spp != 'walleye')
d05

mat.05 = early.mat[-c(2:3),-c(2:3)]
mat.05

biom05 = c(d05$biomass.g_perhec) # biomass # 
loss05 = c(d05$losses) # metabolic losses 
effic05 = c(d05$efficiencies) # efficiencies 

flux05 = fluxing(mat.05, biom05, loss05, effic05, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux05.stability = flux05
flux05 = as_tibble(flux05)

colnames = colnames(flux05)
colnames

flux05$prey = colnames
flux05
flux05 = column_to_rownames(flux05, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux05)) 
fluxsums$fluxfrom = rownames(flux05)
fluxfrom05 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom05 = mutate(fluxfrom05, year = 2005)
outgoing05 = fluxfrom05 %>% 
  mutate(lake.trout = NA, 
         walleye = NA) %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing05

# Make growth rate for zoops (acting as basal in this web) 
growth.05 = d05 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.05
growth.05[growth.05$spp == 'cisco' | growth.05$spp == 'lake.trout' | growth.05$spp == 'walleye' | 
            growth.05$spp == 'chaoborus.larvae' | 
            growth.05$spp == 'leptodora' | growth.05$spp == 'mysis', 'growth.rate'] <- NA 
growth.05

basal.gr = growth.05$growth.rate
basal.gr

# Calculate stability # 
stab05 = stability.value(flux05.stability, biom05, loss05, effic05, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab05

stab05 = data.frame(stab05) 
stab05 = stab05 %>% 
  rename(stability = stab05) %>% 
  mutate(year = 2005)
stab05

##2006
d06 = trout_pelagic.web %>% filter(year == 2006) %>% filter(spp != 'lake.trout' & spp != 'walleye')
d06

mat.06 = early.mat[-c(2:3),-c(2:3)]
mat.06

biom06 = c(d06$biomass.g_perhec) # biomass # 
loss06 = c(d06$losses) # metabolic losses 
effic06 = c(d06$efficiencies) # efficiencies 

flux06 = fluxing(mat.06, biom06, loss06, effic06, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux06.stability = flux06
flux06 = as_tibble(flux06)

colnames = colnames(flux06)
colnames

flux06$prey = colnames
flux06
flux06 = column_to_rownames(flux06, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux06)) 
fluxsums$fluxfrom = rownames(flux06)
fluxfrom06 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom06 = mutate(fluxfrom06, year = 2006)
outgoing06 = fluxfrom06 %>% 
  mutate(lake.trout = NA, 
         walleye = NA) %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing06

# Make growth rate for zoops (acting as basal in this web) 
growth.06 = d06 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.06
growth.06[growth.06$spp == 'cisco' | growth.06$spp == 'lake.trout' | growth.06$spp == 'walleye' | 
            growth.06$spp == 'chaoborus.larvae' | 
            growth.06$spp == 'leptodora' | growth.06$spp == 'mysis', 'growth.rate'] <- NA 
growth.06

basal.gr = growth.06$growth.rate
basal.gr

# Calculate stability # 
stab06 = stability.value(flux06.stability, biom06, loss06, effic06, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab06

stab06 = data.frame(stab06) 
stab06 = stab06 %>% 
  rename(stability = stab06) %>% 
  mutate(year = 2006)
stab06

##2007
d07 = trout_pelagic.web %>% filter(year == 2007) 
d07

mat.07 = early.mat
mat.07

biom07 = c(d07$biomass.g_perhec) # biomass # 
loss07 = c(d07$losses) # metabolic losses 
effic07 = c(d07$efficiencies) # efficiencies 

flux07 = fluxing(mat.07, biom07, loss07, effic07, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux07.stability = flux07
flux07 = as_tibble(flux07)

colnames = colnames(flux07)
colnames

flux07$prey = colnames
flux07
flux07 = column_to_rownames(flux07, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux07)) 
fluxsums$fluxfrom = rownames(flux07)
fluxfrom07 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom07 = mutate(fluxfrom07, year = 2007)
outgoing07 = fluxfrom07 %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing07

# Make growth rate for zoops (acting as basal in this web) 
growth.07 = d07 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.07
growth.07[growth.07$spp == 'cisco' | growth.07$spp == 'lake.trout' | growth.07$spp == 'walleye' | 
            growth.07$spp == 'chaoborus.larvae' | 
            growth.07$spp == 'leptodora' | growth.07$spp == 'mysis', 'growth.rate'] <- NA 
growth.07

basal.gr = growth.07$growth.rate
basal.gr

# Calculate stability # 
stab07 = stability.value(flux07.stability, biom07, loss07, effic07, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab07

stab07 = data.frame(stab07) 
stab07 = stab07 %>% 
  rename(stability = stab07) %>% 
  mutate(year = 2007)
stab07

##2008
d08 = trout_pelagic.web %>% filter(year == 2008) 
d08

mat.08 = early.mat
mat.08

biom08 = c(d08$biomass.g_perhec) # biomass # 
loss08 = c(d08$losses) # metabolic losses 
effic08 = c(d08$efficiencies) # efficiencies 

flux08 = fluxing(mat.08, biom08, loss08, effic08, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux08.stability = flux08
flux08 = as_tibble(flux08)

colnames = colnames(flux08)
colnames

flux08$prey = colnames
flux08
flux08 = column_to_rownames(flux08, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux08)) 
fluxsums$fluxfrom = rownames(flux08)
fluxfrom08 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom08 = mutate(fluxfrom08, year = 2008)
outgoing08 = fluxfrom08 %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing08

# Make growth rate for zoops (acting as basal in this web) 
growth.08 = d08 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.08
growth.08[growth.08$spp == 'cisco' | growth.08$spp == 'lake.trout' | growth.08$spp == 'walleye' | 
            growth.08$spp == 'chaoborus.larvae' | 
            growth.08$spp == 'leptodora' | growth.08$spp == 'mysis', 'growth.rate'] <- NA 
growth.08

basal.gr = growth.08$growth.rate
basal.gr

# Calculate stability # 
stab08 = stability.value(flux08.stability, biom08, loss08, effic08, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab08

stab08 = data.frame(stab08) 
stab08 = stab08 %>% 
  rename(stability = stab08) %>% 
  mutate(year = 2008)
stab08

##2009
d09 = trout_pelagic.web %>% filter(year == 2009) 
d09

mat.09 = early.mat
mat.09

biom09 = c(d09$biomass.g_perhec) # biomass # 
loss09 = c(d09$losses) # metabolic losses 
effic09 = c(d09$efficiencies) # efficiencies 

flux09 = fluxing(mat.09, biom09, loss09, effic09, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux09.stability = flux09
flux09 = as_tibble(flux09)

colnames = colnames(flux09)
colnames

flux09$prey = colnames
flux09
flux09 = column_to_rownames(flux09, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux09)) 
fluxsums$fluxfrom = rownames(flux09)
fluxfrom09 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom09 = mutate(fluxfrom09, year = 2009)
outgoing09 = fluxfrom09 %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing09

# Make growth rate for zoops (acting as basal in this web) 
growth.09 = d09 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.09
growth.09[growth.09$spp == 'cisco' | growth.09$spp == 'lake.trout' | growth.09$spp == 'walleye' | 
            growth.09$spp == 'chaoborus.larvae' | 
            growth.09$spp == 'leptodora' | growth.09$spp == 'mysis', 'growth.rate'] <- NA 
growth.09

basal.gr = growth.09$growth.rate
basal.gr

# Calculate stability # 
stab09 = stability.value(flux09.stability, biom09, loss09, effic09, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab09

stab09 = data.frame(stab09) 
stab09 = stab09 %>% 
  rename(stability = stab09) %>% 
  mutate(year = 2009)
stab09

##2010
d10 = trout_pelagic.web %>% filter(year == 2010) 
d10

mat.10 = early.mat
mat.10

biom10 = c(d10$biomass.g_perhec) # biomass # 
loss10 = c(d10$losses) # metabolic losses 
effic10 = c(d10$efficiencies) # efficiencies 

flux10 = fluxing(mat.10, biom10, loss10, effic10, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux10.stability = flux10
flux10 = as_tibble(flux10)

colnames = colnames(flux10)
colnames

flux10$prey = colnames
flux10
flux10 = column_to_rownames(flux10, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux10)) 
fluxsums$fluxfrom = rownames(flux10)
fluxfrom10 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom10 = mutate(fluxfrom10, year = 2010)
outgoing10 = fluxfrom10 %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing10

# Make growth rate for zoops (acting as basal in this web) 
growth.10 = d10 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.10
growth.10[growth.10$spp == 'cisco' | growth.10$spp == 'lake.trout' | growth.10$spp == 'walleye' | 
            growth.10$spp == 'chaoborus.larvae' | 
            growth.10$spp == 'leptodora' | growth.10$spp == 'mysis', 'growth.rate'] <- NA 
growth.10

basal.gr = growth.10$growth.rate
basal.gr

# Calculate stability # 
stab10 = stability.value(flux10.stability, biom10, loss10, effic10, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab10

stab10 = data.frame(stab10) 
stab10 = stab10 %>% 
  rename(stability = stab10) %>% 
  mutate(year = 2010)
stab10

##2011
d11 = trout_pelagic.web %>% filter(year == 2011) 
d11

mat.11 = early.mat
mat.11

biom11 = c(d11$biomass.g_perhec) # biomass # 
loss11 = c(d11$losses) # metabolic losses 
effic11 = c(d11$efficiencies) # efficiencies 

flux11 = fluxing(mat.11, biom11, loss11, effic11, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux11.stability = flux11
flux11 = as_tibble(flux11)

colnames = colnames(flux11)
colnames

flux11$prey = colnames
flux11
flux11 = column_to_rownames(flux11, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux11)) 
fluxsums$fluxfrom = rownames(flux11)
fluxfrom11 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom11 = mutate(fluxfrom11, year = 2011)
outgoing11 = fluxfrom11 %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing11

# Make growth rate for zoops (acting as basal in this web) 
growth.11 = d11 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.11
growth.11[growth.11$spp == 'cisco' | growth.11$spp == 'lake.trout' | growth.11$spp == 'walleye' | 
            growth.11$spp == 'chaoborus.larvae' | 
            growth.11$spp == 'leptodora' | growth.11$spp == 'mysis', 'growth.rate'] <- NA 
growth.11

basal.gr = growth.11$growth.rate
basal.gr

# Calculate stability # 
stab11 = stability.value(flux11.stability, biom11, loss11, effic11, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab11

stab11 = data.frame(stab11) 
stab11 = stab11 %>% 
  rename(stability = stab11) %>% 
  mutate(year = 2011)
stab11

##2012
d12 = trout_pelagic.web %>% filter(year == 2012) 
d12

mat.12 = early.mat
mat.12

biom12 = c(d12$biomass.g_perhec) # biomass # 
loss12 = c(d12$losses) # metabolic losses 
effic12 = c(d12$efficiencies) # efficiencies 

flux12 = fluxing(mat.12, biom12, loss12, effic12, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux12.stability = flux12
flux12 = as_tibble(flux12)

colnames = colnames(flux12)
colnames

flux12$prey = colnames
flux12
flux12 = column_to_rownames(flux12, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux12)) 
fluxsums$fluxfrom = rownames(flux12)
fluxfrom12 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom12 = mutate(fluxfrom12, year = 2012)
outgoing12 = fluxfrom12 %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing12

# Make growth rate for zoops (acting as basal in this web) 
growth.12 = d12 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.12
growth.12[growth.12$spp == 'cisco' | growth.12$spp == 'lake.trout' | growth.12$spp == 'walleye' | 
            growth.12$spp == 'chaoborus.larvae' | 
            growth.12$spp == 'leptodora' | growth.12$spp == 'mysis', 'growth.rate'] <- NA 
growth.12

basal.gr = growth.12$growth.rate
basal.gr

# Calculate stability # 
stab12 = stability.value(flux12.stability, biom12, loss12, effic12, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab12

stab12 = data.frame(stab12) 
stab12 = stab12 %>% 
  rename(stability = stab12) %>% 
  mutate(year = 2012)
stab12

##2013
d13 = trout_pelagic.web %>% filter(year == 2013) 
d13

mat.13 = early.mat
mat.13

biom13 = c(d13$biomass.g_perhec) # biomass # 
loss13 = c(d13$losses) # metabolic losses 
effic13 = c(d13$efficiencies) # efficiencies 

flux13 = fluxing(mat.13, biom13, loss13, effic13, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux13.stability = flux13
flux13 = as_tibble(flux13)

colnames = colnames(flux13)
colnames

flux13$prey = colnames
flux13
flux13 = column_to_rownames(flux13, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux13)) 
fluxsums$fluxfrom = rownames(flux13)
fluxfrom13 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom13 = mutate(fluxfrom13, year = 2013)
outgoing13 = fluxfrom13 %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing13

# Make growth rate for zoops (acting as basal in this web) 
growth.13 = d13 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.13
growth.13[growth.13$spp == 'cisco' | growth.13$spp == 'lake.trout' | growth.13$spp == 'walleye' | 
            growth.13$spp == 'chaoborus.larvae' | 
            growth.13$spp == 'leptodora' | growth.13$spp == 'mysis', 'growth.rate'] <- NA 
growth.13

basal.gr = growth.13$growth.rate
basal.gr

# Calculate stability # 
stab13 = stability.value(flux13.stability, biom13, loss13, effic13, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab13

stab13 = data.frame(stab13) 
stab13 = stab13 %>% 
  rename(stability = stab13) %>% 
  mutate(year = 2013)
stab13

##2014
d14 = trout_pelagic.web %>% filter(year == 2014) %>% filter(spp != 'lake.trout' & spp != 'walleye')
d14

mat.14 = early.mat[-c(2:3),-c(2:3)]
mat.14

biom14 = c(d14$biomass.g_perhec) # biomass # 
loss14 = c(d14$losses) # metabolic losses 
effic14 = c(d14$efficiencies) # efficiencies 

flux14 = fluxing(mat.14, biom14, loss14, effic14, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux14.stability = flux14
flux14 = as_tibble(flux14)

colnames = colnames(flux14)
colnames

flux14$prey = colnames
flux14
flux14 = column_to_rownames(flux14, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux14)) 
fluxsums$fluxfrom = rownames(flux14)
fluxfrom14 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom14 = mutate(fluxfrom14, year = 2014)
outgoing14 = fluxfrom14 %>% 
  mutate(lake.trout = NA, 
         walleye = NA) %>% 
  select(year, cisco, lake.trout, walleye, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing14

# Make growth rate for zoops (acting as basal in this web) 
growth.14 = d14 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.14
growth.14[growth.14$spp == 'cisco' | growth.14$spp == 'lake.trout' | growth.14$spp == 'walleye' | 
            growth.14$spp == 'chaoborus.larvae' | 
            growth.14$spp == 'leptodora' | growth.14$spp == 'mysis', 'growth.rate'] <- NA 
growth.14

basal.gr = growth.14$growth.rate
basal.gr

# Calculate stability # 
stab14 = stability.value(flux14.stability, biom14, loss14, effic14, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab14

stab14 = data.frame(stab14) 
stab14 = stab14 %>% 
  rename(stability = stab14) %>% 
  mutate(year = 2014)
stab14

## 2015 
d15 = trout_pelagic.web %>% filter(year == 2015) 
d15

mat.15 = late.mat
mat.15

biom15 = c(d15$biomass.g_perhec) # biomass # 
loss15 = c(d15$losses) # metabolic losses 
effic15 = c(d15$efficiencies) # efficiencies 

flux15 = fluxing(mat.15, biom15, loss15, effic15, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux15.stability = flux15
flux15 = as_tibble(flux15)

colnames = colnames(flux15)
colnames

flux15$prey = colnames
flux15
flux15 = column_to_rownames(flux15, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux15)) 
fluxsums$fluxfrom = rownames(flux15)
fluxfrom15 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom15 = mutate(fluxfrom15, year = 2015)
outgoing15 = fluxfrom15 %>% 
  select(year, cisco, lake.trout, walleye, bythotrephes, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing15

# Make growth rate for zoops (acting as basal in this web) 
growth.15 = d15 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.15
growth.15[growth.15$spp == 'cisco' | growth.15$spp == 'lake.trout' | growth.15$spp == 'walleye' | 
            growth.15$spp == 'bythotrephes' | growth.15$spp == 'chaoborus.larvae' | 
            growth.15$spp == 'leptodora' | growth.15$spp == 'mysis', 'growth.rate'] <- NA 
growth.15

basal.gr = growth.15$growth.rate
basal.gr

# Calculate stability # 
stab15 = stability.value(flux15.stability, biom15, loss15, effic15, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab15

stab15 = data.frame(stab15) 
stab15 = stab15 %>% 
  rename(stability = stab15) %>% 
  mutate(year = 2015)
stab15

## 2016 
d16 = trout_pelagic.web %>% filter(year == 2016) %>% filter(spp != 'lake.trout' & spp != 'walleye')
d16

mat.16 = late.mat[-c(2:3),-c(2:3)]
mat.16

biom16 = c(d16$biomass.g_perhec) # biomass # 
loss16 = c(d16$losses) # metabolic losses 
effic16 = c(d16$efficiencies) # efficiencies 

flux16 = fluxing(mat.16, biom16, loss16, effic16, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux16.stability = flux16
flux16 = as_tibble(flux16)

colnames = colnames(flux16)
colnames

flux16$prey = colnames
flux16
flux16 = column_to_rownames(flux16, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux16)) 
fluxsums$fluxfrom = rownames(flux16)
fluxfrom16 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom16 = mutate(fluxfrom16, year = 2016)
outgoing16 = fluxfrom16 %>% 
  mutate(lake.trout = NA, 
         walleye = NA) %>% 
  select(year, cisco, lake.trout, walleye, bythotrephes, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing16

# Make growth rate for zoops (acting as basal in this web) 
growth.16 = d16 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.16
growth.16[growth.16$spp == 'cisco' | growth.16$spp == 'lake.trout' | growth.16$spp == 'walleye' | 
            growth.16$spp == 'bythotrephes' | growth.16$spp == 'chaoborus.larvae' | 
            growth.16$spp == 'leptodora' | growth.16$spp == 'mysis', 'growth.rate'] <- NA 
growth.16

basal.gr = growth.16$growth.rate
basal.gr

# Calculate stability # 
stab16 = stability.value(flux16.stability, biom16, loss16, effic16, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab16

stab16 = data.frame(stab16) 
stab16 = stab16 %>% 
  rename(stability = stab16) %>% 
  mutate(year = 2016)
stab16

## 2017 
d17 = trout_pelagic.web %>% filter(year == 2017) 
d17

mat.17 = late.mat
mat.17

biom17 = c(d17$biomass.g_perhec) # biomass # 
loss17 = c(d17$losses) # metabolic losses 
effic17 = c(d17$efficiencies) # efficiencies 

flux17 = fluxing(mat.17, biom17, loss17, effic17, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux17.stability = flux17
flux17 = as_tibble(flux17)

colnames = colnames(flux17)
colnames

flux17$prey = colnames
flux17
flux17 = column_to_rownames(flux17, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux17)) 
fluxsums$fluxfrom = rownames(flux17)
fluxfrom17 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom17 = mutate(fluxfrom17, year = 2017)
outgoing17 = fluxfrom17 %>% 
  select(year, cisco, lake.trout, walleye, bythotrephes, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing17

# Make growth rate for zoops (acting as basal in this web) 
growth.17 = d17 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.17
growth.17[growth.17$spp == 'cisco' | growth.17$spp == 'lake.trout' | growth.17$spp == 'walleye' | 
            growth.17$spp == 'bythotrephes' | growth.17$spp == 'chaoborus.larvae' | 
            growth.17$spp == 'leptodora' | growth.17$spp == 'mysis', 'growth.rate'] <- NA 
growth.17

basal.gr = growth.17$growth.rate
basal.gr

# Calculate stability # 
stab17 = stability.value(flux17.stability, biom17, loss17, effic17, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab17

stab17 = data.frame(stab17) 
stab17 = stab17 %>% 
  rename(stability = stab17) %>% 
  mutate(year = 2017)
stab17


## 2018 
d18 = trout_pelagic.web %>% filter(year == 2018) %>% filter(spp != 'lake.trout')
d18

mat.18 = late.mat[-c(2),-c(2)]
mat.18

biom18 = c(d18$biomass.g_perhec) # biomass # 
loss18 = c(d18$losses) # metabolic losses 
effic18 = c(d18$efficiencies) # efficiencies 

flux18 = fluxing(mat.18, biom18, loss18, effic18, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux18.stability = flux18
flux18 = as_tibble(flux18)

colnames = colnames(flux18)
colnames

flux18$prey = colnames
flux18
flux18 = column_to_rownames(flux18, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux18)) 
fluxsums$fluxfrom = rownames(flux18)
fluxfrom18 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom18 = mutate(fluxfrom18, year = 2018)
outgoing18 = fluxfrom18 %>% 
  mutate(lake.trout = NA) %>% 
  select(year, cisco, lake.trout, walleye, bythotrephes, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing18

# Make growth rate for zoops (acting as basal in this web) 
growth.18 = d18 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.18
growth.18[growth.18$spp == 'cisco' | growth.18$spp == 'lake.trout' | growth.18$spp == 'walleye' | 
            growth.18$spp == 'bythotrephes' | growth.18$spp == 'chaoborus.larvae' | 
            growth.18$spp == 'leptodora' | growth.18$spp == 'mysis', 'growth.rate'] <- NA 
growth.18

basal.gr = growth.18$growth.rate
basal.gr

# Calculate stability # 
stab18 = stability.value(flux18.stability, biom18, loss18, effic18, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab18

stab18 = data.frame(stab18) 
stab18 = stab18 %>% 
  rename(stability = stab18) %>% 
  mutate(year = 2018)
stab18

## 2019 
d19 = trout_pelagic.web %>% filter(year == 2019) %>% filter(spp != 'walleye')
d19

mat.19 = late.mat[-c(3),-c(3)]
mat.19

biom19 = c(d19$biomass.g_perhec) # biomass # 
loss19 = c(d19$losses) # metabolic losses 
effic19 = c(d19$efficiencies) # efficiencies 

flux19 = fluxing(mat.19, biom19, loss19, effic19, 
                 bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
                 bioms.losses = TRUE, # metabolic loss defined per unit biomass 
                 ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 

flux19.stability = flux19
flux19 = as_tibble(flux19)

colnames = colnames(flux19)
colnames

flux19$prey = colnames
flux19
flux19 = column_to_rownames(flux19, var = 'prey')

# Outgoing flux # 
fluxsums = as_tibble(rowSums(flux19)) 
fluxsums$fluxfrom = rownames(flux19)
fluxfrom19 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
fluxfrom19 = mutate(fluxfrom19, year = 2019)
outgoing19 = fluxfrom19 %>% 
  mutate(walleye = NA) %>% 
  select(year, cisco, lake.trout, walleye, bythotrephes, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
outgoing19

# Make growth rate for zoops (acting as basal in this web) 
growth.19 = d19 %>% 
  mutate(growth.rate = 0.71*bodymass_g^(-0.25))
growth.19
growth.19[growth.19$spp == 'cisco' | growth.19$spp == 'lake.trout' | growth.19$spp == 'walleye' | 
            growth.19$spp == 'bythotrephes' | growth.19$spp == 'chaoborus.larvae' | 
            growth.19$spp == 'leptodora' | growth.19$spp == 'mysis', 'growth.rate'] <- NA 
growth.19

basal.gr = growth.19$growth.rate
basal.gr

# Calculate stability # 
stab19 = stability.value(flux19.stability, biom19, loss19, effic19, basal.gr, 
                         bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
stab19

stab19 = data.frame(stab19) 
stab19 = stab19 %>% 
  rename(stability = stab19) %>% 
  mutate(year = 2019)
stab19


# ## 2020 ==

## No zooplankton data for 2020 :( ## 

# d20 = trout_pelagic.web %>% filter(year == 2020) %>% filter(spp != 'lake.trout')
# d20
# 
# mat.20 = late.mat[-c(2),-c(2)]
# mat.20
# 
# biom20 = c(d20$biomass.g_perhec) # biomass # 
# loss20 = c(d20$losses) # metabolic losses 
# effic20 = c(d20$efficiencies) # efficiencies 
# 
# flux20 = fluxing(mat.20, biom20, loss20, effic20, 
#                  bioms.prefs = TRUE, # scale species diet preferences to biomass of their prey 
#                  bioms.losses = TRUE, # metabolic loss defined per unit biomass 
#                  ef.level = 'prey') # efficiency defined according to prey (efficiency by which it will be assimilated) 
# 
# flux20.stability = flux20
# flux20 = as_tibble(flux20)
# 
# colnames = colnames(flux20)
# colnames
# 
# flux20$prey = colnames
# flux20
# flux20 = column_to_rownames(flux20, var = 'prey')
# 
# # Outgoing flux # 
# fluxsums = as_tibble(rowSums(flux20)) 
# fluxsums$fluxfrom = rownames(flux20)
# fluxfrom20 = pivot_wider(fluxsums, names_from = 'fluxfrom', values_from = 'value')
# fluxfrom20 = mutate(fluxfrom20, year = 2020)
# outgoing20 = fluxfrom20 %>% 
#   mutate(lake.trout = NA) %>% 
#   select(year, cisco, lake.trout, walleye, bythotrephes, chaoborus.larvae, leptodora, mysis, cladocera, copepoda, rotifera)
# outgoing20
# 
# # Make growth rate for zoops (acting as basal in this web) 
# growth.20 = d20 %>% 
#   mutate(growth.rate = 0.71*bodymass_g^(-0.25))
# growth.20
# growth.20[growth.20$spp == 'cisco' | growth.20$spp == 'lake.trout' | growth.20$spp == 'walleye' | 
#             growth.20$spp == 'bythotrephes' | growth.20$spp == 'chaoborus.larvae' | 
#             growth.20$spp == 'leptodora' | growth.20$spp == 'mysis', 'growth.rate'] <- NA 
# growth.20
# 
# basal.gr = growth.20$growth.rate
# basal.gr
# 
# # Calculate stability # 
# stab20 = stability.value(flux20.stability, biom20, loss20, effic20, basal.gr, 
#                          bioms.prefs = T, bioms.losses = T, ef.level = 'prey')
# stab20
# 
# stab20 = data.frame(stab20) 
# stab20 = stab20 %>% 
#   rename(stability = stab20) %>% 
#   mutate(year = 2020)
# stab20

# Base Dataset Fluxes in J/year #===========================
outgoing.flux_final = bind_rows(outgoing01, outgoing02, outgoing03, outgoing04, outgoing05, outgoing06, outgoing07, 
                                outgoing08, outgoing09, outgoing10, outgoing11, outgoing12, outgoing13, outgoing14, 
                                outgoing15, outgoing16, outgoing17, outgoing18, outgoing19)

stability.estimate_final = bind_rows(stab01, stab02, stab03, stab04, stab05, stab06, stab07, stab08, stab09,
                                     stab10, stab11, stab12, stab13, stab14, 
                                     stab15, stab16, stab17, stab18, stab19)  

outgoing.flux.long = outgoing.flux_final %>% 
  pivot_longer(cols = !c(year), 
               names_to = 'taxa', 
               values_to = 'flux_J_yr') %>% 
  drop_na() %>%
  mutate(flux_J_month = flux_J_yr/12) %>% 
  mutate(group = case_when(.$taxa %in% c('cisco') ~ 'piscivory', 
                           .$taxa %in% c('cladocera', 'rotifera', 'copepoda') ~ 'zooplanktivory', 
                           .$taxa %in% c('chaoborus.larvae', 'leptodora', 'mysis') ~ 'pelagic.zoobenthivory',
                           .$taxa %in% 'bythotrephes' ~ 'bythotrephes'))
outgoing.flux.long



# Piscivory #
piscivory.flux = outgoing.flux.long %>%
  filter(group == 'piscivory')
piscivory.flux[piscivory.flux$flux_J_yr == 0, 'flux_J_yr'] <- NA
piscivory.flux[piscivory.flux$flux_J_month == 0, 'flux_J_month'] <- NA
piscivory.flux

# Zooplanktivory # 
zooplanktivory.flux = outgoing.flux.long %>%
  filter(group == 'zooplanktivory') %>% 
  group_by(year) %>% 
  summarize(flux_yr = sum(flux_J_yr), 
            flux_month = sum(flux_J_month)) %>%
  ungroup()
zooplanktivory.flux


#pelagic zoobenthivory # 
pelagic.zoobenthivory.flux = outgoing.flux.long %>%
  filter(group == 'pelagic.zoobenthivory') %>% 
  group_by(year) %>% 
  summarize(flux_yr = sum(flux_J_yr), 
            flux_month = sum(flux_J_month)) %>%
  ungroup()
pelagic.zoobenthivory.flux

# bythotrephes flux # 
byth.flux = outgoing.flux.long %>% 
  filter(group == 'bythotrephes') %>% 
  group_by(year) %>% 
  summarize(flux_yr = sum(flux_J_yr), 
            flux_month = sum(flux_J_month)) %>%
  ungroup()
byth.flux


# Total energy flux # 
total.flux = outgoing.flux.long %>% 
  group_by(year) %>% 
  summarize(total.yr = sum(flux_J_yr), 
            total.month = sum(flux_J_month)) %>% 
  ungroup()
total.flux

# stability # ==============
stability.estimate_final


# Plotting # ====================
windows(height = 4, width = 6)
par( mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Piscivory # 
plot(log(flux_J_yr)~year, type = 'o', pch = 19, lwd =2, col = '#99000d', ylim = c(log(90), log(2000000)), yaxt = 'n', 
     data = piscivory.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
axis(side=2,
     at=c(log(90),
          log(100),log(200),log(300),log(400),log(500),log(600),log(700),log(800),log(900),log(1000),
          log(2000),log(3000),log(4000),log(5000),log(6000),log(7000),log(8000),log(9000),log(10000), 
          log(20000),log(30000),log(40000),log(50000),log(60000),log(70000),log(80000),log(90000), log(100000),
          log(200000),log(300000),log(400000),log(500000),log(600000),log(700000),log(800000),log(900000), 
          log(1000000), log(2000000)), #Where the tick marks should be drawn
     labels = c('90', '100', '', '','','','','','','','1000',
                '', '','','','','','','','10000',
                '', '','','','','','','','100000',
                '', '','','','','','','','1000000', '2000000'),
     las=0)

# Piscivory - no walleye correction # 

#For uncorrected - must run base fluxes first # 
#points(log(flux_J_yr)~ year, data = piscivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray30', xlab = '', ylab = '', lty = 2)

# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Piscovry Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2, cex = 1)
mtext(expression(`Log[Energy` ~ Flux ~ `(J`~ha^-1~year^-1~`)]`), side = 2, line = 2)

# Zooplanktivory # 
points(log(flux_yr)~year, type = 'o', pch = 19, lwd =2, col = '#ef3b2c',
     data = zooplanktivory.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# mtext('Zooplanktivory Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first # 
#points(log(flux_yr) ~ year, data = zooplanktivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray50', xlab = '', ylab = '', lty = 2)


# Pelagic Zoobenthivory Flux # 
points(log(flux_yr)~year, type = 'o', pch = 19, lwd =2, col = '#fc9272',
     data = pelagic.zoobenthivory.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Macroinvertebrate Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first # 
#points(log(flux_yr) ~ year, data = pelagic.zoobenthivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray70', xlab = '', ylab = '', lty = 2)


# Bythotrephes -> Cisco Flux # 
points(log(flux_yr)~year, type = 'o', pch = 19, lwd =2, col = '#fdd0a2',
     data = byth.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2)
# abline(v = 2014, lty = 2)
# mtext('Bythotrephes Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first 
#points(log(flux_yr) ~ year, data = byth.flux, type = 'o', pch = 19, lwd = 1, col = 'black', xlab = '', ylab = '', lty = 2)


legend('bottomleft', legend = c('Piscivory Flux', 'Zooplanktivory Flux', 'Zoobenthivory Flux',
                                'Bythorephes Flux'), pch = 19, col = c('#99000d','#ef3b2c', '#fc9272', '#fdd0a2'),
       bty = 'n', cex = 1)
lines(c(2006.5,2006.5), c(log(1000), log(3000000)), lty = 2)

# Piscivory Flux legend without correction # 
# windows(height=5, width=5)
# par(mai=c(0.9,1,0.6,1))
# plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
# legend('center', legend = c('Piscivory (uncorr.)', 'Zooplanktivory (uncorr.)',
#                             'Zoobenthivory (uncorr.)', 'Bythotrephes (uncorr.)'), 
#        pch=19, bty='n',
#        pt.cex=1, cex=0.8,
#        col = c("gray30", 'gray50', 'gray70', 'black'))

# abline(v = 2006.5, lty = 2)
# abline(h = log(1000))
abline(v = 2014.5, lty = 2)

# Total Flux 
# points(log(total.yr)~year, type = 'o', pch = 19, col = 'black', lwd = 2.5, 
#      data = total.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Total Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# Stability Plot # ==============================
windows(height = 5, width = 6)
plot(stability~year, type = 'o', pch = 19, lwd = 2, data = stability.estimate_final,ylim = c(-3, 300), xlim = c(2001,2019), xlab = '', ylab = '')
mtext('Stability', side = 2, line = 2.8, cex = 1)
mtext('Year', side = 1, line = 2.8, cex = 1)

abline(h=0)
abline(v = 2006.5, lty = 2) 
abline(v = 2014.5, lty = 2)
text(2018, -6, 'Stable', font = 3)
text(2018, 10, 'Unstable', font = 3)
