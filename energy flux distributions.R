# Distribution of energy fluxes + Decomposed Fluxes # 

# Make sure correct fluxing dataset has been run from 'flux_trout' script before running the following
# trout_pelagic.web is the dataset the fluxing code comes from 
# Datasets # 
  # base = no manipulation 
  # base.wae_corrected = take 20% of walleye biomass to correct for littoral feeding
  # hifish.lowzpmiv = 20% of walleye biomass; double fish; half zoop and miv
  # lowfish.hizpmiv = 20% of walleye biomass; half fish; double zoop-miv

## 2001 #=====================
f01 = as.data.frame(flux01)
flux01

# add a column for the prey 
f01 = f01 %>% mutate(prey = rownames(f01))
f01

# Reshape to a long format 
f01.long = f01 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux01') %>% 
  select(predator, prey, flux01)
f01.long

# combine predator prey columns into one column 
f01.long = f01.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux01 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2001)
f01.long

d01 = density(f01.long$flux01)

## 2002 #=====================
f02 = as.data.frame(flux02)
flux02

# add a column for the prey 
f02 = f02 %>% mutate(prey = rownames(f02))
f02

# Reshape to a long format 
f02.long = f02 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux02') %>% 
  select(predator, prey, flux02)
f02.long

# combine predator prey columns into one column 
f02.long = f02.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux02 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2002)
f02.long

d02 = density(f02.long$flux02)

## 2003 #=====================
f03 = as.data.frame(flux03)
flux03

# add a column for the prey 
f03 = f03 %>% mutate(prey = rownames(f03))
f03

# Reshape to a long format 
f03.long = f03 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux03') %>% 
  select(predator, prey, flux03)
f03.long

# combine predator prey columns into one column 
f03.long = f03.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux03 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2003)
f03.long

d03 = density(f03.long$flux03)

## 2004 #======================
f04 = as.data.frame(flux04)
flux04

# add a column for the prey 
f04 = f04 %>% mutate(prey = rownames(f04))
f04

# Reshape to a long format 
f04.long = f04 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux04') %>% 
  select(predator, prey, flux04)
f04.long

# combine predator prey columns into one column 
f04.long = f04.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux04 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2004)
f04.long

d04 = density(f04.long$flux04)

## 2005 #====================
f05 = as.data.frame(flux05)
flux05

# add a column for the prey 
f05 = f05 %>% mutate(prey = rownames(f05))
f05

# Reshape to a long format 
f05.long = f05 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux05') %>% 
  select(predator, prey, flux05)
f05.long

# combine predator prey columns into one column 
f05.long = f05.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux05 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2005)
f05.long

d05 = density(f05.long$flux05)

## 2006 #====================
f06 = as.data.frame(flux06)
flux06

# add a column for the prey 
f06 = f06 %>% mutate(prey = rownames(f06))
f06

# Reshape to a long format 
f06.long = f06 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux06') %>% 
  select(predator, prey, flux06)
f06.long

# combine predator prey columns into one column 
f06.long = f06.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux06 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2006)
f06.long

d06 = density(f06.long$flux06)

# Plotting up the first period # =================================
# Food Web Period 1 # 
p1a = rgb(167, 118, 0, max = 255, alpha = 255)
p1b = rgb(167, 118, 0, max = 255, alpha = 235) 
p1c = rgb(167, 118, 0, max = 255, alpha = 215) 
p1d = rgb(167, 118, 0, max = 255, alpha = 195)
p1e = rgb(167, 118, 0, max = 255, alpha = 175)
p1f = rgb(167, 118, 0, max = 255, alpha = 155)



plot(d01, ylim = c(0, .0001), col = 'white')
points(d01, type = 'l', lwd = 2, col = p1a)
points(d02, type = 'l', lwd = 2, col = p1b)
points(d03, type = 'l', lwd = 2, col = p1c)
points(d04, type = 'l', lwd = 2, col = p1d)
points(d05, type = 'l', lwd = 2, col = p1e)
points(d06, type = 'l', lwd = 2, col = p1f)

## 2007 #====================
f07 = as.data.frame(flux07)
flux07

# add a column for the prey 
f07 = f07 %>% mutate(prey = rownames(f07))
f07

# Reshape to a long format 
f07.long = f07 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux07') %>% 
  select(predator, prey, flux07)
f07.long

# combine predator prey columns into one column 
f07.long = f07.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux07 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2007)
f07.long

d07 = density(f07.long$flux07)

## 2008 #====================
f08 = as.data.frame(flux08)
flux08

# add a column for the prey 
f08 = f08 %>% mutate(prey = rownames(f08))
f08

# Reshape to a long format 
f08.long = f08 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux08') %>% 
  select(predator, prey, flux08)
f08.long

# combine predator prey columns into one column 
f08.long = f08.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux08 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2008)
f08.long

d08 = density(f08.long$flux08)

## 2009 #====================
f09 = as.data.frame(flux09)
flux09

# add a column for the prey 
f09 = f09 %>% mutate(prey = rownames(f09))
f09

# Reshape to a long format 
f09.long = f09 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux09') %>% 
  select(predator, prey, flux09)
f09.long

# combine predator prey columns into one column 
f09.long = f09.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux09 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2009)
f09.long

d09 = density(f09.long$flux09)

## 2010 #====================
f10 = as.data.frame(flux10)
flux10

# add a column for the prey 
f10 = f10 %>% mutate(prey = rownames(f10))
f10

# Reshape to a long format 
f10.long = f10 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux10') %>% 
  select(predator, prey, flux10)
f10.long

# combine predator prey columns into one column 
f10.long = f10.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux10 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2010)
f10.long

d10 = density(f10.long$flux10)

## 2011 #====================
f11 = as.data.frame(flux11)
flux11

# add a column for the prey 
f11 = f11 %>% mutate(prey = rownames(f11))
f11

# Reshape to a long format 
f11.long = f11 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux11') %>% 
  select(predator, prey, flux11)
f11.long

# combine predator prey columns into one column 
f11.long = f11.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux11 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2011)
f11.long

d11 = density(f11.long$flux11)

## 2012 #====================
f12 = as.data.frame(flux12)
flux12

# add a column for the prey 
f12 = f12 %>% mutate(prey = rownames(f12))
f12

# Reshape to a long format 
f12.long = f12 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux12') %>% 
  select(predator, prey, flux12)
f12.long

# combine predator prey columns into one column 
f12.long = f12.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux12 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2012)
f12.long

d12 = density(f12.long$flux12)

## 2013 #====================
f13 = as.data.frame(flux13)
flux13

# add a column for the prey 
f13 = f13 %>% mutate(prey = rownames(f13))
f13

# Reshape to a long format 
f13.long = f13 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux13') %>% 
  select(predator, prey, flux13)
f13.long

# combine predator prey columns into one column 
f13.long = f13.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux13 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2013)
f13.long

d13 = density(f13.long$flux13)

## 2014 #====================
f14 = as.data.frame(flux14)
flux14

# add a column for the prey 
f14 = f14 %>% mutate(prey = rownames(f14))
f14

# Reshape to a long format 
f14.long = f14 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux14') %>% 
  select(predator, prey, flux14)
f14.long

# combine predator prey columns into one column 
f14.long = f14.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux14 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2014)
f14.long

d14 = density(f14.long$flux14)

# Plotting up the second period # =================================

# Food Web Period 2 # 
p2a = rgb(167, 91, 81, max = 255, alpha = 255)
p2b = rgb(167, 91, 81, max = 255, alpha = 235) 
p2c = rgb(167, 91, 81, max = 255, alpha = 215) 
p2d = rgb(167, 91, 81, max = 255, alpha = 195)
p2e = rgb(167, 91, 81, max = 255, alpha = 175)
p2f = rgb(167, 91, 81, max = 255, alpha = 155)
p2g = rgb(167, 91, 81, max = 255, alpha = 135)
p2h = rgb(167, 91, 81, max = 255, alpha = 115)


plot(d01, ylim = c(0, .0001), col = 'white')
points(d07, type = 'l', lwd = 2, col = p2a)
points(d08, type = 'l', lwd = 2, col = p2b)
points(d09, type = 'l', lwd = 2, col = p2c)
points(d10, type = 'l', lwd = 2, col = p2d)
points(d11, type = 'l', lwd = 2, col = p2e)
points(d12, type = 'l', lwd = 2, col = p2f)
points(d13, type = 'l', lwd = 2, col = p2e)
points(d14, type = 'l', lwd = 2, col = p2f)


# 2015 # ============================
f15 = as.data.frame(flux15)
f15

# add a column for the prey 
f15 = f15 %>% mutate(prey = rownames(f15))
f15

# Reshape to a long format 
f15.long = f15 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux15') %>% 
  select(predator, prey, flux15)
f15.long

# combine predator prey columns into one column 
f15.long = f15.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux15 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2015)
f15.long

d15 = density(f15.long$flux15)

# 2016 # ============================
f16 = as.data.frame(flux16)
f16

# add a column for the prey 
f16 = f16 %>% mutate(prey = rownames(f16))
f16

# Reshape to a long format 
f16.long = f16 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux16') %>% 
  select(predator, prey, flux16)
f16.long

# combine predator prey columns into one column 
f16.long = f16.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux16 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2016)
f16.long

d16 = density(f16.long$flux16)

# 2017 # ============================
f17 = as.data.frame(flux17)
f17

# add a column for the prey 
f17 = f17 %>% mutate(prey = rownames(f17))
f17

# Reshape to a long format 
f17.long = f17 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux17') %>% 
  select(predator, prey, flux17)
f17.long

# combine predator prey columns into one column 
f17.long = f17.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux17 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2017)
f17.long

d17 = density(f17.long$flux17)

# 2018 # ============================
f18 = as.data.frame(flux18)
f18

# add a column for the prey 
f18 = f18 %>% mutate(prey = rownames(f18))
f18

# Reshape to a long format 
f18.long = f18 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux18') %>% 
  select(predator, prey, flux18)
f18.long

# combine predator prey columns into one column 
f18.long = f18.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux18 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2018)
f18.long

d18 = density(f18.long$flux18)

# 2019 # ============================
f19 = as.data.frame(flux19)
f19

# add a column for the prey 
f19 = f19 %>% mutate(prey = rownames(f19))
f19

# Reshape to a long format 
f19.long = f19 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux19') %>% 
  select(predator, prey, flux19)
f19.long

# combine predator prey columns into one column 
f19.long = f19.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux19 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2019)
f19.long

d19 = density(f19.long$flux19)

# 2020 # ============================
f20 = as.data.frame(flux20)
f20

# add a column for the prey 
f20 = f20 %>% mutate(prey = rownames(f20))
f20

# Reshape to a long format 
f20.long = f20 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux20') %>% 
  select(predator, prey, flux20)
f20.long

# combine predator prey columns into one column 
f20.long = f20.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux20 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2020)
f20.long

d20 = density(f20.long$flux20)

# 2021 # ============================
f21 = as.data.frame(flux21)
f21

# add a column for the prey 
f21 = f21 %>% mutate(prey = rownames(f21))
f21

# Reshape to a long format 
f21.long = f21 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux21') %>% 
  select(predator, prey, flux21)
f21.long

# combine predator prey columns into one column 
f21.long = f21.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux21 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2021)
f21.long

d21 = density(f21.long$flux21)

# 2022 # ============================
f22 = as.data.frame(flux22)
f22

# add a column for the prey 
f22 = f22 %>% mutate(prey = rownames(f22))
f22

# Reshape to a long format 
f22.long = f22 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux22') %>% 
  select(predator, prey, flux22)
f22.long

# combine predator prey columns into one column 
f22.long = f22.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux22 != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2022)
f22.long

d22 = density(f22.long$flux22)

# Color Gradients # =======================

# Food Web Period 3 # 
p3a = rgb(205, 92, 5, max = 255, alpha = 255)
p3b = rgb(205, 92, 5, max = 255, alpha = 235) 
p3c = rgb(205, 92, 5, max = 255, alpha = 215) 
p3d = rgb(205, 92, 5, max = 255, alpha = 195)
p3e = rgb(205, 92, 5, max = 255, alpha = 175)
p3f = rgb(205, 92, 5, max = 255, alpha = 155)
p3g = rgb(205, 92, 5, max = 255, alpha = 135)


plot(d15, ylim = c(0, .0001), col = 'white')
points(d16, type = 'l', lwd = 2, col = p3a)
points(d17, type = 'l', lwd = 2, col = p3b)
points(d18, type = 'l', lwd = 2, col = p3c)
points(d19, type = 'l', lwd = 2, col = p3d)
points(d20, type = 'l', lwd = 2, col = p3e)
points(d21, type = 'l', lwd = 2, col = p3f)
points(d22, type = 'l', lwd = 2, col = p3g)


# 3 energy flux panel # 
windows(height = 8, width = 6)
par(mfrow =c(3,1), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


# Decomposed Fluxes plotting =========================================

# Combine all individual flux information into one dataset - focus on the predator_prey column 
## 2001 #=====================
f01 = as.data.frame(flux01)
flux01

# add a column for the prey 
f01 = f01 %>% mutate(prey = rownames(f01))
f01

# Reshape to a long format 
f01.long = f01 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f01.long

# combine predator prey columns into one column 
f01.long = f01.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2001)
f01.long

## 2002 #=====================
f02 = as.data.frame(flux02)
flux02

# add a column for the prey 
f02 = f02 %>% mutate(prey = rownames(f02))
f02

# Reshape to a long format 
f02.long = f02 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f02.long

# combine predator prey columns into one column 
f02.long = f02.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2002)
f02.long


## 2003 #=====================
f03 = as.data.frame(flux03)
flux03

# add a column for the prey 
f03 = f03 %>% mutate(prey = rownames(f03))
f03

# Reshape to a long format 
f03.long = f03 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f03.long

# combine predator prey columns into one column 
f03.long = f03.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2003)
f03.long

## 2004 #======================
f04 = as.data.frame(flux04)
flux04

# add a column for the prey 
f04 = f04 %>% mutate(prey = rownames(f04))
f04

# Reshape to a long format 
f04.long = f04 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f04.long

# combine predator prey columns into one column 
f04.long = f04.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2004)
f04.long


## 2005 #====================
f05 = as.data.frame(flux05)
flux05

# add a column for the prey 
f05 = f05 %>% mutate(prey = rownames(f05))
f05

# Reshape to a long format 
f05.long = f05 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f05.long

# combine predator prey columns into one column 
f05.long = f05.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2005)
f05.long


## 2006 #====================
f06 = as.data.frame(flux06)
flux06

# add a column for the prey 
f06 = f06 %>% mutate(prey = rownames(f06))
f06

# Reshape to a long format 
f06.long = f06 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f06.long

# combine predator prey columns into one column 
f06.long = f06.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2006)
f06.long



## 2007 #====================
f07 = as.data.frame(flux07)
flux07

# add a column for the prey 
f07 = f07 %>% mutate(prey = rownames(f07))
f07

# Reshape to a long format 
f07.long = f07 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f07.long

# combine predator prey columns into one column 
f07.long = f07.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2007)
f07.long

## 2008 #====================
f08 = as.data.frame(flux08)
flux08

# add a column for the prey 
f08 = f08 %>% mutate(prey = rownames(f08))
f08

# Reshape to a long format 
f08.long = f08 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f08.long

# combine predator prey columns into one column 
f08.long = f08.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2008)
f08.long


## 2009 #====================
f09 = as.data.frame(flux09)
flux09

# add a column for the prey 
f09 = f09 %>% mutate(prey = rownames(f09))
f09

# Reshape to a long format 
f09.long = f09 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f09.long

# combine predator prey columns into one column 
f09.long = f09.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2009)
f09.long


## 2010 #====================
f10 = as.data.frame(flux10)
flux10

# add a column for the prey 
f10 = f10 %>% mutate(prey = rownames(f10))
f10

# Reshape to a long format 
f10.long = f10 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f10.long

# combine predator prey columns into one column 
f10.long = f10.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2010)
f10.long

## 2011 #====================
f11 = as.data.frame(flux11)
flux11

# add a column for the prey 
f11 = f11 %>% mutate(prey = rownames(f11))
f11

# Reshape to a long format 
f11.long = f11 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f11.long

# combine predator prey columns into one column 
f11.long = f11.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2011)
f11.long

## 2012 #====================
f12 = as.data.frame(flux12)
flux12

# add a column for the prey 
f12 = f12 %>% mutate(prey = rownames(f12))
f12

# Reshape to a long format 
f12.long = f12 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f12.long

# combine predator prey columns into one column 
f12.long = f12.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2012)
f12.long

## 2013 #====================
f13 = as.data.frame(flux13)
flux13

# add a column for the prey 
f13 = f13 %>% mutate(prey = rownames(f13))
f13

# Reshape to a long format 
f13.long = f13 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f13.long

# combine predator prey columns into one column 
f13.long = f13.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2013)
f13.long


## 2014 #====================
f14 = as.data.frame(flux14)
flux14

# add a column for the prey 
f14 = f14 %>% mutate(prey = rownames(f14))
f14

# Reshape to a long format 
f14.long = f14 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f14.long

# combine predator prey columns into one column 
f14.long = f14.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2014)
f14.long


# 2015 # ============================
f15 = as.data.frame(flux15)
f15

# add a column for the prey 
f15 = f15 %>% mutate(prey = rownames(f15))
f15

# Reshape to a long format 
f15.long = f15 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f15.long

# combine predator prey columns into one column 
f15.long = f15.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2015)
f15.long


# 2016 # ============================
f16 = as.data.frame(flux16)
f16

# add a column for the prey 
f16 = f16 %>% mutate(prey = rownames(f16))
f16

# Reshape to a long format 
f16.long = f16 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f16.long

# combine predator prey columns into one column 
f16.long = f16.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2016)
f16.long



# 2017 # ============================
f17 = as.data.frame(flux17)
f17

# add a column for the prey 
f17 = f17 %>% mutate(prey = rownames(f17))
f17

# Reshape to a long format 
f17.long = f17 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f17.long

# combine predator prey columns into one column 
f17.long = f17.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2017)
f17.long


# 2018 # ============================
f18 = as.data.frame(flux18)
f18

# add a column for the prey 
f18 = f18 %>% mutate(prey = rownames(f18))
f18

# Reshape to a long format 
f18.long = f18 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f18.long

# combine predator prey columns into one column 
f18.long = f18.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2018)
f18.long


# 2019 # ============================
f19 = as.data.frame(flux19)
f19

# add a column for the prey 
f19 = f19 %>% mutate(prey = rownames(f19))
f19

# Reshape to a long format 
f19.long = f19 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f19.long

# combine predator prey columns into one column 
f19.long = f19.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2019)
f19.long


# 2020 # ============================
f20 = as.data.frame(flux20)
f20

# add a column for the prey 
f20 = f20 %>% mutate(prey = rownames(f20))
f20

# Reshape to a long format 
f20.long = f20 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f20.long

# combine predator prey columns into one column 
f20.long = f20.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2020)
f20.long


# 2021 # ============================
f21 = as.data.frame(flux21)
f21

# add a column for the prey 
f21 = f21 %>% mutate(prey = rownames(f21))
f21

# Reshape to a long format 
f21.long = f21 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f21.long

# combine predator prey columns into one column 
f21.long = f21.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2021)
f21.long


# 2022 # ============================
f22 = as.data.frame(flux22)
f22

# add a column for the prey 
f22 = f22 %>% mutate(prey = rownames(f22))
f22

# Reshape to a long format 
f22.long = f22 %>%
  pivot_longer(cols = -prey, names_to = 'predator', values_to = 'flux') %>% 
  select(predator, prey, flux)
f22.long

# combine predator prey columns into one column 
f22.long = f22.long %>%
  mutate(predator_prey = paste(predator, prey, sep = '-')) %>% 
  filter(predator != 'cladocera' | predator != 'copepoda' | predator != 'rotifera') %>% 
  filter(flux != 0) %>% # only capturing predator-prey fluxes aka links 
  mutate(year = 2022)
f22.long

flux_dat = rbind(f01.long, f02.long, f03.long, f04.long, f05.long, f06.long,
                 f07.long, f08.long, f09.long, f10.long, f11.long, f12.long, 
                 f13.long, f14.long, f15.long, f16.long, f17.long, f18.long, 
                 f19.long, f20.long, f21.long, f22.long)
flux_dat

# Sum fluxes by macrozooplankton for ease #========================= 
flux_dat_grouped = flux_dat %>% 
  mutate(prey_group = case_when(prey == 'chaoborus.larvae' | prey == 'leptodora' | 
                                  prey == 'mysis' ~ 'macrozooplankton',
                                prey == 'copepoda' | prey == 'cladocera' | prey == 'rotifera' ~ 'zooplankton', 
                                prey == 'cisco' ~ 'cisco', 
                                prey == 'bythotrephes' ~ 'swf')) %>%
  mutate(predator_group = case_when(predator == 'chaoborus.larvae' | predator == 'leptodora' | 
                                  predator == 'mysis' ~ 'macrozooplankton',
                                  predator == 'bythotrephes' ~ 'swf', 
                                  predator == 'cisco' ~ 'cisco', 
                                  predator == 'lake.trout' ~ 'lake.trout',
                                  predator == 'walleye' ~ 'walleye')) %>%
  mutate(predator_prey.grouped = paste(predator_group, prey_group, sep = '-')) %>%
  group_by(year, predator_prey.grouped) %>%
  summarize(flux = sum(flux))
flux_dat_grouped

## Plotting Decomposed Fluxes ## ====================================
unique(flux_dat_grouped$predator_prey.grouped)

# 7 fluxes (predator <- prey) # 
windows(height = 4, width = 6)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))



### Colors #========================================= 
unique(flux_dat_grouped$predator_prey.grouped)

csc.zp = '#A97E15'
csc.mzp = 'gray30'
csc.swf = '#CDC09F'
lt.csc = '#86383A'
lt.mzp = '#c78586'
wae.csc = '#DAC0C1'
mzp.zp = '#CD5C05'
swf.zp = rgb(205,92,5, maxColorValue = 255, alpha = 175)

transparent = rgb(255,255,255 ,maxColorValue = 255, alpha = 100)

### Cisco <- zp #===============================
flux_csc.zp = flux_dat_grouped %>%
  filter(predator_prey.grouped == 'cisco-zooplankton')
plot(log(flux)~year, type = 'o', pch = 19, lwd =2, col = csc.zp, ylim = c(log(0.01), log(1500000)), yaxt = 'n', 
     data = flux_csc.zp, xlim = c(2001, 2022), xaxt = 'n',  
     xlab = '', ylab = '', cex = 1.5, cex.lab = 2)
axis(side=1, 
     at = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
            2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022), 
     labels = c('', '2001', '', '2003', '', '2005', '', '2007', '', '2009', '', '2011',
                '', '2013', '', '2015', '', '2017', '', '2019', '', '2021', ''), 
     cex = 3, las = 2)
axis(side=2,
     at=c(log(0.01), log(0.02), log(0.03), log(0.04), log(0.05), log(0.06), log(0.07), log(0.08), log(0.09), 
       log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
       log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
       log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),
       log(10),log(20),log(30),log(40),log(50),log(60),log(70),log(80),
       log(100),log(200),log(300),log(400),log(500),log(600),log(700),log(800),log(900),log(1000),
       log(2000),log(3000),log(4000),log(5000),log(6000),log(7000),log(8000),log(9000),log(10000), 
       log(20000),log(30000),log(40000),log(50000),log(60000),log(70000),log(80000),log(90000), log(100000),
       log(200000),log(300000),log(400000),log(500000),log(600000),log(700000),log(800000),log(900000), 
       log(1000000), log(2000000)), #Where the tick marks should be drawn
     labels = F,
     # labels = c('100', '', '','','','','','','','1000',
     #            '', '','','','','','','','10000',
     #            '', '','','','','','','','100000',
     #            '', '','','','','','','','1000000', 
     #            '', '','','','','','','','10000000',
     #            '', '','','','','','','','100000000', '200000000', '300000000'),
     las=0, cex = 3)

mtext(expression(`Log[Energy` ~ Flux~`]`) , side = 2, line = 3, cex = 1.1)
mtext(expression(`(kJ`~ha^-1~d^-1~`)]`), side = 2, line = 1.5, cex = 1.1)
mtext('Year', side = 1, cex = 1.1, line = 3)

### Cisco <- mzp #======================== 
flux_csc.mzp = flux_dat_grouped %>%
  filter(predator_prey.grouped == 'cisco-macrozooplankton')
points(log(flux)~year, type = 'o', pch = 19, lwd =2, col = csc.mzp, ylim = c(log(0.01), log(1500000)), yaxt = 'n', 
     data = flux_csc.mzp, xlim = c(2001, 2022), xaxt = 'n',  
     xlab = '', ylab = '', cex = 1.5, cex.lab = 2)

### Cisco <- swf #=========================
flux_csc.swf = flux_dat_grouped %>%
  filter(predator_prey.grouped == 'cisco-swf')


points(log(flux)~year, type = 'o', pch = 19, lwd =2, col = csc.swf, ylim = c(log(0.01), log(1500000)), yaxt = 'n', 
     data = flux_csc.swf, xlim = c(2001, 2022), xaxt = 'n',  
     xlab = '', ylab = '', cex = 1.5, cex.lab = 2)

### Lake Trout <- Cisco #===========================
flux_lt.csc = flux_dat_grouped %>%
  filter(predator_prey.grouped == 'lake.trout-cisco')
flux_lt.csc

points(log(flux)~year, type = 'o', pch = 15, lwd =2, col = lt.csc, ylim = c(log(0.01), log(1500000)), yaxt = 'n', 
     data = flux_lt.csc, xlim = c(2001, 2022), xaxt = 'n',  
     xlab = '', ylab = '', cex = 1.5, cex.lab = 2)

### Lake Trout <- Macrozooplankton #============================
flux_lt.mzp = flux_dat_grouped %>%
  filter(predator_prey.grouped == 'lake.trout-macrozooplankton')
flux_lt.mzp

points(log(flux)~year, type = 'o', pch = 15, lwd =2, col = lt.mzp, ylim = c(log(0.01), log(1500000)), yaxt = 'n', 
     data = flux_lt.mzp, xlim = c(2001, 2022), xaxt = 'n',  
     xlab = '', ylab = '', cex = 1.5, cex.lab = 2)

### Walleye <- Cisco #=============================
flux_wae.csc = flux_dat_grouped %>%
  filter(predator_prey.grouped == 'walleye-cisco')
flux_wae.csc

points(log(flux)~year, type = 'o', pch = 17, lwd =2, col = wae.csc, ylim = c(log(0.01), log(1500000)), yaxt = 'n', 
     data = flux_wae.csc, xlim = c(2001, 2022), xaxt = 'n',  
     xlab = '', ylab = '', cex = 1.5, cex.lab = 2)

### Macrozooplankton <- Zooplankton #=============================
flux_mzp.zp = flux_dat_grouped %>%
  filter(predator_prey.grouped == 'macrozooplankton-zooplankton')
flux_mzp.zp

points(log(flux)~year, type = 'o', pch = 8, lwd =2, col = mzp.zp, ylim = c(log(0.01), log(1500000)), yaxt = 'n', 
     data = flux_mzp.zp, xlim = c(2001, 2022), xaxt = 'n',  
     xlab = '', ylab = '', cex = 1.5, cex.lab = 2)

### Spiny Water Flea <- Zooplankton #===============================
flux_swf.zp = flux_dat_grouped %>%
  filter(predator_prey.grouped == 'swf-zooplankton')
flux_swf.zp

points(log(flux)~year, type = 'o', pch = 18, lwd =2, col = swf.zp, ylim = c(log(0.01), log(1500000)), yaxt = 'n', 
     data = flux_swf.zp, xlim = c(2001, 2022), xaxt = 'n',  
     xlab = '', ylab = '', cex = 1.5, cex.lab = 2)

# Stacked Bar of Fluxes #=============================

csc.zp = '#A97E15'
csc.mzp = '#B79749'
csc.swf = '#CDC09F'
lt.csc = '#86383A'
lt.mzp = '#c78586'
wae.csc = '#DAC0C1'
mzp.zp = '#CD5C05'
swf.zp = rgb(205,92,5, maxColorValue = 255, alpha = 175)

transparent = rgb(255,255,255 ,maxColorValue = 255, alpha = 100)

flux_dat_grouped

# Example # 
# library
library(ggplot2)

# Stacked
flux_dat_grouped$ln.flux = log(flux_dat_grouped$flux+1)
ggplot(flux_dat_grouped, aes(fill= predator_prey.grouped, y=flux, x=year))  +
    scale_fill_manual(values = c(csc.mzp, csc.swf, csc.zp, lt.csc,
                                 lt.mzp, mzp.zp, swf.zp, wae.csc)) + 
  ylab('Energy Flux (%)') + 
  xlab('Year') + 
  geom_bar(position='fill', stat='identity') + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

# Percent Stacked #

# Factor order predator-prey group # 
csc.zp = '#A97E15'
csc.mzp = 'gray30'
csc.swf = '#CDC09F'
lt.csc = '#86383A'
lt.mzp = '#c78586'
wae.csc = '#DAC0C1'
mzp.zp = '#CD5C05'
swf.zp = rgb(205,92,5, maxColorValue = 255, alpha = 175)

windows(height = 3, width = 6)
ggplot(flux_dat_grouped, aes(fill = predator_prey.grouped, y=flux, x=year)) +
    scale_fill_manual(values = c(csc.mzp, csc.swf, csc.zp, lt.csc,
                                 lt.mzp, mzp.zp, swf.zp, wae.csc)) + 
  ylab('Energy Flux (%)') + 
  xlab('Year') + 
 
  geom_bar(position='fill', stat='identity') + 
  geom_vline(xintercept = c(2006.5, 2014.5), 
             linewidth = 1.5, linetype = '23', lineend = 'round') + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
axis.text = element_text(color = 'black')) + 
  # ggtitle('Doubled fish biomass + Halved ZP & MIV biomass')
  # ggtitle('Halved fish biomass + Doubled ZP & MIV biomass')
flux_dat_grouped
