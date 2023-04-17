## Flux percent change & average ## ==========================

# Make sure to run biomass time series & flux scripts first # ===========
pmiv.final
zoop.grouped
fish.final

binary_flux
binary_stability

preference_flux
preference_stability

# Results summary stats # =======================
fish.final

# cisco biomass, 2001 - 2006 # 
d1 = fish.final %>% 
  filter(year > 2000 & year < 2007) %>% 
  select(year, cisco, c.se.u, c.se.l)
d1

mean(d1$cisco)
sd(d1$cisco)

# Walleye biomass, 2007 - 2014 # 
d2 = fish.final %>% 
  filter(year > 2006 & year < 2015) %>% 
  select(year, p20.walleye, w.se.u, w.se.l)
d2

mean(d2$p20.walleye, na.rm = T)
sd(d2$p20.walleye, na.rm = T)

# Walleye and lake trout percent change # 
fish.long = fish.final %>% 
  select(year, cisco, p20.walleye, lake.trout) %>%
  pivot_longer(cols = c(cisco, p20.walleye, lake.trout), 
               names_to = 'fish', 
               values_to = 'biomass_kgha')
fish.long

cisco.pc = fish.final %>%
  select(year, cisco) %>% 
  mutate(lg = lag(cisco)) %>% 
  mutate(pc = ((cisco - lg)/lg)*100)
cisco.pc 

walleye.pc = fish.final %>% 
  select(year, p20.walleye) %>%
  mutate(lg = lag(p20.walleye)) %>%
  mutate(pc = ((p20.walleye - lg)/lg)*100)
walleye.pc

((0.347 - 2.66)/2.66)*100

laketrout.pc = fish.final %>% 
  select(year, lake.trout) %>%
  mutate(lg = lag(lake.trout)) %>%
  mutate(pc = ((lake.trout - lg)/lg)*100)
laketrout.pc

# pelagic miv # 
pmiv.final

chaob.pc = pmiv.final %>% 
  select(year4, chaoborus) %>%
  mutate(lg = lag(chaoborus)) %>%
  mutate(pc = ((chaoborus - lg)/lg)*100)
chaob.pc

# 2012 - 2014 chaoborus 
((1.3 - 18.1)/18.1)*100

lept.pc = pmiv.final %>% 
  select(year4, leptodora) %>%
  mutate(lg = lag(leptodora)) %>%
  mutate(pc = ((leptodora - lg)/lg)*100)
lept.pc

((51.2 - 19)/19)*100

# zooplankton change - second period 
zoop.pc = zp.final %>% 
  select(year, cladocera, copepoda, rotifera) %>% 
  mutate(lg.clad = lag(cladocera), 
         lg.cope = lag(copepoda), 
         lg.rot = lag(rotifera)) %>%
  mutate(clad.pc = ((cladocera - lg.clad)/lg.clad)*100,
         cope.pc = ((copepoda - lg.cope)/lg.cope)*100,
         rote.pc = ((rotifera - lg.rot)/lg.rot)*100) %>%
  select(year, clad.pc, cope.pc, rote.pc)
zoop.pc

z.12 = c(-66.1, -68.8, -72.2)
mean(-66.1, -68.8, -72.2)
sd(z.12)

# Third period dynamics # 
pmiv.final

byth = pmiv.final %>% 
  select(year4, bythotrephes, bytho.se)
byth

# Fluxes # =============================

# Binary Flux # ================================
binary_flux.long = binary_flux %>% 
  pivot_longer(cols = !c(year), 
               names_to = 'taxa', 
               values_to = 'flux_J_s') %>% 
  mutate(flux_kJ_d = flux_J_s*86.4) %>% # Change to daily rate in kilojoules 
  drop_na() %>%
  mutate(group = case_when(.$taxa %in% c('cisco') ~ 'CSC_LT-WAE', 
                           .$taxa %in% c('cladocera', 'rotifera', 'copepoda') & .$year < 2015 ~ 'ZP_MIV-CSC', 
                           .$taxa %in% c('cladocera', 'rotifera', 'copepoda') & .$year > 2014 ~ 'ZP_MIV-CSC-SWF', 
                           .$taxa %in% c('chaoborus.larvae', 'leptodora', 'mysis') ~ 'MIV_CSC-LT',
                           .$taxa %in% 'bythotrephes' ~ 'SWF_CSC'))
binary_flux.long

# Cisco -> Lake Trout and Walleye #
flux.1.binary = binary_flux.long %>%
  filter(group == 'CSC_LT-WAE')
flux.1.binary[flux.1.binary$flux_kJ_d == 0, 'flux_kJ_d'] <- NA
flux.1.binary

# pMIV -> Lake Trout and Cisco # 
flux.2.binary = binary_flux.long %>%
  filter(group == 'MIV_CSC-LT') %>% 
  group_by(year) %>% 
  summarize(flux_kJ_d = sum(flux_kJ_d)) %>%
  ungroup()
flux.2.binary

# Zoop -> Cisco and pMIV # 
flux.3.binary = binary_flux.long %>%
  filter(group == 'ZP_MIV-CSC') %>% 
  group_by(year) %>% 
  summarize(flux_kJ_d = sum(flux_kJ_d)) %>%
  ungroup()
flux.3.binary

# Zoop -> Cisco and pMIV and spiny water flea # 
flux.4.binary = binary_flux.long %>%
  filter(group == 'ZP_MIV-CSC-SWF') %>% 
  group_by(year) %>% 
  summarize(flux_kJ_d = sum(flux_kJ_d)) %>%
  ungroup()
flux.4.binary

# spiny -> Cisco  # 
flux.5.binary = binary_flux.long %>% 
  filter(group == 'SWF_CSC') %>% 
  group_by(year) %>% 
  summarize(flux_kJ_d = sum(flux_kJ_d)) %>%
  ungroup()
flux.5.binary

# Total energy flux # 
total.flux.binary = binary_flux.long %>% 
  group_by(year) %>% 
  summarize(total = sum(flux_kJ_d)) %>% 
  ungroup()
total.flux.binary

# stability # 
binary_stability

# Preference Flux # ================================
preference_flux.long = preference_flux %>% 
  pivot_longer(cols = !c(year), 
               names_to = 'taxa', 
               values_to = 'flux_J_s') %>% 
  mutate(flux_kJ_d = flux_J_s*86.4) %>% # Change to daily rate in kilojoules 
  drop_na() %>%
  mutate(group = case_when(.$taxa %in% c('cisco') ~ 'CSC_LT-WAE', 
                           .$taxa %in% c('cladocera', 'rotifera', 'copepoda') & .$year < 2015 ~ 'ZP_MIV-CSC', 
                           .$taxa %in% c('cladocera', 'rotifera', 'copepoda') & .$year > 2014 ~ 'ZP_MIV-CSC-SWF', 
                           .$taxa %in% c('chaoborus.larvae', 'leptodora', 'mysis') ~ 'MIV_CSC-LT',
                           .$taxa %in% 'bythotrephes' ~ 'SWF_CSC'))
preference_flux.long

# Cisco -> Lake Trout and Walleye #
flux.1.preference = preference_flux.long %>%
  filter(group == 'CSC_LT-WAE')
flux.1.preference[flux.1.preference$flux_kJ_d == 0, 'flux_kJ_d'] <- NA
flux.1.preference

# pMIV -> Lake Trout and Cisco # 
flux.2.preference = preference_flux.long %>%
  filter(group == 'MIV_CSC-LT') %>% 
  group_by(year) %>% 
  summarize(flux_kJ_d = sum(flux_kJ_d)) %>%
  ungroup()
flux.2.preference

# Zoop -> Cisco and pMIV # 
flux.3.preference = preference_flux.long %>%
  filter(group == 'ZP_MIV-CSC') %>% 
  group_by(year) %>% 
  summarize(flux_kJ_d = sum(flux_kJ_d)) %>%
  ungroup()
flux.3.preference

# Zoop -> Cisco and pMIV and spiny water flea # 
flux.4.preference = preference_flux.long %>%
  filter(group == 'ZP_MIV-CSC-SWF') %>% 
  group_by(year) %>% 
  summarize(flux_kJ_d = sum(flux_kJ_d)) %>%
  ungroup()
flux.4.preference

# spiny -> Cisco  # 
flux.5.preference = preference_flux.long %>% 
  filter(group == 'SWF_CSC') %>% 
  group_by(year) %>% 
  summarize(flux_kJ_d = sum(flux_kJ_d)) %>%
  ungroup()
flux.5.preference

# Total energy flux # 
total.flux.preference = preference_flux.long %>% 
  group_by(year) %>% 
  summarize(total = sum(flux_kJ_d)) %>% 
  ungroup()
total.flux.preference

# stability # 
preference_stability


# Plotting Total Energy Flux (Base; Preference) # 
tfb = total.flux.binary %>%
  rename(binary.tf = total.yr)
tfp = total.flux.preference %>% 
  rename(pref.tf = total.yr)


# Plotting Total Energy Flux (Base; Preference) # 
tfb = total.flux.binary %>%
  rename(binary.tf = total)
tfp = total.flux.preference %>% 
  rename(pref.tf = total)



# comparisons # 

pmiv.flux.bin = flux.2.binary %>% 
  rename(flux.bin = flux_kJ_d)
pmiv.flux.pref = flux.2.preference %>% 
  rename(flux.pref = flux_kJ_d)

pmiv.flux = left_join(pmiv.flux.bin, pmiv.flux.pref, by = 'year') %>%
  mutate(pc = ((flux.pref-flux.bin)/flux.bin)*100)
pmiv.flux

mean(pmiv.flux$pc)
sd(pmiv.flux$pc)

byth.flux.bin = flux.5.binary %>%
  rename(flux.bin = flux_yr)
byth.flux.pref = flux.5.binary %>%
  rename(flux.pref = flux_yr)

byth.flux = left_join(byth.flux.bin, byth.flux.pref, by = 'year') %>%
  mutate(pc = ((flux.pref - flux.bin)/flux.bin)*100)
byth.flux

binary_flux

fish.flux.bin = flux.1.binary %>% 
  rename(flux.bin = flux_J_yr)
fish.flux.pref = flux.1.preference %>% 
  rename(flux.pref = flux_J_yr)

fish.flux = left_join(fish.flux.bin, fish.flux.pref, by = 'year') %>% 
  select(year, flux.bin, flux.pref) %>%
  mutate(pc = ((flux.pref-flux.bin)/flux.bin)*100)
fish.flux

fish.flux.one = fish.flux %>% 
  filter(year >2006 & year < 2014)
mean(fish.flux.one$flux.bin)
mean(fish.flux.one$flux.pref)

((74142 - 110141)/110141)*100
((76498 - 112257)/112257)*100

total.flux.binary
max(total.flux.binary$total.yr)
min(total.flux.binary$total.yr)
