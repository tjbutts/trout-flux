##========================================================## 
       ## MAKE SURE THAT STEP 2 WAS RUN FIRST ##
##========================================================##
library(tidyverse)
# plot Trout Lake Energy fluxes # 

# Flux datasets from Step 2 #===============
binary_flux 
binary_stability

preference_flux
preference_stability

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
  summarize(total.yr = sum(flux_kJ_d)) %>% 
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
  summarize(total.yr = sum(flux_kJ_d)) %>% 
  ungroup()
total.flux.preference

# stability # 
preference_stability


# Plotting Total Energy Flux (Base; Preference) # 
tfb = total.flux.binary %>%
  rename(binary.tf = total.yr)
tfp = total.flux.preference %>% 
  rename(pref.tf = total.yr)

total = left_join(tfb, tfp, by = 'year') %>%
  pivot_longer(cols = c(binary.tf, pref.tf), 
               names_to = 'matrix', 
               values_to = 'total.flux') %>% 
  rename(Year = year)
total

windows(height = 3, width = 5)
library(ggplot2)
ggplot(total, aes(fill = matrix, y = total.flux, x = Year)) + 
  geom_bar(position = 'dodge', stat = 'identity') + 
  scale_fill_manual(values = c('black', 'gray60'), 
                    name = 'Interaction\nMatrix',
                    breaks = c('binary.tf', 'pref.tf'),
                    labels = c('Binary', 'Preference')) + theme_bw() + 
  scale_y_continuous(expand = c(0,0)) + 
  geom_vline(xintercept = c(2006.5, 2014.5), linewidth = 1, linetype = 'dotted') +
  ylab(expression(Total~energy~flux~`(`~kJ~ha^-1~d^-1~`)`)) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

# Plotting (Base Matrix) # ====================
windows(height = 6, width = 7)
par(mfrow = c(2,1), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Colors # 
f.1 = '#253494'
f.2 = '#2c7fb8' 
f.3 = '#41b6c4' 
f.4 = '#a1dab4'
f.5 = 'gray30'
transparent = rgb(255,255,255 ,maxColorValue = 255, alpha = 100)

# Cisco -> fish # 
plot(log(flux_kJ_d)~year, type = 'o', pch = 19, lwd =2, col = f.1, ylim = c(log(90), log(300000000)), yaxt = 'n', 
     data = flux.1.binary, xlim = c(2001, 2019), xlab = '', ylab = '', cex = 2, col.axis = transparent)
#mtext(side=3, 'Doubled fish biomass + halved ZP & MIV biomass') # Add and adjust title based on what base dataset was run in Step 2 
axis(side=2,
     at=c(
          # log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          # log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),
          # log(10),log(20),log(30),log(40),log(50),log(60),log(70),log(80),
          log(100),log(200),log(300),log(400),log(500),log(600),log(700),log(800),log(900),log(1000),
          log(2000),log(3000),log(4000),log(5000),log(6000),log(7000),log(8000),log(9000),log(10000), 
          log(20000),log(30000),log(40000),log(50000),log(60000),log(70000),log(80000),log(90000), log(100000),
          log(200000),log(300000),log(400000),log(500000),log(600000),log(700000),log(800000),log(900000), 
          log(1000000), log(2000000), log(3000000), log(4000000), 
          log(5000000), log(6000000), log(7000000), log(8000000), log(9000000),
          log(10000000), log(20000000), log(30000000), log(40000000),
          log(50000000), log(60000000), log(70000000), log(80000000), log(90000000), log(100000000), 
          log(200000000), log(300000000)), #Where the tick marks should be drawn
     labels = c('100', '', '','','','','','','','1000',
                '', '','','','','','','','10000',
                '', '','','','','','','','100000',
                '', '','','','','','','','1000000', 
                '', '','','','','','','','10000000',
                '', '','','','','','','','100000000', '200000000', '300000000'),
     las=0)

mtext(expression(`Log[Energy` ~ Flux ~ `(kJ`~ha^-1~d^-1~`)]`), side = 2, line = 3)
mtext(side =2, line =2.2, 'Binary Matrix')

# pMIV -> LT & CSC# 
points(log(flux_kJ_d)~year, type = 'o', pch = 15, lwd =2, col = f.2, cex = 2,
       data = flux.2.binary, xlim = c(2001, 2019), xlab = '', ylab = '')
# mtext('Zooplanktivory Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first # 
#points(log(flux_kJ_d) ~ year, data = zooplanktivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray50', xlab = '', ylab = '', lty = 2)


# ZP -> CSC & pMIV # 
points(log(flux_kJ_d)~year, type = 'o', pch = 17, lwd =2, col = f.3, cex = 2,
       data = flux.3.binary, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Macroinvertebrate Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first # 
#points(log(flux_kJ_d) ~ year, data = pelagic.zoobenthivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray70', xlab = '', ylab = '', lty = 2)

# ZP -> CSC & pMIV & SWF # 
points(log(flux_kJ_d)~year, type = 'o', pch = 17, lwd = 2, col = f.4, cex = 2,
       data = flux.4.binary, xlim = c(2001, 2019), xlab = '', ylab = '')

# Spiny -> CSC # 
points(log(flux_kJ_d)~year, type = 'o', pch = 18, lwd =2, col = f.5, cex = 2,
       data = flux.5.binary, xlim = c(2001, 2019), xlab = '', ylab = '')

# abline(v = 2007, lty = 2)
# abline(v = 2014, lty = 2)
# mtext('Bythotrephes Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first 
#points(log(flux_kJ_d) ~ year, data = byth.flux, type = 'o', pch = 19, lwd = 1, col = 'black', xlab = '', ylab = '', lty = 2)


legend('bottomleft', legend = c(expression(Cisco~`->`~`LT,WAE`), 'MIV -> Cisco,LT', 'ZP -> Cisco,MIV',
                                expression(`ZP -> Cisco,MIV,`~italic(Bythotrephes)),
                                expression(italic(Bythotrephes)~`-> Cisco`)), pch = c(19, 15, 17, 17, 18), col = c(f.1, f.2, f.3, f.4, f.5),
       bty = 'n', cex = 1, ncol = 2, x.intersp=1, text.width = 4.3)
lines(c(2006.5,2006.5), c(log(10000), log(5000000000)), lty = 2)
lines(c(2014.5, 2014.5), c(log(10000), log(5000000000)), lty = 2)

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

# Cisco -> fish # 
plot(log(flux_kJ_d)~year, type = 'o', pch = 19, lwd =2, col = f.1, ylim = c(log(90), log(300000000)), yaxt = 'n', 
     data = flux.1.preference, xlim = c(2001, 2019), xlab = '', ylab = '', cex = 2)
axis(side=2,
     at=c(
       # log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
       # log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),
       # log(10),log(20),log(30),log(40),log(50),log(60),log(70),log(80),
       log(100),log(200),log(300),log(400),log(500),log(600),log(700),log(800),log(900),log(1000),
       log(2000),log(3000),log(4000),log(5000),log(6000),log(7000),log(8000),log(9000),log(10000), 
       log(20000),log(30000),log(40000),log(50000),log(60000),log(70000),log(80000),log(90000), log(100000),
       log(200000),log(300000),log(400000),log(500000),log(600000),log(700000),log(800000),log(900000), 
       log(1000000), log(2000000), log(3000000), log(4000000), 
       log(5000000), log(6000000), log(7000000), log(8000000), log(9000000),
       log(10000000), log(20000000), log(30000000), log(40000000),
       log(50000000), log(60000000), log(70000000), log(80000000), log(90000000), log(100000000), 
       log(200000000), log(300000000)), #Where the tick marks should be drawn
     labels = c('100', '', '','','','','','','','1000',
                '', '','','','','','','','10000',
                '', '','','','','','','','100000',
                '', '','','','','','','','1000000', 
                '', '','','','','','','','10000000',
                '', '','','','','','','','100000000', '200000000', '300000000'),
     las=0)

mtext(expression(`Log[Energy` ~ Flux ~ `(kJ`~ha^-1~d^-1~`)]`), side = 2, line = 3)


# pMIV -> LT & CSC# 
points(log(flux_kJ_d)~year, type = 'o', pch = 15, lwd =2, col = f.2, cex = 2,
       data = flux.2.preference, xlim = c(2001, 2019), xlab = '', ylab = '')
# mtext('Zooplanktivory Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first # 
#points(log(flux_kJ_d) ~ year, data = zooplanktivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray50', xlab = '', ylab = '', lty = 2)


# ZP -> CSC & pMIV # 
points(log(flux_kJ_d)~year, type = 'o', pch = 17, lwd =2, col = f.3, cex = 2,
       data = flux.3.preference, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Macroinvertebrate Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first # 
#points(log(flux_kJ_d) ~ year, data = pelagic.zoobenthivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray70', xlab = '', ylab = '', lty = 2)

# ZP -> CSC & pMIV & SWF # 
points(log(flux_kJ_d)~year, type = 'o', pch = 17, lwd = 2, col = f.4, cex = 2,
       data = flux.4.preference, xlim = c(2001, 2019), xlab = '', ylab = '')

# Spiny -> CSC # 
points(log(flux_kJ_d)~year, type = 'o', pch = 18, lwd =2, col = f.5, cex = 2,
       data = flux.5.preference, xlim = c(2001, 2019), xlab = '', ylab = '')

# abline(v = 2007, lty = 2)
# abline(v = 2014, lty = 2)
# mtext('Bythotrephes Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first 
#points(log(flux_kJ_d) ~ year, data = byth.flux, type = 'o', pch = 19, lwd = 1, col = 'black', xlab = '', ylab = '', lty = 2)

lines(c(2006.5,2006.5), c(log(0.0001), log(5000000000)), lty = 2)
lines(c(2014.5, 2014.5), c(log(0.00001), log(5000000000)), lty = 2)

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


# Total Flux 
# points(log(total.yr)~year, type = 'o', pch = 19, col = 'black', lwd = 2.5, 
#      data = total.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Total Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)
mtext(side = 2, line = 2.2, 'Preference Matrix')
mtext(side = 1, line = 1.8, 'Year')

# Just Preference Energy Flux Plot #=========================
windows(height = 3, width = 5)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Colors # 
f.1 = '#253494'
f.2 = '#2c7fb8' 
f.3 = '#41b6c4' 
f.4 = '#a1dab4'
f.5 = 'gray30'
transparent = rgb(255,255,255 ,maxColorValue = 255, alpha = 100)

# Cisco -> fish # 
plot(log(flux_kJ_d)~year, type = 'o', pch = 19, lwd =2, col = f.1, ylim = c(log(90), log(300000000)), yaxt = 'n', 
     data = flux.1.preference, xlim = c(2001, 2019), xaxt = 'n',  
     xlab = '', ylab = '', cex = 2, cex.lab = 2)
axis(side=1, 
     at = c(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012,
            2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020), 
     labels = c('', '2001', '', '2003', '', '2005', '', '2007', '', '2009', '', '2011',
                '', '2013', '', '2015', '', '2017', '', '2019', ''), 
     cex = 3, las = 3)
axis(side=2,
     at=c(
       # log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
       # log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),
       # log(10),log(20),log(30),log(40),log(50),log(60),log(70),log(80),
       log(100),log(200),log(300),log(400),log(500),log(600),log(700),log(800),log(900),log(1000),
       log(2000),log(3000),log(4000),log(5000),log(6000),log(7000),log(8000),log(9000),log(10000), 
       log(20000),log(30000),log(40000),log(50000),log(60000),log(70000),log(80000),log(90000), log(100000),
       log(200000),log(300000),log(400000),log(500000),log(600000),log(700000),log(800000),log(900000), 
       log(1000000), log(2000000), log(3000000), log(4000000), 
       log(5000000), log(6000000), log(7000000), log(8000000), log(9000000),
       log(10000000), log(20000000), log(30000000), log(40000000),
       log(50000000), log(60000000), log(70000000), log(80000000), log(90000000), log(100000000), 
       log(200000000), log(300000000)), #Where the tick marks should be drawn
     labels = F,
     # labels = c('100', '', '','','','','','','','1000',
     #            '', '','','','','','','','10000',
     #            '', '','','','','','','','100000',
     #            '', '','','','','','','','1000000', 
     #            '', '','','','','','','','10000000',
     #            '', '','','','','','','','100000000', '200000000', '300000000'),
     las=0, cex = 3)

mtext(expression(`Log[Energy` ~ Flux~`]`) , side = 2, line = 3, cex = 1.2)
mtext(expression(`(kJ`~ha^-1~d^-1~`)]`), side = 2, line = 1.5, cex = 1.2)
mtext('Year', side = 1, cex = 1.2, line = 2.8)

# pMIV -> LT & CSC# 
points(log(flux_kJ_d)~year, type = 'o', pch = 15, lwd =2, col = f.2, cex = 2,
       data = flux.2.preference, xlim = c(2001, 2019), xlab = '', ylab = '')

# ZP -> CSC & pMIV # 
points(log(flux_kJ_d)~year, type = 'o', pch = 17, lwd =2, col = f.3, cex = 2,
       data = flux.3.preference, xlim = c(2001, 2019), xlab = '', ylab = '')

# ZP -> CSC & pMIV & SWF # 
points(log(flux_kJ_d)~year, type = 'o', pch = 17, lwd = 2, col = f.4, cex = 2,
       data = flux.4.preference, xlim = c(2001, 2019), xlab = '', ylab = '')

# Spiny -> CSC # 
points(log(flux_kJ_d)~year, type = 'o', pch = 18, lwd =2, col = f.5, cex = 2,
       data = flux.5.preference, xlim = c(2001, 2019), xlab = '', ylab = '')

lines(c(2006.5,2006.5), c(log(0.0001), log(5000000000)), lty = 2)
lines(c(2014.5, 2014.5), c(log(0.00001), log(5000000000)), lty = 2)


# Stability Plot (Base) # ==============================
windows(height = 6, width = 7)
par(mfrow = c(2,1), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

plot(stability~year, type = 'o', pch = 19, lwd = 2, ylim = c(-150, 350),
     data = binary_stability, 
     xlim = c(2001,2019), xlab = '', ylab = '')
axis(side = 2, at = c(0, 50, 100, 150, 200, 250, 300), labels = T)
mtext(expression(`Stability, ` ~ italic(s)), side = 2, line = 3, cex = 1)
mtext(side = 2, 'Binary Matrix', line = 2)
mtext(side = 3, 'Doubled fish biomass + halved ZP & MIV biomass') # Adjust when manipulating biomass for sensitivity

abline(v = 2006.5, lty = 2) 
abline(v = 2014.5, lty = 2)
abline(h = 0) # only for if values dip into negatives 

plot(stability~year, type = 'o', pch = 19, lwd = 2, 
     data = preference_stability, ylim = c(-150, 350), 
     xlim = c(2001,2019), xlab = '', ylab = '')
mtext(expression(`Stability, ` ~ italic(s)), side = 2, line = 3, cex = 1)
mtext(side = 2, 'Preference Matrix', line = 2)

abline(v = 2006.5, lty = 2)
abline(v = 2014.5, lty = 2)
abline(h = 0) # only for if values dip into negatives 
mtext('Year', side = 1, line = 2, cex = 1)

# Just preference stability plot #=========================
windows(height = 3, width = 7)
par(mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

plot(stability~year, type = 'o', pch = 19, lwd = 2, 
     data = preference_stability, ylim = c(0, 350), 
     xlim = c(2001,2019), xlab = '', ylab = '', xaxt = 'n')
axis(side = 1, 
     at = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,
            2009,2010,2011,2012,2013,2014,2015,2016,2017,
            2018,2019,2020), 
     labels = c('', '2001', '', '2003','','2005','','2007','','2009','','2011',
                '', '2013','','2015','','2017','','2019',''), cex = 3, las = 3)
mtext(expression(`Stability, ` ~ italic(s)), side = 2, line = 3, cex = 1.2)

abline(v = 2006.5, lty = 2)
abline(v = 2014.5, lty = 2)
mtext('Year', side = 1, line = 2.8, cex = 1.2)
