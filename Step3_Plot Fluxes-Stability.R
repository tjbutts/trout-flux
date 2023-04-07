# Plot Fluxes #===============

# Flux Datasets # 
# Binary matrices 
bin.piscivory.flux 
bin.zooplanktivory.flux
bin.pelagic.miv.flux
bin.byth.flux

bin.stability 

bin.total.flux

# Preference matrices 
pref.piscivory.flux
pref.zooplanktivory.flux
pref.pelagic.miv.flux
pref.byth.flux

pref.stability

pref.total.flux

# Plotting Total Energy Flux (Base; Preference) # 
bin.total.flux = bin.total.flux %>%
  rename(bin.total.yr = total.yr) %>% 
  select(year, bin.total.yr)
pref.total.flux = pref.total.flux %>%
  rename(pref.total.yr = total.yr) %>%
  select(year, pref.total.yr)

total = left_join(bin.total.flux, pref.total.flux, by = 'year') %>%
  pivot_longer(cols = c(bin.total.yr, pref.total.yr), 
               names_to = 'matrix', 
               values_to = 'total.flux')
total

windows(height = 4, width = 6)
library(ggplot2)
ggplot(total, aes(fill = matrix, y = total.flux, x = year)) + 
  geom_bar(position = 'dodge', stat = 'identity') + 
  scale_fill_manual(values = c('black', 'gray60'), 
                    name = 'Interaction\nMatrix',
                    breaks = c('bin.total.yr', 'pref.total.yr'),
                    labels = c('Binary', 'Preference')) + theme_bw() + 
  scale_y_continuous(expand = c(0,0)) + 
  geom_vline(xintercept = c(2006.5, 2014.5), linewidth = 1, linetype = 'dotted') +
  ylab(expression(Total~energy~flux~`(`~J~ha^-1~yr^-1~`)`)) +
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))

# Plotting (Base Matrix) # ====================
windows(height = 3, width = 8)
par(mfrow = c(1,2), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

# Piscivory # 
plot(log(flux_J_yr)~year, type = 'o', pch = 19, lwd =2, col = '#99000d', ylim = c(log(1), log(3000000)), yaxt = 'n', 
     data = bin.piscivory.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
axis(side=2,
     at=c(log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),
          log(10),log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),
          log(100),log(200),log(300),log(400),log(500),log(600),log(700),log(800),log(900),log(1000),
          log(2000),log(3000),log(4000),log(5000),log(6000),log(7000),log(8000),log(9000),log(10000), 
          log(20000),log(30000),log(40000),log(50000),log(60000),log(70000),log(80000),log(90000), log(100000),
          log(200000),log(300000),log(400000),log(500000),log(600000),log(700000),log(800000),log(900000), 
          log(1000000), log(2000000), log(3000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','', '', '', '','','','',
                '90', '100', '', '','','','','','','','1000',
                '', '','','','','','','','10000',
                '', '','','','','','','','100000',
                '', '','','','','','','','1000000', '2000000', '3000000'),
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
       data = bin.zooplanktivory.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# mtext('Zooplanktivory Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first # 
#points(log(flux_yr) ~ year, data = zooplanktivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray50', xlab = '', ylab = '', lty = 2)


# Pelagic Zoobenthivory Flux # 
points(log(flux_yr)~year, type = 'o', pch = 19, lwd =2, col = '#fc9272',
       data = bin.pelagic.miv.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Macroinvertebrate Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first # 
#points(log(flux_yr) ~ year, data = pelagic.zoobenthivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray70', xlab = '', ylab = '', lty = 2)


# Bythotrephes -> Cisco Flux # 
points(log(flux_yr)~year, type = 'o', pch = 19, lwd =2, col = '#fdd0a2',
       data = bin.byth.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2)
# abline(v = 2014, lty = 2)
# mtext('Bythotrephes Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first 
#points(log(flux_yr) ~ year, data = byth.flux, type = 'o', pch = 19, lwd = 1, col = 'black', xlab = '', ylab = '', lty = 2)


legend('bottomleft', legend = c('Piscivory Flux', 'Zooplanktivory Flux', 'Pelagic MIV Flux',
                                'Bythorephes Flux'), pch = 19, col = c('#99000d','#ef3b2c', '#fc9272', '#fdd0a2'),
       bty = 'n', cex = 0.9)
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
mtext(side=3, 'Binary Interaction Matrix')
mtext(side = 1, line = 1.8, 'Year')

# Total Flux 
# points(log(total.yr)~year, type = 'o', pch = 19, col = 'black', lwd = 2.5, 
#      data = total.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Total Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)


# Plotting (Preference Matrix) # ====================

# Piscivory # 
plot(log(flux_J_yr)~year, type = 'o', pch = 19, lwd =2, col = '#99000d', ylim = c(log(1), log(3000000)), yaxt = 'n', 
     data = pref.piscivory.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
axis(side=2,
     at=c(log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
          log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),
          log(100),log(200),log(300),log(400),log(500),log(600),log(700),log(800),log(900),log(1000),
          log(2000),log(3000),log(4000),log(5000),log(6000),log(7000),log(8000),log(9000),log(10000), 
          log(20000),log(30000),log(40000),log(50000),log(60000),log(70000),log(80000),log(90000), log(100000),
          log(200000),log(300000),log(400000),log(500000),log(600000),log(700000),log(800000),log(900000), 
          log(1000000), log(2000000), log(3000000)), #Where the tick marks should be drawn
     labels = c('1', '', '','','','','','','','10','', '', '', '','','','',
                '90', '100', '', '','','','','','','','1000',
                '', '','','','','','','','10000',
                '', '','','','','','','','100000',
                '', '','','','','','','','1000000', '2000000', '3000000'),
     las=0)

# Piscivory - no walleye correction # 

#For uncorrected - must run base fluxes first # 
#points(log(flux_J_yr)~ year, data = piscivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray30', xlab = '', ylab = '', lty = 2)

# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Piscovry Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2, cex = 1)
#mtext(expression(`Log[Energy` ~ Flux ~ `(J`~ha^-1~year^-1~`)]`), side = 2, line = 2)

# Zooplanktivory # 
points(log(flux_yr)~year, type = 'o', pch = 19, lwd =2, col = '#ef3b2c',
       data = pref.zooplanktivory.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# mtext('Zooplanktivory Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first # 
#points(log(flux_yr) ~ year, data = zooplanktivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray50', xlab = '', ylab = '', lty = 2)


# Pelagic Zoobenthivory Flux # 
points(log(flux_yr)~year, type = 'o', pch = 19, lwd =2, col = '#fc9272',
       data = pref.pelagic.miv.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Macroinvertebrate Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first # 
#points(log(flux_yr) ~ year, data = pelagic.zoobenthivory.flux, type = 'o', pch = 19, lwd = 1, col = 'gray70', xlab = '', ylab = '', lty = 2)


# Bythotrephes -> Cisco Flux # 
points(log(flux_yr)~year, type = 'o', pch = 19, lwd =2, col = '#fdd0a2',
       data = pref.byth.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2)
# abline(v = 2014, lty = 2)
# mtext('Bythotrephes Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)

# For uncorrected - must run base fluxes first 
#points(log(flux_yr) ~ year, data = byth.flux, type = 'o', pch = 19, lwd = 1, col = 'black', xlab = '', ylab = '', lty = 2)


# legend('bottomleft', legend = c('Piscivory Flux', 'Zooplanktivory Flux', 'Zoobenthivory Flux',
#                                 'Bythorephes Flux'), pch = 19, col = c('#99000d','#ef3b2c', '#fc9272', '#fdd0a2'),
#        bty = 'n', cex = 1)
lines(c(2006.5,2006.5), c(log(0.1), log(3000000)), lty = 2)

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
mtext(side = 1, line = 1.8, 'Year')
mtext(side=3, 'Preference Interaction Matrix')

# Total Flux 
# points(log(total.yr)~year, type = 'o', pch = 19, col = 'black', lwd = 2.5, 
#      data = total.flux, xlim = c(2001, 2019), xlab = '', ylab = '')
# abline(v = 2007, lty = 2) 
# abline(v = 2014, lty = 2)
# mtext('Total Energy Flux (Joules year^-1)', side = 2, line = 2.8, cex = 1)
# mtext('Year', side = 1, line = 2.8, cex = 1)

# Stability Plot (Base) # ==============================
windows(height = 5, width = 6)
plot(stability~year, type = 'o', pch = 19, lwd = 2, data = stability.estimate_final,ylim = c(-3, 300), xlim = c(2001,2019), xlab = '', ylab = '')
mtext('Stability', side = 2, line = 2.8, cex = 1)
mtext('Year', side = 1, line = 2.8, cex = 1)

abline(h=0)
abline(v = 2006.5, lty = 2) 
abline(v = 2014.5, lty = 2)
text(2018, -6, 'Stable', font = 3)
text(2018, 10, 'Unstable', font = 3)

# Stability Plot (Preference) # ==============================
windows(height = 5, width = 6)
plot(stability~year, type = 'o', pch = 19, lwd = 2, data = stability.estimate_final,ylim = c(-3, 300), xlim = c(2001,2019), xlab = '', ylab = '')
mtext('Stability', side = 2, line = 2.8, cex = 1)
mtext('Year', side = 1, line = 2.8, cex = 1)

abline(h=0)
abline(v = 2006.5, lty = 2) 
abline(v = 2014.5, lty = 2)
text(2018, -6, 'Stable', font = 3)
text(2018, 10, 'Unstable', font = 3)
