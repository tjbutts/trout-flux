## Trout Taxa Biomass ## 

# Load in libraries # 
library(tidyverse)
library(lubridate)

# Load in biomass data # 
fish = read_csv('trout_fish_biomass_selectpelagic.csv')
pelagic.miv = read_csv('trout_pmiv_biomass.reduced.csv')
zoop.grouped = read_csv('trout_pzoop_grouped.alldates.csv')

# Fish biomass # 
fish

fish.select = fish %>%
  select(year, spp, biomass.kg_perhec.raw, biomass.kg_perhec.corrected, biomass.kg_upperse.corrected, biomass.kg_lowerse.corrected) %>% 
  filter(year != 2020) # Match other taxa record 

fish.raw = fish.select %>% 
  select(year, spp, biomass.kg_perhec.raw) %>% 
  pivot_wider(id_cols = year, 
              names_from = spp, 
              values_from = biomass.kg_perhec.raw)
fish.raw

corrected.wae = fish.select %>% 
  select(year, spp, biomass.kg_perhec.corrected) %>% 
  pivot_wider(id_cols = year, 
              names_from = spp, 
              values_from = biomass.kg_perhec.corrected) %>% 
  rename(p20.walleye = walleye)

upperse = fish.select %>% 
  select(year, spp, biomass.kg_upperse.corrected) %>% 
  pivot_wider(id_cols = year, 
              names_from = spp, 
              values_from = biomass.kg_upperse.corrected) %>%
  rename(c.se.u = cisco, lt.se.u = lake.trout, w.se.u = walleye)
upperse

lowerse = fish.select %>% 
  select(year, spp, biomass.kg_lowerse.corrected) %>% 
  pivot_wider(id_cols = year, 
              names_from = spp, 
              values_from = biomass.kg_lowerse.corrected)  %>%
  rename(c.se.l = cisco, lt.se.l = lake.trout, w.se.l = walleye)
lowerse

join1 = left_join(corrected.wae, upperse, by = 'year')
fish.final = left_join(join1, lowerse, by = 'year')
fish.final

# Fish colors 
  # Cisco #de2d26
  # lake.trout #fc9272
  # walleye #fee0d2 
windows(height = 8, width = 6)
par(mfrow =c(3,1), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))

wae = '#b5a479'
lt = '#A77600'
cis = 'gray30'

transparent = rgb(255,255,255 ,maxColorValue = 255, alpha = 100)

# Fish plot #===================
# plot(cisco~year, data = fish.raw, type = 'o',col = '#99000d', lwd = 2, pch = 19, cex = 1, ylim = c(0,130),
#      ylab = '', xlab = '')
# mtext(side = 2, line =2.5, expression(Pelagic ~ ~Fish ~ ~Biomass ~`(`~ kg ~ ha^-1~`)`))
# arrows(x0=fish.final$year, y0=fish.final$c.se.l, 
#        x1=fish.final$year, y1=fish.final$c.se.u, col = '#99000d', code = 3, angle=90, length=0, lwd = 2)
# points(lake.trout~year, data = fish.raw, type = 'o', col = '#ef3b2c', lwd = 2, pch = 19, cex = 1)
# arrows(x0=fish.final$year, y0=fish.final$lt.se.l, 
#        x1=fish.final$year, y1=fish.final$lt.se.u, col = '#ef3b2c', code = 3, angle=90, length=0, lwd = 2)
# points(walleye.p20~year, data = fish.raw, type = 'o', col = '#fc9272', lwd = 2, pch = 19, cex = 1)
# arrows(x0=fish.final$year, y0=fish.final$w.se.l, 
#        x1=fish.final$year, y1=fish.final$w.se.u, col = '#fc9272', code = 3, angle=90, length=0, lwd = 2)
# legend('topright', legend = c('Cisco', 'Lake trout', 'Walleye (20%)'), pch = 19, col = c('#99000d','#ef3b2c', '#fc9272'),
#        bty = 'n')

# Log all three plots option # 
plot(log(cisco)~year, data = fish.raw, type = 'o',col = cis, lwd = 2, pch = 17, cex = 2, ylim = c(log(0.1), log(200)),
     ylab = '', xlab = '', yaxt = 'n', col.axis = transparent)
axis(side=2,
     at=c(log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
           log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '', 
                '1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','200'),
     las=0)
mtext(side = 2, line =2.5, expression(Log~`[Fish`~Biomass~`(`~ kg ~ ha^-1~`)]`))
arrows(x0=fish.final$year, y0=log(fish.final$c.se.l), 
       x1=fish.final$year, y1=log(fish.final$c.se.u), col = cis, code = 3, angle=90, length=0, lwd = 3)
points(log(lake.trout)~year, data = fish.raw, type = 'o', col = lt, lwd = 2, pch = 19, cex = 2)
arrows(x0=fish.final$year, y0=log(fish.final$lt.se.l), 
       x1=fish.final$year, y1=log(fish.final$lt.se.u), col = lt, code = 3, angle=90, length=0, lwd = 3)
points(log(p20.walleye)~year, data = corrected.wae, type = 'o', col = wae, lwd = 2, pch = 15, cex = 2)
arrows(x0=fish.final$year, y0=log(fish.final$w.se.l), 
       x1=fish.final$year, y1=log(fish.final$w.se.u), col = wae, code = 3, angle=90, length=0, lwd = 3)
legend('bottomleft', legend = c('Lake trout', 'Cisco', 'Walleye (20%)'), pch = c(19, 17, 15), col = c(lt, cis, wae),
       bty = 'n', cex = 1.5)
abline(v = 2006.5, lty = 2)
abline(v = 2014.5, lty = 2)

# Pelagic MIV # 
pelagic.miv

pmiv.select = pelagic.miv %>% 
  mutate(taxon = tolower(taxon)) %>% 
  select(year4, taxon, biomass.g_perhec, biomass.prop.err.se) %>% 
  mutate(biomass.mg_perhec = (biomass.g_perhec*1000), 
         biomass.mg.prop.err.se = (biomass.prop.err.se*1000)) %>% 
  mutate(biomass.mg_perm2 = biomass.mg_perhec/10000, 
         biomass.mg.perm2.prop.err.se = biomass.mg.prop.err.se/10000) %>%
  filter(year4 != 2020) # Match other taxa record # 
pmiv.select
max(pmiv.select$biomass.mg_perm2)
min(pmiv.select$biomass.mg_perm2)

pmiv.wide = pmiv.select %>% 
  pivot_wider(id_cols = year4, 
              names_from = 'taxon', 
              values_from = 'biomass.mg_perm2') %>% 
  rename(chaoborus = `chaoborus larvae`)
pmiv.wide

pmiv.error = pmiv.select %>% 
  pivot_wider(id_cols = year4, 
              names_from = 'taxon', 
              values_from = 'biomass.mg.perm2.prop.err.se') %>% 
  rename(chaoborus.se = `chaoborus larvae`, 
         leptodora.se = leptodora, 
         mysis.se = mysis, 
         bytho.se = bythotrephes)
pmiv.error  

pmiv.final = left_join(pmiv.wide, pmiv.error, by = 'year4')
pmiv.final

# MIV plot # =============================
mys = '#a75b51' 
chao = '#692e27' 
lep = '#f9afa6'
byth = 'gray30'
  

plot(log(chaoborus)~year4, data = pmiv.final, type = 'o',col = chao, lwd = 2, pch = 17, cex = 2, ylim = c(log(0.001),log(300)),
     ylab = '', xlab = '', yaxt = 'n', col.axis = transparent)
axis(side=2,
     at=c( log(0.001),log(0.002),log(0.003),log(0.004),log(0.005),log(0.006),log(0.007),log(0.008),log(0.009), 
       log(0.01),log(0.02),log(0.03),log(0.04),log(0.05),log(0.06),log(0.07),log(0.08),log(0.09), 
       log(0.1),log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
       log(1),log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
           log(200),log(300)), #Where the tick marks should be drawn
     labels = c('0.001', '', '', '', '', '', '', '', '',
                '0.01', '', '', '', '', '', '', '', '',
                '0.1', '', '', '', '', '', '', '', '', 
                '1', '', '', '', '', '', '', '', '', '10', '', '','','','','','','','100','','300'),
     las=0)
mtext(side = 2, line =2.5, expression(`Log[`~MIV~Biomass ~`(`~ mg ~ m^-2~`)]`))
arrows(x0=pmiv.final$year4, y0=(log(pmiv.final$chaoborus - pmiv.final$chaoborus.se)), 
       x1=pmiv.final$year4, y1=(log(pmiv.final$chaoborus + pmiv.final$chaoborus.se)), col = chao, code = 3, angle=90, length=0, lwd = 3)
points(log(leptodora)~year4, data = pmiv.final, type = 'o', col = lep, lwd = 2, pch = 15, cex = 2)
arrows(x0=pmiv.final$year4, y0=(log(pmiv.final$leptodora - pmiv.final$leptodora.se)), col = lep,
       x1=pmiv.final$year4, y1=(log(pmiv.final$leptodora + pmiv.final$leptodora.se)), code = 3, angle=90, length=0, lwd = 3)
points(log(mysis)~year4, data = pmiv.final, type = 'o', col = mys, lwd = 2, pch = 19, cex = 2)
arrows(x0=pmiv.final$year4, y0=(log(pmiv.final$mysis - pmiv.final$mysis.se)), col = mys,
       x1=pmiv.final$year4, y1=(log(pmiv.final$mysis + pmiv.final$mysis.se)), code = 3, angle=90, length=0, lwd = 3)
points(log(bythotrephes)~year4, data = pmiv.final, type = 'o', col = byth, lwd = 2, pch = 18, cex = 2)
arrows(x0=pmiv.final$year4, y0=(log(pmiv.final$bythotrephes - pmiv.final$bytho.se)), col = byth,
       x1=pmiv.final$year4, y1=(log(pmiv.final$bythotrephes + pmiv.final$bytho.se)), code = 3, angle=90, length=0, lwd = 3)

legend('bottomleft', legend = c('Mysis', 'Chaoborus', 'Leptodora', 'Bythotrephes'), pch = c(19, 17, 15, 18), col = c(mys, chao,lep, byth),
       bty = 'n', cex = 1.5)
abline(v = 2006.5, lty = 2)
abline(v = 2014.5, lty = 2)

# Zooplankton # 
zoop.grouped

zoop.grouped.select = zoop.grouped %>% 
  select(year4, sample_date, larger_group, biomass_ugL, biomass_g.perhec, avg.bodymass_g, biomass_g.per_m2) %>% 
  mutate(sample_date = mdy(sample_date)) %>% 
  mutate(month = month(sample_date)) %>% 
  filter(month == 7 | month == 8) %>%
  rename(year = year4, 
         spp = larger_group, 
         bodymass_g = avg.bodymass_g, 
         biomass.g_perhec = biomass_g.perhec) %>% 
  group_by(year, spp) %>% 
  summarize(biomass.g_perhec = mean(biomass.g_perhec, na.rm = T),
            biomass_ugL = mean(biomass_ugL), 
            biomass.g_perm2 = mean(biomass_g.per_m2, na.rm = T), 
            sd_g.perm2 = sd(biomass_g.per_m2, na.rm = T), 
            g_perhec.sd = sd(biomass.g_perhec, na.rm = T),
            bodymass_g = mean(bodymass_g, na.rm = T), 
            mass_sd = sd(bodymass_g, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(biomass.mg_perm2 = biomass.g_perm2*1000, 
         sd_mg.perm2 = sd_g.perm2*1000) %>% 
  mutate(se_mg.perm2 = sd_mg.perm2/sqrt(2)) %>% 
  select(year, spp, biomass.mg_perm2, se_mg.perm2, biomass_ugL)
zoop.grouped.select

max(zoop.grouped.select$biomass.mg_perm2) # 5000
min(zoop.grouped.select$biomass.mg_perm2) # 20

zp.wide = zoop.grouped.select%>% 
  mutate(spp = tolower(spp)) %>%
  pivot_wider(id_cols = year, 
              names_from = 'spp', 
              values_from = 'biomass.mg_perm2')
zp.wide

zp.wide.check = zoop.grouped.select %>% 
  mutate(spp = tolower(spp)) %>% 
  pivot_wider(id_cols = year,
              names_from = 'spp', 
              values_from = 'biomass_ugL')
zp.wide.check

zp.error = zoop.grouped.select %>% 
  mutate(spp = tolower(spp)) %>%
  pivot_wider(id_cols = year, 
              names_from = 'spp', 
              values_from = 'se_mg.perm2') %>% 
  rename(clad.se = cladocera, 
         cope.se = copepoda, 
         roti.se = rotifera)
zp.error  

zp.final = left_join(zp.wide, zp.error, by = 'year')
zp.final

#Zoop Plot #======================== 
cop = '#cd5c05'
clad = 'gray30' 
rot = '#e2a271'


plot(log(cladocera)~year, data = zp.final, type = 'o',col = clad, lwd = 2, pch = 17, cex = 2, ylim = c(log(1),log(8000)),
     ylab = '', xlab = '', yaxt = 'n') 
axis(side=2,
     at=c(log(0.1), log(0.2),log(0.3),log(0.4),log(0.5),log(0.6),log(0.7),log(0.8),log(0.9),
          log(1), log(2),log(3),log(4),log(5),log(6),log(7),log(8),log(9),log(10),
           log(20),log(30),log(40),log(50),log(60),log(70),log(80),log(90),log(100), 
          log(200),log(300),log(400),log(500),log(600),log(700),log(800),log(900),log(1000),
          log(2000),log(3000),log(4000),log(5000),log(6000),log(7000),log(8000)), #Where the tick marks should be drawn
     labels = c('0.1', '', '', '', '', '', '', '', '',
                '1', '', '', '', '', '', '', '', '',
                '10', '', '', '', '', '', '', '', '',
                '100', '', '', '', '', '', '', '', '',
                '1000', '', '', '', '', '','','8000'),
     las=0)
mtext(side = 2, line =2.5, expression(`Log[`~Zoop~Biomass ~`(`~ mg ~ m^-2~`)]`))
arrows(x0=zp.final$year, y0=(log(zp.final$cladocera - zp.final$clad.se)), 
       x1=zp.final$year, y1=(log(zp.final$cladocera + zp.final$clad.se)), col = clad, code = 3, angle=90, length=0, lwd = 2)
points(log(copepoda)~year, data = zp.final, type = 'o', col = cop, lwd = 2, pch = 19, cex = 2)
arrows(x0=zp.final$year, y0=(log(zp.final$copepoda - zp.final$cope.se)), col = cop,
       x1=zp.final$year, y1=(log(zp.final$copepoda + zp.final$cope.se)), code = 3, angle=90, length=0, lwd = 2)
points(log(rotifera)~year, data = zp.final, type = 'o', col = rot, lwd = 2, pch = 15, cex = 2)
arrows(x0=zp.final$year, y0=(log(zp.final$rotifera - zp.final$roti.se)), col = rot,
       x1=zp.final$year, y1=(log(zp.final$rotifera + zp.final$roti.se)), code = 3, angle=90, length=0, lwd = 2)

legend('bottomleft', legend = c('Copepoda', 'Cladocera',  'Rotifera'), pch = c(19,17,15), col = c(cop, clad, rot),
       bty = 'n', cex = 1.5)
abline(v = 2006.5, lty = 2)
abline(v = 2014.5, lty = 2)
mtext(side = 1, 'Year', line = 2)

