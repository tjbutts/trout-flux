## Fluxweb Sensitivity Analyses ## ===================

# Flux trout sensitivity analysis # ====================
# Fluxing function # 
# 2001 example 
d01 =trout_pelagic.web %>% filter(year == 2001) %>% filter(spp != 'lake.trout' & spp != 'walleye')
d01

mat.01 = early.mat[-c(2:3),-c(2:3)]
mat.01

biom01 = c(d01$biomass.g_perhec) # biomass # 
loss01 = c(d01$losses) # metabolic losses 
effic01 = c(d01$efficiencies) # efficiencies 

# Create cevtors to store standard deviation # 
attach(species.level)
set.seed(12)
losses = 0.71*bodymasses^-0.25

sd.cvs.eff = c()
sd.cvs.los = c()
sd.cvs.mat = c()

for (var in seq(0, 0.09, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat,
                  biomasses=biomasses,
                  losses=losses,
                  efficiencies=efficiencies)
  sd.cvs.eff=c(sd.cvs.eff,mean(res[[2]],na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat, 
                  biomasses=biomasses, 
                  losses=losses, 
                  efficiencies=efficiencies) 
  sd.cvs.los=c(sd.cvs.los,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat, 
                  biomasses=biomasses, 
                  losses=losses, 
                  efficiencies=efficiencies)
  sd.cvs.mat=c(sd.cvs.mat,mean(res[[2]],na.rm=T))
}

x = seq(0, 0.09, 0.01)
plot(d$sd.cvs.eff ~ x, xlim = c(0,0.12) ,
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 16)
points(sd.csv.los ~ seq, col = 'red', pch = 16) 
points(sd.cvs.mat ~ seq, col = 'green', pch = 16) 
abline(a = 0, b = 1, lty = 2) 
legend('topleft', 
       legend = c('efficiency', 'metabolic losses', 'species preferences'), 
       col = c('black', 'red', 'green'), 
       pt.cex = 1.5, bty = 'n', 
       pt.bag = c('black', 'red', 'green'), pch = 21)

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


