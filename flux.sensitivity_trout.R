## Fluxweb Sensitivity Analyses ## ===================

# Flux trout sensitivity analysis - Binary matrix # ====================
# Fluxing function # 

# Create cevtors to store standard deviation # 
set.seed(55)

sd.cvs.eff.01 = c()
sd.cvs.los.01 = c()
sd.cvs.mat.01 = c()
sd.cvs.eff.02 = c()
sd.cvs.los.02 = c()
sd.cvs.mat.02 = c()
sd.cvs.eff.03 = c()
sd.cvs.los.03 = c()
sd.cvs.mat.03 = c()
sd.cvs.eff.04 = c()
sd.cvs.los.04 = c()
sd.cvs.mat.04 = c()
sd.cvs.eff.05 = c()
sd.cvs.los.05 = c()
sd.cvs.mat.05 = c()
sd.cvs.eff.06 = c()
sd.cvs.los.06 = c()
sd.cvs.mat.06 = c()
sd.cvs.eff.07 = c()
sd.cvs.los.07 = c()
sd.cvs.mat.07 = c()
sd.cvs.eff.08 = c()
sd.cvs.los.08 = c()
sd.cvs.mat.08 = c()
sd.cvs.eff.09 = c()
sd.cvs.los.09 = c()
sd.cvs.mat.09 = c()
sd.cvs.eff.10 = c()
sd.cvs.los.10 = c()
sd.cvs.mat.10 = c()
sd.cvs.eff.11 = c()
sd.cvs.los.11 = c()
sd.cvs.mat.11 = c()
sd.cvs.eff.12 = c()
sd.cvs.los.12 = c()
sd.cvs.mat.12 = c()
sd.cvs.eff.13 = c()
sd.cvs.los.13 = c()
sd.cvs.mat.13 = c()
sd.cvs.eff.14 = c()
sd.cvs.los.14 = c()
sd.cvs.mat.14 = c()
sd.cvs.eff.15 = c()
sd.cvs.los.15 = c()
sd.cvs.mat.15 = c()
sd.cvs.eff.16 = c()
sd.cvs.los.16 = c()
sd.cvs.mat.16 = c()
sd.cvs.eff.17 = c()
sd.cvs.los.17 = c()
sd.cvs.mat.17 = c()
sd.cvs.eff.18 = c()
sd.cvs.los.18 = c()
sd.cvs.mat.18 = c()
sd.cvs.eff.19 = c()
sd.cvs.los.19 = c()
sd.cvs.mat.19 = c()


seq = seq(0, 0.12, 0.01)
seq.eff = seq[1:11]

# Ignore efficiencies error (it's because you can't have greater than 1 efficiency )
windows(height = 6.5, width = 8)
par(mfrow = c(4,5), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


# start 
for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.01,
                  biomasses=d01$biomass.g_perhec,
                  losses=d01$losses,
                  efficiencies=d01$efficiencies)
  sd.cvs.eff.01=c(sd.cvs.eff.01, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.01, 
                  biomasses=d01$biomass.g_perhec, 
                  losses=d01$losses, 
                  efficiencies=d01$efficiencies) 
  sd.cvs.los.01=c(sd.cvs.los.01,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.01,
                  biomasses=d01$biomass.g_perhec,
                  losses=d01$losses,
                  efficiencies=d01$efficiencies)
  sd.cvs.mat.01=c(sd.cvs.mat.01,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.01 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
mtext(side =2, 'Observed departure', line = 3)
mtext(side = 2, 'to original results', line = 2)
points(sd.cvs.los.01 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.01 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 
legend('topleft', 
       legend = c('efficiency', 'metabolic losses'), 
       col = c('black', '#7fc97f'), 
       pt.cex = 1.5, bty = 'n', 
       pt.bg = c('black', '#7fc97f'), pch = 21)

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.02,
                  biomasses=d02$biomass.g_perhec,
                  losses=d02$losses,
                  efficiencies=d02$efficiencies)
  sd.cvs.eff.02=c(sd.cvs.eff.02, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.02, 
                  biomasses=d02$biomass.g_perhec, 
                  losses=d02$losses, 
                  efficiencies=d02$efficiencies) 
  sd.cvs.los.02=c(sd.cvs.los.02,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.02,
                  biomasses=d02$biomass.g_perhec,
                  losses=d02$losses,
                  efficiencies=d02$efficiencies)
  sd.cvs.mat.02=c(sd.cvs.mat.02,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.02 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.02 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.02 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.03,
                  biomasses=d03$biomass.g_perhec,
                  losses=d03$losses,
                  efficiencies=d03$efficiencies)
  sd.cvs.eff.03=c(sd.cvs.eff.03, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.03, 
                  biomasses=d03$biomass.g_perhec, 
                  losses=d03$losses, 
                  efficiencies=d03$efficiencies) 
  sd.cvs.los.03=c(sd.cvs.los.03,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.03,
                  biomasses=d03$biomass.g_perhec,
                  losses=d03$losses,
                  efficiencies=d03$efficiencies)
  sd.cvs.mat.03=c(sd.cvs.mat.03,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.03 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.03 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.03 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.04,
                  biomasses=d04$biomass.g_perhec,
                  losses=d04$losses,
                  efficiencies=d04$efficiencies)
  sd.cvs.eff.04=c(sd.cvs.eff.04, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.04, 
                  biomasses=d04$biomass.g_perhec, 
                  losses=d04$losses, 
                  efficiencies=d04$efficiencies) 
  sd.cvs.los.04=c(sd.cvs.los.04,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.04,
                  biomasses=d04$biomass.g_perhec,
                  losses=d04$losses,
                  efficiencies=d04$efficiencies)
  sd.cvs.mat.04=c(sd.cvs.mat.04,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.04 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.04 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.04 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.05,
                  biomasses=d05$biomass.g_perhec,
                  losses=d05$losses,
                  efficiencies=d05$efficiencies)
  sd.cvs.eff.05=c(sd.cvs.eff.05, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.05, 
                  biomasses=d05$biomass.g_perhec, 
                  losses=d05$losses, 
                  efficiencies=d05$efficiencies) 
  sd.cvs.los.05=c(sd.cvs.los.05,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.05,
                  biomasses=d05$biomass.g_perhec,
                  losses=d05$losses,
                  efficiencies=d05$efficiencies)
  sd.cvs.mat.05=c(sd.cvs.mat.05,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.05 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.05 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.05 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.06,
                  biomasses=d06$biomass.g_perhec,
                  losses=d06$losses,
                  efficiencies=d06$efficiencies)
  sd.cvs.eff.06=c(sd.cvs.eff.06, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.06, 
                  biomasses=d06$biomass.g_perhec, 
                  losses=d06$losses, 
                  efficiencies=d06$efficiencies) 
  sd.cvs.los.06=c(sd.cvs.los.06,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.06,
                  biomasses=d06$biomass.g_perhec,
                  losses=d06$losses,
                  efficiencies=d06$efficiencies)
  sd.cvs.mat.06=c(sd.cvs.mat.06,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.06 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.06 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.06 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.07,
                  biomasses=d07$biomass.g_perhec,
                  losses=d07$losses,
                  efficiencies=d07$efficiencies)
  sd.cvs.eff.07=c(sd.cvs.eff.07, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.07, 
                  biomasses=d07$biomass.g_perhec, 
                  losses=d07$losses, 
                  efficiencies=d07$efficiencies) 
  sd.cvs.los.07=c(sd.cvs.los.07,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.07,
                  biomasses=d07$biomass.g_perhec,
                  losses=d07$losses,
                  efficiencies=d07$efficiencies)
  sd.cvs.mat.07=c(sd.cvs.mat.07,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.07 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.07 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.07 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.08,
                  biomasses=d08$biomass.g_perhec,
                  losses=d08$losses,
                  efficiencies=d08$efficiencies)
  sd.cvs.eff.08=c(sd.cvs.eff.08, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.08, 
                  biomasses=d08$biomass.g_perhec, 
                  losses=d08$losses, 
                  efficiencies=d08$efficiencies) 
  sd.cvs.los.08=c(sd.cvs.los.08,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.08,
                  biomasses=d08$biomass.g_perhec,
                  losses=d08$losses,
                  efficiencies=d08$efficiencies)
  sd.cvs.mat.08=c(sd.cvs.mat.08,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.08 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.08 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.08 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.09,
                  biomasses=d09$biomass.g_perhec,
                  losses=d09$losses,
                  efficiencies=d09$efficiencies)
  sd.cvs.eff.09=c(sd.cvs.eff.09, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.09, 
                  biomasses=d09$biomass.g_perhec, 
                  losses=d09$losses, 
                  efficiencies=d09$efficiencies) 
  sd.cvs.los.09=c(sd.cvs.los.09,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.09,
                  biomasses=d09$biomass.g_perhec,
                  losses=d09$losses,
                  efficiencies=d09$efficiencies)
  sd.cvs.mat.09=c(sd.cvs.mat.09,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.09 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.09 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.09 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.10,
                  biomasses=d10$biomass.g_perhec,
                  losses=d10$losses,
                  efficiencies=d10$efficiencies)
  sd.cvs.eff.10=c(sd.cvs.eff.10, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.10, 
                  biomasses=d10$biomass.g_perhec, 
                  losses=d10$losses, 
                  efficiencies=d10$efficiencies) 
  sd.cvs.los.10=c(sd.cvs.los.10,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.10,
                  biomasses=d10$biomass.g_perhec,
                  losses=d10$losses,
                  efficiencies=d10$efficiencies)
  sd.cvs.mat.10=c(sd.cvs.mat.10,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.10 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.10 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.10 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.11,
                  biomasses=d11$biomass.g_perhec,
                  losses=d11$losses,
                  efficiencies=d11$efficiencies)
  sd.cvs.eff.11=c(sd.cvs.eff.11, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.11, 
                  biomasses=d11$biomass.g_perhec, 
                  losses=d11$losses, 
                  efficiencies=d11$efficiencies) 
  sd.cvs.los.11=c(sd.cvs.los.11,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.11,
                  biomasses=d11$biomass.g_perhec,
                  losses=d11$losses,
                  efficiencies=d11$efficiencies)
  sd.cvs.mat.11=c(sd.cvs.mat.11,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.11 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.11 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.11 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.12,
                  biomasses=d12$biomass.g_perhec,
                  losses=d12$losses,
                  efficiencies=d12$efficiencies)
  sd.cvs.eff.12=c(sd.cvs.eff.12, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.12, 
                  biomasses=d12$biomass.g_perhec, 
                  losses=d12$losses, 
                  efficiencies=d12$efficiencies) 
  sd.cvs.los.12=c(sd.cvs.los.12,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.12,
                  biomasses=d12$biomass.g_perhec,
                  losses=d12$losses,
                  efficiencies=d12$efficiencies)
  sd.cvs.mat.12=c(sd.cvs.mat.12,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.12 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.12 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.12 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.13,
                  biomasses=d13$biomass.g_perhec,
                  losses=d13$losses,
                  efficiencies=d13$efficiencies)
  sd.cvs.eff.13=c(sd.cvs.eff.13, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.13, 
                  biomasses=d13$biomass.g_perhec, 
                  losses=d13$losses, 
                  efficiencies=d13$efficiencies) 
  sd.cvs.los.13=c(sd.cvs.los.13,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.13,
                  biomasses=d13$biomass.g_perhec,
                  losses=d13$losses,
                  efficiencies=d13$efficiencies)
  sd.cvs.mat.13=c(sd.cvs.mat.13,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.13 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.13 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.13 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.14,
                  biomasses=d14$biomass.g_perhec,
                  losses=d14$losses,
                  efficiencies=d14$efficiencies)
  sd.cvs.eff.14=c(sd.cvs.eff.14, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.14, 
                  biomasses=d14$biomass.g_perhec, 
                  losses=d14$losses, 
                  efficiencies=d14$efficiencies) 
  sd.cvs.los.14=c(sd.cvs.los.14,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.14,
                  biomasses=d14$biomass.g_perhec,
                  losses=d14$losses,
                  efficiencies=d14$efficiencies)
  sd.cvs.mat.14=c(sd.cvs.mat.14,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.14 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.14 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.14 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.15,
                  biomasses=d15$biomass.g_perhec,
                  losses=d15$losses,
                  efficiencies=d15$efficiencies)
  sd.cvs.eff.15=c(sd.cvs.eff.15, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.15, 
                  biomasses=d15$biomass.g_perhec, 
                  losses=d15$losses, 
                  efficiencies=d15$efficiencies) 
  sd.cvs.los.15=c(sd.cvs.los.15,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.15,
                  biomasses=d15$biomass.g_perhec,
                  losses=d15$losses,
                  efficiencies=d15$efficiencies)
  sd.cvs.mat.15=c(sd.cvs.mat.15,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.15 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.15 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.15 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.16,
                  biomasses=d16$biomass.g_perhec,
                  losses=d16$losses,
                  efficiencies=d16$efficiencies)
  sd.cvs.eff.16=c(sd.cvs.eff.16, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.16, 
                  biomasses=d16$biomass.g_perhec, 
                  losses=d16$losses, 
                  efficiencies=d16$efficiencies) 
  sd.cvs.los.16=c(sd.cvs.los.16,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.16,
                  biomasses=d16$biomass.g_perhec,
                  losses=d16$losses,
                  efficiencies=d16$efficiencies)
  sd.cvs.mat.16=c(sd.cvs.mat.16,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.16 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1),
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.16 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.16 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.17,
                  biomasses=d17$biomass.g_perhec,
                  losses=d17$losses,
                  efficiencies=d17$efficiencies)
  sd.cvs.eff.17=c(sd.cvs.eff.17, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.17, 
                  biomasses=d17$biomass.g_perhec, 
                  losses=d17$losses, 
                  efficiencies=d17$efficiencies) 
  sd.cvs.los.17=c(sd.cvs.los.17,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.17,
                  biomasses=d17$biomass.g_perhec,
                  losses=d17$losses,
                  efficiencies=d17$efficiencies)
  sd.cvs.mat.17=c(sd.cvs.mat.17,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.17 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), 
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.17 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.17 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.18,
                  biomasses=d18$biomass.g_perhec,
                  losses=d18$losses,
                  efficiencies=d18$efficiencies)
  sd.cvs.eff.18=c(sd.cvs.eff.18, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.18, 
                  biomasses=d18$biomass.g_perhec, 
                  losses=d18$losses, 
                  efficiencies=d18$efficiencies) 
  sd.cvs.los.18=c(sd.cvs.los.18,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.18,
                  biomasses=d18$biomass.g_perhec,
                  losses=d18$losses,
                  efficiencies=d18$efficiencies)
  sd.cvs.mat.18=c(sd.cvs.mat.18,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.18 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), 
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.18 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.18 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 
mtext(side =1, 'Variation in parameters', line = 2)

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.19,
                  biomasses=d19$biomass.g_perhec,
                  losses=d19$losses,
                  efficiencies=d19$efficiencies)
  sd.cvs.eff.19=c(sd.cvs.eff.19, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.19, 
                  biomasses=d19$biomass.g_perhec, 
                  losses=d19$losses, 
                  efficiencies=d19$efficiencies) 
  sd.cvs.los.19=c(sd.cvs.los.19,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.19,
                  biomasses=d19$biomass.g_perhec,
                  losses=d19$losses,
                  efficiencies=d19$efficiencies)
  sd.cvs.mat.19=c(sd.cvs.mat.19,mean(res[[2]],na.rm=T))
}

# Binary matrix # 
plot(sd.cvs.eff.19 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), 
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.19 ~ seq.eff, col = '#7fc97f', pch = 19) 
#points(sd.cvs.mat.19 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

# Flux trout sensitivity analysis - Preference matrix # ====================
# Fluxing function # 

# Create cevtors to store standard deviation # 
set.seed(55)

sd.cvs.eff.01 = c()
sd.cvs.los.01 = c()
sd.cvs.mat.01 = c()
sd.cvs.eff.02 = c()
sd.cvs.los.02 = c()
sd.cvs.mat.02 = c()
sd.cvs.eff.03 = c()
sd.cvs.los.03 = c()
sd.cvs.mat.03 = c()
sd.cvs.eff.04 = c()
sd.cvs.los.04 = c()
sd.cvs.mat.04 = c()
sd.cvs.eff.05 = c()
sd.cvs.los.05 = c()
sd.cvs.mat.05 = c()
sd.cvs.eff.06 = c()
sd.cvs.los.06 = c()
sd.cvs.mat.06 = c()
sd.cvs.eff.07 = c()
sd.cvs.los.07 = c()
sd.cvs.mat.07 = c()
sd.cvs.eff.08 = c()
sd.cvs.los.08 = c()
sd.cvs.mat.08 = c()
sd.cvs.eff.09 = c()
sd.cvs.los.09 = c()
sd.cvs.mat.09 = c()
sd.cvs.eff.10 = c()
sd.cvs.los.10 = c()
sd.cvs.mat.10 = c()
sd.cvs.eff.11 = c()
sd.cvs.los.11 = c()
sd.cvs.mat.11 = c()
sd.cvs.eff.12 = c()
sd.cvs.los.12 = c()
sd.cvs.mat.12 = c()
sd.cvs.eff.13 = c()
sd.cvs.los.13 = c()
sd.cvs.mat.13 = c()
sd.cvs.eff.14 = c()
sd.cvs.los.14 = c()
sd.cvs.mat.14 = c()
sd.cvs.eff.15 = c()
sd.cvs.los.15 = c()
sd.cvs.mat.15 = c()
sd.cvs.eff.16 = c()
sd.cvs.los.16 = c()
sd.cvs.mat.16 = c()
sd.cvs.eff.17 = c()
sd.cvs.los.17 = c()
sd.cvs.mat.17 = c()
sd.cvs.eff.18 = c()
sd.cvs.los.18 = c()
sd.cvs.mat.18 = c()
sd.cvs.eff.19 = c()
sd.cvs.los.19 = c()
sd.cvs.mat.19 = c()


seq = seq(0, 0.12, 0.01)
seq.eff = seq[1:11]

# Ignore efficiencies error (it's because you can't have greater than 1 efficiency )
windows(height = 6.5, width = 8)
par(mfrow = c(4,5), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


# start 
for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.01,
                  biomasses=d01$biomass.g_perhec,
                  losses=d01$losses,
                  efficiencies=d01$efficiencies)
  sd.cvs.eff.01=c(sd.cvs.eff.01, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.01, 
                  biomasses=d01$biomass.g_perhec, 
                  losses=d01$losses, 
                  efficiencies=d01$efficiencies) 
  sd.cvs.los.01=c(sd.cvs.los.01,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.01,
                  biomasses=d01$biomass.g_perhec,
                  losses=d01$losses,
                  efficiencies=d01$efficiencies)
  sd.cvs.mat.01=c(sd.cvs.mat.01,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.01 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
mtext(side =2, 'Observed departure', line = 3)
mtext(side = 2, 'to original results', line = 2)
points(sd.cvs.los.01 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.01 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 
legend('topleft', 
       legend = c('efficiency', 'metabolic losses', 'prey preferences'), 
       col = c('black', '#7fc97f', '#beaed4'), 
       pt.cex = 1.5, bty = 'n', 
       pt.bg = c('black', '#7fc97f', '#beaed4'), pch = 21)

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.02,
                  biomasses=d02$biomass.g_perhec,
                  losses=d02$losses,
                  efficiencies=d02$efficiencies)
  sd.cvs.eff.02=c(sd.cvs.eff.02, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.02, 
                  biomasses=d02$biomass.g_perhec, 
                  losses=d02$losses, 
                  efficiencies=d02$efficiencies) 
  sd.cvs.los.02=c(sd.cvs.los.02,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.02,
                  biomasses=d02$biomass.g_perhec,
                  losses=d02$losses,
                  efficiencies=d02$efficiencies)
  sd.cvs.mat.02=c(sd.cvs.mat.02,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.02 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.02 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.02 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.03,
                  biomasses=d03$biomass.g_perhec,
                  losses=d03$losses,
                  efficiencies=d03$efficiencies)
  sd.cvs.eff.03=c(sd.cvs.eff.03, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.03, 
                  biomasses=d03$biomass.g_perhec, 
                  losses=d03$losses, 
                  efficiencies=d03$efficiencies) 
  sd.cvs.los.03=c(sd.cvs.los.03,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.03,
                  biomasses=d03$biomass.g_perhec,
                  losses=d03$losses,
                  efficiencies=d03$efficiencies)
  sd.cvs.mat.03=c(sd.cvs.mat.03,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.03 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.03 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.03 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.04,
                  biomasses=d04$biomass.g_perhec,
                  losses=d04$losses,
                  efficiencies=d04$efficiencies)
  sd.cvs.eff.04=c(sd.cvs.eff.04, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.04, 
                  biomasses=d04$biomass.g_perhec, 
                  losses=d04$losses, 
                  efficiencies=d04$efficiencies) 
  sd.cvs.los.04=c(sd.cvs.los.04,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.04,
                  biomasses=d04$biomass.g_perhec,
                  losses=d04$losses,
                  efficiencies=d04$efficiencies)
  sd.cvs.mat.04=c(sd.cvs.mat.04,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.04 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.04 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.04 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.05,
                  biomasses=d05$biomass.g_perhec,
                  losses=d05$losses,
                  efficiencies=d05$efficiencies)
  sd.cvs.eff.05=c(sd.cvs.eff.05, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.05, 
                  biomasses=d05$biomass.g_perhec, 
                  losses=d05$losses, 
                  efficiencies=d05$efficiencies) 
  sd.cvs.los.05=c(sd.cvs.los.05,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.05,
                  biomasses=d05$biomass.g_perhec,
                  losses=d05$losses,
                  efficiencies=d05$efficiencies)
  sd.cvs.mat.05=c(sd.cvs.mat.05,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.05 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.05 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.05 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.06,
                  biomasses=d06$biomass.g_perhec,
                  losses=d06$losses,
                  efficiencies=d06$efficiencies)
  sd.cvs.eff.06=c(sd.cvs.eff.06, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.06, 
                  biomasses=d06$biomass.g_perhec, 
                  losses=d06$losses, 
                  efficiencies=d06$efficiencies) 
  sd.cvs.los.06=c(sd.cvs.los.06,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.06,
                  biomasses=d06$biomass.g_perhec,
                  losses=d06$losses,
                  efficiencies=d06$efficiencies)
  sd.cvs.mat.06=c(sd.cvs.mat.06,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.06 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.06 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.06 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.07,
                  biomasses=d07$biomass.g_perhec,
                  losses=d07$losses,
                  efficiencies=d07$efficiencies)
  sd.cvs.eff.07=c(sd.cvs.eff.07, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.07, 
                  biomasses=d07$biomass.g_perhec, 
                  losses=d07$losses, 
                  efficiencies=d07$efficiencies) 
  sd.cvs.los.07=c(sd.cvs.los.07,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.07,
                  biomasses=d07$biomass.g_perhec,
                  losses=d07$losses,
                  efficiencies=d07$efficiencies)
  sd.cvs.mat.07=c(sd.cvs.mat.07,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.07 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.07 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.07 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.08,
                  biomasses=d08$biomass.g_perhec,
                  losses=d08$losses,
                  efficiencies=d08$efficiencies)
  sd.cvs.eff.08=c(sd.cvs.eff.08, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.08, 
                  biomasses=d08$biomass.g_perhec, 
                  losses=d08$losses, 
                  efficiencies=d08$efficiencies) 
  sd.cvs.los.08=c(sd.cvs.los.08,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.08,
                  biomasses=d08$biomass.g_perhec,
                  losses=d08$losses,
                  efficiencies=d08$efficiencies)
  sd.cvs.mat.08=c(sd.cvs.mat.08,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.08 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.08 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.08 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.09,
                  biomasses=d09$biomass.g_perhec,
                  losses=d09$losses,
                  efficiencies=d09$efficiencies)
  sd.cvs.eff.09=c(sd.cvs.eff.09, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.09, 
                  biomasses=d09$biomass.g_perhec, 
                  losses=d09$losses, 
                  efficiencies=d09$efficiencies) 
  sd.cvs.los.09=c(sd.cvs.los.09,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.09,
                  biomasses=d09$biomass.g_perhec,
                  losses=d09$losses,
                  efficiencies=d09$efficiencies)
  sd.cvs.mat.09=c(sd.cvs.mat.09,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.09 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.09 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.09 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.10,
                  biomasses=d10$biomass.g_perhec,
                  losses=d10$losses,
                  efficiencies=d10$efficiencies)
  sd.cvs.eff.10=c(sd.cvs.eff.10, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.10, 
                  biomasses=d10$biomass.g_perhec, 
                  losses=d10$losses, 
                  efficiencies=d10$efficiencies) 
  sd.cvs.los.10=c(sd.cvs.los.10,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.10,
                  biomasses=d10$biomass.g_perhec,
                  losses=d10$losses,
                  efficiencies=d10$efficiencies)
  sd.cvs.mat.10=c(sd.cvs.mat.10,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.10 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.10 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.10 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.11,
                  biomasses=d11$biomass.g_perhec,
                  losses=d11$losses,
                  efficiencies=d11$efficiencies)
  sd.cvs.eff.11=c(sd.cvs.eff.11, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.11, 
                  biomasses=d11$biomass.g_perhec, 
                  losses=d11$losses, 
                  efficiencies=d11$efficiencies) 
  sd.cvs.los.11=c(sd.cvs.los.11,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.11,
                  biomasses=d11$biomass.g_perhec,
                  losses=d11$losses,
                  efficiencies=d11$efficiencies)
  sd.cvs.mat.11=c(sd.cvs.mat.11,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.11 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.11 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.11 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.12,
                  biomasses=d12$biomass.g_perhec,
                  losses=d12$losses,
                  efficiencies=d12$efficiencies)
  sd.cvs.eff.12=c(sd.cvs.eff.12, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.12, 
                  biomasses=d12$biomass.g_perhec, 
                  losses=d12$losses, 
                  efficiencies=d12$efficiencies) 
  sd.cvs.los.12=c(sd.cvs.los.12,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.12,
                  biomasses=d12$biomass.g_perhec,
                  losses=d12$losses,
                  efficiencies=d12$efficiencies)
  sd.cvs.mat.12=c(sd.cvs.mat.12,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.12 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.12 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.12 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.13,
                  biomasses=d13$biomass.g_perhec,
                  losses=d13$losses,
                  efficiencies=d13$efficiencies)
  sd.cvs.eff.13=c(sd.cvs.eff.13, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.13, 
                  biomasses=d13$biomass.g_perhec, 
                  losses=d13$losses, 
                  efficiencies=d13$efficiencies) 
  sd.cvs.los.13=c(sd.cvs.los.13,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.13,
                  biomasses=d13$biomass.g_perhec,
                  losses=d13$losses,
                  efficiencies=d13$efficiencies)
  sd.cvs.mat.13=c(sd.cvs.mat.13,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.13 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.13 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.13 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.14,
                  biomasses=d14$biomass.g_perhec,
                  losses=d14$losses,
                  efficiencies=d14$efficiencies)
  sd.cvs.eff.14=c(sd.cvs.eff.14, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.14, 
                  biomasses=d14$biomass.g_perhec, 
                  losses=d14$losses, 
                  efficiencies=d14$efficiencies) 
  sd.cvs.los.14=c(sd.cvs.los.14,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.14,
                  biomasses=d14$biomass.g_perhec,
                  losses=d14$losses,
                  efficiencies=d14$efficiencies)
  sd.cvs.mat.14=c(sd.cvs.mat.14,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.14 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.14 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.14 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.15,
                  biomasses=d15$biomass.g_perhec,
                  losses=d15$losses,
                  efficiencies=d15$efficiencies)
  sd.cvs.eff.15=c(sd.cvs.eff.15, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.15, 
                  biomasses=d15$biomass.g_perhec, 
                  losses=d15$losses, 
                  efficiencies=d15$efficiencies) 
  sd.cvs.los.15=c(sd.cvs.los.15,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.15,
                  biomasses=d15$biomass.g_perhec,
                  losses=d15$losses,
                  efficiencies=d15$efficiencies)
  sd.cvs.mat.15=c(sd.cvs.mat.15,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.15 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.15 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.15 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.16,
                  biomasses=d16$biomass.g_perhec,
                  losses=d16$losses,
                  efficiencies=d16$efficiencies)
  sd.cvs.eff.16=c(sd.cvs.eff.16, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.16, 
                  biomasses=d16$biomass.g_perhec, 
                  losses=d16$losses, 
                  efficiencies=d16$efficiencies) 
  sd.cvs.los.16=c(sd.cvs.los.16,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.16,
                  biomasses=d16$biomass.g_perhec,
                  losses=d16$losses,
                  efficiencies=d16$efficiencies)
  sd.cvs.mat.16=c(sd.cvs.mat.16,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.16 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1),
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.16 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.16 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.17,
                  biomasses=d17$biomass.g_perhec,
                  losses=d17$losses,
                  efficiencies=d17$efficiencies)
  sd.cvs.eff.17=c(sd.cvs.eff.17, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.17, 
                  biomasses=d17$biomass.g_perhec, 
                  losses=d17$losses, 
                  efficiencies=d17$efficiencies) 
  sd.cvs.los.17=c(sd.cvs.los.17,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.17,
                  biomasses=d17$biomass.g_perhec,
                  losses=d17$losses,
                  efficiencies=d17$efficiencies)
  sd.cvs.mat.17=c(sd.cvs.mat.17,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.17 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), 
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.17 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.17 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.18,
                  biomasses=d18$biomass.g_perhec,
                  losses=d18$losses,
                  efficiencies=d18$efficiencies)
  sd.cvs.eff.18=c(sd.cvs.eff.18, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.18, 
                  biomasses=d18$biomass.g_perhec, 
                  losses=d18$losses, 
                  efficiencies=d18$efficiencies) 
  sd.cvs.los.18=c(sd.cvs.los.18,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.18,
                  biomasses=d18$biomass.g_perhec,
                  losses=d18$losses,
                  efficiencies=d18$efficiencies)
  sd.cvs.mat.18=c(sd.cvs.mat.18,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.18 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), 
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.18 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.18 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 
mtext(side =1, 'Variation in parameters', line = 2)

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.19,
                  biomasses=d19$biomass.g_perhec,
                  losses=d19$losses,
                  efficiencies=d19$efficiencies)
  sd.cvs.eff.19=c(sd.cvs.eff.19, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.19, 
                  biomasses=d19$biomass.g_perhec, 
                  losses=d19$losses, 
                  efficiencies=d19$efficiencies) 
  sd.cvs.los.19=c(sd.cvs.los.19,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.19,
                  biomasses=d19$biomass.g_perhec,
                  losses=d19$losses,
                  efficiencies=d19$efficiencies)
  sd.cvs.mat.19=c(sd.cvs.mat.19,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.19 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), 
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.19 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.19 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 


# Stability function - binary matrix # =================================

# Create cevtors to store standard deviation # 
set.seed(55)

sd.cvs.eff.01 = c()
sd.cvs.los.01 = c()
sd.cvs.mat.01 = c()
sd.cvs.eff.02 = c()
sd.cvs.los.02 = c()
sd.cvs.mat.02 = c()
sd.cvs.eff.03 = c()
sd.cvs.los.03 = c()
sd.cvs.mat.03 = c()
sd.cvs.eff.04 = c()
sd.cvs.los.04 = c()
sd.cvs.mat.04 = c()
sd.cvs.eff.05 = c()
sd.cvs.los.05 = c()
sd.cvs.mat.05 = c()
sd.cvs.eff.06 = c()
sd.cvs.los.06 = c()
sd.cvs.mat.06 = c()
sd.cvs.eff.07 = c()
sd.cvs.los.07 = c()
sd.cvs.mat.07 = c()
sd.cvs.eff.08 = c()
sd.cvs.los.08 = c()
sd.cvs.mat.08 = c()
sd.cvs.eff.09 = c()
sd.cvs.los.09 = c()
sd.cvs.mat.09 = c()
sd.cvs.eff.10 = c()
sd.cvs.los.10 = c()
sd.cvs.mat.10 = c()
sd.cvs.eff.11 = c()
sd.cvs.los.11 = c()
sd.cvs.mat.11 = c()
sd.cvs.eff.12 = c()
sd.cvs.los.12 = c()
sd.cvs.mat.12 = c()
sd.cvs.eff.13 = c()
sd.cvs.los.13 = c()
sd.cvs.mat.13 = c()
sd.cvs.eff.14 = c()
sd.cvs.los.14 = c()
sd.cvs.mat.14 = c()
sd.cvs.eff.15 = c()
sd.cvs.los.15 = c()
sd.cvs.mat.15 = c()
sd.cvs.eff.16 = c()
sd.cvs.los.16 = c()
sd.cvs.mat.16 = c()
sd.cvs.eff.17 = c()
sd.cvs.los.17 = c()
sd.cvs.mat.17 = c()
sd.cvs.eff.18 = c()
sd.cvs.los.18 = c()
sd.cvs.mat.18 = c()
sd.cvs.eff.19 = c()
sd.cvs.los.19 = c()
sd.cvs.mat.19 = c()


seq = seq(0, 0.12, 0.01)
seq.eff = seq[1:11]

# Ignore efficiencies error (it's because you can't have greater than 1 efficiency )
windows(height = 6.5, width = 8)
par(mfrow = c(4,5), mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0))


# start 
for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.01,
                  biomasses=d01$biomass.g_perhec,
                  losses=d01$losses,
                  efficiencies=d01$efficiencies)
  sd.cvs.eff.01=c(sd.cvs.eff.01, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.01, 
                  biomasses=d01$biomass.g_perhec, 
                  losses=d01$losses, 
                  efficiencies=d01$efficiencies) 
  sd.cvs.los.01=c(sd.cvs.los.01,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.01,
                  biomasses=d01$biomass.g_perhec,
                  losses=d01$losses,
                  efficiencies=d01$efficiencies)
  sd.cvs.mat.01=c(sd.cvs.mat.01,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.01 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
mtext(side =2, 'Observed departure', line = 3)
mtext(side = 2, 'to original results', line = 2)
points(sd.cvs.los.01 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.01 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 
legend('topleft', 
       legend = c('efficiency', 'metabolic losses', 'prey preferences'), 
       col = c('black', '#7fc97f', '#beaed4'), 
       pt.cex = 1.5, bty = 'n', 
       pt.bg = c('black', '#7fc97f', '#beaed4'), pch = 21)

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.02,
                  biomasses=d02$biomass.g_perhec,
                  losses=d02$losses,
                  efficiencies=d02$efficiencies)
  sd.cvs.eff.02=c(sd.cvs.eff.02, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.02, 
                  biomasses=d02$biomass.g_perhec, 
                  losses=d02$losses, 
                  efficiencies=d02$efficiencies) 
  sd.cvs.los.02=c(sd.cvs.los.02,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.02,
                  biomasses=d02$biomass.g_perhec,
                  losses=d02$losses,
                  efficiencies=d02$efficiencies)
  sd.cvs.mat.02=c(sd.cvs.mat.02,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.02 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.02 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.02 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.03,
                  biomasses=d03$biomass.g_perhec,
                  losses=d03$losses,
                  efficiencies=d03$efficiencies)
  sd.cvs.eff.03=c(sd.cvs.eff.03, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.03, 
                  biomasses=d03$biomass.g_perhec, 
                  losses=d03$losses, 
                  efficiencies=d03$efficiencies) 
  sd.cvs.los.03=c(sd.cvs.los.03,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.03,
                  biomasses=d03$biomass.g_perhec,
                  losses=d03$losses,
                  efficiencies=d03$efficiencies)
  sd.cvs.mat.03=c(sd.cvs.mat.03,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.03 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.03 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.03 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.04,
                  biomasses=d04$biomass.g_perhec,
                  losses=d04$losses,
                  efficiencies=d04$efficiencies)
  sd.cvs.eff.04=c(sd.cvs.eff.04, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.04, 
                  biomasses=d04$biomass.g_perhec, 
                  losses=d04$losses, 
                  efficiencies=d04$efficiencies) 
  sd.cvs.los.04=c(sd.cvs.los.04,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.04,
                  biomasses=d04$biomass.g_perhec,
                  losses=d04$losses,
                  efficiencies=d04$efficiencies)
  sd.cvs.mat.04=c(sd.cvs.mat.04,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.04 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.04 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.04 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.05,
                  biomasses=d05$biomass.g_perhec,
                  losses=d05$losses,
                  efficiencies=d05$efficiencies)
  sd.cvs.eff.05=c(sd.cvs.eff.05, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.05, 
                  biomasses=d05$biomass.g_perhec, 
                  losses=d05$losses, 
                  efficiencies=d05$efficiencies) 
  sd.cvs.los.05=c(sd.cvs.los.05,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.05,
                  biomasses=d05$biomass.g_perhec,
                  losses=d05$losses,
                  efficiencies=d05$efficiencies)
  sd.cvs.mat.05=c(sd.cvs.mat.05,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.05 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.05 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.05 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.06,
                  biomasses=d06$biomass.g_perhec,
                  losses=d06$losses,
                  efficiencies=d06$efficiencies)
  sd.cvs.eff.06=c(sd.cvs.eff.06, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.06, 
                  biomasses=d06$biomass.g_perhec, 
                  losses=d06$losses, 
                  efficiencies=d06$efficiencies) 
  sd.cvs.los.06=c(sd.cvs.los.06,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.06,
                  biomasses=d06$biomass.g_perhec,
                  losses=d06$losses,
                  efficiencies=d06$efficiencies)
  sd.cvs.mat.06=c(sd.cvs.mat.06,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.06 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.06 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.06 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.07,
                  biomasses=d07$biomass.g_perhec,
                  losses=d07$losses,
                  efficiencies=d07$efficiencies)
  sd.cvs.eff.07=c(sd.cvs.eff.07, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.07, 
                  biomasses=d07$biomass.g_perhec, 
                  losses=d07$losses, 
                  efficiencies=d07$efficiencies) 
  sd.cvs.los.07=c(sd.cvs.los.07,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.07,
                  biomasses=d07$biomass.g_perhec,
                  losses=d07$losses,
                  efficiencies=d07$efficiencies)
  sd.cvs.mat.07=c(sd.cvs.mat.07,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.07 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.07 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.07 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.08,
                  biomasses=d08$biomass.g_perhec,
                  losses=d08$losses,
                  efficiencies=d08$efficiencies)
  sd.cvs.eff.08=c(sd.cvs.eff.08, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.08, 
                  biomasses=d08$biomass.g_perhec, 
                  losses=d08$losses, 
                  efficiencies=d08$efficiencies) 
  sd.cvs.los.08=c(sd.cvs.los.08,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.08,
                  biomasses=d08$biomass.g_perhec,
                  losses=d08$losses,
                  efficiencies=d08$efficiencies)
  sd.cvs.mat.08=c(sd.cvs.mat.08,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.08 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.08 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.08 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.09,
                  biomasses=d09$biomass.g_perhec,
                  losses=d09$losses,
                  efficiencies=d09$efficiencies)
  sd.cvs.eff.09=c(sd.cvs.eff.09, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.09, 
                  biomasses=d09$biomass.g_perhec, 
                  losses=d09$losses, 
                  efficiencies=d09$efficiencies) 
  sd.cvs.los.09=c(sd.cvs.los.09,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.09,
                  biomasses=d09$biomass.g_perhec,
                  losses=d09$losses,
                  efficiencies=d09$efficiencies)
  sd.cvs.mat.09=c(sd.cvs.mat.09,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.09 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.09 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.09 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.10,
                  biomasses=d10$biomass.g_perhec,
                  losses=d10$losses,
                  efficiencies=d10$efficiencies)
  sd.cvs.eff.10=c(sd.cvs.eff.10, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.10, 
                  biomasses=d10$biomass.g_perhec, 
                  losses=d10$losses, 
                  efficiencies=d10$efficiencies) 
  sd.cvs.los.10=c(sd.cvs.los.10,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.10,
                  biomasses=d10$biomass.g_perhec,
                  losses=d10$losses,
                  efficiencies=d10$efficiencies)
  sd.cvs.mat.10=c(sd.cvs.mat.10,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.10 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.10 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.10 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.11,
                  biomasses=d11$biomass.g_perhec,
                  losses=d11$losses,
                  efficiencies=d11$efficiencies)
  sd.cvs.eff.11=c(sd.cvs.eff.11, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.11, 
                  biomasses=d11$biomass.g_perhec, 
                  losses=d11$losses, 
                  efficiencies=d11$efficiencies) 
  sd.cvs.los.11=c(sd.cvs.los.11,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.11,
                  biomasses=d11$biomass.g_perhec,
                  losses=d11$losses,
                  efficiencies=d11$efficiencies)
  sd.cvs.mat.11=c(sd.cvs.mat.11,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.11 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.11 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.11 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.12,
                  biomasses=d12$biomass.g_perhec,
                  losses=d12$losses,
                  efficiencies=d12$efficiencies)
  sd.cvs.eff.12=c(sd.cvs.eff.12, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.12, 
                  biomasses=d12$biomass.g_perhec, 
                  losses=d12$losses, 
                  efficiencies=d12$efficiencies) 
  sd.cvs.los.12=c(sd.cvs.los.12,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.12,
                  biomasses=d12$biomass.g_perhec,
                  losses=d12$losses,
                  efficiencies=d12$efficiencies)
  sd.cvs.mat.12=c(sd.cvs.mat.12,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.12 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.12 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.12 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.13,
                  biomasses=d13$biomass.g_perhec,
                  losses=d13$losses,
                  efficiencies=d13$efficiencies)
  sd.cvs.eff.13=c(sd.cvs.eff.13, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.13, 
                  biomasses=d13$biomass.g_perhec, 
                  losses=d13$losses, 
                  efficiencies=d13$efficiencies) 
  sd.cvs.los.13=c(sd.cvs.los.13,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.13,
                  biomasses=d13$biomass.g_perhec,
                  losses=d13$losses,
                  efficiencies=d13$efficiencies)
  sd.cvs.mat.13=c(sd.cvs.mat.13,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.13 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.13 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.13 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.14,
                  biomasses=d14$biomass.g_perhec,
                  losses=d14$losses,
                  efficiencies=d14$efficiencies)
  sd.cvs.eff.14=c(sd.cvs.eff.14, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.14, 
                  biomasses=d14$biomass.g_perhec, 
                  losses=d14$losses, 
                  efficiencies=d14$efficiencies) 
  sd.cvs.los.14=c(sd.cvs.los.14,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.14,
                  biomasses=d14$biomass.g_perhec,
                  losses=d14$losses,
                  efficiencies=d14$efficiencies)
  sd.cvs.mat.14=c(sd.cvs.mat.14,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.14 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.14 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.14 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.15,
                  biomasses=d15$biomass.g_perhec,
                  losses=d15$losses,
                  efficiencies=d15$efficiencies)
  sd.cvs.eff.15=c(sd.cvs.eff.15, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.15, 
                  biomasses=d15$biomass.g_perhec, 
                  losses=d15$losses, 
                  efficiencies=d15$efficiencies) 
  sd.cvs.los.15=c(sd.cvs.los.15,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.15,
                  biomasses=d15$biomass.g_perhec,
                  losses=d15$losses,
                  efficiencies=d15$efficiencies)
  sd.cvs.mat.15=c(sd.cvs.mat.15,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.15 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), xaxt = 'n',
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =1, at = c(0,.02,.04,.06,.08,.10,.12), labels = F)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.15 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.15 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.16,
                  biomasses=d16$biomass.g_perhec,
                  losses=d16$losses,
                  efficiencies=d16$efficiencies)
  sd.cvs.eff.16=c(sd.cvs.eff.16, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.16, 
                  biomasses=d16$biomass.g_perhec, 
                  losses=d16$losses, 
                  efficiencies=d16$efficiencies) 
  sd.cvs.los.16=c(sd.cvs.los.16,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.16,
                  biomasses=d16$biomass.g_perhec,
                  losses=d16$losses,
                  efficiencies=d16$efficiencies)
  sd.cvs.mat.16=c(sd.cvs.mat.16,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.16 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1),
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.16 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.16 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.17,
                  biomasses=d17$biomass.g_perhec,
                  losses=d17$losses,
                  efficiencies=d17$efficiencies)
  sd.cvs.eff.17=c(sd.cvs.eff.17, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.17, 
                  biomasses=d17$biomass.g_perhec, 
                  losses=d17$losses, 
                  efficiencies=d17$efficiencies) 
  sd.cvs.los.17=c(sd.cvs.los.17,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.17,
                  biomasses=d17$biomass.g_perhec,
                  losses=d17$losses,
                  efficiencies=d17$efficiencies)
  sd.cvs.mat.17=c(sd.cvs.mat.17,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.17 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), 
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.17 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.17 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.18,
                  biomasses=d18$biomass.g_perhec,
                  losses=d18$losses,
                  efficiencies=d18$efficiencies)
  sd.cvs.eff.18=c(sd.cvs.eff.18, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.18, 
                  biomasses=d18$biomass.g_perhec, 
                  losses=d18$losses, 
                  efficiencies=d18$efficiencies) 
  sd.cvs.los.18=c(sd.cvs.los.18,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.18,
                  biomasses=d18$biomass.g_perhec,
                  losses=d18$losses,
                  efficiencies=d18$efficiencies)
  sd.cvs.mat.18=c(sd.cvs.mat.18,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.18 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), 
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.18 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.18 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 
mtext(side =1, 'Variation in parameters', line = 2)

for (var in seq(0, 0.12, 0.01)){
  cat('var: ', var, '\n')
  # efficiencies 
  res=sensitivity(fluxing, 'efficiencies', var,50,
                  mat=mat.19,
                  biomasses=d19$biomass.g_perhec,
                  losses=d19$losses,
                  efficiencies=d19$efficiencies)
  sd.cvs.eff.19=c(sd.cvs.eff.19, mean(res[[2]], na.rm=T))
  #for losses
  res=sensitivity(fluxing,'losses',var,50, 
                  mat=mat.19, 
                  biomasses=d19$biomass.g_perhec, 
                  losses=d19$losses, 
                  efficiencies=d19$efficiencies) 
  sd.cvs.los.19=c(sd.cvs.los.19,mean(res[[2]],na.rm=T))
  #for preferences
  res=sensitivity(fluxing, 'mat',var,50, 
                  mat=mat.19,
                  biomasses=d19$biomass.g_perhec,
                  losses=d19$losses,
                  efficiencies=d19$efficiencies)
  sd.cvs.mat.19=c(sd.cvs.mat.19,mean(res[[2]],na.rm=T))
}

# Preference matrix # 
plot(sd.cvs.eff.19 ~ seq.eff, xlim = c(0,0.12) , ylim = c(0,0.1), 
     xlab = 'variation in parameters', 
     ylab = 'observed departure to original results', 
     pch = 19)
axis(side =2, at = c(0, 0.02, 0.04, .06,.08,.1), labels = F)
points(sd.cvs.los.19 ~ seq.eff, col = '#7fc97f', pch = 19) 
points(sd.cvs.mat.19 ~ seq.eff, col = '#beaed4', pch = 19) 
abline(a = 0, b = 1, lty = 2) 


