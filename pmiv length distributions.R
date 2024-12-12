# Trout Length Distributions # 

# Pelagic miv Length data # 
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Trout")
setwd("J:/Box Sync/Butts_Scripts/Trout")
pmiv_length = read_csv('trout_lengthdat.csv') %>% 
  mutate(year = substr(sampleid, 3, 6), 
         depth_of_sample = substr(sampleid, 10, 11), 
         MDD = substr(sampleid, 7, 9)) %>% 
  unite('date', MDD, year, remove = T, sep = '')
pmiv_length # Measure in millimeters 

# Adjust date column to put in slashes 
# write_csv(pmiv_length, 'trout_lengthdat_dateadjust.csv')
setwd("J:/Box Sync/Butts_Scripts/Trout")
setwd("C:/Users/tjbut/Box Sync/Butts_Scripts/Trout")
pmiv_length = read_csv('trout_lengthdat_dateadjust.csv')
pmiv_length

# split by species and year # ===================================
length_dist = pmiv_length %>%
  select(date, taxon, measurement, length) %>%
  mutate(taxon = tolower(taxon)) %>%
  mutate(date = mdy(date)) %>%
  mutate(year4 = year(date)) %>%
  select(date, year4, taxon, measurement, length)
length_dist

## leptodora distribution ##================================= 
lept_dist = length_dist %>% 
  filter(taxon == 'leptodora')
lept_dist

## chaoborus distribution ##==============================
chaob_dist.bl = length_dist %>%
  filter(taxon == 'chaoborus') %>%
  filter(measurement == 'BL')
chaob_dist

## bythotrephes distribution ##=========================
byth_dist.bl = length_dist %>%
  filter(taxon == 'bythotrephes') %>% 
  filter(measurement == 'BL')
byth_dist.bl

byth_dist.sl = length_dist %>%
  filter(taxon == 'bythotrephes') %>%
  filter(measurement == 'SL')
byth_dist.sl

# Ridge plots # =======================
library(ggridges)

## Leptodora ## 
lept_dist = mutate(lept_dist, year4 = rev(as.factor(year4))) %>% 
  drop_na()
lept_dist

p1 = ggplot(
  lept_dist, 
  aes(y = year4)) +
  geom_density_ridges( # Create the density plots for Leptodora size distribution 
    aes(x = length, fill = year4), jittered_points = FALSE, 
    alpha = .8, color = "black", size = 1, quantile_lines=TRUE, quantile_fun=function(x,...)mean(x)) +
  labs( # Axis labels 
    x = "Length (mm)",
    y = "Day of Year, 2019"
  ) +
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) + # Present data in a log scale
  # annotation_logticks(sides = 'b') +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text = element_text(size =11, colour = 'black')) + 
  xlab(label = bquote(Length~(mm))) + 
  ylab(label = 'Year') +
  scale_fill_cyclical( # Create a gradient color scheme to differentiate time
    labels = c(`length` = "Leptodora Size"),
    values = c('#008a64', # 2001
               '#009d73',
                # 2003
               '#00c48f',
               '#00d89d',
               '#00ecac',
               '#00ffba',
               '#14ffbf',
               '#27ffc5', # 2009
               '#3bffca',
               '#4fffcf',
               '#62ffd4', 
               '#76ffda',
                # 2014
                # 2015
               '#a4009f', 
               '#b900b4',
                #2018
               '#cf00ca',
               '#e500df',
               '#fa00f4',
               '#ff11f9'),
    name = "measure", guide = "legend") 
p1

# Bythotrephes ##=========================
## Leptodora ## 
byth_dist.bl = mutate(byth_dist.bl, year4 = rev(as.factor(year4))) %>% 
  drop_na()
byth_dist.bl

p2 = ggplot(
  byth_dist.bl, 
  aes(y = year4)) +
  geom_density_ridges( # Create the density plots for Leptodora size distribution 
    aes(x = length, fill = year4), jittered_points = FALSE, 
    alpha = .8, color = "black", size = 1, quantile_lines=TRUE, quantile_fun=function(x,...)mean(x)) +
  labs( # Axis labels 
    x = "Length (mm)",
    y = "Day of Year, 2019"
  ) +
  # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #               labels = trans_format("log10", math_format(10^.x))) + # Present data in a log scale
  # annotation_logticks(sides = 'b') +
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) + 
  theme(axis.text = element_text(size =11, colour = 'black')) + 
  xlab(label = bquote(Length~(mm))) + 
  ylab(label = 'Year') +
  scale_fill_cyclical( # Create a gradient color scheme to differentiate time
    labels = c(`length` = "Leptodora Size"),
    values = c('#008a64', # 2001
               '#009d73',
                # 2003
               '#00c48f',
               '#00d89d',
               '#00ecac',
               '#00ffba',
               '#14ffbf',
               '#27ffc5', # 2009
               '#3bffca',
               '#4fffcf',
               '#62ffd4', 
               '#76ffda',
                # 2014
                # 2015
               '#a4009f', 
               '#b900b4',
                #2018
               '#cf00ca',
               '#e500df',
               '#fa00f4',
               '#ff11f9'),
    name = "measure", guide = "legend") 
p2
