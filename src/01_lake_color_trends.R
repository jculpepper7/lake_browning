# The goal of this script is to:
# 1. Rad in the LimnoSat-US data 
# Citation for LimnoSat-US data: 
# Topp, S. N., Pavelsky, T. M., Yang, X., Ross, M. R. V., & Gardner, J. (2020). 
# LimnoSat-US: A remote sensing dataset for U.S. Lakes from 1984–2020. 
# Zenodo. https://doi.org/10.5281/zenodo.4139695



# Libraries ---------------------------------------------------------------

#general reading, cleaning, manipulation, etc
library(tidyverse)
library(here)
library(lubridate)
library(janitor)

#Time series trend test
library(wql) 
#Opted for wql over "trend" or "Kendall" because it gives the mann-kendall p-value 
#and sen's slope in one test that you can extract easily

#Libraries for mapping
library(sf)
library(sp)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)

# 1. Import LimnoSat data -------------------------------------------------

limnosat <- read_csv(here('data/LimnoSat/LimnoSat_20200628.csv')) %>% 
  select(2,3,5:8,16,17)


# 2. Clean and trim data --------------------------------------------------

# This file is too large to work with, so I'm going to cut it down in size

limnosat <- limnosat %>% 
  group_by(Hylak_id) %>% 
  arrange(Hylak_id, year)

# Remove lakes that never reach the brown threshold.
# Essentially, if a lake is <569 nm, then we eliminate the time series

ls_browning <- limnosat %>% 
  group_by(Hylak_id) %>%
  filter(any(dWL >= 569)) 

#How many lakes (Hylak_id) remain?
n_distinct(limnosat$Hylak_id) #56,792
n_distinct(ls_browning$Hylak_id) #46,443, so we eliminated 10,349


#Seems high, and looking over the data, it seems that there are some spikes
#I'll try a yearly average to smooth the data a bit and we'll try again

ls_mean <- limnosat %>%
  select(Hylak_id, year, dWL) %>% 
  group_by(Hylak_id, year) %>% 
  summarise(
    dWL_mean = mean(dWL)
  )
n_distinct(ls_mean$Hylak_id) #56,792 -- same number as limnosat, so that's good.

ls_browning_mean <- ls_mean %>% 
  group_by(Hylak_id) %>%
  filter(any(dWL_mean >= 569))

n_distinct(ls_browning_mean$Hylak_id) #19,705 -- Much more manageable value for now

# 3. Water color trends ---------------------------------------------------

# Use a Mann-Kendall test to determine trends

ls_mk <- ls_browning_mean %>% 
  group_by(Hylak_id) %>% 
  summarize(color_sen = mannKen(dWL_mean)$sen.slope,
            color_pval = mannKen(dWL_mean)$p.value)
  
n_distinct(ls_mk %>% filter(color_sen > 0))
n_distinct(ls_mk %>% filter(color_sen < 0))
n_distinct(ls_mk %>% filter(color_sen == 0))


# 4. Visualize trend data -------------------------------------------------

# Density plot of the trends

ggplot(data = ls_mk)+
  geom_density(aes(color_sen), fill = 'grey50')+
  geom_vline(xintercept = 0)+
  geom_vline(xintercept = median(ls_mk$color_sen, na.rm = T), linetype = 'dashed')+
  xlim(c(-3,3))+
  theme_classic()+
  xlab('Color Trend (nm/yr)')+
  ylab('')+
  theme(
    text = element_text(size = 25)
  )
  

#ggsave(here('results/color_trend_density_plt.png'), dpi = 300, units = 'in', width = 8, height = 5)

#Density plot of trends, but only keep significant p values

ggplot(data = ls_mk %>% filter(color_pval<=0.05))+
  geom_density(aes(color_sen), fill = 'grey50')+
  geom_vline(xintercept = 0)+
  geom_vline(xintercept = -0.25, linetype = 'dashed')+
  xlim(c(-3,3))+
  theme_classic()+
  xlab('Color Trend (nm/yr)')+
  ylab('')+
  theme(
    text = element_text(size = 25)
  )

#ggsave(here('results/color_trend_density_significantTrends_plt.png'), dpi = 300, units = 'in', width = 8, height = 5)

#Density plot of trends for positive values

ggplot(data = ls_mk %>% filter(color_pval<=0.05, color_sen>0))+
  geom_density(aes(color_sen), fill = 'grey50')+
  #geom_vline(xintercept = 0)+
  geom_vline(xintercept = 0.45, linetype = 'dashed')+
  xlim(c(-1,3))+
  theme_classic()+
  xlab('Color Trend (nm/yr)')+
  ylab('')+
  theme(
    text = element_text(size = 25)
  )

#ggsave(here('results/color_trend_density_pos_plt.png'), dpi = 300, units = 'in', width = 8, height = 5)

test <- ls_mk %>% filter(color_pval<=0.05, color_sen>0)
median(test$color_sen)


# 5. Maps of trends -------------------------------------------------------

#Load HydroLAKES data
hydro_lakes <- read_csv(here('data/HydroLAKES_points_v10.csv')) %>% 
  clean_names() %>%
  filter(continent == "North America") %>% 
  #select(-1, -23) %>% 
  mutate(
    hylak_id = as.factor(hylak_id)
  ) %>% 
  rename(Hylak_id = hylak_id)

#Join data
#Convert Hylak_id to factor in ls_mk dataset

ls_mk_hl <- ls_mk %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id)
  ) %>% 
  inner_join(hydro_lakes, by = 'Hylak_id') %>% 
  filter(color_sen < 3,
         color_sen > -3)

#Map of mk trends

#Load map data
na <- rnaturalearth::ne_states(
  returnclass = "sf") 

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))

#Map code
color_trend_map <- ggplot() +
  ggplot2::geom_sf(data = na) +
  geom_sf(color = "#2b2b2b", fill = "white", size=0.125)+
  coord_sf(xlim = c(-140, -55), ylim = c(25, 50), expand = FALSE)+
  #coord_sf(crs = st_crs("+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"), datum = NA) +
  #coord_sf(datum = NA) +
  ggthemes::theme_map()+
  geom_point(data = ls_mk_hl %>% filter(color_pval >= 0.05), aes(x = pour_long, y = pour_lat, fill = color_sen), size = 2, pch =21, stroke = 0.5)+
  geom_point(data = ls_mk_hl %>% filter(color_pval <= 0.05), aes(x = pour_long, y = pour_lat, fill = color_sen), stroke = 0.5, pch = 24, size = 2)+
  scale_fill_gradient2(midpoint = 0, low = '#2158bc', mid = 'white', high = '#9f4d04', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("")+
  ylab("")+
  #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
  #labs(fill = '', tag = 'A')+
  theme(
    legend.position = 'bottom',
    #legend.key.height = unit(0.4, 'in'),
    #aspect.ratio = 1
    #plot.margin = unit(c(0, 0, 0, 1), 'in')
  )

color_trend_map

#ggsave(here('results/NA_color_trend_map.png'), dpi = 300, units = 'in', width = 20, height = 15)

#This doesn't look right
#Will try to plot the full dataset

ls_small <- limnosat %>% 
  group_by(Hylak_id) %>% 
  arrange(Hylak_id, year) %>% 
  select(1)

ls_small2 <- ls_small %>% 
  summarise(
    Hylak_id = mean(Hylak_id)
  ) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id)
  )

hl_small <- hydro_lakes %>% 
  select(Hylak_id, pour_lat, pour_long) %>% 
  mutate(
    Hylak_id = as.factor(Hylak_id)
  )
  
ls_hl_small <- ls_small %>%
  mutate(
    Hylak_id = as.factor(Hylak_id)
  ) %>% 
  inner_join(hl_small)

  
color_trend_map <- ggplot() +
  ggplot2::geom_sf(data = na) +
  #coord_sf(xlim = c(-125, -103), ylim = c(25, 50), expand = FALSE)+
  geom_point(data = limnosat, aes(x = pour_long, y = pour_lat, fill = color_sen), size = 2, pch =21, stroke = 0.5)+
  #geom_point(data = ls_mk %>% filter(color_pval <= 0.05), aes(x = pour_long, y = pour_lat, fill = color_sen), stroke = 0.5, pch = 24, size = 2)+
  scale_fill_gradient2(midpoint = 0, low = 'blue', mid = 'white', high = 'red', space = 'Lab')+
  #scale_colour_gradient(low = 'blue', high = 'red', space = 'Lab')+
  xlab("")+
  ylab("")+
  
  #labs(fill = 'Ice On Trend \nSlope Magnitude \nDays/yr')
  labs(fill = '', tag = 'A')+
  theme(
    legend.position = 'right',
    legend.key.height = unit(0.4, 'in'),
    aspect.ratio = 1
    #plot.margin = unit(c(0, 0, 0, 1), 'in')
  )



