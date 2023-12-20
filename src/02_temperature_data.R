library(tidyverse)
library(ncdf4)
library(here)

#NetCDF file 1
wtr_tmp1 <- nc_open(here('data/01_predicted_temp_N24-53_W98-126.nc'))

site_id1 <- ncvar_get(wtr_tmp1, 'site_id')

lat1 <- ncvar_get(wtr_tmp1, 'lat')

lon1 <- ncvar_get(wtr_tmp1, 'lon')

elevation1 <- ncvar_get(wtr_tmp1, 'alt')

temp1 <- ncvar_get(wtr_tmp1, 'surftemp') #NOTE: the dimensions of temp1 are temp1[time,site_id]

#NetCDF file 2

wtr_tmp2 <- nc_open(here('data/02_predicted_temp_N40-53_W67-98.nc'))

site_id2 <- ncvar_get(wtr_tmp1, 'site_id')

elevation2 <- ncvar_get(wtr_tmp1, 'alt')

temp2 <- ncvar_get(wtr_tmp1, 'surftemp')

#NetCDF file 3

wtr_tmp3 <- nc_open(here('data/03_predicted_temp_N24-40_W67-98.nc'))

site_id3 <- ncvar_get(wtr_tmp1, 'site_id')

elevation3 <- ncvar_get(wtr_tmp1, 'alt')

temp3 <- ncvar_get(wtr_tmp1, 'surftemp')




#Ok I think that I know what to do:
# 1. Join hydro_lakes with nhdhr using Michael's dataset
# 2. Then filter the above three data sets one at a time by subsetting with the 
#    filtered 'site_id' variable
#    i.e., you filter the 'site_id' variable using the joined hydrolakes and nhdhr dataset
# 3. Then the data should be small enough to join together. Especially if I run a 
#    MK test on each site and only keep the trend. Then we'd be back down to 
#    the number of variables in the data from script 01
##NOTE: the dimensions of temp1 are temp1[time,site_id]
#   That means that we can subset temp1,2,3 by filtering the site_id value first
#   So I first limit the site_id based on the Hylak_id that we use. 
#   Then, I can limit the size of temp1,2,3 using those site_id1,2,3 subsets. 
#   Then, I can convert temp1,2,3 to get their time series
#   After that, they should be limited enough to bind them all together and compare
#   them to the browning trends.