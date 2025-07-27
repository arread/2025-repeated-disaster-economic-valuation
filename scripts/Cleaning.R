# Setup -------------------------------------------------------------------

#packages
library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)

# Loading data tables -----------------------------------------------------

census_1970 <- readRDS("data/census_1970.rds")
census_1980 <- readRDS("data/census_1980.rds")
census_1990 <- readRDS("data/census_1990.rds")
census_2000 <- readRDS("data/census_2000.rds")
census_2010 <- readRDS("data/census_2010.rds")
torn <- readRDS("data/torn.rds")
tract_geo <- readRDS("data/tract_geo.rds")


# Cleaning census data -----------------------------------------------------

#restricting to POPULATED tracts with OCCUPIED HOUSING UNITS and dropping water-only tracts (land area of zero)

#occupied units - 70, 80, 90, 00, 10
census_1970 <- census_1970 %>% 
  filter(pop_70 != 0 | is.na(pop_70)) %>% 
  filter(occ_70 != 0 | is.na(occ_70))

census_1980 <- census_1980 %>% 
  filter(pop_80 != 0 | is.na(pop_80)) %>% 
  filter(occ_80 != 0| is.na(occ_80)) %>% 
  filter(area_80 != 0 | is.na(area_80))

census_1990 <- census_1990 %>% 
  filter(pop_90 != 0 | is.na(pop_90)) %>% 
  filter(occ_90 != 0| is.na(occ_90)) %>% 
  filter(area_90 != 0 | is.na(area_90))

census_2000 <- census_2000 %>% 
  filter(pop_00 != 0 | is.na(pop_00)) %>% 
  filter(occ_00 != 0| is.na(occ_00)) %>% 
  filter(area_00 != 0 | is.na(area_00))

census_2010 <- census_2010 %>% 
  filter(pop_10 != 0 | is.na(pop_10)) %>% 
  filter(occ_10 != 0| is.na(occ_10)) %>% 
  filter(area_10 != 0 | is.na(area_10))

#area - 80, 90, 00, 10 (logging area)
census_1980 <- census_1980 %>% 
  mutate(logarea_80 = log(area_80)) 
census_1990 <- census_1990 %>% 
  mutate(logarea_90 = log(area_90)) 
census_2000 <- census_2000 %>% 
  mutate(logarea_00 = log(area_00)) 
census_2010 <- census_2010 %>% 
  mutate(logarea_10 = log(area_10)) 

#racial composition - 70, 80, 90, 00, 10
census_1970 <- census_1970 %>% 
  mutate(prophisp_70 = (hisp_70 / pop_70), 
         propwht_70 = (wht_70 / pop_70), 
         propblk_70 = (blk_70 / pop_70))
census_1980 <- census_1980 %>% 
  mutate(prophisp_80 = (hisp_80 / pop_80), 
         propwht_80 = (wht_80 / pop_80), 
         propblk_80 = (blk_80 / pop_80))
census_1990 <- census_1990 %>% 
  mutate(prophisp_90 = (hisp_90 / pop_90), 
         propwht_90 = (wht_90 / pop_90), 
         propblk_90 = (blk_90 / pop_90))
census_2000 <- census_2000 %>% 
  mutate(prophisp_00 = (hisp_00 / pop_00), 
         propwht_00 = (wht_00 / pop_00), 
         propblk_00 = (blk_00 / pop_00))
census_2010 <- census_2010 %>% 
  mutate(prophisp_10 = (hisp_10 / pop_10), 
         propwht_10 = (wht_10 / pop_10), 
         propblk_10 = (blk_10 / pop_10))


#housing age - 80, 90, 00 - fixing instances where the calculation took YYYY-NA for original NAs
census_1980 <- census_1980 %>% 
  mutate(hage_80 = ifelse(hage_80 == 1980, NA, hage_80))
census_1990 <- census_1990 %>% 
  mutate(hage_90 = ifelse(hage_90 == 1990, NA, hage_90))
census_2000 <- census_2000 %>% 
  mutate(hage_00 = ifelse(hage_00 == 2000, NA, hage_00))


#rented units - 70, 80, 90, 00, 10
census_1970 <- census_1970 %>% 
  mutate(proprnt_70 = (rntd_70 / occ_70))
census_1980 <- census_1980 %>% 
  mutate(proprnt_80 = (rntd_80 / occ_80))
census_1990 <- census_1990 %>% 
  mutate(proprnt_90 = (rntd_90 / occ_90))
census_2000 <- census_2000 %>% 
  mutate(proprnt_00 = (rntd_00 / occ_00))
census_2010 <- census_2010 %>% 
  mutate(proprnt_10 = (rntd_10 / occ_10))


#owned units - 70, 80, 90, 00, 10
census_1970 <- census_1970 %>% 
  mutate(propown_70 = (own_70 / occ_70))
census_1980 <- census_1980 %>% 
  mutate(propown_80 = (own_80 / occ_80))
census_1990 <- census_1990 %>% 
  mutate(propown_90 = (own_90 / occ_90))
census_2000 <- census_2000 %>% 
  mutate(propown_00 = (own_00 / occ_00))
census_2010 <- census_2010 %>% 
  mutate(propown_10 = (own_10 / occ_10))


#persons in poverty - 80, 90, 00
census_1980 <- census_1980 %>% 
  mutate(proppov_80 = (pov_80 / pop_80))
census_1990 <- census_1990 %>% 
  mutate(proppov_90 = (pov_90 / pop_90))
census_2000 <- census_2000 %>% 
  mutate(proppov_00 = (pov_00 / pop_00))


#young population - 80, 90, 00
census_1980 <- census_1980 %>% 
  mutate(propyng_80 = (yng_80 / pop_80))
census_1990 <- census_1990 %>% 
  mutate(propyng_90 = (yng_90 / pop_90))
census_2000 <- census_2000 %>% 
  mutate(propyng_00 = (yng_00 / pop_00))


#rural - 80, 90, 00
census_1980 <- census_1980 %>% 
  mutate(proprur_80 = (rur_80 / pop_80))
census_1990 <- census_1990 %>% 
  mutate(proprur_90 = (rur_90 / per_90))
census_2000 <- census_2000 %>% 
  mutate(proprur_00 = (rur_00 / pop_00))

# merging census
census <- left_join(census_1970, census_1980, by = "Geo_FIPS")
census <- left_join(census, census_1990, by = "Geo_FIPS")
census <- left_join(census, census_2000, by = "Geo_FIPS")
census <- left_join(census, census_2010, by = "Geo_FIPS")
glimpse(census)
rm(census_1970, census_1980, census_1990, census_2000, census_2010)

#To Use: logarea, pop, proprur, propblk, propwht, prophisp, hage, mbl, inc, proppov, propyng, proprnt, propown, unit, rent, val


# Merging tornado data ---------------------------------------------------


#joining tornadoes to 2010 geographies

geo_torn <- st_join(tract_geo, torn, join = st_intersects, 
                    suffix = c(.geo, .torn), left = TRUE)
glimpse(geo_torn)
rm(torn, tract_geo)

#coding binary yes/no experienced tornado

geo_torn$binaryTx <- as.numeric(ifelse(is.na(geo_torn$om), "0", "1"))
glimpse(geo_torn)

#mark tracts exposed 1970 to 1979

geo_torn$prior_exp <- as.numeric(ifelse(geo_torn$yr <= 1979, "1", "0"))
glimpse(geo_torn)


#restrict to tracts NOT exposed 1970 to 1980

geo_torn <- geo_torn %>%
  group_by(GEO_ID) %>%
  filter(!any(prior_exp == 1, na.rm = TRUE))


#seeing how many treated and untreated tracts I have
#treated (6213)
geo_torn %>%
  group_by(GEO_ID) %>%
  filter(binaryTx == 1) 


#untreated (42704)
geo_torn %>%
  group_by(GEO_ID) %>%
  filter(binaryTx == 0) 




#trimming down to vars I need


geo_torn <- geo_torn %>% 
  select(GEO_ID,
         CENSUSAREA,
         STATE,
         COUNTY,
         TRACT,
         om,
         yr,
         date,
         time,
         mag,
         inj,
         fat,
         loss,
         slat,
         slon,
         elat,
         elon,
         len,
         wid,
         fc,
         binaryTx,
         prior_exp,
         geometry) %>%
  rename(torn_date = date)

glimpse(geo_torn)

# Merging tornadoes and census ---------------------------------------


#in geo_torn, GEO ID is 1400000USSTCTYTRACT# where ST is 2-digit state code, CTY is 3 digit county code, and TRACT# is 6 digit tract code
geo_torn$Geo_FIPS <- as.numeric(substr(geo_torn$GEO_ID, start=10, stop=20))
final_data <- left_join(geo_torn, census, by="Geo_FIPS")

rm(census, geo_torn)

#population trend 1970-1980

final_data <- final_data %>% 
  mutate(ptrend = (pop_80 - pop_70))

# Creating exposure variables ---------------------------------------------


#time period of exposure
final_data$Tx_80 <- ifelse(final_data$binaryTx == 1 & final_data$yr <= 1989, 1, 0)
final_data$Tx_90 <- ifelse(final_data$binaryTx == 1 & final_data$yr >= 1990 & final_data$yr <= 1999, 1, 0)
final_data$Tx_00 <- ifelse(final_data$binaryTx == 1 & final_data$yr >= 2000 & final_data$yr <= 2009, 1, 0)


#frequency/dosage

final_data <- final_data %>% 
  add_count(Geo_FIPS, wt = Tx_80) %>% 
  rename(freq_80 = n)


final_data <- final_data %>% 
  add_count(Geo_FIPS, wt = Tx_90) %>% 
  rename(freq_90 = n)


final_data <- final_data %>% 
  add_count(Geo_FIPS, wt = Tx_00) %>% 
  rename(freq_00 = n)


# Finishing up -----------------------------------------


#trimming
final_data <- final_data %>% 
  select(-GEO_ID, -CENSUSAREA, -om, -time, -fc, -prior_exp, -torn_date)
glimpse(final_data)

#getting final sample size (6213 treated)
final_data %>% 
  filter(binaryTx == 1) %>%
  group_by(Geo_FIPS)

# 42704 untreated
final_data %>% 
  filter(binaryTx == 0) %>% 
  group_by(Geo_FIPS)

# 48917 total
final_data %>% 
  group_by(Geo_FIPS)


#formatting

final_data$COUNTY <- paste(final_data$STATE, final_data$COUNTY, sep = "_")

p_all <- final_data %>% select(c(COUNTY, Geo_FIPS, pop_70, wht_70, blk_70,
                                 hisp_70, inc_70, occ_70, val_70, rent_70,
                                 own_70, rntd_70, prophisp_70, propblk_70,
                                 propwht_70, proprnt_70, propown_70, unit_70,
                                 area_80, pop_80, proprur_80, rur_80, hisp_80,
                                 blk_80, wht_80, inc_80, hage_80, occ_80, own_80,
                                 rntd_80, mbl_80, rent_80, val_80, pov_80, yng_80,
                                 logarea_80, prophisp_80, propwht_80, propblk_80,
                                 proprnt_80, propown_80, proppov_80, propyng_80,
                                 unit_80, pop_90, inc_90, hage_90, occ_90,
                                 own_90, rntd_90,rur_90,  mbl_90, val_90, rent_90,
                                 proprur_90, pov_90, area_90, wht_90, blk_90,
                                 hisp_90, yng_90, logarea_90, prophisp_90,
                                 propwht_90, propblk_90, proprnt_90, propown_90,
                                 proppov_90, propyng_90, unit_90, pop_00, inc_00,
                                 occ_00, own_00, rntd_00, mbl_00, val_00,
                                 rent_00, rur_00, area_00, proprur_00, wht_00,
                                 blk_00,hisp_00, yng_00, pov_00, hage_00,
                                 logarea_00, prophisp_00, propwht_00, propblk_00,
                                 proprnt_00, propown_00, proppov_00, propyng_00,
                                 unit_00, pop_10, occ_10, own_10, rntd_10,
                                 area_10, wht_10, blk_10, hisp_10, yng_10,
                                 logarea_10, prophisp_10, propwht_10, propblk_10,
                                 proprnt_10, propown_10, unit_10, val_10, rent_10,
                                 ptrend)) %>% 
  filter(duplicated(Geo_FIPS) == FALSE) %>% 
  as.data.frame() %>% 
  select(-"geometry")


t_80 <- final_data %>% 
  select(Geo_FIPS, mag, Tx_80, freq_80) %>% 
  filter(Tx_80 == 1) %>% 
  group_by(Geo_FIPS) %>% 
  mutate(mag_80 = max(mag, na.rm = TRUE)) %>% 
  select(-c("mag"))%>% 
  as.data.frame(filter(duplicated(Geo_FIPS) == FALSE)) %>% 
  select(-"geometry")

t_80 <- distinct(t_80, Geo_FIPS, Tx_80, freq_80, mag_80, .keep_all = TRUE)

t_90 <- final_data %>% 
  select(Geo_FIPS, mag, Tx_90, freq_90) %>% 
  filter(Tx_90 == 1)%>% 
  group_by(Geo_FIPS) %>% 
  mutate(mag_90 = max(mag, na.rm = TRUE)) %>% 
  select(-c("mag"))%>% 
  as.data.frame(filter(duplicated(Geo_FIPS) == FALSE)) %>% 
  select(-"geometry")

t_90 <- distinct(t_90, Geo_FIPS, Tx_90, freq_90, mag_90, .keep_all=TRUE)


t_00 <- final_data %>% 
  select(Geo_FIPS, mag,Tx_00, freq_00) %>% 
  filter(Tx_00 == 1) %>% 
  group_by(Geo_FIPS) %>% 
  mutate(mag_00 = max(mag, na.rm = TRUE)) %>% 
  select(-c("mag")) %>% 
  as.data.frame(filter(duplicated(Geo_FIPS) == FALSE)) %>% 
  select(-"geometry")

t_00 <- distinct(t_00, Geo_FIPS, Tx_00, freq_00, mag_00, .keep_all=TRUE)


torn <- full_join(t_80, t_90, by="Geo_FIPS")
torn <- full_join(torn, t_00, by="Geo_FIPS")

rm(t_80, t_90, t_00)

torn <- torn %>% 
  filter(duplicated(Geo_FIPS) == FALSE) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>% 
  pivot_longer(
    !Geo_FIPS,
    names_to = c(".value", "Yr"),
    names_sep = "_"
  ) 


p_all <- p_all %>% 
  pivot_longer(
    !c(Geo_FIPS, COUNTY, ptrend),
    names_to = c(".value", "Yr"),
    names_sep = "_"
  ) %>% mutate(FIPS_YR = paste(Geo_FIPS,Yr, sep = "_"))


torn <- torn %>% mutate(FIPS_YR = paste(Geo_FIPS, Yr, sep = "_"))


all <- left_join(p_all, torn, by = "FIPS_YR")


all <- all %>% select(-c("Yr.y", "Geo_FIPS.y")) %>% 
  rename(Yr=Yr.x, Geo_FIPS=Geo_FIPS.x) %>% 
  mutate_at(vars(c(Tx, freq, mag)), ~replace(., is.na(.), 0))


final_long <- all


final_wide <- final_long %>% 
  select(-FIPS_YR) %>% 
  pivot_wider(
    names_from = Yr,
    names_sep = "_",
    values_from = c(area, pop, proprur, hisp, rur, blk, wht, inc, hage, unit,
                    occ, own, rntd, mbl, rent, val, pov, yng, logarea, prophisp,
                    propwht, propblk, inc, proprnt, propown, proppov, propyng,
                    Tx, freq, mag))


final_wide <- final_wide %>% 
  select(-c(area_70, proprur_70, proprur_10, rur_70, rur_10, inc_10, hage_70,
            hage_10, mbl_70, mbl_10, pov_70, pov_10, yng_70,  yng_10, logarea_70,
            proppov_70, proppov_10, propyng_70, propyng_10, mag_70, mag_10))


## saving cleaned data files


saveRDS(final_data, file="data/CLEANED.RDS")
saveRDS(final_long, file="data/LONG.RDS")
saveRDS(final_wide, file="data/WIDE.RDS")
