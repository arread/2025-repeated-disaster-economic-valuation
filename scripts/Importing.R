# Setup -------------------------------------------------------------------


#packages
library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)

#other setup
options(tigris_use_cache = TRUE)
if (!dir.exists("data")) dir.create("data")


#setting list of states to include

state_codes <- c("AL", "AZ", "AR", "CO", "FL", "GA", "IL", "IN", "IA",
                 "KS", "KY", "LA", "MA", "MI", "MN", "MS", "MO", "MT",
                 "NE", "NY", "NC", "ND", "OH", "OK", "PA", "SC", "SD",
                 "TN", "TX", "VA", "WV", "WI")


# Load census tract geographies -----------------------------------


#GEOGRAPHIES (2010 census tracts, counties, states)
tract_geo <- map_df(state_codes, ~tracts(state = .x, cb = TRUE, year = 2010))
glimpse(tract_geo)
saveRDS(tract_geo, file = "data/tract_geo.rds")


# Load NOAA SVRGIS tornado tracks ------------------------------------

#SVRGIS
temp_noaa <- tempfile()
torn_shp <- download.file("https://www.spc.noaa.gov/gis/svrgis/zipped/1950-2024-torn-aspath.zip", temp_noaa)
zip::unzip(zipfile = temp_noaa, exdir = "data/", junkpaths = TRUE)
torn_all <- sf::st_read("data/1950-2024-torn-aspath.shp")

#restricting to 1970 to 2020
torn <- torn_all %>%
  filter(yr >= 1970 & yr <= 2010, na.rm = TRUE)
glimpse(torn)

#restricting to F2 and above
torn <- torn %>%
  filter(mag >= 2, na.rm = TRUE)
glimpse(torn)

#fixing coordinate reference system to match census
st_crs(tract_geo)
torn <- st_set_crs(torn, 4269)
torn <- st_transform(torn, 4269)
st_crs(torn)

saveRDS(torn, file = "data/torn.rds")
saveRDS(torn_all, file = "data/torn_all.rds")


# Loading Social Explorer census and ACS tables ------------------

# 1970 Decennial census on 2010 boundaries

census_1970 <- read.csv("data/1970 on 2010.csv")
census_1970 <- census_1970[-1, ]
census_1970 <-  census_1970 %>%
  rename(Geo_FIPS = FIPS,
         pop_70 = Total.Population,
         wht_70 = White,
         blk_70 = Black,
         inc_70 = Median.Family.Income.in.2010.Dollars..Dollars.adjusted.for.inflation.to.match.value.in.2010.,
         occ_70 = Total.Occupied,
         unit_70 = Total.Housing.Units,
         own_70 = Total.Occupied..Owner.Occupied,
         rntd_70 = Total.Occupied..Renter.Occupied,
         val_70 = Count.of.Units.for.Which.Value.Is.Tabulated..Median.Value..Dollars.adjusted.for.inflation.to.match.value.in.2010.,
         rent_70 = Specified.Renter.Occupied.Paying.Cash.Rent..Median.Gross.Rent..Dollars.adjusted.for.inflation.to.match.value.in.2010.,
         hisp_70 = Count.of.Spanish.American.Persons) %>%
  select(Geo_FIPS, pop_70, wht_70, blk_70, hisp_70, occ_70, unit_70,
         rntd_70, own_70, rent_70, val_70, inc_70) %>%
  mutate_if(is.character, as.numeric)

glimpse(census_1970)

saveRDS(census_1970, file = "data/census_1970.rds")

# 1980 Decennial census on 2010 boundaries

census_1980 <- read.csv("data/1980 on 2010.csv")
census_1980 <- census_1980[-1, ]
census_1980 <- census_1980 %>%
  rename(Geo_FIPS = FIPS,
         area_80 = Area..Land.,
         pop_80 = Total.Population,
         rur_80 = Rural,
         pop8018 = Total.Population..18.to.24.Years,
         pop8025 = Total.Population..25.to.34.Years,
         hisp_80 = Total.Population..Persons.of.Spanish.Origin,
         blk_80 = Total.Population..Persons.not.of.Spanish.Origin..Black,
         wht_80 = Total.Population..Persons.not.of.Spanish.Origin..White,
         inc_80 = Median.Household.Income.in.1979.Dollars,
         hage_80 = Median.Year.Structure.Built,
         unit_80 = Total.Housing.Units,
         occ_80 = Occupied.Housing.Units,
         own_80 = Occupied.Housing.Units..Owner.Occupied,
         rntd_80 = Occupied.Housing.Units..Renter.Occupied,
         mbl_80 = Total.Housing.Units..Mobile.Home.or.Trailer..Etc.,
         rent_80 = Specified.Renter.Occupied.Paying.Cash.Rent..Median.Gross.Rent,
         val_80 = Specified.Owner.Occupied.Noncondominium.Housing.Units..Median.Value..STF1.,
         pov_80 = Population.for.Whom.Poverty.Status.is.Determined..Below.Poverty.Level) %>%
  select(Geo_FIPS, area_80, pop_80, rur_80, pop8018, pop8025, hisp_80, blk_80,
         wht_80, inc_80, hage_80, unit_80, occ_80, own_80, rntd_80, mbl_80,
         rent_80, val_80, pov_80)

census_1980$pop8018 <- as.numeric(census_1980$pop8018)
census_1980$pop8025 <- as.numeric(census_1980$pop8025)


census_1980 <- census_1980 %>%
  mutate(yng_80 = (pop8018 + pop8025))
census_1980 <- census_1980 %>%
  select(-c(pop8018, pop8025))

census_1980 <- census_1980 %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(hage_80 = 1980 - hage_80)

glimpse(census_1980)

saveRDS(census_1980, file = "data/census_1980.rds")


# 1990 Decennial census on 2010 boundaries


census_1990 <- read.csv("data/1990 on 2010.csv")
census_1990 <- census_1990[-1, ]
census_1990 <- census_1990 %>%
  rename(Geo_FIPS = FIPS,
         area_90 = Area..Land.,
         per_90 = Persons,
         rur_90 = Persons..Rural,
         pop_90 = Total.Population,
         inc_90 = Median.Household.Income.in.1989.Dollars,
         hage_90 = Median.Year.Structure.Built,
         unit_90 = Housing.units,
         occ_90 = Occupied.housing.units,
         own_90 = Occupied.housing.units..Owner.occupied,
         rntd_90 = Occupied.housing.units..Renter.occupied,
         mbl_90 = Housing.units..Mobile.home.or.trailer,
         val_90 = Specified.Owner.Occupied.Housing.Units..Median.Value,
         rent_90 = Specified.Renter.Occupied.Paying.Cash.Rent..Median.Gross.Rent,
         pov_90 = Persons.for.whom.poverty.status.is.determined..Income.in.1989.below.poverty.level,
         age18 = Persons..18.to.24.years,
         age24 = Persons..25.to.34.years,
         wht_90 = Total.Population..Non.Hispanic..White,
         blk_90 = Total.Population..Non.Hispanic..Black,
         hisp_90 = Total.Population..Hispanic) %>%
  select(Geo_FIPS, area_90, per_90, rur_90, pop_90, inc_90, hage_90, unit_90,
         occ_90, own_90, rntd_90, mbl_90, val_90, rent_90, pov_90, age18,
         age24, wht_90, blk_90, hisp_90) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(yng_90 = (age18 + age24)) %>%
  select(-c("age18", "age24"))


census_1990 <- census_1990 %>%
  mutate(hage_90 = 1990 - hage_90)
glimpse(census_1990)

saveRDS(census_1990, file = "data/census_1990.rds")


# 2000 Decennial census on 2010 boundaries


census_2000 <- read.csv("data/2000 on 2010.csv")
census_2000 <- census_2000[-1, ]


census_2000 <- census_2000 %>%
  rename(Geo_FIPS = FIPS,
         pop_00 = Total.Population,
         inc_00 = Median.Household.Income.in.1999.Dollars,
         unit_00 = Housing.Units,
         occ_00 = Occupied.Housing.Units,
         own_00 = Occupied.Housing.Units..Owner.Occupied,
         rntd_00 = Occupied.Housing.Units..Renter.Occupied,
         mbl_00 = Housing.Units..Mobile.Home,
         val_00 = Owner.Occupied.Housing.Units..Median.Value,
         rent_00 = Specified.Renter.Occupied.Paying.Cash.Rent..Median.Gross.Rent,
         area_00 = Area..Land.,
         rur_00 = Rural,
         age18 = X18.to.24.Years,
         age24 = X25.to.34.Years,
         wht_00 = Not.Hispanic.or.Latino..White.Alone,
         blk_00 = Not.Hispanic.or.Latino..Black.or.African.American.Alone,
         hisp_00 = Hispanic.or.Latino,
         pov_00 = Population.for.Whom.Poverty.Status.is.Determined..Income.in.1999.Below.Poverty.Level,
         hage_00 = Median.Year.Structure.Built) %>%
  select(Geo_FIPS, pop_00, inc_00, unit_00, occ_00, own_00, rntd_00,
         mbl_00, val_00, rent_00, area_00, rur_00, age18, age24, wht_00,
         blk_00, hisp_00, pov_00, hage_00) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(yng_00 = (age18 + age24)) %>%
  mutate(hage_00 = 2000 - hage_00) %>%
  select(-c("age18", "age24"))

saveRDS(census_2000, file = "data/census_2000.rds")



#2010


census_2010 <- read.csv("data/2010.csv")
census_2010 <- census_2010[-1, ]


census_2010 <- census_2010 %>%
  rename(Geo_FIPS = FIPS,
         pop_10 = Total.Population,
         occ_10 = Occupied.housing.units,
         own_10 = Occupied.housing.units..Owner.Occupied,
         rntd_10 = Occupied.housing.units..Renter.occupied,
         area_10 = Area..Land.,
         age18 = Total.Population..18.to.24.years,
         age25 = Total.Population..25.to.34.years,
         wht_10 = Total.population..Not.Hispanic.or.Latino..White.alone,
         blk_10 = Total.population..Not.Hispanic.or.Latino..Black.or.African.American.alone,
         hisp_10 = Total.population..Hispanic.or.Latino,
         rur_10 = Total.population..Rural) %>%
  select(Geo_FIPS, pop_10, occ_10, own_10, rntd_10, area_10, age18,
         age25, wht_10, blk_10, hisp_10, rur_10) %>%
  mutate_if(is.character, as.numeric) %>%
  mutate(yng_10 = (age18 + age25)) %>%
  select(-c("age18", "age25"))

glimpse(census_2010)


#2010 ACS


acs_2010 <- read.csv("data/acs_2010_2014.csv")
acs_2010 <- acs_2010[-1, ]
acs_2010 <- acs_2010 %>%
  rename(Geo_FIPS = FIPS,
         val_10 = Median.Value,
         rent_10 = Median.Gross.Rent,
         unit_10 = Housing.Units.) %>%
  select(Geo_FIPS, val_10, rent_10, unit_10) %>%
  mutate_if(is.character, as.numeric)

glimpse(acs_2010)

census_2010 <- left_join(census_2010, acs_2010, by = "Geo_FIPS")
glimpse(census_2000)
rm(acs_2010)

saveRDS(census_2010, file = "data/census_2010.rds")
