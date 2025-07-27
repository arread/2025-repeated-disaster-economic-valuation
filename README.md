# PROJECT: Repeated disaster and the economic valuation of place: temporal dynamics of tornado effects on housing prices in the United States, 1980-2010
Paper citation: Read, A. (2025). Repeated disaster and the economic valuation of place: Temporal dynamics of tornado effects on housing prices in the United States, 1980–2010. Population and Environment, 47(3), 29. https://doi.org/10.1007/s11111-025-00502-w

## Notes
I am publishing this code as-is (July 2025), meaning that there are places where the code could be refined and commented out more robustly. I may add comments at a later date, but the point of this repository is to show what I did to produce the paper, so I will not be editing the code otherwise. Cleaner, more efficient versions of the code will be published under other repositories in the future as part of work I'm doing to extend and add to the analyses in this paper (for other projects).

Please also note that this code was originally run in January 2023; I used a late 2022 version of the `did` package.

## Data Sources
- Census 1970-2010; pulled from Social Explorer (harmonized to 2010 boundaries)
- ACS 2010-2014 (for 2010 estimates) from Social Explorer
- SVRGIS tornado tracks 1970-2010

## Data Access
To obtain the data, you will need:

### Census data

I used the following downloaded historical tables/variables from [Social Explorer](https://www.socialexplorer.com/home/dataset-entry/us-census-data):

- 1970 on 2010 geographies: 
  - SE_T011_001: Total population,
  - SE_T011_002: Total population - White,
  - SE_T011_003: Total population - Black,
  - ORG_H063_001: Count of Spanish American Persons,
  - SE_T501_001: Median family income in 2010 dollars,
  - SE_T099_001: Total housing units,
  - SE_T100_001: Total housing units - Total occupied,
  - SE_T100_001: Total housing units - Total occupied - owner occupied,
  - SE_T100_003: Total housing units - Total occupied - renter occupied,
  - SE_T163_001: Count of units for which value is tabulated - median value - 2010 dollars,
  - SE_T167_001: Specified renter occupied paying cash rent - median gross rent - 2010 dollars
  
- 1980 on 2010 geographies:
  - Geo_AREALAND: Land area,
  - SE_T001_001: Total population,
  - SE_T003_003: Total population - Rural,
  - SE_T007_006: Total population - 18-24 years,
  - SE_T007_007: Total population - 25-34 years,
  - SE_T013_007: Total population - persons of Spanish origin,
  - SE_T013_004: Total population - persons not of Spanish origin - Black,
  - SE_T013_003: Total population - persons not of Spanish origin - White,
  - SE_T107_001: Median household income in 2010 dollars,
  - SE_T160_001: Median year structure built,
  - SE_T069_001: Total housing units,
  - SE_T066_001: Occupied housing units,
  - SE_T066_002: Occupied housing units - owner occupied,
  - SE_T066_003: Occupied housing units - renter occupied,
  - SE_T069_008: Total housing units - mobile home or trailer etc.,
  - SE_T167_001: Specified renter occupied - median gross rent - 2010 dollars,
  - SE_T164_001: Specified owner occupied - median value - 2010 dollars,
  - SE_T082_003: Population for whom poverty status is determined - below poverty level
  
- 1990 on 2010 geographies:
  - Geo_AREALAND: Land area,
  - SE_T001_001: Total population
  - SE_T003_001: Persons (used for denominator of proportion rural), 
  - SE_T003_005: Persons - rural,
  - SE_T088_001: Median household income in 1989 dollars - 2010 dollars,
  - SE_T160_001: Median year structure built, 
  - SE_T056_001: Housing units,
  - SE_T053_001: Occupied housing units,
  - SE_T053_002: Occupied housing units - owner occupied,
  - SE_T053_003: Occupied housing units - renter occupied,
  - SE_T056_011: Housing units - mobile home or trailer,
  - SE_T164_001: Specified owner occupied housing units - median value - 2010 dollars,
  - SE_T167_001: Specified renter occupied housing units - median gross rent - 2010 dollars,
  - SE_T065_002: Persons for whom poverty status is determined - income in 1989 below poverty level,
  - SE_T007_006: Persons age 18-24,
  - SE_T007_007: Persons age 25-34,
  - SE_T012_003: Total population - non Hispanic white,
  - SE_T012_004: Total population - non Hispanic Black,
  - SE_T012_008: Total population - Hispanic (any race)
  
- 2000 on 2010 geographies:
  - SE_T001_001: Total population,
  - SE_T091_001: Median household income - 2010 dollars,
  - SE_T159_001: Housing units,
  - SE_T156_001: Occupied housing units,
  - SE_T156_002: Occupied housing units - owner occupied,
  - SE_T156_003: Occupied housing units - renter occupied,
  - SE_159_011: Housing units - mobile home,
  - SE_T163_001: Owner occupied housing units - median value - 2010 dollars,
  - SE_T167_001: Specified renter occupied paying cash rent - median gross rent - 2010 dollars,
  - Geo_AREALAND: Land area,
  - SE_T003_005: Total population - Rural,
  - SE_T007_006: Total population - 18-24 years,
  - SE_T007_007: Total population - 25-34 years,
  - SE_T013_003: Total population - Not Hispanic or Latino - White alone,
  - SE_T013_004: Total population - Not Hispanic or Latino - Black or African American alone,
  - SE_T013_010: Total population - Hispanic or Latino,
  - SE_T160_001: Median year structure built,
  - RC2000SF3_008_P087002: Population for whom poverty status is determined - income in 1999 below poverty level
  
- 2010:
  - SE_T001_001: Total population,
  - SE_T068_001: Housing units,
  - SE_T069_001: Occupied housing units,
  - SE_T069_002: Occupied housing units - owner occupied,
  - SE_T069_003: Occupied housing units - renter occupied,
  - Geo_AREALAND: Land area,
  - SE_T008_006: Total population - 18-24 years
  - SE_T008_007: Total population - 25-34 years
  - SE_T055_003: Total population - Not Hispanic or Latino - White alone
  - SE_T055_004: Total population - Not Hispanic or Latino - Black or African American alone
  - SE_T055_010: Total population - Hispanic or Latino
  - SF1_P0020005: Total population - Rural
  
- ACS 2010-2014: 
  - SE_A10036_001: Median house value for all owner-occupied housing units
  - SE_A18009_001: Median gross rent
  - SE_A10032_001: Housing units

NOTE: When you export the Social Explorer data, make sure to select the options to 1) adjust for inflation to 2010 dollars, 2) keep descriptive variable names, 3) use compatible variable names, 4) keep all geographic identifiers

### Tornado data

These are available from [NOAA/NWS Storm Prediction Center SVRGIS](https://www.spc.noaa.gov/gis/svrgis/). You can either go to the website and download the most recent tornado paths dataset, or use the code in the `Importing.R` script to automatically fetch the data.

## Tools
The vast majority of this analysis was done in R/R Studio. One part of the analysis (imputation) was done in STATA. Although I would prefer to rewrite the code to do the imputation in R as well, I will leave that for future projects. The purpose of publishing this code is to show what I actually did to produce the paper, which in this case included using STATA for a portion of the analysis. The .do file is included in the Scripts folder.

## Scripts
Run scripts in this order:

1. `Importing.R` - Ingests the data from the social explorer tables and the SVRGIS tornado paths, and imposes highest level data filters (selected variables; range of years; minimum tornado magnitude)
2. `Cleaning.R` - Generates the merged dataset and cleans/restricts/calculates variables used
3. `Imputation.do` - (run in STATA) imputes missing values due to geography shifts in 1970-1980
4. `Matching.R` - Obtains the matched control sample used in analysis
6. `Trimming.R` - Generates the final data frame for analysis
5. `Analysis.R` - Runs the difference-in-difference models used in analysis
6. `Tables.R` - Generates the descriptive/summary tables used in the paper
