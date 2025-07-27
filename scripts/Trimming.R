#Setup

library(tidyverse)

#load data

m.out1 <- readRDS("data/matching_output.RDS")

# Format and trim dataset for analysis ----------------------------------------


m.data <- complete(m.out1, "long")


#assigning treatment period
m.data$firstp <- ifelse(m.data$Tx_80 == 1, 2, 
                        ifelse(m.data$Tx_90 == 1 & m.data$Tx_80 == 0, 3,
                               ifelse(m.data$Tx_00 == 1 & m.data$Tx_80 == 0 & m.data$Tx_90 == 0, 4, 0)))


final_for_analysis <- m.data  %>% 
  pivot_longer(
    !c(.imp, .id, Geo_FIPS, ptrend, firstp, state, COUNTY, weights, binaryTx, distance),
    names_to=c(".value", "Yr"),
    names_sep = "_"
  ) %>% mutate_at(vars(c(Tx, mag, freq)), ~replace(., is.na(.), 0))


final_for_analysis$period <- ifelse(final_for_analysis$Yr == "70", 1, 
                                    ifelse(final_for_analysis$Yr == "80", 2, 
                                           ifelse(final_for_analysis$Yr == "90", 3, 
                                                  ifelse(final_for_analysis$Yr == "00", 4, 5))))


final_for_analysis <- final_for_analysis %>% 
  mutate(.imp = (.imp - 1), iduniq = seq(length.out = nrow(final_for_analysis)))

#taking out year 10 since I'm only using it as an output year
#and I already defined the yr 10 output measurements to be included in the yr 2000 subset
#but saving a copy with year 10 for summary tables later

saveRDS(m.out1, file="data/final_with_yr_10.RDS")
final_for_analysis <- final_for_analysis %>% filter(Yr != "10")

rm(m.out1, m.data)

#removing variables I won't need in my models (only used for matching and
#to generate other variables)
final_trimmed <- final_for_analysis %>% 
  select(-c(area, own, logarea, proprnt, rur, proprur, hage, rntd, mbl, pov,
            yng, hisp, blk, wht, proppov, propyng))
final_trimmed <- final_trimmed %>% 
  mutate(.imp = .imp + 1)

rm(final_for_analysis)


# Interactions variables and standardization ---------------------------------------------------------

#Standardizing racial comp and income
final_trimmed <- final_trimmed %>%  mutate(zinc = (inc - mean(inc)) / sd(inc), 
                                           zwht = (propwht - mean(propwht)) / sd(propwht), 
                                           zblk = (propblk - mean(propblk)) / sd(propblk),
                                           zhis = (prophisp - mean(prophisp)) / sd(prophisp))


final_trimmed$zincint <- ifelse(final_trimmed$zinc <= -1, -1, 
                                ifelse(final_trimmed$zinc > -1 & final_trimmed$zinc < 1, 0,
                                       ifelse(final_trimmed$zinc >= 1, 1, NA)))


final_trimmed$zwhtint <- ifelse(final_trimmed$zwht <= -1, -1, 
                                ifelse(final_trimmed$zwht > -1 & final_trimmed$zwht < 1, 0,
                                       ifelse(final_trimmed$zwht >= 1, 1, NA)))


final_trimmed$zblkint <- ifelse(final_trimmed$zblk <= -1, -1, 
                                ifelse(final_trimmed$zblk > -1 & final_trimmed$zblk < 1, 0,
                                       ifelse(final_trimmed$zblk >= 1, 1, NA)))


final_trimmed$zhisint <- ifelse(final_trimmed$zhis <= -1, -1, 
                                ifelse(final_trimmed$zhis > -1 & final_trimmed$zhis < 1, 0,
                                       ifelse(final_trimmed$zhis >= 1, 1, NA)))

#Assigning unique label ids by first period and interaction variable category
wide <- final_trimmed %>% 
  pivot_wider(
    id_cols = c(Geo_FIPS, .imp, .id, ptrend, state, binaryTx, weights, distance,
                firstp, COUNTY),
    names_from = Yr,
    names_sep = "_",
    values_from = c(pop, inc, unit, occ, rent, val, prophisp, propwht, propblk,
                    propown, Tx, freq, mag, out, valdv, rentdv, occdv, unitdv,
                    zinc, zwht, zblk, zhis, zincint, zwhtint, zblkint, zhisint,
                    period, iduniq))

#defining subsamples for treatment

wide$firstpmag <- ifelse(wide$firstp==0, 0,
                         ifelse(wide$firstp==2 & wide$mag_80==2, 2,
                                ifelse(wide$firstp==3 & wide$mag_90==2, 3,
                                       ifelse(wide$firstp==4 & wide$mag_00==2, 4,
                                              ifelse(wide$firstp==2 & wide$mag_80==3, 5,
                                                     ifelse(wide$firstp==3 & wide$mag_90==3, 6,
                                                            ifelse(wide$firstp==4 & wide$mag_00==3, 7,
                                                                   ifelse(wide$firstp==2 & wide$mag_80>=4, 8,
                                                                          ifelse(wide$firstp==3 & wide$mag_90>=4, 9,
                                                                                 ifelse(wide$firstp==4 & wide$mag_00>=4, 10, NA))))))))))

wide$mag2 <- ifelse(wide$firstp == 2 & wide$mag_80 == 2 & wide$mag_90 <= 2 & wide$mag_00 <= 2, 2,
                    ifelse(wide$firstp == 3 & wide$mag_90 == 2 & wide$mag_00 <= 2, 3,
                           ifelse(wide$firstp == 4 & wide$mag_00 == 2, 4, 
                                  ifelse(wide$firstp == 0, 0, NA))))


wide$mag3 <- ifelse(wide$firstp == 2 & wide$mag_80 == 3 & wide$mag_90 <= 3 & wide$mag_00 <= 3, 2,
                    ifelse(wide$firstp == 3 & wide$mag_90 == 3 & wide$mag_00 <= 3, 3,
                           ifelse(wide$firstp == 4 & wide$mag_00 == 3, 4, 
                                  ifelse(wide$firstp == 0, 0, NA))))


wide$mag4 <- ifelse(wide$firstp == 2 & wide$mag_80 >= 4, 2,
                    ifelse(wide$firstp == 3 & wide$mag_90 >= 4, 3,
                           ifelse(wide$firstp == 4 & wide$mag_00 >= 4, 4,
                                  ifelse(wide$firstp == 0, 0, NA))))

wide$firstpfreq <- ifelse(wide$firstp == 0, 0,
                          ifelse(wide$firstp == 2 & wide$freq_80 == 1, 2,
                                 ifelse(wide$firstp==3 & wide$freq_90==1, 3,
                                        ifelse(wide$firstp==4 & wide$freq_00==1, 4,
                                               ifelse(wide$firstp==2 & wide$freq_80>=2, 5,
                                                      ifelse(wide$firstp==3 & wide$freq_90>=2, 6,
                                                             ifelse(wide$firstp==4 & wide$freq_00>=2, 7, NA)))))))

wide$freq1 <- ifelse(wide$firstp==0, 0,
                     ifelse(wide$firstp==2 & wide$Tx_80==1 & wide$Tx_90==0 & wide$Tx_00==0, 2, 
                            ifelse(wide$firstp==2 & wide$Tx_80==0 & wide$Tx_90==1 & wide$Tx_00==0, 2,
                                   ifelse(wide$firstp==2 & wide$Tx_80==0 & wide$Tx_90==0 & wide$Tx_00==1, 2,
                                          ifelse(wide$firstp==3 & wide$Tx_90==1 & wide$Tx_00==0, 3, 
                                                 ifelse(wide$firstp==3 & wide$Tx_90==0 & wide$Tx_00==1, 3,
                                                        ifelse(wide$firstp==4 & wide$Tx_00==1, 4, NA)))))))


wide$freq2 <- ifelse(wide$firstp==0, 0,
                     ifelse(wide$firstp==2 & wide$Tx_80==1 & wide$Tx_90==1 & wide$Tx_00==0, 2,
                            ifelse(wide$firstp==2 & wide$Tx_80==1 & wide$Tx_90==0 & wide$Tx_00==1, 2,
                                   ifelse(wide$firstp==2 & wide$Tx_80==0 & wide$Tx_90==1 & wide$Tx_00==1, 2,
                                          ifelse(wide$firstp==2 & wide$Tx_80==1 & wide$Tx_90==1 & wide$Tx_00==1, 2,
                                                 ifelse(wide$firstp==3 & wide$Tx_90==1 & wide$Tx_00==1, 3, NA))))))




#defining subsamples for income level and racial composition
wide$firstpinc <- ifelse(wide$firstp==0, 0,
                         ifelse(wide$firstp==2 & wide$zincint_80==-1, 2,
                          ifelse(wide$firstp==3 & wide$zincint_90==-1, 3,
                           ifelse(wide$firstp==4 & wide$zincint_00==-1, 4,
                            ifelse(wide$firstp==2 & wide$zincint_80==0, 5,
                             ifelse(wide$firstp==3 & wide$zincint_90==0, 6,
                              ifelse(wide$firstp==4 & wide$zincint_00==0, 7,
                               ifelse(wide$firstp==2 & wide$zincint_80==1, 8,
                                ifelse(wide$firstp==3 & wide$zincint_90==1, 9,
                                 ifelse(wide$firstp==4 & wide$zincint_00==1, 10, NA))))))))))

wide$firstpwht <- ifelse(wide$firstp==0, 0,
                   ifelse(wide$firstp==2 & wide$zwhtint_80==-1, 2,
                    ifelse(wide$firstp==3 & wide$zwhtint_90==-1, 3,
                     ifelse(wide$firstp==4 & wide$zwhtint_00==-1, 4,
                      ifelse(wide$firstp==2 & wide$zwhtint_80==0, 5,
                       ifelse(wide$firstp==3 & wide$zwhtint_90==0, 6,
                        ifelse(wide$firstp==4 & wide$zwhtint_00==0, 7,
                         ifelse(wide$firstp==2 & wide$zwhtint_80==1, 8,
                          ifelse(wide$firstp==3 & wide$zwhtint_90==1, 9,
                           ifelse(wide$firstp==4 & wide$zwhtint_00==1, 10, NA))))))))))


wide$firstpblk <- ifelse(wide$firstp==0, 0,
                   ifelse(wide$firstp==2 & wide$zblkint_80==-1, 2,
                    ifelse(wide$firstp==3 & wide$zblkint_90==-1, 3,
                     ifelse(wide$firstp==4 & wide$zblkint_00==-1, 4,
                      ifelse(wide$firstp==2 & wide$zblkint_80==0, 5,
                       ifelse(wide$firstp==3 & wide$zblkint_90==0, 6,
                        ifelse(wide$firstp==4 & wide$zblkint_00==0, 7,
                         ifelse(wide$firstp==2 & wide$zblkint_80==1, 8,
                          ifelse(wide$firstp==3 & wide$zblkint_90==1, 9,
                           ifelse(wide$firstp==4 & wide$zblkint_00==1, 10, NA))))))))))


wide$firstphisp <- ifelse(wide$firstp==0, 0,
                    ifelse(wide$firstp==2 & wide$zhisint_80==-1, 2,
                     ifelse(wide$firstp==3 & wide$zhisint_90==-1, 3,
                      ifelse(wide$firstp==4 & wide$zhisint_00==-1, 4,
                       ifelse(wide$firstp==2 & wide$zhisint_80==0, 5,
                        ifelse(wide$firstp==3 & wide$zhisint_90==0, 6,
                         ifelse(wide$firstp==4 & wide$zhisint_00==0, 7,
                          ifelse(wide$firstp==2 & wide$zhisint_80==1, 8,
                           ifelse(wide$firstp==3 & wide$zhisint_90==1, 9,
                            ifelse(wide$firstp==4 & wide$zhisint_00==1, 10, NA))))))))))


#pivoting back out long
final_trimmed <- wide %>% 
  pivot_longer(
    !c(Geo_FIPS, COUNTY, ptrend, .imp, .id, ptrend, state, binaryTx, weights, distance, firstp, COUNTY,
       firstpinc, firstpwht, firstpblk, firstphisp, freq1, freq2, firstpfreq, firstpmag, mag2, mag3, mag4),
    names_to=c(".value", "Yr"),
    names_sep = "_"
  ) 




#stratifying by income level
final_trimmed$lowinc <- ifelse(final_trimmed$firstpinc==0, 0, 
                               ifelse(final_trimmed$firstpinc==2, 2,
                                      ifelse(final_trimmed$firstpinc==3, 3, 
                                             ifelse(final_trimmed$firstpinc==4, 4, NA))))




final_trimmed$midinc <- ifelse(final_trimmed$firstpinc==0, 0,
                               ifelse(final_trimmed$firstpinc==5, 2,
                                      ifelse(final_trimmed$firstpinc==6, 3,
                                             ifelse(final_trimmed$firstpinc==7, 4, NA))))


final_trimmed$highinc <- ifelse(final_trimmed$firstpinc==0, 0,
                                ifelse(final_trimmed$firstpinc==8, 2,
                                       ifelse(final_trimmed$firstpinc==9, 3,
                                              ifelse(final_trimmed$firstpinc==10, 4, NA))))




#stratifying by proportion white
final_trimmed$lowwht <- ifelse(final_trimmed$firstpwht==0, 0, 
                               ifelse(final_trimmed$firstpwht==2, 2,
                                      ifelse(final_trimmed$firstpwht==3, 3, 
                                             ifelse(final_trimmed$firstpwht==4, 4, NA))))
final_trimmed$midwht <- ifelse(final_trimmed$firstpwht==0, 0,
                               ifelse(final_trimmed$firstpwht==5, 2,
                                      ifelse(final_trimmed$firstpwht==6, 3,
                                             ifelse(final_trimmed$firstpwht==7, 4, NA))))


final_trimmed$highwht <- ifelse(final_trimmed$firstpwht==0, 0,
                                ifelse(final_trimmed$firstpwht==8, 2,
                                       ifelse(final_trimmed$firstpwht==9, 3,
                                              ifelse(final_trimmed$firstpwht==10, 4, NA))))


#stratifying by proportion black
final_trimmed$lowblk <- ifelse(final_trimmed$firstpblk==0, 0, 
                               ifelse(final_trimmed$firstpblk==2, 2,
                                      ifelse(final_trimmed$firstpblk==3, 3, 
                                             ifelse(final_trimmed$firstpblk==4, 4, NA))))
final_trimmed$midblk <- ifelse(final_trimmed$firstpblk==0, 0,
                               ifelse(final_trimmed$firstpblk==5, 2,
                                      ifelse(final_trimmed$firstpblk==6, 3,
                                             ifelse(final_trimmed$firstpblk==7, 4, NA))))


final_trimmed$highblk <- ifelse(final_trimmed$firstpblk==0, 0,
                                ifelse(final_trimmed$firstpblk==8, 2,
                                       ifelse(final_trimmed$firstpblk==9, 3,
                                              ifelse(final_trimmed$firstpblk==10, 4, NA))))


#stratifying by proportion hispanic
final_trimmed$lowhisp <- ifelse(final_trimmed$firstphisp==0, 0, 
                                ifelse(final_trimmed$firstphisp==2, 2,
                                       ifelse(final_trimmed$firstphisp==3, 3, 
                                              ifelse(final_trimmed$firstphisp==4, 4, NA))))
final_trimmed$midhisp <- ifelse(final_trimmed$firstphisp==0, 0,
                                ifelse(final_trimmed$firstphisp==5, 2,
                                       ifelse(final_trimmed$firstphisp==6, 3,
                                              ifelse(final_trimmed$firstphisp==7, 4, NA))))


final_trimmed$highhisp <- ifelse(final_trimmed$firstphisp==0, 0,
                                 ifelse(final_trimmed$firstphisp==8, 2,
                                        ifelse(final_trimmed$firstphisp==9, 3,
                                               ifelse(final_trimmed$firstphisp==10, 4, NA))))


#stratifying by magnitude
final_trimmed$mag2 <- ifelse(final_trimmed$firstpmag==0, 0, 
                             ifelse(final_trimmed$firstpmag==2, 2,
                                    ifelse(final_trimmed$firstpmag==3, 3, 
                                           ifelse(final_trimmed$firstpmag==4, 4, NA))))
final_trimmed$mag3 <- ifelse(final_trimmed$firstpmag==0, 0,
                             ifelse(final_trimmed$firstpmag==5, 2,
                                    ifelse(final_trimmed$firstpmag==6, 3,
                                           ifelse(final_trimmed$firstpmag==7, 4, NA))))


final_trimmed$mag4 <- ifelse(final_trimmed$firstpmag==0, 0,
                             ifelse(final_trimmed$firstpmag==8, 2,
                                    ifelse(final_trimmed$firstpmag==9, 3,
                                           ifelse(final_trimmed$firstpmag==10, 4, NA))))


#stratifying by frequency
final_trimmed$freq1 <- ifelse(final_trimmed$firstpfreq==0, 0, 
                              ifelse(final_trimmed$firstpfreq==2, 2,
                                     ifelse(final_trimmed$firstpfreq==3, 3, 
                                            ifelse(final_trimmed$firstpfreq==4, 4, NA))))
final_trimmed$freq2 <- ifelse(final_trimmed$firstpfreq==0, 0,
                              ifelse(final_trimmed$firstpfreq==5, 2,
                                     ifelse(final_trimmed$firstpfreq==6, 3,
                                            ifelse(final_trimmed$firstpfreq==7, 4, NA))))

final_trimmed <- final_trimmed %>% 
  select(-c(COUNTY, zinc, zwht, zblk, zhis, zincint, zwhtint, zblkint, zhisint))

glimpse(final_trimmed)

#saving long version - will use this one for analysis
saveRDS(final_trimmed, file="data/final_trimmed_long.RDS")

#saving a wide ver. as well
trimmed_wide <- final_trimmed %>% 
  pivot_wider(
    id_cols = c(Geo_FIPS, .imp, .id, ptrend, state, binaryTx, weights, distance, firstp, firstpinc, firstpwht, firstpblk, firstphisp, firstpmag, mag2, mag3, mag4, firstpfreq, freq1, freq2, lowinc, midinc, highinc, lowwht, midwht, highwht, lowblk, midblk, highblk, lowhisp, midhisp, highhisp),
    names_from = Yr,
    names_sep = "_",
    values_from = c(pop, inc, unit, occ, rent, val, prophisp, propwht, propblk, propown, Tx, freq, mag, out, valdv, rentdv, occdv, unitdv, period, iduniq))


glimpse(trimmed_wide)


saveRDS(trimmed_wide, file="data/final_trimmed_wide.RDS")
