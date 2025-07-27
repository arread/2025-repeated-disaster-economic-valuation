# Setup -------------------------------------------------------------------

library(tidyverse)
library(foreign)
library(readstata13)
library(mice)
library(MatchThem)


#load the following files:
final_data <- readRDS("data/CLEANED.RDS")
final_long <- readRDS("data/LONG.RDS")
final_wide <- readRDS("data/WIDE.RDS")


# Imputation -----------------------------------------------------


#Please note: While I used R for the majority of my work, 
#I used STATA for the actual imputation process. 
#This is the code for prepping, exporting, and importing the imputed datasets. 
#Refer to the provided .do file for the actual imputation code.


p_missing <- unlist(lapply(final_wide, function(x) sum(is.na(x))))/nrow(final_wide)
sort(p_missing[p_missing >0], decreasing = TRUE)


final_for_imp <- as.data.frame(final_wide)


final_for_imp <- final_for_imp[rowSums(is.na(final_for_imp))!=143,]
final_for_imp <- final_for_imp %>% mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(., -Inf)))


write.dta(final_for_imp, "data/final_for_imp.dta")


#at this point, transfer to STATA and use the .do file "Imputation.do" to impute
#then return and proceed from here for re-import and matching


#importing imputed data back from STATA

imp <- as.data.frame(read.dta13("data/final_imputed_flong.dta"))
impb <- imp %>% 
  mutate(impID = `_mi_m`, state = substr(COUNTY, 1,2)) %>% 
  select(-c(`_mi_miss`, `_mi_m`, `_mi_id`))


impb$binaryTx <- ifelse(impb$Tx_80 == 1, 1, 
                        ifelse(impb$Tx_90 == 1, 1,
                               ifelse(impb$Tx_00 == 1, 1, 0)))

#specifying the outcome to be taken AFTER the treatment
#(need to set it up this way for the model to run correctly)
impb <- impb %>% 
  mutate(out_70 = propown_80, 
         out_80 = propown_90,
         out_90 = propown_00,
         out_00 = propown_10,
         valdv_70 = val_80,
         valdv_80 = val_90,
         valdv_90 = val_00,
         valdv_00 = val_10,
         rentdv_70 = rent_80,
         rentdv_80 = rent_90,
         rentdv_90 = rent_00,
         rentdv_00 = rent_10,
         occdv_70 = occ_80,
         occdv_80 = occ_90,
         occdv_90 = occ_00,
         occdv_00 = occ_10,
         unitdv_70 = unit_80,
         unitdv_80 = unit_90,
         unitdv_90 = unit_00,
         unitdv_00 = unit_10,
         ptrend = (pop_80 - pop_70),
         .id = Geo_FIPS)

impmidswide <- as.mids(impb, .imp = "impID", .id = ".id")


# Matching ----------------------------------------------------------------


#To Use: pop, propyng, propwht, propblk, prophisp, proppov, inc, occ, hage,
# propown, proprur, mbl, val, rent, logarea, ptrend


m.out1 <- matchthem(binaryTx ~ pop_80 + propyng_80 + propwht_80 + propblk_80 + prophisp_80 + proppov_80 + inc_80 + occ_80 + hage_80 + propown_80 + proprur_80 + mbl_80 + val_80 + rent_80 + logarea_80 + ptrend,
                    impmidswide, approach = "within", method = "nearest", 
                    distance = "glm", replace = TRUE, exact = ~state, 
                    drop.unmatched = TRUE)


summary(m.out1)


saveRDS(m.out1, file="data/matching_output.RDS")
