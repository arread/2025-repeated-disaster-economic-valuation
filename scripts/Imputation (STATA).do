
clear all
set more off
set maxvar 32000

///////////////////////////////// STEP ONE /////////////////////////////////////


**variables to impute

_rmcoll  ptrend pop_70 hisp_70 blk_70 wht_70 inc_70 unit_70 occ_70 own_70 rntd_70 rent_70 val_70 prophisp_70 propwht_70 propblk_70 proprnt_70 propown_70 hage_80 pop_80 proprur_80 hisp_80 rur_80 blk_80 wht_80 inc_80 unit_80 occ_80 own_80 rntd_80 mbl_80 rent_80 val_80 pov_80 yng_80 prophisp_80 propwht_80 propblk_80 proprnt_80 propown_80 proppov_80 propyng_80 rent_10 val_10 area_10 pop_10 hisp_10 blk_10 wht_10 unit_10 occ_10 own_10 rntd_10 logarea_10 prophisp_10 propwht_10 propblk_10 proprnt_10 propown_10 hage_00 pop_90 proprur_90 hisp_90 rur_90 blk_90 wht_90 inc_90 hage_90 unit_90 occ_90 own_90 rntd_90 mbl_90 rent_90 val_90 pov_90 yng_90 prophisp_90 propwht_90 propblk_90 proprnt_90 propown_90 proppov_90 propyng_90 area_80 logarea_80 area_90 pop_00 proprur_00 hisp_00 rur_00 blk_00 wht_00 inc_00 unit_00 occ_00 own_00 rntd_00 mbl_00 rent_00 val_00 pov_00 yng_00 logarea_90 prophisp_00 propwht_00 propblk_00 proprnt_00 propown_00 proppov_00 propyng_00 area_00 logarea_00


*removing: area_80, area_90, area_00, area_10, logarea_90, logarea_00, logarea_10, own_70, own_80, own_90, own_00, own_10, proprnt_70, proprnt_80, proprnt_90, proprnt_00, proprnt_10 because of collinearity, and because I do not need to use these variables for matching or analysis

*also removing ptrend because of collinearity; will re-generate post-MI using imputed pop_## variables


_rmcoll pop_70 hisp_70 blk_70 wht_70 inc_70 unit_70 occ_70 rntd_70 rent_70 val_70 prophisp_70 propwht_70 propblk_70 propown_70 hage_80 pop_80 proprur_80 hisp_80 rur_80 blk_80 wht_80 inc_80 unit_80 occ_80 rntd_80 mbl_80 rent_80 val_80 pov_80 yng_80 prophisp_80 propwht_80 propblk_80 propown_80 proppov_80 propyng_80 rent_10 val_10 pop_10 hisp_10 blk_10 wht_10 unit_10 occ_10 rntd_10 prophisp_10 propwht_10 propblk_10 propown_10 hage_00 pop_90 proprur_90 hisp_90 rur_90 blk_90 wht_90 inc_90 hage_90 unit_90 occ_90 rntd_90 mbl_90 rent_90 val_90 pov_90 yng_90 prophisp_90 propwht_90 propblk_90 propown_90 proppov_90 propyng_90 logarea_80 pop_00 proprur_00 hisp_00 rur_00 blk_00 wht_00 inc_00 unit_00 occ_00 rntd_00 mbl_00 rent_00 val_00 pov_00 yng_00 prophisp_00 propwht_00 propblk_00 propown_00 proppov_00 propyng_00

**all work

// missing patterns table //

misstable patterns 


// missing completely at random test //

mcartest  pop_70 hisp_70 blk_70 wht_70 inc_70 unit_70 occ_70 rntd_70 rent_70 val_70 prophisp_70 propwht_70 propblk_70 propown_70 hage_80 pop_80 proprur_80 hisp_80 rur_80 blk_80 wht_80 inc_80 unit_80 occ_80 rntd_80 mbl_80 rent_80 val_80 pov_80 yng_80 prophisp_80 propwht_80 propblk_80 propown_80 proppov_80 propyng_80 rent_10 val_10 pop_10 hisp_10 blk_10 wht_10 unit_10 occ_10 rntd_10 prophisp_10 propwht_10 propblk_10 propown_10 hage_00 pop_90 proprur_90 hisp_90 rur_90 blk_90 wht_90 inc_90 hage_90 unit_90 occ_90 rntd_90 mbl_90 rent_90 val_90 pov_90 yng_90 prophisp_90 propwht_90 propblk_90 propown_90 proppov_90 propyng_90 logarea_80 pop_00 proprur_00 hisp_00 rur_00 blk_00 wht_00 inc_00 unit_00 occ_00 rntd_00 mbl_00 rent_00 val_00 pov_00 yng_00 prophisp_00 propwht_00 propblk_00 propown_00 proppov_00 propyng_00

*** not MCAR; assuming MAR
 


/////////////////////////////// STEP THREE /////////////////////////////////////


mi set wide


///////////////////////////// STEP FOUR ////////////////////////////////////////


mi register imputed  pop_70 hisp_70 blk_70 wht_70 inc_70 unit_70 occ_70 rntd_70 rent_70 val_70 prophisp_70 propwht_70 propblk_70 propown_70 hage_80 pop_80 proprur_80 hisp_80 rur_80 blk_80 wht_80 inc_80 unit_80 occ_80 rntd_80 mbl_80 rent_80 val_80 pov_80 yng_80 prophisp_80 propwht_80 propblk_80 propown_80 proppov_80 propyng_80 rent_10 val_10 pop_10 hisp_10 blk_10 wht_10 unit_10 occ_10 rntd_10 prophisp_10 propwht_10 propblk_10 propown_10 hage_00 pop_90 proprur_90 hisp_90 rur_90 blk_90 wht_90 inc_90 hage_90 unit_90 occ_90 rntd_90 mbl_90 rent_90 val_90 pov_90 yng_90 prophisp_90 propwht_90 propblk_90 propown_90 proppov_90 propyng_90 logarea_80 pop_00 proprur_00 hisp_00 rur_00 blk_00 wht_00 inc_00 unit_00 occ_00 rntd_00 mbl_00 rent_00 val_00 pov_00 yng_00 prophisp_00 propwht_00 propblk_00 propown_00 proppov_00 propyng_00
  


//////////////////////////// STEP FIVE /////////////////////////////////////////
**mi register regular (none to add)

//////////////////////////// STEP SIX //////////////////////////////////////////

mi impute chained (regress)  pop_70 hisp_70 blk_70 wht_70 inc_70 unit_70 occ_70 rntd_70 rent_70 val_70 prophisp_70 propwht_70 propblk_70 propown_70 hage_80 pop_80 proprur_80 hisp_80 rur_80 blk_80 wht_80 inc_80 unit_80 occ_80 rntd_80 mbl_80 rent_80 val_80 pov_80 yng_80 prophisp_80 propwht_80 propblk_80 propown_80 proppov_80 propyng_80 rent_10 val_10 pop_10 hisp_10 blk_10 wht_10 unit_10 occ_10 rntd_10 prophisp_10 propwht_10 propblk_10 propown_10 hage_00 pop_90 proprur_90 hisp_90 rur_90 blk_90 wht_90 inc_90 hage_90 unit_90 occ_90 rntd_90 mbl_90 rent_90 val_90 pov_90 yng_90 prophisp_90 propwht_90 propblk_90 propown_90 proppov_90 propyng_90 logarea_80 pop_00 proprur_00 hisp_00 rur_00 blk_00 wht_00 inc_00 unit_00 occ_00 rntd_00 mbl_00 rent_00 val_00 pov_00 yng_00 prophisp_00 propwht_00 propblk_00 propown_00 proppov_00 propyng_00, add(20) rseed(1234) augment force chaindots



//// save wide

save final_imputed, replace

//// save long

mi convert flong
save final_imputed_flong, replace


