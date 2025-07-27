# SETUP -------------------------------------------------------------------
library(tidyverse)
library(did) #version 2.1.2
library(gtsummary)

final_trimmed <- readRDS("data/final_trimmed_long.RDS")

if (!dir.exists("results")) dir.create("results")

#This is the code run for Model 1 - full sample, home value.
#For all other models, make adjustments:
#Use rentdv as the outcome for median rent (instead of valdv)
#Use subset data samples for comparisons between frequency/magnitude levels

# M1—Value -----------------------------------------------------------------

#initial analysis
fit.lista <- setNames(vector("list",
                      length(unique(final_trimmed$.imp))),
                      unique(final_trimmed$.imp))

#I decided to track the processing time since some of the models took a while to run
#And I'm working on improving efficiency for future projects using similar methods
#Each model is run once on each imputed dataset (20 iterations) and then the results are pooled

start <- proc.time()
for (i in 1:20) {
  fit.lista[[i]] <- att_gt(yname="valdv", #home value measured after end of time period
                           tname="period", #defines the time period
                           idname="iduniq",
                           gname="firstp", #defines the treatment group
                           xformla=NULL,
                           panel=TRUE,
                           allow_unbalanced_panel = TRUE,
                           control_group = "nevertreated",
                           anticipation=0,
                           weightsname="weights",
                           data = final_trimmed[final_trimmed$.imp == i,]) 
}
dopar_loop <- proc.time()-start
summary(dopar_loop)

saveRDS(fit.lista, file="results/m1_value.RDS")

#aggregating the above-generated group-time estimates by group
#this gives the overall result for each treatment group
fit.listb <- setNames(vector("list", length(unique(final_trimmed$.imp))), unique(final_trimmed$.imp))

start <- proc.time()
for (i in 1:20) {
  fit.listb[[i]] <- aggte(fit.lista[[i]], type="group") 
}
dopar_loop <- proc.time()-start
summary(dopar_loop)

saveRDS(fit.listb, file="results/m1_value_flgrp.RDS")
#done


#aggregating by time since first treatment

fit.listc <- setNames(vector("list", length(unique(final_trimmed$.imp))), unique(final_trimmed$.imp))

start <- proc.time()
for (i in 1:20) {
  fit.listc[[i]] <- aggte(fit.lista[[i]], type="dynamic") 
}
dopar_loop <- proc.time()-start
summary(dopar_loop)

saveRDS(fit.listc, file="results/m1_value_fles.RDS")


# M1 (pooling imputed estimates) ----------------------------------------------------------
# There was no easy way to use established imputation results pooling with the did package
# So these are pooled step by step instead

#beta:
beta_neg2 <- (fit.listc[[20]]$att.egt[[1]]+
                fit.listc[[1]]$att.egt[[1]]+
                fit.listc[[2]]$att.egt[[1]]+
                fit.listc[[3]]$att.egt[[1]]+
                fit.listc[[4]]$att.egt[[1]]+
                fit.listc[[5]]$att.egt[[1]]+
                fit.listc[[6]]$att.egt[[1]]+
                fit.listc[[7]]$att.egt[[1]]+
                fit.listc[[8]]$att.egt[[1]]+
                fit.listc[[9]]$att.egt[[1]]+
                fit.listc[[10]]$att.egt[[1]]+
                fit.listc[[11]]$att.egt[[1]]+
                fit.listc[[12]]$att.egt[[1]]+
                fit.listc[[13]]$att.egt[[1]]+
                fit.listc[[14]]$att.egt[[1]]+
                fit.listc[[15]]$att.egt[[1]]+
                fit.listc[[16]]$att.egt[[1]]+
                fit.listc[[17]]$att.egt[[1]]+
                fit.listc[[18]]$att.egt[[1]]+
                fit.listc[[19]]$att.egt[[1]])/20


beta_neg1 <- (fit.listc[[20]]$att.egt[[2]]+
                fit.listc[[1]]$att.egt[[2]]+
                fit.listc[[2]]$att.egt[[2]]+
                fit.listc[[3]]$att.egt[[2]]+
                fit.listc[[4]]$att.egt[[2]]+
                fit.listc[[5]]$att.egt[[2]]+
                fit.listc[[6]]$att.egt[[2]]+
                fit.listc[[7]]$att.egt[[2]]+
                fit.listc[[8]]$att.egt[[2]]+
                fit.listc[[9]]$att.egt[[2]]+
                fit.listc[[10]]$att.egt[[2]]+
                fit.listc[[11]]$att.egt[[2]]+
                fit.listc[[12]]$att.egt[[2]]+
                fit.listc[[13]]$att.egt[[2]]+
                fit.listc[[14]]$att.egt[[2]]+
                fit.listc[[15]]$att.egt[[2]]+
                fit.listc[[16]]$att.egt[[2]]+
                fit.listc[[17]]$att.egt[[2]]+
                fit.listc[[18]]$att.egt[[2]]+
                fit.listc[[19]]$att.egt[[2]])/20


beta_0 <- (fit.listc[[20]]$att.egt[[3]]+
             fit.listc[[1]]$att.egt[[3]]+
             fit.listc[[2]]$att.egt[[3]]+
             fit.listc[[3]]$att.egt[[3]]+
             fit.listc[[4]]$att.egt[[3]]+
             fit.listc[[5]]$att.egt[[3]]+
             fit.listc[[6]]$att.egt[[3]]+
             fit.listc[[7]]$att.egt[[3]]+
             fit.listc[[8]]$att.egt[[3]]+
             fit.listc[[9]]$att.egt[[3]]+
             fit.listc[[10]]$att.egt[[3]]+
             fit.listc[[11]]$att.egt[[3]]+
             fit.listc[[12]]$att.egt[[3]]+
             fit.listc[[13]]$att.egt[[3]]+
             fit.listc[[14]]$att.egt[[3]]+
             fit.listc[[15]]$att.egt[[3]]+
             fit.listc[[16]]$att.egt[[3]]+
             fit.listc[[17]]$att.egt[[3]]+
             fit.listc[[19]]$att.egt[[3]]+
             fit.listc[[18]]$att.egt[[3]])/20


beta_1 <- (fit.listc[[20]]$att.egt[[4]]+
             fit.listc[[1]]$att.egt[[4]]+
             fit.listc[[2]]$att.egt[[4]]+
             fit.listc[[3]]$att.egt[[4]]+
             fit.listc[[4]]$att.egt[[4]]+
             fit.listc[[5]]$att.egt[[4]]+
             fit.listc[[6]]$att.egt[[4]]+
             fit.listc[[7]]$att.egt[[4]]+
             fit.listc[[8]]$att.egt[[4]]+
             fit.listc[[9]]$att.egt[[4]]+
             fit.listc[[10]]$att.egt[[4]]+
             fit.listc[[11]]$att.egt[[4]]+
             fit.listc[[12]]$att.egt[[4]]+
             fit.listc[[13]]$att.egt[[4]]+
             fit.listc[[14]]$att.egt[[4]]+
             fit.listc[[15]]$att.egt[[4]]+
             fit.listc[[16]]$att.egt[[4]]+
             fit.listc[[17]]$att.egt[[4]]+
             fit.listc[[19]]$att.egt[[4]]+
             fit.listc[[18]]$att.egt[[4]])/20


beta_2 <- (fit.listc[[20]]$att.egt[[5]]+
             fit.listc[[1]]$att.egt[[5]]+
             fit.listc[[2]]$att.egt[[5]]+
             fit.listc[[3]]$att.egt[[5]]+
             fit.listc[[4]]$att.egt[[5]]+
             fit.listc[[5]]$att.egt[[5]]+
             fit.listc[[6]]$att.egt[[5]]+
             fit.listc[[7]]$att.egt[[5]]+
             fit.listc[[8]]$att.egt[[5]]+
             fit.listc[[9]]$att.egt[[5]]+
             fit.listc[[10]]$att.egt[[5]]+
             fit.listc[[11]]$att.egt[[5]]+
             fit.listc[[12]]$att.egt[[5]]+
             fit.listc[[13]]$att.egt[[5]]+
             fit.listc[[14]]$att.egt[[5]]+
             fit.listc[[15]]$att.egt[[5]]+
             fit.listc[[16]]$att.egt[[5]]+
             fit.listc[[17]]$att.egt[[5]]+
             fit.listc[[18]]$att.egt[[5]]+
             fit.listc[[19]]$att.egt[[5]])/20


#standard error


var_w_neg2 <- (fit.listc[[20]]$se.egt[[1]]^2+
                 fit.listc[[1]]$se.egt[[1]]^2+
                 fit.listc[[2]]$se.egt[[1]]^2+
                 fit.listc[[3]]$se.egt[[1]]^2+
                 fit.listc[[4]]$se.egt[[1]]^2+
                 fit.listc[[5]]$se.egt[[1]]^2+
                 fit.listc[[6]]$se.egt[[1]]^2+
                 fit.listc[[7]]$se.egt[[1]]^2+
                 fit.listc[[8]]$se.egt[[1]]^2+
                 fit.listc[[9]]$se.egt[[1]]^2+
                 fit.listc[[10]]$se.egt[[1]]^2+
                 fit.listc[[11]]$se.egt[[1]]^2+
                 fit.listc[[12]]$se.egt[[1]]^2+
                 fit.listc[[13]]$se.egt[[1]]^2+
                 fit.listc[[14]]$se.egt[[1]]^2+
                 fit.listc[[15]]$se.egt[[1]]^2+
                 fit.listc[[16]]$se.egt[[1]]^2+
                 fit.listc[[17]]$se.egt[[1]]^2+
                 fit.listc[[18]]$se.egt[[1]]^2+
                 fit.listc[[19]]$se.egt[[1]]^2)/20


var_w_neg1 <- (fit.listc[[20]]$se.egt[[2]]^2+
                 fit.listc[[1]]$se.egt[[2]]^2+
                 fit.listc[[2]]$se.egt[[2]]^2+
                 fit.listc[[3]]$se.egt[[2]]^2+
                 fit.listc[[4]]$se.egt[[2]]^2+
                 fit.listc[[5]]$se.egt[[2]]^2+
                 fit.listc[[6]]$se.egt[[2]]^2+
                 fit.listc[[7]]$se.egt[[2]]^2+
                 fit.listc[[8]]$se.egt[[2]]^2+
                 fit.listc[[9]]$se.egt[[2]]^2+
                 fit.listc[[10]]$se.egt[[2]]^2+
                 fit.listc[[11]]$se.egt[[2]]^2+
                 fit.listc[[12]]$se.egt[[2]]^2+
                 fit.listc[[13]]$se.egt[[2]]^2+
                 fit.listc[[14]]$se.egt[[2]]^2+
                 fit.listc[[15]]$se.egt[[2]]^2+
                 fit.listc[[16]]$se.egt[[2]]^2+
                 fit.listc[[17]]$se.egt[[2]]^2+
                 fit.listc[[18]]$se.egt[[2]]^2+
                 fit.listc[[19]]$se.egt[[2]]^2)/20


var_w_0 <- (fit.listc[[20]]$se.egt[[3]]^2+
              fit.listc[[1]]$se.egt[[3]]^2+
              fit.listc[[2]]$se.egt[[3]]^2+
              fit.listc[[3]]$se.egt[[3]]^2+
              fit.listc[[4]]$se.egt[[3]]^2+
              fit.listc[[5]]$se.egt[[3]]^2+
              fit.listc[[6]]$se.egt[[3]]^2+
              fit.listc[[7]]$se.egt[[3]]^2+
              fit.listc[[8]]$se.egt[[3]]^2+
              fit.listc[[9]]$se.egt[[3]]^2+
              fit.listc[[10]]$se.egt[[3]]^2+
              fit.listc[[11]]$se.egt[[3]]^2+
              fit.listc[[12]]$se.egt[[3]]^2+
              fit.listc[[13]]$se.egt[[3]]^2+
              fit.listc[[14]]$se.egt[[3]]^2+
              fit.listc[[15]]$se.egt[[3]]^2+
              fit.listc[[16]]$se.egt[[3]]^2+
              fit.listc[[17]]$se.egt[[3]]^2+
              fit.listc[[18]]$se.egt[[3]]^2+
              fit.listc[[19]]$se.egt[[3]]^2)/20


var_w_1 <- ( fit.listc[[20]]$se.egt[[4]]^2+
               fit.listc[[1]]$se.egt[[4]]^2+
               fit.listc[[2]]$se.egt[[4]]^2+
               fit.listc[[3]]$se.egt[[4]]^2+
               fit.listc[[4]]$se.egt[[4]]^2+
               fit.listc[[5]]$se.egt[[4]]^2+
               fit.listc[[6]]$se.egt[[4]]^2+
               fit.listc[[7]]$se.egt[[4]]^2+
               fit.listc[[8]]$se.egt[[4]]^2+
               fit.listc[[9]]$se.egt[[4]]^2+
               fit.listc[[10]]$se.egt[[4]]^2+
               fit.listc[[11]]$se.egt[[4]]^2+
               fit.listc[[12]]$se.egt[[4]]^2+
               fit.listc[[13]]$se.egt[[4]]^2+
               fit.listc[[14]]$se.egt[[4]]^2+
               fit.listc[[15]]$se.egt[[4]]^2+
               fit.listc[[16]]$se.egt[[4]]^2+
               fit.listc[[17]]$se.egt[[4]]^2+
               fit.listc[[18]]$se.egt[[4]]^2+
               fit.listc[[19]]$se.egt[[4]]^2)/20


var_w_2 <- ( fit.listc[[20]]$se.egt[[5]]^2+
               fit.listc[[1]]$se.egt[[5]]^2+
               fit.listc[[2]]$se.egt[[5]]^2+
               fit.listc[[3]]$se.egt[[5]]^2+
               fit.listc[[4]]$se.egt[[5]]^2+
               fit.listc[[5]]$se.egt[[5]]^2+
               fit.listc[[6]]$se.egt[[5]]^2+
               fit.listc[[7]]$se.egt[[5]]^2+
               fit.listc[[8]]$se.egt[[5]]^2+
               fit.listc[[9]]$se.egt[[5]]^2+
               fit.listc[[10]]$se.egt[[5]]^2+
               fit.listc[[11]]$se.egt[[5]]^2+
               fit.listc[[12]]$se.egt[[5]]^2+
               fit.listc[[13]]$se.egt[[5]]^2+
               fit.listc[[14]]$se.egt[[5]]^2+
               fit.listc[[15]]$se.egt[[5]]^2+
               fit.listc[[16]]$se.egt[[5]]^2+
               fit.listc[[17]]$se.egt[[5]]^2+
               fit.listc[[18]]$se.egt[[5]]^2+
               fit.listc[[19]]$se.egt[[5]]^2)/20


var_b_neg2 <- ((fit.listc[[20]]$att.egt[[1]]-beta_neg2)^2+
                 (fit.listc[[1]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[2]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[3]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[4]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[5]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[6]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[7]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[8]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[9]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[10]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[11]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[12]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[13]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[14]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[15]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[16]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[17]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[18]]$att.egt[[1]]-beta_neg2)^2+ 
                 (fit.listc[[19]]$att.egt[[1]]-beta_neg2)^2)/19


var_b_neg1 <- ((fit.listc[[20]]$att.egt[[2]]-beta_neg1)^2+
                 (fit.listc[[1]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[2]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[3]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[4]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[5]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[6]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[7]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[8]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[9]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[10]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[11]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[12]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[13]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[14]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[15]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[16]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[17]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[18]]$att.egt[[2]]-beta_neg1)^2+ 
                 (fit.listc[[19]]$att.egt[[2]]-beta_neg1)^2)/19


var_b_0 <- ((fit.listc[[20]]$att.egt[[3]]-beta_0)^2+
              (fit.listc[[1]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[2]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[3]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[4]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[5]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[6]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[7]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[8]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[9]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[10]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[11]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[12]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[13]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[14]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[15]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[16]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[17]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[18]]$att.egt[[3]]-beta_0)^2+ 
              (fit.listc[[19]]$att.egt[[3]]-beta_0)^2)/19


var_b_1 <- ((fit.listc[[20]]$att.egt[[4]]-beta_1)^2+
              (fit.listc[[1]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[2]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[3]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[4]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[5]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[6]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[7]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[8]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[9]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[10]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[11]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[12]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[13]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[14]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[15]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[16]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[17]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[18]]$att.egt[[4]]-beta_1)^2+ 
              (fit.listc[[19]]$att.egt[[4]]-beta_1)^2)/19


var_b_2 <- ((fit.listc[[20]]$att.egt[[5]]-beta_2)^2+
              (fit.listc[[1]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[2]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[3]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[4]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[5]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[6]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[7]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[8]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[9]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[10]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[11]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[12]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[13]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[14]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[15]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[16]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[17]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[18]]$att.egt[[5]]-beta_2)^2+ 
              (fit.listc[[19]]$att.egt[[5]]-beta_2)^2)/19


v_tot_neg2 <- var_w_neg2+var_b_neg2+(var_b_neg2/20)
se_pool_neg2 <- sqrt(v_tot_neg2)


v_tot_neg1 <- var_w_neg1+var_b_neg1+(var_b_neg1/20)
se_pool_neg1 <- sqrt(v_tot_neg1)


v_tot_0 <- var_w_0+var_b_0+(var_b_0/20)
se_pool_0 <- sqrt(v_tot_0)


v_tot_1 <- var_w_1+var_b_1+(var_b_1/20)
se_pool_1 <- sqrt(v_tot_1)


v_tot_2 <- var_w_2+var_b_2+(var_b_2/20)
se_pool_2 <- sqrt(v_tot_2)


#Significance and CI


r_neg2 <- ((1+(1/20))*var_b_neg2)/var_w_neg2 #relative increase in var due to missing
r_neg1 <- ((1+(1/20))*var_b_neg1)/var_w_neg1
r_0 <- ((1+(1/20))*var_b_0)/var_w_0
r_1 <- ((1+(1/20))*var_b_1)/var_w_1
r_2 <- ((1+(1/20))*var_b_2)/var_w_2


lambda_neg2 <- ((1+(1/20))*var_b_neg2)/v_tot_neg2 #relative increase in var due to missing
lambda_neg1 <- ((1+(1/20))*var_b_neg1)/v_tot_neg1
lambda_0 <- ((1+(1/20))*var_b_0)/v_tot_0
lambda_1 <- ((1+(1/20))*var_b_1)/v_tot_1
lambda_2 <- ((1+(1/20))*var_b_2)/v_tot_2


df_old_neg2 <- 19/lambda_neg2^2
df_old_neg1 <- 19/lambda_neg1^2
df_old_0 <- 19/lambda_0^2
df_old_1 <- 19/lambda_1^2
df_old_2 <- 19/lambda_2^2


k <- 1
n_all <- 6026+4594
n_untreated <- 4594
n_treated <- 6026
n_grp2 <- 2583
n_grp3 <- 2039
n_grp4 <- 1404
n_neg2 <- n_grp4+n_grp3
n_neg1 <- n_treated
n_0 <- n_treated
n_1 <- n_grp2+n_grp3
n_2 <- n_grp2


df_com_neg2 <- n_neg2-k
df_com_neg1 <- n_neg1-k
df_com_0 <- n_0-k
df_com_1 <- n_1-k
df_com_2 <- n_2-k


df_obs_neg2 <- ((df_com_neg2+1)/(df_com_neg2+3))*df_com_neg2*(1-lambda_neg2)
df_adj_neg2 <- (df_old_neg2*df_obs_neg2)/(df_old_neg2+df_obs_neg2)


df_obs_neg1 <- ((df_com_neg1+1)/(df_com_neg1+3))*df_com_neg1*(1-lambda_neg1)
df_adj_neg1 <- (df_old_neg1*df_obs_neg1)/(df_old_neg1+df_obs_neg1)


df_obs_0 <- ((df_com_0+1)/(df_com_0+3))*df_com_0*(1-lambda_0)
df_adj_0 <- (df_old_0*df_obs_0)/(df_old_0+df_obs_0)


df_obs_1 <- ((df_com_1+1)/(df_com_1+3))*df_com_1*(1-lambda_1)
df_adj_1 <- (df_old_1*df_obs_1)/(df_old_1+df_obs_1)


df_obs_2 <- ((df_com_2+1)/(df_com_2+3))*df_com_2*(1-lambda_2)
df_adj_2 <- (df_old_2*df_obs_2)/(df_old_2+df_obs_2)


t_neg2 <- qt(p=0.05, df=df_adj_neg2, lower.tail = T)
t_neg1 <- qt(p=0.05, df=df_adj_neg1, lower.tail = T)
t_0 <- qt(p=0.05, df=df_adj_0, lower.tail = T)
t_1 <- qt(p=0.05, df=df_adj_1, lower.tail = T)
t_2 <- qt(p=0.05, df=df_adj_2, lower.tail = T)


ciu_neg2 <- beta_neg2+1.96*se_pool_neg2
cil_neg2 <- beta_neg2-1.96*se_pool_neg2


ciu_neg1 <- beta_neg1+1.96*se_pool_neg1
cil_neg1 <- beta_neg1-1.96*se_pool_neg1


ciu_0 <- beta_0+1.96*se_pool_0
cil_0 <- beta_0-1.96*se_pool_0


ciu_1 <- beta_1+1.96*se_pool_1
cil_1 <- beta_1-1.96*se_pool_1


ciu_2 <- beta_2+1.96*se_pool_2
cil_2 <- beta_2-1.96*se_pool_2


Fc_neg2 <- (0-beta_neg2)^2/v_tot_neg2
Fc_neg1 <- (0-beta_neg1)^2/v_tot_neg1
Fc_0 <- (0-beta_0)^2/v_tot_0
Fc_1 <- (0-beta_1)^2/v_tot_1
Fc_2 <- (0-beta_2)^2/v_tot_2


p_neg2 <- 1-pf(Fc_neg2, 1, df_adj_neg2)
p_neg1 <- 1-pf(Fc_neg1, 1, df_adj_neg1)
p_0 <- 1-pf(Fc_0, 1, df_adj_0)
p_1 <- 1-pf(Fc_1, 1, df_adj_1)
p_2 <- 1-pf(Fc_2, 1, df_adj_2)


#Putting into an object to saveRDS
grp_names <- as.data.frame(c("-2", "-1", "0", "1", "2"))
colnames(grp_names) <- "Time Period"
m1_grp_coef <- c(beta_neg2, beta_neg1, beta_0, beta_1, beta_2)
m1_grp_wvar <- c(var_w_neg2, var_w_neg1, var_w_0, var_w_1, var_w_2)
m1_grp_bvar <- c(var_b_neg2, var_b_neg1, var_b_0, var_b_1, var_b_2)
m1_grp_tvar <- c(v_tot_neg2, v_tot_neg1, v_tot_0, v_tot_1, v_tot_2)
m1_grp_se <- c(se_pool_neg2, se_pool_neg1, se_pool_0, se_pool_1, se_pool_2)
m1_grp_df <- c(df_adj_neg2, df_adj_neg1, df_adj_0, df_adj_1, df_adj_2)
m1_grp_F <- c(Fc_neg2, Fc_neg1, Fc_0, Fc_1, Fc_2)
m1_grp_sig <- c(p_neg2, p_neg1, p_0, p_1, p_2)
m1_grp_ciu <- c(ciu_neg2, ciu_neg1, ciu_0, ciu_1, ciu_2)
m1_grp_cil <- c(cil_neg2, cil_neg1, cil_0, cil_1, cil_2)
M1_exp_results <- as.data.frame(cbind(grp_names, m1_grp_coef, m1_grp_wvar, 
                                      m1_grp_bvar, m1_grp_tvar, m1_grp_se, 
                                      m1_grp_df, m1_grp_F, m1_grp_sig, m1_grp_ciu, 
                                      m1_grp_cil))
glimpse(M1_exp_results)


M1_exp_results <- M1_exp_results %>% 
  rename(Estimate=m1_grp_coef, 
         WVar=m1_grp_wvar, BVar=m1_grp_bvar, 
         Var=m1_grp_tvar, StdError=m1_grp_se, 
         df=m1_grp_df, Fstat=m1_grp_F, 
         p=m1_grp_sig, Lower=m1_grp_cil, Upper=m1_grp_ciu)


glimpse(M1_exp_results)
view(M1_exp_results)
saveRDS(M1_exp_results, file="results/m1expRESULTS.RDS")


# M1 (pool group) [DONE] -------------------------------------------------------
#beta:
beta_overall <- (fit.listb[[1]]$overall.att[[1]]+
                   fit.listb[[2]]$overall.att[[1]]+
                   fit.listb[[3]]$overall.att[[1]]+
                   fit.listb[[4]]$overall.att[[1]]+
                   fit.listb[[5]]$overall.att[[1]]+
                   fit.listb[[6]]$overall.att[[1]]+
                   fit.listb[[7]]$overall.att[[1]]+
                   fit.listb[[8]]$overall.att[[1]]+
                   fit.listb[[9]]$overall.att[[1]]+
                   fit.listb[[10]]$overall.att[[1]]+
                   fit.listb[[11]]$overall.att[[1]]+
                   fit.listb[[12]]$overall.att[[1]]+
                   fit.listb[[13]]$overall.att[[1]]+
                   fit.listb[[14]]$overall.att[[1]]+
                   fit.listb[[15]]$overall.att[[1]]+
                   fit.listb[[16]]$overall.att[[1]]+
                   fit.listb[[17]]$overall.att[[1]]+
                   fit.listb[[18]]$overall.att[[1]]+
                   fit.listb[[19]]$overall.att[[1]]+
                   fit.listb[[20]]$overall.att[[1]])/20


beta_grp2 <- (fit.listb[[1]]$att.egt[[1]]+
                fit.listb[[2]]$att.egt[[1]]+
                fit.listb[[3]]$att.egt[[1]]+
                fit.listb[[4]]$att.egt[[1]]+
                fit.listb[[5]]$att.egt[[1]]+
                fit.listb[[6]]$att.egt[[1]]+
                fit.listb[[7]]$att.egt[[1]]+
                fit.listb[[8]]$att.egt[[1]]+
                fit.listb[[9]]$att.egt[[1]]+
                fit.listb[[10]]$att.egt[[1]]+
                fit.listb[[11]]$att.egt[[1]]+
                fit.listb[[12]]$att.egt[[1]]+
                fit.listb[[13]]$att.egt[[1]]+
                fit.listb[[14]]$att.egt[[1]]+
                fit.listb[[15]]$att.egt[[1]]+
                fit.listb[[16]]$att.egt[[1]]+
                fit.listb[[17]]$att.egt[[1]]+
                fit.listb[[18]]$att.egt[[1]]+
                fit.listb[[19]]$att.egt[[1]]+
                fit.listb[[20]]$att.egt[[1]])/20


beta_grp3 <- (fit.listb[[1]]$att.egt[[2]]+
                fit.listb[[2]]$att.egt[[2]]+
                fit.listb[[3]]$att.egt[[2]]+
                fit.listb[[4]]$att.egt[[2]]+
                fit.listb[[5]]$att.egt[[2]]+
                fit.listb[[6]]$att.egt[[2]]+
                fit.listb[[7]]$att.egt[[2]]+
                fit.listb[[8]]$att.egt[[2]]+
                fit.listb[[9]]$att.egt[[2]]+
                fit.listb[[10]]$att.egt[[2]]+
                fit.listb[[11]]$att.egt[[2]]+
                fit.listb[[12]]$att.egt[[2]]+
                fit.listb[[13]]$att.egt[[2]]+
                fit.listb[[14]]$att.egt[[2]]+
                fit.listb[[15]]$att.egt[[2]]+
                fit.listb[[16]]$att.egt[[2]]+
                fit.listb[[17]]$att.egt[[2]]+
                fit.listb[[18]]$att.egt[[2]]+
                fit.listb[[19]]$att.egt[[2]]+
                fit.listb[[20]]$att.egt[[2]])/20


beta_grp4 <- (fit.listb[[1]]$att.egt[[3]]+
                fit.listb[[2]]$att.egt[[3]]+
                fit.listb[[3]]$att.egt[[3]]+
                fit.listb[[4]]$att.egt[[3]]+
                fit.listb[[5]]$att.egt[[3]]+
                fit.listb[[6]]$att.egt[[3]]+
                fit.listb[[7]]$att.egt[[3]]+
                fit.listb[[8]]$att.egt[[3]]+
                fit.listb[[9]]$att.egt[[3]]+
                fit.listb[[10]]$att.egt[[3]]+
                fit.listb[[11]]$att.egt[[3]]+
                fit.listb[[12]]$att.egt[[3]]+
                fit.listb[[13]]$att.egt[[3]]+
                fit.listb[[14]]$att.egt[[3]]+
                fit.listb[[15]]$att.egt[[3]]+
                fit.listb[[16]]$att.egt[[3]]+
                fit.listb[[17]]$att.egt[[3]]+
                fit.listb[[18]]$att.egt[[3]]+
                fit.listb[[19]]$att.egt[[3]]+
                fit.listb[[20]]$att.egt[[3]])/20




#standard error


var_w_all <- (fit.listb[[1]]$overall.se[[1]]^2+
                fit.listb[[2]]$overall.se[[1]]^2+
                fit.listb[[3]]$overall.se[[1]]^2+
                fit.listb[[4]]$overall.se[[1]]^2+
                fit.listb[[5]]$overall.se[[1]]^2+
                fit.listb[[6]]$overall.se[[1]]^2+
                fit.listb[[7]]$overall.se[[1]]^2+
                fit.listb[[8]]$overall.se[[1]]^2+
                fit.listb[[9]]$overall.se[[1]]^2+
                fit.listb[[10]]$overall.se[[1]]^2+
                fit.listb[[11]]$overall.se[[1]]^2+
                fit.listb[[12]]$overall.se[[1]]^2+
                fit.listb[[13]]$overall.se[[1]]^2+
                fit.listb[[14]]$overall.se[[1]]^2+
                fit.listb[[15]]$overall.se[[1]]^2+
                fit.listb[[16]]$overall.se[[1]]^2+
                fit.listb[[17]]$overall.se[[1]]^2+
                fit.listb[[18]]$overall.se[[1]]^2+
                fit.listb[[19]]$overall.se[[1]]^2+
                fit.listb[[20]]$overall.se[[1]]^2)/20


var_w_grp2 <- (fit.listb[[1]]$se.egt[[1]]^2+
                 fit.listb[[2]]$se.egt[[1]]^2+
                 fit.listb[[3]]$se.egt[[1]]^2+
                 fit.listb[[4]]$se.egt[[1]]^2+
                 fit.listb[[5]]$se.egt[[1]]^2+
                 fit.listb[[6]]$se.egt[[1]]^2+
                 fit.listb[[7]]$se.egt[[1]]^2+
                 fit.listb[[8]]$se.egt[[1]]^2+
                 fit.listb[[9]]$se.egt[[1]]^2+
                 fit.listb[[10]]$se.egt[[1]]^2+
                 fit.listb[[11]]$se.egt[[1]]^2+
                 fit.listb[[12]]$se.egt[[1]]^2+
                 fit.listb[[13]]$se.egt[[1]]^2+
                 fit.listb[[14]]$se.egt[[1]]^2+
                 fit.listb[[15]]$se.egt[[1]]^2+
                 fit.listb[[16]]$se.egt[[1]]^2+
                 fit.listb[[17]]$se.egt[[1]]^2+
                 fit.listb[[18]]$se.egt[[1]]^2+
                 fit.listb[[19]]$se.egt[[1]]^2+
                 fit.listb[[20]]$se.egt[[1]]^2)/20


var_w_grp3 <- (fit.listb[[1]]$se.egt[[2]]^2+
                 fit.listb[[2]]$se.egt[[2]]^2+
                 fit.listb[[3]]$se.egt[[2]]^2+
                 fit.listb[[4]]$se.egt[[2]]^2+
                 fit.listb[[5]]$se.egt[[2]]^2+
                 fit.listb[[6]]$se.egt[[2]]^2+
                 fit.listb[[7]]$se.egt[[2]]^2+
                 fit.listb[[8]]$se.egt[[2]]^2+
                 fit.listb[[9]]$se.egt[[2]]^2+
                 fit.listb[[10]]$se.egt[[2]]^2+
                 fit.listb[[11]]$se.egt[[2]]^2+
                 fit.listb[[12]]$se.egt[[2]]^2+
                 fit.listb[[13]]$se.egt[[2]]^2+
                 fit.listb[[14]]$se.egt[[2]]^2+
                 fit.listb[[15]]$se.egt[[2]]^2+
                 fit.listb[[16]]$se.egt[[2]]^2+
                 fit.listb[[17]]$se.egt[[2]]^2+
                 fit.listb[[18]]$se.egt[[2]]^2+
                 fit.listb[[19]]$se.egt[[2]]^2+
                 fit.listb[[20]]$se.egt[[2]]^2)/20


var_w_grp4 <- (fit.listb[[1]]$se.egt[[3]]^2+
                 fit.listb[[2]]$se.egt[[3]]^2+
                 fit.listb[[3]]$se.egt[[3]]^2+
                 fit.listb[[4]]$se.egt[[3]]^2+
                 fit.listb[[5]]$se.egt[[3]]^2+
                 fit.listb[[6]]$se.egt[[3]]^2+
                 fit.listb[[7]]$se.egt[[3]]^2+
                 fit.listb[[8]]$se.egt[[3]]^2+
                 fit.listb[[9]]$se.egt[[3]]^2+
                 fit.listb[[10]]$se.egt[[3]]^2+
                 fit.listb[[11]]$se.egt[[3]]^2+
                 fit.listb[[12]]$se.egt[[3]]^2+
                 fit.listb[[13]]$se.egt[[3]]^2+
                 fit.listb[[14]]$se.egt[[3]]^2+
                 fit.listb[[15]]$se.egt[[3]]^2+
                 fit.listb[[16]]$se.egt[[3]]^2+
                 fit.listb[[17]]$se.egt[[3]]^2+
                 fit.listb[[18]]$se.egt[[3]]^2+
                 fit.listb[[19]]$se.egt[[3]]^2+
                 fit.listb[[20]]$se.egt[[3]]^2)/20


var_b_all <- ((fit.listb[[1]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[2]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[3]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[4]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[5]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[6]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[7]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[8]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[9]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[10]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[11]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[12]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[13]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[14]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[15]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[16]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[17]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[18]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[19]]$overall.att[[1]]-beta_overall)^2+ 
                (fit.listb[[20]]$overall.att[[1]]-beta_overall)^2)/19


var_b_grp2 <- ((fit.listb[[1]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[2]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[3]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[4]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[5]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[6]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[7]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[8]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[9]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[10]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[11]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[12]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[13]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[14]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[15]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[16]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[17]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[18]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[19]]$att.egt[[1]]-beta_grp2)^2+ 
                 (fit.listb[[20]]$att.egt[[1]]-beta_grp2)^2)/19


var_b_grp3 <- ((fit.listb[[1]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[2]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[3]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[4]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[5]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[6]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[7]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[8]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[9]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[10]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[11]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[12]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[13]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[14]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[15]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[16]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[17]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[18]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[19]]$att.egt[[2]]-beta_grp3)^2+ 
                 (fit.listb[[20]]$att.egt[[2]]-beta_grp3)^2)/19


var_b_grp4 <- ((fit.listb[[1]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[2]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[3]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[4]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[5]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[6]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[7]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[8]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[9]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[10]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[11]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[12]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[13]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[14]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[15]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[16]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[17]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[18]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[19]]$att.egt[[3]]-beta_grp4)^2+ 
                 (fit.listb[[20]]$att.egt[[3]]-beta_grp4)^2)/19


v_tot_all <- var_w_all+var_b_all+(var_b_all/20)
se_pool_all <- sqrt(v_tot_all)


v_tot_grp2 <- var_w_grp2+var_b_grp2+(var_b_grp2/20)
se_pool_grp2 <- sqrt(v_tot_grp2)


v_tot_grp3 <- var_w_grp3+var_b_grp3+(var_b_grp3/20)
se_pool_grp3 <- sqrt(v_tot_grp3)


v_tot_grp4 <- var_w_grp4+var_b_grp4+(var_b_grp4/20)
se_pool_grp4 <- sqrt(v_tot_grp4)


#Significance and CI


r_all <- ((1+(1/20))*var_b_all)/var_w_all #relative increase in var due to missing
r_grp2 <- ((1+(1/20))*var_b_grp2)/var_w_grp2
r_grp3 <- ((1+(1/20))*var_b_grp3)/var_w_grp3
r_grp4 <- ((1+(1/20))*var_b_grp4)/var_w_grp4


lambda_all <- ((1+(1/20))*var_b_all)/v_tot_all #relative increase in var due to missing
lambda_grp2 <- ((1+(1/20))*var_b_grp2)/v_tot_grp2
lambda_grp3 <- ((1+(1/20))*var_b_grp3)/v_tot_grp3
lambda_grp4 <- ((1+(1/20))*var_b_grp4)/v_tot_grp4


df_old_all <- 19/lambda_all^2
df_old_grp2 <- 19/lambda_grp2^2
df_old_grp3 <- 19/lambda_grp3^2
df_old_grp4 <- 19/lambda_grp4^2


k <- 1
n_all <- 6026+4594
n_untreated <- 4594
n_treated <- 6026
n_grp2 <- 2583
n_grp3 <- 2039
n_grp4 <- 1404
n_neg2 <- n_grp4+n_grp3
n_neg1 <- n_treated
n_0 <- n_treated
n_1 <- n_grp2+n_grp3
n_2 <- n_grp2


df_com_all <- n_all-k
df_com_grp2 <- n_grp2-k
df_com_grp3 <- n_grp3-k
df_com_grp4 <- n_grp4-k


df_obs_all <- ((df_com_all+1)/(df_com_all+3))*df_com_all*(1-lambda_all)
df_adj_all <- (df_old_all*df_obs_all)/(df_old_all+df_obs_all)


df_obs_grp2 <- ((df_com_grp2+1)/(df_com_grp2+3))*df_com_grp2*(1-lambda_grp2)
df_adj_grp2 <- (df_old_grp2*df_obs_grp2)/(df_old_grp2+df_obs_grp2)


df_obs_grp3 <- ((df_com_grp3+1)/(df_com_grp3+3))*df_com_grp3*(1-lambda_grp3)
df_adj_grp3 <- (df_old_grp3*df_obs_grp3)/(df_old_grp3+df_obs_grp3)


df_obs_grp4 <- ((df_com_grp4+1)/(df_com_grp4+3))*df_com_grp4*(1-lambda_grp4)
df_adj_grp4 <- (df_old_grp4*df_obs_grp4)/(df_old_grp4+df_obs_grp4)


t_all <- qt(p=0.05, df=df_adj_all, lower.tail = T)
t_grp2 <- qt(p=0.05, df=df_adj_grp2, lower.tail = T)
t_grp3 <- qt(p=0.05, df=df_adj_grp3, lower.tail = T)
t_grp4 <- qt(p=0.05, df=df_adj_grp4, lower.tail = T)


ciu_all <- beta_overall+1.96*se_pool_all
cil_all <- beta_overall-1.96*se_pool_all


ciu_grp2 <- beta_grp2+1.96*se_pool_grp2
cil_grp2 <- beta_grp2-1.96*se_pool_grp2


ciu_grp3 <- beta_grp3+1.96*se_pool_grp3
cil_grp3 <- beta_grp3-1.96*se_pool_grp3


ciu_grp4 <- beta_grp4+1.96*se_pool_grp4
cil_grp4 <- beta_grp4-1.96*se_pool_grp4


Fc_all <- (0-beta_overall)^2/v_tot_all
Fc_grp2 <- (0-beta_grp2)^2/v_tot_grp2
Fc_grp3 <- (0-beta_grp3)^2/v_tot_grp3
Fc_grp4 <- (0-beta_grp4)^2/v_tot_grp4


p_all <- 1-pf(Fc_all, 1, df_adj_all)
p_grp2 <- 1-pf(Fc_grp2, 1, df_adj_grp2)
p_grp3 <- 1-pf(Fc_grp3, 1, df_adj_grp3)
p_grp4 <- 1-pf(Fc_grp4, 1, df_adj_grp4)


#Putting into an object to saveRDS
grp_names <- as.data.frame(c("overall", "group2", "group3", "group4"))
colnames(grp_names) <- "Group"
m1_grp_coef <- c(beta_overall, beta_grp2, beta_grp3, beta_grp4)
m1_grp_wvar <- c(var_w_all, var_w_grp2, var_w_grp3, var_w_grp4)
m1_grp_bvar <- c(var_b_all, var_b_grp2, var_b_grp3, var_b_grp4)
m1_grp_tvar <- c(v_tot_all, v_tot_grp2, v_tot_grp3, v_tot_grp4)
m1_grp_se <- c(se_pool_all, se_pool_grp2, se_pool_grp3, se_pool_grp4)
m1_grp_df <- c(df_adj_all, df_adj_grp2, df_adj_grp3, df_adj_grp4)
m1_grp_F <- c(Fc_all, Fc_grp2, Fc_grp3, Fc_grp4)
m1_grp_sig <- c(p_all, p_grp2, p_grp3, p_grp4)
m1_grp_ciu <- c(ciu_all, ciu_grp2, ciu_grp3, ciu_grp4)
m1_grp_cil <- c(cil_all, cil_grp2, cil_grp3, cil_grp4)
M1_group_results <- as.data.frame(cbind(grp_names, m1_grp_coef, m1_grp_wvar, m1_grp_bvar, m1_grp_tvar, 
                                        m1_grp_se, m1_grp_df, m1_grp_F, m1_grp_sig, m1_grp_ciu, m1_grp_cil))
M1_group_results <- rbind(M1_group_results, c("Group", "Coeff", "Var_Within", "Var_Between", "Var", "SE",
                                              "adj_df", "F-stat", "p-val", "97.5%", "2.5%"))
glimpse(M1_group_results)


M1_group_results <- M1_group_results %>% 
  rename(Estimate=m1_grp_coef, 
         WVar=m1_grp_wvar, BVar=m1_grp_bvar, 
         Var=m1_grp_tvar, StdError=m1_grp_se, 
         df=m1_grp_df, Fstat=m1_grp_F, 
         p=m1_grp_sig, Lower=m1_grp_cil, Upper=m1_grp_ciu) %>% 
  filter(Group!="Group")


glimpse(M1_group_results)
view(M1_group_results)
saveRDS(M1_group_results, file="results/m1grpRESULTS.RDS")
