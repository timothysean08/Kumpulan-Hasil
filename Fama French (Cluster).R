library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)
library(GA)
library(doParallel)
library(lmtest)
library(car)
install.packages("tseries")  # Install tseries package if not already installed
library(tseries)  # Load the tseries package
############## Variabel x ################
cluster = read_xlsx('Cluster.xlsx')
tradi = read_xlsx('Tradisional.xlsx')
market = read_xlsx('Market Risk Premium.xlsx')
############# Variabel y ########
equal = read_xlsx('Equal.xlsx')
value = read_xlsx('Value.xlsx')

H_Equal = equal$'H'
L_Equal = equal$'L'
M_Equal = equal$'M'
N_Equal = equal$'N'

H_Value = value$'H'
L_Value = value$'L'
M_Value = value$'M'
N_Value = value$'N'

MRP = market$MRP
RF = market$`Risk free rate`

SMB_Cluster = cluster$SMB
HML_Cluster = cluster$HML
RMW_Cluster = cluster$RMW
CMA_Cluster = cluster$CMA
MOM_Cluster = cluster$MOM

SMB_Tradi = tradi$SMB
HML_Tradi = tradi$HML
RMW_Tradi = tradi$RMW
CMA_Tradi = tradi$CMA
MOM_Tradi = tradi$MOM


# Cluster ########################
########## FF3################
######################Equal Weighted##############
ff3_H_Equal_Cluster <- lm((H_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster)
ff3_L_Equal_Cluster <- lm((L_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster)
ff3_M_Equal_Cluster <- lm((M_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster)
ff3_N_Equal_Cluster <- lm((N_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster)

######################Value Weighted##############
ff3_H_Value_Cluster <- lm((H_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster)
ff3_L_Value_Cluster <- lm((L_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster)
ff3_M_Value_Cluster <- lm((M_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster)
ff3_N_Value_Cluster <- lm((N_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster)

###################### GA Weighted ##############
ff3_H_GA_Cluster <- lm((H_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster)
ff3_L_GA_Cluster <- lm((L_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster)
ff3_M_GA_Cluster <- lm((M_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster)
ff3_N_GA_Cluster <- lm((N_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster)

########## FF5################
######################Equal Weighted##############
ff5_H_Equal_Cluster <- lm((H_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster)
ff5_L_Equal_Cluster <- lm((L_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster)
ff5_M_Equal_Cluster <- lm((M_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster)
ff5_N_Equal_Cluster <- lm((N_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster)

######################Value Weighted##############
ff5_H_Value_Cluster <- lm((H_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster)
ff5_L_Value_Cluster <- lm((L_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster)
ff5_M_Value_Cluster <- lm((M_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster)
ff5_N_Value_Cluster <- lm((N_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster)

###################### GA Weighted ##############
ff5_H_GA_Cluster <- lm((H_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster + CMA_Cluster)
ff5_L_GA_Cluster <- lm((L_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster + CMA_Cluster)
ff5_M_GA_Cluster <- lm((M_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster + CMA_Cluster)
ff5_N_GA_Cluster <- lm((N_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster + CMA_Cluster)

########## FF6################
######################Equal Weighted##############
ff6_H_Equal_Cluster <- lm((H_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster + MOM_Cluster)
ff6_L_Equal_Cluster <- lm((L_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster+ MOM_Cluster)
ff6_M_Equal_Cluster <- lm((M_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster+ MOM_Cluster)
ff6_N_Equal_Cluster <- lm((N_Equal-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster+ MOM_Cluster)

######################Value Weighted##############
ff6_H_Value_Cluster <- lm((H_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster+ MOM_Cluster)
ff6_L_Value_Cluster <- lm((L_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster+ MOM_Cluster)
ff6_M_Value_Cluster <- lm((M_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster+ MOM_Cluster)
ff6_N_Value_Cluster <- lm((N_Value-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster +CMA_Cluster+ MOM_Cluster)
###################### GA Weighted ##############
ff6_H_GA_Cluster <- lm((H_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster + CMA_Cluster + MOM_Cluster)
ff6_L_GA_Cluster <- lm((L_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster + CMA_Cluster + MOM_Cluster)
ff6_M_GA_Cluster <- lm((M_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster + CMA_Cluster + MOM_Cluster)
ff6_N_GA_Cluster <- lm((N_GA-RF) ~ MRP + SMB_Cluster + HML_Cluster + RMW_Cluster + CMA_Cluster + MOM_Cluster)

###############################################ff3_H_Equal_Cluster###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (H)
bgtest(ff3_H_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (H)
bptest(ff3_H_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (H)
vif(ff3_H_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (H)
residuals_ff3_H_Equal_Cluster <- residuals(ff3_H_Equal_Cluster)
ks.test(residuals_ff3_H_Equal_Cluster, "pnorm", mean = mean(residuals_ff3_H_Equal_Cluster), sd = sd(residuals_ff3_H_Equal_Cluster))

###############################################ff3_L_Equal_Cluster###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (L)
bgtest(ff3_L_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (L)
bptest(ff3_L_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (L)
vif(ff3_L_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (L)
residuals_ff3_L_Equal_Cluster <- residuals(ff3_L_Equal_Cluster)
ks.test(residuals_ff3_L_Equal_Cluster, "pnorm", mean = mean(residuals_ff3_L_Equal_Cluster), sd = sd(residuals_ff3_L_Equal_Cluster))

###############################################ff3_M_Equal_Cluster###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (M)
bgtest(ff3_M_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (M)
bptest(ff3_M_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (M)
vif(ff3_M_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (M)
residuals_ff3_M_Equal_Cluster <- residuals(ff3_M_Equal_Cluster)
ks.test(residuals_ff3_M_Equal_Cluster, "pnorm", mean = mean(residuals_ff3_M_Equal_Cluster), sd = sd(residuals_ff3_M_Equal_Cluster))

###############################################ff3_N_Equal_Cluster###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (N)
bgtest(ff3_N_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (N)
bptest(ff3_N_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (N)
vif(ff3_N_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (N)
residuals_ff3_N_Equal_Cluster <- residuals(ff3_N_Equal_Cluster)
ks.test(residuals_ff3_N_Equal_Cluster, "pnorm", mean = mean(residuals_ff3_N_Equal_Cluster), sd = sd(residuals_ff3_N_Equal_Cluster))




###############################################ff3_H_Value_Cluster###################################
# Uji autokorelasi untuk model FF3 Value Weighted (H)
bgtest(ff3_H_Value_Cluster)
# Uji heteroskedastisitas untuk model FF3 Value Weighted (H)
bptest(ff3_H_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 Value Weighted (H)
vif(ff3_H_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Value Weighted (H)
residuals_ff3_H_Value_Cluster <- residuals(ff3_H_Value_Cluster)
ks.test(residuals_ff3_H_Value_Cluster, "pnorm", mean = mean(residuals_ff3_H_Value_Cluster), sd = sd(residuals_ff3_H_Value_Cluster))

###############################################ff3_L_Value_Cluster###################################
# Uji autokorelasi untuk model FF3 Value Weighted (L)
bgtest(ff3_L_Value_Cluster)
# Uji heteroskedastisitas untuk model FF3 Value Weighted (L)
bptest(ff3_L_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 Value Weighted (L)
vif(ff3_L_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Value Weighted (L)
residuals_ff3_L_Value_Cluster <- residuals(ff3_L_Value_Cluster)
ks.test(residuals_ff3_L_Value_Cluster, "pnorm", mean = mean(residuals_ff3_L_Value_Cluster), sd = sd(residuals_ff3_L_Value_Cluster))

###############################################ff3_M_Value_Cluster###################################
# Uji autokorelasi untuk model FF3 Value Weighted (M)
bgtest(ff3_M_Value_Cluster)
# Uji heteroskedastisitas untuk model FF3 Value Weighted (M)
bptest(ff3_M_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 Value Weighted (M)
vif(ff3_M_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Value Weighted (M)
residuals_ff3_M_Value_Cluster <- residuals(ff3_M_Value_Cluster)
ks.test(residuals_ff3_M_Value_Cluster, "pnorm", mean = mean(residuals_ff3_M_Value_Cluster), sd = sd(residuals_ff3_M_Value_Cluster))

###############################################ff3_N_Value_Cluster###################################
# Uji autokorelasi untuk model FF3 Value Weighted (N)
bgtest(ff3_N_Value_Cluster)
# Uji heteroskedastisitas untuk model FF3 Value Weighted (N)
bptest(ff3_N_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 Value Weighted (N)
vif(ff3_N_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Value Weighted (N)
residuals_ff3_N_Value_Cluster <- residuals(ff3_N_Value_Cluster)
ks.test(residuals_ff3_N_Value_Cluster, "pnorm", mean = mean(residuals_ff3_N_Value_Cluster), sd = sd(residuals_ff3_N_Value_Cluster))



###############################################ff5_H_Equal_Cluster###################################
# Uji autokorelasi untuk model FF5 Equal Weighted (H)
bgtest(ff5_H_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF5 Equal Weighted (H)
bptest(ff5_H_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 Equal Weighted (H)
vif(ff5_H_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Equal Weighted (H)
residuals_ff5_H_Equal_Cluster <- residuals(ff5_H_Equal_Cluster)
ks.test(residuals_ff5_H_Equal_Cluster, "pnorm", mean = mean(residuals_ff5_H_Equal_Cluster), sd = sd(residuals_ff5_H_Equal_Cluster))

###############################################ff5_L_Equal_Cluster###################################
# Uji autokorelasi untuk model FF5 Equal Weighted (L)
bgtest(ff5_L_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF5 Equal Weighted (L)
bptest(ff5_L_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 Equal Weighted (L)
vif(ff5_L_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Equal Weighted (L)
residuals_ff5_L_Equal_Cluster <- residuals(ff5_L_Equal_Cluster)
ks.test(residuals_ff5_L_Equal_Cluster, "pnorm", mean = mean(residuals_ff5_L_Equal_Cluster), sd = sd(residuals_ff5_L_Equal_Cluster))

###############################################ff5_M_Equal_Cluster###################################
# Uji autokorelasi untuk model FF5 Equal Weighted (M)
bgtest(ff5_M_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF5 Equal Weighted (M)
bptest(ff5_M_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 Equal Weighted (M)
vif(ff5_M_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Equal Weighted (M)
residuals_ff5_M_Equal_Cluster <- residuals(ff5_M_Equal_Cluster)
ks.test(residuals_ff5_M_Equal_Cluster, "pnorm", mean = mean(residuals_ff5_M_Equal_Cluster), sd = sd(residuals_ff5_M_Equal_Cluster))

###############################################ff5_N_Equal_Cluster###################################
# Uji autokorelasi untuk model FF5 Equal Weighted (N)
bgtest(ff5_N_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF5 Equal Weighted (N)
bptest(ff5_N_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 Equal Weighted (N)
vif(ff5_N_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Equal Weighted (N)
residuals_ff5_N_Equal_Cluster <- residuals(ff5_N_Equal_Cluster)
ks.test(residuals_ff5_N_Equal_Cluster, "pnorm", mean = mean(residuals_ff5_N_Equal_Cluster), sd = sd(residuals_ff5_N_Equal_Cluster))




###############################################ff5_H_Value_Cluster###################################
# Uji autokorelasi untuk model FF5 Value Weighted (H)
bgtest(ff5_H_Value_Cluster)
# Uji heteroskedastisitas untuk model FF5 Value Weighted (H)
bptest(ff5_H_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 Value Weighted (H)
vif(ff5_H_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Value Weighted (H)
residuals_ff5_H_Value_Cluster <- residuals(ff5_H_Value_Cluster)
ks.test(residuals_ff5_H_Value_Cluster, "pnorm", mean = mean(residuals_ff5_H_Value_Cluster), sd = sd(residuals_ff5_H_Value_Cluster))

###############################################ff5_L_Value_Cluster###################################
# Uji autokorelasi untuk model FF5 Value Weighted (L)
bgtest(ff5_L_Value_Cluster)
# Uji heteroskedastisitas untuk model FF5 Value Weighted (L)
bptest(ff5_L_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 Value Weighted (L)
vif(ff5_L_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Value Weighted (L)
residuals_ff5_L_Value_Cluster <- residuals(ff5_L_Value_Cluster)
ks.test(residuals_ff5_L_Value_Cluster, "pnorm", mean = mean(residuals_ff5_L_Value_Cluster), sd = sd(residuals_ff5_L_Value_Cluster))

###############################################ff5_M_Value_Cluster###################################
# Uji autokorelasi untuk model FF5 Value Weighted (M)
bgtest(ff5_M_Value_Cluster)
# Uji heteroskedastisitas untuk model FF5 Value Weighted (M)
bptest(ff5_M_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 Value Weighted (M)
vif(ff5_M_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Value Weighted (M)
residuals_ff5_M_Value_Cluster <- residuals(ff5_M_Value_Cluster)
ks.test(residuals_ff5_M_Value_Cluster, "pnorm", mean = mean(residuals_ff5_M_Value_Cluster), sd = sd(residuals_ff5_M_Value_Cluster))

###############################################ff5_N_Value_Cluster###################################
# Uji autokorelasi untuk model FF5 Value Weighted (N)
bgtest(ff5_N_Value_Cluster)
# Uji heteroskedastisitas untuk model FF5 Value Weighted (N)
bptest(ff5_N_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 Value Weighted (N)
vif(ff5_N_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Value Weighted (N)
residuals_ff5_N_Value_Cluster <- residuals(ff5_N_Value_Cluster)
ks.test(residuals_ff5_N_Value_Cluster, "pnorm", mean = mean(residuals_ff5_N_Value_Cluster), sd = sd(residuals_ff5_N_Value_Cluster))





###############################################ff6_H_Equal_Cluster###################################
# Uji autokorelasi untuk model FF6 Equal Weighted (H)
bgtest(ff6_H_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF6 Equal Weighted (H)
bptest(ff6_H_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 Equal Weighted (H)
vif(ff6_H_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Equal Weighted (H)
residuals_ff6_H_Equal_Cluster <- residuals(ff6_H_Equal_Cluster)
ks.test(residuals_ff6_H_Equal_Cluster, "pnorm", mean = mean(residuals_ff6_H_Equal_Cluster), sd = sd(residuals_ff6_H_Equal_Cluster))

###############################################ff6_L_Equal_Cluster###################################
# Uji autokorelasi untuk model FF6 Equal Weighted (L)
bgtest(ff6_L_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF6 Equal Weighted (L)
bptest(ff6_L_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 Equal Weighted (L)
vif(ff6_L_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Equal Weighted (L)
residuals_ff6_L_Equal_Cluster <- residuals(ff6_L_Equal_Cluster)
ks.test(residuals_ff6_L_Equal_Cluster, "pnorm", mean = mean(residuals_ff6_L_Equal_Cluster), sd = sd(residuals_ff6_L_Equal_Cluster))

###############################################ff6_M_Equal_Cluster###################################
# Uji autokorelasi untuk model FF6 Equal Weighted (M)
bgtest(ff6_M_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF6 Equal Weighted (M)
bptest(ff6_M_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 Equal Weighted (M)
vif(ff6_M_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Equal Weighted (M)
residuals_ff6_M_Equal_Cluster <- residuals(ff6_M_Equal_Cluster)
ks.test(residuals_ff6_M_Equal_Cluster, "pnorm", mean = mean(residuals_ff6_M_Equal_Cluster), sd = sd(residuals_ff6_M_Equal_Cluster))

###############################################ff6_N_Equal_Cluster###################################
# Uji autokorelasi untuk model FF6 Equal Weighted (N)
bgtest(ff6_N_Equal_Cluster)
# Uji heteroskedastisitas untuk model FF6 Equal Weighted (N)
bptest(ff6_N_Equal_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 Equal Weighted (N)
vif(ff6_N_Equal_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Equal Weighted (N)
residuals_ff6_N_Equal_Cluster <- residuals(ff6_N_Equal_Cluster)
ks.test(residuals_ff6_N_Equal_Cluster, "pnorm", mean = mean(residuals_ff6_N_Equal_Cluster), sd = sd(residuals_ff6_N_Equal_Cluster))




###############################################ff6_H_Value_Cluster###################################
# Uji autokorelasi untuk model FF6 Value Weighted (H)
bgtest(ff6_H_Value_Cluster)
# Uji heteroskedastisitas untuk model FF6 Value Weighted (H)
bptest(ff6_H_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 Value Weighted (H)
vif(ff6_H_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Value Weighted (H)
residuals_ff6_H_Value_Cluster <- residuals(ff6_H_Value_Cluster)
ks.test(residuals_ff6_H_Value_Cluster, "pnorm", mean = mean(residuals_ff6_H_Value_Cluster), sd = sd(residuals_ff6_H_Value_Cluster))

###############################################ff6_L_Value_Cluster###################################
# Uji autokorelasi untuk model FF6 Value Weighted (L)
bgtest(ff6_L_Value_Cluster)
# Uji heteroskedastisitas untuk model FF6 Value Weighted (L)
bptest(ff6_L_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 Value Weighted (L)
vif(ff6_L_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Value Weighted (L)
residuals_ff6_L_Value_Cluster <- residuals(ff6_L_Value_Cluster)
ks.test(residuals_ff6_L_Value_Cluster, "pnorm", mean = mean(residuals_ff6_L_Value_Cluster), sd = sd(residuals_ff6_L_Value_Cluster))

###############################################ff6_M_Value_Cluster###################################
# Uji autokorelasi untuk model FF6 Value Weighted (M)
bgtest(ff6_M_Value_Cluster)
# Uji heteroskedastisitas untuk model FF6 Value Weighted (M)
bptest(ff6_M_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 Value Weighted (M)
vif(ff6_M_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Value Weighted (M)
residuals_ff6_M_Value_Cluster <- residuals(ff6_M_Value_Cluster)
ks.test(residuals_ff6_M_Value_Cluster, "pnorm", mean = mean(residuals_ff6_M_Value_Cluster), sd = sd(residuals_ff6_M_Value_Cluster))

###############################################ff6_N_Value_Cluster###################################
# Uji autokorelasi untuk model FF6 Value Weighted (N)
bgtest(ff6_N_Value_Cluster)
# Uji heteroskedastisitas untuk model FF6 Value Weighted (N)
bptest(ff6_N_Value_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 Value Weighted (N)
vif(ff6_N_Value_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Value Weighted (N)
residuals_ff6_N_Value_Cluster <- residuals(ff6_N_Value_Cluster)
ks.test(residuals_ff6_N_Value_Cluster, "pnorm", mean = mean(residuals_ff6_N_Value_Cluster), sd = sd(residuals_ff6_N_Value_Cluster))


############################################### ff3_H_GA_Cluster ###################################
# Uji autokorelasi untuk model FF3 GA Weighted (H)
bgtest(ff3_H_GA_Cluster)
# Uji heteroskedastisitas untuk model FF3 GA Weighted (H)
bptest(ff3_H_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 GA Weighted (H)
vif(ff3_H_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 GA Weighted (H)
residuals_ff3_H_GA_Cluster <- residuals(ff3_H_GA_Cluster)
ks.test(residuals_ff3_H_GA_Cluster, "pnorm", mean = mean(residuals_ff3_H_GA_Cluster), sd = sd(residuals_ff3_H_GA_Cluster))

############################################### ff3_L_GA_Cluster ###################################
# Uji autokorelasi untuk model FF3 GA Weighted (L)
bgtest(ff3_L_GA_Cluster)
# Uji heteroskedastisitas untuk model FF3 GA Weighted (L)
bptest(ff3_L_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 GA Weighted (L)
vif(ff3_L_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 GA Weighted (L)
residuals_ff3_L_GA_Cluster <- residuals(ff3_L_GA_Cluster)
ks.test(residuals_ff3_L_GA_Cluster, "pnorm", mean = mean(residuals_ff3_L_GA_Cluster), sd = sd(residuals_ff3_L_GA_Cluster))

############################################### ff3_M_GA_Cluster ###################################
# Uji autokorelasi untuk model FF3 GA Weighted (M)
bgtest(ff3_M_GA_Cluster)
# Uji heteroskedastisitas untuk model FF3 GA Weighted (M)
bptest(ff3_M_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 GA Weighted (M)
vif(ff3_M_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 GA Weighted (M)
residuals_ff3_M_GA_Cluster <- residuals(ff3_M_GA_Cluster)
ks.test(residuals_ff3_M_GA_Cluster, "pnorm", mean = mean(residuals_ff3_M_GA_Cluster), sd = sd(residuals_ff3_M_GA_Cluster))

############################################### ff3_N_GA_Cluster ###################################
# Uji autokorelasi untuk model FF3 GA Weighted (N)
bgtest(ff3_N_GA_Cluster)
# Uji heteroskedastisitas untuk model FF3 GA Weighted (N)
bptest(ff3_N_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF3 GA Weighted (N)
vif(ff3_N_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 GA Weighted (N)
residuals_ff3_N_GA_Cluster <- residuals(ff3_N_GA_Cluster)
ks.test(residuals_ff3_N_GA_Cluster, "pnorm", mean = mean(residuals_ff3_N_GA_Cluster), sd = sd(residuals_ff3_N_GA_Cluster))

############################################### ff5_H_GA_Cluster ###################################
# Uji autokorelasi untuk model FF5 GA Weighted (H)
bgtest(ff5_H_GA_Cluster)
# Uji heteroskedastisitas untuk model FF5 GA Weighted (H)
bptest(ff5_H_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 GA Weighted (H)
vif(ff5_H_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 GA Weighted (H)
residuals_ff5_H_GA_Cluster <- residuals(ff5_H_GA_Cluster)
ks.test(residuals_ff5_H_GA_Cluster, "pnorm", mean = mean(residuals_ff5_H_GA_Cluster), sd = sd(residuals_ff5_H_GA_Cluster))

############################################### ff5_L_GA_Cluster ###################################
# Uji autokorelasi untuk model FF5 GA Weighted (L)
bgtest(ff5_L_GA_Cluster)
# Uji heteroskedastisitas untuk model FF5 GA Weighted (L)
bptest(ff5_L_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 GA Weighted (L)
vif(ff5_L_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 GA Weighted (L)
residuals_ff5_L_GA_Cluster <- residuals(ff5_L_GA_Cluster)
ks.test(residuals_ff5_L_GA_Cluster, "pnorm", mean = mean(residuals_ff5_L_GA_Cluster), sd = sd(residuals_ff5_L_GA_Cluster))

############################################### ff5_M_GA_Cluster ###################################
# Uji autokorelasi untuk model FF5 GA Weighted (M)
bgtest(ff5_M_GA_Cluster)
# Uji heteroskedastisitas untuk model FF5 GA Weighted (M)
bptest(ff5_M_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 GA Weighted (M)
vif(ff5_M_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 GA Weighted (M)
residuals_ff5_M_GA_Cluster <- residuals(ff5_M_GA_Cluster)
ks.test(residuals_ff5_M_GA_Cluster, "pnorm", mean = mean(residuals_ff5_M_GA_Cluster), sd = sd(residuals_ff5_M_GA_Cluster))

############################################### ff5_N_GA_Cluster ###################################
# Uji autokorelasi untuk model FF5 GA Weighted (N)
bgtest(ff5_N_GA_Cluster)
# Uji heteroskedastisitas untuk model FF5 GA Weighted (N)
bptest(ff5_N_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF5 GA Weighted (N)
vif(ff5_N_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 GA Weighted (N)
residuals_ff5_N_GA_Cluster <- residuals(ff5_N_GA_Cluster)
ks.test(residuals_ff5_N_GA_Cluster, "pnorm", mean = mean(residuals_ff5_N_GA_Cluster), sd = sd(residuals_ff5_N_GA_Cluster))

############################################### ff6_H_GA_Cluster ###################################
# Uji autokorelasi untuk model FF6 GA Weighted (H)
bgtest(ff6_H_GA_Cluster)
# Uji heteroskedastisitas untuk model FF6 GA Weighted (H)
bptest(ff6_H_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 GA Weighted (H)
vif(ff6_H_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 GA Weighted (H)
residuals_ff6_H_GA_Cluster <- residuals(ff6_H_GA_Cluster)
ks.test(residuals_ff6_H_GA_Cluster, "pnorm", mean = mean(residuals_ff6_H_GA_Cluster), sd = sd(residuals_ff6_H_GA_Cluster))

############################################### ff6_L_GA_Cluster ###################################
# Uji autokorelasi untuk model FF6 GA Weighted (L)
bgtest(ff6_L_GA_Cluster)
# Uji heteroskedastisitas untuk model FF6 GA Weighted (L)
bptest(ff6_L_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 GA Weighted (L)
vif(ff6_L_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 GA Weighted (L)
residuals_ff6_L_GA_Cluster <- residuals(ff6_L_GA_Cluster)
ks.test(residuals_ff6_L_GA_Cluster, "pnorm", mean = mean(residuals_ff6_L_GA_Cluster), sd = sd(residuals_ff6_L_GA_Cluster))

############################################### ff6_M_GA_Cluster ###################################
# Uji autokorelasi untuk model FF6 GA Weighted (M)
bgtest(ff6_M_GA_Cluster)
# Uji heteroskedastisitas untuk model FF6 GA Weighted (M)
bptest(ff6_M_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 GA Weighted (M)
vif(ff6_M_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 GA Weighted (M)
residuals_ff6_M_GA_Cluster <- residuals(ff6_M_GA_Cluster)
ks.test(residuals_ff6_M_GA_Cluster, "pnorm", mean = mean(residuals_ff6_M_GA_Cluster), sd = sd(residuals_ff6_M_GA_Cluster))

############################################### ff6_N_GA_Cluster ###################################
# Uji autokorelasi untuk model FF6 GA Weighted (N)
bgtest(ff6_N_GA_Cluster)
# Uji heteroskedastisitas untuk model FF6 GA Weighted (N)
bptest(ff6_N_GA_Cluster)
# Uji multikolinieritas dengan VIF untuk model FF6 GA Weighted (N)
vif(ff6_N_GA_Cluster)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 GA Weighted (N)
residuals_ff6_N_GA_Cluster <- residuals(ff6_N_GA_Cluster)
ks.test(residuals_ff6_N_GA_Cluster, "pnorm", mean = mean(residuals_ff6_N_GA_Cluster), sd = sd(residuals_ff6_N_GA_Cluster))



########################################################################################################################

# Pustaka yang dibutuhkan
library(lmtest)    # Untuk uji ARCH
library(urca) # Untuk uji Unit Root (ADF)
library(tseries)   # Untuk uji Unit Root (ADF)

# Fungsi untuk memeriksa ARCH Effect
check_ARCH <- function(residuals) {
  arch_test <- ArchTest(residuals, lag = 1)  # Uji ARCH dengan lag = 1
  return(arch_test$p.value)  # Mengembalikan p-value dari uji ARCH
}

# Fungsi untuk menguji Unit Root (ADF)
check_unit_root <- function(residuals) {
  adf_test <- ur.df(residuals, type = "none", selectlags = "AIC")  # ADF test tanpa tren dan memilih lag dengan AIC
  return(adf_test@teststat)  # Mengembalikan nilai statistik uji ADF
}

# Mengambil Residual dari Model Fama-French
# Misalnya, ff3_H_GA_Cluster adalah salah satu model

# Menyusun model-model Fama-French (contoh beberapa)
# Anda sudah punya model Fama-French sebelumnya, tinggal mengekstrak residunya

# Extract Residuals for FF3, FF5, FF6 for each weighted group
residual_ff3_H_Equal_Cluster <- residuals(ff3_H_Equal_Cluster)
residual_ff3_L_Equal_Cluster <- residuals(ff3_L_Equal_Cluster)
residual_ff3_M_Equal_Cluster <- residuals(ff3_M_Equal_Cluster)
residual_ff3_N_Equal_Cluster <- residuals(ff3_N_Equal_Cluster)

residual_ff3_H_Value_Cluster <- residuals(ff3_H_Value_Cluster)
residual_ff3_L_Value_Cluster <- residuals(ff3_L_Value_Cluster)
residual_ff3_M_Value_Cluster <- residuals(ff3_M_Value_Cluster)
residual_ff3_N_Value_Cluster <- residuals(ff3_N_Value_Cluster)

residual_ff3_H_GA_Cluster <- residuals(ff3_H_GA_Cluster)
residual_ff3_L_GA_Cluster <- residuals(ff3_L_GA_Cluster)
residual_ff3_M_GA_Cluster <- residuals(ff3_M_GA_Cluster)
residual_ff3_N_GA_Cluster <- residuals(ff3_N_GA_Cluster)

# Untuk FF5
residual_ff5_H_Equal_Cluster <- residuals(ff5_H_Equal_Cluster)
residual_ff5_L_Equal_Cluster <- residuals(ff5_L_Equal_Cluster)
residual_ff5_M_Equal_Cluster <- residuals(ff5_M_Equal_Cluster)
residual_ff5_N_Equal_Cluster <- residuals(ff5_N_Equal_Cluster)

residual_ff5_H_Value_Cluster <- residuals(ff5_H_Value_Cluster)
residual_ff5_L_Value_Cluster <- residuals(ff5_L_Value_Cluster)
residual_ff5_M_Value_Cluster <- residuals(ff5_M_Value_Cluster)
residual_ff5_N_Value_Cluster <- residuals(ff5_N_Value_Cluster)

residual_ff5_H_GA_Cluster <- residuals(ff5_H_GA_Cluster)
residual_ff5_L_GA_Cluster <- residuals(ff5_L_GA_Cluster)
residual_ff5_M_GA_Cluster <- residuals(ff5_M_GA_Cluster)
residual_ff5_N_GA_Cluster <- residuals(ff5_N_GA_Cluster)

# Untuk FF6
residual_ff6_H_Equal_Cluster <- residuals(ff6_H_Equal_Cluster)
residual_ff6_L_Equal_Cluster <- residuals(ff6_L_Equal_Cluster)
residual_ff6_M_Equal_Cluster <- residuals(ff6_M_Equal_Cluster)
residual_ff6_N_Equal_Cluster <- residuals(ff6_N_Equal_Cluster)

residual_ff6_H_Value_Cluster <- residuals(ff6_H_Value_Cluster)
residual_ff6_L_Value_Cluster <- residuals(ff6_L_Value_Cluster)
residual_ff6_M_Value_Cluster <- residuals(ff6_M_Value_Cluster)
residual_ff6_N_Value_Cluster <- residuals(ff6_N_Value_Cluster)

residual_ff6_H_GA_Cluster <- residuals(ff6_H_GA_Cluster)
residual_ff6_L_GA_Cluster <- residuals(ff6_L_GA_Cluster)
residual_ff6_M_GA_Cluster <- residuals(ff6_M_GA_Cluster)
residual_ff6_N_GA_Cluster <- residuals(ff6_N_GA_Cluster)

# Cek ARCH Effect dan Unit Root untuk seluruh model

# Model FF3
cat("ARCH Test for ff3_H_Equal_Cluster: ", check_ARCH(residual_ff3_H_Equal_Cluster), "\n")
cat("ARCH Test for ff3_L_Equal_Cluster: ", check_ARCH(residual_ff3_L_Equal_Cluster), "\n")
cat("ARCH Test for ff3_M_Equal_Cluster: ", check_ARCH(residual_ff3_M_Equal_Cluster), "\n")
cat("ARCH Test for ff3_N_Equal_Cluster: ", check_ARCH(residual_ff3_N_Equal_Cluster), "\n")

cat("Unit Root Test for ff3_H_Equal_Cluster: ", check_unit_root(residual_ff3_H_Equal_Cluster), "\n")
cat("Unit Root Test for ff3_L_Equal_Cluster: ", check_unit_root(residual_ff3_L_Equal_Cluster), "\n")
cat("Unit Root Test for ff3_M_Equal_Cluster: ", check_unit_root(residual_ff3_M_Equal_Cluster), "\n")
cat("Unit Root Test for ff3_N_Equal_Cluster: ", check_unit_root(residual_ff3_N_Equal_Cluster), "\n")

# Model FF5
cat("ARCH Test for ff5_H_Equal_Cluster: ", check_ARCH(residual_ff5_H_Equal_Cluster), "\n")
cat("ARCH Test for ff5_L_Equal_Cluster: ", check_ARCH(residual_ff5_L_Equal_Cluster), "\n")
cat("ARCH Test for ff5_M_Equal_Cluster: ", check_ARCH(residual_ff5_M_Equal_Cluster), "\n")
cat("ARCH Test for ff5_N_Equal_Cluster: ", check_ARCH(residual_ff5_N_Equal_Cluster), "\n")

cat("Unit Root Test for ff5_H_Equal_Cluster: ", check_unit_root(residual_ff5_H_Equal_Cluster), "\n")
cat("Unit Root Test for ff5_L_Equal_Cluster: ", check_unit_root(residual_ff5_L_Equal_Cluster), "\n")
cat("Unit Root Test for ff5_M_Equal_Cluster: ", check_unit_root(residual_ff5_M_Equal_Cluster), "\n")
cat("Unit Root Test for ff5_N_Equal_Cluster: ", check_unit_root(residual_ff5_N_Equal_Cluster), "\n")

# Model FF6
cat("ARCH Test for ff6_H_Equal_Cluster: ", check_ARCH(residual_ff6_H_Equal_Cluster), "\n")
cat("ARCH Test for ff6_L_Equal_Cluster: ", check_ARCH(residual_ff6_L_Equal_Cluster), "\n")
cat("ARCH Test for ff6_M_Equal_Cluster: ", check_ARCH(residual_ff6_M_Equal_Cluster), "\n")
cat("ARCH Test for ff6_N_Equal_Cluster: ", check_ARCH(residual_ff6_N_Equal_Cluster), "\n")

cat("Unit Root Test for ff6_H_Equal_Cluster: ", check_unit_root(residual_ff6_H_Equal_Cluster), "\n")
cat("Unit Root Test for ff6_L_Equal_Cluster: ", check_unit_root(residual_ff6_L_Equal_Cluster), "\n")
cat("Unit Root Test for ff6_M_Equal_Cluster: ", check_unit_root(residual_ff6_M_Equal_Cluster), "\n")
cat("Unit Root Test for ff6_N_Equal_Cluster: ", check_unit_root(residual_ff6_N_Equal_Cluster), "\n")
