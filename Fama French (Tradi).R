library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)
library(GA)
library(doParallel)
library(lmtest)
library(car)
library(nlme)
library(prais)
library(t-test)
############## Variabel x ################
cluster = read_xlsx('Cluster.xlsx')
tradi = read_xlsx('Tradisional.xlsx')
market = read_xlsx('Market Risk Premium.xlsx')
############# Variabel y ########
equal = read_xlsx('Equal.xlsx')
value = read_xlsx('Value.xlsx')
GA = read_xlsx('GA.xlsx')
H_Equal = equal$'H'
L_Equal = equal$'L'
M_Equal = equal$'M'
N_Equal = equal$'N'

H_Value = value$'H'
L_Value = value$'L'
M_Value = value$'M'
N_Value = value$'N'

H_GA = GA$'H'
L_GA = GA$'L'
M_GA = GA$'M'
N_GA = GA$'N'

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
##################Menggabungkan ###########
dataset <- data.frame(
  H_Equal = H_Equal,
  L_Equal = L_Equal,
  M_Equal = M_Equal,
  N_Equal = N_Equal,
  H_Value = H_Value,
  L_Value = L_Value,
  M_Value = M_Value,
  N_Value = N_Value,
  MRP = MRP,
  RF = RF,
  SMB_Cluster = SMB_Cluster,
  HML_Cluster = HML_Cluster,
  RMW_Cluster = RMW_Cluster,
  CMA_Cluster = CMA_Cluster,
  MOM_Cluster = MOM_Cluster,
  SMB_Tradi = SMB_Tradi,
  HML_Tradi = HML_Tradi,
  RMW_Tradi = RMW_Tradi,
  CMA_Tradi = CMA_Tradi,
  MOM_Tradi = MOM_Tradi
)
# Membuat kolom 'id' dalam 'dataset' yang berisi nilai 1 hingga 96
dataset <- dataset %>% 
  mutate(id = 1:96)
# Menampilkan data gabungan
head(combined_data)







# TRADI ########################
########## FF3################
######################Equal Weighted##############
ff3_H_Equal_Tradi <- lm((H_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi)
ff3_L_Equal_Tradi<- lm((L_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi)
ff3_M_Equal_Tradi <- lm((M_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi)
ff3_N_Equal_Tradi<- lm((N_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi)

######################Value Weighted##############
ff3_H_Value_Tradi <- lm((H_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi)
ff3_L_Value_Tradi<- lm((L_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi)
ff3_M_Value_Tradi <- lm((M_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi)
ff3_N_Value_Tradi<- lm((N_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi)

###################### GA Weighted ##############
ff3_H_GA_Tradi <- lm((H_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi)
ff3_L_GA_Tradi <- lm((L_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi)
ff3_M_GA_Tradi <- lm((M_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi)
ff3_N_GA_Tradi <- lm((N_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi)

########## FF5################
######################Equal Weighted##############
ff5_H_Equal_Tradi <- lm((H_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi)
ff5_L_Equal_Tradi<- lm((L_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi)
ff5_M_Equal_Tradi <- lm((M_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi)
ff5_N_Equal_Tradi<- lm((N_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi)

######################Value Weighted##############
ff5_H_Value_Tradi <- lm((H_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi)
ff5_L_Value_Tradi<- lm((L_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi)
ff5_M_Value_Tradi <- lm((M_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi)
ff5_N_Value_Tradi<- lm((N_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi)

###################### GA Weighted ##############
ff5_H_GA_Tradi <- lm((H_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi + CMA_Tradi)
ff5_L_GA_Tradi <- lm((L_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi + CMA_Tradi)
ff5_M_GA_Tradi <- lm((M_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi + CMA_Tradi)
ff5_N_GA_Tradi <- lm((N_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi + CMA_Tradi)

########## FF6################
######################Equal Weighted##############
ff6_H_Equal_Tradi <- lm((H_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi + MOM_Tradi)
ff6_L_Equal_Tradi<- lm((L_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi+ MOM_Tradi)
ff6_M_Equal_Tradi <- lm((M_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi+ MOM_Tradi)
ff6_N_Equal_Tradi<- lm((N_Equal-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi+ MOM_Tradi)

######################Value Weighted##############
ff6_H_Value_Tradi <- lm((H_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi+ MOM_Tradi)
ff6_L_Value_Tradi<- lm((L_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi+ MOM_Tradi)
ff6_M_Value_Tradi <- lm((M_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi+ MOM_Tradi)
ff6_N_Value_Tradi<- lm((N_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi+ MOM_Tradi)
###################### GA Weighted ##############
ff6_H_GA_Tradi <- lm((H_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi + CMA_Tradi + MOM_Tradi)
ff6_L_GA_Tradi <- lm((L_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi + CMA_Tradi + MOM_Tradi)
ff6_M_GA_Tradi <- lm((M_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi + CMA_Tradi + MOM_Tradi)
ff6_N_GA_Tradi <- lm((N_GA-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi + CMA_Tradi + MOM_Tradi)


###############################################ff3_H_Equal_Tradi###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (H)
bgtest(ff3_H_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (H)
bptest(ff3_H_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (H)
vif(ff3_H_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (H)
residuals_ff3_H_Equal_Tradi <- residuals(ff3_H_Equal_Tradi)
ks.test(residuals_ff3_H_Equal_Tradi, "pnorm", mean = mean(residuals_ff3_H_Equal_Tradi), sd = sd(residuals_ff3_H_Equal_Tradi))

###############################################ff3_L_Equal_Tradi###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (H)
bgtest(ff3_L_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (H)
bptest(ff3_L_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (H)
vif(ff3_L_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (H)
residuals_ff3_L_Equal_Tradi <- residuals(ff3_L_Equal_Tradi)
ks.test(residuals_ff3_L_Equal_Tradi, "pnorm", mean = mean(residuals_ff3_L_Equal_Tradi), sd = sd(residuals_ff3_L_Equal_Tradi))

###############################################ff3_M_Equal_Tradi###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (H)
bgtest(ff3_M_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (H)
bptest(ff3_M_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (H)
vif(ff3_M_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (H)
residuals_ff3_M_Equal_Tradi <- residuals(ff3_M_Equal_Tradi)
ks.test(residuals_ff3_M_Equal_Tradi, "pnorm", mean = mean(residuals_ff3_M_Equal_Tradi), sd = sd(residuals_ff3_M_Equal_Tradi))

###############################################ff3_N_Equal_Tradi###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (H)
bgtest(ff3_N_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (H)
bptest(ff3_N_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (H)
vif(ff3_N_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (H)
residuals_ff3_N_Equal_Tradi <- residuals(ff3_N_Equal_Tradi)
ks.test(residuals_ff3_N_Equal_Tradi, "pnorm", mean = mean(residuals_ff3_N_Equal_Tradi), sd = sd(residuals_ff3_N_Equal_Tradi))





###############################################ff3_H_Value_Tradi###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (H)
bgtest(ff3_H_Value_Tradi)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (H)
bptest(ff3_H_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (H)
vif(ff3_H_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (H)
residuals_ff3_H_Value_Tradi <- residuals(ff3_H_Value_Tradi)
ks.test(residuals_ff3_H_Value_Tradi, "pnorm", mean = mean(residuals_ff3_H_Value_Tradi), sd = sd(residuals_ff3_H_Value_Tradi))

###############################################ff3_L_Value_Tradi###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (H)
bgtest(ff3_L_Value_Tradi)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (H)
bptest(ff3_L_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (H)
vif(ff3_L_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (H)
residuals_ff3_L_Value_Tradi <- residuals(ff3_L_Value_Tradi)
ks.test(residuals_ff3_L_Value_Tradi, "pnorm", mean = mean(residuals_ff3_L_Value_Tradi), sd = sd(residuals_ff3_L_Value_Tradi))

###############################################ff3_M_Value_Tradi###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (H)
bgtest(ff3_M_Value_Tradi)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (H)
bptest(ff3_M_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (H)
vif(ff3_M_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (H)
residuals_ff3_M_Value_Tradi <- residuals(ff3_M_Value_Tradi)
ks.test(residuals_ff3_M_Value_Tradi, "pnorm", mean = mean(residuals_ff3_M_Value_Tradi), sd = sd(residuals_ff3_M_Value_Tradi))

###############################################ff3_N_Value_Tradi###################################
# Uji autokorelasi untuk model FF3 Equal Weighted (H)
bgtest(ff3_N_Value_Tradi)
# Uji heteroskedastisitas untuk model FF3 Equal Weighted (H)
bptest(ff3_N_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 Equal Weighted (H)
vif(ff3_N_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 Equal Weighted (H)
residuals_ff3_N_Value_Tradi <- residuals(ff3_N_Value_Tradi)
ks.test(residuals_ff3_N_Value_Tradi, "pnorm", mean = mean(residuals_ff3_N_Value_Tradi), sd = sd(residuals_ff3_N_Value_Tradi))



###############################################ff5_H_Equal_Tradi###################################
# Uji autokorelasi untuk model FF5 Equal Weighted (H)
bgtest(ff5_H_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF5 Equal Weighted (H)
bptest(ff5_H_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 Equal Weighted (H)
vif(ff5_H_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Equal Weighted (H)
residuals_ff5_H_Equal_Tradi <- residuals(ff5_H_Equal_Tradi)
ks.test(residuals_ff5_H_Equal_Tradi, "pnorm", mean = mean(residuals_ff5_H_Equal_Tradi), sd = sd(residuals_ff5_H_Equal_Tradi))

###############################################ff5_L_Equal_Tradi###################################
# Uji autokorelasi untuk model FF5 Equal Weighted (L)
bgtest(ff5_L_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF5 Equal Weighted (L)
bptest(ff5_L_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 Equal Weighted (L)
vif(ff5_L_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Equal Weighted (L)
residuals_ff5_L_Equal_Tradi <- residuals(ff5_L_Equal_Tradi)
ks.test(residuals_ff5_L_Equal_Tradi, "pnorm", mean = mean(residuals_ff5_L_Equal_Tradi), sd = sd(residuals_ff5_L_Equal_Tradi))

###############################################ff5_M_Equal_Tradi###################################
# Uji autokorelasi untuk model FF5 Equal Weighted (M)
bgtest(ff5_M_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF5 Equal Weighted (M)
bptest(ff5_M_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 Equal Weighted (M)
vif(ff5_M_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Equal Weighted (M)
residuals_ff5_M_Equal_Tradi <- residuals(ff5_M_Equal_Tradi)
ks.test(residuals_ff5_M_Equal_Tradi, "pnorm", mean = mean(residuals_ff5_M_Equal_Tradi), sd = sd(residuals_ff5_M_Equal_Tradi))

###############################################ff5_N_Equal_Tradi###################################
# Uji autokorelasi untuk model FF5 Equal Weighted (N)
bgtest(ff5_N_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF5 Equal Weighted (N)
bptest(ff5_N_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 Equal Weighted (N)
vif(ff5_N_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Equal Weighted (N)
residuals_ff5_N_Equal_Tradi <- residuals(ff5_N_Equal_Tradi)
ks.test(residuals_ff5_N_Equal_Tradi, "pnorm", mean = mean(residuals_ff5_N_Equal_Tradi), sd = sd(residuals_ff5_N_Equal_Tradi))





###############################################ff5_H_Value_Tradi###################################
# Uji autokorelasi untuk model FF5 Value Weighted (H)
bgtest(ff5_H_Value_Tradi)
# Uji heteroskedastisitas untuk model FF5 Value Weighted (H)
bptest(ff5_H_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 Value Weighted (H)
vif(ff5_H_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Value Weighted (H)
residuals_ff5_H_Value_Tradi <- residuals(ff5_H_Value_Tradi)
ks.test(residuals_ff5_H_Value_Tradi, "pnorm", mean = mean(residuals_ff5_H_Value_Tradi), sd = sd(residuals_ff5_H_Value_Tradi))

###############################################ff5_L_Value_Tradi###################################
# Uji autokorelasi untuk model FF5 Value Weighted (L)
bgtest(ff5_L_Value_Tradi)
# Uji heteroskedastisitas untuk model FF5 Value Weighted (L)
bptest(ff5_L_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 Value Weighted (L)
vif(ff5_L_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Value Weighted (L)
residuals_ff5_L_Value_Tradi <- residuals(ff5_L_Value_Tradi)
ks.test(residuals_ff5_L_Value_Tradi, "pnorm", mean = mean(residuals_ff5_L_Value_Tradi), sd = sd(residuals_ff5_L_Value_Tradi))

###############################################ff5_M_Value_Tradi###################################
# Uji autokorelasi untuk model FF5 Value Weighted (M)
bgtest(ff5_M_Value_Tradi)
# Uji heteroskedastisitas untuk model FF5 Value Weighted (M)
bptest(ff5_M_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 Value Weighted (M)
vif(ff5_M_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Value Weighted (M)
residuals_ff5_M_Value_Tradi <- residuals(ff5_M_Value_Tradi)
ks.test(residuals_ff5_M_Value_Tradi, "pnorm", mean = mean(residuals_ff5_M_Value_Tradi), sd = sd(residuals_ff5_M_Value_Tradi))

###############################################ff5_N_Value_Tradi###################################
# Uji autokorelasi untuk model FF5 Value Weighted (N)
bgtest(ff5_N_Value_Tradi)
# Uji heteroskedastisitas untuk model FF5 Value Weighted (N)
bptest(ff5_N_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 Value Weighted (N)
vif(ff5_N_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 Value Weighted (N)
residuals_ff5_N_Value_Tradi <- residuals(ff5_N_Value_Tradi)
ks.test(residuals_ff5_N_Value_Tradi, "pnorm", mean = mean(residuals_ff5_N_Value_Tradi), sd = sd(residuals_ff5_N_Value_Tradi))




###############################################ff6_H_Equal_Tradi###################################
# Uji autokorelasi untuk model FF6 Equal Weighted (H)
bgtest(ff6_H_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF6 Equal Weighted (H)
bptest(ff6_H_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 Equal Weighted (H)
vif(ff6_H_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Equal Weighted (H)
residuals_ff6_H_Equal_Tradi <- residuals(ff6_H_Equal_Tradi)
ks.test(residuals_ff6_H_Equal_Tradi, "pnorm", mean = mean(residuals_ff6_H_Equal_Tradi), sd = sd(residuals_ff6_H_Equal_Tradi))

###############################################ff6_L_Equal_Tradi###################################
# Uji autokorelasi untuk model FF6 Equal Weighted (L)
bgtest(ff6_L_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF6 Equal Weighted (L)
bptest(ff6_L_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 Equal Weighted (L)
vif(ff6_L_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Equal Weighted (L)
residuals_ff6_L_Equal_Tradi <- residuals(ff6_L_Equal_Tradi)
ks.test(residuals_ff6_L_Equal_Tradi, "pnorm", mean = mean(residuals_ff6_L_Equal_Tradi), sd = sd(residuals_ff6_L_Equal_Tradi))

###############################################ff6_M_Equal_Tradi###################################
# Uji autokorelasi untuk model FF6 Equal Weighted (M)
bgtest(ff6_M_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF6 Equal Weighted (M)
bptest(ff6_M_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 Equal Weighted (M)
vif(ff6_M_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Equal Weighted (M)
residuals_ff6_M_Equal_Tradi <- residuals(ff6_M_Equal_Tradi)
ks.test(residuals_ff6_M_Equal_Tradi, "pnorm", mean = mean(residuals_ff6_M_Equal_Tradi), sd = sd(residuals_ff6_M_Equal_Tradi))

###############################################ff6_N_Equal_Tradi###################################
# Uji autokorelasi untuk model FF6 Equal Weighted (N)
bgtest(ff6_N_Equal_Tradi)
# Uji heteroskedastisitas untuk model FF6 Equal Weighted (N)
bptest(ff6_N_Equal_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 Equal Weighted (N)
vif(ff6_N_Equal_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Equal Weighted (N)
residuals_ff6_N_Equal_Tradi <- residuals(ff6_N_Equal_Tradi)
ks.test(residuals_ff6_N_Equal_Tradi, "pnorm", mean = mean(residuals_ff6_N_Equal_Tradi), sd = sd(residuals_ff6_N_Equal_Tradi))





###############################################ff6_H_Value_Tradi###################################
# Uji autokorelasi untuk model FF6 Value Weighted (H)
bgtest(ff6_H_Value_Tradi)
# Uji heteroskedastisitas untuk model FF6 Value Weighted (H)
bptest(ff6_H_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 Value Weighted (H)
vif(ff6_H_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Value Weighted (H)
residuals_ff6_H_Value_Tradi <- residuals(ff6_H_Value_Tradi)
ks.test(residuals_ff6_H_Value_Tradi, "pnorm", mean = mean(residuals_ff6_H_Value_Tradi), sd = sd(residuals_ff6_H_Value_Tradi))

###############################################ff6_L_Value_Tradi###################################
# Uji autokorelasi untuk model FF6 Value Weighted (L)
bgtest(ff6_L_Value_Tradi)
# Uji heteroskedastisitas untuk model FF6 Value Weighted (L)
bptest(ff6_L_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 Value Weighted (L)
vif(ff6_L_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Value Weighted (L)
residuals_ff6_L_Value_Tradi <- residuals(ff6_L_Value_Tradi)
ks.test(residuals_ff6_L_Value_Tradi, "pnorm", mean = mean(residuals_ff6_L_Value_Tradi), sd = sd(residuals_ff6_L_Value_Tradi))

###############################################ff6_M_Value_Tradi###################################
# Uji autokorelasi untuk model FF6 Value Weighted (M)
bgtest(ff6_M_Value_Tradi)
# Uji heteroskedastisitas untuk model FF6 Value Weighted (M)
bptest(ff6_M_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 Value Weighted (M)
vif(ff6_M_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Value Weighted (M)
residuals_ff6_M_Value_Tradi <- residuals(ff6_M_Value_Tradi)
ks.test(residuals_ff6_M_Value_Tradi, "pnorm", mean = mean(residuals_ff6_M_Value_Tradi), sd = sd(residuals_ff6_M_Value_Tradi))

###############################################ff6_N_Value_Tradi###################################
# Uji autokorelasi untuk model FF6 Value Weighted (N)
bgtest(ff6_N_Value_Tradi)
# Uji heteroskedastisitas untuk model FF6 Value Weighted (N)
bptest(ff6_N_Value_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 Value Weighted (N)
vif(ff6_N_Value_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 Value Weighted (N)
residuals_ff6_N_Value_Tradi <- residuals(ff6_N_Value_Tradi)
ks.test(residuals_ff6_N_Value_Tradi, "pnorm", mean = mean(residuals_ff6_N_Value_Tradi), sd = sd(residuals_ff6_N_Value_Tradi))


############################################### ff3_H_GA_Tradi ###################################
# Uji autokorelasi untuk model FF3 GA Weighted (H)
bgtest(ff3_H_GA_Tradi)
# Uji heteroskedastisitas untuk model FF3 GA Weighted (H)
bptest(ff3_H_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 GA Weighted (H)
vif(ff3_H_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 GA Weighted (H)
residuals_ff3_H_GA_Tradi <- residuals(ff3_H_GA_Tradi)
ks.test(residuals_ff3_H_GA_Tradi, "pnorm", mean = mean(residuals_ff3_H_GA_Tradi), sd = sd(residuals_ff3_H_GA_Tradi))

############################################### ff3_L_GA_Tradi ###################################
# Uji autokorelasi untuk model FF3 GA Weighted (L)
bgtest(ff3_L_GA_Tradi)
# Uji heteroskedastisitas untuk model FF3 GA Weighted (L)
bptest(ff3_L_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 GA Weighted (L)
vif(ff3_L_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 GA Weighted (L)
residuals_ff3_L_GA_Tradi <- residuals(ff3_L_GA_Tradi)
ks.test(residuals_ff3_L_GA_Tradi, "pnorm", mean = mean(residuals_ff3_L_GA_Tradi), sd = sd(residuals_ff3_L_GA_Tradi))

############################################### ff3_M_GA_Tradi ###################################
# Uji autokorelasi untuk model FF3 GA Weighted (M)
bgtest(ff3_M_GA_Tradi)
# Uji heteroskedastisitas untuk model FF3 GA Weighted (M)
bptest(ff3_M_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 GA Weighted (M)
vif(ff3_M_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 GA Weighted (M)
residuals_ff3_M_GA_Tradi <- residuals(ff3_M_GA_Tradi)
ks.test(residuals_ff3_M_GA_Tradi, "pnorm", mean = mean(residuals_ff3_M_GA_Tradi), sd = sd(residuals_ff3_M_GA_Tradi))

############################################### ff3_N_GA_Tradi ###################################
# Uji autokorelasi untuk model FF3 GA Weighted (N)
bgtest(ff3_N_GA_Tradi)
# Uji heteroskedastisitas untuk model FF3 GA Weighted (N)
bptest(ff3_N_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF3 GA Weighted (N)
vif(ff3_N_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF3 GA Weighted (N)
residuals_ff3_N_GA_Tradi <- residuals(ff3_N_GA_Tradi)
ks.test(residuals_ff3_N_GA_Tradi, "pnorm", mean = mean(residuals_ff3_N_GA_Tradi), sd = sd(residuals_ff3_N_GA_Tradi))

############################################### ff5_H_GA_Tradi ###################################
# Uji autokorelasi untuk model FF5 GA Weighted (H)
bgtest(ff5_H_GA_Tradi)
# Uji heteroskedastisitas untuk model FF5 GA Weighted (H)
bptest(ff5_H_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 GA Weighted (H)
vif(ff5_H_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 GA Weighted (H)
residuals_ff5_H_GA_Tradi <- residuals(ff5_H_GA_Tradi)
ks.test(residuals_ff5_H_GA_Tradi, "pnorm", mean = mean(residuals_ff5_H_GA_Tradi), sd = sd(residuals_ff5_H_GA_Tradi))

############################################### ff5_L_GA_Tradi ###################################
# Uji autokorelasi untuk model FF5 GA Weighted (L)
bgtest(ff5_L_GA_Tradi)
# Uji heteroskedastisitas untuk model FF5 GA Weighted (L)
bptest(ff5_L_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 GA Weighted (L)
vif(ff5_L_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 GA Weighted (L)
residuals_ff5_L_GA_Tradi <- residuals(ff5_L_GA_Tradi)
ks.test(residuals_ff5_L_GA_Tradi, "pnorm", mean = mean(residuals_ff5_L_GA_Tradi), sd = sd(residuals_ff5_L_GA_Tradi))

############################################### ff5_M_GA_Tradi ###################################
# Uji autokorelasi untuk model FF5 GA Weighted (M)
bgtest(ff5_M_GA_Tradi)
# Uji heteroskedastisitas untuk model FF5 GA Weighted (M)
bptest(ff5_M_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 GA Weighted (M)
vif(ff5_M_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 GA Weighted (M)
residuals_ff5_M_GA_Tradi <- residuals(ff5_M_GA_Tradi)
ks.test(residuals_ff5_M_GA_Tradi, "pnorm", mean = mean(residuals_ff5_M_GA_Tradi), sd = sd(residuals_ff5_M_GA_Tradi))

############################################### ff5_N_GA_Tradi ###################################
# Uji autokorelasi untuk model FF5 GA Weighted (N)
bgtest(ff5_N_GA_Tradi)
# Uji heteroskedastisitas untuk model FF5 GA Weighted (N)
bptest(ff5_N_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF5 GA Weighted (N)
vif(ff5_N_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF5 GA Weighted (N)
residuals_ff5_N_GA_Tradi <- residuals(ff5_N_GA_Tradi)
ks.test(residuals_ff5_N_GA_Tradi, "pnorm", mean = mean(residuals_ff5_N_GA_Tradi), sd = sd(residuals_ff5_N_GA_Tradi))

############################################### ff6_H_GA_Tradi ###################################
# Uji autokorelasi untuk model FF6 GA Weighted (H)
bgtest(ff6_H_GA_Tradi)
# Uji heteroskedastisitas untuk model FF6 GA Weighted (H)
bptest(ff6_H_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 GA Weighted (H)
vif(ff6_H_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 GA Weighted (H)
residuals_ff6_H_GA_Tradi <- residuals(ff6_H_GA_Tradi)
ks.test(residuals_ff6_H_GA_Tradi, "pnorm", mean = mean(residuals_ff6_H_GA_Tradi), sd = sd(residuals_ff6_H_GA_Tradi))

############################################### ff6_L_GA_Tradi ###################################
# Uji autokorelasi untuk model FF6 GA Weighted (L)
bgtest(ff6_L_GA_Tradi)
# Uji heteroskedastisitas untuk model FF6 GA Weighted (L)
bptest(ff6_L_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 GA Weighted (L)
vif(ff6_L_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 GA Weighted (L)
residuals_ff6_L_GA_Tradi <- residuals(ff6_L_GA_Tradi)
ks.test(residuals_ff6_L_GA_Tradi, "pnorm", mean = mean(residuals_ff6_L_GA_Tradi), sd = sd(residuals_ff6_L_GA_Tradi))

############################################### ff6_M_GA_Tradi ###################################
# Uji autokorelasi untuk model FF6 GA Weighted (M)
bgtest(ff6_M_GA_Tradi)
# Uji heteroskedastisitas untuk model FF6 GA Weighted (M)
bptest(ff6_M_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 GA Weighted (M)
vif(ff6_M_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 GA Weighted (M)
residuals_ff6_M_GA_Tradi <- residuals(ff6_M_GA_Tradi)
ks.test(residuals_ff6_M_GA_Tradi, "pnorm", mean = mean(residuals_ff6_M_GA_Tradi), sd = sd(residuals_ff6_M_GA_Tradi))

############################################### ff6_N_GA_Tradi ###################################
# Uji autokorelasi untuk model FF6 GA Weighted (N)
bgtest(ff6_N_GA_Tradi)
# Uji heteroskedastisitas untuk model FF6 GA Weighted (N)
bptest(ff6_N_GA_Tradi)
# Uji multikolinieritas dengan VIF untuk model FF6 GA Weighted (N)
vif(ff6_N_GA_Tradi)
# Uji normalitas Kolmogorov-Smirnov untuk residual dari model FF6 GA Weighted (N)
residuals_ff6_N_GA_Tradi <- residuals(ff6_N_GA_Tradi)
ks.test(residuals_ff6_N_GA_Tradi, "pnorm", mean = mean(residuals_ff6_N_GA_Tradi), sd = sd(residuals_ff6_N_GA_Tradi))







#Hasiil percobaan GLS
gls_model <- prais_winsten((L_Value-RF) ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi +CMA_Tradi+ MOM_Tradi,
                data = dataset, index = 'id')
# Melihat ringkasan hasil GLS
summary(gls_model)
# Ekstrak residuals dari model GLS
gls_residuals <- residuals(gls_model)
acf(gls_residuals)
# Buat regresi untuk residuals sebagai fungsi dari prediktor asli
resid_model <- lm(gls_residuals^2 ~ MRP + SMB_Tradi + HML_Tradi + RMW_Tradi + CMA_Tradi, data = dataset)
summary(resid_model)
