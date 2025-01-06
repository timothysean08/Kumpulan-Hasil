# Hitung selisih beta_0
beta_diff_H_N_ff3_GA <- coef(ff3_H_GA_Tradi)[1] - coef(ff3_N_GA_Tradi)[1]
beta_diff_L_N_ff3_GA <- coef(ff3_L_GA_Tradi)[1] - coef(ff3_N_GA_Tradi)[1]
beta_diff_M_N_ff3_GA <- coef(ff3_M_GA_Tradi)[1] - coef(ff3_N_GA_Tradi)[1]

# Tampilkan hasil
cat("FF3 GA Weighted:\n")
cat("Selisih beta_0 H - N: ", beta_diff_H_N_ff3_GA, "\n")
cat("Selisih beta_0 L - N: ", beta_diff_L_N_ff3_GA, "\n")
cat("Selisih beta_0 M - N: ", beta_diff_M_N_ff3_GA, "\n\n")

# Hitung selisih beta_0
beta_diff_H_N_ff5_GA <- coef(ff5_H_GA_Tradi)[1] - coef(ff5_N_GA_Tradi)[1]
beta_diff_L_N_ff5_GA <- coef(ff5_L_GA_Tradi)[1] - coef(ff5_N_GA_Tradi)[1]
beta_diff_M_N_ff5_GA <- coef(ff5_M_GA_Tradi)[1] - coef(ff5_N_GA_Tradi)[1]

# Tampilkan hasil
cat("FF5 GA Weighted:\n")
cat("Selisih beta_0 H - N: ", beta_diff_H_N_ff5_GA, "\n")
cat("Selisih beta_0 L - N: ", beta_diff_L_N_ff5_GA, "\n")
cat("Selisih beta_0 M - N: ", beta_diff_M_N_ff5_GA, "\n\n")

# Hitung selisih beta_0
beta_diff_H_N_ff6_GA <- coef(ff6_H_GA_Tradi)[1] - coef(ff6_N_GA_Tradi)[1]
beta_diff_L_N_ff6_GA <- coef(ff6_L_GA_Tradi)[1] - coef(ff6_N_GA_Tradi)[1]
beta_diff_M_N_ff6_GA <- coef(ff6_M_GA_Tradi)[1] - coef(ff6_N_GA_Tradi)[1]

# Tampilkan hasil
cat("FF6 GA Weighted:\n")
cat("Selisih beta_0 H - N: ", beta_diff_H_N_ff6_GA, "\n")
cat("Selisih beta_0 L - N: ", beta_diff_L_N_ff6_GA, "\n")
cat("Selisih beta_0 M - N: ", beta_diff_M_N_ff6_GA, "\n\n")

# Hitung selisih beta_0
beta_diff_H_N_ff3_GA_cluster <- coef(ff3_H_GA_Cluster)[1] - coef(ff3_N_GA_Cluster)[1]
beta_diff_L_N_ff3_GA_cluster <- coef(ff3_L_GA_Cluster)[1] - coef(ff3_N_GA_Cluster)[1]
beta_diff_M_N_ff3_GA_cluster <- coef(ff3_M_GA_Cluster)[1] - coef(ff3_N_GA_Cluster)[1]

# Tampilkan hasil
cat("FF3 GA Weighted Cluster:\n")
cat("Selisih beta_0 H - N: ", beta_diff_H_N_ff3_GA_cluster, "\n")
cat("Selisih beta_0 L - N: ", beta_diff_L_N_ff3_GA_cluster, "\n")
cat("Selisih beta_0 M - N: ", beta_diff_M_N_ff3_GA_cluster, "\n\n")

# Hitung selisih beta_0
beta_diff_H_N_ff5_GA_cluster <- coef(ff5_H_GA_Cluster)[1] - coef(ff5_N_GA_Cluster)[1]
beta_diff_L_N_ff5_GA_cluster <- coef(ff5_L_GA_Cluster)[1] - coef(ff5_N_GA_Cluster)[1]
beta_diff_M_N_ff5_GA_cluster <- coef(ff5_M_GA_Cluster)[1] - coef(ff5_N_GA_Cluster)[1]

# Tampilkan hasil
cat("FF5 GA Weighted Cluster:\n")
cat("Selisih beta_0 H - N: ", beta_diff_H_N_ff5_GA_cluster, "\n")
cat("Selisih beta_0 L - N: ", beta_diff_L_N_ff5_GA_cluster, "\n")
cat("Selisih beta_0 M - N: ", beta_diff_M_N_ff5_GA_cluster, "\n\n")

# Hitung selisih beta_0

beta_diff_H_N_ff6_GA_cluster <- coef(ff6_H_GA_Cluster)[1] - coef(ff6_N_GA_Cluster)[1]
beta_diff_L_N_ff6_GA_cluster <- coef(ff6_L_GA_Cluster)[1] - coef(ff6_N_GA_Cluster)[1]
beta_diff_M_N_ff6_GA_cluster <- coef(ff6_M_GA_Cluster)[1] - coef(ff6_N_GA_Cluster)[1]

# Tampilkan hasil
cat("FF6 GA Weighted Cluster:\n")
cat("Selisih beta_0 H - N: ", beta_diff_H_N_ff6_GA_cluster, "\n")
cat("Selisih beta_0 L - N: ", beta_diff_L_N_ff6_GA_cluster, "\n")
cat("Selisih beta_0 M - N: ", beta_diff_M_N_ff6_GA_cluster, "\n\n")
