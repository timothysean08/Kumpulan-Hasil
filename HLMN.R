# Load library
library(readxl)
library(dplyr)

# Baca data dari file Excel
file_path <- "/Users/timothysean/Documents/FINAL DATA/REGRESI FAMA FRENCH/HLMN.xlsx" # Ganti dengan path yang benar
data <- read_excel(file_path, sheet = "Sheet1")

# Lihat beberapa baris pertama dari data untuk memeriksa format
head(data)

H <- data[data$Portfolio == "H", "Intercept"]
L <- data[data$Portfolio == "L", "Intercept"]
M <- data[data$Portfolio == "M", "Intercept"]
N <- data[data$Portfolio == "N", "Intercept"]

# Lakukan uji Shapiro-Wilk untuk normalitas pada masing-masing kelompok
H_values <- H$Intercept  # Mengambil kolom Intercept
L_values <- L$Intercept  # Mengambil kolom Intercept
M_values <- M$Intercept  # Mengambil kolom Intercept
N_values <- N$Intercept  # Mengambil kolom Intercept

shapiro_test_H <- shapiro.test(H_values)  # Uji normalitas pada H
shapiro_test_L <- shapiro.test(L_values)  # Uji normalitas pada H
shapiro_test_M <- shapiro.test(M_values)  # Uji normalitas pada H
shapiro_test_N <- shapiro.test(N_values)  # Uji normalitas pada H


# Asumsikan data memiliki dua kolom: "group" dan "value"
# Uji Kruskal-Wallis
kruskal_test <- kruskal.test(Intercept ~ Portfolio, data = data)
# Tampilkan hasil uji
print(kruskal_test)

##########################H V N###############

# Misalkan H_values dan N_values adalah dua vektor data untuk ESG High dan Non
t_test_result <- t.test(H_values, N_values)

# Melihat hasil uji t
print(t_test_result)

# Ukuran Efek r
z_value <- qnorm(HvN$p.value / 2, lower.tail = FALSE) # Hitung nilai Z dari p-value
n_total <- nrow(HN) # Total jumlah sampel
r_effect_size <- z_value / sqrt(n_total) # Ukuran efek r

# Delta Median (Selisih Median)
median_H <- median(HN$Intercept[HN$Portfolio == "H"])
median_N <- median(HN$Intercept[HN$Portfolio == "N"])
delta_median <- median_H - median_N

# Tampilkan Hasil
cat("Ukuran Efek r: ", r_effect_size, "\n")
cat("Delta Median: ", delta_median, "\n")



##########################L V N###############
# Asumsi data L_values dan N_values adalah numeric dan sudah ada
t_test_result_L_N <- t.test(L_values, N_values)

# Menampilkan hasil uji
print(t_test_result_L_N)

##########################M V N###############
# Filter data hanya untuk grup H dan N
MN <- data %>% filter(Portfolio %in% c("M", "N"))

# Uji Mann-Whitney U (Wilcoxon Rank-Sum Test)
MvN <- wilcox.test(Intercept ~ Portfolio, data = MN, exact = FALSE)

# Tampilkan hasil
print(MvN)


# Filter data hanya untuk grup M dan N
MN <- data %>% filter(Portfolio %in% c("M", "N"))

# Uji Mann-Whitney
MvN <- wilcox.test(Intercept ~ Portfolio, data = MN, exact = FALSE)

# Ukuran Efek r
z_value_MvN <- qnorm(MvN$p.value / 2, lower.tail = FALSE) # Hitung nilai Z dari p-value
n_total_MN <- nrow(MN) # Total jumlah sampel
r_effect_size_MvN <- z_value_MvN / sqrt(n_total_MN) # Ukuran efek r

# Delta Median (Selisih Median)
median_M <- median(MN$Intercept[MN$Portfolio == "M"])
median_N <- median(MN$Intercept[MN$Portfolio == "N"])
delta_median_MvN <- median_M - median_N

# Tampilkan Hasil
cat("Ukuran Efek r: ", r_effect_size_MvN, "\n")
cat("Delta Median: ", delta_median_MvN, "\n")


##############
# Tampilkan semua nama sheet dalam file Excel
file_path <- "/Users/timothysean/Documents/FINAL DATA/REGRESI FAMA FRENCH/metode HLMN.xlsx" # Sesuaikan path file Anda
sheets <- excel_sheets(file_path)
print(sheets) # Tampilkan nama sheet

# Baca salah satu sheet (ganti "SheetName" dengan nama sheet yang diinginkan)
clustertradi <- read_excel(file_path, sheet = "Metode")

# Periksa struktur data untuk memastikan kolom yang akan digunakan
str(clustertradi)

# Memisahkan data berdasarkan kategori 'Metode'
tradi <- clustertradi$Intercept[clustertradi$Metode == "Tradi"]
# Misalkan kategori lain adalah "NonTradi" (sesuaikan dengan data Anda)
cluster <- clustertradi$Intercept[clustertradi$Metode == "Cluster"]

shapiro.test(tradi)
shapiro.test(cluster)
# Uji Mann-Whitney U (Wilcoxon Rank-Sum Test)
clusterVStradi <- wilcox.test(Intercept ~ Metode, data = clustertradi, exact = FALSE)

# Tampilkan hasil
print(clusterVStradi)



#############bobot
sheets <- excel_sheets(file_path)
print(sheets) # Tampilkan nama sheet

# Baca salah satu sheet (ganti "SheetName" dengan nama sheet yang diinginkan)
bobot <- read_excel(file_path, sheet = "BOBOT")

# Periksa struktur data untuk memastikan kolom yang akan digunakan
str(bobot)


# Memisahkan data berdasarkan kategori 'Weight'
equal <- bobot$Intercept[bobot$Weight == "Equal"]
value <- bobot$Intercept[bobot$Weight == "Value"]
GA <- bobot$Intercept[bobot$Weight == "GA"]

# Uji Shapiro-Wilk untuk normalitas
shapiro.test(equal)
shapiro.test(value)
shapiro.test(GA)

# Uji Mann-Whitney (Wilcoxon Rank-Sum Test) antar masing-masing pasangan (Equal vs Value, Equal vs GA, Value vs GA)
wilcox_equal_value <- wilcox.test(equal, value)
wilcox_equal_GA <- wilcox.test(equal, GA)
wilcox_value_GA <- wilcox.test(value, GA)

# Tampilkan hasil uji Mann-Whitney
print(wilcox_equal_value)
print(wilcox_equal_GA)
print(wilcox_value_GA)

# Uji Kruskal-Wallis
bobottest <- kruskal.test(Intercept ~ Weight, data = bobot)

# Tampilkan hasil Kruskal-Wallis
print(bobottest)



#############bobot
sheets <- excel_sheets(file_path)
print(sheets) # Tampilkan nama sheet

# Baca salah satu sheet (ganti "SheetName" dengan nama sheet yang diinginkan)
Model <- read_excel(file_path, sheet = "Model")

# Periksa struktur data untuk memastikan kolom yang akan digunakan
str(Model)

# Memisahkan data berdasarkan kategori 'Metode'
ff3 <- Model$Intercept[Model$Model == "FF3"]
ff5 <- Model$Intercept[Model$Model == "FF5"]
ff6 <- Model$Intercept[Model$Model == "FF6"]

# Uji Shapiro-Wilk untuk normalitas
shapiro.test(ff3)
shapiro.test(ff5)
shapiro.test(ff6)

# Uji Mann-Whitney (Wilcoxon Rank-Sum Test) antar masing-masing pasangan (FF3 vs FF5, FF3 vs FF6, FF5 vs FF6)
wilcox_ff3_ff5 <- wilcox.test(ff3, ff5)
wilcox_ff3_ff6 <- wilcox.test(ff3, ff6)
wilcox_ff5_ff6 <- wilcox.test(ff5, ff6)

# Tampilkan hasil uji Mann-Whitney
print(wilcox_ff3_ff5)
print(wilcox_ff3_ff6)
print(wilcox_ff5_ff6)

# Uji Kruskal-Wallis
Modeltest <- kruskal.test(Intercept ~ Model, data = Model)

# Tampilkan hasil Kruskal-Wallis
print(Modeltest)
