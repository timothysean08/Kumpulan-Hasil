library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)
library(GA)
library(doParallel)


set.seed(1)
# Memuat data dari file
year <- '2015'
target_month <- 'Aug.2015'
data <- read_xlsx('Aug 2015.xlsx', sheet = 1, skip = 1)

previous_year <- as.character(as.numeric(year) - 1)  # Mengubah year menjadi numerik, lalu dikurangi 1, kemudian dikembalikan menjadi string
Profitability <- read_xlsx('PROFITABILITY.xlsx', sheet = 1, skip = 1)
Growth <- read_xlsx('Growth.xlsx', sheet = 1, skip = 1)
Aset <- read_xlsx('total_aset.xlsx')
Rf <- read_xlsx('Market Risk Premium.xlsx')

# Convert the first column to Date format and then format it to "Jul.2015"
Rf[[1]] <- format(as.Date(Rf[[1]]), "%b.%Y")

risk_free_rate_value <- Rf %>%
  filter(`...1` == target_month) %>%  # Replace `...1` with the actual column name if it's different
  pull(`Risk free rate`)    


data['profitability'] <- Profitability[year]
data['growth'] <- Growth[year]
data['aset'] <- Aset[previous_year]


rows_to_remove <- c(96 ,431,580)
data <- data %>%
  slice(-rows_to_remove)


# Mengisi nilai NA di kolom ESG dengan "N" jika tidak ditemukan hasil VLOOKUP
data <- data %>%
  replace_na(list(ESG = "N"))


# Membersihkan kolom 'Kode perusahaan': Menghapus spasi di depan/belakang dan mengubah ke huruf besar
data$`Kode perusahaan` <- str_trim(data$`Kode perusahaan`) %>% toupper()

# Memuat data harga penutupan dari file adjusted closing price
closing_prices <- read_xlsx('closingprice.xlsx')
closing_prices$Ticker <- str_trim(closing_prices[[1]]) %>% toupper()
date_cols <- as.Date(as.numeric(names(closing_prices)[-1]), origin = "1899-12-30")
formatted_dates <- format(date_cols, "%b %Y")
names(closing_prices)[-1] <- formatted_dates

# Memeriksa nama kolom
colnames(closing_prices)

# Mengganti nama kolom kosong atau NA dengan nama yang valid
valid_names <- make.names(colnames(closing_prices), unique = TRUE)
colnames(closing_prices) <- valid_names

# Memastikan tidak ada kolom dengan nama kosong atau NA setelah ini
colnames(closing_prices)

# Misalkan target_month sudah didefinisikan sebagai "Sep.2015"

target_index <- match(target_month, colnames(closing_prices))

# Identifikasi kolom untuk 12 bulan sebelumnya (Sep 2014 hingga Aug 2015)
twelve_months_cols <- colnames(closing_prices)[(target_index - 12):(target_index - 1)]

# Inisialisasi data frame untuk menyimpan hasil log return dengan header bulannya
log_returns_df <- data.frame(matrix(nrow = nrow(closing_prices), ncol = length(twelve_months_cols)))
colnames(log_returns_df) <- twelve_months_cols


# Loop untuk menghitung log return setiap bulan
for (i in 1:length(twelve_months_cols)) {
  # Indeks kolom bulan sekarang dan bulan sebelumnya
  current_col <- twelve_months_cols[i]
  previous_col <- colnames(closing_prices)[match(current_col, colnames(closing_prices)) - 1]
  
  # Menghitung log return dan menyimpannya ke dalam data frame
  log_returns_df[, current_col] <- log(closing_prices[, current_col] / closing_prices[, previous_col])
}

# Menampilkan data frame hasil log return dengan header bulannya
log_returns_df


# Menghitung rata-rata log return untuk setiap baris
average_return_12m <- rowMeans(log_returns_df, na.rm = TRUE)

# Menambahkan rata-rata log return ke dalam data frame hasil sebagai kolom baru
log_returns_df$average_return_12m <- average_return_12m 
log_returns_df$Emiten <- data$Emiten


# Menambahkan kolom rata-rata return 12 bulan sebelumnya ke data utama
data$average_return_12m <- log_returns_df$average_return_12m
previous_month <- colnames(closing_prices)[target_index - 1]

# Menghitung return berdasarkan bulan sebelumnya dan target month
data$bulan_sebelumnya <- closing_prices[[previous_month]]
data$bulan_setelahnya <- closing_prices[[target_month]]
data$Return <- log(data$bulan_setelahnya / data$bulan_sebelumnya)

###############################################################################################
# Menampilkan perusahaan yang memiliki NA di kolom Total equity atau AVERAGE MARKET CAP
perusahaan_teks_na <- data %>%
  filter(`Total equty` == "NA" | `AVERAGE MARKET CAP` == "NA")

# Menyimpan daftar perusahaan yang memiliki NA di kolom Total equity atau AVERAGE MARKET CAP
perusahaan_real_na <- data %>%
  filter(is.na(`Total equty`) | is.na(`AVERAGE MARKET CAP`)) %>%
  select(Emiten, `Total equty`, `AVERAGE MARKET CAP`)


# Menghapus baris dengan NA di kolom Total equity atau AVERAGE MARKET CAP dari dataframe asli
data <- data %>%
  filter(!is.na(`Total equty`) & !is.na(`AVERAGE MARKET CAP`) & `Total equty` != "NA" & `AVERAGE MARKET CAP` != "NA")

# Menampilkan data setelah baris dengan NA dihapus
print(data)

###############################################################################################
data$`AVERAGE MARKET CAP` <- as.numeric(data$`AVERAGE MARKET CAP`)
# Menambahkan kolom log dari market cap
data <- data %>%
  mutate(Log_Market_Cap = log(`AVERAGE MARKET CAP`))

# Identifikasi emiten yang memiliki NA pada kolom tertentu dan simpan dalam variabel
emiten_na <- data %>%
  filter(is.na(profitability) | is.na(growth) | is.na(`BE/ME`) | is.na(Log_Market_Cap) | is.na(average_return_12m)) %>%
  select(Emiten)

# Menghapus baris dengan NA pada kolom profitability, growth, BE/ME, Log_Market_Cap, dan average_return_12m langsung di variabel data
data <- data %>%
  filter(!is.na(profitability) & !is.na(growth) & !is.na(`BE/ME`) & !is.na(Log_Market_Cap) & !is.na(average_return_12m))


# Buat subset dari data yang memiliki nilai Inf pada average_return_12m
perusahaan_inf <- data %>%
  filter(!is.finite(average_return_12m)) %>%
  select(Emiten)  # Ganti 'Emiten' dengan nama kolom yang berisi nama perusahaan

data <- data %>%
  filter(is.finite(average_return_12m))

# Memastikan tidak ada lagi nilai Inf di data
summary(data$average_return_12m)

# Gabungkan semua dataset yang berisi emiten yang ingin dihapus
daftar_emiten_dihapus <- bind_rows(
  emiten_na %>% select(Emiten),
  perusahaan_inf %>% select(Emiten),
  perusahaan_real_na %>% select(Emiten),
  perusahaan_teks_na %>% select(Emiten)
) %>%
  distinct()  # Hapus emiten duplikat jika ada
# Menghapus emiten dari log_returns_df berdasarkan daftar emiten yang akan dihapus
log_returns_df <- log_returns_df %>%
  anti_join(daftar_emiten_dihapus, by = "Emiten")


# Gabungkan log_returns_df dengan data berdasarkan kolom Emiten
# Pastikan kolom Emiten di kedua dataset sudah dalam format yang sama (uppercase, trim whitespace)
log_returns_df$Emiten <- str_trim(toupper(log_returns_df$Emiten))
data$Emiten <- str_trim(toupper(data$Emiten))

# Lakukan left join untuk mendapatkan informasi ESG dari dataset 'data'
log_returns_df$ESG <- data$ESG


# Iterasi melalui setiap kolom log_returns_df yang berisi log return bulanan
for (col in twelve_months_cols) {
  # Ganti NA pada kolom log_return dengan nilai dari average_return_12m di baris yang sama
  log_returns_df[[col]][is.na(log_returns_df[[col]])] <- log_returns_df$average_return_12m[is.na(log_returns_df[[col]])]
}

# Menggunakan Base R untuk menjadikan kolom 'Emiten' sebagai rownames
rownames(log_returns_df) <- log_returns_df$Emiten
log_returns_df <- log_returns_df[, -which(names(log_returns_df) == "Emiten")]  # Menghapus kolom 'Emiten' setelah dijadikan rownames


#######################################################################################################################################################################################################################

# Memisahkan return untuk kelompok ESG H, L, M, dan N berdasarkan 12 bulan terakhir
H <- log_returns_df %>%
  filter(ESG == "H") %>%
  select(twelve_months_cols)
H <- t(H)

L <- log_returns_df %>%
  filter(ESG == "L") %>%
  select(twelve_months_cols)
L <- t(L)

M <- log_returns_df %>%
  filter(ESG == "M") %>%
  select(twelve_months_cols)
M <- t(M)

N <- log_returns_df %>%
  filter(ESG == "N") %>%
  select(twelve_months_cols)
N <- t(N)

# Fungsi untuk menghitung standar deviasi portofolio menggunakan generalisasi rumus
portfolio_sd_custom <- function(weights, returns) {
  # Menghitung matriks kovarian antara semua sekuritas dalam portofolio
  cov_matrix <- cov(returns)
  
  # Menghitung varians portofolio: W' * COV * W
  portfolio_variance <- t(weights) %*% cov_matrix %*% weights
  
  # Mengembalikan akar kuadrat dari varians portofolio (standar deviasi)
  portfolio_sd <- sqrt(portfolio_variance)
  
  return(as.numeric(portfolio_sd))
}



# Fungsi GA untuk optimasi portofolio berdasarkan Sharpe Ratio
optimize_portfolio <- function(returns, risk_free_rate_value, popSize = 50, maxiter = 1000) {
  n_assets <- ncol(returns)  # Jumlah sekuritas dalam kelompok ESG
  fitness = function(w) {
    # Menghitung return portofolio
    wn = c(w,1-sum(w))
    wnn = wn * wn #dibikin supaya lebih kecil lagi bobotnya
    portfolio_return <- sum(wnn * colMeans(returns))
    
    # Menghitung standar deviasi portofolio menggunakan rumus yang baru
    portfolio_sd <- portfolio_sd_custom(wnn, returns)
    
    # Menghitung Sharpe Ratio
    sharpe <- (portfolio_return - risk_free_rate_value) / portfolio_sd
    return(-sharpe)  # Mengembalikan nilai negatif Sharpe Ratio untuk minimisasi
  }
  
  
  ga_optim <- ga(
    fitness = fitness,
    type = "real-valued",
    lower = rep(0, n_assets -1),
    upper = rep(1, n_assets -1),
    popSize = popSize,
    maxiter = maxiter,
    run = 100,
    pmutation = 0.2,
    suggestions = rep(1/n_assets, n_assets-1),  # Awal bobot yang sama rata
    parallel = TRUE,
    optim = TRUE,
    pcrossover = 0.8
    #constraint = function(weights) sum(weights) - 1  # Constraint total bobot harus 1
  )
  
  return(ga_optim@solution)
}


# Optimasi untuk dataset H
opt_weights_H <- optimize_portfolio(H, risk_free_rate_value, popSize = 50, maxiter = 400)
opt_weights_H <- c(opt_weights_H, 1 - sum(opt_weights_H))  # Tambahkan bobot terakhir

# Optimasi untuk dataset L
opt_weights_L <- optimize_portfolio(L, risk_free_rate_value, popSize = 50, maxiter = 400)
opt_weights_L <- c(opt_weights_L, 1 - sum(opt_weights_L))  # Tambahkan bobot terakhir

# Optimasi untuk dataset M
opt_weights_M <- optimize_portfolio(M, risk_free_rate_value, popSize = 50, maxiter = 400)
opt_weights_M <- c(opt_weights_M, 1 - sum(opt_weights_M))  # Tambahkan bobot terakhir

# Optimasi untuk dataset N
opt_weights_N <- optimize_portfolio(N, risk_free_rate_value, popSize = 50, maxiter = 150)
opt_weights_N <- c(opt_weights_N, 1 - sum(opt_weights_N))  # Tambahkan bobot terakhir

# Hasil
list(
  H = opt_weights_H,
  L = opt_weights_L,
  M = opt_weights_M,
  N = opt_weights_N
)
emiten_H <- colnames(H)
emiten_L <- colnames(L)
emiten_M <-colnames(M)
emiten_N <- colnames(N)

# Step 2: Buat data frame untuk masing-masing bobot
df_H <- data.frame(Emiten = emiten_H, Bobot = opt_weights_H)
df_L <- data.frame(Emiten = emiten_L, Bobot = opt_weights_L)
df_M <- data.frame(Emiten = emiten_M, Bobot = opt_weights_M)
df_N <- data.frame(Emiten = emiten_N, Bobot = opt_weights_N)

# Menggabungkan bobot dengan data return
df_H <- df_H %>%
  left_join(data %>% select(Emiten, Return), by = "Emiten")  # Gabungkan bobot dengan return
df_L <- df_L %>%
  left_join(data %>% select(Emiten, Return), by = "Emiten")
df_M <- df_M %>%
  left_join(data %>% select(Emiten, Return), by = "Emiten")
df_N <- df_N %>%
  left_join(data %>% select(Emiten, Return), by = "Emiten")

# Filter data berdasarkan nilai ESG H, L, M, dan N
data_H <- data %>% filter(ESG == "H")
data_L <- data %>% filter(ESG == "L")
data_M <- data %>% filter(ESG == "M")
data_N <- data %>% filter(ESG == "N")

# Menggabungkan bobot dengan data return untuk ESG H
df_H$Return = data_H$Return
# Menggabungkan bobot dengan data return untuk ESG L
df_L$Return = data_L$Return

# Menggabungkan bobot dengan data return untuk ESG M
df_M$Return = data_M$Return

# Menggabungkan bobot dengan data return untuk ESG N
df_N$Return = data_N$Return

# Mengalikan bobot dengan return
df_H <- df_H %>%
  mutate(Weighted_Return = Bobot * Return)
df_L <- df_L %>%
  mutate(Weighted_Return = Bobot * Return)
df_M <- df_M %>%
  mutate(Weighted_Return = Bobot * Return)
df_N <- df_N %>%
  mutate(Weighted_Return = Bobot * Return)

# Menghitung total return portofolio untuk masing-masing kelompok ESG
portfolio_return_H <- sum(df_H$Weighted_Return, na.rm = TRUE)
portfolio_return_L <- sum(df_L$Weighted_Return, na.rm = TRUE)
portfolio_return_M <- sum(df_M$Weighted_Return, na.rm = TRUE)
portfolio_return_N <- sum(df_N$Weighted_Return, na.rm = TRUE)

list(
  H_w = sum(df_H$Bobot),
  L_w = sum(df_L$Bobot),
  M_w = sum(df_M$Bobot),
  N_w = sum(df_N$Bobot)
)

list(
  negative_H <- any(df_H$Bobot < 0),
  negative_l <- any(df_L$Bobot < 0),
  negative_m <- any(df_M$Bobot < 0),
  negative_n <- any(df_N$Bobot < 0)
)

# Menampilkan hasil return portofolio untuk masing-masing kelompok ESG
list(
  H = portfolio_return_H,
  L = portfolio_return_L,
  M = portfolio_return_M,
  N = portfolio_return_N
)

# Misalkan nama dataset adalah 'data'
data_selected <- df_N[, c("Emiten", "Bobot")]
write_xlsx(data_selected, "Bobot_N_Mar.xlsx")

# Misalkan nama dataset adalah 'data'
#data_selected1 <- df_L[, c("Emiten", "Bobot")]
#write_xlsx(data_selected1, "Bobot_L_Jul.xlsx")

# Misalkan nama dataset adalah 'data'
data_selected2 <- df_M[, c("Emiten", "Bobot")]
write_xlsx(data_selected2, "Bobot_M_Jul.xlsx")

# Misalkan nama dataset adalah 'data'
data_selected3 <- df_H[, c("Emiten", "Bobot")]
write_xlsx(data_selected3, "Bobot_H_Nov.xlsx")
