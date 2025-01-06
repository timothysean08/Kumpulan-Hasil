library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)

set.seed(1)
# Memuat data dari file
year <- '2017'
target_month <- 'Dec.2017'
data <- read_xlsx('Des 2017.xlsx', sheet = 1, skip = 1)


previous_year <- as.character(as.numeric(year) - 1)  # Mengubah year menjadi numerik, lalu dikurangi 1, kemudian dikembalikan menjadi string
Profitability <- read_xlsx('PROFITABILITY.xlsx', sheet = 1, skip = 1)
Growth <- read_xlsx('Growth.xlsx', sheet = 1, skip = 1)
Aset <- read_xlsx('total_aset.xlsx')
data['profitability'] <- Profitability[year]
data['growth'] <- Growth[year]
data['aset'] <- Aset[previous_year]


rows_to_remove <- c(96 ,431,580)
data <- data %>%
  slice(-rows_to_remove)

esg <- read_xlsx('esg dataset.xlsx', sheet = 1)
# Menghapus baris di mana 'nama perusahaan' hanya angka
esg <- esg %>%
  filter(!grepl("^[0-9]+$", `nama perusahaan`)) %>%
  select(company_name = `nama perusahaan`, ESG)

# Membersihkan kolom 'Emiten' dan 'nama perusahaan' dengan menghapus spasi dan mengubah ke huruf besar
data$Emiten <- str_trim(data$Emiten) %>% toupper()
esg$company_name <- str_trim(esg$company_name) %>% toupper()

# Melakukan left join seperti VLOOKUP untuk menambahkan kolom ESG ke dalam dataset 'data'
data <- data %>%
  left_join(esg, by = c("Emiten" = "company_name"))

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

data1 <- data


#######################################
# Menggunakan k-means clustering untuk Log Market Cap
k_log_market_cap <- 2  # Jumlah klaster yang diinginkan
km_log_market_cap <- kmeans(data$Log_Market_Cap, centers = k_log_market_cap)
data <- data %>%
  mutate(LogMarketCapCluster = as.factor(km_log_market_cap$cluster))

############# BE/ME ############################

# Menggunakan k-means clustering untuk BE/ME
k_be_me <- 3  # Jumlah klaster yang diinginkan
data$`BE/ME` <- as.numeric(data$`BE/ME`)

# Fungsi untuk melakukan clustering pada BE/ME berdasarkan klaster Log Market Cap
cluster_be_me <- function(sub_data, k_be_me) {
  set.seed(1)  # Menetapkan seed di sini juga untuk konsistensi
  km_BEME <- kmeans(sub_data$`BE/ME`, centers = k_be_me)
  return(as.factor(km_BEME$cluster))
}

# Inisialisasi kolom untuk klaster BE/ME
data$BE_ME_Cluster <- NA

# Melakukan clustering BE/ME berdasarkan setiap klaster Log Market Cap
for (cluster in unique(data$LogMarketCapCluster)) {
  sub_data <- data %>% filter(LogMarketCapCluster == cluster)
  be_me_clusters <- cluster_be_me(sub_data, k_be_me)
  data$BE_ME_Cluster[data$LogMarketCapCluster == cluster] <- be_me_clusters
}

# Menggabungkan informasi klaster Log Market Cap dan BE/ME menjadi satu kolom
data <- data %>%
  mutate(FinalCluster = paste(LogMarketCapCluster, BE_ME_Cluster, sep = "-"))

# Menghitung Sum Log Market Cap, BE/ME, dan Total Return untuk masing-masing klaster
cluster_summary_BE_ME <- data %>%
  group_by(LogMarketCapCluster, BE_ME_Cluster, FinalCluster) %>%
  summarise(
    Count = n(),
    Sum_Return = sum(Return, na.rm = TRUE),
    Mean_Log_MarketCap = mean(Log_Market_Cap, na.rm = TRUE),
    Mean_BE_ME = mean(`BE/ME`, na.rm = TRUE)
  )

# Menentukan kategori Log Market Cap berdasarkan rata-rata Log Market Cap di masing-masing klaster
log_marketcap_summary <- cluster_summary_BE_ME %>%
  group_by(LogMarketCapCluster) %>%
  summarise(Mean_Log_MarketCap = mean(Mean_Log_MarketCap, na.rm = TRUE)) %>%
  mutate(LogMarketCap_Category = ifelse(Mean_Log_MarketCap > mean(Mean_Log_MarketCap), "Big", "Small"))


# Menentukan kategori BE/ME berdasarkan nilai rata-rata BE/ME dalam setiap subset cluster Log Market Cap
cluster_summary_BE_ME <- cluster_summary_BE_ME %>%
  group_by(LogMarketCapCluster) %>%
  mutate(
    BE_ME_Category = case_when(
      Mean_BE_ME == min(Mean_BE_ME) ~ "Small",
      Mean_BE_ME == max(Mean_BE_ME) ~ "Big",
      TRUE ~ "Medium"
    )
  )

# Menggabungkan kategori Log Market Cap ke data utama
data <- data %>%
  left_join(log_marketcap_summary %>% select(LogMarketCapCluster, LogMarketCap_Category), by = "LogMarketCapCluster")

# Menggabungkan kategori BE/ME ke data utama
data <- data %>%
  left_join(cluster_summary_BE_ME %>% select(FinalCluster, BE_ME_Category), by = "FinalCluster")

# Membuat variabel baru berdasarkan kategori
data <- data %>%
  mutate(
    Cluster_Label = case_when(
      LogMarketCap_Category == "Small" & BE_ME_Category == "Small" ~ "SS",
      LogMarketCap_Category == "Small" & BE_ME_Category == "Medium" ~ "SM",
      LogMarketCap_Category == "Small" & BE_ME_Category == "Big" ~ "SB",
      LogMarketCap_Category == "Big" & BE_ME_Category == "Small" ~ "BS",
      LogMarketCap_Category == "Big" & BE_ME_Category == "Medium" ~ "BM",
      LogMarketCap_Category == "Big" & BE_ME_Category == "Big" ~ "BB"
    )
  )

# Menghitung rata-rata return untuk masing-masing klaster
average_return <- data %>%
  group_by(Cluster_Label) %>%
  summarise(Mean_Return = mean(Return, na.rm = TRUE))

# Menghitung Small Minus Big (SMB) sebagai (SS + SM + SB) - (BS + BM + BB)
SMB_cluster <- average_return %>%
  filter(Cluster_Label %in% c("SS", "SM", "SB", "BS", "BM", "BB")) %>%
  summarise(
    Small = mean(Mean_Return[Cluster_Label %in% c("SS", "SM", "SB")]),
    Big = mean(Mean_Return[Cluster_Label %in% c("BS", "BM", "BB")])
  ) %>%
  mutate(SMB_cluster = Small - Big)

# Menghitung High Minus Low (HML) sebagai (Big BE/ME) - (Small BE/ME)
HML_cluster <- average_return %>%
  filter(Cluster_Label %in% c("SS", "SB", "BS", "BB")) %>%
  summarise(
    High = mean(Mean_Return[Cluster_Label %in% c("SB", "BB")]),
    Low = mean(Mean_Return[Cluster_Label %in% c("SS", "BS")])
  ) %>%
  mutate(HML_cluster = High - Low)

# Menampilkan hasil SMB dan HML
print(SMB_cluster)
print(HML_cluster)
    

########## PROFITABILITY ############################

# Menggunakan k-means clustering untuk profitability
k_profitability <- 3  # Jumlah klaster yang diinginkan
data$profitability <- as.numeric(data$profitability)

# Fungsi untuk melakukan clustering pada profitability berdasarkan klaster Log Market Cap (menggunakan LogMarketCapCluster.x)
cluster_profitability <- function(sub_data, k_profitability) {
  set.seed(1)  # Menetapkan seed di sini juga untuk konsistensi
  km_profitability <- kmeans(sub_data$profitability, centers = k_profitability)
  return(as.factor(km_profitability$cluster))
}

# Inisialisasi kolom untuk klaster profitability
data$Profitability_Cluster <- NA

# Melakukan clustering profitability berdasarkan setiap klaster Log Market Cap (LogMarketCapCluster.x)
for (cluster in unique(data$LogMarketCapCluster.x)) {
  sub_data <- data %>% filter(LogMarketCapCluster.x == cluster)
  profitability_clusters <- cluster_profitability(sub_data, k_profitability)
  data$Profitability_Cluster[data$LogMarketCapCluster.x == cluster] <- profitability_clusters
}

# Menggabungkan informasi klaster Log Market Cap dan profitability menjadi satu kolom
data <- data %>%
  mutate(FinalCluster = paste(LogMarketCapCluster.x, Profitability_Cluster, sep = "-"))

# Menghitung Sum Log Market Cap, profitability, dan Total Return untuk masing-masing klaster
cluster_summary_profitability <- data %>%
  group_by(LogMarketCapCluster.x, Profitability_Cluster, FinalCluster) %>%
  summarise(
    Count = n(),
    Sum_Return = sum(Return, na.rm = TRUE),
    Mean_Log_MarketCap = mean(Log_Market_Cap, na.rm = TRUE),
    Mean_Profitability = mean(profitability, na.rm = TRUE)
  )


# Menampilkan hasil cluster_summary_profitability
print(cluster_summary_profitability)

# Menentukan kategori profitability berdasarkan nilai rata-rata profitability dalam setiap subset cluster Log Market Cap
cluster_summary_profitability <- cluster_summary_profitability %>%
  group_by(LogMarketCapCluster.x) %>%
  mutate(
    Profitability_Category = case_when(
      Mean_Profitability == min(Mean_Profitability) ~ "Small",
      Mean_Profitability == max(Mean_Profitability) ~ "Big",
      TRUE ~ "Medium"
    )
  )

# Menggabungkan kategori profitability ke data utama
data <- data %>%
  left_join(cluster_summary_profitability %>% select(FinalCluster, Profitability_Category), by = "FinalCluster")

# Membuat variabel baru berdasarkan kategori
data <- data %>%
  mutate(
    Cluster_Label_Profitability = case_when(
      LogMarketCap_Category == "Small" & Profitability_Category == "Small" ~ "SS",
      LogMarketCap_Category == "Small" & Profitability_Category == "Medium" ~ "SM",
      LogMarketCap_Category == "Small" & Profitability_Category == "Big" ~ "SB",
      LogMarketCap_Category == "Big" & Profitability_Category == "Small" ~ "BS",
      LogMarketCap_Category == "Big" & Profitability_Category == "Medium" ~ "BM",
      LogMarketCap_Category == "Big" & Profitability_Category == "Big" ~ "BB"
    )
  )

# Menghitung rata-rata return untuk masing-masing klaster profitability
average_return_profitability <- data %>%
  group_by(Cluster_Label_Profitability) %>%
  summarise(Mean_Return = mean(Return, na.rm = TRUE))

# Menghitung Robust Minus Weak (RMW) sebagai (Big profitability) - (Small profitability)
RMW_cluster <- average_return_profitability %>%
  filter(Cluster_Label_Profitability %in% c("SB", "BB", "SS", "BS")) %>%
  summarise(
    RMW_cluster = mean(Mean_Return[Cluster_Label_Profitability %in% c("SB", "BB")]) - 
      mean(Mean_Return[Cluster_Label_Profitability %in% c("SS", "BS")])
  )

# Menampilkan hasil RMW
print(RMW_cluster)

###############################CMA################################v
# Menggunakan k-means clustering untuk growth
k_growth <- 3  # Jumlah klaster yang diinginkan
data$growth <- as.numeric(data$growth)

# Fungsi untuk melakukan clustering pada growth berdasarkan klaster Log Market Cap
cluster_growth <- function(sub_data, k_growth) {
  set.seed(1)  # Menetapkan seed di sini juga untuk konsistensi
  km_growth <- kmeans(sub_data$growth, centers = k_growth)
  return(as.factor(km_growth$cluster))
}

# Inisialisasi kolom untuk klaster growth
data$Growth_Cluster <- NA

# Melakukan clustering growth berdasarkan setiap klaster Log Market Cap (LogMarketCapCluster.y)
for (cluster in unique(data$LogMarketCapCluster.y)) {
  sub_data <- data %>% filter(LogMarketCapCluster.y == cluster)
  growth_clusters <- cluster_growth(sub_data, k_growth)
  data$Growth_Cluster[data$LogMarketCapCluster.y == cluster] <- growth_clusters
}

# Menggabungkan informasi klaster Log Market Cap dan growth menjadi satu kolom
data <- data %>%
  mutate(FinalCluster_Growth = paste(LogMarketCapCluster.y, Growth_Cluster, sep = "-"))

# Menghitung Mean Log Market Cap, Growth, dan Sum Return untuk masing-masing klaster
cluster_summary_growth <- data %>%
  group_by(LogMarketCapCluster.y, Growth_Cluster, FinalCluster_Growth) %>%
  summarise(
    Count = n(), 
    Sum_Return = sum(Return, na.rm = TRUE), 
    Mean_Log_MarketCap = mean(Log_Market_Cap, na.rm = TRUE), 
    Mean_Growth = mean(growth, na.rm = TRUE)
  )
# Menentukan kategori growth berdasarkan nilai rata-rata growth dalam setiap subset cluster Log Market Cap
cluster_summary_growth <- cluster_summary_growth %>%
  group_by(LogMarketCapCluster.y) %>%
  mutate(
    Growth_Category = case_when(
      Mean_Growth == min(Mean_Growth) ~ "Conservative",
      Mean_Growth == max(Mean_Growth) ~ "Aggressive",
      TRUE ~ "Medium"
    )
  )

# Menggabungkan kategori growth ke data utama
data <- data %>%
  left_join(cluster_summary_growth %>% select(FinalCluster_Growth, Growth_Category), by = "FinalCluster_Growth")

# Membuat variabel baru berdasarkan kategori
data <- data %>%
  mutate(
    Cluster_Label_Growth = case_when(
      LogMarketCap_Category == "Small" & Growth_Category == "Conservative" ~ "SC",
      LogMarketCap_Category == "Small" & Growth_Category == "Medium" ~ "SM",
      LogMarketCap_Category == "Small" & Growth_Category == "Aggressive" ~ "SA",
      LogMarketCap_Category == "Big" & Growth_Category == "Conservative" ~ "BC",
      LogMarketCap_Category == "Big" & Growth_Category == "Medium" ~ "BM",
      LogMarketCap_Category == "Big" & Growth_Category == "Aggressive" ~ "BA"
    )
  )

# Menghitung rata-rata return untuk masing-masing klaster growth
average_return_growth <- data %>%
  group_by(Cluster_Label_Growth) %>%
  summarise(Mean_Return = mean(Return, na.rm = TRUE))

# Menghitung Conservative Minus Aggressive (CMA) sebagai (Conservative growth) - (Aggressive growth)
CMA_cluster <- average_return_growth %>%
  filter(Cluster_Label_Growth %in% c("SC", "BC", "SA", "BA")) %>%
  summarise(
    CMA_cluster = mean(Mean_Return[Cluster_Label_Growth %in% c("SC", "BC")]) - 
      mean(Mean_Return[Cluster_Label_Growth %in% c("SA", "BA")])
  )

# Menampilkan hasil CMA Cluster saja
print(CMA_cluster)

############MOMENTUM##############
# Perform clustering on average_log_return_12m
set.seed(1)
k_momentum <- 2  # Number of clusters
km_momentum <- kmeans(data$average_return_12m , centers = k_momentum)

# Add the clustering result to the data
data$Momentum_Cluster <- as.factor(km_momentum$cluster)

# Determine Winner and Loser clusters
momentum_clusters <- data %>%
  group_by(Momentum_Cluster) %>%
  summarise(Mean_Avg_Return_12m = mean(average_return_12m , na.rm = TRUE)) %>%
  arrange(Mean_Avg_Return_12m)

# Assign labels to clusters
momentum_clusters <- momentum_clusters %>%
  mutate(Momentum_Label = ifelse(row_number() == 1, "Loser", "Winner"))

# Join the cluster labels back to the main data
data <- data %>%
  left_join(momentum_clusters %>% select(Momentum_Cluster, Momentum_Label), by = "Momentum_Cluster")

# Calculate average returns for the Winner and Loser clusters
average_return_mom <- data %>%
  group_by(Momentum_Label) %>%
  summarise(Average_Return = mean(Return, na.rm = TRUE))

# Calculate Momentum (MoM) as (Winner - Loser)
MoM_Cluster <- average_return_mom %>%
  summarise(MoM = Average_Return[Momentum_Label == "Winner"] - 
              Average_Return[Momentum_Label == "Loser"])

# Return only the MoM_Cluster result
print(MoM_Cluster)

################################################################
#cara tradisional
set.seed(1)

head(data1)  # Untuk melihat beberapa baris pertama dari data

# Menambahkan kolom log dari market cap
data1 <- data1 %>%
  mutate(Log_Market_Cap = log(`AVERAGE MARKET CAP`))

# 1. Mengurutkan Log Market Cap dari terkecil ke terbesar
data1 <- data1 %>% arrange(Log_Market_Cap)

# 2. Membagi data menjadi dua kelompok berdasarkan Log Market Cap
n <- nrow(data1)
half_n <- ceiling(n / 2)

# Menggunakan mutate untuk menerapkan row_number()
data1 <- data1 %>%
  mutate(MarketCapCategory = ifelse(row_number() <= half_n, "Small", "Big"))

# 3. Mengurutkan BE/ME dalam setiap kelompok Log Market Cap dan membagi menjadi tiga kelompok
data1 <- data1 %>%
  group_by(MarketCapCategory) %>%
  arrange(`BE/ME`) %>%
  mutate(
    BE_ME_Category = case_when(
      row_number() <= n() * 0.3 ~ "Small",
      row_number() > n() * 0.3 & row_number() <= n() * 0.7 ~ "Medium",
      TRUE ~ "Big"
    )
  ) %>%
  ungroup()

# 4. Membuat variabel baru berdasarkan kategori
data1 <- data1 %>%
  mutate(
    Cluster_Label = case_when(
      MarketCapCategory == "Small" & BE_ME_Category == "Small" ~ "SS1",
      MarketCapCategory == "Small" & BE_ME_Category == "Medium" ~ "SM1",
      MarketCapCategory == "Small" & BE_ME_Category == "Big" ~ "SB1",
      MarketCapCategory == "Big" & BE_ME_Category == "Small" ~ "BS1",
      MarketCapCategory == "Big" & BE_ME_Category == "Medium" ~ "BM1",
      MarketCapCategory == "Big" & BE_ME_Category == "Big" ~ "BB1"
    )
  )

print(head(data1))

# 5. Menghitung rata-rata return untuk masing-masing klaster
average_return <- data1 %>%
  group_by(Cluster_Label) %>%
  summarise(Mean_Return = mean(Return, na.rm = TRUE))

print(average_return)

# 6. Menghitung SMB dan HML
SMB_tradi <- average_return %>%
  summarise(SMB_tradi = mean(Mean_Return[Cluster_Label %in% c("SS1", "SM1", "SB1")]) - 
              mean(Mean_Return[Cluster_Label %in% c("BS1", "BM1", "BB1")]))

print(SMB_tradi)

HML_tradi <- average_return %>%
  summarise(HML_tradi = mean(Mean_Return[Cluster_Label %in% c("SB1", "BB1")]) - 
              mean(Mean_Return[Cluster_Label %in% c("SS1", "BS1")]))

print(HML_tradi)


#############################RMW TRADI###########################
# Mengurutkan Profitability dalam setiap kelompok Log Market Cap dan membagi menjadi tiga kelompok untuk RMW
data1 <- data1 %>%
  group_by(MarketCapCategory) %>%
  arrange(profitability) %>%
  mutate(
    ProfitabilityCategory = case_when(
      row_number() <= n() * 0.3 ~ "Small",
      row_number() > n() * 0.3 & row_number() <= n() * 0.7 ~ "Medium",
      TRUE ~ "Big"
    )
  ) %>%
  ungroup()

# Membuat variabel baru berdasarkan kategori untuk RMW
data1 <- data1 %>%
  mutate(
    Cluster_Label_RMW = case_when(
      MarketCapCategory == "Small" & ProfitabilityCategory == "Small" ~ "SS2",
      MarketCapCategory == "Small" & ProfitabilityCategory == "Medium" ~ "SM2",
      MarketCapCategory == "Small" & ProfitabilityCategory == "Big" ~ "SB2",
      MarketCapCategory == "Big" & ProfitabilityCategory == "Small" ~ "BS2",
      MarketCapCategory == "Big" & ProfitabilityCategory == "Medium" ~ "BM2",
      MarketCapCategory == "Big" & ProfitabilityCategory == "Big" ~ "BB2"
    )
  )

# Menghitung rata-rata return untuk masing-masing klaster RMW
average_return_rmw <- data1 %>%
  group_by(Cluster_Label_RMW) %>%
  summarise(Mean_Return = mean(Return, na.rm = TRUE))

print(average_return_rmw)

# Menghitung RMW (Big Profitability - Small Profitability)
RMW_tradi <- average_return_rmw %>%
  summarise(RMW_tradi = mean(Mean_Return[Cluster_Label_RMW %in% c("SB2", "BB2")]) - 
              mean(Mean_Return[Cluster_Label_RMW %in% c("SS2", "BS2")]))

print(RMW_tradi)



######################CMA##########################
# Mengurutkan Growth dalam setiap kelompok Log Market Cap dan membagi menjadi tiga kelompok untuk CMA
data1 <- data1 %>%
  group_by(MarketCapCategory) %>%
  arrange(growth) %>%
  mutate(
    GrowthCategory = case_when(
      row_number() <= n() * 0.3 ~ "Small",
      row_number() > n() * 0.3 & row_number() <= n() * 0.7 ~ "Medium",
      TRUE ~ "Big"
    )
  ) %>%
  ungroup()

# Membuat variabel baru berdasarkan kategori untuk CMA
data1 <- data1 %>%
  mutate(
    Cluster_Label_CMA = case_when(
      MarketCapCategory == "Small" & GrowthCategory == "Small" ~ "SS3",
      MarketCapCategory == "Small" & GrowthCategory == "Medium" ~ "SM3",
      MarketCapCategory == "Small" & GrowthCategory == "Big" ~ "SB3",
      MarketCapCategory == "Big" & GrowthCategory == "Small" ~ "BS3",
      MarketCapCategory == "Big" & GrowthCategory == "Medium" ~ "BM3",
      MarketCapCategory == "Big" & GrowthCategory == "Big" ~ "BB3"
    )
  )

# Menghitung rata-rata return untuk CMA
average_return_cma <- data1 %>%
  group_by(Cluster_Label_CMA) %>%
  summarise(Mean_Return = mean(Return, na.rm = TRUE))

print(average_return_cma)

# Menghitung CMA (Big Growth - Small Growth) dalam masing-masing kelompok Log Market Cap
CMA_tradi <- average_return_cma %>%
  summarise(CMA_tradi = mean(Mean_Return[Cluster_Label_CMA %in% c("SB3", "BB3")]) - 
              mean(Mean_Return[Cluster_Label_CMA %in% c("SS3", "BS3")]))

print(CMA_tradi)


# Mengurutkan Return dan membagi menjadi dua kelompok untuk MoM
# Mengurutkan data berdasarkan average_log_return_12m
data1 <- data1 %>%
  arrange(average_return_12m)

# Membagi perusahaan menjadi dua kelompok berdasarkan momentum
data1 <- data1 %>%
  mutate(
    MoMCategory = case_when(
      row_number() <= n() * 0.5 ~ "Loser",
      TRUE ~ "Winner"
    )
  )

# Menghitung rata-rata return untuk masing-masing kelompok
average_return_mom <- data1 %>%
  group_by(MoMCategory) %>%
  summarise(Mean_Return = mean(Return, na.rm = TRUE))

# Menghitung Momentum (MoM) sebagai (Winner - Loser)
MoM_tradi <- average_return_mom %>%
  summarise(MoM_tradi = Mean_Return[MoMCategory == "Winner"] - 
              Mean_Return[MoMCategory == "Loser"])

# Output hasil
print(MoM_tradi)

# Keseluruhan hasil untuk SMB, HML, RMW, CMA, MoM
results <- list(
  SMB = SMB_tradi,
  HML = HML_tradi,
  RMW = RMW_tradi,
  CMA = CMA_tradi,
  MoM = MoM_tradi
)
############################RMW###########################

# Output hasil
print(SMB_cluster)
print(HML_cluster)
print(RMW_cluster)
print(CMA_cluster)
print(MoM_Cluster)

########### MENGHITUNG RETURN###################
# Mengelompokkan data berdasarkan ESG dan menghitung rata-rata return
equal_weight <- data %>%
  group_by(ESG) %>%
  summarise(average_return = mean(Return, na.rm = TRUE))

# Menampilkan hasil
print(equal_weight )


# Langkah 1: Bersihkan karakter non-numerik dari kolom 'aset'
data$aset <- gsub(",", "", data$aset)  # Menghapus tanda koma jika ada
data$aset <- as.numeric(data$aset)     # Konversi ke numeric

# Cek apakah ada nilai NA yang muncul setelah konversi
if (any(is.na(data$aset))) {
  warning("Ada nilai aset yang tidak dapat dikonversi ke numeric, mohon periksa dataset Anda.")
}
# Langkah 2: Menghitung total aset untuk setiap kelompok ESG
data <- data %>%
  group_by(ESG) %>%
  mutate(total_asset_by_esg = sum(aset, na.rm = TRUE))

# Langkah 3: Menghitung bobot aset setiap perusahaan berdasarkan kelompok ESG-nya
data <- data %>%
  mutate(asset_weight = aset / total_asset_by_esg)

# Langkah 4: Menghitung return tertimbang (value-weighted return) untuk setiap perusahaan
data <- data %>%
  mutate(weighted_return = asset_weight * Return)

# Langkah 5: Menghitung rata-rata tertimbang (value-weighted) return untuk setiap kelompok ESG
value_weighted_return_by_esg <- data %>%
  group_by(ESG) %>%
  summarise(value_weighted_average_return = sum(weighted_return, na.rm = TRUE))

# Menampilkan hasil rata-rata tertimbang berdasarkan ESG
print(value_weighted_return_by_esg)


# Membuat data frame untuk merangkum hasil faktor
Factor_Summary <- data.frame(
  Method = c("Cluster", "Traditional"),
  SMB = c(SMB_cluster$SMB_cluster, SMB_tradi$SMB_tradi),
  HML = c(HML_cluster$HML_cluster, HML_tradi$HML_tradi),
  RMW = c(RMW_cluster$RMW_cluster, RMW_tradi$RMW_tradi),
  CMA = c(CMA_cluster$CMA_cluster, CMA_tradi$CMA_tradi),
  MoM = c(MoM_Cluster$MoM, MoM_tradi$MoM_tradi)
)

# Menggabungkan kedua hasil value-weighted dan equal-weighted berdasarkan ESG
summary_by_esg <- value_weighted_return_by_esg %>%
  left_join(equal_weight, by = "ESG") %>%
  rename(
    Value_Weighted_Return = value_weighted_average_return,  # Mengubah nama kolom hasil value-weighted
    Equal_Weighted_Return = average_return  # Mengubah nama kolom hasil equal-weighted
  )

# Menampilkan hasil ringkasan penggabungan value-weighted dan equal-weighted return
print(summary_by_esg)

# Menampilkan tabel ringkasan faktor
print(Factor_Summary)

# Menampilkan daftar emiten yang dihapus karena memiliki NA
print(emiten_na$`Emiten`)
# Menampilkan daftar perusahaan
print(perusahaan_inf)
# Menampilkan hasil
print(perusahaan_teks_na)

