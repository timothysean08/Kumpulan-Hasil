
library(lmtest)

# Fungsi untuk menghitung AIC dan BIC untuk berbagai jumlah lag
aic_bic_lag <- function(model, max_lag) {
  aic_values <- numeric(max_lag)
  bic_values <- numeric(max_lag)
  
  for (p in 1:max_lag) {
    bg_test_result <- bgtest(model, order = p)
    aic_values[p] <- AIC(bg_test_result)
    bic_values[p] <- BIC(bg_test_result)
  }
  
  list(AIC = aic_values, BIC = bic_values)
}

# Contoh data
dataset <- data.frame(y = c(1, 2, 3, 4, 5), 
                      x1 = c(1, 0, 2, 1, 3), 
                      x2 = c(2, 1, 4, 3, 5))

# Regresi awal
model <- lm(y ~ x1 + x2, data = dataset)

# Hitung AIC dan BIC untuk lag 1 hingga 3
results <- aic_bic_lag(model, max_lag = 3)
print(results)

# Misalnya lag optimal adalah 2
optimal_lag <- 2

# Uji Breusch-Godfrey dengan lag optimal
bg_test_result <- bgtest(model, order = optimal_lag)
print(bg_test_result)
