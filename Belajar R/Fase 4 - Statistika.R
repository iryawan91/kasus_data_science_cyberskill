library(tidyverse)

# ==========================================
# 1. STATISTIK DESKRIPTIF
# ==========================================
summary(mpg$hwy) # Ringkasan cepat (Min, Max, Mean, Median)

sd_hwy <- sd(mpg$hwy) # Standar Deviasi
korelasi <- cor(mpg$displ, mpg$hwy) # Hubungan mesin & efisiensi

print(paste("Korelasi Mesin vs Efisiensi:", round(korelasi, 2)))


# ==========================================
# 2. UJI HIPOTESIS (T-Test)
# Apakah ada perbedaan efisiensi (hwy) antara tahun 1999 dan 2008?
# ==========================================
uji_t <- t.test(hwy ~ year, data = mpg)
print(uji_t)


# ==========================================
# 3. ANOVA
# Apakah ada perbedaan efisiensi berdasarkan jenis penggerak (drv: f, r, 4)?
# ==========================================
hasil_anova <- aov(hwy ~ drv, data = mpg)
summary(hasil_anova)


# ==========================================
# 4. REGRESI LINEAR (Prediksi)
# Memprediksi 'hwy' berdasarkan 'displ' (kapasitas mesin)
# ==========================================
model_regresi <- lm(hwy ~ displ, data = mpg)

# Melihat detail statistik dari model
summary(model_regresi)

