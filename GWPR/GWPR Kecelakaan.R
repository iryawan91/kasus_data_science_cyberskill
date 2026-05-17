# 1. SET WORKING DIRECTORY
setwd("D:/GWPR")
library(readxl)   # Untuk import data langsung dari Excel (.xlsx)
library(spdep)    # Untuk analisis autokorelasi & keterkaitan spasial
library(lmtest)   # Untuk uji heteroskedastisitas (Breusch-Pagan Test)
library(plm)      # Untuk pemodelan regresi data panel standar
library(car)      # Untuk uji multikolinearitas (Variance Inflation Factor / VIF)
library(GWmodel)  # Library inti untuk pemodelan spasial terboboti (GWR/GWPR)

# 2.DATA UTAMA
data.GWPR <- read_excel("DATASET GWPR NEW.xlsx")

# Mengubah format tibble Excel menjadi Data Frame standar R agar kompatibel dengan GWmodel
data.GWPR <- as.data.frame(data.GWPR)

# Menampilkan nama-nama kolom untuk memastikan tidak ada typo
colnames(data.GWPR)

# Menampilkan tipe data tiap kolom 
str(data.GWPR)

# Membuka tabel data secara visual di tab baru RStudio
View(data.GWPR)


# 3. TRANSFORMASI SKALA DATA (STANDARDISASI Z-SCORE)

# Membuat salinan data frame baru khusus untuk pemodelan
data.GWPR.scaled <- data.GWPR

# Menentukan daftar variabel independen (X) yang ingin disamakan skalanya
variabel_x <- c("Kepadatan_penduduk", "poporsi_sim_c", "poporsi_sim_a", 
                "lancar", "macet")

# Menerapkan fungsi scale() untuk standardisasi z-score
data.GWPR.scaled[variabel_x] <- scale(data.GWPR[variabel_x])

# Menampilkan ringkasan statistik variabel X setelah di-scale
summary(data.GWPR.scaled[variabel_x])

# 4. PEMODELAN REGRESI PANEL GLOBAL & UJI ASUMSI

# --- 4.1 Membuat Model Regresi Panel Standar (Pooling/OLS biasa) ---
# Ini digunakan khusus untuk mencek nilai VIF awal
model.ols <- lm(Jumlah_Kecelakaan_KM2 ~ Kepadatan_penduduk + poporsi_sim_c + 
                  poporsi_sim_a + lancar + macet, data = data.GWPR.scaled)

# Uji Multikolinearitas (VIF)
# Pastikan nilai VIF untuk semua variabel < 10. Jika ada yang > 10, data harus dievaluasi kembali.
vif(model.ols)


# --- 4.2 Pemodelan Regresi Data Panel (Fixed Effect vs Random Effect) ---

# A. Model Fixed Effect (FE) / Within Model
model.fe <- plm(Jumlah_Kecelakaan_KM2 ~ Kepadatan_penduduk + poporsi_sim_c + 
                  poporsi_sim_a + lancar + macet, 
                data = data.GWPR.scaled, 
                index = c("Kecamatan", "Tahun"), 
                model = "within")
summary(model.fe)

# B. Model Random Effect (RE)
model.re <- plm(Jumlah_Kecelakaan_KM2 ~ Kepadatan_penduduk + poporsi_sim_c + 
                  poporsi_sim_a + lancar + macet, 
                data = data.GWPR.scaled, 
                index = c("Kecamatan", "Tahun"), 
                model = "random")
summary(model.re)


# --- 4.3 Uji Hausman (Penentu Model Terbaik) ---
# H0: Model Random Effect lebih tepat
# H1: Model Fixed Effect lebih tepat
# Interpretasi: Jika p-value < 0.05, maka pilih FIXED EFFECT. Jika > 0.05, pilih RANDOM EFFECT.
phtest(model.fe, model.re)

# 5. PERSIAPAN DATA SPASIAL & TRANSFORMASI PANEL (GWPR)

# --- 5.1 Proses Demeaned Data (Within Transformation) ---
# Menghitung rata-rata antar waktu untuk setiap Kecamatan
rata_rata_kec <- aggregate(cbind(Jumlah_Kecelakaan_KM2, Kepadatan_penduduk, 
                                 poporsi_sim_c, poporsi_sim_a, lancar, macet) ~ Kecamatan, 
                           data = data.GWPR.scaled, FUN = mean)

# Menggabungkan data rata-rata ke tabel utama
data.GWPR.merge <- merge(data.GWPR.scaled, rata_rata_kec, by = "Kecamatan", suffixes = c("", "_mean"))

# Membuat data frame hasil selisih (Fixed Effect)
data.GWPR.fe <- data.frame(
  Kecamatan = data.GWPR.merge$Kecamatan,
  Tahun     = data.GWPR.merge$Tahun,
  long      = data.GWPR.merge$long,
  lat       = data.GWPR.merge$lat,
  Y_fe      = data.GWPR.merge$Jumlah_Kecelakaan_KM2 - data.GWPR.merge$Jumlah_Kecelakaan_KM2_mean,
  X1_fe     = data.GWPR.merge$Kepadatan_penduduk - data.GWPR.merge$Kepadatan_penduduk_mean,
  X2_fe     = data.GWPR.merge$poporsi_sim_c - data.GWPR.merge$poporsi_sim_c_mean,
  X3_fe     = data.GWPR.merge$poporsi_sim_a - data.GWPR.merge$poporsi_sim_a_mean,
  X5_fe     = data.GWPR.merge$macet - data.GWPR.merge$macet_mean
)

# --- 5.2 Konversi ke Objek Spasial Menggunakan Library 'sf' ---
# Membuat objek sf dengan penentuan koordinat dan CRS WGS84 yang aman dari keterbalikan sumbu
data.GWPR.sf <- st_as_sf(data.GWPR.fe, coords = c("long", "lat"), crs = 4326)

# --- 5.3 Konversi Akhir ke Format 'Spatial' (sp) untuk GWmodel ---
# Fungsi dari GWmodel masih membutuhkan format lama 'SpatialPointsDataFrame'
data.GWPR.sp <- as(data.GWPR.sf, "Spatial")


# 6. PENENTUAN BANDWIDTH OPTIMUM & EKSEKUSI GWPR
# ==============================================================================

# 6.1 Mencari Jarak Bandwidth Optimum (Fixed Gaussian Kernel)
bw.gwpr <- bw.gwr(
  formula = Y_fe ~ X1_fe + X2_fe + X3_fe + X5_fe, 
  data = data.GWPR.sp, 
  approach = "CV", 
  kernel = "gaussian", 
  adaptive = FALSE
)
print(paste("Bandwidth Optimum GWPR (Jarak Derajat):", bw.gwpr))

# 6.2 Menjalankan Model Utama GWPR Fixed Effect
model.gwpr <- gwr.basic(
  formula = Y_fe ~ X1_fe + X2_fe + X3_fe + X5_fe, 
  data = data.GWPR.sp, 
  bw = bw.gwpr, 
  kernel = "gaussian", 
  adaptive = FALSE
)
print(model.gwpr)


# ==============================================================================
# 7. EKSTRAKSI HASIL & KATEGORISASI SIGNIFIKANSI LOKAL
# ==============================================================================

# Mengonversi Spatial Data Frame hasil estimasi kembali menjadi data frame standar R
hasil_gwpr_df <- data.frame(model.gwpr$SDF)

# Menghitung Adjusted P-Value Lokal menggunakan fungsi gwr.t.adjust
p_value_lokal <- data.frame(gwr.t.adjust(model.gwpr)$result$p)
colnames(p_value_lokal) <- paste0(colnames(p_value_lokal), "_p")

# Memberikan label kategori signifikansi (Alpha = 0.05)
p_value_lokal$ket.Kepadatan <- as.factor(ifelse(p_value_lokal$X1_fe_p < 0.05, "Berpengaruh", "Tidak Berpengaruh"))
p_value_lokal$ket.SIM_C     <- as.factor(ifelse(p_value_lokal$X2_fe_p < 0.05, "Berpengaruh", "Tidak Berpengaruh"))
p_value_lokal$ket.SIM_A     <- as.factor(ifelse(p_value_lokal$X3_fe_p < 0.05, "Berpengaruh", "Tidak Berpengaruh"))
p_value_lokal$ket.Macet     <- as.factor(ifelse(p_value_lokal$X5_fe_p < 0.05, "Berpengaruh", "Tidak Berpengaruh"))


# 8. PENGGABUNGAN AKHIR & EKSPOR DATA KE CSV

# Karena st_coordinates() mengekstrak kembali posisi asli dari objek sf
koordinat_matriks <- st_coordinates(data.GWPR.sf)

output_gwpr_lengkap <- cbind(
  Kecamatan = data.GWPR.sf$Kecamatan,
  Tahun     = data.GWPR.sf$Tahun,
  Long      = koordinat_matriks[, "X"],
  Lat       = koordinat_matriks[, "Y"],
  hasil_gwpr_df,
  p_value_lokal
)

# Ekspor hasil akhir ke komputer Anda
write.csv(output_gwpr_lengkap, "Hasil_Analisis_GWPR_Jakarta.csv", row.names = FALSE)
message("Proses selesai dengan library sf! File 'Hasil_Analisis_GWPR_Jakarta.csv' siap dianalisis.")
