# ==============================================================================
# 1. PENGATURAN DIREKTORI & INPUT DATA
# ==============================================================================

# Menentukan folder kerja (working directory)
setwd("D:/GWR")

# --- Loading Libraries ---
# Kelompok Analisis Statistik Dasar
library(nortest)  # Untuk uji normalitas (misal: Anderson-Darling, Lilliefors)
library(car)      # Untuk uji Multikolinearitas (VIF)
library(lmtest)   # Untuk uji Heteroskedastisitas (Breusch-Pagan)

# Kelompok Analisis Spasial & GWR
library(sf)       # Library utama pengolah data spasial (modern)
library(sp)       # Format spasial lama (dibutuhkan oleh GWmodel)
library(spdep)    # Analisis keterkaitan spasial (Moran's I)
library(GWmodel)  # Library inti untuk pemodelan GWR

# Kelompok Visualisasi
library(ggplot2)  # Pembuatan peta dan grafik profesional

# --- Input Data Statistik ---
# Membaca file data format CSV
# Pastikan file "Data APS Sulsel.csv" berada di folder D:/GWR
dataku <- read.csv("Data APS Sulsel.csv", header = TRUE, sep = ",")

# --- Pengecekan Awal Data ---
# Melihat 6 baris pertama data untuk memastikan data terbaca dengan benar
head(dataku)

# Melihat struktur data (memastikan mana yang angka/numeric)
str(dataku)

# ==============================================================================
# 2. PEMODELAN REGRESI GLOBAL (OLS) & UJI ASUMSI
# ==============================================================================

# --- Membangun Model Regresi OLS ---
# Y = PutusSekolah (Variabel Dependen)
# X = RataLamaSekolah, PresentasePenduduk, PresentaseAksesGadget, PendudukMiskin
pers <- lm(PutusSekolah ~ RataLamaSekolah + PresentasePenduduk + 
             PresentaseAksesGadget + PendudukMiskin, data = dataku)

# Menampilkan hasil ringkasan model (R-Square dan Signifikansi Global)
summary(pers)

# --- 2.1 Uji Normalitas (Lilliefors) ---
# Mengambil nilai residual dari model
residual <- residuals(pers)

# Uji Lilliefors (Bagian dari nortest)
# H0: Residual berdistribusi normal
# Interpretasi: Jika p-value > 0.05, maka Asumsi Normalitas Terpenuhi.
lillie.test(residual)

# --- 2.2 Uji Multikolinearitas (VIF) ---
# Mengukur apakah ada hubungan linear yang terlalu kuat antar variabel independen (X)
# Interpretasi: Jika Nilai VIF < 10, berarti tidak ada Multikolinearitas (Aman).
vif(pers)

# --- 2.3 Uji Heteroskedastisitas Spasial (Breusch-Pagan) ---
# Ini adalah uji terpenting untuk lanjut ke GWR!
# H0: Varians residual homogen (sama antar wilayah)
# Interpretasi: Jika p-value < 0.05, maka terjadi HETEROGENITAS SPASIAL.
# Artinya: Pengaruh variabel X terhadap Y berbeda-beda di setiap lokasi (Syarat GWR).
bptest(pers, studentize = FALSE)

# --- 2.4 Uji Autokorelasi (Durbin-Watson) - Tambahan ---
# Untuk memastikan tidak ada korelasi antar baris data
# Interpretasi: Jika p-value > 0.05, tidak ada autokorelasi (Aman).
dwtest(pers)

# ============================================================
# 3. PERSIAPAN DATA SPASIAL
# ============================================================

# 1. Membaca file peta (Shapefile)
# dsn adalah nama folder, layer adalah nama file .shp (tanpa ekstensi)
peta_sf <- st_read(dsn = "Peta sulsel", layer = "sul-sel")

# --- PERBAIKAN TOPOLOGI ---
# Mengatasi error "Loop is not valid" atau vertex ganda yang sering muncul pada SHP lokal
peta_sf <- st_make_valid(peta_sf)

# 2. Menetapkan Sistem Koordinat (CRS)
# Wajib dilakukan agar R tahu satuan koordinatnya (4326 = WGS84 / Derajat Desimal)
st_crs(peta_sf) <- 4326

# 3. Mengambil Titik Pusat (Centroid) untuk Koordinat GWR
# Fungsi st_centroid mencari titik tengah kabupaten untuk perhitungan jarak antar wilayah
# kord akan berisi kolom X (Bujur) dan Y (Lintang)
kord <- st_coordinates(st_centroid(peta_sf))
kord_df <- as.data.frame(kord)

# 4. Menggabungkan Data Statistik dengan Koordinat
# Kita gunakan cbind untuk menyatukan tabel dataku dengan kord_df
# Lalu diubah menjadi objek spasial 'sf'
dataku_sf <- st_as_sf(cbind(dataku, kord_df), coords = c("X", "Y"), crs = 4326)

# 5. Verifikasi Akhir
# Pastikan jumlah baris di peta_sf sama dengan dataku_sf
print(nrow(peta_sf))
print(nrow(dataku_sf))

# ============================================================
# 4. PENENTUAN BANDWIDTH OPTIMUM
# ============================================================

# Konversi ke format Spatial satu kali saja untuk efisiensi
data_sp <- as(dataku_sf, "Spatial")

# Mencari Bandwidth menggunakan fungsi bw.gwr
# Fixed Bandwidth (adaptive = F) mencari jarak radius tetap dalam satuan koordinat
bw.fg <- bw.gwr(
  formula = PutusSekolah ~ RataLamaSekolah + PresentasePenduduk + 
    PresentaseAksesGadget + PendudukMiskin, 
  data = data_sp, 
  approach = "CV",       # Metode Cross-Validation
  kernel = "gaussian",   # Fungsi pembobot Gaussian
  adaptive = FALSE       # FALSE = Fixed Bandwidth
)

# Menampilkan nilai bandwidth optimum
print(paste("Bandwidth Optimum ditemukan pada jarak:", bw.fg))

# ============================================================
# 5. PEMODELAN GWR & EKSTRAKSI HASIL
# ============================================================

# Menjalankan model GWR Dasar
fit1 <- gwr.basic(
  formula = PutusSekolah ~ RataLamaSekolah + PresentasePenduduk + 
    PresentaseAksesGadget + PendudukMiskin, 
  data = data_sp, 
  bw = bw.fg, 
  kernel = "gaussian", 
  adaptive = FALSE
)

# Menampilkan ringkasan model (Cek R-square & AICc di sini)
print(fit1)

# Ekstraksi Koefisien Lokal (SDF) dan P-Value (Adjusted)
h1 <- data.frame(fit1$SDF)
h2 <- data.frame(gwr.t.adjust(fit1)$result$p)

# Mengganti nama kolom p-value agar lebih jelas (Contoh: RataLamaSekolah_p)
colnames(h2) <- paste0(colnames(h2), "_p")

# ============================================================
# 6. PENGGABUNGAN & EKSPOR DATA
# ============================================================

# Menggabungkan data asli, koefisien, dan p-value
# Menambahkan nama Kabupaten dari data awal agar mudah dibaca di Excel
output_final <- cbind(Kabupaten = dataku$Kabupaten, h1, h2)

# Simpan ke CSV (Menggunakan write.csv lebih stabil untuk Excel)
write.csv(output_final, "Hasil_Analisis_GWR_Lengkap.csv", row.names = FALSE)

# Pesan konfirmasi
message("Proses GWR selesai. File 'Hasil_Analisis_GWR_Lengkap.csv' telah disimpan.")

# ==============================================================================
# 7. PEMBUATAN LABEL SIGNIFIKANSI (KATEGORISASI P-VALUE)
# ==============================================================================

# Kita membuat kolom baru dengan awalan 'ket.' untuk memudahkan identifikasi saat plotting
# Menggunakan ambang batas (alpha) 0.05 atau 5%

# 7.1 Kategori Signifikansi Rata-rata Lama Sekolah
h2$ket.RLS <- as.factor(ifelse(h2$RataLamaSekolah_p < 0.05, 
                               "Berpengaruh", "Tidak Berpengaruh"))

# 7.2 Kategori Signifikansi Presentase Penduduk
h2$ket.PP <- as.factor(ifelse(h2$PresentasePenduduk_p < 0.05, 
                              "Berpengaruh", "Tidak Berpengaruh"))

# 7.3 Kategori Signifikansi Presentase Akses Gadget
h2$ket.RMTG <- as.factor(ifelse(h2$PresentaseAksesGadget_p < 0.05, 
                                "Berpengaruh", "Tidak Berpengaruh"))

# 7.4 Kategori Signifikansi Penduduk Miskin
h2$ket.JPM <- as.factor(ifelse(h2$PendudukMiskin_p < 0.05, 
                               "Berpengaruh", "Tidak Berpengaruh"))

# Memeriksa ringkasan jumlah kabupaten yang berpengaruh vs tidak berpengaruh
summary(h2[, c("ket.RLS", "ket.PP", "ket.RMTG", "ket.JPM")])

# ==============================================================================
# 8. VISUALISASI SIGNIFIKANSI SPASIAL (MODERN MAPPING)
# ==============================================================================

# 1. Menggabungkan label signifikansi (h2) ke objek peta (peta_sf)
peta_hasil <- cbind(peta_sf, h2)

# --- 8.1 Peta Rata-rata Lama Sekolah (RLS) ---
p1 <- ggplot(data = peta_hasil) +
  geom_sf(aes(fill = ket.RLS)) +
  geom_sf_text(aes(label = Kabupaten), size = 2, color = "black") +
  scale_fill_manual(values = c("Berpengaruh" = "lightblue", 
                               "Tidak Berpengaruh" = "red")) +
  labs(title = "Signifikansi RLS terhadap Angka Putus Sekolah",
       subtitle = "Provinsi Sulawesi Selatan",
       fill = "Keterangan") +
  theme_bw()
print(p1)

# --- 8.2 Peta Persentase Penduduk (PP) ---
p2 <- ggplot(data = peta_hasil) +
  geom_sf(aes(fill = ket.PP)) +
  geom_sf_text(aes(label = Kabupaten), size = 2, color = "black") +
  scale_fill_manual(values = c("Berpengaruh" = "yellow", 
                               "Tidak Berpengaruh" = "green")) +
  labs(title = "Signifikansi Persentase Penduduk terhadap Angka Putus Sekolah",
       subtitle = "Provinsi Sulawesi Selatan",
       fill = "Keterangan") +
  theme_bw()
print(p2)

# --- 8.3 Peta Akses Gadget (RMTG) ---
p3 <- ggplot(data = peta_hasil) +
  geom_sf(aes(fill = ket.RMTG)) + 
  geom_sf_text(aes(label = Kabupaten), size = 2, color = "black") +
  scale_fill_manual(values = c("Berpengaruh" = "blue", 
                               "Tidak Berpengaruh" = "brown")) +
  labs(title = "Signifikansi Akses Gadget terhadap Angka Putus Sekolah",
       subtitle = "Provinsi Sulawesi Selatan",
       fill = "Keterangan") +
  theme_bw()
print(p3)

# --- 8.4 Peta Penduduk Miskin (JPM) ---
p4 <- ggplot(data = peta_hasil) +
  geom_sf(aes(fill = ket.JPM)) + 
  geom_sf_text(aes(label = Kabupaten), size = 2, color = "black") +
  scale_fill_manual(values = c("Berpengaruh" = "gold", 
                               "Tidak Berpengaruh" = "grey")) +
  labs(title = "Signifikansi Jumlah Penduduk Miskin terhadap Angka Putus Sekolah",
       subtitle = "Provinsi Sulawesi Selatan",
       fill = "Keterangan") +
  theme_bw()
print(p4)
