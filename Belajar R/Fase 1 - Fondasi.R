# ==========================================
# STUDI KASUS: MANAJEMEN KLUB OLAHRAGA
# ==========================================

# 1. VARIABEL & TIPE DATA (Dasar)
nama_klub <- "Garuda Fitness"        # Character
biaya_pendaftaran <- 150000          # Numeric
buka_hari_ini <- TRUE                # Logical

# 2. VECTOR (Satu tipe data)
nama_anggota <- c("Andi", "Budi", "Caca", "Deni")
berat_badan <- c(70, 85, 60, 92)      # dalam kg
tinggi_badan <- c(1.70, 1.75, 1.65, 1.80) # dalam meter

# 3. OPERATOR ARITMATIKA & PERBANDINGAN
# Menghitung BMI (Body Mass Index): Berat / Tinggi^2
bmi <- berat_badan / (tinggi_badan^2)

# Menentukan siapa yang memiliki BMI > 25 (Overweight)
is_overweight <- bmi > 25

# 4. DATA FRAME (Tabel Utama)
# Menggabungkan data anggota ke dalam tabel
tabel_anggota <- data.frame(
  Nama = nama_anggota,
  Berat = berat_badan,
  Tinggi = tinggi_badan,
  BMI = round(bmi, 1),
  Status_Overweight = is_overweight
)

# 5. MATRIX (Data Angka Saja)
# Misal: Data kehadiran 4 anggota selama 3 hari (1 = Hadir, 0 = Absen)
data_hadir <- matrix(
  c(1, 1, 0, 
    1, 0, 1, 
    1, 1, 1, 
    0, 0, 1), 
  nrow = 4, byrow = TRUE
)
colnames(data_hadir) <- c("Senin", "Selasa", "Rabu")

# 6. OPERATOR LOGIKA
# Mencari anggota yang BMI > 25 DAN hadir di hari Rabu
target_khusus <- tabel_anggota$Status_Overweight & (data_hadir[, "Rabu"] == 1)

# 7. LIST (Menyimpan SEMUA informasi di atas)
laporan_klub <- list(
  Info_Dasar = nama_klub,
  Detail_Anggota = tabel_anggota,
  Presensi = data_hadir,
  Target_Promosi = nama_anggota[target_khusus]
)

# ==========================================
# MENAMPILKAN HASIL
# ==========================================

print("--- Data Tabel Anggota ---")
print(tabel_anggota)

print("--- Anggota Overweight yang Hadir Rabu ---")
print(laporan_klub$Target_Promosi)

# Menggunakan Operator Aritmatika untuk diskon
total_pendapatan <- biaya_pendaftaran * nrow(tabel_anggota)
print(paste("Total Estimasi Pendapatan:", total_pendapatan))

