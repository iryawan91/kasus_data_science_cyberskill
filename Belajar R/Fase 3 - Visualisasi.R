library(tidyverse)

# Kita gunakan dataset 'mpg'
data_mobil <- mpg

# 1. SCATTER PLOT (Hubungan: Mesin vs Efisiensi)
# geom_point()
ggplot(data_mobil, aes(x = displ, y = hwy)) +
  geom_point(aes(color = drv), size = 2) +
  labs(title = "1. Scatter Plot: Kapasitas Mesin vs Efisiensi",
       subtitle = "Semakin besar mesin, semakin boros BBM",
       x = "Kapasitas Mesin (L)", y = "Mil per Galon (Hwy)") +
  theme_minimal()

# 2. BAR CHART (Kategori: Jumlah mobil per Jenis)
# geom_bar()
ggplot(data_mobil, aes(x = class, fill = class)) +
  geom_bar() +
  labs(title = "2. Bar Chart: Jumlah Mobil per Kelas",
       x = "Kelas Mobil", y = "Jumlah Unit") +
  theme_light()

# 3. BOXPLOT (Distribusi Kategori: Efisiensi per Jenis Penggerak)
# geom_boxplot()
ggplot(data_mobil, aes(x = drv, y = hwy, fill = drv)) +
  geom_boxplot() +
  labs(title = "3. Boxplot: Sebaran Efisiensi per Jenis Penggerak",
       x = "Penggerak (f=depan, r=belakang, 4=4wd)", y = "Efisiensi (Hwy)") +
  theme_bw()

# 4. HISTOGRAM (Distribusi Angka: Sebaran Efisiensi BBM)
# geom_histogram()
ggplot(data_mobil, aes(x = hwy)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 15) +
  labs(title = "4. Histogram: Sebaran Efisiensi Seluruh Mobil",
       x = "Mil per Galon (Hwy)", y = "Frekuensi") +
  theme_minimal()

# 5. LINE CHART (Tren: Efisiensi rata-rata berdasarkan tahun)
# Karena tahun di 'mpg' hanya ada 1999 dan 2008, kita hitung rata-ratanya dulu
data_mobil %>%
  group_by(year) %>%
  summarise(rata_hwy = mean(hwy)) %>%
  ggplot(aes(x = year, y = rata_hwy)) +
  geom_line(size = 1.2, color = "darkred") +
  geom_point(size = 3) +
  scale_x_continuous(breaks = c(1999, 2008)) +
  labs(title = "5. Line Chart: Tren Efisiensi Rata-rata (1999-2008)",
       x = "Tahun Produksi", y = "Rata-rata Hwy") +
  theme_minimal()

# 6. FACETING (Kombinasi: Scatter Plot dibagi per Tahun)
ggplot(data_mobil, aes(x = displ, y = hwy)) +
  geom_point(color = "darkgreen") +
  facet_wrap(~ year) +
  labs(title = "6. Faceting: Perbandingan Performa 1999 vs 2008",
       x = "Kapasitas Mesin", y = "Efisiensi") +
  theme_gray()

