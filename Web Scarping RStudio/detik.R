# Mengaktifkan library 'rvest' untuk proses web scraping (mengambil data dari HTML)
library(rvest)
# Mengaktifkan library 'xml2' untuk menangani struktur dokumen XML/HTML
library(xml2)
# Mengaktifkan library 'openxlsx' jika ingin menyimpan hasil ke format Excel (.xlsx)
library(openxlsx)

# 1. MENENTUKAN URL TARGET
# Menyimpan alamat website pencarian Detik dengan kata kunci 'bandung' ke dalam variabel 'url'
url <- "https://www.detik.com/search/searchall?query=bandung&result_type=latest&page=2"

# Membaca seluruh struktur kode HTML dari URL tersebut ke dalam variabel 'laman'
laman <- read_html(url)

# 2. AMBIL SELURUH KONTAINER BERITA
# html_nodes('article') mencari semua elemen dengan tag <article>
# Ini adalah "bungkus" besar yang berisi judul, link, dan tanggal untuk satu berita
kontainer <- laman %>% html_nodes('article')

# 3. EKSTRAKSI DATA DARI DALAM KONTAINER
# Kita gunakan html_node (tanpa 's') agar jika data tidak ditemukan, baris tersebut diisi NA (tidak hilang/geser)

# Mengambil teks judul yang berada di dalam tag h3 dengan class 'media__title' dan tag link <a>
judul <- kontainer %>% 
  html_node('h3.media__title a') %>% 
  html_text(trim = TRUE) # trim = TRUE untuk menghapus spasi/enter yang tidak perlu

# Mengambil alamat link (URL) artikel yang ada di dalam atribut 'href' pada tag <a>
linkberita <- kontainer %>% 
  html_node('h3.media__title a') %>% 
  html_attr("href")

# Mengambil kategori berita (misal: detikNews, detikHot) dari class 'media__subtitle'
kategori <- kontainer %>% 
  html_node('h2.media__subtitle') %>% 
  html_text(trim = TRUE)

# Mengambil informasi waktu/tanggal dari atribut 'title' pada class 'media__date span'
tanggal <- kontainer %>% 
  html_node('.media__date span') %>% 
  html_attr("title")

# Mengambil potongan singkat isi berita (deskripsi) dari class 'media__desc'
sekilasisi <- kontainer %>% 
  html_node('.media__desc') %>% 
  html_text(trim = TRUE)

# 4. GABUNGKAN KE DALAM DATA FRAME
# Membuat tabel (DataFrame) agar data tersusun rapi per kolom
df <- data.frame(
  Judul = judul, 
  Kategori = kategori, 
  Tanggal = tanggal, 
  Ringkasan = sekilasisi, 
  Link = linkberita, 
  stringsAsFactors = FALSE # Memastikan teks tetap sebagai karakter, bukan kategori (factor)
)

# 5. MEMBERSIHKAN DATA (DATA CLEANING)
# Kadang iklan juga dibungkus tag <article>, namun tidak punya judul.
# Baris ini menghapus baris di tabel jika kolom 'Judul' bernilai NA.
df <- df[!is.na(df$Judul), ]

# 6. MENAMPILKAN HASIL
# Membuka jendela viewer di RStudio untuk melihat tabel hasil scraping secara interaktif
View(df)