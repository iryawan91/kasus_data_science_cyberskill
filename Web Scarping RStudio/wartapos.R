# Mengaktifkan library untuk scraping (rvest), olah XML (xml2), manipulasi teks (stringr), dan ekspor Excel (openxlsx)
library(rvest)
library(xml2)
library(stringr)
library(openxlsx)

# ============================================================
# 1. SCRAPING DAFTAR BERITA (HALAMAN KATEGORI)
# ============================================================

# Menentukan URL target untuk kategori 'Politik'
url <- "https://wartapos.co/category/internasional/"
# Membaca seluruh kode HTML dari halaman kategori tersebut
laman <- read_html(url)

# Mengambil seluruh kontainer artikel menggunakan tag <article>
# Strategi ini penting agar data Judul, Tgl, dan Link tetap berada dalam satu baris yang sama (sinkron)
posts <- laman %>% html_nodes('article') 

# Mengambil Judul: Mencari tag <a> yang memiliki atribut rel="bookmark"
judul <- posts %>% 
  html_node('a[rel="bookmark"]') %>% 
  html_text(trim = TRUE)

# Mengambil Tanggal: Mencari tag <time> yang memiliki class khusus 'entry-date published'
tgl <- posts %>% 
  html_node('time.entry-date.published') %>% 
  html_text(trim = TRUE)

# Mengambil Kategori: Mencari tag <a> yang judul atributnya dimulai dengan (^=) kata tertentu
kategori <- posts %>% 
  html_node('a[title^="Lihat semua posts di"]') %>% 
  html_text(trim = TRUE)

# Mengambil Link: Mengambil alamat URL (href) dari judul berita untuk diproses di tahap berikutnya
link_judul <- posts %>% 
  html_node('a[rel="bookmark"]') %>% 
  html_attr("href")

# ============================================================
# 2. FUNGSI UNTUK MENGAMBIL ISI BERITA DARI TIAP LINK
# ============================================================

# Membuat fungsi kustom 'dptisi' dengan input 'x' (yaitu alamat URL)
dptisi <- function(x) {
  # Proteksi: Jika link ternyata kosong/NA, kembalikan nilai NA agar program tidak error
  if (is.na(x)) return(NA)
  
  # tryCatch digunakan untuk menangani error jika salah satu link tidak bisa dibuka
  tryCatch({
    # Membaca HTML dari masing-masing link berita
    laman_berita <- read_html(x)
    
    # Mengambil teks berita yang biasanya berada di dalam tag <p> (paragraf) 
    # di dalam kontainer dengan class '.entry-content'
    isi <- laman_berita %>% 
      html_nodes('.entry-content p') %>% 
      html_text(trim = TRUE) %>% 
      # paste(collapse = " ") menggabungkan banyak paragraf terpisah menjadi satu teks utuh
      paste(collapse = " ") 
    
    return(isi)
  }, error = function(e) { return(NA) }) # Jika proses ambil isi gagal, beri nilai NA
}

# ============================================================
# 3. PROSES PENGAMBILAN ISI (ITERASI)
# ============================================================

# sapply akan menjalankan fungsi 'dptisi' secara otomatis ke seluruh daftar 'link_judul'
# Ini adalah proses otomatisasi: "Buka link 1 -> Ambil isi, Buka link 2 -> Ambil isi, dst"
isiberita <- sapply(link_judul, FUN = dptisi, USE.NAMES = FALSE)

# ============================================================
# 4. PENGGABUNGAN KE DATAFRAME & VISUALISASI TABEL
# ============================================================

# Menyatukan semua hasil ekstraksi ke dalam satu tabel besar (DataFrame)
wartapos_final <- data.frame(
  Judul = judul, 
  Tanggal = tgl, 
  Kategori = kategori, 
  Isi_Berita = isiberita, 
  URL = link_judul,
  stringsAsFactors = FALSE
)

# Membuka tabel hasil akhir di jendela Viewer RStudio
View(wartapos_final)

# (Opsional) Mengaktifkan perintah ini jika ingin menyimpan ke file Excel asli
write.xlsx(wartapos_final, "Berita_Wartapos_Lengkap.xlsx")