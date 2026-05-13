# ============================================
# MEMANGGIL LIBRARY YANG DIBUTUHKAN
# ============================================

library(rvest) # Library untuk mengambil data dari website (web scraping)
library(RColorBrewer) # Library untuk menyediakan berbagai pilihan warna
library(wordcloud) # Library untuk membuat visualisasi Word Cloud
library(NLP) # Library untuk pemrosesan bahasa alami (Natural Language Processing)
library(tm) # Digunakan untuk membersihkan, mengolah, dan menganalisis teks

# ============================================
# MENGAMBIL DATA HTML
# ============================================

# Membaca file HTML lokal menggunakan fungsi read_html()
webpage <- read_html("Daredevil_ Born Again_ Season 2 _ Reviews _ Rotten Tomatoes.html")

# Mengambil semua elemen <div> yang memiliki atribut slot="review". div[slot="review"]
review_node <- html_nodes(webpage, 'div[slot="review"]')

# Mengambil teks dari setiap review, trim = TRUE digunakan untuk menghapus spasi kosong
review_data <- html_text(review_node, trim = TRUE)

# Membuat data frame dengan 1 kolom bernama review_text
df_review <- data.frame(
  review_text = review_data,
  stringsAsFactors = FALSE
)

# ============================================
# MEMBUAT DATA UNTUK CORPUS
# ============================================

# Package tm membutuhkan format data tertentu:
# - doc_id  : ID unik untuk setiap dokumen
# - text    : isi teks / review

# Membuat data frame baru untuk kebutuhan corpus
main_data.corpus <- data.frame(
  
  # Membuat ID otomatis:
  doc_id = paste0("doc_", 1:nrow(df_review)),
  
  # Mengambil isi review dari dataframe sebelumnya
  text = df_review$review_text,
  
  # Agar teks tidak otomatis menjadi factor
  stringsAsFactors = FALSE
)

# DataframeSource() : Mengubah dataframe menjadi sumber data teks
# VCorpus() : Kumpulan dokumen teks yang disimpan di memori untuk proses text mining
corpus <- VCorpus(
  DataframeSource(main_data.corpus)
)

# Mengubah teks menjadi huruf kecil (tolower)
corpus.processed = tm_map(corpus, content_transformer(tolower))


# Membuat dan menjalankan fungsi penghapus tag HTML
cleanHTMLCode = function(x) {
  return(gsub(pattern = "<.*?>", replacement = "", x))
}
corpus.processed = tm_map(corpus.processed, content_transformer(cleanHTMLCode))


# Menghapus angka
corpus.processed = tm_map(corpus.processed, removeNumbers)

# Menghapus tanda baca (titik, koma, tanda tanya, dll)
corpus.processed = tm_map(corpus.processed, removePunctuation)

# Menghapus kata umum bahasa Inggris (the, a, is, dll)
corpus.processed = tm_map(corpus.processed, removeWords, stopwords("en"))

# Menghapus spasi berlebih (stripWhitespace)
corpus.processed = tm_map(corpus.processed, stripWhitespace)


# Mengubah hasil Corpus kembali ke Dataframe baru
df_clean = data.frame(
  text_clean = sapply(corpus.processed, content), 
  stringsAsFactors = FALSE
)


# Membuat tabel sederhana dari hasil preprocessing
tabel_review <- data.frame(
  
  # Membuat nomor review otomatis : Review 1, Review 2, dst
  No = paste0("Review ", 1:nrow(df_clean)),
  
  # Mengambil teks review yang sudah dibersihkan
  Isi_Review = df_clean$text_clean,
  
  # Agar teks tidak otomatis menjadi factor
  stringsAsFactors = FALSE
)

View(tabel_review)

# TermDocumentMatrix (TDM) yang berisi:
# - Baris   = kata
# - Kolom   = dokumen/review
# - Isi     = jumlah kemunculan kata

# weightTf yaitu menghitung frekuensi kemunculan kata secara langsung
tdm <- TermDocumentMatrix(
  corpus.processed,
  control = list(weighting = weightTf)
)

# Menampilkan isi matriks untuk melihat kata dan dokumen
inspect(tdm)

# Mengubah TDM menjadi matriks biasa
matrix_v <- as.matrix(tdm)

# Menghitung total frekuensi setiap kata. rowSums() menjumlahkan setiap baris
word_freq <- rowSums(matrix_v)

# Mengurutkan frekuensi dari terbesar ke terkecil
word_freq <- sort(word_freq, decreasing = TRUE)

# Membuat dataframe berisi:
# - word : nama kata
# - freq : jumlah kemunculan

df_word_freq <- data.frame(
  word = names(word_freq),
  freq = word_freq
)

# Menampilkan 10 kata dengan frekuensi tertinggi
head(df_word_freq, 10)


# ============================================
# VISUALISASI WORD CLOUD
# Visualisasi kata berdasarkan frekuensi kemunculan
# Semakin sering kata muncul, maka ukuran kata akan semakin besar

set.seed(123)

# MEMBUAT WORD CLOUD
wordcloud(
  
  # Sumber kata berasal dari corpus yang sudah diproses
  words = corpus.processed,
  
  # Kata minimal harus muncul 2 kali agar ditampilkan
  min.freq = 2,
  
  # Maksimal jumlah kata yang ditampilkan
  max.words = 100,
  
  # FALSE = kata diurutkan berdasarkan frekuensi dari yang paling sering muncul
  random.order = FALSE,
  
  # Persentase kata yang diputar vertikal 0.35 = sekitar 35%
  rot.per = 0.35,
  
  # Menggunakan palet warna "Dark2" dari package RColorBrewer
  colors = brewer.pal(8, "Dark2")
)


