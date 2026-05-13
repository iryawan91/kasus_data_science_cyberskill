# Mengaktifkan library 'shiny' untuk membuat aplikasi web interaktif
library(shiny)
# Mengaktifkan library 'ggplot2' untuk membuat grafik yang elegan
library(ggplot2)

# ============================================================
# 1. USER INTERFACE (UI) - Bagian Tampilan
# ============================================================
# fluidPage membuat halaman web yang lebar tampilannya bisa menyesuaikan ukuran layar browser
ui <- fluidPage(
  # Menentukan judul utama yang akan muncul di bagian atas aplikasi
  titlePanel("Simulasi Regresi Linear Sederhana"),
  
  # Membuat tata letak dengan satu bilah samping (sidebar) dan satu panel utama (main)
  sidebarLayout(
    # Panel di sisi kiri untuk tempat kontrol/input dari pengguna
    sidebarPanel(
      # Membuat kotak input angka untuk menentukan berapa banyak data (N) yang ingin disimulasikan
      numericInput(inputId = "n_sampel", 
                   label = "Jumlah Data (n):", 
                   value = 50, min = 10, max = 500),
      
      # Membuat tombol klik untuk memerintahkan sistem membuat ulang data acak
      actionButton(inputId = "update", 
                   label = "Acak Data Baru", 
                   class = "btn-primary"), # class 'btn-primary' memberikan warna biru pada tombol
      
      # Menambahkan garis horizontal sebagai pemisah visual
      hr(),
      # Menampilkan teks bantuan atau keterangan tambahan di sidebar
      helpText("Kodingan ini membuat variabel X secara acak, 
                lalu menghitung Y dengan rumus: Y = 2X + error")
    ),
    
    # Panel di sisi kanan untuk menampilkan hasil (output) dari kodingan
    mainPanel(
      # Menyediakan tempat untuk menampilkan grafik (output plot)
      plotOutput(outputId = "plotRegresi"),
      
      # Menyediakan tempat untuk menampilkan teks statis/hasil statistik (verbatim)
      verbatimTextOutput(outputId = "summaryRegresi")
    )
  )
)

# ============================================================
# 2. SERVER - Bagian Logika (Otak Aplikasi)
# ============================================================
# Fungsi server memproses input dari pengguna dan mengirimkan hasilnya kembali ke UI
server <- function(input, output) {
  
  # Fungsi reactive() digunakan untuk membuat data yang akan 'hidup' secara otomatis
  # Data ini akan dihitung ulang setiap kali input$n_sampel berubah atau input$update diklik
  data_simulasi <- reactive({
    input$update # Menghubungkan variabel ini ke tombol 'update' agar memicu perhitungan ulang
    
    n <- input$n_sampel # Mengambil nilai jumlah sampel dari input UI
    x <- runif(n, min = 0, max = 100)           # Menghasilkan angka acak berdistribusi seragam (0-100)
    error <- rnorm(n, mean = 0, sd = 20)        # Menghasilkan 'noise' atau galat acak (distribusi normal)
    y <- 10 + 2.5 * x + error                   # Membuat hubungan linear: Y = Intersept + Slope*X + Error
    
    data.frame(X = x, Y = y) # Menggabungkan X dan Y menjadi tabel (data frame)
  })
  
  # Mengirimkan instruksi pembuatan grafik ke ID 'plotRegresi' yang ada di UI
  output$plotRegresi <- renderPlot({
    df <- data_simulasi() # Memanggil data hasil simulasi tadi
    
    # Menggunakan ggplot2 untuk menggambar grafik
    ggplot(df, aes(x = X, y = Y)) +
      geom_point(color = "darkblue", alpha = 0.6) + # Menggambar titik scatter plot warna biru transparan
      geom_smooth(method = "lm", col = "red") +     # Menambahkan garis tren regresi (Linear Model) warna merah
      theme_minimal() +                             # Menggunakan tema grafik yang bersih/minimalis
      labs(title = "Scatter Plot dan Garis Regresi") # Memberikan judul pada grafik
  })
  
  # Mengirimkan instruksi tampilan teks statistik ke ID 'summaryRegresi' yang ada di UI
  output$summaryRegresi <- renderPrint({
    df <- data_simulasi()          # Memanggil data yang sama dengan grafik
    model <- lm(Y ~ X, data = df)  # Melakukan analisis regresi linear sederhana (Y dipengaruhi X)
    summary(model)                 # Menampilkan detail statistik (koefisien, R-squared, p-value, dll)
  })
}

# ============================================================
# 3. RUN APP - Menjalankan Aplikasi
# ============================================================
# Perintah terakhir untuk menyatukan UI dan Server menjadi aplikasi utuh
shinyApp(ui = ui, server = server)