# 1. LOAD LIBRARY
library(tidyverse)

# 2. LOAD DATASET BAWAAN
# Kita gunakan dataset 'starwars'
data_sw <- starwars

# 3. WORKFLOW: CLEANING & MANIPULASI
# Kita ingin mencari karakter manusia yang paling tinggi, 
# lalu menghitung BMI mereka.
hasil_analisis <- data_sw %>%
  # --- Step A: Select (dplyr) ---
  # Pilih kolom yang relevan saja
  select(name, height, mass, species, homeworld) %>%
  
  # --- Step B: Cleaning Data (tidyr) ---
  # Hapus baris yang tinggi (height) atau beratnya (mass) kosong
  drop_na(height, mass) %>%
  
  # --- Step C: Filter (dplyr) ---
  # Fokus hanya pada spesies Manusia (Human)
  filter(species == "Human") %>%
  
  # --- Step D: Mutate (dplyr) ---
  # Buat kolom BMI. Rumus: mass / (height/100)^2
  # Kita bagi height dengan 100 karena satuan di dataset ini adalah cm
  mutate(bmi = mass / (height/100)^2) %>%
  
  # --- Step E: Arrange (dplyr) ---
  # Urutkan berdasarkan BMI tertinggi
  arrange(desc(bmi))

# 4. AGREGASI (group_by & summarise)
# Mari kita bandingkan rata-rata tinggi dan berat per planet (homeworld)
ringkasan_planet <- data_sw %>%
  drop_na(height, homeworld) %>%
  group_by(homeworld) %>%
  summarise(
    jumlah_karakter = n(),
    rata_tinggi = mean(height),
    berat_maksimal = max(mass, na.rm = TRUE)
  ) %>%
  filter(jumlah_karakter > 1) # Hanya tampilkan planet dengan lebih dari 1 karakter

# 5. TAMPILKAN HASIL
print("--- Karakter Manusia dengan BMI Tertinggi ---")
print(head(hasil_analisis))

print("--- Statistik Karakter Berdasarkan Planet Asal ---")
print(head(ringkasan_planet))