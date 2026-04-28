# Mengimpor library yang dibutuhkan
from flask import Flask, render_template, request  # Flask untuk web, request untuk ambil input user
import joblib  # Untuk load model machine learning
import pandas as pd  # Untuk mengolah data dalam bentuk tabel (DataFrame)

# Membuat aplikasi Flask
app = Flask(__name__)

# Memuat model yang sudah dilatih (file .pkl)
model = joblib.load('model_fraud.pkl')

# Route untuk halaman utama (homepage)
@app.route('/')
def home():
    # Menampilkan file HTML (index.html) saat pertama kali web dibuka
    return render_template('index.html')

# Route untuk proses prediksi (ketika tombol submit ditekan)
@app.route('/predict', methods=['POST'])
def predict():
    
    # ================================
    # 1. Mengambil input dari user (form HTML)
    # ================================
    # request.form['nama_input'] harus sama dengan name di HTML
    data = {
        'Income': [float(request.form['income'])],      # Mengambil input income lalu ubah ke float
        'Profession': [request.form['profession']]      # Mengambil input profession (kategori)
        
        # Jika ada fitur lain (Age, Gender, dll), tambahkan di sini
    }

    # ================================
    # 2. Mengubah data menjadi DataFrame
    # ================================
    # Model ML hanya bisa menerima data dalam bentuk tabel
    df_input = pd.DataFrame(data)

    # ================================
    # 3. Melakukan prediksi menggunakan model
    # ================================
    prediction = model.predict(df_input)  # Output: array, misal [0] atau [1]

    # ================================
    # 4. Mengubah hasil prediksi menjadi teks
    # ================================
    # Jika 1 = Fraud (penipuan), jika 0 = Aman
    hasil = "PENIPUAN" if prediction[0] == 1 else "AMAN"

    # ================================
    # 5. Menampilkan hasil ke halaman web
    # ================================
    return render_template(
        'index.html',
        prediction_text=f'Hasil Deteksi: {hasil}'
    )

# Menjalankan aplikasi
if __name__ == '__main__':
    # debug=True → agar error terlihat saat development
    app.run(debug=True)