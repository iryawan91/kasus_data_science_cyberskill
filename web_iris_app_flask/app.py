from flask import Flask, render_template, request
import pickle
import numpy as np

app = Flask(__name__)

# 1. Load model yang sudah kita buat di Colab
model = pickle.load(open('model_iris.pkl', 'rb'))

@app.route('/')
def home():
    # Menampilkan halaman utama
    return render_template('index.html')

@app.route('/predict', methods=['POST'])
def predict():
    # 2. Ambil input dari form HTML
    # Urutannya: sepal_length, sepal_width, petal_length, petal_width
    input_data = [float(x) for x in request.form.values()]
    final_features = [np.array(input_data)]
    
    # 3. Prediksi menggunakan model
    prediction = model.predict(final_features)
    
    # 4. Kirim hasil prediksi kembali ke halaman web
    return render_template('index.html', 
                           prediction_text=f'Spesies Bunga adalah: {prediction[0]}')

if __name__ == "__main__":
    app.run(debug=True)