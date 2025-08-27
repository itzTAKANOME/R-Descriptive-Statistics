# FUNGSI MEAN
hitung_mean <- function(data) {
  # Periksa Numerik
  if (!is.numeric(data)) {
    stop("Error: Data harus berupa vektor numerik!")
  }
  
  # Periksa nilai NA
  if (any(is.na(data))) {
    warning("Peringatan: Data mengandung nilai NA yang akan diabaikan")
    data <- data[!is.na(data)]  # Hilangkan nilai NA
  }
  
  # Periksa data setelah menghilangkan NA
  if (length(data) == 0) {
    stop("Error: Tidak ada data numerik yang valid!")
  }
  
  # Hitung mean
  jumlah_total <- 0
  for (i in 1:length(data)) {
    jumlah_total <- jumlah_total + data[i]
  }
  
  rata_rata <- jumlah_total / length(data)
  return(rata_rata)
}

# FUNGSI STANDARD DEVIATION
hitung_sd <- function(data, populasi = FALSE) {
  # Periksa numerik
  if (!is.numeric(data)) {
    stop("Error: Data harus berupa vektor numerik!")
  }
  
  # Periksa nilai NA
  if (any(is.na(data))) {
    warning("Peringatan: Data mengandung nilai NA yang akan diabaikan")
    data <- data[!is.na(data)]  # Hilangkan nilai NA
  }
  
  # Periksa data setelah menghilangkan NA
  if (length(data) == 0) {
    stop("Error: Tidak ada data numerik yang valid!")
  }
  
  # Periksa apakah ada cukup data untuk menghitung SD
  if (length(data) < 2 && !populasi) {
    stop("Error: Butuh minimal 2 observasi untuk menghitung sample standard deviation!")
  }
  
  # Hitung mean
  rata_rata <- hitung_mean(data)
  
  # Hitung jumlah kuadrat selisih dari mean
  jumlah_kuadrat_selisih <- 0
  for (i in 1:length(data)) {
    selisih <- data[i] - rata_rata
    kuadrat_selisih <- selisih * selisih
    jumlah_kuadrat_selisih <- jumlah_kuadrat_selisih + kuadrat_selisih
  }
  
  # Tentukan pembagi
  if (populasi) {
    # Untuk populasi
    pembagi <- length(data)
  } else {
    # Untuk sampel
    pembagi <- length(data) - 1
  }
  
  # Hitung varians
  varians <- jumlah_kuadrat_selisih / pembagi
  
  # Standard deviation
  std_dev <- sqrt(varians)
  
  return(std_dev)
}

# FUNGSI SUMMARY
summary_stats <- function(data, populasi = FALSE) {
  # Periksa numerik
  if (!is.numeric(data)) {
    stop("Error: Data harus berupa vektor numerik!")
  }
  
  # Hitung statistik
  rata_rata <- hitung_mean(data)
  std_dev <- hitung_sd(data, populasi = populasi)
  
  # Hitung statistik tambahan secara manual
  n <- length(data[!is.na(data)])  # Jumlah observasi valid
  varians <- std_dev^2  # Varians
  
  # Buat list hasil
  hasil <- list(
    n = n,
    mean = rata_rata,
    variance = varians,
    std_deviation = std_dev,
    type = if(populasi) "Populasi" else "Sampel"
  )
  
  return(hasil)
}

# DATA FRAME
# Data mahasiswa dengan berbagai variabel
data_mahasiswa <- data.frame(
  nama = c("Andi", "Budi", "Citra", "Dina", "Eko", "Farah", "Gilang", "Hana"),
  umur = c(20, 21, 19, 22, 20, 21, 23, 19),
  tinggi_cm = c(165, 170, 158, 172, 168, 160, 175, 162),
  berat_kg = c(55, 65, 50, 68, 60, 52, 70, 48),
  nilai_matematika = c(85, 78, 92, 76, 88, 95, 82, 90),
  nilai_fisika = c(80, 85, 88, 72, 84, 91, 79, 87),
  jenis_kelamin = c("L", "L", "P", "P", "L", "P", "L", "P"),
  jurusan = c("Teknik", "Ekonomi", "Teknik", "Psikologi", "Teknik", "Ekonomi", "Teknik", "Psikologi"),
  stringsAsFactors = FALSE
)

# nilai NA untuk testing
data_dengan_na <- c(85, 78, 92, NA, 88, 95, 82, 90, NA)


cat("DATA MAHASISWA:\n")
print(data_mahasiswa)
cat("\n")

mean_umur <- hitung_mean(data_mahasiswa$umur)
cat("Mean umur mahasiswa:", mean_umur, "tahun\n")

mean_tinggi <- hitung_mean(data_mahasiswa$tinggi_cm)
cat("Mean tinggi mahasiswa:", mean_tinggi, "cm\n")

mean_nilai_mat <- hitung_mean(data_mahasiswa$nilai_matematika)
cat("Mean nilai matematika:", mean_nilai_mat, "\n\n")

sd_umur <- hitung_sd(data_mahasiswa$umur)
cat("Standard deviation umur (sampel):", sd_umur, "tahun\n")

sd_tinggi <- hitung_sd(data_mahasiswa$tinggi_cm)
cat("Standard deviation tinggi (sampel):", sd_tinggi, "cm\n")

# Bandingkan populasi
sd_tinggi_pop <- hitung_sd(data_mahasiswa$tinggi_cm, populasi = TRUE)
cat("Standard deviation tinggi (populasi):", sd_tinggi_pop, "cm\n\n")


stats_berat <- summary_stats(data_mahasiswa$berat_kg)
cat("Statistik Berat Badan (Sampel):\n")
cat("  Jumlah observasi:", stats_berat$n, "\n")
cat("  Mean:", round(stats_berat$mean, 2), "kg\n")
cat("  Varians:", round(stats_berat$variance, 2), "\n")
cat("  Standard Deviation:", round(stats_berat$std_deviation, 2), "kg\n")
cat("  Tipe:", stats_berat$type, "\n\n")


cat("Data dengan NA:", paste(data_dengan_na, collapse = ", "), "\n")
mean_dengan_na <- hitung_mean(data_dengan_na)
sd_dengan_na <- hitung_sd(data_dengan_na)
cat("Mean (mengabaikan NA):", round(mean_dengan_na, 2), "\n")
cat("Standard Deviation (mengabaikan NA):", round(sd_dengan_na, 2), "\n\n")


cat("ANALISIS UNTUK SEMUA VARIABEL NUMERIK\n")
cat("----------------------------------------\n")

variabel_numerik <- c("umur", "tinggi_cm", "berat_kg", "nilai_matematika", "nilai_fisika")

for (var in variabel_numerik) {
  cat("Variabel:", var, "\n")
  stats <- summary_stats(data_mahasiswa[[var]])
  cat("  Mean:", round(stats$mean, 2), "\n")
  cat("  SD:", round(stats$std_deviation, 2), "\n")
  cat("  N:", stats$n, "\n\n")
}

