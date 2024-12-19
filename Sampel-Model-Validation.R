##### PERHITUNGAN BESAR SAMPEL MODEL PREDIKSI LINEAR DENGAN LUARAN KONTINU #####

###### HAK CIPTA #####
# 2024 © MADE RADITYA ARHYA PUTRA

###### PAPER RUJUKAN #####
# 1. Metodologi Perhitungan Sampel:
#    Riley RD, Snell KIE, Ensor J, et al. 
#    "Minimum sample size for developing a multivariable prediction model: PART I - Continuous outcomes"
#    Statistics in Medicine. 2019;38(7):1262-1275
#    DOI: https://doi.org/10.1002/sim.7993
#
# 2. Sumber Data Rerata dan Standar Deviasi Skor GENSINI:
#    Wahyuni I, Wijaya IP, Sukrisman L, Nasution SA, Rumende CM.
#    "Diagnostic Accuracy of Platelet/Lymphocyte Ratio for Screening Complex Coronary Lesion 
#    in Different Age Group of Patients with Acute Coronary Syndrome"
#    Acta Medica Indonesiana. 2018;50(3)
#    URL: https://www.actamedindones.org/index.php/ijim/article/view/401

###### METODE PERHITUNGAN #####
# Menggunakan package R 'pmsampsize', yang mengimplementasikan metodologi Riley et al.
# Package ini menghitung ukuran sampel minimum yang diperlukan untuk mengembangkan model prediksi
# Berdasarkan empat kriteria utama untuk meminimalkan overfitting dan memastikan estimasi parameter yang presisi:
# 1. Overfitting yang kecil (ditentukan oleh expected shrinkage ≤ 10%)
# 2. Perbedaan absolut yang kecil (0.05) antara R-squared apparent dan adjusted
# 3. Estimasi yang presisi dari standar deviasi residual
# 4. Estimasi yang presisi dari nilai outcome rata-rata

###### PERHITUNGAN BESAR SAMPEL #####
# Memuat package yang diperlukan
library(pmsampsize)

# Menghitung ukuran sampel minimum
hasil_sampel <- pmsampsize(
  type = "c",           # Menentukan luaran kontinu (skor GENSINI)
  rsquared = 0.15,      # Perkiraan R-squared (estimasi konservatif untuk prediksi medis)
  parameters = 5,       # Jumlah parameter prediktor (usia [1], LDL [1], gula darah sewaktu [1], sistolik [1], IMT [1]) -> Nilai dapat disesuaikan dengan ketersediaan data
  intercept = 169.3,    # Rerata skor GENSINI tertinggi (dari kelompok usia ≤45 tahun dengan Gensini ≤53)
  sd = 74.1,            # SD tertinggi (dari kelompok usia ≤45 tahun dengan Gensini ≤53)
  shrinkage = 0.9,      # Faktor shrinkage target (memungkinkan 10% overfitting) - default 0.9
  mmoe = 1.1            # (Multiplicative Margin of Error) Margin error multiplikatif untuk estimasi intercept - default 1.1
)

###### PENJELASAN PARAMETER #####
# type = "c"         : Menunjukkan luaran kontinu (skor GENSINI)
# rsquared = 0.15    : Estimasi konservatif karena tidak ada model serupa sebelumnya / seberapa besar nilai variabilitas outcome pada model
# parameters = 4     : Empat prediktor (usia kontinu, jenis kelamin biner, LDL kontinu, gula darah sewaktu kontinu)
# intercept = 169.3  : Nilai rerata tertinggi skor GENSINI dari studi Wahyuni et al. (2018)
# sd = 74.1          : Nilai SD tertinggi dari studi Wahyuni et al. (2018)
# shrinkage = 0.9    : Nilai yang direkomendasikan untuk mengontrol overfitting sesuai paper Riley et al, 2019
# mmoe = 1.1         : Nilai yang direkomendasikan untuk estimasi intercept yang presisi sesuai paper Riley et al, 2019, 
#                      artinya diperbolehkan 10% error dalam model menentukan mean dikemudian hari

###### INTERPRETASI HASIL #####
# Hasil perhitungan akan menampilkan:
# 1. Ukuran sampel minimum yang diperlukan untuk setiap kriteria
# 2. Ukuran sampel final yang direkomendasikan (nilai tertinggi dari semua kriteria)
# 3. Jumlah minimal kejadian yang diperlukan
# 4. Rasio subjek per parameter prediktor

# Menampilkan hasil perhitungan
print(hasil_sampel)


#### MENYIAPKAN ENVIRONMENT #####

# Membersihkan semua objek dari memory
rm(list=ls()) 

# Mengatur seed untuk reproducible random numbers
set.seed(42) 




#### DATA SIMULASI ####
###### MEMBUAT DATA SIMULASI ######
library(MASS)

# Menentukan jumlah observasi simulasi
N <- 100 

# Membuat matriks korelasi dengan pola exponential decay
Sigma <- outer(1:10, 1:10, function(x,y) 0.5^abs(x-y))
# Menghasilkan matriks 10x10 dengan sifat:
# - Simetris (korelasi i,j sama dengan j,i)
# - Definit positif (syarat untuk matriks kovarian)
# - Nilai diagonal = 1 (korelasi variabel dengan dirinya sendiri)
# - Korelasi menurun secara exponential berdasarkan jarak antar variabel

# Membuat data multivariat normal
x <- mvrnorm(N, rep(0,10), Sigma)
# - Menggunakan fungsi mvrnorm dari package MASS
# - Menghasilkan matriks N×10 (100 observasi, 10 variabel)
# - Mean setiap variabel = 0 (rep(0,10))
# - Struktur korelasi antar variabel mengikuti matriks Sigma

# Mengubah beberapa variabel kontinyu menjadi kategorik
x[,3] <- ifelse(x[,3] > 0.5, 1, 0)    # Mengubah x3 menjadi biner dengan threshold 0.5
x[,4] <- ifelse(x[,4] > 0, 1, 0)      # Mengubah x4 menjadi biner dengan threshold 0
x[,5] <- cut(x[,5], breaks=c(-Inf, -1, 0, 1, Inf), labels = FALSE)  # Mengubah x5 menjadi kategorik 4 level
x[,8] <- ifelse(x[,8] > -0.5, 1, 0)   # Mengubah x8 menjadi biner dengan threshold -0.5
x[,9] <- ifelse(x[,9] > 0.5, 1, 0)    # Mengubah x9 menjadi biner dengan threshold 0.5
x[,10] <- cut(x[,10], breaks=c(-Inf, -1, 0, 1, Inf), labels = FALSE) # Mengubah x10 menjadi kategorik 4 level

# Mengubah matriks menjadi data frame dan memberi nama kolom
data.cont.complete <- data.frame(x)
colnames(data.cont.complete) <- c(paste0("x", 1:5), paste0("z", 1:5))

# Membuat variabel respon (y) dengan hubungan non-linear
?with
data.cont.complete$y <- with(
  data.cont.complete,
  x1 + 0.2*x1^2 + 0.5*x2 - 0.2*x2^2 + 0.3*x3 + 0.2*x4 + 0.2*(x5==2) - 0.1*(x5==3) + 0.4*(x5==4) + rnorm(N,0,1)
)
# Komponen dalam pembuatan y:
# - Efek linear: x1, x2
# - Efek kuadratik: x1^2, x2^2
# - Efek variabel biner: x3, x4
# - Efek kategorik x5:
#   * Level 2: +0.2
#   * Level 3: -0.1
#   * Level 4: +0.4
#   * Level 1: 0 (kategori referensi)
# - Random error: rnorm(N,0,1) untuk menambahkan noise

# Melihat 6 baris pertama data
head(data.cont.complete)

# Mengubah variabel kategorik menjadi faktor
data.cont.complete[,c(3:5, 8:10)] <- lapply(data.cont.complete[,c(3:5, 8:10)], factor)
# - Kolom 3,4,8,9: variabel biner (0/1)
# - Kolom 5,10: variabel kategorik (1-4)
# Data ini cocok untuk:
# - Simulasi model regresi
# - Pengujian metode statistik
# - Studi simulasi dengan prediktor campuran (kontinyu, biner, kategorik)

# Melihat 6 baris pertama data setelah konversi ke faktor
head(data.cont.complete)





###### MEMBUAT MISSING DATA ######
# Step 1: Create matrix of zeros with same dimensions as our data
missing.matrix = matrix(0, nrow=nrow(data.cont.complete), ncol=ncol(data.cont.complete))
# Membuat matriks kosong (berisi 0) dengan ukuran yang sama dengan data asli 

# Step 2: Fill matrix with random binary (0/1) using rbinom
missing.matrix = matrix(rbinom(length(missing.matrix), 1, p=0.1), nrow=nrow(data.cont.complete))
# rbinom(n, size=1, prob=0.1) menghasilkan bilangan random bernoulli (0 atau 1)
# p=0.1 artinya ada 10% peluang mendapatkan nilai 1 (missing)
# length(missing.matrix) adalah total sel dalam matriks

# Step 3: Create copy of complete data
data.cont = data.cont.complete

# Step 4: Set values to NA where missing.matrix = 1
data.cont[missing.matrix==1] = NA
dim(data.cont)
dim(missing.matrix)
# Mengubah nilai menjadi NA (missing) pada posisi dimana missing.matrix bernilai 1
# Sehingga dimensi matriks data.cont dan missing.matrix itu harus sama
# Sehingga sekitar 10% data akan menjadi missing secara random (MCAR - Missing Completely At Random)

# Step 5: Create a clustering variable
# Tujuan: Mengelompokkan data untuk internal-external validation
data.cont$clust <- factor(
  sample(                  # Mengambil sampel random
    1:5,                   # Dari angka 1-5 (5 cluster)
    size = N,              # Sebanyak N (jumlah observasi)
    replace = TRUE,        # Dengan pengembalian atau nilai yang sama dapat diambil kembali
    prob = rep(0.2,5)      # Probabilitas sama untuk tiap cluster (0.2)
  )
)

# Mengurutkan data berdasarkan cluster
# Tujuan: Memudahkan visualisasi dan analisis per cluster
data.cont <- data.cont[order(data.cont$clust),]

# Step 6: View first few rows
head(data.cont)

# Melihat proporsi missing di setiap kolom
colMeans(is.na(data.cont))

# Melihat posisi NA dalam data
which(is.na(data.cont), arr.ind=TRUE)

# Membandingkan data asli vs data dengan missing
head(data.cont.complete) # data asli
head(data.cont)         # data dengan NA





###### VISUALISASI DISTRIBUSI OUTCOME DARI DATA SIMULASI ####
library(ggplot2)
# Create histogram of outcome variable y
ggplot(data.cont, aes(x=y)) + geom_histogram(color="black", fill="white", binwidth = 0.5)
summary(data.cont$y)
# Membuat histogram untuk melihat distribusi variabel y
# binwidth = 0.5 menentukan lebar tiap bar dalam histogram


#### KOREKSI MISSING DATA ####
###### MULTIPLE IMPUTATION UNTUK MISSING DATA ####
# Tujuan: Mengisi missing data dengan mempertimbangkan hubungan antar variabel
# Metode: Multiple imputation dengan aregImpute dari package Hmisc
# Output: 10 dataset lengkap dengan nilai imputasi yang berbeda-beda

# Periksa jumlah missing data
sum(complete.cases(data.cont[,1:5])) # jumlah pasien dengan data observasi lengkap untuk prediktor x1,2,3,4,5 (5 kolom pertama)
sum(complete.cases(data.cont$y)) # jumlah pasien dengan data observasi lengkap untuk outcome y

# Visualisasikan missing data dengan mice dan VIM (Visualization of Missing and Imputed Values)
library(mice)
md.pattern(data.cont, plot = TRUE)

library(VIM)
aggr_plot <- aggr(
  data.cont,
  col=c('navyblue','red'),
  numbers=TRUE,
  sortVars=TRUE,
  labels=names(data.cont),
  cex.axis=.7,
  gap=3,
  ylab=c("Histogram of missing data","Pattern")
)


# Load package yang dibutuhkan
library(Hmisc)

# Proporsi missing di data asli
colSums(is.na(data.cont))/nrow(data.cont) 

# Menentukan jumlah imputasi
# - Minimal direkomendasikan 5-10 imputasi
# - Semakin besar n.impute, semakin stabil estimasi tapi komputasi lebih lama
# - Rule of thumb: n.impute minimal sama dengan persentase missing data
n.impute <- 10

# Melakukan multiple imputation menggunakan aregImpute
a <- aregImpute(
  # Formula model:
  # - y sebagai outcome
  # - x1,x2: variabel kontinyu dengan splines (hubungan non-linear)
  # - x3,x4,x5: variabel kategorik untuk predictor pertama (dibungkus dengan I())
  # - z1,z2: variabel kontinyu tanpa splines
  # - z3,z4,z5: variabel kategorik untuk predictor kedua (dibungkus dengan I())
  formula = I(y)~x1+x2+I(x3)+I(x4)+I(x5)+z1+z2+I(z3)+I(z4)+I(z5),
  data = data.cont,    # dataset dengan missing values
  n.impute = n.impute, # jumlah dataset imputasi yang diinginkan
  nk = 3,              # jumlah knots untuk splines (x1 dan x2)
  match = 'closest'    # metode predictive mean matching
)

# Menyimpan hasil imputasi dalam list
# Setiap elemen list berisi satu dataset lengkap hasil imputasi
imputed1 <- list()

# Loop untuk mendapatkan setiap dataset hasil imputasi
for (i in 1:n.impute){
  # impute.transcan: mengaplikasikan hasil aregImpute ke data
  # - Mengambil hasil transformasi dari aregImpute
  # - Mengaplikasikan ke missing values
  # - Menghasilkan dataset lengkap
  imputed1[[i]] <- impute.transcan(
    a,                # hasil dari aregImpute
    imputation = i,   # mengambil hasil imputasi ke-i
    data = data.cont, # data original dengan missing
    list.out = TRUE,  # output dalam bentuk list
    pr = TRUE,       # tidak print progress
    check = FALSE     # tidak cek konsistensi
  )
}

# PENTING: Penanganan missing pada outcome (y)
# JIKA tidak menggunakan auxiliary variables:
missing.y <- which(is.na(data.cont$y))
for (i in 1:n.impute){
  imputed1[[i]] <- as.data.frame(imputed1[[i]])
  imputed1[[i]] <- imputed1[[i]][-missing.y,]
}

# JIKA menggunakan auxiliary variables:
# Biarkan kode selesai sampai di atas loop kedua

# Melihat hasil imputasi pertama
head(imputed1[[1]])

# Melihat hasil imputasi dan membandingkan data dengan missing values (data.cont)

# Proporsi missing di data asli
colSums(is.na(data.cont))/nrow(data.cont)  

# Mengkonversi hasil imputasi ke format data frame untuk pengecekan
imputed_df1 <- as.data.frame(imputed1[[1]])
colSums(is.na(imputed_df1))/nrow(imputed_df1)  # seharusnya semua 0 karena sudah diimputasi

# Alternatif: melihat struktur data hasil imputasi
str(imputed1[[1]])

# Melihat beberapa baris pertama hasil imputasi
head(as.data.frame(imputed1[[1]]))




#### MODEL OLS ####
###### FIT PREDICTION MODELS TIPE OLS ####

# Load library yang dibutuhkan
library(rms)    # untuk fungsi ols dan rcs

# Membuat list untuk menyimpan model regresi
regression.splines <- list()

# Fit model untuk setiap dataset hasil imputation
for (i in 1:n.impute){
  # Ordinary Least Squares dengan restricted cubic splines
  regression.splines[[i]] <- ols(
    # Formula model:
    # - y: outcome
    # - rcs(x1,3): restricted cubic splines untuk x1 dengan 3 knots
    # - rcs(x2,3): restricted cubic splines untuk x2 dengan 3 knots
    # - x3,x4,x5: predictor kategorik
    formula = y ~ rcs(x1,3) + rcs(x2,3) + x3 + x4 + x5,
    data = imputed1[[i]]
  )
}

###### VISUALISASI SPLINES UNTUK MODEL OLS ####
# Tujuan: Memvisualisasikan hubungan non-linear antara prediktor kontinyu (x1, x2) 
# dengan outcome (y) yang dimodelkan menggunakan restricted cubic splines

# Load package untuk menyusun multiple plot
library(gridExtra)  

###### PLOT UNTUK VARIABEL X1 -------------------------
p.sp1 <- ggplot(
  # Membuat data frame untuk plotting x1
  data.frame(
    # Membuat sequence nilai x1 dari -3 sampai 3 dengan interval 0.1
    x1 = seq(-3,3, 0.1),  
    
    # Prediksi y menggunakan model OLS dengan splines
    # - Variasi nilai x1 sesuai sequence
    # - Nilai prediktor lain dibuat tetap:
    #   * x2 = 0 (nilai tengah)
    #   * x3 = 1 (kategori referensi)
    #   * x4 = 1 (kategori referensi) 
    #   * x5 = 2 (kategori referensi)
    y = Predict(regression.splines[[i]], 
                x1 = seq(-3,3, 0.1),
                x2 = 0, 
                x3 = 1, 
                x4 = 1, 
                x5 = 2)$yhat
  ),
  # Mapping estetika plot
  aes(x=x1, y=y)
) + geom_smooth()  # Tambahkan garis smooth dengan confidence interval

###### PLOT UNTUK VARIABEL X2 -------------------------
p.sp2 <- ggplot(
  # Membuat data frame untuk plotting x2
  data.frame(
    # Membuat sequence nilai x2 dari -3 sampai 3 dengan interval 0.1 
    x2 = seq(-3,3, 0.1),
    
    # Prediksi y menggunakan model OLS dengan splines
    # - Variasi nilai x2 sesuai sequence
    # - Nilai prediktor lain dibuat tetap:
    #   * x1 = 1 (nilai tengah)
    #   * x3 = 1 (kategori referensi)
    #   * x4 = 1 (kategori referensi)
    #   * x5 = 2 (kategori referensi)
    y = Predict(regression.splines[[i]], 
                x1 = 1,
                x2 = seq(-3,3, 0.1),
                x3 = 1,
                x4 = 1,
                x5 = 2)$yhat
  ),
  # Mapping estetika plot  
  aes(x=x2, y=y)
) + geom_smooth()  # Tambahkan garis smooth dengan confidence interval
?Predict
######  TAMPILKAN PLOT -------------------------
# Arrange kedua plot secara berdampingan dengan 2 kolom
grid.arrange(p.sp1, p.sp2, ncol=2)

# Output yang dihasilkan:
# - Dua plot yang menunjukkan hubungan non-linear antara x1 dan x2 dengan y
# - Sumbu x: nilai prediktor (x1 atau x2)
# - Sumbu y: nilai prediksi outcome (y)
# - Garis biru: estimasi hubungan non-linear dari splines
# - Area abu-abu: confidence interval dari estimasi




###### FUNGSI PREDIKSI (OLS) ####
# Menerima input: new.patient (data frame pasien baru), single.fit (model tunggal), multiple.fit (multiple model dari multiple imputation)
prediction.ols <- function(new.patient, single.fit = NULL, multiple.fit = NULL){
  
  # Jika menggunakan multiple imputation models
  if(!is.null(multiple.fit)){
    # Membuat grid untuk semua kombinasi pasien dan model imputed
    # k = index pasien, i = index model
    mygrid <- expand.grid(k = 1:dim(new.patient)[1], i = 1:length(multiple.fit))
    
    # Fungsi helper untuk membuat prediksi untuk satu pasien dengan satu model
    ff <- function(k,i){
      with(new.patient, 
           Predict(multiple.fit[[i]], 
                   x1 = x1[k], 
                   x2 = x2[k], 
                   x3 = x3[k], 
                   x4 = x4[k], 
                   x5 = x5[k])$y)
    }
    
    # Membuat matrix prediksi untuk semua pasien dari semua model
    # Baris = pasien, Kolom = model imputed
    prediction_matrix <- matrix(mapply(ff, mygrid$k, mygrid$i),
                                nrow = dim(new.patient)[1], 
                                ncol = length(multiple.fit))
    
    # Menghitung rata-rata prediksi across semua model untuk tiap pasien
    prediction <- apply(prediction_matrix, 1, mean)
    
    # Jika menggunakan single model (tanpa multiple imputation)  
  } else if(!is.null(single.fit)){
    # Fungsi helper untuk prediksi satu pasien
    ff <- function(k){
      with(new.patient, 
           Predict(single.fit, 
                   x1 = x1[k], 
                   x2 = x2[k], 
                   x3 = x3[k], 
                   x4 = x4[k], 
                   x5 = x5[k])$y)
    }
    # Membuat prediksi untuk semua pasien menggunakan single model
    prediction <- sapply(1:dim(new.patient)[1], ff)
  }
  
  return(prediction)
}



###### SAMPEL MANUAL UNTUK DIUJI MANUAL PADA OLS, GAM, RIDGE ####
new.patient <- data.frame(x1=1.2, x2=-1.6, x3=0, x4=1, x5=4)




###### TEST FUNGSI PREDIKSI (OLS) ####
hasil_prediksi <- prediction.ols(new.patient, multiple.fit = regression.splines)
print(hasil_prediksi)




###### TEST FUNGSI PREDIKSI (OLS) DENGAN DATASET DAN BANDINGKAN DENGAN OBSERVASI ####

# Mengambil subset data yang memiliki nilai lengkap (tidak ada NA) 
# untuk kolom 1-5 (prediktor) dan kolom 11 (outcome y)
complete.data <- data.cont[complete.cases(data.cont[,c(1:5,11)]),]

# Menggunakan fungsi prediction.ols untuk memprediksi outcome
# pada data lengkap menggunakan model regression.splines yang sudah difit
predicted.ols <- prediction.ols(complete.data, multiple.fit = regression.splines)

# Membuat plot menggunakan ggplot2
ggplot(
  # Membuat data frame dengan 2 kolom: nilai observasi dan nilai prediksi
  data.frame(observed=complete.data$y, predicted.ols=predicted.ols),
  # Menentukan variable untuk sumbu x dan y
  aes(x=predicted.ols, y=observed)
) +
  # Menambahkan titik-titik scatter plot
  geom_point(size=1) +
  # Menambahkan garis diagonal (y=x) sebagai referensi perfect prediction
  geom_abline(
    intercept = 0,    # garis mulai dari 0
    slope = 1,        # dengan kemiringan 45 derajat
    color="black",    # warna garis
    linetype="dashed", # garis putus-putus
    size=0.5          # ketebalan garis
  ) +
  # Membatasi rentang sumbu x
  xlim(-3,3) +
  # Membatasi rentang sumbu y
  ylim(-3,3) +
  # Menambahkan garis regresi linear dengan confidence interval
  geom_smooth(method=lm)



#### TIPE GAM ####
###### FIT PREDICTION MODELS TIPE GAM ####
library(mgcv) # library untuk menjalankan GAM
fit.gam=list()
# s() menandakan smooth function
for(i in 1:n.impute){fit.gam[[i]] <- gam(y ~ x3 + x4 + x5 + s(x1) + s(x2), data = imputed1[[i]])}


###### FUNGSI PREDIKSI (GAM) ####
prediction.gam <- function(new.patient, single.fit = NULL, multiple.fit =
                             NULL){
  if(!is.null(multiple.fit)){
    mygrid <- expand.grid(k = 1:dim(new.patient)[1],i =
                            1:length(multiple.fit))
    ff <- function(k,i){
      predict.gam(multiple.fit[[i]], newdata = new.patient[k,])}
    prediction_matrix <- matrix(mapply(ff, mygrid$k, mygrid$i),
                                nrow = dim(new.patient)[1], ncol =
                                  length(multiple.fit))
    prediction <- apply(prediction_matrix, 1, mean)
  } else if(!is.null(single.fit)){
    ff <- function(k){
      predict.gam(single.fit, newdata = new.patient[k,])
    }
    prediction <- sapply(1:dim(new.patient)[1], ff) }
  return(prediction) }


####### Visualisasi hasil fitting splines ######
new1 <- data.frame(x1=seq(-3,3, 0.2), x2=0, x3=1, x4=1, x5=2)
p.g1 <- ggplot(data.frame(x1=seq(-3,3, 0.2),
                          y=prediction.gam(new1, multiple.fit = fit.gam)), aes(x=x1,
                                                                               y=y)) +
  geom_smooth()
new2 <- data.frame(x1=2, x2=seq(-3,3, 0.2), x3=1, x4=1, x5=2)
p.g2 <- ggplot(data.frame(x2=seq(-3,3, 0.2),
                          y=prediction.gam(new2, multiple.fit = fit.gam)), aes(x=x2,
                                                                               y=y)) +
  geom_smooth()
grid.arrange(p.g1, p.g2, ncol=2)
ggplot(data.frame(observed=complete.data$y,
                  predicted.gam=predicted.gam),
       aes(x=predicted.gam, y=observed)) + geom_point(size=1) +
  geom_abline(intercept = 0, slope = 1, color="black",
              linetype="dashed", size=0.5) +
  xlim(-5,5) + ylim(-5,5)


###### TEST FUNGSI PREDIKSI (GAM) ####
hasil_prediksi <- prediction.gam(new.patient, multiple.fit = fit.gam)
print(hasil_prediksi)




###### TEST FUNGSI PREDIKSI (GAM) DENGAN DATASET DAN BANDINGKAN DENGAN OBSERVASI ####
predicted.gam <- prediction.gam(complete.data, multiple.fit = fit.gam)
ggplot(data.frame(observed=complete.data$y,
                  predicted.gam=predicted.gam),
       aes(x=predicted.gam, y=observed)) + geom_point(size=1) +
  geom_abline(intercept = 0, slope = 1, color="black",
linetype="dashed", size=0.5) +
xlim(-3,3) + ylim(-3,3)+geom_smooth(method=lm)


#### MODEL RIDGE REGRESSION ####
###### FIT PREDICTION MODEL TIPE RIDGE REGRESSION ########
library(splines2)
library(glmnet)
lambdas <- 10^seq(2, -10, by = -0.3)
#first find the position of the knots for the splines
bsMat.x1 <- bSpline(data.cont$x1[complete.cases(data.cont$x1)],
                    knots = quantile(data.cont$x1, c(0.25, 0.5, 0.75),
                                     na.rm = TRUE))
bsMat.x2 <- bSpline(data.cont$x2[complete.cases(data.cont$x2)],
                    knots = quantile(data.cont$x2, c(0.25, 0.5, 0.75),
                                     na.rm = TRUE))
fit.ridge <- list()
for( i in 1:n.impute){
  dfSplined.x1 <- as.data.frame(predict(bsMat.x1, imputed1[[i]]$x1))
  dfSplined.x2 <- as.data.frame(predict(bsMat.x2, imputed1[[i]]$x2))
  imp <- imputed1[[i]]
  imp <- cbind(imp$y, imp$x3, imp$x4, imp$x5, dfSplined.x1, dfSplined.x2)
  colnames(imp) <- c("y", "x3", "x4", "x5", paste0("V",
                                                   as.character(1:(length(colnames(imp))-4))))
  data_glmnet <- model.matrix(y ~., data = imp)
  X <- as.matrix(data_glmnet[,-1])
  colnames(X)[1:2] <- c("x3", "x4")
  Y <- imp$y
  cvfit <- cv.glmnet(X,Y,family = "gaussian", alpha=0,
                     lambda = lambdas, nfolds=10)
  lambda.min <- cvfit$lambda.min
  fit.ridge[[i]] <- glmnet(X,Y,family = "gaussian", alpha=0, lambda =
                             lambda.min)
}


###### FUNGSI PREDIKSI (RIDGE) ######
prediction.ridge <- function(new.patient, single.fit = NULL, multiple.fit =
                               NULL){
  if(!is.null(multiple.fit)){
    mygrid <- expand.grid(k = 1:dim(new.patient)[1],i =
                            1:length(multiple.fit))
    ff <- function(k,i){
      dfSplined.x1 <- as.data.frame(predict(bsMat.x1, new.patient$x1[k]))
      dfSplined.x2 <- as.data.frame(predict(bsMat.x2, new.patient$x2[k]))
      imp <-
        data.frame(x3=1*(new.patient$x3[k]==1),x4=1*(new.patient$x4[k]==1),x52=1*(n
                                                                                  ew.patient$x5[k]==2),
                   x53=1*(new.patient$x5[k]==3),
                   x54=1*(new.patient$x5[k]==4), dfSplined.x1,
                   dfSplined.x2)
      colnames(imp)=c("x3","x4","x52","x53","x54", paste0("V",
                                                          as.character(1:(length(colnames(imp))-5))))
      predict(multiple.fit[[i]], newx = as.matrix(imp))
    }
    prediction_matrix <- matrix(mapply(ff, mygrid$k, mygrid$i),
                                nrow = dim(new.patient)[1], ncol =
                                  length(multiple.fit))
    prediction <- apply(prediction_matrix, 1, mean)
  } else if(!is.null(single.fit)){
    ff <- function(k){
      dfSplined.x1 <- as.data.frame(predict(bsMat.x1, new.patient$x1[k]))
      dfSplined.x2 <- as.data.frame(predict(bsMat.x2, new.patient$x2[k]))
      imp <-
        data.frame(x3=1*(new.patient$x3[k]==1),x4=1*(new.patient$x4[k]==1),x52=1*(new.patient$x5[k]==2),
                   x53=1*(new.patient$x5[k]==3),
                   x54=1*(new.patient$x5[k]==4), dfSplined.x1,
                   dfSplined.x2)
      colnames(imp)=c("x3","x4","x52","x53","x54", paste0("V",
                                                          as.character(1:(length(colnames(imp))-5))))
      predict(single.fit, newx = as.matrix(imp))
    }
    prediction <- sapply(1:dim(new.patient)[1], ff)
  }
  return(prediction)
}
prediction.ridge(new.patient, multiple.fit = fit.ridge)


###### GRAFIK #####
predicted.ridge <- prediction.ridge(complete.data, multiple.fit =
                                      fit.ridge)
ggplot(data.frame(observed=complete.data$y,
                  predicted.ridge=predicted.ridge),
       aes(x=predicted.ridge, y=observed))+ geom_point(size=1)+
  geom_abline(intercept = 0, slope = 1, color="black",
              linetype="dashed", size=0.5) +
  xlim(-3,3) + ylim(-3,3)+geom_smooth(method=lm)


##### KOMPARASI 3 MODEL (OLS/GAM/RIDGE) ######
p1<-ggplot(data.frame(predicted.gam=predicted.gam,
                      predicted.ols=predicted.ols),
           aes(x=predicted.ols, y=predicted.gam))+ geom_point(size=1)+
  geom_abline(intercept = 0, slope = 1,
              color="black",linetype="dashed", size=0.5)+
  xlim(-4.5,4.5)+ylim(-4.5,4.5)
p2<-ggplot(data.frame(predicted.ols=predicted.ols,
                      predicted.ridge=predicted.ridge),
           aes(x=predicted.ridge, y=predicted.ols))+ geom_point(size=1)+
  geom_abline(intercept = 0, slope = 1,
              color="black",linetype="dashed", size=0.5)+
  xlim(-4.5,4.5)+ylim(-4.5,4.5)
p3<-ggplot(data.frame(predicted.gam=predicted.gam,
                      predicted.ridge=predicted.ridge),
           aes(x=predicted.ridge, y=predicted.gam))+ geom_point(size=1)+
  geom_abline(intercept = 0, slope = 1,
              color="black",linetype="dashed", size=0.5)+
  xlim(-4.5,4.5)+ylim(-4.5,4.5)
grid.arrange(p1, p2, p3, ncol=3)


##### APPARENT PERFORMANCE SETIAP MODEL ####
### Kode ini akan komparasi performa berdasarkan predicted vs observed outcome
calculate_performance <- function(observed, predicted){
  MAE <- mean(abs(observed - predicted)) # mean absolute error
  MSE <- mean((observed - predicted)^2) # mean squared error
  R2 <- summary(lm(observed~predicted))$r.squared
  vec <- c(MAE, MSE, R2)
  names(vec) <- c("MAE", "MSE", "R2")
  return(vec)
}

apparent.ols <- calculate_performance(complete.data$y, predicted.ols)
apparent.gam <- calculate_performance(complete.data$y, predicted.gam)
apparent.ridge <- calculate_performance(complete.data$y, predicted.ridge)
round(rbind(apparent.ols, apparent.gam, apparent.ridge), digits=2)

##### INTERNAL VALIDATION: BOOTSTRAP OPTIMISM CORRECTION #####
# Menghitung optimism dan performa terkoreksi model (optimism-corrected performance model) menggunakan bootstrap validation
# Metode ini merupakan bagian dari validasi internal untuk menilai seberapa optimis performa model pada data pengembangan
# Sumber: (Steyerberg, 2019, Efthimiou, 2024)
# Metode bootstraping pada model ini dilakukan dengan n.impute = 10 * N.bootstrap = 10 sehingga totalnya 100x

# Menentukan jumlah bootstrap replikasi
N.bootstrap=10

# Membuat matriks kosong untuk menyimpan hasil optimism untuk setiap bootstrap
# Dimensi: N.bootstrap baris x 3 kolom (untuk MAE, MSE, R2)
optimism.ols.eachbootstrap <- optimism.gam.eachbootstrap <- optimism.ridge.eachbootstrap <- matrix(NA, N.bootstrap, 3)

# Membuat matriks untuk menyimpan rata-rata optimism dari setiap imputasi
optimism.ols <- optimism.gam <- optimism.ridge <- matrix(NA, n.impute, 3)

# Loop untuk setiap dataset hasil imputasi (10 kali)
for (i in 1:n.impute){
  # Loop untuk setiap bootstrap sample (10 kali)
  for(j in 1:N.bootstrap){
    # Mengambil sampel dengan pengembalian seukuran dataset asli
    boot.sample <- sample(length(imputed1[[i]]$y), replace = T)
    
    # Membuat bootstrap sample dari dataset yang telah diimputasi
    imp.boot <- lapply(imputed1[[i]], function(x){x[boot.sample]})
    
    # Fit model pada bootstrap sample
    regression.splines.boot <- ols(y~rcs(x1,3)+rcs(x2,3)+x3+x4+x5, data=imp.boot)
    fit.gam.boot <- gam(y ~ x3 + x4 + x5 + s(x1) + s(x2), data = imp.boot)
    
    dfSplined.x1 <- as.data.frame(predict(bsMat.x1, imp.boot$x1))
    dfSplined.x2 <- as.data.frame(predict(bsMat.x2, imp.boot$x2))
    
    imp <- cbind(imp.boot$y, imp.boot$x3, imp.boot$x4, imp.boot$x5, dfSplined.x1, dfSplined.x2)
    colnames(imp) = c("y", "x3", "x4", "x5", paste0("V", as.character(1:(length(colnames(imp))-4))))
    data_glmnet <- model.matrix(y ~ ., data = imp)
    X <- as.matrix(data_glmnet[,-1])
    colnames(X)[1:2] <- c("x3", "x4")
    Y <- imp$y
    cvfit <- cv.glmnet(X,Y,family = "gaussian", alpha=0, lambda = lambdas, nfolds=10)
    lambda.min <- cvfit$lambda.min
    fit.ridge.boot <- glmnet(X,Y,family = "gaussian", alpha=0, lambda = lambda.min)
    # predict in bootstrap
    f1 <- as.data.frame(
                        do.call(
                          cbind,
                          lapply(
                            imp.boot,
                            function(x){
                              as.numeric(as.character(x))
                            }
                          )
                        )
                      )
    
    # Prediksi pada bootstrap sample (apparent performance)
    boot.prediction.ols <- prediction.ols(f1, single.fit = regression.splines.boot)
    boot.prediction.gam <- prediction.gam(f1, single.fit = fit.gam.boot)
    boot.prediction.ridge <- prediction.ridge(f1, single.fit = fit.ridge.boot)
    
    # Hitung performa pada bootstrap sample
    ols.boot <- calculate_performance(f1$y, boot.prediction.ols)
    gam.boot <- calculate_performance(f1$y, boot.prediction.gam)
    ridge.boot <- calculate_performance(f1$y, boot.prediction.ridge)
    # predict in test data
    f2 <- as.data.frame(do.call(cbind, lapply(imputed1[[i]], function(x)
    {as.numeric(as.character(x))})))
    
    # Prediksi pada data original (test performance)
    test.prediction.ols <- prediction.ols(f2, single.fit =
                                            regression.splines.boot)
    test.prediction.gam <- prediction.gam(f2, single.fit = fit.gam.boot)
    test.prediction.ridge <- prediction.ridge(f2, single.fit =
                                                fit.ridge.boot)
    
    # Hitung performa pada data original
    ols.test <- calculate_performance(f2$y, test.prediction.ols)
    gam.test <- calculate_performance(f2$y, test.prediction.gam)
    ridge.test <- calculate_performance(f2$y, test.prediction.ridge)
    
    # Hitung optimism (selisih performa bootstrap dan test)
    optimism.ols.eachbootstrap[j,] <- ols.boot - ols.test
    optimism.gam.eachbootstrap[j,] <- gam.boot - gam.test
    optimism.ridge.eachbootstrap[j,] <- ridge.boot - ridge.test
  }
  
  # Hitung rata-rata optimism untuk setiap imputasi
  optimism.ols[i,] <- apply(optimism.ols.eachbootstrap, 2, mean)
  optimism.gam[i,] <- apply(optimism.gam.eachbootstrap, 2, mean)
  optimism.ridge[i,] <- apply(optimism.ridge.eachbootstrap, 2, mean)
  print(paste0("Imputasi selesai: ", i))
}

# Hitung rata-rata optimism dari seluruh imputasi
mean.optimism.ols <- apply(optimism.ols, 2, mean)
mean.optimism.gam <- apply(optimism.gam, 2, mean)
mean.optimism.ridge <- apply(optimism.ridge, 2, mean)

# Hitung performa terkoreksi optimism
optimism.corrected.ols <- apparent.ols - mean.optimism.ols
optimism.corrected.gam <- apparent.gam - mean.optimism.gam
optimism.corrected.ridge <- apparent.ridge - mean.optimism.ridge
round(rbind(optimism.corrected.ols, optimism.corrected.gam, optimism.corrected.ridge), digits=2)


##### INTERNAL-EXTERNAL CROSS VALIDATION #####
# Validasi internal menggunakan metode cross-validation berbasis cluster
# Membagi data berdasarkan cluster untuk menilai performa model pada subset populasi yang berbeda

# Mendapatkan nilai unik cluster
clusters <- unique(data.cont$clust)
N.clust <- length(clusters) # 5 clusters in this example

# Membuat list kosong untuk menyimpan data training dan validasi
data.in <- data.leftout <- list()

# Membagi data berdasarkan cluster untuk internal-external validation
for(i in 1:N.clust){
  # Data training: semua data kecuali cluster ke-i
  data.in[[i]]<-data.cont[data.cont$clust!=clusters[i],]
  
  # Data validasi: hanya cluster ke-i
  data.leftout[[i]]<-data.cont[data.cont$clust==clusters[i],]
  
  # Hapus baris dengan missing value pada data validasi
  complete.index <- complete.cases(data.leftout[[i]][,c(paste0("x", 1:5), "y")])
  data.leftout[[i]] <- data.leftout[[i]][complete.index,]
}

#impute the data and fit the model
n.impute <- 10
imputed <- regression.splines.CV <- fit.gam.CV <- fit.ridge.CV <- list()
leftout.prediction.ols <- leftout.prediction.gam <- leftout.prediction.ridge <- list()
leftout.performance.ols <- leftout.performance.gam <- leftout.performance.ridge <- list()

for (i in 1:N.clust){
  a <- aregImpute(
                  data=data.in[[i]],
                  I(y)~x1+x2+I(x3)+I(x4)+I(x5),
                  n.impute=n.impute,
                  nk=3,
                  match='closest'
                  )
  
  for (j in 1:n.impute){
    imputed[[j]] <- impute.transcan(
                                    a,
                                    imputation=j,
                                    data=data.in[[i]],
                                    list.out=TRUE,
                                    pr=FALSE,
                                    check=FALSE)
    regression.splines.CV[[j]] <- ols(
                                      y~rcs(x1,3)+rcs(x2,3)+x3+x4+x5,
                                      data=imputed[[j]])
    fit.gam.CV[[j]] <- gam(
                          y ~ x3+x4+x5+s(x1)+s(x2),
                          data = imputed[[j]])
    dfSplined.x1 <- as.data.frame(predict(bsMat.x1, imputed[[j]]$x1))
    dfSplined.x2 <- as.data.frame(predict(bsMat.x2, imputed[[j]]$x2))
    imp <- cbind(imputed[[j]]$y, imputed[[j]]$x3, imputed[[j]]$x4,
                 imputed[[j]]$x5, dfSplined.x1, dfSplined.x2)
    colnames(imp) <- c("y", "x3", "x4", "x5", paste0("V",
                                                     as.character(1:(length(colnames(imp))-4))))
    data_glmnet <- model.matrix(y ~ ., data = imp)
    X <- as.matrix(data_glmnet[,-1])
    colnames(X)[1:2] <- c("x3", "x4")
    Y <- imp$y
    cvfit <- cv.glmnet(X,Y,family = "gaussian", alpha=0,
                       lambda = lambdas, nfolds=10)
    lambda.min <- cvfit$lambda.min
    fit.ridge.CV[[j]] <- glmnet(X,Y,family = "gaussian", alpha=0, lambda =
                                  lambda.min)
  }
  leftout.prediction.ols[[i]] <- prediction.ols(data.leftout[[i]],
                                                multiple.fit = regression.splines.CV)
  leftout.prediction.gam[[i]] <- prediction.gam(data.leftout[[i]],
                                                multiple.fit = fit.gam.CV)
  leftout.prediction.ridge[[i]] <- prediction.ridge(data.leftout[[i]],
                                                    multiple.fit = fit.ridge.CV)
  leftout.performance.ols[[i]] <-
    calculate_performance(data.leftout[[i]]$y, leftout.prediction.ols[[i]])
  leftout.performance.gam[[i]] <-
    calculate_performance(data.leftout[[i]]$y, leftout.prediction.gam[[i]])
  leftout.performance.ridge[[i]] <-
    calculate_performance(data.leftout[[i]]$y, leftout.prediction.ridge[[i]])
}
# performance per cluster
#leftout.performance.ols
#leftout.performance.gam
#leftout.performance.ridge

# performance aggregating all clusters
leftout.prediction.ols <- do.call(c, leftout.prediction.ols)
leftout.prediction.gam <- do.call(c, leftout.prediction.gam)
leftout.prediction.ridge <- do.call(c, leftout.prediction.ridge)

IECV.observed <- do.call(rbind, data.leftout)$y
IECV.cluster <- do.call(rbind, data.leftout)$clust

IECV.ols <- calculate_performance(IECV.observed, leftout.prediction.ols)
IECV.gam <- calculate_performance(IECV.observed, leftout.prediction.gam)
IECV.ridge <- calculate_performance(IECV.observed, leftout.prediction.ridge)

round(rbind(IECV.ols, IECV.gam, IECV.ridge), digits=2)

###### PLOT EXTERNAL & INTERNAL VALIDATION ####
p4<-ggplot(IECV,
            aes(x=leftout.prediction.ols, y=IECV.observed, color=cluster))+
            geom_point(size=2)+
            geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed", size=0.5)+
            xlim(-4,4)+ylim(-4,4) + theme(legend.position = "none")+
            geom_smooth(method=lm, se=F) +ylab("observed")
p5<-ggplot(IECV,
            aes(x=leftout.prediction.gam, y=IECV.observed, color=cluster))+
            geom_point(size=2)+
            geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed", size=0.5)+
            xlim(-4,4)+ylim(-4,4) + theme(legend.position = "none")+
            geom_smooth(method=lm, se=F) +ylab("observed")
p6<-ggplot(IECV,
            aes(x=leftout.prediction.ridge, y=IECV.observed, color=cluster))+
            geom_point(size=2)+
            geom_abline(intercept = 0, slope = 1, color="black",linetype="dashed", size=0.5)+
            xlim(-4,4)+ylim(-4,4)+geom_smooth(method=lm, se=F) +ylab("observed")

library(ggpubr)
ggarrange(p4, p5, p6, ncol=3, common.legend = TRUE, legend="bottom")


##### FIND CLUSTER SPECIFIC PERFORMANCE #####
per.cluster.ols=matrix(leftout.performance.ols[[1]], nrow=1)
for(i in 2: N.clust){
  per.cluster.ols=rbind(per.cluster.ols,
                        matrix(leftout.performance.ols[[i]], nrow=1))}
per.cluster.ols=data.frame(per.cluster.ols)
colnames(per.cluster.ols)=c("MAE", "MSE", "R2")
rownames(per.cluster.ols)=paste("cluster",clusters,":")
round(per.cluster.ols, digits=2)
