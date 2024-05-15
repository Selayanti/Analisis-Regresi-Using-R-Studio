regresi=function(X,y)
{
  n =length(y)
  p =ncol(X)
  bo=rep(1,n)
  X = cbind(bo,X)
  X =as.matrix(X)
  
  # Step 1: Menghitung nilai beta
  beta=solve(t(X)%*%X)%*%t(X)%*%y
  
  # Step 2: Menghitung nilai sigma (MSE)
  H = X%*%solve(t(X)%*%X)%*%t(X)
  I = diag(rep(1,n))
  SSE = t(y)%*%(I-H)%*%y
  db = n-p-1
  MSE = SSE/db
  
  # Step 3: Menghitung RSquare
  J = matrix(rep(1,n*n),ncol=n)
  SST = t(y)%*%(I-(1/n)*J)%*%y
  Rsquare=1-(SSE/SST)
  
  # Step 4: Menghitung nilai prediksi dan Error
  ytopi = X%*%beta
  error = y-ytopi
  Hasil = cbind(y,ytopi,error)
  colnames(Hasil)=c("y","yhat","Error")
  
  #Uji normalitas residual Syarat 1
  error = y-X%*%beta
  data_uji = error
  a = mean(data_uji)
  b = sd(data_uji)
  uji_norm_error = ks.test(data_uji, 'pnorm', a, b)
  PV_1 = uji_norm_error$p.value
  ket_1 = ifelse(PV_1>=0.05, "Data Berdistribusi Normal", "Data tidak berdistribusi Normal")
  
  #Uji kelinearan Syarat 2
  z = as.matrix(x)
  uji_linear = resettest(y~z)
  PV_2 = uji_linear$p.value
  ket_2 = ifelse(PV_2>=0.05, "Hubungan antara variabel x dan y adalah hubungan linear", "Hubungan antara variabel x dan y adalah hubungan non linear")
  #jika p value > 0.05 maka data tidak linear 
  
  #Uji homoskedastisitas Syarat 3
  model = lm(y~z)
  uji_homoskedastisitas = bptest(model)
  PV_3 = uji_homoskedastisitas$p.value
  ket_3 = ifelse(PV_3>=0.05, "Variansi error ber Homoskedastisitas", "Variansi error tidak ber Homoskedastisitas")
  #jika p value > 0.05 terdapat homoskedastisitas/ragam sisaan homogen 
  
  #Auto korelasi Syarat 4
  model = lm(y~z)
  uji_autokorelasi = bgtest(model)
  PV_4 = uji_autokorelasi$p.value
  ket_4 = ifelse(PV_4>=0.05, "Antar nilai error tidak terjadi korelasi", "Antar nilai error terjadi korelasi")
  #jika nilai pvalue > 0.05 terjadi autokorelasi 
  
  # Step 5: Uji Kecocokan Model (Uji Serentak)
  SSR=SST-SSE
  MSR=SSR/p
  Fhit=MSR/MSE
  sig_F=1-pf(Fhit,p,db)
  Fstat=cbind(Fhit,sig_F)
  colnames(Fstat)=c("F_hit","Sig")
  
  # Step 6: Uji Parsial Parameter regresi
  SE_beta=sqrt(diag(solve(t(X)%*%X))*as.vector(MSE))
  t_hit=(1/SE_beta)*beta
  sig_t=2*(1-pt(abs(t_hit),n-p-1))
  T_stat=cbind(beta,SE_beta,t_hit,sig_t)
  colnames(T_stat)=c("beta","SE_beta","t_hit","Sig")
  
  # Step 7: Interval Konfidensi (100-alpha)% untuk beta
  alpha=0.05
  tmin=qt(alpha/2,n-p-1)
  tmax=qt(1-alpha/2,n-p-1)
  beta_min=beta+SE_beta*tmin
  beta_max=beta+SE_beta*tmax
  int_beta=cbind(beta_min,beta_max)
  colnames(int_beta)=c("Lower","Upper")
  
  cat("⬤ uji normalitas =\n")
  if(PV_1>=alpha){
    print(uji_norm_error)
    print(ket_1)
    cat("\n")
    
    cat("⬤ uji linear =\n") 
    if(PV_2>=alpha){
      print(uji_linear)
      print(ket_2)
      cat("\n")
      
      cat("⬤ uji_homoskedastisitas =\n")
      if(PV_3>=alpha){
        print(uji_homoskedastisitas)
        print(ket_3)
        cat("\n")
        
        cat("⬤ uji_autokorelasi =\n")
        if(PV_4>=alpha){
          print(uji_autokorelasi)
          print(ket_4)
          cat("\n")
          
          cat("⬤ Coefficients=\n")
          print(beta)
          
          cat("⬤ Nilai MSE =\n")
          print(MSE)
          
          cat("⬤ Nilai RSquare =\n")
          print(Rsquare)
          
          cat("⬤ Hasil Regresi =\n")
          print(Hasil)
          
          cat("⬤ Uji Hipotesis Kecocokan Model Regresi =\n")
          print(Fstat)
          
          cat("⬤ Uji Hipotesis Parameter Model Regresi =\n")
          print(T_stat)
          
          cat("⬤ Interval Konfidensi Beta =\n")
          print(int_beta)
          
  #Simpan Output
          return(list(Coeff = beta, Residual = error, Fitted = ytopi, MSE = MSE, Rsquare = Rsquare))
        }
        else("Data tidak dapat di uji regresi karena tidak memenuhi syarat non autokorelasi")
      }
      else("Data tidak dapat di uji regresi karena tidak memenuhi syarat Homoskedastisitas")
    }
    else("Data tidak dapat di uji regresi karena tidak memenuhi syarat Linearitas")
  }
  else("Data tidak dapat di uji regresi karena tidak memenuhi syarat Normalitas")
}

#INPUT DATA KE-1
setwd("E:/SELA/SEMESTER 2/STATKOM/TUGAS")
data_1 = read.delim('file tugas day 9.txt')
data_1

#PER KOLOM
x = data_1[,c(2,3)]
x
y = data_1[,c(4)]
y

#PEMBULATAN BILANGAN DESIMAL
round(y)

#MENCARI NILAI REGRESI
regresi(x,y)

#INPUT DATA KE-2
setwd("E:/SELA/SEMESTER 2")
data_2 = read.delim('persentase.txt')
data_2

#PER KOLOM
x = data_2[,c(3,4)]
x
y = data_2[,c(2)]
y

#PEMBULATAN BILANGAN DESIMAL
round(y)

#MENCARI NILAI REGRESI33
regresi(x,y)


