N=400
age=runif(N,30,80)
En=-0.03*(age-30)
C=rep(NA,N)
for(i in 1:N) C[i]=rbinom(1,1,1/(1+exp(En)))
write.table(data.frame(Age=age,C=C),file="data1_script3.dat")
