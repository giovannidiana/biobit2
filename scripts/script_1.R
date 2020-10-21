# Estimation of mean and variance from univariate normally distributed data

datatable<-read.table("data1_script1.dat")

model="model {
         
         for(i in 1:N){
			      X[i] ~ dnorm(m,t)
         }

         m ~ dt(2,2,1)
         t ~ dgamma(1,1)		               
		          
}"

data=list(X=datatable$X,N=nrow(datatable));
varnames=c("m","t")
burn_in=100;
steps=10000;
thin=1;

library(rjags)
fileConn=file("model.tmp")
writeLines(model,fileConn);
close(fileConn)

m=jags.model(file="model.tmp",data=data);
update(m,burn_in)
draw=jags.samples(m,steps,thin=thin,variable.names=varnames)

