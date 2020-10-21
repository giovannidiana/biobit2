
datatable<-read.table("data1_script6.dat")
model="model {

		 for(i in 1:N){
			 g[i] ~ dnorm(mu[trial[i]],precision[trial[i]])
		 }

         for(i in 1:K){
			 mu[i] ~ dnorm(mu0,1e-3)
			 precision[i] ~ dgamma(2,0.01)
		 }

		 mu0 ~ dnorm(0,1e-3)

}"

data=list(g=datatable$expression, trial=datatable$trial, N=nrow(datatable),K=max(datatable$trial));
varnames=c("mu","mu0")
burn_in=100;
steps=1000;
thin=1;

library(rjags)
fileConn=file("model.tmp")
writeLines(model,fileConn);
close(fileConn)

m=jags.model(file="model.tmp",data=data);
update(m,burn_in)
draw=jags.samples(m,steps,thin=thin,variable.names=varnames)

