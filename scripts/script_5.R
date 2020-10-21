model1="model {
         
		 pB ~ dbeta(1,1)
         
		 n ~ dbinom(pB,Ntot)
		 	  
         
}"

model2="model {
         
		 for(i in 1:3){
			 pB[i] ~ dbeta(alpha,beta)
		 }
         
	     alpha ~ dexp(0.001)
		 beta ~ dexp(0.001)
		 
         for(j in 1:3){
			  n[j] ~ dbinom(pB[j],Ntot[j])
		 }	  
         
}"

n=c(10,25,35)
Ntot=c(100,100,100)
data1=list(n=sum(n),Ntot=sum(Ntot));
data2=list(n=n,Ntot=Ntot);
varnames1=c("pB")
varnames2=c("pB","alpha","beta")
burn_in=1000;
steps=100000;
thin=1;

library(rjags)
fileConn=file("model1.tmp")
writeLines(model1,fileConn);
close(fileConn)

fileConn=file("model2.tmp")
writeLines(model2,fileConn);
close(fileConn)

m1=jags.model(file="model1.tmp",data=data1);
m2=jags.model(file="model2.tmp",data=data2);
update(m1,burn_in)
update(m2,burn_in)
draw1=jags.samples(m1,steps,thin=thin,variable.names=varnames1)
draw2=jags.samples(m2,steps,thin=thin,variable.names=varnames2)


