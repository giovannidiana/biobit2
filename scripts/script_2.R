# Linear regression 
# Y=3.2*X+34+rnorm(n=120,mean=0,sd=2)

datatable<-read.table("data1_script2.dat")

model="model {
         
     for(i in 1:N){
			 Y[i] ~ dnorm(a*X[i]+b,t)
		 }
     a ~ dnorm(1,0.001)
		 b ~ dnorm(20,0.001)
     t ~ dgamma(1,1)		               
		          
}"

data=list(X=datatable$X,Y=datatable$Y,N=nrow(datatable));
varnames=c("a","b","t")
burn_in=1000;
steps=10000;
thin=1;

library(rjags)
fileConn=file("model.tmp")
writeLines(model,fileConn);
close(fileConn)

m=jags.model(file="model.tmp",data=data);
update(m,burn_in)
draw=jags.samples(m,steps,thin=thin,variable.names=varnames)
