# Logistic inference 
# En=-0.001*(1.0*(vol-1260)-1.2*age)

datatable<-read.table("data1_script3.dat")

model="model {
         
     for(i in 1:N){
		     p[i] = 1/(1+exp(alpha*A[i]))
			   #status[i] ~ dbern(   1/(1+exp(alpha*A[i]))  )
         status[i] ~ dbern(p[i])
         
	 }
      
	 alpha ~ dnorm(0,0.001) 
 		          
}"

data=list(A=datatable$Age,status=datatable$C,N=nrow(datatable));
varnames=c("alpha")
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
