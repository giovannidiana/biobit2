angle_dist<-read.table("data1_script7.dat")
sample=F
model="model {
        

		 for(i in 1:N){
			 angle[i] ~ dnorm(mu[ct[i]+1],precision[ct[i]+1])
		 }

# PRIORS
         for(i in 1:N){
			 ct[i] ~ dbin(0.5,1)
		 }

		 mu[1] ~ dnorm(0,0.001)
		 mu[2] ~ dnorm(0,0.001)
         
		 precision[1] ~ dgamma(2,0.01)
		 precision[2] ~ dgamma(2,0.01)
}"

data=list(angle=angle_dist$angle, N=nrow(angle_dist));
varnames=c("ct","mu","precision")
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

get_params <- function(){
	means=rep(0,2);
	sigmas=rep(0,2);
	means[1]=mean(draw$mu[1,,1])
	means[2]=mean(draw$mu[2,,1])
	sigmas[1]=sqrt(mean(1./draw$precision[1,,1]))
	sigmas[2]=sqrt(mean(1./draw$precision[2,,1]))
	rho=mean(apply(draw$ct[,,1],2,function(x) { sum(x==1)/3000}))
	
	return(list(mu=means,sd=sigmas,prob=rho))
}

plot_params <- function(){
	params<-get_params();
	hist(angle_dist$angle,freq=F,ylim=c(0,0.017),xlab="angle",
	     cex.lab=1.5,cex.axis=1.5,main="")
	arange=seq(0,130,0.01);
	lines(arange,(1-params$prob)*dnorm(arange,params$mu[1],params$sd[1]),col="red")
	lines(arange,params$prob*dnorm(arange,params$mu[2],params$sd[2]),col="green")

}	

plot_stat <- function(){
	layout(matrix(1:2,ncol=2))

	boxplot(t(draw$mu[,200:1000,1]),main="group means")
	boxplot(t(sqrt(1/draw$precision[,200:1000,1])),main="group sd")
}

