model="model {
         
		 n[1:4] ~ dmulti(u,sample_size)
         
		 u ~ ddirich(alpha)
		 	  
         
}"

n=c(40,5,20,35)
data=list(n=n,sample_size=sum(n),alpha=rep(10,4));
varnames=c("u")
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

get_cond <- function(){
	c1=draw$u[1,,1]/(draw$u[1,,1]+draw$u[3,,1])
	c2=draw$u[2,,1]/(draw$u[2,,1]+draw$u[4,,1])
	return(list(c1,c2))
}

gen_plot <- function(){
	cond=get_cond();
	hist(cond[[1]],breaks=seq(0,1,0.01),freq=F,
		 xlab="",ylab="",
		 main="conditional probabilities",
		 cex.axis=2)
	hist(cond[[2]],breaks=seq(0,1,0.01),add=T,col="red",freq=F)
	legend("topright", legend=c("P(sick|A)", "P(sick|B)"),fill=c("black","red"),cex=1.5)
}

gen_box<- function() {
	c1=draw$u[1,,1]/(draw$u[1,,1]+draw$u[3,,1])
	c2=draw$u[2,,1]/(draw$u[2,,1]+draw$u[4,,1])
    
	par(mar=c(5,5,2,2))
	boxplot(data.frame(A=c1,B=c2),outline=F,ylab="P(sick| *)",cex.lab=1.5,cex.axis=1.5);
}

gen_cumul<-function() {
	c1=draw$u[1,,1]/(draw$u[1,,1]+draw$u[3,,1])
	c2=draw$u[2,,1]/(draw$u[2,,1]+draw$u[4,,1])
	plot(ecdf(c1-c2), main="cumulative distribution of the difference",xlab="P(sick|A)-P(sick|B)",ylab="probability",cex.lab=1.5,cex.axis=1.5)
}
