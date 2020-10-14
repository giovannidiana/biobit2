tab<-read.table("~/Lectures/Rintro/20200210c3_amplitude_variance_trains.csv",header=T)
myfun <- function(t,u) dgamma(t %% 10000 ,10,5e-2)+rnorm(length(t),0,u)

dt <- data.frame(time=tab[,1], amplitude=myfun(tab[,1],0.005),baseline_var=runif(n=nrow(tab)))
write.table(dt,file="table.dat",row.names=F)
