# practice 4.2
# function of estimated means
# N - runif trial
# M - number of loops
# O/P - min/max of runif

CLT_example <- function(N,M,O,P){
  means = rep(0,M)
  for(i in 1:M){
    dat <- runif(N,min=O,max=P)
    means[i] <- mean(dat)
  }
  hist(means, xlab="Estimated means", probability=T,
       breaks=30, xlim=c(O,P))
  x.temp = seq(O,P,length.out=1000)
  dens <- dnorm(x.temp, mean = (O+P)/2, sd=((P-O)/sqrt(12*N)))
  lines(x.temp, dens, col = "red", lwd = 3)
}

CLT_example(50,10000,1,10)