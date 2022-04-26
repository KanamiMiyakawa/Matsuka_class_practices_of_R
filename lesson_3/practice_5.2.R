#実習5.2
COV_example <- function(x.pre,y.pre,z.pre,alpha.pre,beta.pre,trial){

  #未感染者の初期値
  x0 = x.pre
  #感染者の初期値
  y0 = y.pre
  #脱感染者の初期値
  z0 = z.pre
  #時間変化量
  dt = 0.01
  #繰り返し回数
  T = trial
  #感染係数
  alpha = alpha.pre
  #脱感染係数
  beta = beta.pre
  #未感染者、感染者、脱感染者の推移ベクトル
  x=y=z=rep(0,T)
  x[1] = x0
  y[1] = y0
  z[1] = z0
  
  for(t in 1:(T-1)){
    x[t+1] = x[t] - (alpha*x[t]*y[t])*dt
    y[t+1] = y[t] + (alpha*x[t]*y[t]-beta*y[t])*dt
    z[t+1] = z[t] + (beta*y[t])*dt
  }

  plot(x=1:T,x,type="l",col=2,lwd=3,ylim=c(0,x0))
  lines(x=1:T,y,col=3,lwd=3)
  lines(x=1:T,z,col=4,lwd=3)

}

COV_example(1e5,10,0,1e-5*0.2,0.1,10000)
