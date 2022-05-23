welch.df <- function(x1,x2){
  var1 = var(x1)
  var2 = var(x2)
  n1 = length(x1)
  n2 = length(x2)
  adj.df = (var1/n1 + var2/n2)^2/((var1/n1)^2/(n1-1) + (var2/n2)^2/(n2-1))
  return(adj.df = adj.df)
}

cu.ttest2 <- function(x1,x2){
  ##
  # x1,x2  標本群
  # mean  平均
  # var  分散
  # n  標本数
  
  mean1 = mean(x1)
  mean2 = mean(x2)
  diff = mean1 - mean2
  var1 = var(x1)
  var2 = var(x2)
  n1 = length(x1)
  n2 = length(x2)
  df = n1 + n2 - 2
  df.W = welch.df(x1,x2)
  
  t = (mean1 - mean2)/sqrt(var1/n1 + var2/n2)
  p = (1-pt(q=abs(t),df=df))*2
  p.W = (1-pt(q=abs(t),df=df.W))*2
  q95 = qt(c(0.025,0.975), df=df)
  q95.W = qt(c(0.025,0.975), df=df.W)
  ci95 = q95*sqrt(var1/n1 + var2/n2)+diff
  ci95.W = q95.W*sqrt(var1/n1 + var2/n2)+diff
  
  what_is_tested = paste("Two sample t-test (equal & non-equal variances) \n",
                         "Ho: difference in means = 0 \n",
                         "Ha: difference in means != 0 \n\n")
  result = data.frame(mean.G1 = c(mean1,NA),
                      mean.G2 = c(mean2,NA),
                      diff = c(diff, NA),
                      t = c(t,NA),
                      df = c(df,df.W),
                      pVal = c(p,p.W))
  row.names(result) <- c("equal var", "non-equal var")
  ci = data.frame(CI95_lower = c(ci95[1],ci95.W[1]),
                  estimate = c(diff, diff),
                  CI95_upeer = c(ci95[2],ci95.W[2]))
  row.names(ci) <- c("equal var", "non-equal var")
  
  boxplot(x1,x2,col=c(2,4))
  cat(what_is_tested)
  cat("test statistics \n")
  round(result,4) %>% as.matrix() %>% print(na.print="")
  cat("\n95% confidence interval \n")
  round(ci,4) %>% as.matrix() %>% print(na.print="")
}

#set.seed(444)
#nsubj = 15
#x1 <- rnorm(n = nsubj, mean = 25.5, sd =2) %>% round()
#x2 <- rnorm(n = nsubj, mean = 24, sd =2) %>% round()
#x3 <- rnorm(n = nsubj, mean = 25, sd =1) %>% round()
#cu.ttest2(x1,x3)