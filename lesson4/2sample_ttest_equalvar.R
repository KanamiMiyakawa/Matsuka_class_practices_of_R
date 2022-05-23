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
  
  t = (mean1 - mean2)/sqrt(var1/n1 + var2/n2)
  p = (1-pt(q=abs(t),df=df))*2
  q95 = qt(c(0.025,0.975), df=df)
  ci95 = q95*sqrt(var1/n1 + var2/n2)+diff
  
  what_is_tested = paste("Two sample t-test \n",
                         "Ho: difference in means = 0 \n",
                         "Ha: difference in means != 0 \n\n")
  cat("\ntest statistics")
  result = data.frame(mean.G1 = mean1,
                      mean.G2 = mean2,
                      t = t,
                      df = df,
                      pVal = p)
  ci = data.frame(CI95_lower = ci95[1],
                  estimate = diff,
                  CI95_upeer = ci95[2])
  
  cat(what_is_tested)
  round(result,4) %>% print(row.names = F)
  cat("\n95% confidence interval \n")
  round(ci,4) %>% print(row.names = F)
}

#set.seed(444)
#nsubj = 15
#x1 <- rnorm(n = nsubj, mean = 25.5, sd =2) %>% round()
#x2 <- rnorm(n = nsubj, mean = 24, sd =2) %>% round()
#x3 <- rnorm(n = nsubj, mean = 25, sd =1) %>% round()
#cu.ttest2(x1,x2)