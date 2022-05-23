welch.df <- function(x1,x2){
  var1 = var(x1)
  var2 = var(x2)
  n1 = length(x1)
  n2 = length(x2)
  adj.df = (var1/n1 + var2/n2)^2/((var1/n1)^2/(n1-1) + (var2/n2)^2/(n2-1))
  return(adj.df = adj.df)
}

cu.ttestR <- function(x1, x2 = 0, labels){
  # x2の長さが1の数値ならone sample t-test、それ以外ならtwo sample t-test
  type = ifelse(length(x2)==1, "one","two")
  
  ##
  # x1,x2  標本群
  # labels  変数名、グループ名１、グループ名２
  # mean  平均
  # var  分散
  # n  標本数
  
  mean1 = mean(x1)
  mean2 = mean(x2)
  diff = mean1 - mean2
  var1 = var(x1)
  var2 = ifelse(type=="one",0,var(x2))
  n1 = length(x1)
  n2 = length(x2)
  df = n1 + n2 - 2
  
  # ウェルチを使用しない値計算
  t = (mean1 - mean2)/sqrt(var1/n1 + var2/n2)
  p = (1-pt(q=abs(t),df=df))*2
  q95 = qt(c(0.025,0.975), df=df)
  result = data.frame(diff = diff,
                      t = t,
                      df = df,
                      pVal = p)
  
  if (type == "two"){
    
    boxplot(x1, x2, col = c(2,4), ylab = labels[1], xaxt = "n")
    axis(1, at = c(1,2), c(labels[2],labels[3]))
    
    # ウェルチを使用した値計算
    df.W = welch.df(x1,x2)
    p.W = (1-pt(q=abs(t),df=df.W))*2
    q95.W = qt(c(0.025,0.975), df=df.W)
    ci95 = q95*sqrt(var1/n1 + var2/n2)+diff
    ci95.W = q95.W*sqrt(var1/n1 + var2/n2)+diff
    
    # データまとめ
    ci = data.frame(CI95_lower = c(ci95[1],ci95.W[1]),
                    diff_mean = c(diff, diff),
                    CI95_upper = c(ci95[2], ci95.W[2]))
    result = rbind(result, c(NA, NA, df.W, p.W))
    row.names(result) <- c("equal var", "non-equal var")
    row.names(ci) <- c("equal var", "non-equal var")
    
    what_is_tested = paste("Two sample t-test (equal & non-equal variances) \n",
                           "Ho: difference in means = 0 \n",
                           "Ha: difference in means != 0 \n\n")
    cat(what_is_tested)
    cat("test statistics \n")
    round(result,4) %>% as.matrix() %>% print(na.print="")
    cat("\n95% confidence interval \n")
    round(ci,4) %>% as.matrix() %>% print(na.print="")
    
  }else{
    
    boxplot(x1, col=4, ylab=labels[1])
    ci95 = q95*sqrt(var1/n1)+mean1
    ci = data.frame(CI95_lower = ci95[1],
                    mean_of_x = mean1,
                    CI95_upper = ci95[2])
    what_is_tested = paste("One sample t-test \n",
                           "Ho: mean of x =", mean2, "\n",
                           "Ha: mean of x !=", mean2, "\n\n")
    cat(what_is_tested)
    cat("test statistics \n")
    round(result,4) %>% print(row.names=F)
    cat("\n95% confidence interval \n")
    round(ci,4) %>% print(row.names=F)
    
  }
  
}

#set.seed(444)
#nsubj = 15
#x1 <- rnorm(n = nsubj, mean = 25.5, sd =2) %>% round()
#x2 <- rnorm(n = nsubj, mean = 24, sd =2) %>% round()
#x3 <- rnorm(n = nsubj, mean = 25, sd =1) %>% round()
#resTWO <- cu.ttestR(x1, x2, c("Shoe size", "Group 1", "Group 2"))

