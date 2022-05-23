cu.ttest <- function(x, mu){
  ##
  # x  標本群
  # mu  想定される母平均
  # mean  標本平均
  # sd  標準偏差
  # n  標本数
  # df  自由度
  
  boxplot(x, col=4)
  mean.x = mean(x)
  sd.x = sd(x)
  n.x = length(x)
  df.x = n.x - 1
  
  diff = mean.x - mu
  t.x = (mean.x - mu)/(sd.x/sqrt(n.x))
  p.x = (1-pt(q = abs(t.x), df = df.x))*2
  q95 = qt(c(0.025, 0.975), df = df.x)
  ci95 = q95*(sd.x/sqrt(n.x)) + mean.x
  
  # 結果まとめ
  result = data.frame(mean_of_x = mean.x,
                      diff = diff,
                      t = t.x,
                      df = df.x,
                      pVal = p.x)
  ci = data.frame(CI95_lower = ci95[1],
                  mean_of_x = mean.x,
                  CI95_upper = ci95[2])
  
  # 表示
  what_is_tested = paste("One sample t-test \n",
                         "Ho: mean of x =", mu, "\n",
                         "Ha: mean of x !=", mu, "\n\n")
  
  cat(what_is_tested)
  cat("test statistics \n")
  round(result, 4) %>% print(row.names = F)
  cat("\n95% confidence interval \n")
  round(ci,4) %>% print(row.names = F)
  
  return(result = list(what_is_tested, result, ci))
}

# res <- cu.ttest(x1, 24)