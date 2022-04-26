m <- (10/16)*2^32
num <- table(sample(m, 1e6, replace=T)%%10)
num.prob <- prop.table(num)
num
num.prob

barplot(num, main="乱数生成器の結果")
