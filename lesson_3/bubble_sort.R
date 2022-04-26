# bubble sort
set.seed(99)
vec = sample(1:15)

len_vec = length(vec)
for(loop1 in 1:(len_vec - 1)){
  for(loop2 in 2:(len_vec - loop1 + 1)){
    if(vec[loop2] < vec[(loop2-1)]){
      temp_num = vec[loop2]
      vec[loop2] = vec[(loop2-1)]
      vec[(loop2-1)] = temp_num
    }
  }
  print(paste("result after loop1 = ", loop1))
  print(vec)
  if(all(vec==c(1:15))){
    break
  }
}