# じゃんけん関数の作成実習

jankenHoi <- function(){
  # function for 2-player janken
  # input arg - none
  # output - winner : winning player
  #          winTE : winnig "TE"
  winner = "aiko"
  while(winner == "aiko"){
    print("janken!")
    te = sample(c("G","C","P"),2,replace = T)
    
    if(te[1]==te[2]){
      winner = "aiko"
    }else{
      if((te[1]=="G"&te[2]=="C")|
         (te[1]=="C"&te[2]=="P")|
         (te[1]=="P"&te[2]=="G")){
        winner = "player1"
        idx = 1
      }else{
        winner = "player2"
        idx = 2
      }
    }
  }
  return(list(winner = winner, winTE = te[idx]))
}


acchimuiteHoi <-function(){
  # function for acchimuite-hoi
  # input arg - none
  # output - result of acchimuite-hoi
  #          "GO" - game is over
  #          "CONT" - game continues
  print("attimuite hoi!")
  yubi = sample(c("N","E","W","S"),1)
  kao = sample(c("N","E","W","S"),1)
  
  condition = ifelse(yubi==kao,"GO","CONT")
  return(condition)
}

JA_hoi <- function(){
  AH ="CONT"
  while(AH == "CONT"){
    janken <- jankenHoi()
    AH <- acchimuiteHoi()
  }
  print(paste("the winner is", janken$winner))
}

JA_hoi()