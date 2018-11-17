
## General procedure:
#generate a vector of 0s and 1s with 1s only on the first positions of sequences
#relevant for the analysis (with >5 number of zeros in between) to use then in cumsum function
children_function<-function(x){
  #Figure out where 1s are located:
  one_pos<-which(rle(x)$values==1 %in% TRUE)
  #Find their concrete positions
  positions<-vector(length = length(one_pos))
  for (i in 1:length(one_pos)){
    positions[i]<-sum(rle(x)$lengths[1:one_pos[i]-1])+1
  }
  ## 0s examination
  
  #find positions of 0s with <5 length
  small_zeroes<-which(rle(x)$length<5&rle(x)$values==0 %in% TRUE)
  #remove position of first zero sequence as there's no criteria for it
  small_zeroes<-small_zeroes[small_zeroes>1] #positions of sequences
  #find positions of first sequence zeros
  
  small_positions<-vector(length = length(small_zeroes))
  for (i in 1:length(small_zeroes)){
    small_positions[i]<-sum(rle(x)$lengths[1:small_zeroes[i]-1])+1
  }
  #find length of all 0 sequences
  len_zeros<-rle(x)$length[rle(x)$values==0]
  #remove length of 1st position if it is less than 5
  #this is made for not including it in criteria analysis
  if(len_zeros[1]<5){
    len_zeros1<-len_zeros[2:length(len_zeros)]
  }
  #remove length of 0 sequences <5
  len_zeros1<-len_zeros1[len_zeros1<5] #length of 0 sequences <5
  
  #find 1s positions not under 0>5 criteria
  
  positions_to_delete<-small_positions+len_zeros1
  
  #remove positions not under 0 length criteria
  positions<-positions[!positions %in% positions_to_delete]
  #create a vector of 0s and 1s with 1s only on starting positions of relevant sequences of 1s
  l_vec<-sum(rle(x)$length) 
  output<-vector(length=l_vec)
  for (i in 1:l_vec){
    if (i %in% positions==TRUE){
      output[i]<-1
    }
    else{
      output[i]<-0
    }
  }
  #use cumsum to obtain final output
  output<-cumsum(output)
  output
}

children_function(c(0,0,0,1,1,1,0,0))

