SplitAdr <- function(adr){
  x <- c()
  impair <- c(1,3,5,7)
  for (i in impair) {
    x <- append(x,substr(adr,i,i+1))
  }
  return(x)
}

Convert <- function(addr, base){
  x <- c()
  impair <- c(1,3,5,7)
  for (i in impair) {
    x <- append(x,substr(addr,i,i+1))}
  if(base==16){return(paste0(x,collapse = '.'))}
  else{x<- strtoi(x,16)
  return(paste0(x,collapse = '.'))}
}

InetC <- function(addr){
  x <- SplitAdr(addr)
  x <- strtoi(x,16)
  if(x[[1]]>=0 && x[[1]]<= 127){return('A')}
  if(x[[1]]>=128 && x[[1]]<= 191){return('B')}
  if(x[[1]]>=192 && x[[1]]<= 223){return('C')}
  if(x[[1]]>=224 && x[[1]]<= 239){return('D')}
  if(x[[1]]>=240 && x[[1]]<= 255){return('E')}
}

InetC('C0290614')
InetC('863B83AC')
InetC('C0A80001')
InetC('F0040506')

Netid <- function(addr){
  x <- SplitAdr(addr)
  y <- c()
  if(InetC(addr) == 'A'){
    y[1] <- paste0(x[1],collapse = '')
    y[2] <- paste0(x[2],x[3],x[4],collapse = '')
  }
  if(InetC(addr) == 'B'){
    y[1] <- paste0(x[1:2],collapse = '')
    y[2] <- paste0(x[3:4],collapse = '')
  }
  if(InetC(addr) == 'C'){
    y[1] <- paste0(x[1],x[2],x[3],collapse = '')
    y[2] <- paste0(x[4],collapse = '')}
  if(InetC(addr) == 'D' || InetC(addr) == 'E'){
    y[1] <- paste0(x,collapse = '')
    y[2] <- paste0(x,collapse = '')}
  if(InetC(addr) == 'F'){
    y[1] <- paste0(x,collapse = '')
    }
  return(paste0(y,collapse =' '))
}


