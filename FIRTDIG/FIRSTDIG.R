FirstDigit <- function(n,base){
  if(n==0){return(0)}
  if(n<0){n <- (-1)*n}
  if(n>1 && n<base){return(floor(n))}
  if(n==base){return(1)}
  if(n<1 && n>0){while (n<1) {
    n <- n*base}}
  if(n>1 && n<base){return(floor(n))}
  while (n>base) {
    n <- n%/%base
  }
  if(n==base){return(1)}
  return(n)
}

FirstDigit(0.00000000,10)
FirstDigit(0.50000000,10)
FirstDigit(1.00000000,10)
FirstDigit(2048.00000000,10)
FirstDigit(0.00781250,10)
FirstDigit(0.50781250,10)
FirstDigit(1.00781250,10)
FirstDigit(2048.00781250,10)
FirstDigit(2147483647.00000000,10)
FirstDigit(-0.00000000,10)
FirstDigit(-0.50000000,10)
FirstDigit(-1.00000000,10)
FirstDigit(-2048.00000000,10)
FirstDigit(-0.00781250,10)
FirstDigit(-0.50781250,10)
FirstDigit(-1.00781250,10)
FirstDigit(-2048.00781250,10)
FirstDigit(-2147483647.00000000,10)
FirstDigit(10,16)
FirstDigit(163,16)
FirstDigit(179,16)
FirstDigit(179,36)
FirstDigit(-1234567,36)

0
5
1
2
7
5
1
2
2
0
5
1
2
7
5
1
2
2
10
10
11