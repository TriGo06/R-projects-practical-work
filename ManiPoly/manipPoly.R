library(polynom)
#1 list(c(-3,5),c(1,4),c(2,0))
#2 c(2,0,0,0,1,-3)
#3 Quand le vecteur du polynome comporte beaucoup de 0
#4 La représentation creuse est plus econome en terme de mémoire car elle ne contient pas les monomes nuls.


#si la liste représentant le polynome est vide sa longueur est de 0 est donc le polynome est nul -> TRUE, sinon on retourne FALSE
is_poly0 <- function(p){
  if(length(p)==0){return(TRUE)}
  return(FALSE)
}
#teste de la fonction is_poly0
is_poly0(list(c(-3,5),c(1,4),c(2,0)))
is_poly0(list())


p <- list(c(2,5), c(-1,4),c(-2,1))
q <- list(c(1,4), c(7,2),c(-1,0))

degre <- function(mp) {
  if(is_poly0(mp)) {return(NA)} #Si le polynome est nul on retourne NA
  if (is.list(mp)) {mp <- mp[[1]]}#Si le polynome est une liste on le transforme en vecteur
  if (is.vector(mp) && length(mp) == 2) {return(mp[2])}#Si le polynome est un vecteur de longueur 2 on retourne son degré c'est à dire le second element du vecteur
  else {return(NULL)}#Sinon on retourne NULL
}

#Pour renvoyer le coefficient d'un monome de la liste formant le polynome même principe que la fonction degre mais on retourne le coefficient du monome c'est à dire le premier element du vecteur
coef <- function(mp) {
  if(is_poly0(mp)) {return(NA)}
  if (is.list(mp)) {mp <- mp[[1]]}
  if (is.vector(mp) && length(mp) == 2) {return(mp[1])}
  else {return(NULL)}
}

#poly2str renvoie une chaine de caractère représentant le polynome
poly2str <- function(p){
  x <- list()                     #On crée une liste "x" vide
  for (i in 1:length(p)) {        #On parcourt la liste p qui représente le polynome
    x[i] <- paste(coef(p[[i]]),degre(p[[i]]),sep = '*X^') #On ajoute au vecteur x le coefficient et le degré du monome, on les concatène avec "*X^" au mileu
  }
  x <- paste(x,collapse = ' + ')#Il ne reste que à concaténer les chaînes de charactères de la liste x avec " + " au mileu
  return(x)
}
#teste de la fonction poly2str
poly2str(p)

mult_ext <- function(p, k){ #On multiplie chaque coefficient du polynome par k et on retourne le polynome 
  lapply(p,function(x){c(k*x[1], x[2])})#grâce à lapply qui applique la fonction c(k*x[1], x[2]) à chaque élément de la liste p et renvoie directement une liste
}                                       #ce qui est plus efficace qu'avec la fonction sapply qui renvoie un vecteur

#teste de la fonction mult_ext
poly2str(mult_ext(p,2))


mult_ext2 <- function(p,k){#moins efficace avec une boucle for
  x <- list()
  for (i in 1:length(p)) {
    x[[i]] <- c(coef(p[i])*k,degre(p[i]))
  }
  return(x)
}

make_poly <- function(x){#on prend en argument un polynome en representation pleine et on le transforme en liste creuse 
  j <- 1 #j représente l'indice du monome dans la liste
  y <- list() #on crée une liste vide pour y accumuler les monomes non nuls
  for (i in length(x):1) {
    if(x[[i]]!=0){#si le nombre du vecteur de representation pleine n'est pas nul on l'ajoute à la liste y avec son degré sinon on ne fait rien et la boucle continue
      y[[j]] <- c(x[i],i-1)
      j <- j+1
    }
  }
  return(y)
}
#teste de la fonction make_poly
poly2str(make_poly(c(1,0,2,0,1,-3)))


rand_poly <- function(n,coeffs){#creation d'un polynome aléatoire de degré n avec des coefficients pris dans le vecteurs coeffs
  x <- sample(coeffs,n,replace = TRUE)#on tire n coefficients aléatoirement dans le vecteur coeffs avec remise avec la fonction sample
  return(make_poly(x))#on retourne le polynome en representation creuse grâce à la fonction make_poly
}
#test de la fonction rand_poly
poly2str(rand_poly(7,1:10))


sort_monoms <- function(p){#on retourne l'inverse d'un polynome trié par degré croissant donc il sera trié par degré décroissant
  return(rev(p[order(sapply(p,'[[',2))]))   #https://stackoverflow.com/questions/28100593/how-to-sort-a-list-of-lists-in-r
}#sapply(p,'[[',2) permet de retourner un vecteur contenant les degrés de chaque monome du polynome p, order permet de trier ce vecteur dansl'ordre croissant, puis rev permet de retourner d'inverser de ce vecteur

#si deux vecteurs ont le même degré, on additionne les coefficients
#si un vecteur a un coefficient nul, on le supprime
#si la liste est vide, on renvoie une liste vide
#si la liste contient un seul vecteur, on la renvoie
#si la liste contient plus de 1 vecteurs, on renvoie la liste fusionnée
merge_monomes <- function(p){
  if(length(p)==0){return(list())}
  if(length(p)==1){return(p)}
  p <- sort_monoms(p)
  x <- list()
  x[[1]] <- p[[1]]
  j <- 1
  for (i in 2:length(p)){
    if(degre(p[[i]])==degre(x[[j]])){
      x[[j]] <- c(coef(x[[j]])+coef(p[[i]]),degre(x[[j]]))
    }
    else{
      j <- j+1
      x[[j]] <- p[[i]]
    }
  }
  x <- x[sapply(x,function(x){x[1]!=0})]
  return(x)
}
#test de la fonction merge_monomes
merge_monomes(c(p,q))


add <- function(p1,p2){
  return(merge_monomes(c(p1,p2)))#on fusionne les deux polynomes en une liste et on retourne le résultat de la fonction merge_monomes
}
#test de la fonction add
add(p,q)

sub <- function(p,q){
  add(p,mult_ext(q,-1))#on multiplie chaque coefficient de q par -1 et on additionne le résultat à p
}
#test de la fonction sub
sub(p,q)



#on ajoute 1 au degré et on divise le coefficient par le nouveau degré
#si le degré est nul on renvoie le polynome
primitive <- function(p){
  if(length(p)==0){return(list())}
  x <- list()
  for (i in 1:length(p)){
    if(degre(p[[i]])==0){
      x[[i]] <- p[[i]]
    }
    else{
      x[[i]] <- c(coef(p[[i]])/(degre(p[[i]])+1),degre(p[[i]])+1) #on peut aussi utliser la fonction round pour arrondir le coefficient par exemple round(coef(p[[i]])/(degre(p[[i]])+1),3) 
    }                                                             #mais pour plus de precision dans les calculs lors de l'evaluation du polynome en un point on ne l'utilise pas
  }
  return(x)
}

#si le degré est nul, on renvoie le polynome
#si le degré est supérieur à 1 on multiplie le coefficient par le degré et degre = degre-1
derive <- function(p){
  if(length(p)==0){return(list())}
  x <- list()
  for (i in 1:length(p)){
    if(degre(p[[i]])==0){
      x[[i]] <- p[[i]]
    }
    else{
      x[[i]] <- c(coef(p[[i]])*degre(p[[i]]),degre(p[[i]])-1)
    }
  }
  return(x)
}
#test de la fonction derive et primitive
testderivprimitive <- function(){
  cat("P(x) = ",poly2str(primitive(list(c(1,2),c(3,4),c(5,6)))),"\n")
  cat("p(x)' =",poly2str(derive(list(c(1,2),c(3,4),c(5,6)))),"\n")
  cat("p(x) =",poly2str(list(c(1,2),c(3,4),c(5,6))),"\n")
}


mult <- function(p,q){
  if(length(p)==0 | length(q)==0){return(list())}#si l'un des deux polynomes est nul on renvoie une liste vide
  x <- list()
  for (i in 1:length(p)){ 
    for (j in 1:length(q)){ #deux boucles for imbriqué permettent de recréer une sorte de double distributivité, c'est a dire qu'il y aura length(p)*length(q) itérations
      x[[length(x)+1]] <- c(coef(p[[i]])*coef(q[[j]]),degre(p[[i]])+degre(q[[j]]))#on multiplie les coefficients et on additionne les degrés c'est a dire on multiplie le premier monome de p par tous les monomes de q et ainsi de suite
    }
  }
  return(merge_monomes(x))#on fusionne la liste final pour regroupé les termes de meme degré
}
#teste de la fonction mult
p <- rand_poly(6,1:8)
q <- rand_poly(3,1:10)
mult(p,q)



polyval <- function(p,x){
  if(length(p) == 0) {return(0)}
  acc <- 0 #on initialise l'accumulateur à 0
  for (i in 1:length(p)){
    acc <- acc + coef(p[[i]])*x^degre(p[[i]])#on ajoute le coefficient du monome multiplié par x^degré du monome à l'accumulateur
  }
  return(acc)
}
#test de la fonction polyval
testpolyval <- function(){ 
  cat("p(x) =",poly2str(p),'\n')
  for(i in -1:1) {
    print(paste("p(",i,") = ",polyval(p,i), sep=""))
  }
  cat("q(x) =",poly2str(q),'\n')
  for(i in -1:1) {
  print(paste("q(",i,") = ",polyval(q,i), sep=""))
  }
}

creuxtoplein <- function(p){
  n <- length(p)
  m <- max(sapply(p,function(x){x[2]}))#determination du degre maximum
  x <- numeric(m+1)#creation du vecteur x avec pré-allocation de memoire(ce sera un vecteur de longueur m + 1 car il y a le x^0)
  for (i in 1:n){
    x[p[[i]][2]+1] <- p[[i]][1] #on remplace les zeros dans le vecteur numeric de zero par les coefs du polynome de sortes que p[i] soit de degré i+1
  }
  return(x)
}

polyhorn <- function(p,x){ #la methode de horner consiste à appliquer la formule suivante: p(x) = a0 + x(a1 + x(a2 + x(a3 + ... + x(an-1 + x(an))...)))
  p <- rev(creuxtoplein(p))
  y <- p[[1]]
  for (i in 2:length(p)){
    y <- y*x + p[[i]]
  }
  return(y)
}

#teste de la fonction polyhorn
testpolyhorn <- function(){
  cat("p(x) =",poly2str(p),'\n')
  for(i in -1:1) {
    print(paste("p(",i,") = ",polyhorn(p,i), sep=""))
  }
  cat("q(x) =",poly2str(q),'\n')
  for(i in -1:1) {
    print(paste("q(",i,") = ",polyhorn(q,i), sep=""))
  }
}

#comparaison polyval/polyhorn
x <- seq(2-0.02,2.02,length.out=1001)
vec <- c(-2,0,1)
## p(x) = (x^2-2)^16
p1 <- make_poly(vec)
for(i in 1:4) {
  p1 <- mult(p1,p1)
}
## Evaluate p using our methods (observed)
vn <- sapply(x, polyval, p=p1)
vh <- sapply(x, polyhorn, p=p1)


library("polynom")
## Evaluate p using R package (expected)
pr <-as.polynomial(c(-2,0,1))
## p is an R object ! I directly used the power operator.
pr <- pr ** 16
## I also use a generic function
vr <- predict(pr,x)

## Last, visualize the results
cols <- c("red", "gold")
matplot(x, cbind(vn-vr,vh-vr),t="l", lwd=2, lty=1,col=cols, xlab="x",ylab="P(x)-P^R(x)")
legend("topleft", inset=.05, legend=c("Naive", "Horner"), horiz=TRUE, lwd=2, lty=1, col=cols)

dessiner <- function(p,x){
  plot(x,polyval(p,x),type = 'l',col = 'blue')#on trace le graphe de la fonction
  lines(x,polyval(derive(p),x),type = 'l',col = 'red')#on trace le graphe de la dérivée de la fonction
  lines(x,polyval(primitive(p),x),type = 'l',col = 'green')#on trace le graphe de l'intégrale de la fonction
}
x <- seq(-2,4,length.out=1000)
p1 <- make_poly(c(-1,-1,1))
dessiner(p1,x)


airepoly <- function(p,a,b){#integrale entre deux bornes grâce à la fonction primitive codée précédemment
  return(polyhorn(primitive(p),b)-polyhorn(primitive(p),a))
}
airepoly(p,0,1)
