library(combinat)


studentProjectScore <- function(n, type = "random", pref = (1:n)^2)
{
  res <-  NULL
  if(type == "pref")
  {
    res <- matrix(1:n,n,n, byrow = TRUE)
    res <- t(apply(res,1,function(x) sample(x, prob = pref)))
  }
  if(type == "random")
  {
    res <- matrix(1:n,n,n, byrow = TRUE)
    res <- t(apply(res,1,sample))
  }
  return(res)
}


#4132
#Les exemples de matrice score
mat5=matrix(c(1,2,3,4,5,3,4,2,1,5,4,2,5,1,3,2,3,5,1,4,5,4,1,3,2),ncol = 5,nrow = 5,byrow = TRUE)
mat6=matrix(c(1,3,2,4,5,6,
  1,4,2,3,6,5,
  5,1,3,2,4,6,
  2,1,3,6,4,5,
  1,3,2,4,5,6,
  5,4,2,3,1,6),ncol = 6,nrow = 6,byrow = TRUE)

mat20=matrix(c(3,  4, 16, 14, 17,  7,  6, 19, 18, 13, 20,  2,  9,  5, 11, 15, 10,
               12,  8,  1, 13,  4, 14,  3, 19,  8, 17, 12,  9,  6,  2, 15, 18, 16,
               11,  5, 10, 20,  7,  1, 13,  6, 14, 17,  2, 19,  3,  7,  9,  1,  8,
               4, 18, 16, 12, 20, 10, 15, 11,  5, 14,  2,  4, 17,  9,  5,  1,  3,
               7, 15, 19,  8,  6, 13, 20, 12, 10, 18, 11, 16,  1,  3, 14,  6,  7,
               11, 18,  9, 20,  4, 17, 10,  2, 12, 13, 19,  5,  8, 16, 15, 13, 11,
               3,  1, 18,  2, 17, 15,  5,  4, 16,  6, 19,  7, 12, 20, 10,  9,  8,
               14,  8, 16, 18, 13, 20,  3,  9, 10, 11,  4, 12,  5,  1,  6, 19, 17,
               15, 14,  7,  2, 18,  3,  1, 12,  7,  2, 10, 14,  9,  5,  4, 20,  8,
               16, 13,  6, 17, 19, 11, 15, 15, 17,  1, 10,  2, 20, 13, 12,  7, 19,
               16, 14,  9,  5,  3,  4, 18, 11,  6,  8,  8,  6, 10,  4, 20,  9,  5,
               1, 11, 15,  7, 12,  3, 14, 16, 19, 17, 13, 18,  2, 11,  8, 14, 17,
               16,  7,  3,  9, 12,  1,  2, 18, 20, 19, 13, 10, 15,  5,  6,  4, 13,
               2,  6, 10,  9, 12, 19, 11, 16, 15,  7,  4, 17,  3, 18,  5, 20, 14,
               8,  1,  6,  2,  7,  9,  1,  3, 14, 12,  5, 11,  8, 18, 19, 10, 15,
               16, 17, 13,  4, 20,  1, 15, 19,  7,  2,  6, 12,  5,  3, 20, 13, 18,
               9, 14, 11, 10,  4,  8, 17, 16, 15,  9, 19, 14,  4,  7,  6, 11,  8,
               3, 12, 18,  1,  5, 13, 16, 17, 10, 20,  2, 10, 12, 11, 18,  1, 14,
               16, 15,  6,  9,  7, 13,  8, 17,  3,  5,  2, 20,  4, 19,  2,  8, 14,
               7,  3,  6, 17,  4, 15, 16, 13, 11, 19, 18, 12,  5, 20,  9,  1, 10,
               14,  3, 16,  6,  4, 11,  1, 18, 17,  5, 12,  7, 10, 20, 15,  8, 13,
               2,  9, 19, 17,  2, 11,  7, 19, 15, 13,  8,  1,  4, 20,  5,  9, 18,
               16, 14, 10,  6, 12,  3, 17,  7,  5, 11,  3, 10, 13, 15,  4, 12,  8,
               6,  9, 19, 14, 16, 18,  2, 20,  1),ncol = 20,nrow = 20,byrow = TRUE)



test2=studentProjectScore(10)




score<-function(M,x){   #une fonction qui permet de calculer la score d'un vecteur
  n=ncol(M)
  s1=0
  for (i in 1:n) {
    s1=s1+M[i,x[i]]
  }
  return(s1)
}



mutation<-function(v,p){  #fonction de mutation
  
  cases=1:length(v)    #création d'un vecteur de 1 à n
  
  pi=rep(p/(length(v)-1),length(v)) #on distribue proba p/n pour i ème projet , par exemple:premier case de pi indique proba de mutation pour 1ère projet
  temp=v[1]                 
  pi[v[1]]=1-p         #proba de rester le meme est 1-p                
  v[1]=sample(cases,prob = pi,size = 1,replace = TRUE)  # on fait mutation  à partir du premier élément
  
  if(temp!=v[1]){
    for (j in 2:length(v)) {      # on échange avec l'élément exitant qui permet d'échanger 2 éléments dans proba p
      if(v[j]==v[1]){
        v[j]=temp
      }
    }
  }
  
  for (i in 2:(length(v)-1)) {       #on fait la meme chose pour élément de 2 à n-1
    temp=v[i]
    pi=rep(p/(length(v)-1),length(v))
    pi[v[i]]=1-p
    v[i]=sample(cases,prob = pi,size = 1,replace = TRUE)
    
    if(temp!=v[i]){
      for (j in 1:(i-1)) {
        if(v[j]==v[i]){
          v[j]=temp
        }
      }
      
      for (j in (i+1):length(v)) {
        if(v[j]==v[i]){
          v[j]=temp
        }
      }
    }
  }
  
  
  pi=rep(p/(length(v)-1),length(v))    # on refait la meme chose pour dernier élément
  temp=v[length(v)]
  pi[v[length(v)]]=1-p
  v[length(v)]=sample(cases,prob = pi,size = 1,replace = TRUE)
  
  if(temp!=v[length(v)]){
    for (j in 1:(length(v)-1)) {
      if(v[j]==v[length(v)]){
        v[j]=temp
      }
    }
  }
  
  return(v)
  
}






studentProjectGenetique<-function(M,iter,p){
  T1<-Sys.time()
  n=ncol(M)
  scoremax=n*n
  quantile=(3/4)*scoremax  # définissons un quantile pour que l'on a un chromosome qualifié initial
  
  
  
  x1 <- sample(1:n,n,replace=F)
  
  while (score(M,x1)<quantile) {
    x1 <- sample(1:n,n,replace=F)
  }
  
  for (j in 1:iter) {
    
  
  x2 <- sample(1:n,n,replace=F)
  
  while (score(M,x2)<quantile) {
    x2 <- sample(1:n,n,replace=F)
  }
  s1=score(M,x1)
  s2=score(M,x2)
  s3=rep(0,n)
  if(s1>s2){
    for (i in 1:n) {
      if(M[i,x1[i]]>=M[i,x2[i]]){
        s3[i]=x1[i]      #on remplit s3 basant sur le vecteur dont score est plus grand
      }                  #on remplit s3 par vecteur d'ou élément est plus grand
    }
  }
  else{
    for (i in 1:n) {
      if(M[i,x1[i]]<=M[i,x2[i]]){
        s3[i]=x2[i]
      }
    }
  }
  pos=which(s3==0)    # trouvons les postions qui sont pas encore remplit de s3
  if(s1>s2){          #remplir les positions par qui sont plus grand de x1 et x2
    for (i in pos) {
      if(M[i,x1[i]]<M[i,x2[i]] & (any(s3==x2[i])==FALSE)){
        s3[i]=x2[i]         
      }
    }
  }
  else{
    for (i in pos) {
      if(M[i,x1[i]]>M[i,x2[i]] & (any(s3==x1[i])==FALSE)){
        s3[i]=x1[i]
      }
    }
  }
  
  if(length(which(s3==0))!=0){
    student_sans_projet=which(s3==0)
    projet_a_distribuer=setdiff(x = (1:n),y= s3)   #s'il exite des cases qui sont pas encore remplit, on les remplit des projets non distribués aléatoirement
    if(length(projet_a_distribuer)==1){
      s3[student_sans_projet]=projet_a_distribuer
    }
    else{
      s3[student_sans_projet]=sample(projet_a_distribuer,size = length(projet_a_distribuer),replace = FALSE,prob = rep(1/length(projet_a_distribuer),length(projet_a_distribuer)))
    }
  }
  
  s3=mutation(s3,p)
  x1=s3  #on itére avec x1 remplacé par s3 
  
  }
  T2<-Sys.time()
  mylist=list("Score_total"=score(M,x1),"Projet_a_distribuer"=x1,"Temps d'éxecution:"=difftime(T2,T1))
  return(mylist)
}




studentProjectGenetique(mat5,50,0.001)
studentProjectGenetique(mat6,50,0.001)
studentProjectGenetique(mat20,50,0.001)



###################
#######NAIVE#######
###################



  


studentProjectNaive<-function(lenth,K){
  T1<-Sys.time()
  a=rep(0,times=K)
  n=lenth
  allAllocations <- permn(1:n)   #considérons toutes les permutations possibles de distribution de projets
  nbLoops <- factorial(n)
  res=0
  res_vect=rep(0,n)
  for (nb in 1:K) {
    m=studentProjectScore(lenth)
  for(i in 1:nbLoops)
  {
    s=score(m,allAllocations[[i]])
    if(res<s){                      #on garde chaque fois la permutation dont le score le plus grand
      res=s
      res_vect=allAllocations[[i]]
    }
  }
    a[nb]=res
  }
  moy_score=mean(a)
  T2<-Sys.time()
  mylist=list("Score_total"=moy_score,"Projet_a_distribuer"=res_vect,"Temps d'exécution:"=difftime(T2,T1)/K)
  return(mylist)
}


#studentProjectNaive(5,6)
