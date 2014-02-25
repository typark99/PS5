####CLASS ACTIVITY
#1
voters.normal<-function(n, mu=0, sigma1=1, sigma2=1){
  id<-1:n
  d1<-rnorm(n, mu, sd=sigma1)
  d2<-rnorm(n, mu, sd=sigma2)
  mat<-cbind(id, d1, d2)
return(mat)  
}

voters.uniform<-function(n, a=0, b=1){
  id<-1:n
  d1<-runif(n, sd=sigma1)
  d2<-runif(n, sd=sigma2)
  mat<-cbind(id, d1, d2)
  return(mat)    
}

voters.multi<-function(n, Mu, Sigma){
 require(MASS)
  id<-1:n
  mat<-mvrnorm(n, Mu, Sigma)
 mat<-cbind(id,mat) 
 return(mat)    
}

voters.multi2<-function(n, Mu1, Mu2, Mu3, Sigma, r=3){
  require(MASS)
  id<-1:n
  if(r==2){
    mat1<-mvrnorm(n%/%2, Mu1, Sigma)
    mat2<-mvrnorm(n-n%/%2, Mu2, Sigma)
    mat.voters<-rbind(mat1,mat2)
  }
  if(r==3){
    mat1<-mvrnorm(n%/%3, Mu1, Sigma)
    mat2<-mvrnorm(n%/%3, Mu2, Sigma)
    mat3<-mvrnorm(n-2*(n%/%3),Mu3,Sigma)
    mat.voters<-rbind(mat1,mat2,mat3)
  }
  mat.voters<-cbind(id,mat.voters)
  return(mat.voters)
}


call.voters<-function(n, mu=0, Mu, Mu1, Mu2, Mu3, r=3, sigma1=1, sigma2=1, Sigma, a=0, b=1, method="normal"){
  if(method=="normal"){
    mat.voters<-voters.normal(n, mu, sigma1, sigma2)
  } 
  if (method=="snormal"){
    mat.voters<-voters.normal(n)
  }
  if (method=="uniform"){
    mat.voters<-voters.uniform(n, a, b)
  }
  if (method=="multivariate"){
    mat.voters<-voters.multi(n, Mu, Sigma)
  }
  if (method=="mixmulti"){
    mat.voters<-voters.multi2(n, Mu1, Mu2, Mu3, Sigma, r)
  }
  colnames(mat.voters)<-c("ID", "Dimension 1", "Dimension 2")
  return(mat.voters)
}

mat.voters<-call.voters(1000, method="snormal")


###
parties<-call.voters(2, method="normal")

distance<-function(voters,parties){
  require(pdist)
  mat.distance<-as.matrix(pdist(voters[,2:3], parties[,2:3]))  ##matrix of distances from voter to party - rows are voters, columns are parties
  return(as.numeric(mat.distance[,1]>mat.distance[,2]))  ##returns a vector of 0's for voters closer to party 1, and 1 for voters closer to party 2
}

visualize<-function(voters,parties){
  affiliate<-distance(voters,parties)  ##returns a vector with 0's indicating affiliation with party 1
  plot(voters[,2],voters[,3],col=ifelse(affiliate,"red","blue"),pch=20)  ##plot voters - affiliation with party 1 is blue, party 2 is red
  points(parties[,2],parties[,3],col="black",bg=c("blue","red"),pch=23,cex=2,lwd=2)  ##plot parties as diamonds - party 1 is blue, 2 is red
  abline(h=0)
  abline(v=0)
}


