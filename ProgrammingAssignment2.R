#Constructs a matrix whose inverse will be cached
makeCacheMatrix<-function(A = matrix()){
    inv<-NULL
    set<-function(y){
      A<<-y
      inv<<-NULL
    }
    
  #  
  get<-function() A
  setsolve<-function(solve) inv<<-solve
  getsolve<-function() inv
  
  #Constructs a list that contains the matrix and its inverse
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

#First searches to see if the inverse matrix has been computed.
#If not then computes the inverse and puts it into the cache.
#If the inverse has been cached then it displays the inverse.
cacheSolve<-function(A,...){
  
  #Pulls the inverse matrix from the makeCacheMatrix list
  inv<-A$getsolve()
  
  #Checks to see if inverse has already been calculated.
  #If it has then it pulls it from memory and displays again.
  if(!is.null(inv)){
    message("getting cached inverse")
    return(inv)
  }
  
  #Gets the matrix from the makeCacheMatrix list whose 
  #inverse we want to find.
  inverse<-A$get()

  #Calculate the inverse of the matrix
  inv1<-solve(inverse,...)

  #Puts the inverse matrix into cache
  A$setsolve(inv1)
  
  #Returns the inverse of the matrix
  return(inv1)

}
