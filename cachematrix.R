
## Function makeCacheMatrix will accept a matrix as a parameter
## and will return a list containing 4 function elements: get,set,getInverse,setInverse
## 1) the get function will return the passed in matrix
## 2) the set function may be used to update the matrix captured in this environment, this will cause the 
## cached inverse to be nulled out. 
## 3) the setInverse function will set the inverse into the environment
## 4) the getInverse function will return the inverse in set if there is one, otherwise NULL

makeCacheMatrix <- function(x = matrix()) {
   
  inverseX <- NULL
  ## get Returns the original matrix 
  get <- function() x
  ## replaces the original matrix with a new value, causes cached inverse to be reset
  set <- function(y) {
    x<<-y
    inverseX<<-NULL
  }
  ##set the inverse
  setInverse <- function(theInverse) inverseX <<- theInverse
  ##get the inverse that is stored (null if not yet set)
  getInverse <- function() inverseX
  #return the list of functions
  list(
       get = get,
       set = set,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve returns the inverse of the matrix that is acquired when calling x$get().
## If the matrix is cached within x already then the cached copy is returned, 
## otherwise the matrix inverse is calculated and added to the cache. 

cacheSolve <- function(x, ...) {
  # just return the inverse if it is not null. 
  inv<-x$getInverse()    
  if(!is.null(inv)){
    return(inv)
  }
  #otherwise, get the original matrix, find the inverse via solve(...),
  #store it with setInverse, and return it. 
  original<-x$get()
  inv<-solve(original, ...)
  x$setInverse(inv)
  inv
}

