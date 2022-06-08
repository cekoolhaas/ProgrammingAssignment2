## makeCachematrix takes a square matrix as an argument and 
## stores the inverse in a cache.
## cacheSolve will pull the inverse of that square matrix from the cache.

## First, makeCachematrix generates the inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {
  myinverse<-NULL       ## Placeholder for the inverse
  set<-function(y){
    x<<-y
    myinverse<<-NULL
  }
  get <-function()x
  setinverse <-function(inverse)myinverse <<- inverse
  getinv <- function() {   ## Here is where the matrix gets converted to
    inver <-solve(x)       ## an inverse.
    inver%*%x              ## In order to prevent an error saying the matrix was 
  }                        ## not square, I multiplied the matrix using %*%.
  list(set = set, get = get,
       setinverse = setinverse,   ## This is setting up how the user will 
       getinv = getinv)           ## call the function to get an answer.
}                                 ## For example, typing x$getinv()
                                  ## into the console will give you the inverse
                                  ## where "x" is the variable for your
                                  ## matrix (must be a square matrix!).

## cacheSolve digs out the solution to the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
  myinverse<-x$getinv()
  if(!is.null(myinverse)) {        ## This is saying, if myinverse is NOT empty, 
    message("getting cached data") ## then it must be in the cache.
    return(myinverse)
  }
  data<-x$get()
  myinverse<-solve(data, ...)      ## Finalizing the cache retrieval
  x$setinverse(myinverse)
  myinverse
                                   ## The inverse is ready! Use x$get() and 
}                                  ## x$getinv() to get the original matrix
                                   ## and the inverse matrix, where "x" is
                                   ## the name of the variable where your
                                   ## matrix is stored. 