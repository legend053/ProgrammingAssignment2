## Put comments here that give an overall description of what your
## functions do

## The purpose of makeCacheMatrix is to create a list containing a function
## to (1) set the value of the matrix, (2) get the value of the matrix, (3) 
## set the inverse matrix, (4) get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL #set the null value for m
  set<-function(y){ ##Define the set function
    x<<-y
    m<<-NULL
  }
  get<-function() x # Get the original matrix
  setinverse<-function(solve) m<<- solve #using solve function to set the inverse matrix
  getinverse<-function() m #get the inverse matrix
  list(set=set, get=get, # create a list which contains four functions
       setinverse=setinverse,
       getinverse=getinverse) 
}


## cacheSolve function is used to calculate the inverse matrix. It has two functions:
## (1)check if the same calculation (get inverse matrix) has been performed. if the same 
## calculation has been done before and the results have been stored in cache, this function 
## will extract the results without calculating again which can save time in computing. (2)
## calculate the inverse matrix and set the inverse matrix iva the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinverse(m)
  m
}