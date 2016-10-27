## Put comments here that give an overall description of what your
## functions do

# Matrix inversion computation takes time and so there is advantage to caching the matrix inversion. The objective is to cache the matrix inversion 
# and avoid calculating it repeatedly, saving computation time. The two functions "makeCacheMatrix" and "cacheSolve"are important for carrying out 
#the matrix inversion cache. 

## Write a short comment describing this function

#The "makeCacheMatrix" creates a list that contains various functions to: 
#1. Set value of the matrix
#2. get value of the matrix 
#3  set value of the inverted matrix 
#4  get value of the inverted matix 

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
                  set = function(y){ x <<-y
                  inv <<- NULL
                  }
                  get = function() x 
                  setinv = function(inverse)     inv <<- inverse
                  getinv = function()  inv
                  list(set=set,get=get, setinv=setinv, 
                  getinv=getinv)
}
## Write a short comment describing this function

# The "cacheSolve matrix" below returns to us the matrix inversion. The first part of the function checks to see 
#if the matrix inversion has already been calculated. If it is solved, the computation is skiped and the inversion 
#matrix results are returned. If the matrix inversion is not computed, the matrix inversion in computed using the 
#the first function and then sets the matrix value in the Cache using the setinverse function. 

cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  if(!is.null(inv)) {
  message("get cache inverse data")
      
  ## Return a matrix that is the inverse of 'x'
  return(inv)
}

mat.data=x$get()
    inv=solve(mat.data, ...)
    x$setinv(inv)
    return(inv)
    }

