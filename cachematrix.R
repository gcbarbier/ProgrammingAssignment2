  
# GCB
#The following two functions are used to cache the inverse of a matrix. 

makeCacheMatrix <- function(x = matrix()) { 
  
  m <- NULL 

  set <- function(y) { 
    

    x <<- y 
    

    m <<- NULL 
    
  } 
  
  get <- function() x 
  setinverse <- function(inverse) m <<- inverse 
  
  getinverse <- function() m 

  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse) 
  
} 



# The following function returns the inverse of the matrix.

# This function assumes that the matrix is always invertible. 

cacheSolve <- function(x, ...) { 
 
  m <- x$getinverse() 
  
 
  if(!is.null(m)) { 
    
 
    message("getting cached data.") 
  
    return(m) 
 
  } 
  
 
  data <- x$get() 
  

  m <- solve(data) 
  
  x$setinverse(m) 
  
  m 
  

} 

## Sample run: 
x = rbind(c(1, 3), c(-3, 1)) 
f = makeCacheMatrix(x) 
f$get() 

## No cache in the first run
cacheSolve(f) 

## Retrieving from the cache in the second run 
cacheSolve(f)

