## Caching the Inverse of a Matrix
## Given a matrix - the functions retuns the inverse of the matrix 
# by using the buil-in solve method if the input matrix is new, or a cached inverse value
# if the matrix is already inputed.

## The 'MakeCacheMatrix' takes a matrix and creates a list with
# sets and gets the initial value of x, set and gets the inverse of the
#matrix

makeCacheMatrix <- function(x = matrix()) {
  
    # the value of m is set to null
         m <- NULL
    
    # assigns the set variable to the value of the intered matrix
    # sets the value of m to null
    
    set <- function(y) {
        x <<- y 
       
        m <<- NULL
    }
    
    # assigns the get variable to a function that returns the initial
    # value of the intered matrix
    
       get <- function() x
    
    # assigns the setinverse variable to a function that sets
    # the value of m from a value set in cacheSolve function
    
        setinverse <- function(d) m <<- d
    
    #the getinverse function is assigned to a function that returns
    # the value of m 
    
    getinverse <- function() m
    
    # creates a list of funtions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
 
}


## The cacheSolve function returns the inverse a matrix from cache
# if inverse is calculated already or by finding the inverse using 
# the solve built-in function

cacheSolve <- function(x, ...) {
        
   # assign m the inverse value by calling the getinverse function
   # found in makeCacheMatrix function
    
    m <- x$getinverse()
    
    # The conditional if close returns a message and value of m -
    # if m is not null from the above assignment
    # the function execution terminates if 'm' is not null
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # the data variable is assigned the value of the matrix found by calling
    # get function in makeCacheMatrix
       
        data <- x$get()
    # m is assigned the inverse of the entered matrix
    m <- solve(data)
    
    # the setinverse function is executed and passed the value of m - which sets 
    # the m is set in the shared environment - to be accessed later as cached value.
   
    x$setinverse(m)
    m
}
