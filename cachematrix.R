## Put comments here that give an overall description of what your
## functions do
## By Tamrerk Nasomyont
## Date, Time Sunday 26, October 2014, 6:04:12 AM
## Write a short comment describing this function
## 1. Function makeCacheMatrix
## 1.1 Creates a special matrix
## 1.2 Can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL                                      ## Initialize local variable (inv) - assign as NULL to reserve the memory
        set <- function(y) {                             ## Set function - overwrite x with y (new value),  
                x <<- y                   
                inv <<- NULL
        }
        get <- function() x                              ## Return the original input matrix
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv                     ## Return in computed inverse matrix
        list(set = set, get = get,                       ## Call each function individually  
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function
## 2. Function cacheSolve
## 2.1 Computes the inverse of the special matrix
## 2.2 If the inverse has already been calculated -> retrieve the inverse from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {                                ## Condition: If there is a computed value in inv, print("getting cached data") 
                message("getting cached data")            
                return(inv)                                ## Return it's value
        }
        data <- x$get()                                    ## Assign the value of matrix from the get function
        inv <- solve(data, ...)                            ## Compute the inverse of the assigned matrix 
        x$setinverse(inv)
        inv
}
