## Together these functions solve the inverse of a solvable matrix and cache
## the solution for later retrieval

## makeCacheMatrix generates a list of functions initiated in the 
## makeCacheMatrix execution environment where the values of x (the submitted
## matrix) and s (the inversed matrix) are held
 
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL  
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solved) s <<- solved
        getsolve <- function() s
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve() takes an argument of type makeCacheMatrix. It retrieves the
## value of s from the makeCacheMatrix execution environment. If the value is
## NULL it solves and returns the inverse matrix, then caches it within 
## the execution environment of makeCacheMatrix. 
## If the value is not NULL (a cached inverse) than the 
## previously computed inverse is returned 

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)){
                message('inverse already solved')
                return(s)
        }
        matrix <- x$get()
        s <- solve(matrix)
        x$setsolve(s)
        s
}
