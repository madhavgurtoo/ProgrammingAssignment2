## Week 3 programming assignment.

## A function makeCacheMatrix() to make a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{
        inverse <- NULL
        set <- function(y)
        {
                x <<- y
                inverse<<- NULL
        }
        get <- function() x
        set_inverse <- function(i = matrix()) inverse <<- i
        get_inverse <- function() inverse
        list(set = set, get=get , set_inverse = set_inverse , get_inverse = get_inverse)
        
}


## A function cacheSolve() to compute inverse or use cache to get inverse of matrix 

cacheSolve <- function(x, ...) 
{
        inverse <- x$get_inverse()
        if(!is.null(inverse)) 
        {
                message("getting cached data")
                return(inverse)
        }
        i <- solve(x$get())
        x$set_inverse(i)
        i
}
