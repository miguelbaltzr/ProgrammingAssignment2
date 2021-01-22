## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## This code calculates the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) 
{
        i  <- NULL
        set  <- function(y){
                x <<- y
                i <<- NULL 
        }
        get  <- function() x
        set_inverse  <- function(inverse) i  <<- inverse ## sets the value of the inverse matrix
        get_inverse  <- function() i ## gets the value of the inverse matrix
        list(set= set, get = get, 
             set_inverse = set_inverse, 
             get_inverse = get_inverse)
}

## This code calculates the inverse of a special matrix
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        i  <- x$get_inverse()
        if (!is.null(i)){
                message("getting cached data") ##displays the message
                return(i)
        }
        data  <- x$get() ## calculates the inverse of a matrix via set_inverse function
        i  <- solve(data, ...)
        x$set_inverse(i)
        i
}
