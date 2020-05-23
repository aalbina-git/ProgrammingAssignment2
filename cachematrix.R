## These functions encapsulate the data and methods in R objects
## enabling envrionment portability.
## Suggestions for tetsting at the R prompt:

# > mat <- matrix(data = rexp(200, rate = 10), nrow = 4,ncol = 4)
# > mat
# > newMat = makeCacheMatrix(mat)
# > newMat$get()
# > newInv <- cacheSolve(newMat)
# > newInv
# > newMat$getInverse()

## Input: requires a balanced matrix of numeric elements. such as :
##        mat <- matrix(data = rexp(200, rate = 10), nrow = 4,ncol = 4)
##  Output: an R object of type makeCacheMatrix with methods and data

makeCacheMatrix <- function(x = matrix()) { #initializes x
    
    mx <- NULL  #initialize matrix as NULL to avoid an error with x$get()
    set <- function(y) {  #dfine mutator method
        x <<- y    #assigns y to the parent environment x
        mx <<- NULL  # assign NULL to the parent envrioment of mx which clears
                     # any previous cached value
    }
    get <- function() x  #define accessor method (x is a free variable and R goes to the parent)
    setInverse <- function(inverse) mx <<- inverse #assigns input arg to value of mx in parent env 
    getInverse <- function() mx 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse) #assigns functions as elements in a list
                                  # we can use x$get, x$set    

}


## This function completes the function above and requires as an argument
## an object of type makeCahcedMatrix.  It will calculate the inversion of
## the matrix if it doesn't exist in cache already.

cacheSolve <- function(x, ...) {  #cacheSolve REQUIRES a matrix from makeCachedMatrix
        ## Return a matrix that is the inverse of 'x'
    mx <- x$getInverse()  # get inverse matrix by calling the passed in object getInverse method
    if(!is.null(mx)) {    # check to see if it is NOT NULL - then it has been calculated so return it
        message("getting cached data")
        return(mx)
    }
    data <- x$get()  # if x$getInverse IS NULL then get the matrix from the object
    mx <- solve(data, ...) # calculate the inverse
    x$setInverse(mx) #set the Inverse
    mx # return mx
}
