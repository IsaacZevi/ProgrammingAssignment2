
## The following functions create a "special" matrix that has different behaviours 
## than the regular atomic matrix.
## Once the inverse of a "special" matrix is being calculated, the result is being cached
## so such a computation will not be repeated unnecessarily

## The following function get as input a matrix (here named theMatrix)
## then set it's inverse to be NULL 
## this function also creats for theMatrix four functions
## that enable to set and get values

makeCacheMatrix <- function(theMatrix = matrix()) {
        #initializing the inverseMatrix of the input matrix with NULL value 
        inverseMatrix <- NULL
        #once a "special" matrix was created the "set" function
        #can be used to change the matrix's values
        #such a change cause the inverse matrix to get NULL value again
        set <- function(y) {
                theMatrix <<- y
                inverseMatrix <<- NULL
        }
        #"get" just returns the input matrix (theMatrix)
        get <- function() theMatrix
        #"setInv" is being used to assign a value to the inverse matrix
        #it can be used by the cacheSolve function after the inverse was calculated or 
        #directly by the user on the command-line 
        setInv <- function(inverse) inverseMatrix <<- inverse
        #"getInv" just returns the inverse matrix
        getInv <- function() inverseMatrix
        #the following list is being returned to enable 
        #any newlly created "special' matrix to have the access to the functions abouve
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## The following function get as input a "special" matrix
## and return it's inverse matrix


cacheSolve <- function(theMatrix, ...) {
        ## in case the inverse was already calculated/set
        ## the inverse is not being calculated again but retuned form the cache
        ## and "cacheSolve" will stop executing at this point
        inverseMatrix <- theMatrix$getInv()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        # the following lines will be executed 
        # justin case the inverse is still null 
        
        #getting the matrix values
        data <- theMatrix$get()
        #calculating it's inverse
        inverseMatrix <- solve(data, ...)
        #seting it to the "special" matrix and returning the inverse matrix
        theMatrix$setInv(inverseMatrix)
        inverseMatrix
}
