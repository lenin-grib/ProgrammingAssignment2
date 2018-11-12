## Two functions below optimize the caching of 
## calculating given matrix's inverse


## Constructor for special object storing the matrix, the cache'd inverse and its methods
makeCacheMatrix <- function(x = matrix()) {

        m_inv <- NULL #clear any previously stored value if new object is created
        set <- function(y) {
                x <<- y
                m_inv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m_inv <<- inv
        getinv <- function() m_inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
}


## cacheSolve checks is there is any prevoiusly calculated inverse for passed matrix. 
## If yes it retrives the data; if not - calculating it and saving in the "cache matrix" object

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinv()
        if(!is.null(m_inv)) {
                message("getting cached data")
                return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data, ...)
        x$setinv(m_inv)
        m_inv
}
