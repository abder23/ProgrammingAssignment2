
##This function take a matrice as variable
##Its particularity is to hide the value of the matrice inverse if it has been
##already calculate
##get() and set() functions will  work with the matrice
##getinv and setinv functions will  work with the matrice inversion
##this function will return a list of the above 4 predefine functions

makeCacheMatrix <- function(matx = matrix()) {
                invmat <- NULL
                
                set <- function(maty){
                    matx <<- maty
                    invmat <<- NULL
                }
                
                get <- function() matx
                
                setinv <- function(solve) invmat <<- solve
                getinv <- function() invmat
                
                list(set=set, get=get, setinv=setinv, getinv=getinv)

}



##allow to calculate or retreive the inverse value of the matrice 
##it will retreive if the inversion existe already in a cache
##It will calculate if the matrice is new in the system

cacheSolve <- function(matx, ...) {
        ## Return a matrix that is the inverse of 'x'
           invmat <- matx$getinv()
           if(!is.null(invmat)){
             message("getting cached data")
             return(invmat)
           }
           data <- matx$get()
           invmat <- solve(data,...)
           matx$setinv(invmat)
           invmat
}


##voila