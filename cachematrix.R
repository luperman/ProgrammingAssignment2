makeCacheMatrix <- function (x=matrix()) { #restricts input as a matrix
        
        m<- NULL        #initializes m as null eveytime the function is makeCachematrix is called explicitly.
                        #note that makeCacheMatrix should not be called directly but instead it must be assigned
                        #to an object, e.g.: if z is a squared matrix, then: k<-makeCacheMatrix(z)

        get<-function(){x}  #returns the content of the original matrix.
        
        setinverse<-function(solve){ #cacheSolve will call this function to superstore the inverse
                m<<-solve}
        
        getinverse<-function() {m} #getinverse will return the value of m (if exists) to cacheSolve
        
        list(get=get, setinverse=setinverse, getinverse=getinverse) #this is what is returned on this function 
        
}

cacheSolve<-function(x, ...){   #cachesolve will only work if the list to be evaluated
                                #has been created WITH makeCacheMatrix function
        
        m<-x$getinverse()       #reads the object x and gets the inverse (if exists)
        
        if(!is.null(m)) {       #checks if the inverse already exists (m IS NOT null)
                
                message("...getting cached data...")    #and shows a message (no new calculation here)
                return(m)       #returs the existing inverse matrix m
        }
        data<-x$get()           #otherwise the function will calculate the inverse in the next line
        m<-solve(data, ...)     #function calculates inverse matrix and stores it on m
        x$setinverse(m)         #transfers m to makeCacheMatrix where m will be superstored
        m                       #finally m is displayed directly. 
}
