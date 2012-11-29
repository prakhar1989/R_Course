anotherrank = function(outcome, num="best"){
    outcome.file = read.csv("outcome-of-care-measures.csv", colClasses="character")
    outcome.split = split(outcome.file, outcome.file$State)

    
    result.frame = data.frame(matrix(ncol = 2, nrow = length(outcome.split)))
    names(result.frame) = c("hospital", "state")
    
    if(outcome == "heart attack"){
        col.number = 11
    } 
    else if(outcome == "heart failure"){
        col.number = 17
    }
    else if(outcome == "pneumonia") {
        col.number = 23
    }
    else {
        stop("Invalid outcome")
    }

    
    
    #loop over each state list
    test.value = lapply(outcome.split, function(x){
        
        sorted.list = sort(as.numeric(x[, col.number]))
        
        if(num == "best") {
            value.to.return = sorted.list[1]
        }
        else if (num == "worst") {
            value.to.return = sorted.list[length(sorted.list)]
        }
        found.index = which(x[, col.number] == value.to.return)  
        hospital.name = outcome.file[found.index, ]$Hospital.Name
        
        #result.frame[i, ] = c(hospital.name, x$State[1])
        result.frame[1, ] = c("something", "32")
        
    })
    result.frame[2, ] = c("ok", "dokay")
    return(result.frame)
}