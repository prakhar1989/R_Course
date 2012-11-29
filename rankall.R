rankall = function(outcome, num="best" ) {
    
    # read file and get state names
    outcome.file = read.csv("outcome-of-care-measures.csv", colClasses="character")     
    states = names(table(outcome.file$State))
    
    # initialize data frame
    result.frame = data.frame(matrix(ncol = 2, nrow = length(states)))
    names(result.frame) = c("hospital", "state")
    
    # set column number
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
    
    i = 1
    
    # loop through states
    for (s in states) {
        
        state.subset = subset(outcome.file, State == s)
        ranking.metric = sort(as.numeric(state.subset[, col.number]))
        
        if(num == "best"){
            value.to.return = ranking.metric[1]  
        }
        
        else if (num == "worst"){
            value.to.return = ranking.metric[length(ranking.metric)]
        }
        
        else if (is.numeric(num)){
            value.to.return = ranking.metric[num]
        }
        
        hospital.subset = subset(state.subset, state.subset[, col.number] == value.to.return)
        result.frame[i, ] = c(hospital.subset$Hospital.Name[1], s)
        i = i + 1   
    }
    
    return(result.frame)
}
