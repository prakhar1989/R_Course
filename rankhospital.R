rankhospital = function(state, outcome, num="best" ) {
    outcome = tolower(outcome)
    outcome.file = read.csv("outcome-of-care-measures.csv", colClasses="character")   
    if(state %in% outcome.file$State){
        subset.output = subset(outcome.file, State == state)
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
        subset.output[, col.number] = as.numeric(subset.output[, col.number])
        death.outcome = sort(subset.output[, col.number])
        if(num == "best"){
            value.to.return = death.outcome[1]  
        }
        else if (num == "worst"){
            value.to.return = death.outcome[length(death.outcome)]
        }
        else if (is.numeric(num)){
            if(num > nrow(subset.output)) {
                return(NA)
            }
            value.to.return = death.outcome[num]
        }
        consider.outcome = subset(subset.output, subset.output[col.number] == value.to.return)
        return(consider.outcome$Hospital.Name[1])
    }
    else {
        stop("Invalid State")
    }
    
}