best = function(state, outcome) {
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
            stop("Invalid outcome selected")
        }
        max.value = min(as.numeric(subset.output[, col.number]), na.rm=T)
        consider.outcome = subset(subset.output, subset.output[col.number] == max.value)
        return(sort(consider.outcome$Hospital.Name)[1])
    }
    else {
        stop("Invalid State Selected")
    }
    
}