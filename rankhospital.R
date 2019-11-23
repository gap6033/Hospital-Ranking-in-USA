rankhospital <- function(state, outcome, num = "best"){
        data <- read.csv("outcome-of-care-measures.csv")
        state_vec <- as.vector(data$State)
        state_name <- unique(state_vec)
        if(state %in% state_name == FALSE) stop("invalid state")
        names(data)[11]<- "heart attack"
        names(data)[17]<- "heart failure"
        names(data)[23]<- "pneumonia"
        if(outcome %in% c('heart attack', 'heart failure', 'pneumonia') == FALSE) stop("invalid outcome")
        state_data <- data[data$State == state,] #subsetting the data
        criteria <- state_data[[outcome]]
        num_criteria <- as.vector(criteria)
        hosp_name <- state_data$Hospital.Name
        relevant_data <- data.frame(hosp_name, num_criteria, stringsAsFactors = FALSE)
        relevant_data$num_criteria <- as.numeric(relevant_data$num_criteria)
        sorted_data <- relevant_data[order(relevant_data$num_criteria, relevant_data$hosp_name),]
        good_data <- na.omit(sorted_data)
        print(good_data)
        if (num == 'best') num <- 1
        if (num == 'worst') num <- nrow(good_data)
        relevant_hosp <- good_data[num,1]
        as.vector(relevant_hosp)
}
rankhospital("TX", "heart failure", 'worst')
rankhospital("NC", "heart attack", "worst")                                                                                                                                                          
rankhospital("WA", "heart attack", 7)
rankhospital("TX", "pneumonia", 10)
rankhospital("NY", "heart attack", 7)
