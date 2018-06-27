tidy_hour <- function(hour, separator = ":"){
    if(nchar(hour) == 2){
        return(paste(hour,separator, "00", sep = ""))  
    }else if(nchar(hour == 1)){
        return(paste("0",hour,separator, "00", sep = ""))
    }else message("Invalid hour")
}