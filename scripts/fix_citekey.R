
library(stringr)

fix_citekey <- function(mystring, delimiter){
    # If there is a 1 directly after a letter...
    if(grepl("[a-z][1]", mystring)){
        # Split on the first 1
        mystring<-
            str_split(mystring,"[1]",n=2) %>%
            unlist() %>%
            # Insert delimiter and reinsert the 1
            {paste(.[1],delimiter,"1", .[2], sep="")}
    # If there is a 2 directly after a letter...
    }else if(grepl("[a-z][2]", mystring)){
        # Split on the first 2
        mystring<-
            str_split(mystring,"[2]",n=2) %>%
            unlist() %>%
            # Insert delimiter and reinsert the 2
            {paste(.[1],delimiter,"2", .[2], sep="")}
    }else{
        mystring <- mystring
    }
    return(mystring)
}
    
