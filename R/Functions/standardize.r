logitTransform      <- function(p) {log(p/(1-p)) }

standardize <- function(data){
    out <- (data - mean(data, na.rm = TRUE)) /
        sd(data, na.rm = TRUE)
    return(out)
}


log.standardize <- function(data){
    data <- log10(data + 1)
    out <- (data - mean(data, na.rm = TRUE)) /
        sd(data, na.rm = TRUE)
    return(out)
}

reset.warning <- function(){
    if(length(warnings()) != 0) {
    assign("last.warning", NULL, envir = baseenv())
    invisible(TRUE)
 }
}
