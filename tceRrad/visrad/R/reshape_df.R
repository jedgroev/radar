#' reshape dataframe 
#'
#' Uses a row and column and a parameter variable to reshape a data frame 

#' @param data A data frame obtained from dbGetQuery_levelplots
#' @param rowvar The variable that represents the rows in the reshaped data frame
#' @param colvar The variable that represents the columns the reshaped data frame 
#' @param variable The variable representing the values in the reshaped data frame 

#' @keywords radar, aggregate, reshape

#' @examples
#' connect to database 
#' library(RPostgreSQL)
#' con <- dbConnect('PostgreSQL', 
#'                  dbname='staticeemshaven', 
#'                  host='hostname', 
#'                  user='username', 
#'                  password = 'password')
#' df <- dbGetQuery_levelplots()
#' density_log <- reshape_df(df,
#'               rowvar='altitude_bin',
#'               colvar='timestamp_bin',
#'               variable='density_log')

reshape_df <- function(data = df, 
                       rowvar='altitude_bin', 
                       colvar='timestamp_bin',
                       variable='speed') 
      {
      nrows <- length(unique(data[,rowvar]))
      ncols <- length(unique(data[,colvar]))
      mm <- as.data.frame(matrix(data=NA, nrows,ncols))
      colnames(mm) <- unique(data[, colvar])
      rownames(mm) <- unique(data[, rowvar])
      data <- data[,c(rowvar,colvar,variable)]
      colnames(data) <- c('rowvar','colvar','variable') 
        for(i in 1:ncols)
          {
          samp <- subset(data,colvar == unique(data$colvar)[i])[,c('rowvar', 'variable')]
          samp <- samp[order(samp[,'rowvar']),'variable']
          mm[,i] <- c(samp, rep(NA, nrows - length(samp)))
          }
      as.matrix(mm)
      
      # change the column names from timestamp to hour
      x <- (as.integer(as.POSIXct(colnames(mm))) - as.integer(as.POSIXct(paste0(substr(colnames(mm),1,10),' 00:00:00'))))/60/60
      colnames(mm) <- x
      
      return(mm)
      }