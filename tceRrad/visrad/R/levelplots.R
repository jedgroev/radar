#' levelplots 
#'
#' Generate a levelplot

#' @param data A data frame obtained from reshape_df
#' @param autoscale if autoscale is true than the color scale bins and min-max is automatically set
#' @param main title of the plot 
#' @param col colors to use
#' @param at the color bins
#' @export
#' @examples
#' # Connect to database 
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
#' direction <- reshape_df(df,
#'               rowvar='altitude_bin',
#'               colvar='timestamp_bin',
#'               variable='density')
#' plot_density_log <- levelplots(density_log,
#'                                autoscale=TRUE, 
#'                                main='density [log]', 
#'                                col= colorRampPalette(c('yellow','red','black'))(50))
#' plot_direction <- levelplots(data=direction,
#'                              autoscale=FALSE, 
#'                              main='direction [0=North]',
#'                              col= colorRampPalette(c('red', 'yellow', 'green', 'cyan', 'blue', 'pink', 'red'))(360),
#'                              at=seq(0, 360, length.out=360))
#' grid.arrange(plot_density_log, plot_direction)

levelplots <- function(data= density, autoscale=TRUE, main='', col= colorRampPalette(c('yellow','red','black'))(50), at= seq(0,1000,50)) 
  {
  if("lattice" %in% rownames(installed.packages()) == FALSE) {install.packages("lattice")}
  if("gridExtra" %in% rownames(installed.packages()) == FALSE) {install.packages("gridExtra")}

  require(lattice)
  require(gridExtra)
  x <- as.numeric(colnames(data))
  if(autoscale==TRUE){
  plotres <- levelplot(t(data),
                        xlab=list('time (h)',cex=0.7),
                        ylab=list('altitude (m)',cex=0.7), 
                        main=list(main,cex=0.8),
                        scales=list(x=list(at=seq(1,length(x),4), 
                                           labels=unique(round(x)),rot=0),cex=0.4),
                        col.regions = col)
  } else {
  plotres <- levelplot(t(data),
                        xlab=list('time (h)',cex=0.7),
                        ylab=list('altitude (m)',cex=0.7), 
                        main=list(main,cex=0.8),
                        scales=list(x=list(at=seq(1,length(x),4), 
                                           labels=unique(round(x)),rot=0),cex=0.4),
                        col.regions = col,
                        at=at)
  } 
  return(plotres)
}
