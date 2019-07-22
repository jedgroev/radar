#' dbGetQuery levelplots
#'
#' Export aggregated data from robin radar eemshaven database to generate level plots 

#' @param PARstarttime Start time of the radar data that should be extracted
#' @param PARendtime End time of the radar data that should be extracted
#' @param PARaltbin The aggregation bins for altitude in meters (50 meter)
#' @param PARtimebin The aggregation bins for the time in minutes (15 minutes)
#' @param PARaltmax The maximum altitude in meters for which metrics are extracted (500 meter)
#' @param PARclassification_id Classification_ids of interest (see database for the meaning of the codes)
#' @param PARdegrees The angles of interest in degrees 
#' @param pseudorep Account for pseudoreplication by averaging per track or not

#' @keywords RpostgreSQL, radar, aggregate

#' @examples 
#' # connect to database 
#' library(RPostgreSQL)
#' con <- dbConnect('PostgreSQL', dbname='staticeemshaven', host='hostname', user='username', password = 'password')

#' #run functions with standard settings 
#' a <- dbGetQuery_levelplots()
#' get data 
#' data <- a$data
#' get SQL QUERY()
#' query <- a$query


dbGetQuery_levelplots <- function(avoidpseudorep=TRUE,
                                  PARstarttime='2018-10-10 15:00:00', 
                                  PARendtime='2018-10-11 09:00:00', 
                                  PARaltbin=50, 
                                  PARtimebin=15,
                                  PARaltmax=500,
                                  PARclassification_id='5,8',
                                  PARdegrees=c(0,135,-54,-45)){
  
# Import data from database 
if(avoidpseudorep==FALSE){
  ## Average per altitude and timestamp (avg)
query <- paste0("
 SELECT 
    timestamp_bin,
    altitude_bin,
    speed,
    CASE WHEN direction < 0 THEN 360 + direction ELSE direction END direction,
    density_log,
    density
    FROM (
  SELECT
    timestamp_bin,
    altitude_bin,
    avg(velocity) speed,
    degrees(atan2(sum(sin(heading)), sum(cos(heading)))) direction,
    log(count(*)) density_log,
    count(*) density
  FROM (
    SELECT
      date_trunc('hour', timestamp) + cast('",PARtimebin, " min' AS interval) * floor((date_part('minute', timestamp)::float + date_part('second', timestamp) / 60.)::float / ",PARtimebin,"::float) timestamp_bin,
      floor(st_z (position) /", PARaltbin,") * ", PARaltbin, " altitude_bin,
      velocity,
      heading
    FROM
      public.trackestimate
      JOIN public.track ON (trackestimate.track_id = track.id)
    WHERE
      timestamp BETWEEN '", PARstarttime,
      "' AND '", PARendtime,
      "' AND classification_id in (",PARclassification_id,
      ") AND (floor(st_z (position) /", PARaltbin,") * ", PARaltbin, ") <= ",PARaltmax," ) x 
  WHERE 
    (heading BETWEEN radians(",PARdegrees[1],") AND radians(",PARdegrees[2],")
    OR heading BETWEEN radians(",PARdegrees[3],") AND radians(",PARdegrees[4],")) 
  GROUP BY
    timestamp_bin, altitude_bin
  ORDER BY
    altitude_bin,
    timestamp_bin ) z")
  } else {
## Average of averaged track per altitude and timestamp (avg_track)
query <- paste0("
   SELECT 
    timestamp_bin,
    altitude_bin,
    speed,
    CASE WHEN direction < 0 THEN 360 + direction ELSE direction END direction,
    density_log,
    density
    FROM (
  SELECT
    timestamp_bin,
    floor(altitude/", PARaltbin,") * ", PARaltbin, " altitude_bin, 
    avg(speed) speed,
    degrees(atan2(sum(sin(direction_circ)), sum(cos(direction_circ)))) direction, 
    log(count(*)) density_log,
    count(*) density
  FROM (
    SELECT
      track_id,
      timestamp_bin,
      altitude_bin,
      avg(altitude) altitude,
      avg(velocity) speed,
      atan2(sum(sin(heading)), sum(cos(heading))) direction_circ,       
      log(count(*)),
      count(*)
    FROM (
      SELECT
      date_trunc('hour', timestamp) + cast('",PARtimebin, " min' AS interval) * floor((date_part('minute', timestamp)::float + date_part('second', timestamp) / 60.)::float / ",PARtimebin,"::float) timestamp_bin,
      floor(st_z (position) /", PARaltbin,") * ", PARaltbin, " altitude_bin,
      velocity,
      st_z (position) altitude,
      heading, track_id
    FROM
      public.trackestimate
      JOIN public.track ON (trackestimate.track_id = track.id)
    WHERE
      timestamp BETWEEN '", PARstarttime,
      "' AND '", PARendtime,
      "' AND classification_id in (",PARclassification_id,") AND  
      (floor(st_z (position) /", PARaltbin,") * ", PARaltbin, ") <= ",PARaltmax,") x
    GROUP BY
      track_id,
      timestamp_bin,
      altitude_bin
    ORDER BY
      altitude_bin,
      timestamp_bin) y
  WHERE (direction_circ BETWEEN radians(",PARdegrees[1],") AND radians(",PARdegrees[2],")
    OR direction_circ BETWEEN radians(",PARdegrees[3],") AND radians(",PARdegrees[4],")) 
  GROUP BY
    timestamp_bin,
    floor(altitude /", PARaltbin,") * ", PARaltbin, " 
  ORDER BY
    altitude_bin,
    timestamp_bin
  ) z")
  }
  
data <- dbGetQuery(con,query)

return(list(data=data, query=query))

}

