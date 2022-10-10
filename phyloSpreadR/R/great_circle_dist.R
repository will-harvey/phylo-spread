#' Great circle distance
#'
#' Given two locations supplied as latitude and longitude coordinates, calculate
#' the great circle distance between them in kilometres.
#'
#' @param lat1
#' @param long1
#' @param lat2
#' @param long2
#' @param radius
#'
#' @return
#' @export
#'
#' @examples
great_circle_dist <- function(lat1, long1, lat2, long2, radius = 6371) {

  # Convert degrees to radians
  lat1 <- deg2rad(lat1)
  long1 <- deg2rad(long1)
  lat2 <- deg2rad(lat2)
  long2 <- deg2rad(long2)

  d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * radius
  return(d) # Distance in km
}
