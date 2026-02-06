#' Generate location latlon
#'
#' Generate new lat/lon coordinate at a given distance from a specified start-point.
#'
#' Works with angular degrees (lat/lon) used in geographic coordinate systems.
#' To work with projected corrdinates (Eastings (X) and Northings(Y)), see
#' generate_location_proj().
#'
#' @param lat start-point latitude, in degrees
#' @param lon start-point longitude, in degrees
#' @param dist target distance from initial point (in m)
#' @param bearing heading in degrees (1-360)
#' @param R mean radius of Earth (in m)
#'
#' @returns New lat/lon coordinates
#' @export
generate_location_latlon <- function(lat, lon, dist, bearing = runif(1, 0, 360), R = 6378137) {

  stopifnot("lat value not in range {-90, 90}" = !any(lat < -90 | lat > 90))
  stopifnot("lon value not in range {-180, 180}" = !any(lon < -180 | lon > 180))
  stopifnot("bearing not in range {0, 360}" = !any(bearing < 0 | bearing > 360))

  ## convert to radians
  lat1 <- lat * (pi/180)
  lon1 <- lon * (pi/180)
  a <- bearing * (pi/180)
  d <- dist

  ## generate new position
  lat2 <- asin(sin(lat1) * cos(d/R) + cos(lat1) * sin(d/R) * cos(a))
  lon2 <- lon1 + atan2(
    sin(a) * sin(d/R) * cos(lat1),
    cos(d/R) - sin(lat1) * sin(lat2)
  )

  ## convert back to degrees
  lat2 <- lat2 * (180/pi)
  lon2 <- lon2 * (180/pi)

  ## return
  return(cbind(lat2, lon2))
}
