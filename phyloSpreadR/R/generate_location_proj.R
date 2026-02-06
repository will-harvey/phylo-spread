#' Generate location projected
#'
#' Generate new projected coordinate at a given distance from a specified start-point.
#'
#' Works with projected corrdinates (Eastings (X) and Northings(Y)).
#' To work with angular degrees (lat/lon) used in geographic coordinate systems, see
#' generate_location_latlon().

#'
#' @param X projected X coordinate
#' @param Y projected Y coordinate
#' @param dist target distance from initial point (in m)
#' @param bearing heading in degrees (0-360)
#'
#' @returns new X/Y coordinate {d} m from initial, in degrees
#' @export
generate_location_proj <- function(X, Y, dist, bearing) {

  stopifnot("bearing not in range {0, 360}" = !any(bearing < 0 | bearing > 360))

  bearing_rad <- bearing * (pi / 180)
  d <- dist
  delta_x <- d * sin(bearing_rad)
  delta_y <- d * cos(bearing_rad)
  new_x <- X + delta_x
  new_y <- Y + delta_y
  return(cbind(new_x, new_y))
}
