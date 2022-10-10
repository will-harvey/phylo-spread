#' Process spatio-temporal information
#'
#' @param tree_dat dataframe with tree structure and columns location1, location2, and branch.length
#'
#' @return
#' @export
#'
#' @examples
process_spatiotemporal <- function(tree_dat = NA) {

  # Use lat-lon in tree_dat to calc. distances covered by each branch
  # flat distance, great circle distance (gcd), and gcd from root
  tree_dat$dist <- NA
  tree_dat$gcd <- NA
  tree_dat$dist_root <- NA

  # dist_root requires lat and long for root (node == parent)
  lat_root <- tree_dat$location1[tree_dat$node == tree_dat$parent]
  lat_root <- as.numeric(as.character(lat_root))
  long_root <- tree_dat$location2[tree_dat$node == tree_dat$parent]
  long_root <- as.numeric(as.character(long_root))

  # Loop through nodes/rows calculating distances
  for (i in 1:nrow(tree_dat)) {
    # get locations
    lat1 <- as.numeric(as.character(tree_dat$location1[i]))
    long1 <- as.numeric(as.character(tree_dat$location2[i]))
    curr_parent <- tree_dat$parent[i]
    lat2 <- as.numeric(as.character(tree_dat$location1[tree_dat$node == curr_parent]))
    long2 <- as.numeric(as.character(tree_dat$location2[tree_dat$node == curr_parent]))

    # Fill dist, gcd and gcd to root (dist_root)
    tree_dat$dist[i] <- sqrt((lat1-lat2)^2 + (long1-long2)^2)
    tree_dat$gcd[i] <- great_circle_dist(lat1, long1, lat2, long2)
    tree_dat$dist_root[i] <- great_circle_dist(lat1, long1, lat_root, long_root)
  }

  # For root, branch doesn't cover distance and node is distance = 0 from itself
  tree_dat$dist[tree_dat$node == tree_dat$parent] <- NA
  tree_dat$gcd[tree_dat$node == tree_dat$parent] <- NA
  tree_dat$dist_root[tree_dat$node == tree_dat$parent] <- 0

  # Now that dist_root is complete, calc. wavefront (max dist at time of node)
  tree_dat$wavefront <- NA
  for (i in 1:nrow(tree_dat)) {
    tree_dat$wavefront[i] <-
      max(tree_dat$dist_root[tree_dat$date <= tree_dat$date[i]])
  }

  # Calculate velocity per branch
  tree_dat$velocity <- tree_dat$gcd / tree_dat$branch.length
  # Calculate diffusion rate per branch
  tree_dat$diffusion <- tree_dat$gcd^4 / tree_dat$branch.length^2

  tree_dat
}
