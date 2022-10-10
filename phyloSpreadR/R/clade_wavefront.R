#' Process spatio-temporal information
#'
#' @param tree_dat dataframe with tree structure and columns location1, location2, and branch.length
#'
#' @return
#' @export
#'
#' @examples
clade_wavefront <- function(tree_dat = NA, target_node = NULL) {

  # If no target node is supplied, choose root
  if (is.null(target_node) == T) {
    target_node <- tree_dat$node[tree_dat$node == tree_dat$parent]
  }

  # require lat and long for target node
  lat_target <- tree_dat$location1[tree_dat$node == target_node]
  lat_target <- as.numeric(as.character(lat_target))
  long_target <- tree_dat$location2[tree_dat$node == target_node]
  long_target <- as.numeric(as.character(long_target))

  # ID nodes descended from target_node
  descendants <- toolkitSeqTree::node_descendants(tree_dat,
                                                  node_id = target_node)

  # Loop through nodes/rows calculating distances for descendants
  tree_dat$dist_node <- NA
  for (i in 1:nrow(tree_dat)) {

    if (tree_dat$node[i] %in% descendants) {
      # get location for node
      lat1 <- as.numeric(as.character(tree_dat$location1[i]))
      long1 <- as.numeric(as.character(tree_dat$location2[i]))
      # gcd for current node from target_node
      tree_dat$dist_node[i] <- great_circle_dist(lat1, long1, lat_target, long_target)
    }

  }
  # Distance for target node is 0
  tree_dat$dist_node[tree_dat$node == target_node] <- 0

  # Now that dist_root is complete, calc. wavefront (max dist at time of node)
  tree_dat$wavefront_node <- NA
  for (i in 1:nrow(tree_dat)) {
    if (tree_dat$node[i] %in% descendants) {
      tree_dat$wavefront_node[i] <-
      max(tree_dat$dist_node[tree_dat$date <= tree_dat$date[i]], na.rm = T)
    }
  }

  # rename columns incorporating target node
  names(tree_dat) <- gsub('^dist_node$',
                          paste('dist_node', target_node, sep = '_'),
                          names(tree_dat))
  names(tree_dat) <- gsub('^wavefront_node$',
                          paste('wavefront_node', target_node, sep = '_'),
                          names(tree_dat))

  tree_dat
}
