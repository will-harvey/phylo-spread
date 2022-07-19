#' Calculate dispersal velocity
#'
#' By default, the un-weighted dispersal velocity which is the mean per-branch velocity
#' averaged across all branches of the phylogeny. The weighted dispersal velocity which
#' places greater emphasis on longer branches (in terms of time) can be returned on request.
#'
#' @param tree_dat dataframe with columns velocity and branch length
#' @param weighted logical, should function return un-weighted or weighted statistic
#' @param target_node single clade-defining node or set of nodes can be supplied
#'
#' @return
#' @export
#'
#' @examples
calc_dispersal_velocity <- function(tree_dat = NA, weighted = F,
                                    target_node = NULL) {

  # if target node is null, calc. stat for whole tree
  if (is.null(target_node) == T) {
    if (weighted == F) {
      res <- mean(tree_dat$velocity, na.rm = T)
    } else if (weighted == T) {
      res <- sum(tree_dat$velocity * tree_dat$branch.length, na.rm = T) /
        sum(tree_dat$branch.length, na.rm = T)
    }
  }

  # if target_node is length == 1, get clade below node and calc. stat
  if (length(target_node) == 1) {
    descendants <- toolkitSeqTree::node_descendants(tree_dat,
                                                    node_id = target_node)
    tree_dat <- tree_dat[tree_dat$node %in% descendants,]

    # calc. weighted or un-weighted stat as above but for subsetted 'tree_dat'
    if (weighted == F) {
      res <- mean(tree_dat$velocity, na.rm = T)
    } else if (weighted == T) {
      res <- sum(tree_dat$velocity * tree_dat$branch.length, na.rm = T) /
        sum(tree_dat$branch.length, na.rm = T)
    }
  }

  # if target_node is length > 1, calc. stat for that set of branches
  if (length(target_node) > 1) {
    tree_dat <- tree_dat[tree_dat$node %in% target_node,]

    # calc. weighted or un-weighted stat as above but for subsetted 'tree_dat'
    if (weighted == F) {
      res <- mean(tree_dat$velocity, na.rm = T)
    } else if (weighted == T) {
      res <- sum(tree_dat$velocity * tree_dat$branch.length, na.rm = T) /
        sum(tree_dat$branch.length, na.rm = T)
    }
  }

  res
}
