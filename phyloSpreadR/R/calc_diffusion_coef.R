#' Calculate diffusion coefficient
#'
#' By default, the un-weighted diffusion coefficient vis the mean per-branch diffusion
#' rate across all branches of the phylogeny. The weighted diffusion coefficient which
#' places greater emphasis on longer branches (in terms of time) can be returned on request.
#'
#' @param tree_dat dataframe with columns diffusion, gcd and branch length
#' @param weighted logical, should function return un-weighted or weighted statistic
#' @param target_node single clade-defining node or set of nodes can be supplied
#'
#' @return
#' @export
#'
#' @examples
calc_diffusion_coef <- function(tree_dat = NA, weighted = F) {

    # if target node is null, calc. stat for whole tree
    if (is.null(target_node) == T) {
      if (weighted == F) {
        res <- mean(tree_dat$diffusion, na.rm = T)
      } else if (weighted == T) {
        res <- sum(tree_dat$gcd^4 / tree_dat$branch.length^2, na.rm = T)
      }
    }

    # if target_node is length == 1, get clade below node and calc. stat
    if (length(target_node) == 1) {
      descendants <- toolkitSeqTree::node_descendants(tree_dat,
                                                      node_id = target_node)
      tree_dat <- tree_dat[tree_dat$node %in% descendants,]

      # calc. weighted or un-weighted stat as above but for subsetted 'tree_dat'
      if (weighted == F) {
        res <- mean(tree_dat$diffusion, na.rm = T)
      } else if (weighted == T) {
        res <- sum(tree_dat$gcd^4 / tree_dat$branch.length^2, na.rm = T)
      }

    }

    # if target_node is length > 1, calc. stat for that set of branches
    if (length(target_node) > 1) {
      tree_dat <- tree_dat[tree_dat$node %in% target_node,]

      # calc. weighted or un-weighted stat as above but for subsetted 'tree_dat'
      if (weighted == F) {
        res <- mean(tree_dat$diffusion, na.rm = T)
      } else if (weighted == T) {
        res <- sum(tree_dat$gcd^4 / tree_dat$branch.length^2, na.rm = T)
      }
    }

    res
}
