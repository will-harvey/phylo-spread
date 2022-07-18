#' Calculate dispersal velocity
#'
#' By default, the un-weighted dispersal velocity which is the mean per-branch velocity
#' averaged across all branches of the phylogeny. The weighted dispersal velocity which
#' places greater emphasis on longer branches (in terms of time) can be returned on request.
#'
#' @param tree_dat dataframe with columns velocity and branch length
#' @param weighted logical, should function return un-weighted or weighted statistic
#'
#' @return
#' @export
#'
#' @examples
calc_dispersal_velocity <- function(tree_dat = NA, weighted = F) {
  if (weighted == F) {
    mean(beast_dat$velocity, na.rm = T)
  } else if (weighted == T) {
    sum(beast_dat$velocity * beast_dat$branch.length, na.rm = T) /
      sum(beast_dat$branch.length, na.rm = T)
  }
}
