#' Calculate diffusion coefficient
#'
#' By default, the un-weighted diffusion coefficient vis the mean per-branch diffusion
#' rate across all branches of the phylogeny. The weighted diffusion coefficient which
#' places greater emphasis on longer branches (in terms of time) can be returned on request.
#'
#' @param tree_dat dataframe with columns diffusion, gcd and branch length
#' @param weighted logical, should function return un-weighted or weighted statistic
#'
#' @return
#' @export
#'
#' @examples
calc_diffusion_coef <- function(tree_dat = NA, weighted = F) {
  if (weighted == F) {
    mean(beast_dat$diffusion, na.rm = T)
  } else if (weighted == T) {
    sum(beast_dat$gcd^4 / beast_dat$branch.length^2, na.rm = T)
  }
}
