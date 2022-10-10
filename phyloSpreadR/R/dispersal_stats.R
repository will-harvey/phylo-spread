#' Dispersal statistics
#'
#' Wrapper function that returns four disperal summary statBy default, the un-weighted diffusion coefficient vis the mean per-branch diffusion
#' rate across all branches of the phylogeny. The weighted diffusion coefficient which
#' places greater emphasis on longer branches (in terms of time) can be returned on request.
#'
#' @param tree_dat dataframe with columns velocity, diffusion, gcd and branch length
#' @param target_node single clade-defining node or set of nodes can be supplied

#'
#' @return
#' @export
#'
#' @examples
dispersal_stats <- function(tree_dat = NA, target_node = NULL) {
    velocity <- calc_dispersal_velocity(tree_dat, target_node = target_node)
    velocity_w <- calc_dispersal_velocity(tree_dat, weighted = T,
                                          target_node = target_node)
    diffusion <- calc_diffusion_coef(tree_dat,
                                     target_node = target_node)
    diffusion_w <- calc_diffusion_coef(tree_dat, weighted = T,
                                       target_node = target_node)


  summary <- data.frame(statistic = c('Dispersal velocity',
                                      'Dispersal velocity (weighted)',
                                      'Diffusion coefficient',
                                      'Diffusion coefficient (weighted)'),
                        value = c(velocity, velocity_w, diffusion, diffusion_w))
  summary$value <- signif(summary$value, 5)

  summary
}
