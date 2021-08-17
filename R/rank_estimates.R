#' Rank estimated transitions by their predicted importance
#'
#' A helper function for get_estimates, which is also used inside add_estimates. Rank-orders estimated transitions by predicted importance, such that estiamted transitions are passed to model starting parameters in order of decreasin predicted importance. Handles the very common case in which more transitons are estimated from the input data than will be fit by the downstream model. The case where fewer estimates are identified than are fit in the downstream model is handled by default values provided to to in tidy_estimates.
#'
#' @seealso \code{\link{get_estimates}}
#' @seealso \code{\link{add_estimates}}
#' @seealso \code{\link{tidy_estimates}}
#'
#' @param est_df a tibble of estimated transitions, as output by tidy_estimates
#' @param ... permit the presence additional parameters, which may be passed via ... from upstream functions, but are ignored here.
#'
#' @return do this
#' @export
rank_estimates <-
  function(est_df,
           ...) {
    est_c <-
      cluster_estimates(est_df)

    out <-
      est_c %>%
      dplyr::group_by(.data$cluster) %>%
      dplyr::mutate(pick_est = dplyr::if_else(.data$est_rank == min(.data$est_rank),
                                       true = TRUE,
                                       false = FALSE)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(.data$pick_est), .data$est_type, .data$est_rank)  %>%
      dplyr::select(-.data$pick_est) %>%
      .data$mutate(est_rank =  .data$row_number()) # overwrite est_rank
  }

#' Cluster both estimated transitions
#'
#' Groups estimated transtions, without distinguishing between major and minor, using hclust. Sorts estimations into clusters, and overwrites the input est_row with the cluster number to which the estimate was assigned. Individual clusters are defined by a minimum inter-cluster distance. With default usage, clusters the estimates using the row/measurement number of the estimates (corresponding to temperature), with minimum inter-measurment distance of 4.
#'
#' I don't recall how carefully I considered the downstream consequences of over-writing the given row. If the clustering fails, is all of this informaiton lost? It might be good to specify a safer behavior in the hclust_points function.
#'
#' @param est_df a tibble of estimated transitions, as output by tidy_estimates
#' @param .est_row_col a string, providing the name of the numeric column passed to hclust. Defaults to "est_row", the measurment number of the estimated transtion.
#' @param min_dist the minimum distance between two estimates. Defaults to 4. For a dataset with one measurement per degree, this corresponds to a minimum inter-estimate distance of 4 degrees.
#' @param ... permit the presence additional parameters, which may be passed via ... from upstream functions, but are ignored here.
#'
#' @return the input tibble, with identical format, but the input .est_row_col overwritten by the cluster number to which the estimate belongs.
#'
#' @seealso \code{\link{hclust_points}}
#' @importFrom rlang `:=`
#' @export
cluster_estimates <-
  function(est_df,
           .est_row_col = "est_row",
           min_dist = 4,
           ...) {
    dmat_c <-
      est_df %>%
      dplyr::pull({{ .est_row_col }}) %>%
      hclust_points()

    clust_cut <- stats::cutree(dmat_c, h = min_dist)

    clust_p <-
      tibble::tibble(!!.est_row_col := as.numeric(names(clust_cut)),
                     cluster = clust_cut)

    out <-
      est_df %>%
      dplyr::left_join(clust_p, by = {{ .est_row_col }})

  }

#' Identify clusters within a numeric integer vector
#'
#' A helper function to cluster_estimates. Given a numeric integer vector, constructs a self-by-self dissimilarity matrix. Applies hclust to this matrix to identify clusters within the input vector. Output is passed back to cluster_estimates.
#'
#' @seealso \code{\link{cluster_estimates}}
#'
#' @param points numeric vector of integrers containing values to be clustered.
#'
#' @return object as returned by hclust on a self-by-self distance matrix constructed from the supplied points vector.
#' @export
hclust_points <-
  function(points) {
    dmat <-
      expand.grid(points, points) %>%
      purrr::set_names(c("Var1", "Var2")) %>%
      dplyr::mutate(dec_diff = abs(.data$Var1 - .data$Var2)) %>%
      tidyr::pivot_wider(names_from = .data$Var2, values_from = .data$dec_diff) %>%
      tibble::column_to_rownames( var = "Var1") %>%
      as.matrix()

    dmat_c <-
      dmat %>%
      stats::as.dist() %>%
      stats::hclust()
  }
