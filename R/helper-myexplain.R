#' @importFrom assertthat assert_that
#' @import lime
#'
#' @export myexplain

# My version of the explain function.
#
# The only difference between mine and limes' is that I return a list with
# two objects. The first object is the dataframe that is returned by lime,
# and the second object is a dataframe of the perturbations.

myexplain <- function (x, explainer, labels = NULL, n_labels = NULL, n_features,
                       fn_permutations = 5000, feature_select = "auto",
                       dist_fun = "gower", kernel_width = NULL, gower_pow = 1, ...) {

  assert_that(lime:::is.data_frame_explainer(explainer))
  m_type <- lime:::model_type(explainer)
  o_type <- lime:::output_type(explainer)
  if (m_type == "regression") {
    if (!is.null(labels) || !is.null(n_labels)) {
      warning("\"labels\" and \"n_labels\" arguments are ignored when explaining regression models")
    }
    n_labels <- 1
    labels <- NULL
  }
  assert_that(is.null(labels) + is.null(n_labels) == 1, msg = "You need to choose between labels and n_labels parameters.")
  assert_that(is.count(n_features))
  assert_that(is.count(n_permutations))
  if (is.null(kernel_width)) {
    kernel_width <- sqrt(ncol(x)) * 0.75
  }
  kernel <- lime:::exp_kernel(kernel_width)
  case_perm <- lime:::permute_cases(x, n_permutations, explainer$feature_distribution,
                             explainer$bin_continuous, explainer$bin_cuts, explainer$use_density)
  case_res <- lime:::predict_model(explainer$model, explainer$preprocess(case_perm),
                            type = o_type, ...)
  case_res <- lime:::set_labels(case_res, explainer$model)
  case_ind <- split(seq_len(nrow(case_perm)), rep(seq_len(nrow(x)),
                                                  each = n_permutations))
  res <- lapply(seq_along(case_ind), function(ind) {
    i <- case_ind[[ind]]
    if (dist_fun == "gower") {
      sim <- 1 - (gower_dist(case_perm[i[1], , drop = FALSE],
                             case_perm[i, , drop = FALSE]))^gower_pow
    }
    perms <- lime:::numerify(case_perm[i, ], explainer$feature_type,
                      explainer$bin_continuous, explainer$bin_cuts)
    if (dist_fun != "gower") {
      sim <- kernel(c(0, dist(feature_scale(perms, explainer$feature_distribution,
                                            explainer$feature_type, explainer$bin_continuous),
                              method = dist_fun)[seq_len(n_permutations - 1)]))
    }
    res <- lime:::model_permutations(as.matrix(perms), case_res[i,
                                                         , drop = FALSE], sim, labels, n_labels, n_features,
                              feature_select)
    res$feature_value <- unlist(case_perm[i[1], res$feature])
    res$feature_desc <- lime:::describe_feature(res$feature, case_perm[i[1],
                                                                ], explainer$feature_type, explainer$bin_continuous,
                                         explainer$bin_cuts)
    guess <- which.max(abs(case_res[i[1], ]))
    res$case <- rownames(x)[ind]
    res$label_prob <- unname(as.matrix(case_res[i[1], ]))[match(res$label,
                                                                colnames(case_res))]
    res$data <- list(as.list(case_perm[i[1], ]))
    res$prediction <- list(as.list(case_res[i[1], ]))
    res$model_type <- m_type
    res
  })
  res <- do.call(rbind, res)
  res <- res[, c("model_type", "case", "label", "label_prob",
                 "model_r2", "model_intercept", "model_prediction", "feature",
                 "feature_value", "feature_weight", "feature_desc", "data",
                 "prediction")]
  if (m_type == "regression") {
    res$label <- NULL
    res$label_prob <- NULL
    res$prediction <- unlist(res$prediction)
  }
  return(list(explanations = as_tibble(res), perms = case_perm))

}
