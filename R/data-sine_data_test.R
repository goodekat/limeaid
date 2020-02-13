#' Sine Classifier Testing Data
#'
#' A dataset that contains the testing portion of the sine classifier data.
#'
#' @details The sine classifier data contains 600 observations with
#' three features and one response variable. The features, \eqn{x_1},
#' \eqn{x_2}, and \eqn{x_3}, were randomly sampled from Unif(-10, 10),
#' Unif(-10, 10), and N(0,1) distributions, respectively. A binary
#' response variable \eqn{y} was created using a rotated sine curve. In
#' particular, let \eqn{x'_1=x_1\cos(\theta)-x_2\sin(\theta)} and
#' \eqn{x'_2=x_1\sin(\theta)+x_2\cos(\theta)} where \eqn{\theta=-0.9}.
#' Then \eqn{y} is defined as
#'
#' \deqn{y=\begin{cases} 1 & \mbox{ if } x'_2 > \sin\left(x'_1\right) \\
#' 0 & \mbox{ if } x'_2 \le \sin\left(x'_1\right). \end{cases}}
#'
#' 100 observations were randomly selected from the generated data to be
#' included in the training portion of the data.
#'
#' @format A data.frame.

"sine_data_test"
