#' Clusterboot Interface To Diana
#'
#' Provides an interface to \code{cluster::\link[cluster]{diana}()} that allows the execution
#' from \code{fpc::\link[fpc]{clusterboot}()}.
#'
#' @param x a distance matrix
#' @param k the number of clusters
#'
#' @return A \code{list} containing the following components:
#' \describe{
#'     \item{result}{an object of class "\code{diana}",
#'     the output of the call to \code{cluster::\link[cluster]{diana}()}}
#'     \item{nc}{the number of clusters (\code{k})}
#'     \item{clusterlist}{a list consisting of logical vectors of length of
#'     the number of data points for each cluster indicating whether a point is a member of this cluster}
#'     \item{partition}{an integer vector of length of
#'     the number of data points partitioning the data}
#'     \item{clustermethod}{"\code{cluster::diana}"}
#' }
#'
#' @seealso \code{fpc::\link[fpc]{clusterboot}()}
#'
#' @export
dianaCBI <- function(
  x,
  k
) {

  assertive::assert_is_a_number(k)
  assertive::assert_all_are_positive(k)

  x <- as.dist(x)

  dianaObj <- cluster::diana(x, diss = TRUE)
  dianaCut <- cutree(as.hclust(dianaObj), k = k)

  out <- list(
    result = list(dianaObj),
    nc = k,
    clusterlist = lapply(
      1:k,
      function(., clusters) clusters == .,
      clusters = dianaCut
    ),
    partition = dianaCut,
    clustermethod = "cluster::diana"
  )
}
