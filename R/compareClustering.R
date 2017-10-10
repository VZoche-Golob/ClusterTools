#' Parallel Validation Of Clusteranalyses
#'
#' @param dataMatrix a data matrix accepted by \code{stats::\link[stats]{dist}()}
#' @param maxClusters the maximum number of clusters to evaluate
#' @param distanceMeasures a character vector of the distance measures to use
#'     (currently, only "\code{euclidean}", "\code{maximum}",
#'     "\code{manhattan}", "\code{canberra}", "\code{binary}" or "\code{minkowski}" are allowed)
#' @param clusteringMethods a character vector of cluster methods to use (currently, the following are allowed:
#'     \itemize{
#'         \item{\code{"ward.D"}, \code{"ward.D2"}, \code{"single"}, \code{"complete"}
#'         , \code{"average"}, \code{"mcquitty"}, \code{"median"}, \code{"centroid"}
#'         (use \code{stats::\link[stats]{hclust}})}
#'         \item{\code{"diana"} (uses \code{cluster::\link[cluster]{diana}})}
#'         \item{\code{"kmeans"} (uses \code{cluster::\link[cluster]{pam}})}
#'     })
#' @param sfParallel logical Should \code{\link[snowfall]{snowfall}} be used in parallel mode?
#' @param sfCpus number of cpu to use
#' @param ... passed to \code{snowfall::\link[snowfall]{sfInit}()}
#'
#' @return a \code{tibble::\link[tibble]{tibble}} with one row per distance measure,
#'     method and number of clusters
#'     from 2 to \code{k} and the columns:
#'     \describe{
#'         \item{distance}{= \code{dm}}
#'         \item{method}{= \code{method}}
#'         \item{nCluster}{= \code{k}}
#'         \item{totalAvgSilWidth}{overall average silhoutte width (\code{cluster::\link[cluster]{summary.silhouette}$avg.width})}
#'         \item{minClustAvgSilWidth}{minimal average cluster silhoutte width (\code{cluster::\link[cluster]{summary.silhouette}$clus.avg.widths})}
#'         \item{minSilWidth}{minimal silhoutte width (\code{cluster::\link[cluster]{summary.silhouette}$si.summary$`Min.`})}
#'         \item{pPosSilWidths}{percentage of positive silhoutte widths}
#'         \item{minClustJacMean}{minimal cluster bootstrap mean of Jaccard's index (\code{fpc::\link[fpc]{clusterboot}$bootmean})}
#'         \item{pClustJacOver06}{percentage of cluster bootstrap means of Jaccard's index above 0.6}
#'         \item{separationIndex}{\code{fpc::\link[fpc]{cluster.stats}$sindex}}
#'         \item{avgDistWithin}{\code{fpc::\link[fpc]{cluster.stats}$average.within}}
#'         \item{withinVsBetween}{\code{fpc::\link[fpc]{cluster.stats}$wb.ratio}}
#'     }
#'
#' @import snowfall
#'
#' @export

compareClustering <- function(
  dataMatrix,
  maxClusters,
  distanceMeasures = c("euclidean", "manhattan"),
  clusteringMethods = c("ward.D2", "single", "complete", "average", "mcquitty", "diana", "kmeans"),
  sfParallel = TRUE,
  sfCpus = 2,
  ...
) {

  require(snowfall, quietly = TRUE)

  assertive::assert_is_all_of(dataMatrix, "matrix")
  assertive::assert_all_are_true(
    c(
      distanceMeasures %in% c(
        "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
      ),
      clusteringMethods %in% c(
        "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", # via stats::hclust()
        "diana", # via cluster::diana()
        "kmeans" # via cluster::pam()
      )
    )
  )
  assertive::assert_is_a_number(maxClusters) ; assertive::assert_all_are_positive(maxClusters)
  assertive::assert_is_a_bool(sfParallel)
  assertive::assert_is_a_number(sfCpus) ; assertive::assert_all_are_positive(sfCpus)

  distList <- lapply(
    distanceMeasures,
    function(x, dm) {
      dist(dm, method = x)
    },
    dm = dataMatrix
  )
  names(distList) <- distanceMeasures

  lcM <- length(clusteringMethods)
  ldM <- length(distanceMeasures)
  distanceMeasures <- rep(distanceMeasures, each = lcM)
  clusteringMethods <- rep(clusteringMethods, ldM)
  evalList <- lapply(
    seq_along(distanceMeasures),
    function(.) {
      list(
        dm = distanceMeasures[.],
        method = clusteringMethods[.],
        k = maxClusters,
        distData = distList[[distanceMeasures[.]]]
      )
    }
  )

  sfInit(parallel = sfParallel, cpus = sfCpus, ...)
  sfLibrary(cluster)
  compareList <- sfLapply(
    evalList,
    evaluateClusters
  )
  sfStop()

  compareList %>%
    dplyr::bind_rows(.) %>%
    return(.)
}

