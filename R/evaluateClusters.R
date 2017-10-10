#' Validating A Clusteranalysis
#'
#' Called by \code{\link{compareClustering}()}.
#'
#' @param xList a list with the following elements:
#' \describe{
#'     \item{dm}{a string describing a distance measure (currently, only "\code{euclidean}", "\code{maximum}",
#'     "\code{manhattan}", "\code{canberra}", "\code{binary}" or "\code{minkowski}" are allowed)}
#'     \item{method}{a string naming a clustering method (currently, the following are allowed:
#'     \itemize{
#'         \item{\code{"ward.D"}, \code{"ward.D2"}, \code{"single"}, \code{"complete"}
#'         , \code{"average"}, \code{"mcquitty"}, \code{"median"}, \code{"centroid"}
#'         (use \code{stats::\link[stats]{hclust}})}
#'         \item{\code{"diana"} (uses \code{cluster::\link[cluster]{diana}})}
#'         \item{\code{"kmeans"} (uses \code{cluster::\link[cluster]{pam}})}
#'     }
#'     )}
#'     \item{k}{the maximum number of clusters}
#'     \item{distData}{a distance matrix}
#' }
#'
#' @return a \code{tibble::\link[tibble]{tibble}} with one row per number of clusters
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
#' @export

evaluateClusters <- function(
  xList
) {

  assertive::assert_all_are_true(
    c(
      xList$dm %in% c(
        "euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"
      ),
      xList$method %in% c(
        "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median", "centroid", # via stats::hclust()
        "diana", # via cluster::diana()
        "kmeans" # via cluster::pam()
      )
    )
  )
  assertive::assert_is_a_number(xList$k)
  assertive::assert_all_are_positive(xList$k)
  assertive::assert_is_all_of(xList$distData, "dist")

  if (xList$method == "kmeans") {

    out <- rep(NULL, 10)
    for (i in 2:xList$k) {

      clusterObj <- cluster::pam(xList$distData, i, diss = TRUE)
      sep <- fpc::cluster.stats(xList$distData, clusterObj$clustering)
      sil <- cluster::silhouette(clusterObj$clustering, xList$distData)
      stab <- fpc::clusterboot(
        xList$distData,
        B = 1000,
        distances = TRUE,
        clustermethod = fpc::claraCBI,
        bootmethod = "boot",
        bscompare = FALSE,
        multipleboot = FALSE,
        k = i,
        diss = TRUE,
        usepam = TRUE,
        showplots = FALSE,
        seed = 0815,
        count = FALSE
      )

      out <- rbind(
        out,
        c(
          i,
          summary(sil)$avg.width,
          min(summary(sil)$clus.avg.widths),
          summary(sil)$si.summary["Min."],
          sum(sil[, 3] > 0) / attr(xList$distData, "Size"),
          min(stab$bootmean),
          sum(stab$bootmean > 0.6) / i,
          sep$sindex,
          sep$average.within,
          sep$wb.ratio
        )
      )

    }

  } else {
    if (xList$method == "diana") {
      clusterObj <- cluster::diana(xList$distData, diss = TRUE)
    } else {
      clusterObj <- hclust(xList$distData, method = xList$method)
    }

    out <- rep(NULL, 10)
    for (i in 2:xList$k) {

      sep <- fpc::cluster.stats(xList$distData, cutree(clusterObj, k = i))
      sil <- cluster::silhouette(cutree(clusterObj, k = i), xList$distData)
      if (xList$method == "diana") {
        stab <- fpc::clusterboot(
          xList$distData,
          B = 1000,
          distances = TRUE,
          clustermethod = ClusterTools::dianaCBI,
          bootmethod = "boot",
          bscompare = FALSE,
          multipleboot = FALSE,
          k = i,
          showplots = FALSE,
          seed = 0815,
          count = FALSE
        )
      } else {
        stab <- fpc::clusterboot(
          xList$distData,
          B = 1000,
          distances = TRUE,
          clustermethod = fpc::disthclustCBI,
          bootmethod = "boot",
          bscompare = FALSE,
          multipleboot = FALSE,
          k = i,
          cut = "number",
          method = xList$method,
          showplots = FALSE,
          seed = 0815,
          count = FALSE
        )
      }

      out <- rbind(
        out,
        c(
          i,
          summary(sil)$avg.width,
          min(summary(sil)$clus.avg.widths),
          summary(sil)$si.summary["Min."],
          sum(sil[, 3] > 0) / attr(xList$distData, "Size"),
          min(stab$bootmean),
          sum(stab$bootmean > 0.6) / i,
          sep$sindex,
          sep$average.within,
          sep$wb.ratio
        )
      )

    }

  }


  dimnames(out) <- list(
    NULL,
    c(
      "nCluster",
      "totalAvgSilWidth",
      "minClustAvgSilWidth",
      "minSilWidth",
      "pPosSilWidths",
      "minClustJacMean",
      "pClustJacOver06",
      "separationIndex",
      "avgDistWithin",
      "withinVsBetween"
    )
  )
  out <- dplyr::bind_cols(
      tibble::as_tibble(
        list(
          distance = rep(xList$dm, nrow(out)),
          method = rep(xList$method, nrow(out))
        )
      ),
      tibble::as_tibble(out)
    )

  return(out)
}
