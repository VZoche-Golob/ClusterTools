#' Plot Of The Comparative Cluster Validation
#'
#' @param results a \code{tibble::\link[tibble]{tibble}} with one row per distance measure,
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
#' @return a plot generated using \code{\link[ggplot2]{ggplot2}}
#'
#' @export

plotComparison <- function(
  result
) {

  result %>%
    tidyr::gather("key", "value", 4:12) %>%
    ggplot(., aes(x = nCluster, y = value)) +
    geom_line(aes(color = method)) +
    facet_grid(key ~ distance, scales = "free_y", labeller = labeller(key = label_value, distance = label_both)) +
    theme(axis.title.y = element_blank()) %>%
    return(.)

}
