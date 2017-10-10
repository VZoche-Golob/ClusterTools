#' Creates A Heatmap With Marginal Dendrogram(s)
#'
#' @param x the numerical matrix to be plotted
#' @param dendRow a \code{stats::\link[stats]{dendrogram}} object ordering the rows
#' @param dendCol a \code{stats::\link[stats]{dendrogram}} object ordering the columns
#' @param topDGadd top dendrogram, a list passed to \code{latticeExtra::\link[latticeExtra]{dendrogramGrob}()} as '\code{add}'
#' @param rightDGadd right dendrogram, a list passed to \code{latticeExtra::\link[latticeExtra]{dendrogramGrob}()} as '\code{add}'
#' @param topDGsize passed to \code{latticeExtra::\link[latticeExtra]{dendrogramGrob}()}
#' @param rightDGsize passed to \code{latticeExtra::\link[latticeExtra]{dendrogramGrob}()}
#' @param colLabels column labels
#' @param ... parameters passed to \code{lattice::\link[lattice]{levelplot}()}, e.g. \code{main}, \code{par.settings}, ...
#'
#' @return an object of class "\code{trellis}"
#'
#' @seealso \code{\link{Lattice}}, \code{lattice::\link[lattice]{levelplot}}
#'
#' @export

dendroHeatmap <- function(
  x,
  dendRow,
  dendCol = NULL,
  topDGadd = NULL,
  rightDGadd = NULL,
  topDGsize = 10,
  rightDGsize = 10,
  colLabels = NULL,
  ...
) {

  rowOrd <- order.dendrogram(dendRow)

  if (is.null(colLabels)) {
    colLabels <- dimnames(x)[[2]]
  }

  if (!is.null(dendCol)) {
    colOrd <- order.dendrogram(dendCol)
    levelplot(
      t(x[rowOrd, colOrd]),
      aspect = "fill",
      scales = list(x = list(rot = 90, labels = colLabels), y = list(cex = 0.5)),
      colorkey = list(space = "left"),
      legend = list(
        right = list(
          fun = latticeExtra::dendrogramGrob,
          args = list(x = dendRow, ord = rowOrd, side = "right", size = rightDGsize,
                      add = rightDGadd)
        ),
        top = list(
          fun = latticeExtra::dendrogramGrob,
          args = list(x = dendCol, ord = colOrd, side = "top", size = topDGsize,
                      add = topDGadd)
        )
      ),
      ...
    )
  } else {
    levelplot(
      t(x[rowOrd, ]),
      aspect = "fill",
      scales = list(x = list(rot = 90, labels = colLabels), y = list(cex = 0.5)),
      colorkey = list(space = "left"),
      legend = list(
        right = list(
          fun = latticeExtra::dendrogramGrob,
          args = list(x = dendRow, ord = rowOrd, side = "right", size = rightDGsize,
                      add = rightDGadd)
        )
      ),
      ...
    )
  }


}
