# ported from https://github.com/hrbrmstr/ggchicklet/blob/master/R/a-geom-rect.R
# changes:
#  * moved radius to aes
#  * changed unit to native
#  * renamed to `geom_rounded_rect()`

#' Rounded rectangles
#'
#' Does what [ggplot2::geom_rect()] does, only _curvier_. Use the `radius` aesthetic
#' to change the corner radius.
#'
#' @inherit ggplot2::geom_rect
#' @export
#' @examples
#' library(ggplot2)
#'
#' df <- data.frame(
#'   x = rep(c(2, 5, 7, 9, 12), 2),
#'   y = rep(c(1, 2), each = 5),
#'   z = factor(rep(1:5, each = 2)),
#'   w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2)
#' )
#'
#' ggplot(df) +
#'   geom_rounded_rect(
#'     aes(
#'       xmin = x - w / 2,
#'       xmax = x + w / 2,
#'       ymin = y,
#'       ymax = y + 1,
#'       radius = .5,
#'       fill = z
#'     ),
#'     colour = "white"
#'   )
geom_rounded_rect <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    ...,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomRoundedRect,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

GeomRoundedRect <- ggplot2::ggproto(
  "GeomRoundedRect", ggplot2::Geom,
  default_aes = ggplot2::aes(
    colour = NA, fill = "grey35", linewidth = 0.5, linetype = 1, alpha = NA,
    radius = 0.5
  ),
  required_aes = c("xmin", "xmax", "ymin", "ymax", "radius"),
  draw_panel = function(self, data, panel_params, coord, lineend = "butt", linejoin = "mitre") {
    coords <- coord$transform(data, panel_params)

    gl <- lapply(seq_along(coords$xmin), function(i) {
      grid::roundrectGrob(
        x = coords$xmin[i],
        y = coords$ymax[i],
        width = (coords$xmax[i] - coords$xmin[i]),
        height = (coords$ymax[i] - coords$ymin[i]),
        r = grid::unit(coords$radius[i], "native"),
        default.units = "native",
        just = c("left", "top"),
        gp = grid::gpar(
          col = coords$colour[i],
          fill = alpha(coords$fill[i], coords$alpha[i]),
          lwd = coords$linewidth[i] * .pt,
          lty = coords$linetype[i],
          linejoin = linejoin,
          lineend = lineend
        )
      )
    })

    grobs <- do.call(grid::gList, gl)
    grob <- grid::grobTree(children = grobs)
    grob$name <- grid::grobName(grob, "geom_rounded_rect")
    grob
  },
  draw_key = ggplot2::draw_key_polygon,
  rename_size = TRUE
)
