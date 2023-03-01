.osm_streetmap <- function(user_agent = getOption("HTTPUserAgent")) {
  sprintf("<GDAL_WMS><Service name=\"TMS\"><ServerUrl>https://tile.openstreetmap.org/${z}/${x}/${y}.png</ServerUrl></Service><DataWindow><UpperLeftX>-20037508.34</UpperLeftX><UpperLeftY>20037508.34</UpperLeftY><LowerRightX>20037508.34</LowerRightX><LowerRightY>-20037508.34</LowerRightY><TileLevel>18</TileLevel><TileCountX>1</TileCountX><TileCountY>1</TileCountY><YOrigin>top</YOrigin></DataWindow><Projection>EPSG:3857</Projection><BlockSizeX>256</BlockSizeX><BlockSizeY>256</BlockSizeY><BandsCount>3</BandsCount><!--<UserAgent>%s</UserAgent>--><Cache /></GDAL_WMS>",
          user_agent)
}

.virtualearth_imagery <- function() {
  "<GDAL_WMS><Service name=\"VirtualEarth\"><ServerUrl>http://a${server_num}.ortho.tiles.virtualearth.net/tiles/a${quadkey}.jpeg?g=90</ServerUrl></Service><MaxConnections>4</MaxConnections><Cache/></GDAL_WMS>"
}

#' Add background imagery
#'
#' Uses OpenStreetMap or VirtualEarth to add background imagery, or a custom source via 'dsn'.
#'  If you are publishing
#' a map using these tiles, make sure to use the proper attribution
#' (e.g., "Copyright OpenStreetMap contributors" when using an
#' OpenStreetMap-based tile set).  Ditto for VirtualEarth or any dsn you use.
#'
#' @param dsn The map source (currently 'osm' or 'virtualearth' are built-in - whatarelief streetmap, or imagery, otherwise use a GDAL DSN)
#' @param interpolate Passed to [grid::rasterGrob()]
#' @param alpha Use to make this layer semi-transparent
#' @param data,mapping Specify data and mapping to use this geom with facets
#'
#' @return A ggplot2 layer
#' @export
#' @importfrom scales rescale
#' @examples
#' \donttest{
#' library(ggplot2)
#' load_longlake_data(which = "longlake_waterdf")
#'
#' ggplot() +
#'   annotation_gdalwarp() +
#'   geom_sf(data = sf::st_transform(longlake_waterdf, "+proj=laea +lon_0=-64 +lat_0=45"), fill = NA, col = "grey50")
#'
#' pts <- do.call(cbind, maps::map(plot = F)[1:2])
#' pts <- pts[!is.na(pts[,1]), ]
#' pts <- pts[seq(1, nrow(pts), length.out = 8000), ]
#' sf <- sf::st_sf(geom = sf::st_sfc(sf::st_multipoint(pts), crs = "OGC:CRS84"))
#' ggplot() +
#'   annotation_gdalwarp(dsn = "virtualearth") +
#'   geom_sf(data = sf::st_transform(sf, "+proj=laea +lon_0=-64 +lat_0=45"), fill = NA, col = "yellow", pch = ".")
#'
#' pts2 <- pts[pts[,1] > -130 & pts[,1] < -60 & pts[,2] > 30 & pts[,2] < 80, ]
#' sf <- sf::st_sf(geom = sf::st_sfc(sf::st_multipoint(pts2), crs = "OGC:CRS84"))
#' ggplot() +
#'   annotation_gdalwarp(dsn = "virtualearth") +
#'   geom_sf(data = sf::st_transform(sf, "EPSG:5070"), fill = NA, col = "yellow", pch = ".")
#' wms_arcgis_mapserver_tms <-
#' "<GDAL_WMS><Service name=\"TMS\"><ServerUrl>http://services.arcgisonline.com/ArcGIS/rest/services/World_Street_Map/MapServer/tile/${z}/${y}/${x}</ServerUrl></Service><DataWindow><UpperLeftX>-20037508.34</UpperLeftX><UpperLeftY>20037508.34</UpperLeftY><LowerRightX>20037508.34</LowerRightX><LowerRightY>-20037508.34</LowerRightY><TileLevel>17</TileLevel><TileCountX>1</TileCountX><TileCountY>1</TileCountY><YOrigin>top</YOrigin></DataWindow><Projection>EPSG:900913</Projection><BlockSizeX>256</BlockSizeX><BlockSizeY>256</BlockSizeY><BandsCount>3</BandsCount><MaxConnections>10</MaxConnections><Cache /></GDAL_WMS>"
#' ggplot() +
#'   annotation_gdalwarp(dsn = wms_arcgis_mapserver_tms, resample ="lanczos") +
#'   geom_sf(data = sf::st_transform(sf, "EPSG:5070"), fill = NA, col = "hotpink", pch = 19, cex = 0.2)
#'
#' }
#'
annotation_gdalwarp <- function(dsn = c("osm", "virtualearth"), resample = "bilinear",
                            interpolate = FALSE, data = NULL, mapping = NULL, alpha = 1) {

  if(is.null(data)) {
    data <- data.frame(dsn = dsn, resample = resample, stringsAsFactors = FALSE)
    mapping <- ggplot2::aes(dsn = dsn, resample = resample)
  }

  c(
    ggplot2::layer(
      data = data,
      mapping = mapping,
      geom = GeomGdal,
      stat = "identity",
      position = "identity",
      params = list(
        interpolate = interpolate,
        alpha = alpha
      ),
      inherit.aes = FALSE,
      show.legend = FALSE
    )
  )
}

#' @export
#' @rdname annotation_gdal_warp
GeomGdal <- ggplot2::ggproto(
  "GeomGdal",
  ggplot2::Geom,

  extra_params = "",

  handle_na = function(data, params) {
    data
  },

  default_aes = ggplot2::aes(dsn = "osm", resample = "bilinear" ),

  draw_panel = function(
    data, panel_params, coordinates,  interpolate = TRUE, alpha = 1
  ) {

    coord_crs <- sf::st_crs(panel_params$crs)
    ex <- c(panel_params$x_range, panel_params$y_range)
    prj <- coord_crs$wkt


    ## here we need the dims to be smart, just pivot on the aspect ratio to a constant size for now
    dm <- as.integer(rep(min(dev.size("px")), 2L)) ## / facet_sqrt something something
    rat <- diff(ex[1:2])/diff(ex[3:4])
    dm <- dm * sort(c(rat, 1))
    src <- NULL
    dsn <- as.character(data$dsn[[1L]])
    resample <- as.character(data$resample[[1L]])
    if (dsn == "osm") {
      src <- .osm_streetmap()
    }
    if (dsn == "virtualearth") {
      src <- .virtualearth_imagery()
    }

    ## the user has passed in a DSN
    if (is.null(src))  src <- dsn

    err <- sf::gdal_utils("warp", source = src, destination = tf <- tempfile(fileext = ".tif"), options = c("-te", ex[1], ex[3], ex[2], ex[4],
                                                                                                     "-ts", dm[1], dm[2],
                                                                                                     "-t_srs", prj,
                                                                                     "-r", resample), quiet = TRUE)
    if (file.exists(tf)) {
      img1 <- sf::gdal_read(tf)
      img <- as.raster(aperm(attr(img1, "data"), c(2, 1, 3))/255)
    } else {
      stop(sprintf("raster read of %s failed\n", src))
    }
    if (alpha < 1) {
       img <- as.raster(matrix(scales::alpha(img, alpha = alpha), dm[2], byrow = TRUE))
    }
    corners <- data.frame(x = ex[1:2], y = ex[3:4])

    # transform corners to viewport cs
    corners_trans <- coordinates$transform(corners, panel_params)
    x_rng <- range(corners_trans$x, na.rm = TRUE)
    y_rng <- range(corners_trans$y, na.rm = TRUE)

    # return raster grob of the img
    grid::rasterGrob(
      img,
      x_rng[1], y_rng[1],
      diff(x_rng), diff(y_rng), default.units = "native",
      just = c("left","bottom"), interpolate = interpolate
    )
  }
)


