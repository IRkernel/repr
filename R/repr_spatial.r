#' Representations of spatial objects: See \link[geojsonio]{geojson_list} for supported classes.
#' 
#' @param obj  The spatial object to create a representation for
#' @param ...  ignored
#' 
#' @name repr_geojson.*
NULL

#' @name repr_geojson.*
#' @export
repr_geojson.geo_list <- function(obj, ...) unclass(obj)

repr_geojson_via_geo_list <- function(obj, ...) repr_geojson(geojsonio::geojson_list(obj), ...)

#' @name repr_geojson.*
#' @export
repr_geojson.SpatialCollections <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialPolygons <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialPolygons <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialPolygonsDataFrame <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialPoints <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialPointsDataFrame <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialLines <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialLinesDataFrame <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialGrid <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialGridDataFrame <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialPixels <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialPixelsDataFrame <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialRings <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.SpatialRingsDataFrame <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.sf <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.sfg <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
repr_geojson.sfc <- repr_geojson_via_geo_list
#' @name repr_geojson.*
#' @export
