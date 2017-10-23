package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Visualization._

/**
  * 3rd milestone: interactive visualization
  * Reference: https://en.wikipedia.org/wiki/Web_Mercator
  */
object Interaction {

  /**
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(zoom: Int, x: Int, y: Int): Location = {
    val n = 1 << zoom
    val latRad = math.atan(math.sinh(math.Pi*(1.0 - 2.0*y/n)))
    
    Location(latRad*180.0/math.Pi, x*360.0/n - 180.0)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param zoom Zoom level
    * @param x X coordinate
    * @param y Y coordinate
    * @return A 256×256 image showing the contents of the tile defined by `x`, `y` and `zooms`
    */
  def tile(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)], zoom: Int, x: Int, y: Int): Image = {
    val width = 256
    val height = 256
    
    val pixels = (0 until width*height).par.map(index => {
      val (x_pos, y_pos) = ((index % width).toDouble/width + x, (index / width).toDouble/height + y)
      val location = {
        val n = 1 << zoom
        val latRad = math.atan(math.sinh(math.Pi*(1.0 - 2.0*y_pos/n)))
        Location(latRad*180.0/math.Pi, x_pos*360.0/n - 180.0)
      }
      val predTemp = Visualization.predictTemperature(temperatures, location)
      val predColor = Visualization.interpolateColor(colors, predTemp)
      
      Pixel.apply(predColor.red, predColor.green, predColor.blue, 128)
    }).toArray
    
    Image.apply(width, height, pixels)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Int, Data)],
    generateImage: (Int, Int, Int, Int, Data) => Unit
  ): Unit = {
    for{
      (year, data) <- yearlyData
      zoom <- (0 to 3)
      x <- (0 until 1 << zoom)
      y <- (0 until 1 << zoom)
    }{
      generateImage(year, zoom, x, y, data)
    }
  }

}
