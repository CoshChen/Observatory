package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import Visualization._

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  /**
    * @param x X coordinate between 0 and 1
    * @param y Y coordinate between 0 and 1
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    x: Double,
    y: Double,
    d00: Double,
    d01: Double,
    d10: Double,
    d11: Double
  ): Double = {
    y*((1.0-x)*d01 + x*d11) + (1.0-y)*((1.0-x)*d00 + x*d10)
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param zoom Zoom level of the tile to visualize
    * @param x X value of the tile to visualize
    * @param y Y value of the tile to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: (Int, Int) => Double,
    colors: Iterable[(Double, Color)],
    zoom: Int,
    x: Int,
    y: Int
  ): Image = {
    val width = 256
    val height = 256
    
    val pixels = (0 until width*height).par.map(index => {
        val (x_pos,y_pos) = ((index % width).toDouble/width + x, (index / height).toDouble/height + y)
        val location = {
          val n = 1 << zoom
          val latRad = math.atan(math.sinh(math.Pi*(1.0 - 2.0*y_pos/n)))
          Location(latRad*180.0/math.Pi, x_pos*360.0/n - 180.0)
        }
        val (x0, x1, y1, y0) = (math.floor(location.lon).toInt, math.ceil(location.lon).toInt, 
            math.floor(location.lat).toInt, math.ceil(location.lat).toInt)
        val (d00,d01,d10,d11) = (grid(y0,x0), grid(y1,x0), grid(y0,x1), grid(y1,x1))
        
        val predTemp = bilinearInterpolation(location.lon - x0, y0 - location.lat, d00,d01,d10,d11)
        val predColor = Visualization.interpolateColor(colors, predTemp)
        
        Pixel.apply(predColor.red, predColor.green, predColor.blue, 128)
        }).toArray
    
     
     Image.apply(width, height, pixels)
  }

}
