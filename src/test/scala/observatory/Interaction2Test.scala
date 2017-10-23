package observatory

import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait Interaction2Test extends FunSuite with Checkers {
  
  test("generateImage"){
    val stationsFile:String = "/stations.csv"
    val layer = Interaction2.availableLayers.head
    val colors = layer.colorScale
    val layerNameID = layer.layerName.id
    val year = layer.bounds.head
    
    val temperaturesFile:String = s"/$year.csv"
    val temperatures = Extraction.locationYearlyAverageRecords(
        Extraction.locateTemperatures(year, stationsFile, temperaturesFile))
      
    val grid: (Int, Int) => Double = Manipulation.makeGrid(temperatures) 
    
    val (zoom, x, y) = (0,0,0)
    val path = "target/testImages/" + year + "/" + zoom + "/" + x
    new java.io.File(path).mkdirs()
    
    Visualization2.visualizeGrid(grid, colors, zoom, x, y).output(new java.io.File(path + "/" + y + ".png"))
  }
}
