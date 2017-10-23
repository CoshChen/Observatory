package observatory

object Main extends App {
  
  val stationsFile:String = "/stations.csv"
  
  Interaction2.availableLayers.foreach(layer => {
    val colors = layer.colorScale
    val layerNameID = layer.layerName.id
    
    for{year <- layer.bounds}{
      
      // load yearly temperature file
      val temperaturesFile:String = s"/$year.csv"
      val temperatures = Extraction.locationYearlyAverageRecords(
          Extraction.locateTemperatures(year, stationsFile, temperaturesFile))
      
      val grid: (Int, Int) => Double = Manipulation.makeGrid(temperatures) 
          
      // generate tiles    
      for{zoom <- (0 to 3)
        x <- (0 until 1 << zoom)
        y <- (0 until 1 << zoom)}{
          
          val path = "target/" + layerNameID + "/" + year + "/" + zoom + "/" + x
          Visualization2.visualizeGrid(grid, colors, zoom, x, y).output(new java.io.File(path + "/" + y + ".png"))
        }
    }
    
  })

}
