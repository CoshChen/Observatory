package observatory

import com.sksamuel.scrimage.{Image, Pixel}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Double)], location: Location): Double = {
    val targetLat = location.lat
    val targetLon = location.lon
    
    //Return distance(km) from other location to the given location
    def getDistTo(other: Location): Double = {
      val rad = math.Pi/180.0
      6371*math.acos(
              math.sin(targetLat*rad)*math.sin(other.lat*rad) 
              + math.cos(targetLat*rad)*math.cos(other.lat*rad)*math.cos((targetLon - other.lon)*rad)
           )
    }
    
    def weightedSum(power: Double, distTemp: Iterable[(Double, Double)]): Double = {
      //(Distance, Temperature) => (InverseDistancePower, WeightedTemperature)
      val weightedPair = distTemp.map(pair => (1.0/math.pow(pair._1, power), pair._2/math.pow(pair._1, power)))
      val total = weightedPair.foldLeft((0.0,0.0))((ws, pair) => (ws._1+ pair._1, ws._2 + pair._2))
      
      total._2/total._1
    }
    
    // (Location, Temperature) => (Distance, Temperature)
    val distTemp = temperatures.map(pair => (getDistTo(pair._1), pair._2))
    
    distTemp.find(_._1 < 1.0) match{
      case Some((_, temp)) => temp
      case None => weightedSum(3.0, distTemp)
    }
    
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Double, Color)], value: Double): Color = {
    //Obtain an estimated color for given value
    def getInterColor(value: Double): Color = {
      val sortedPartition = points.toList.sortWith((pairA, pairB) => pairA._1 < pairB._1)
                                .partition(pair => pair._1 < value)
                                
      if(sortedPartition._1.isEmpty) sortedPartition._2.head._2
      else if(sortedPartition._2.isEmpty) sortedPartition._1.last._2
      else{
        val (lowerPair, upperPair) = (sortedPartition._1.last, sortedPartition._2.head)
        val diffValue = upperPair._1 - lowerPair._1
        val midDiffValue = value - lowerPair._1
        val (lowerColor, upperColor) = (lowerPair._2, upperPair._2)
        
        val red = math.round((upperColor.red - lowerColor.red)*midDiffValue/diffValue + lowerColor.red)
        val green = math.round((upperColor.green - lowerColor.green)*midDiffValue/diffValue + lowerColor.green)
        val blue = math.round((upperColor.blue - lowerColor.blue)*midDiffValue/diffValue + lowerColor.blue)
        Color(red.toInt, green.toInt, blue.toInt)
      }
    }
    
    points.find(_._1 == value) match{
      case Some((_, color)) => color
      case None => getInterColor(value)
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Double)], colors: Iterable[(Double, Color)]): Image = {
    val width = 360
    val height = 180
    
    def idxToLocation(index: Integer): Location = {
      val x = index % width
      val y = index / width
      
      Location(90.0 - y * 180.0/height, x*360.0/width - 180.0)
    }
    
    val pixels = (0 until width*height).par.map(index => {
          val predTemp = predictTemperature(temperatures, idxToLocation(index))
          val predColor = interpolateColor(colors, predTemp)
          
          Pixel.apply(predColor.red, predColor.green, predColor.blue, 0)
          }).toArray
    
    Image.apply(width, height, pixels)
  }

}

