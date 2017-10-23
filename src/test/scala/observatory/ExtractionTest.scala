package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

trait ExtractionTest extends FunSuite {
  val year = 1975
  
  val stationsFile:String = "/stations.csv"
  val temperaturesFile:String = s"/$year.csv"
  
  lazy val stations = Extraction.readStations(stationsFile).persist()
  lazy val temps = Extraction.readTemperatures(temperaturesFile).persist()
  lazy val localTemps = Extraction.locateTemperatures(year, stationsFile, temperaturesFile)
  
  test("readStations"){
    stations.show()
    stations.printSchema()
  }
  
  test("readTemperatures"){
    temps.show()
    temps.printSchema()
  }
  
  //May cause OOM
  test("localTemps"){
    localTemps.take(10).foreach(println)
  }
  
  test("localYearlyAve"){
    Extraction.locationYearlyAverageRecords(localTemps).take(10).foreach(println)
  }
  
}
