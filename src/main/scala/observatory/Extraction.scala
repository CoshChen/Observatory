package observatory

import java.time.LocalDate
import java.nio.file.Paths

import org.apache.spark.sql._
import org.apache.spark.sql.types._

/**
  * 1st milestone: data extraction
  */
object Extraction {
  
  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions.rand
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)
  
  lazy val spark: SparkSession = {
    SparkSession.builder().appName("Data Extraction").config("spark.master", "local[4]").getOrCreate()
  }
  
  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Int, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Double)] = {
    val stations = readStations(stationsFile)
    val temperatures = readTemperatures(temperaturesFile)
    val numRow = math.max(5000, temperatures.count()/25).toInt
    
    val joined = temperatures.join(stations)
        .where(temperatures.col("STN") === stations.col("STN") && temperatures.col("WBAN") === stations.col("WBAN"))
        .select(temperatures.col("Month"), temperatures.col("Day"), stations.col("Lat"), stations.col("Long"), temperatures.col("Temp_C"))
        .as[LocalTempRow]
        .orderBy(rand()) //Due to OOM, instead of using an entire file, I randomly select certain portion of rows.
        .limit(numRow)
        .collect()
    
    joined.par.map(row => (LocalDate.of(year, row.Month, row.Day), Location(row.Lat, row.Long), row.Temp_C)).seq
        
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Double)]): Iterable[(Location, Double)] = {
    records.par.groupBy(_._2).mapValues(
        t => {
          t.foldLeft(0.0)((ave_sum, r) => ave_sum + (r._3/t.size))
        })
        .seq
  }
  
  def fsPath(resource: String): String = {
    Paths.get(getClass.getResource(resource).toURI).toString
  }
  
  def readStations(stationsFile: String): DataFrame = {
    val dataSchema = StructType(Array(
            StructField("STN", StringType, true),
            StructField("WBAN", StringType, true),
            StructField("Lat", DoubleType, true),
            StructField("Long", DoubleType, true)
        ))
        
    val rawData = spark.read.schema(dataSchema).csv(fsPath(stationsFile))
    
    rawData.filter(rawData.col("Lat").isNotNull && rawData.col("Long").isNotNull)
        .na.fill("-")
  }
  
  def readTemperatures(temperaturesFile: String): DataFrame = {
    val dataSchema = StructType(Array(
            StructField("STN", StringType, true),
            StructField("WBAN", StringType, true),
            StructField("Month", IntegerType, true),
            StructField("Day", IntegerType, true),
            StructField("Temp_F", DoubleType, true)
        ))
        
     val rawData = spark.read.schema(dataSchema).csv(fsPath(temperaturesFile))
     
     rawData.filter(rawData.col("Month").isNotNull && rawData.col("Day").isNotNull && rawData.col("Temp_F").isNotNull)
         .na.fill("-")
         .withColumn("Temp_C", (rawData.col("Temp_F") - 32.0)*5.0/9.0)
         .filter($"Temp_C" < 200.00 && $"Temp_C" > -200.00)
         .drop("Temp_F")
  }

}

case class LocalTempRow(Month: Integer, Day: Integer, Lat: Double, Long: Double, Temp_C: Double)