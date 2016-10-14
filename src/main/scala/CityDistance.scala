import scalaj.http._
import play.api.libs.json._

object CityDistance extends App {  

  def getDistinceFromLatLonInKm(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    def deg2Rad(deg: Double): Double = {
      deg * (Math.PI / 180)
    }

    val R = 6371
    val dLat = deg2Rad(lat2-lat1)
    val dLon = deg2Rad(lon2-lon1)

    val a = 
      Math.sin(dLat/2) * Math.sin(dLat/2) +
      Math.cos(deg2Rad(lat1)) * Math.cos(deg2Rad(lat2)) *
      Math.sin(dLon/2) * Math.sin(dLon/2)

    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a))
    R * c    
  }

  def getCityLocation(name: String): Option[((Double,Double), String)] = {
    val response: HttpResponse[JsValue] = Http("https://maps.googleapis.com/maps/api/geocode/json").
    proxy("127.0.0.1", 1080).
    header("accept-language", "zh-CN,zh;q=0.8,en;q=0.6").
    param("address",name).    
    execute(parser = {inputStream => Json.parse(inputStream)})
    val location = ((response.body \ "results")(0) \ "geometry" \ "location")

    for {
      lat <- (location \ "lat").toOption
      lng <- (location \ "lng").toOption
      name <- ((response.body \ "results")(0) \ "formatted_address").toOption
    } yield ((lat.asInstanceOf[JsNumber].value.doubleValue, 
             lng.asInstanceOf[JsNumber].value.doubleValue), 
             name.asInstanceOf[JsString].value)
  }

  val Array(city1, city2) = args

  for {
    ((lat1, lon1), cityName1) <- getCityLocation(city1)
    ((lat2, lon2), cityName2) <- getCityLocation(city2)
    dist = getDistinceFromLatLonInKm(lat1, lon1, lat2, lon2)
  } { 
    println(s"""$cityName1 to $cityName2 distinc is $dist killomitter""")
  }
}
