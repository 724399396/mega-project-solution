import scalafx.application.JFXApp
import scalafx.beans.property.{ReadOnlyObjectWrapper}
import javafx.scene.media.{Media, MediaPlayer}
import java.util.{Date, Locale}
import java.text.SimpleDateFormat



object AlarmClock extends JFXApp {


  private val _mediaPlayer = new ReadOnlyObjectWrapper[MediaPlayer](this, "mediaPlayer").readOnlyProperty

  def init() = {
    try {
      val media = new Media("file:///media/weili/新加卷1/CloudMusic/Alan-Walker-Fade.mp3") {
        metadata.onChange((_, change) => {
          change match {
            case Add(key, added) => handleMetadata(key, added)
            case _ =>
          }
        })
      }

      _mediaPlayer() = new MediaPlayer(media) {
        // Handle errors during playback
        onError = {
          val errorMessage = media.error().getMessage
          println("MediaPlayer Error: " + errorMessage)
        }
      }
    } catch {
      // Handle construction errors
      case re: RuntimeException => println("Caught Exception: " + re.getMessage)
    }
  }

 

  def mains(args: Array[String]): Unit = {
    val Array(mode, time) = args

    mode match {
      case "alarm" =>
        val setTime = new SimpleDateFormat("yyyy/MM/dd HH:mm:ss", Locale.CHINESE).parse(time)
        println(setTime)
        val now = new Date()
        while (now.compareTo(setTime) < 0) {
        }
    }
  }
}
