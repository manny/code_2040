import io.Source
import java.util.StringTokenizer  
import scala.collection.mutable.ArrayBuffer

object WordCount{
  def main(args: Array[String]){
    println("Enter sentences for concordance output: ")
    val text = readLine
    parseText(text)
  }


  def parseText(text: String){
    
    val st = new StringTokenizer(text)
    var word = ""

    while(st.hasMoreTokens()){
      word = st.nextToken()
      println(word)
    }
    println("finished parsing")
  }
}
