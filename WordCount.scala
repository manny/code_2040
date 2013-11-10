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

    var sentenceCount = 0
    var incrementCount = false

    var wordList = ArrayBuffer[String]()
    var concordanceList = ArrayBuffer[ArrayBuffer[Int]]()

    val st = new StringTokenizer(text, " ,;:(){}[]\"")
    var word = ""

    while(st.hasMoreTokens()){
      word = st.nextToken()
      word = word.toLowerCase()

      if(singlePunc(word)){
        incrementCount = true 
      }else if(wordCheck(word)){
        if(word.endsWith(".") || word.endsWith("?") || word.endsWith("!")){
          if(endOfSentence(word)){
            word = word.substring(0, word.length-1)
            incrementCount = true
          }
        }
        //if found word, add sentence occurence and increment count
        if(wordList.contains(word)){
          concordanceList(wordList.indexOf(word))(0)+=1
          concordanceList(wordList.indexOf(word))+=sentenceCount
        }else{
        //if not found, add word and a new entry to the concordanceList
          wordList+=word
          var newEntry = ArrayBuffer[Int]()
          newEntry+=(1, sentenceCount)
          concordanceList+=newEntry
        }
      }

      if(incrementCount){
        sentenceCount+=1
        incrementCount = false
      }
    }
    println(wordList.toString)
    println(concordanceList.toString)

    println("finished parsing")
    outputConcordence(wordList, concordanceList)
  }
  
  def endOfSentence(word: String) : Boolean = {
    word.indexOf('.') == word.lastIndexOf('.')
  }

  def wordCheck(word: String) : Boolean = {
    word.charAt(0).isLetter
  }
  
  def singlePunc(word: String) : Boolean = {
    (word.equals(".") || word.equals("?") || word.equals("!"))
  }

  def outputConcordence(wordList: ArrayBuffer[String], concordanceList: ArrayBuffer[ArrayBuffer[Int]]){ 
    var i = 0
    for(i <- 0 until wordList.length){
      wordList(i) = wordList(i) + " " + formatEntry(concordanceList(i))
      println(wordList(i))
      println("")
    }
  }

  def formatEntry(concordanceEntry: ArrayBuffer[Int]) : String = {
    var i = 1
    var outputString = "{" + concordanceEntry(0).toString()+ ":"
    for(i <- 1 until concordanceEntry.length){
      outputString = outputString + concordanceEntry(i).toString() + ", "
    }
    outputString = outputString.trim()
    outputString.substring(0, outputString.length-1) + "}"
  }
}
