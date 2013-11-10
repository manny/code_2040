import io.Source
import java.util.StringTokenizer  
import scala.collection.mutable.ArrayBuffer

/*Concordance
 *Takes in a string of one or more sentences from stdin
 *Prints a concordance of sorted words with number of occurences
 *and 0 based index of sentence occurences
 * */
object WordCount{

  def main(args: Array[String]){
    println("Enter sentences for concordance output: ")
    val text = readLine
    parseText(text)
  }
  
  /* parseText takes in the users input and performs tokenization to build parallel
   * ArrayBuffers, one with the tokenized word, the other to keep track of each words 
   * count and sentence occurence
   **/
  def parseText(text: String){

    var sentenceCount = 0
    var incrementCount = false

    var wordList = ArrayBuffer[String]()
    var concordanceList = ArrayBuffer[ArrayBuffer[Int]]()

    val st = new StringTokenizer(text, " ,;:(){}[]\"")
    var word = ""

    while (st.hasMoreTokens()){
      word = st.nextToken()
      word = word.toLowerCase()

      if (singlePunc(word)){
        incrementCount = true
      }else if (wordCheck(word)){

        if(word.endsWith(".") || word.endsWith("?") || word.endsWith("!")){
          if(endOfSentence(word)){
            word = word.substring(0, word.length-1)
            incrementCount = true
          }
        }
        if(wordList.contains(word)){
          concordanceList(wordList.indexOf(word))(0)+=1
          concordanceList(wordList.indexOf(word))+=sentenceCount
        
        }else{
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
    sortWords(wordList, concordanceList)
  }
    

  /*Helper functions that help clean up parseText
   */

  /*endOfSentence determine whether a word with a period at the 
   * end is neccesarily a word a the end of a sentence, mostly for
   * dealing with the pesky case of the word "e.g.
   */
  def endOfSentence(word: String) : Boolean = {
    word.indexOf('.') == word.lastIndexOf('.')
  }
  
  /*wordCheck verifies that the token is a word before
   * adding to the concordance, basically by determining
   * if the word startes with a letter
   */
  def wordCheck(word: String) : Boolean = {
    word.charAt(0).isLetter
  }
  
  /* singlePunc handles the rare occurrence when a token consist
   * of a lone rouge end punctuation*/
  def singlePunc(word: String) : Boolean = {
    (word.equals(".") || word.equals("?") || word.equals("!"))
  }

  /*sortWords takes the parrallel ArrayBuffers built by parseText,
   * then combines and sorts them to pass along to the printing function
   */
  def sortWords(wordList: ArrayBuffer[String], concordanceList: ArrayBuffer[ArrayBuffer[Int]]){
    for(i <- 0 until wordList.length){
      wordList(i) = wordList(i) + " " + formatEntry(concordanceList(i))
    }
    val sortedList = wordList.toArray
    scala.util.Sorting.quickSort(sortedList)
    outputConcordance(sortedList)

  }
  /*formatEntry takes in an ArrayBuffer of a words numerical data
   * and iterates thru it to produce a correctly formated string
   * that represents a concordance entry
   */
  def formatEntry(concordanceEntry: ArrayBuffer[Int]) : String = {
    var i = 1
    var outputString = "{" + concordanceEntry(0).toString()+ ":"
    for(i <- 1 until concordanceEntry.length){
      outputString = outputString + concordanceEntry(i).toString() + ","
    }
    outputString.substring(0, outputString.length-1) + "}"
  }


  /*outputConcordence takes in the sorted array built by sortWords
   * and iterates thru all entries for printing
   */
  def outputConcordance(finalConcordance: Array[String]){ 
    println("")
    var i = 0
    for(i <- 0 until finalConcordance.length){
      println(finalConcordance(i))
      println("")
    }
  }

}
