package graphalignment

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

import java.io.PrintWriter

/**
  * Created by rebeccas on 4/3/17.
  */
object GetExampleSentence {

  def loadQuestionAnswerIndex (filename: String): Map[String, Seq[String]] = {
    val index = new mutable.HashMap[String, Seq[String]]()
    val source = scala.io.Source.fromFile(filename)
    val lines = source.getLines().toArray
    for {
      line <- lines
      fields = line.split("\t")
      question = fields(0).trim
      answers = fields.slice(1, fields.length).toSeq.map(_.trim)
    } index.put(question, answers)
    source.close()
    index.toMap
  }


  def processQuestionsFile(filename: String, questionAnswerIndex: Map[String, Seq[String]], outputFilename: String) = {
    val pw = new PrintWriter(outputFilename)
    val source = scala.io.Source.fromFile(filename)
    val lines = source.getLines().toArray
    for (line <- lines) {
      getSentencesFromQuestion(line, questionAnswerIndex, pw)
    }
    source.close()
    pw.close()
  }

  def getSentencesFromQuestion(line: String, questionAnswerIndex: Map[String, Seq[String]], pw: PrintWriter): Unit = {
    // Extract the KB tuples from the string, keep the rest
    val fields = line.trim.split("\t")
    val questionIndex = fields(0)
    val questionText = fields(1).trim
    val label = fields(4).toInt

    val answerSentences = questionAnswerIndex.get(questionText)
    if (answerSentences.nonEmpty) {
      val correctAnswerSentence = answerSentences.get(label)

      val background = fields(3)
      val backgroundTuples = background.split("\\$\\$\\$")

      // From each tuple, get the background sentences (as a set)
      val backgroundSentences = backgroundTuples
          .map(tupleString => contextSentenceFromTupleString(tupleString))
          .toSet
      println(s"There were ${backgroundSentences.size} background sentences from this line.")

      // Filter to only sentences which contain text from correct answer
      val correctAnswerWords = getAnswerText(questionText, label).split(" ").toSet
      if (correctAnswerWords.isEmpty) {
        println ("WARNING: no correct answer words for question: " + questionText)
      }
      val filtered = backgroundSentences.filter(sent => sent.split(" ").toSet.intersect(correctAnswerWords).nonEmpty)
      println(s"There were ${filtered.size} background sentences from this line after filtering by correct answer choice.")

      pw.println(Seq(questionIndex, questionText, correctAnswerSentence, filtered.mkString("###")).mkString("\t"))
    }
  }

  def contextSentenceFromTupleString(tupleString: String): String = {
    tupleString.split("context:")(1).split("<>")(0).trim.toLowerCase
  }

  def getAnswerText(question: String, answerIndex: Int): String = {
    val split = question.split("\\([ABCD]\\)")
    split(answerIndex + 1).trim
  }



  def main(args: Array[String]): Unit = {

    val answerSentencesFile = "/Users/rebeccas/data/brittleness/questionSentences.txt"
    val questionAnswerIndex = loadQuestionAnswerIndex(answerSentencesFile)
    println("The question-answers index has " + questionAnswerIndex.size + " questions in it.")

    val fold = "train"
    val inputFilename = s"/Users/rebeccas/data/brittleness/omni4_${fold}_brittleness.tuples.q.labeled"
    processQuestionsFile(inputFilename, questionAnswerIndex, inputFilename + ".examples")
  }

}
