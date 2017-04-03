package antonyms

import org.clulab.processors.{Document, Sentence}
import org.clulab.processors.fastnlp.FastNLPProcessor
import org.clulab.struct.{GraphMap, Tree}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by rebeccas on 3/22/17.
  */
object CreateAntonymSet extends App {

  val processor = new FastNLPProcessor()

  def parseQuestionsFile(filename: String): Seq[Document] = {
    val source = scala.io.Source.fromFile(filename)
    val lines = source.getLines().toArray
    val documents = for {
      line <- lines
      questionText = line.split("\t")(0).split("\\(A")(0).trim
      doc = processor.mkDocument(questionText)
    } yield mkPartialAnnotation(doc)

    // Housekeeping
    source.close()

    documents
  }

  def parseAntonymFile(filename: String): Map[String, mutable.Set[String]] = {
    val antonyms = new mutable.HashMap[String, mutable.Set[String]]()

    val source = scala.io.Source.fromFile(filename)
    val lines = source.getLines().toArray
    val antonymPairs = new ArrayBuffer[(String, String)]
    for {
      line <- lines
      fields = line.trim.split(",").map(_.trim)
      lineAntonyms = handleCases(fields(1), fields(2))
    } antonymPairs.appendAll(lineAntonyms)

    // Add antonym pairs to Map
    for (pair <- antonymPairs) {
      // If first word not already in map, add a set to hold its pair members
      if (!antonyms.contains(pair._1)) {
        antonyms.put(pair._1, mutable.Set[String]())
      }
      // Add the pair member
      antonyms(pair._1).add(pair._2)
    }

    // Housekeeping
    source.close()

    antonyms.toMap
  }

  def handleCases(text1: String, text2:String): Seq[(String, String)] = {
    val antonymPairs = new ArrayBuffer[(String, String)]
    val words1 = text1.split(" ")
    val words2 = text2.split(" ")
    if (words1.length == 1 || words2.length == 1) {
      antonymPairs.append((text1, text2))
      antonymPairs.append((text2, text1))
    } else if (words1.length < 3 && words2.length < 3) {
      // Check to see if first word the same, if so add rest as antonyms
      if (words1.head == words2.head) {
        val phrase1 = words1.slice(1, words1.length).mkString(" ")
        val phrase2 = words2.slice(1, words2.length).mkString(" ")
        antonymPairs.append((phrase1, phrase2))
        antonymPairs.append((phrase2, phrase1))
      }
      // Check to see if the last word the same, if so, add the first words as antonyms
      else if (words1.tail == words2.tail) {
        val phrase1 = words1.slice(0, words1.length - 1).mkString(" ")
        val phrase2 = words2.slice(0, words2.length - 1).mkString(" ")
        antonymPairs.append((phrase1, phrase2))
        antonymPairs.append((phrase2, phrase1))
      }
    } else {
        println(s"More than three words found: ($text1, $text2)")
    }
    antonymPairs
  }

  def mkPartialAnnotation(doc: Document): Document = {
    processor.tagPartsOfSpeech(doc)
    processor.lemmatize(doc)
    processor.parse(doc)
    doc.clear()
    doc
  }

  def checkAdjectiveNounsPattern(questions: Seq[Document]): Set[String] = {
    var numWIthAdjNoun: Double = 0.0
    val adjectiveSet = scala.collection.mutable.HashSet[String]()
    for (questionDoc <- questions) {
      var hasPattern = false
      for (sentence <- questionDoc.sentences) {
        val tags = sentence.tags.get
        val numTags = tags.length
        for (i <- 0 until numTags - 1) {
          if (tags(i).startsWith("JJ") && tags(i + 1).startsWith("NN")) {
            //println ("JJNN found: " + sentence.getSentenceText())
            hasPattern = true
            adjectiveSet.add(sentence.words(i).toLowerCase())
          }
        }
      }
      if (hasPattern) numWIthAdjNoun += 1.0
    }

    val proportion = 100 * numWIthAdjNoun / questions.length.toDouble
    println (s"Of ${questions.length} questions, $numWIthAdjNoun had the JJ-NN pattern ($proportion%)")

    println ("ADJ word set:" + adjectiveSet)

    adjectiveSet.toSet
  }

  def makeQuestionsOpposite(questions: Seq[Document], antonymMap: Map[String, mutable.Set[String]]): Unit = {
    val negativesOfQuestions = Array.fill[Seq[Document]](questions.length)(Seq[Document]())
    var numReplacements: Double = 0.0
    var numQuestionsWithReplacements: Double = 0.0
    var multipleOptions: Int = 0
    for (i <- questions.indices) {
      val negativesOfCurrentQuestion = new ArrayBuffer[Document]
      val questionDoc = questions(i)
      val originalText = questionDoc.sentences.map(_.getSentenceText).mkString(" ")
      var replacementMade = false
      for (j <- questionDoc.sentences.indices) {
        // Adjective Replacements
//        val newDocument1 = mkPartialAnnotation(processor.mkDocument(originalText))
//        val (newDocumentAdjective, adjectiveReplacementMade, adjectiveMultipleOptions) =
//          adjectiveNounCase(newDocument1, j, antonymMap)
//        if (adjectiveReplacementMade) {
//          replacementMade = true
//          negativesOfCurrentQuestion.append(newDocumentAdjective)
//          multipleOptions += adjectiveMultipleOptions
//          println("\tadjective replacement made")

                // Verb replacements
                val newDocument2 = mkPartialAnnotation(processor.mkDocument(originalText))
                val (newDocumentVerb, verbReplacementMade, verbMultipleOptions) = verbCase(newDocument2, j, antonymMap)
                if (verbReplacementMade) {
                  replacementMade = true
                  negativesOfCurrentQuestion.append(newDocumentVerb)
                  multipleOptions += verbMultipleOptions
                  println ("\tverb replacement made")
                }
        //        // Noun replacements
        //        val newDocument3 = mkPartialAnnotation(processor.mkDocument(originalText))
        //        val (newDocumentNoun, nounReplacementMade, nounMultipleOptions) = nounCase(newDocument3, j, antonymMap)
        //        if (nounReplacementMade) {
        //          replacementMade = true
        //          negativesOfCurrentQuestion.append(newDocumentNoun)
        //          multipleOptions += nounMultipleOptions
        //          println ("\tnoun replacement made")
        //        }
//              }
      }
      if (replacementMade) {
        println ("Before replacement: " + originalText)
        for (negativeVersion <- negativesOfCurrentQuestion) {
          val newText = negativeVersion.sentences.map(_.getSentenceText).mkString(" ")
          println ("  After replacement: " + newText)
        }
        println("")
        numQuestionsWithReplacements += 1.0
        negativesOfQuestions(i) = negativesOfCurrentQuestion
      }
    }

    val proportion = 100 * numQuestionsWithReplacements / questions.length.toDouble
    println (s"Of ${questions.length} questions, $numQuestionsWithReplacements had an antonym replacement ($proportion%)")
    println (s"There were $numReplacements replacements made.")
    println (s"There were $multipleOptions times where multiple options for replacement existed!! (HANDLE THESE!)")
  }

  def adjectiveNounCase(
    document: Document, i: Int,
    antonymMap: Map[String, mutable.Set[String]]
  ): (Document, Boolean, Int) = {

    val sentence = document.sentences(i)
    var hasPattern = false
    var replacementMade = false
    var multipleOptions = 0
    val tags = sentence.tags.get
    val numTags = tags.length
    for (i <- 0 until numTags - 1) {
      if (tags(i).startsWith("JJ") && tags(i + 1).startsWith("NN")) {
        //println ("JJNN found: " + sentence.getSentenceText())
        hasPattern = true
        if (antonymMap.contains(sentence.words(i))) {
          val antonymOptions = antonymMap(sentence.words(i))
          if (antonymOptions.size > 1) {
            multipleOptions += 1
          }
          // TODO: this isn't really valid....
          sentence.words(i) = antonymOptions.head
          replacementMade = true
        }
      }
    }
    (document, replacementMade, multipleOptions)
  }

  def verbCase(
    document: Document, i: Int,
    antonymMap: Map[String, mutable.Set[String]]
  ): (Document, Boolean, Int) = {

    val sentence = document.sentences(i)
    var hasPattern = false
    var replacementMade = false
    var multipleOptions = 0
    val tags = sentence.tags.get
    val numTags = tags.length
    for (i <- 0 until numTags - 1) {
      if (tags(i).startsWith("VB")) {
        //println ("JJNN found: " + sentence.getSentenceText())
        hasPattern = true
        if (antonymMap.contains(sentence.words(i))) {
          val antonymOptions = antonymMap(sentence.words(i))
          if (antonymOptions.size > 1) {
            multipleOptions += 1
          }
          // TODO: this isn't really valid....
          sentence.words(i) = antonymOptions.head
          println (s"\tverb sub (${sentence.words(i)})")
          replacementMade = true
        }
      }
    }
    (document, replacementMade, multipleOptions)
  }

  def nounCase(
    document: Document, i: Int,
    antonymMap: Map[String, mutable.Set[String]]
  ): (Document, Boolean, Int) = {

    val sentence = document.sentences(i)
    var hasPattern = false
    var replacementMade = false
    var multipleOptions = 0
    val tags = sentence.tags.get
    val numTags = tags.length
    for (i <- 0 until numTags - 1) {
      if (tags(i).startsWith("NN")) {
        //println ("JJNN found: " + sentence.getSentenceText())
        hasPattern = true
        if (antonymMap.contains(sentence.words(i))) {
          val antonymOptions = antonymMap(sentence.words(i))
          if (antonymOptions.size > 1) {
            multipleOptions += 1
          }
          // TODO: this isn't really valid....
          sentence.words(i) = antonymOptions.head
          replacementMade = true
        }
      }
    }
    (document, replacementMade, multipleOptions)
  }

  def copySentence(sentence: Sentence): Sentence = {
    val copyOfOriginalSentence = Sentence(
      sentence.words,
      sentence.startOffsets,
      sentence.endOffsets: Array[Int],
      sentence.tags,
      sentence.lemmas,
      sentence.entities,
      sentence.norms,
      sentence.chunks,
      sentence.syntacticTree,
      sentence.dependenciesByType)
    copyOfOriginalSentence
  }

  val questionFile = "/Users/rebeccas/data/omnibus4/head100.queryTuples_Omnibus_newDelim.txt"
  val antonymFile = "/Users/rebeccas/data/antonyms/wordnet-antonyms.filtered.verbs.csv" //wordnet-antonyms.tsv.csv"

  // Parse the questions into Documents
  val questionDocuments = parseQuestionsFile(questionFile)

  // Check how many have the JJ-NN pattern at all! (# and propportion)
  val adjWords = checkAdjectiveNounsPattern(questionDocuments)

  // Parse the adjective file
  val antonymMap = parseAntonymFile(antonymFile)

  // Check for overlap with the adjective sets...
  // Words:
  val numAdjectivesWithAntonyms = adjWords.count(adjective => antonymMap.contains(adjective))
  println (s"Of ${adjWords.size} adjectives, $numAdjectivesWithAntonyms have an antonym!")

  // Count how many question can be made opposite with antonyms, and qualitatively evaluate:
  makeQuestionsOpposite(questionDocuments, antonymMap)

  // TODO: grab subgraph words for repeating -- check about integrating these into the query (Tushar's code)

}
