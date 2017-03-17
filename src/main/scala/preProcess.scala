package prime

import java.io.{BufferedInputStream, FileInputStream}

import opennlp.tools.cmdline.parser.ParserTool
import opennlp.tools.namefind.{NameFinderME, TokenNameFinderModel}
import opennlp.tools.parser.{ParserFactory, ParserModel}
import opennlp.tools.postag.{POSModel, POSTaggerME}

import scala.io.Source

/**
  * Created by 527204 on 2/24/2017.
  */


object preProcess {

  val modelLocation = "src/main/resources/en-ner-date.bin";

    //val parserModelLocation = "src/main/resources/en-parser-chunking.bin"
   // val searchFilePath = "src/main/resources/searchs.txt";

    // POS Tags

    /*getSearchStringsFromFile(searchFilePath).foreach(searchTerm=>{
      val tags = getTags(modelLocation,searchTerm);
      val wordsWithTags = tokenize(searchTerm).zip(tags);
      wordsWithTags.foreach(x=>println(x));
      //wordsWithTags.filter(x=>x._2.startsWith("NN")).foreach(x=>{println(x._1+"-->"+searchTerm)});
    })*/

    // Parser


  def getTags(modelLocation:String,searchString:String):Array[String] = {

    val posModel = new POSModel(new BufferedInputStream(new FileInputStream(modelLocation)));
    val posTagger = new POSTaggerME(posModel);
    val tokenizedString = tokenize(searchString);
    val tags = posTagger.tag(tokenizedString);
    tags
  }

  def tokenize(searchString:String):Array[String] = {
    searchString.split(" ");
  }

  def getSearchStringsFromFile(filePath:String):Array[String] = {
    Source.fromFile(filePath).getLines().toArray;
  }

  def parseSentence(modelLocation:String,searchString:String):String  = {
    val parserModel = new TokenNameFinderModel(new BufferedInputStream(new FileInputStream(modelLocation)));
    val parser = new NameFinderME(parserModel);
    val parsedValue = parser.find(tokenize(searchString))
    parsedValue.mkString(" ");
  }

}