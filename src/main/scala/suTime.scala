/**
  * Created by prime on 29/3/17.
  */
package prime

import java.text.SimpleDateFormat
import java.util.Properties

import edu.stanford.nlp.ling.CoreAnnotations
import edu.stanford.nlp.pipeline._
import edu.stanford.nlp.time._

import scala.collection.JavaConversions._
import java.util.Date

object suTime {

    var isInitialized = false

    val pipeline :StanfordCoreNLP = {  // Initializes pipeline and stanford.
        val props = new Properties()
        props.put("annotators", "tokenize, ssplit, pos, lemma")
        // Annotators pre-required for SuTime
        val pipe = new StanfordCoreNLP(props)
        val timeAnnotator = new TimeAnnotator("sutime", props)
        pipe.addAnnotator(timeAnnotator)
        isInitialized = true
        pipe
    }

    def extractDates(searchQuery:String){
      val doc = new Annotation(searchQuery)
      doc.set(classOf[CoreAnnotations.DocDateAnnotation], new SimpleDateFormat("yyyy-MM-dd").format(new java.util.Date()))
      pipeline.annotate(doc)
      val timexAnnotations = doc.get(classOf[TimeAnnotations.TimexAnnotations])
      for (timexAnn <- timexAnnotations) {
        val annotated = timexAnn.get(classOf[TimeExpression.Annotation])
          val cc = DateString(annotated.getText.toLowerCase)
          cc.dateValue = dateOps.getDateRange(annotated.getTemporal.toString)
          LexiconOps.injectLexicon(cc)
      }

    }
}
