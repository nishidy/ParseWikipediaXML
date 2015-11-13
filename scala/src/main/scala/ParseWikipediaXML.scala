import scala.io.Source
import scala.xml.XML
import scala.language.postfixOps
import akka.actor.{Actor,ActorSystem,Props}
import akka.routing.{DefaultResizer, RoundRobinRouter}
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.Await
import scala.concurrent.duration._
import java.util.concurrent._

object ParseWikipediaXML{

    import java.io.{ FileOutputStream=>FileStream, OutputStreamWriter=>StreamWriter }

    case class Args(
        ifwiki: String = "",
        ifdict: String = "",
        ofcont: String = "",
        oftitle: String = "",
        minl: Int = 2,
        maxl: Int = 64,
        minc: Int = 1,
        workers: Int = 1
    )

    case class Text(text:String, mapDict:Map[String,String], minl:Int, maxl:Int, minc:Int, writer:StreamWriter)

    val stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your".split(",")

    class bowActor extends Actor {

        def receive = {

            case Text(text,mapDict,minl,maxl,minc,writer) =>

                // allwords is list
                // exclude stopwords and words that include symbols
                val allwords =
                    text.split( Array(' ',',','.') )
                    .filter( x => (!stopwords.contains(x)) && isWord(x) )
                    .map( x => mapDict.get(x) match { case None =>  x; case Some(v) => v } )

                // onewords is list in which redundant words are excluded
                val onewords = allwords.toSet.toList

                if( allwords.size > maxl || allwords.size < minl ) ()
                else {
                    val zipped =
                        for( word <- onewords )
                            yield ( word, allwords.filter( _==word ).size.toString )

                    if( zipped.size>0 )
                        synchronized {
                            writer.write( zipped.filter( _._2.toInt>=minc )
                            .map( x => List(x._1,x._2) )
                            .flatten.mkString(""," ","\n") )
                        }
                    else ()
                }

            case "Finished" => sender ! "Finished"

            case _ =>

        }

        def isWord(x:String): Boolean = {
            val reg = """^([a-z][a-z0-9'-]+[a-z0-9])$""".r
            x match{
                case reg(_) => true
                case _ => false
            }
        }

    }

    def main(args: Array[String]) = {

        val parser = new scopt.OptionParser[Args]("ParseWikipediaXML"){

            opt[String]('i',"input-file") required() action { (x,c) =>
                c.copy(ifwiki = x)
            } text ("Input File(Wikipedia)")

            opt[String]('d',"dict-file") required() action { (x,c) =>
                c.copy(ifdict = x)
            } text ("Input File(Dictionary)")

            opt[String]('s',"output-cont-file") required() action { (x,c) =>
                c.copy(ofcont = x)
            } text ("Output File(Contents)")

            opt[String]('t',"output-title-file") action { (x,c) =>
                c.copy(oftitle = x)
            } text ("Output File(Title)")

            opt[Int]('m',"min-word-length") action { (x,c) =>
                c.copy(minl = x.toInt)
            } text ("Minimum number of words that a page should have")

            opt[Int]('x',"max-word-length") action { (x,c) =>
                c.copy(maxl = x.toInt)
            } text ("Maximum number of words that a page should have")

            opt[Int]('c',"min-word-count") action { (x,c) =>
                c.copy(minc = x.toInt)
            } text ("Minimum number that a word should have")

            opt[Int]('w',"workers") action { (x,c) =>
                c.copy(workers = x.toInt)
            } text ("Number of threads at most")

            help("help") text("print this usage text.")

        }


        parser.parse(args, Args()) match {
            case Some(c) => {

                val system = ActorSystem("system")
                val resizer = DefaultResizer(lowerBound=1, upperBound=c.workers)
                val actor = system.actorOf(Props[bowActor].withRouter(RoundRobinRouter(resizer = Some(resizer))),"router")

                val dictionary = readDict(c.ifdict)

                val fs = new FileStream( c.ofcont, true )
                val writer = new StreamWriter( fs, "UTF-8" )

                val page = new StringBuilder
                val s = Source.fromFile(c.ifwiki)
                try{
                    for( line <- s.getLines ) {

                        if( line.contains("<page>") || page.size > 0 ){
                            page.append(line)
                        }

                        if( line.contains("</page>") ){

                            for{ node <- XML.loadString( page.toString ) \ "revision" \ "text"
                                 text = node text }{
                                actor ! new Text(text,dictionary,c.minl,c.maxl,c.minc,writer)
                            }
                            page.clear
                        }

                    }
                    s.close
                } finally {
                    println("Finished to read from the input file.")
                    try {
                        implicit val timeout = Timeout(10,MINUTES)
                        val future = Await.result( actor ? "Finished", timeout.duration )
                    } catch {
                        case e :TimeoutException => println("Too long to wait. Timeout happened.")
                    }

                    println("Finished to write to the output file.")
                    system.shutdown()
                    writer.close
                }
            }

            case None => {
                println("Failed to parse args.")
                sys.exit(1)
            }
        }

        def readDict(ifdict:String): Map[String,String]= {
            var mapDict = Map.empty[String,String]
            val s = Source.fromFile(ifdict)
            for( line <- s.getLines ){
                val lineSp = line.split("[ \t]+")
                if( ! lineSp.exists(_==";;;") ){
                    mapDict += (lineSp(0)->lineSp(1))
                }
            }
            mapDict
        }

    }
}
