import scala.io.Source
import scala.xml.XML
import scala.language.postfixOps
import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

object test{

	case class Args(
		ifwiki: String = "",
		ifdict: String = "",
		ofcont: String = "",
		oftitle: String = "",
		minl: Int = 2,
		maxl: Int = 64,
		minc: Int = 1
	)

	case class Text(text:String, mapDict:Map[String,String], minl:Int, maxl:Int, minc:Int)

	class bowActor extends Actor {
		def receive = {
			case Text(text,mapDict,minl,maxl,minc) =>

				// allwords is list
				// exclude stopwords and words that include symbols
				val allwords = text.split( Array(' ',',','.') ).filter( x => x.size>=minl && x.size<=maxl && (!stopword(x)) && allAlnum(x) ).map( x => if( mapDict.get(x)==None ){ x }else{ mapDict.get(x).get } )

				// onewords is list in which redundant words are excluded
				val onewords = allwords.toSet.toList

				val zipped = for( word <- onewords ) yield ( word, allwords.filter( _==word ).size.toString )

				if( zipped.size>0 ) println(zipped.filter( _._2.toInt>=minc ).map( x => List(x._1,x._2) ).flatten.mkString(" "))
				else ()

			case _ =>

		}

		def stopword(x:String): Boolean = {
			val stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your".split(',')
			stopwords.contains(x)
		}

		def allAlnum(x:String): Boolean = {
			val reg = """^([a-z0-9-]+)$""".r
			x match{
				case reg("""^[-]+$""") => false
				case reg(_) => true
				case _ => false
			}
		}

	}

	def main(args: Array[String]) = {

		val parser = new scopt.OptionParser[Args]("ParseWikipediaXML"){

			opt[String]('i',"input-file") action { (x,c) =>
				c.copy(ifwiki = x)
			} text ("Input File(Wikipedia)")

			opt[String]('d',"dict-file") action { (x,c) =>
				c.copy(ifdict = x)
			} text ("Input File(Dictionary)")

			opt[String]('s',"output-cont-file") action { (x,c) =>
				c.copy(ofcont = x)
			} text ("Output File(Contents)")

			opt[String]('t',"output-title-file") action { (x,c) =>
				c.copy(oftitle = x)
			} text ("Output File(Title)")

			opt[Int]('m',"min-word-length") action { (x,c) =>
				c.copy(minl = x.toInt)
			} text ("Minimum word length")

			opt[Int]('x',"max-word-length") action { (x,c) =>
				c.copy(maxl = x.toInt)
			} text ("Maximum word length")

			opt[Int]('c',"min-word-count") action { (x,c) =>
				c.copy(minc = x.toInt)
			} text ("Minimum word count")

			help("help") text("print this usage text.")
		}

		val system = ActorSystem("system")
		val actor = system.actorOf(Props[bowActor],"bow")

		parser.parse(args, Args()) match {
			case Some(c) => {

				val dictionary = readDict(c.ifdict)

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
								actor ! new Text(text,dictionary,c.minl,c.maxl,c.minc)
							}
							page.clear
						}

					}
				} finally {
					s.close
				}
			}

			case None => {
				println("Failed to parse.")
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
