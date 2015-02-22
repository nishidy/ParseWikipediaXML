import scala.io.Source
import scala.xml.XML
import scala.language.postfixOps

object test{

	case class Args(
		ifwiki: String = "",
		ofcont: String = "",
		oftitle: String = ""
	)


	def main(args: Array[String]) = {

		val parser = new scopt.OptionParser[Args]("ParseWikipediaXML"){
			opt[String]('i',"input-file") action { (x,c) =>
				c.copy(ifwiki = x)
			} text ("Input File(Wikipedia)")

			opt[String]('s',"output-cont-file") action { (x,c) =>
				c.copy(ofcont = x)
			} text ("Output File(Contents)")

			opt[String]('t',"output-title-file") action { (x,c) =>
				c.copy(oftitle = x)
			} text ("Output File(Title)")

			help("help") text("print this usage text.")
		}

		parser.parse(args, Args()) match {
			case Some(c) => {
				val s = Source.fromFile(c.ifwiki)
				val page = new StringBuilder
				try{
					for( line <- s.getLines ) {

						if( line.contains("<page>") || page.size > 0 ){
							page.append(line)
						}

						if( line.contains("</page>") ){

							for{ node <- XML.loadString( page.toString ) \ "revision" \ "text"
								 text = node text }{

								// allwords is list
								// exclude stopwords and words that include symbols
								val allwords = text.split( Array(' ',',','.') ).filter( x=>x.size>1 && (!stopword(x)) && allAlnum(x) )

								// onewords is list in which redundant words are excluded
								val onewords = allwords.toSet.toList

								val zipped = for( word <- onewords ) yield List( word, allwords.filter( _ == word ).size.toString )

								if(zipped.size>0) println(zipped.flatten.mkString(" "))
								else ()
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

		def stopword(x:String): Boolean = {
			val stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your".split(',')
			stopwords.contains(x)
		}

		def allAlnum(x:String): Boolean = {
			val reg = """^([a-z0-9-]+)$""".r
			x match{
				case reg(_) => true
				case _ => false
			}
		}

	}
}
