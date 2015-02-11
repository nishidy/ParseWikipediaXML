import scala.io.Source

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
				try{
					for( line <- s.getLines ) {

						val allwords = line.split(Array(' ',',','.')).
									   filter(x=>x.size>1&&(!stopword(x))&&allAlnum(x))
									   //.map(_.filter(x=>allAlnum(_.toString)))
						val onewords = allwords.toSet.toList

						val listWords = for( word <- onewords ) yield ( word, allwords.filter( _ == word ).size )
						val zipped = for( z <- listWords.toMap.keys zip listWords.toMap.values.map(_.toString) ) yield List(z._1,z._2)

						if(zipped.size>0)
							// zipped is Set(), so it needs toList
							println(zipped.toList.flatten.mkString(" "))
						else
							()
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
