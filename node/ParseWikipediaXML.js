var co = require('co')
var fs = require('fs')
var async = require('async')
var minimist = require('minimist')
var merge = require('merge')
var kuromoji = require('kuromoji')

var BUFSIZE = 65535*32

var argv = minimist(process.argv.slice(2), {
	string: [
		'inWikiFile',
		'inDictFile',
		'outBofwFile',
		'outTitleFile',
		'minWordsInDoc',
		'maxWordsInDoc',
		'minWordForCount',
	],
	boolean: [
		'isJapanese',
	],
	alias: {
		i: 'inWikiFile',
		d: 'inDictFile',
		s: 'outBofwFile',
		t: 'outTitleFile',
		m: 'minWordsInDoc',
		x: 'maxWordsInDoc',
		c: 'minWordForCount',
		j: 'isJapanese',
	},
	default: {
		inDictFile: "",
		minWordsInDoc: 2,
		maxWordsInDoc: 65535,
		minWordForCount: 1,
		isJapanese: false,
	},
})

var stopwordsEn = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"

var stopwordsJp = "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ"

var Parser = function(argv){

	this.isJapanese = argv.j
	this.inputFile = argv.i
	this.outputFile = argv.s

	var inputStats = fs.statSync(argv.i)
	this.inputFileSize = inputStats["size"]

	if(this.isJapanese){
		this.parse = parseJp
		this.stopwords = stopwordsJp.split(",")
		this.tokenizer
	}else{
		this.parse = parseEn
		this.stopwords = stopwordsEn.split(",")
		this.dictFile = argv.d

		var dictStats = fs.statSync(this.dictFile)
		this.dictFileSize = dictStats["size"]
	}

}

parser = new Parser(argv)

function genTokenizer(){
	var DIC_PATH = "node_modules/kuromoji/dist/dict/"
	return new Promise(function(resolve,reject){
		kuromoji.builder({ dicPath: DIC_PATH }).build(function (err,tokenizer){
			if(err){ reject(err); return }
			resolve(tokenizer)
		})
	})
}

function openAsync(file){
	return new Promise(function(resolve,reject){
		fs.open(file,'r',function(err,hInputFile){
			if(err){ reject(err); return }
			resolve(hInputFile)
		})
	})
}

function readAsync(hInputFile,length){
	return new Promise(function(resolve,reject){
		var buf = new Buffer(BUFSIZE)
		fs.read(hInputFile,buf,0,BUFSIZE,null,function(err,bs,b){
			if(err){ reject(err); return }
			text = b.slice(0,length).toString('utf8')
			resolve(text)
		})
	})
}

function readDictionary(text){
	return new Promise(function(resolve){
		var mapDict = {}
		text.split(/\n/).forEach(function(line){
			if(line.indexOf(";;;")>-1){ return }
			var col = line.split(/[\t ]/)
			var from  = col[0]
			var trans = col[3]
			mapDict[from]=trans
		})
		resolve(mapDict)
	})
}


function parseJp(page){

	var mapWordFreq = {}
	var numWordsInDoc = 0
	var text= page.match(/<text[^<>]*>([\s\S]*?)<\/text>/)[1]

	var stopwords = this.stopwords
	var outputFile = this.outputFile

	tokens = this.tokenizer.tokenize(text)
	for(var idx in tokens){

		var token = tokens[idx]
		var word = token['basic_form']

		if(word=="*"){
			continue
		}else{
			if ( ( token['pos'] == "名詞" && token['pos_detail_1'] == "サ変接続") ||
				( token['pos'] in ["動詞","形容詞","副詞"] ) ){
				word = token['surface_form']
			}else{
				continue
			}
		}

		if(stopwords.indexOf(word)>-1){ return }

		if(word in mapWordFreq){
			mapWordFreq[word] += 1
		}else{
			mapWordFreq[word] = 1
		}
		numWordsInDoc++;

	}

	var tupleWordFreq = []
	for(var key in mapWordFreq){ tupleWordFreq.push([key,mapWordFreq[key]]) }
	tupleWordFreq.sort(function(a,b){ return a[1] < b[1] ? 1 : a[1] > b[1] ? -1 : 0 } )

	var strBofw = ""
	for(var idx in tupleWordFreq){
		strBofw += tupleWordFreq[idx][0] + " " + tupleWordFreq[idx][1]
		if(idx < tupleWordFreq.length-1){ strBofw += " " }
	}

	if(numWordsInDoc>argv.m && strBofw.length>0){
		fs.appendFileSync(outputFile,strBofw.slice(0,strBofw.length-1)+"\n")
	}

}

function parseEn(page){

	var mapWordFreq = {}
	var numWordsInDoc = 0
	var text= page.match(/<text[^<>]*>([\s\S]*?)<\/text>/)[1]

	// To deliver these propaties to forEach function
	var stopwords = this.stopwords
	var mapDict = this.mapDict
	var outputFile = this.outputFile

	text.replace(/\n/g," ").split(" ").forEach(function(word){
		word = word.toLowerCase()

		if(word.match(/^[a-z][\-'0-9a-z]*[0-9a-z]$/)==null){ return }
		if(stopwords.indexOf(word)>-1){ return }
		if(word in mapDict){ word = mapDict[word] }

		if(word in mapWordFreq){
			mapWordFreq[word] += 1
		}else{
			mapWordFreq[word] = 1
		}
		numWordsInDoc++;
	})

	var tupleWordFreq = []
	for(var key in mapWordFreq){ tupleWordFreq.push([key,mapWordFreq[key]]) }
	tupleWordFreq.sort(function(a,b){ return a[1] < b[1] ? 1 : a[1] > b[1] ? -1 : 0 } )

	var strBofw = ""
	for(var idx in tupleWordFreq){
		strBofw += tupleWordFreq[idx][0] + " " + tupleWordFreq[idx][1]
		if(idx < tupleWordFreq.length-1){ strBofw += " " }
	}

	if(numWordsInDoc>argv.m && strBofw.length>0){
		fs.appendFileSync(outputFile,strBofw.slice(0,strBofw.length-1)+"\n")
	}
}

co(function *(){

	var offset = 0
	var text = ""

	if(parser.isJapanese){
		parser.tokenizer = yield genTokenizer();
	}else{
		// Read dictionary
		var hDictFile = yield openAsync(parser.dictFile)
		var mapDict = {}
		console.log("Reading dictionary begins.")
		while(offset<parser.dictFileSize){
			console.log(~~(offset*100/parser.dictFileSize)+"%")

			var length = (parser.dictFileSize-offset)>BUFSIZE ? BUFSIZE : parser.dictFileSize-offset
			text += yield readAsync(hDictFile,length)
			merge(mapDict, yield readDictionary(text.slice(0,text.indexOf(/\n/))))
			text = text.slice(text.lastIndexOf(/\n/)+1,text.length)
			offset += BUFSIZE
		}
		parser.mapDict = mapDict
		console.log("Reading dictionary finished.")
	}

	// Parse WikipediaXML
	offset = 0
	var hInputFile = yield openAsync(parser.inputFile)
	console.log("Reading WikipediaXML begins.")
	while(offset<parser.inputFileSize){
		console.log(~~(offset*100/parser.inputFileSize)+"%")

		var length = (parser.inputFileSize-offset)>BUFSIZE ? BUFSIZE : parser.inputFileSize-offset
		text += yield readAsync(hInputFile,length)
		while(text.indexOf("<page>")>-1 && text.indexOf("</page>")>-1){
			var page = text.match(/<page>([\s\S]*?)<\/page>/)[1]
			parser.parse(page)
			text=text.substr(text.indexOf("</page>")+"</page>".length,text.length)
		}
		offset += BUFSIZE
	}
	console.log("Reading WikipediaXML finished.")

}).catch(function(err){
	console.log(err.message)
})

