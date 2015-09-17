var co = require('co')
var fs = require('fs')
var async = require('async')
var minimist = require('minimist')

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
	boolean: [],
	alias: {
		i: 'inWikiFile',
		d: 'inDictFile',
		s: 'outBofwFile',
		t: 'outTitleFile',
		m: 'minWordsInDoc',
		x: 'maxWordsInDoc',
		c: 'minWordForCount',
	},
	default: {
		minWordsInDoc: 2,
		maxWordsInDoc: 65535,
		minWordForCount: 1,
	},
})
	

var inputFile = argv.i
var outputFile = argv.s

var stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your".split(",")

var stats = fs.statSync(inputFile)
var fileSize = stats["size"]

var bufsize = 512

function openAsync(file){
	return new Promise(function(resolve,reject){
		fs.open(file,'r',function(err,fd){
			if(err){ reject(err); return }
			resolve(fd)
		})
	})
}

function readAsync(fd,length){
	return new Promise(function(resolve,reject){
		var buf = new Buffer(bufsize)
		fs.read(fd,buf,0,bufsize,null,function(err,bs,b){
			if(err){ reject(err); return }
			text = b.slice(0,length).toString('utf8')
			resolve(text)
		})
	})
}

function parse(page){
	var mapWordFreq = {}
	var numWordsInDoc = 0
	var text= page.match(/<text[^<>]*>([\s\S]*?)<\/text>/)[1]
	text.replace(/\n/g," ").split(" ").forEach(function(word){
		word = word.toLowerCase()

		if(word.match(/^[0-9a-zA-Z][\-0-9a-zA-Z]*$/)==null){ return }

		if(stopwords.indexOf(word)>-1){ return }

		if(word in mapWordFreq){
			mapWordFreq[word] += 1
		}else{
			mapWordFreq[word] = 1
		}

		numWordsInDoc++;

	})

	var bofw = ""
	for(var word in mapWordFreq){
		bofw += word + " " + mapWordFreq[word] + " "
	}

	if(numWordsInDoc>argv.m && bofw.length>0){
		fs.appendFileSync(outputFile,bofw.slice(0,bofw.length-1)+"\n")
	}
}

co(function *(){

	var fd = yield openAsync(inputFile)

	var text = ""
	var offset = 0
	while(offset<fileSize){
		var length = (fileSize-offset)>bufsize ? bufsize : fileSize-offset
		text += yield readAsync(fd,length)
		while(text.indexOf("<page>")>-1 && text.indexOf("</page>")>-1){
			var page = text.match(/<page>([\s\S]*?)<\/page>/)[1]
			parse(page)
			text=text.substr(text.indexOf("</page>")+"</page>".length,text.length)
		}
		offset += bufsize
	}

}).catch(function(err){
	console.log(err.message)
})


