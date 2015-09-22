#import <stdio.h>
#import <Foundation/NSObject.h>
#import "YLFileReader.h"
 
@interface Parser : NSObject
{
	NSArray *stopwords;
	NSRegularExpression *regexForTerm;
}
- (NSString *)parsePage:(NSString *)page;
@end
 
@implementation Parser

- (id)init{

	[super init];

	stopwords = [ 
		@"a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your"
		componentsSeparatedByString:@","
	];

	NSError *error;
	regexForTerm=
		[ NSRegularExpression
			regularExpressionWithPattern:@"^[a-z][0-9a-z'-]*[0-9a-z]$"
			options:0
			error:&error
		];

	return self;
}

- (NSString *)parsePage:(NSString *)page{

	NSMutableDictionary *mapDict = [NSMutableDictionary dictionary];

	NSError *error = nil;
	NSString *text = @"";

	NSRegularExpression *regex = [NSRegularExpression regularExpressionWithPattern:@"<text.*?>([^<>]*)</text>" options:0 error:&error];

	NSTextCheckingResult *matches =
		[ regex
			firstMatchInString:page
			options:0
			range:NSMakeRange(0,page.length)
		];

	text = [ page substringWithRange:[matches rangeAtIndex:1] ];

	NSArray *terms = [ text componentsSeparatedByString:@" " ];

	int numTermInDoc = 0;
	for( NSString *term in terms ){

		NSRange rangeOfMatchForTerm =
			[ regexForTerm
				rangeOfFirstMatchInString:term
				options:0
				range:NSMakeRange(0,term.length)
			];

		if(rangeOfMatchForTerm.location == NSNotFound){ continue; }
		if([stopwords indexOfObject:term] != NSNotFound){ continue; }

		id freq = [mapDict objectForKey:term];
		if(freq == nil){
			mapDict[term] = @"1";
		}else{
			mapDict[term] = [NSString stringWithFormat:@"%ld",[freq integerValue]+1];
		}
		numTermInDoc ++;
	}

	NSString *bofw = @"";
	for(id key in [mapDict keyEnumerator]){
		bofw = [bofw stringByAppendingString: [NSString stringWithFormat:@"%@ %@ ",key,[mapDict valueForKey:key]]];
	}
	if([bofw length])
		bofw = [bofw stringByAppendingString: @"\n"];

	return bofw;
}
 
@end
 

int main(int argc, const char *argv[]) {
 
	NSArray *args = [[NSProcessInfo processInfo] arguments];
	id parser = [Parser new];

	NSString *inWikiFile= [[NSString alloc] initWithString:[args objectAtIndex:1]];
	NSString *outBofwFile= [[NSString alloc] initWithString:[args objectAtIndex:2]];

	NSStringEncoding encoding = NSASCIIStringEncoding;
	NSInteger bufferSize = 65536;
	YLFileReader *hInWikiFile =
		[[YLFileReader alloc] initWithFilePath:inWikiFile encoding:encoding bufferSize:bufferSize];

	NSFileManager *fileManager = [NSFileManager defaultManager];
	BOOL result = [fileManager fileExistsAtPath:outBofwFile];
	if(![fileManager fileExistsAtPath:outBofwFile]){
		if( !result ){
			result = [fileManager createFileAtPath:outBofwFile contents:nil attributes:nil];
		}
		if( !result ){
			NSLog(@"Failed to create file %@",outBofwFile);
			return 2;
		}
	}

	NSFileHandle *hOutBofwFile = [NSFileHandle fileHandleForWritingAtPath:outBofwFile];
	if(!hOutBofwFile){
		NSLog(@"Failed to create file handler for %@",outBofwFile);
		return 1;
	}

	NSString *page = @"";
	NSString *line;
	BOOL insidePageTag = false;
	BOOL outsidePageTag = false;

	while ((line=[hInWikiFile readLine]) != nil){

		if([line rangeOfString:@"<page>"].location != NSNotFound){ insidePageTag = true; }
		if([line rangeOfString:@"</page>"].location != NSNotFound){ outsidePageTag = true; }

		if(insidePageTag){ page = [page stringByAppendingString:line]; }

		if(outsidePageTag){

			NSError *error = nil;
			NSRegularExpression *regex =
				[ NSRegularExpression
					regularExpressionWithPattern:@"<page>([\\s\\S]*)</page>"
					options:0
					error:&error
				];

			NSTextCheckingResult *matches =
				[ regex
					firstMatchInString:page
					options:0
					range:NSMakeRange(0,page.length)
				];

			page = [page substringWithRange:[matches rangeAtIndex:1]];

			NSString *bofw = [parser parsePage:page];

			if(![bofw isEqual:@"\n"]){
				NSData *outBofwData =
					[ NSData
						dataWithBytes:bofw.UTF8String
						length:bofw.length
					];
				[hOutBofwFile seekToEndOfFile];
				[hOutBofwFile writeData:outBofwData];
			}

			insidePageTag=outsidePageTag= false;
			page = @"";
		}
	}

	[hOutBofwFile closeFile];
	[hInWikiFile close];

	return 0;
}
