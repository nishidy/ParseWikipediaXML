#!/usr/bin/env perl

package main;
use strict;
use warnings;
use 5.6.0;
use utf8;
use Data::Dumper;
use Getopt::ArgParse;
use threads;
use Thread::Queue;
use Redis;
use Time::HiRes qw/ gettimeofday /;
use Mouse;

has Args => (
	is => "ro",
	isa => "Getopt::ArgParse::Namespace",
	default => sub { parseArgs() },
	required => 0,
);

__PACKAGE__->meta->make_immutable();

no Mouse;

main->new()->start();

sub parseArgs {
	my $self = shift;

	my $ap = Getopt::ArgParse->new_parser(
		prog => 'ParseWikipediaXML',
		description => '',
		epilog => ''
	);

	$ap->add_arg('--inWikiFile', '-i',
		required => 1, help => "Wikipedia XML file as input");
	$ap->add_arg('--inDictFile', '-d',
		default => undef, help => "Dictionary file as input ");
	$ap->add_arg('--outBofwFile', '-s',
		default => undef, help => "Output file with bag-of-words of each page");
	$ap->add_arg('--outTitleFile', '-t',
		help => "Output file with title of each page");
	$ap->add_arg('--minTermsInPage', '-m',
		default => 1, help => "How many terms page should have at least");
	$ap->add_arg('--maxTermsInPage', '-x',
		default => 65535, help => "How many terms page should have at most");
	$ap->add_arg('--minFreqsOfTerm', '-c',
		default => 2, help => "How many times a term should appear in a page");
	$ap->add_arg('--recateg', '-g',
		default => ".*", help => "Regex that should match with the category title");
	$ap->add_arg('--ngram', '-n',
		default => 1, help => "The N number for N-gram");
	$ap->add_arg('--isJapanese', '-j',
		default => undef, type => 'Bool', help => "If this is for Japanese text");
	$ap->add_arg('--workers', '-w',
		default => 1, help => "# of workers");

	return $ap->parse_args();
}

sub start {
	my $self = shift;

	my $redis = Redis->new(server => "127.0.0.1:6379");

	my $parser;
	if($self->{Args}->isJapanese){
		$parser = JapParser->new($self->{Args});
	}else{
		$parser = EngParser->new($self->{Args});
		$parser->readDictionary();
	}

	$redis->set( start_time => gettimeofday()."");
	$parser->startParse();
	$redis->set( finish_time => gettimeofday()."");

}

1;


package AbstParser;
use Encode;
use Devel::Peek;

sub new {
	my ($class, $args) = @_;

	my $queue = new Thread::Queue;
	my $mutex :shared;

	my $fout;
	open $fout, '>', $args->outBofwFile or $fout = undef;

	my $self = {
		Args => $args,
		hdlrOutBofwFile => $fout,
		PageQueue => $queue,
		WriteMutex => \$mutex,
		Finished => "::FINISHED::"
	};
	return bless $self, $class;
}

sub startParse {
	my $self = shift;

	my @threads;
	for(my $w=0;$w<$self->{Args}->workers;$w++){
		my $th = threads->create(\&ThreadWorker::bowCreate,$self);
		push(@threads, $th);
	}
	#my $nthread = @threads;

	my $fh;
	# This will lead auto decode by 'use utf8;' from UTF8 to UTF8-flagged
	open $fh, '<:utf8', $self->{Args}->inWikiFile or die "Cannot open $self->{Args}->inWikiFile:$!";

	my $sflag=0;
	my $eflag=0;
	my $page="";
	while(<$fh>){
		$sflag=1 if index($_, "<page>") > -1;
		$eflag=1 if index($_, "</page>") > -1;
		$page.=$_ if $sflag;
		if($eflag){

			# Devel::Peek::Dump($page);
			#   UTF8-flagged if explicitly open for UTF8 (auto decode)
			#   UTF8(not flagged) otherwise

			$self->{PageQueue}->enqueue($page);
			$sflag=$eflag=0;
			$page="";
		}
	}

	foreach (@threads) {
		$self->{PageQueue}->enqueue($self->{Finished});
		$_->join();
	}

	if( $self->{hdlOutBofwFile} ){
		close $self->{hdlrOutBofwFile};
	}
	close $fh;

}

sub writeToFile {
	my ($self,$bofw) = @_;

	if(defined($bofw)){
		lock($self->{WriteMutex});
		# Encode to UTF8
		print {$self->{hdlrOutBofwFile}?$self->{hdlrOutBofwFile}:*STDOUT} encode('utf-8',$bofw."\n");
	}
}

1;


package EngParser;
use base 'AbstParser';
use strict;
use utf8;
use List::Util qw/reduce/;
use Devel::Peek;
use Time::HiRes qw/ gettimeofday /;

sub new {
	my ($class,$args) = @_;
	my $super = $class->SUPER::new($args);

	my $stopword="a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";
	my @stopwords=split(/,/,$stopword);

	my %hashDict;
	my $self = {
		stopwords => \@stopwords,
		hashDict => \%hashDict,
	};

	$self = {%$super, %$self};
	return bless $self, $class;
}

sub readDictionary {
	my $self = shift;

	printf("Begin %s.\n", (caller 0)[3]);
	my $start = gettimeofday();

	my $fh;
	if( $self->{Args}->inDictFile ){
		open $fh, '<:utf8', $self->{Args}->inDictFile or die "Cannot open $self->{Args}->inDictFile :$!";

		while(<$fh>){
			if( index($_,";;;") == -1 ){
				my @words = map { $_ =~ s/\s*$//; $_ } split(/\t/, $_);
				$self->{hashDict}->{$words[0]} = $words[2];
			}
		}

		close $fh;
	}
	printf("Finished %s in %.2f sec.\n", (caller 0)[3],gettimeofday()-$start);
}

sub parseText {
	my( $self, $text ) = @_;

	my %hashDoc;
	my $totalWordNum=0;
	my @ngrams;
	my @words = split(/ /,$text);

	foreach my $word ( map { chomp; lc } @words ) {
		next unless $word =~ "^[a-z][a-z0-9'-]*[a-z0-9]\$";
		next if grep { $_ eq $word } @{$self->{stopwords}};

		my $dword;
		if(exists $self->{hashDict}{$word}){
			$dword = $self->{hashDict}{$word};
		}else{
			$dword = $word;
		}

		push(@ngrams, ($dword));
		# Note that $#ngrams returns the number of last element starting from 0
		if( $#ngrams+1 < $self->{Args}->ngram ){
			next;
		}
		my $keyword = join ":", @ngrams;

		if(exists $hashDoc{$keyword}){
			$hashDoc{$keyword}++;
		}else{
			$hashDoc{$keyword}=1;
		}
		#print "$keyword:$hashDoc{$keyword}\n";

		$totalWordNum++;

		shift(@ngrams);
	}

	if( $totalWordNum< $self->{Args}->minTermsInPage or
		$totalWordNum> $self->{Args}->maxTermsInPage ) {
		return ("",0,0);
	} else {
		my $output =
			reduce {
				$hashDoc{$b} >= $self->{Args}->minFreqsOfTerm ?
					( defined($a) ? $a." ".$b." ".$hashDoc{$b} : $b." ".$hashDoc{$b} ) : $a
			} undef, sort{ $hashDoc{$b} <=> $hashDoc{$a} || $a cmp $b } keys %hashDoc;

		my $wordNum = keys %hashDoc;
		return ($output, $totalWordNum, $wordNum);
	}

}

1;


package JapParser;
use base 'AbstParser';
use strict;
use warnings;
use utf8;
use Text::MeCab;
use Data::Dumper;
use List::Util qw/reduce/;
use Encode;

sub new {
	my ($class, $args) = @_;
	my $super = $class->SUPER::new($args);

	my $stopword = "の,に,は,を,た,が,で,て,と,し,れ,さ,ある,いる,も,する,から,な,こと,として,い,や,れる,など,なっ,ない,この,ため,その,あっ,よう,また,もの,という,あり,まで,られ,なる,へ,か,だ,これ,によって,により,おり,より,による,ず,なり,られる,において,ば,なかっ,なく,しかし,について,せ,だっ,その後,できる,それ,う,ので,なお,のみ,でき,き,つ,における,および,いう,さらに,でも,ら,たり,その他,に関する,たち,ます,ん,なら,に対して,特に,せる,及び,これら,とき,では,にて,ほか,ながら,うち,そして,とともに,ただし,かつて,それぞれ,または,お,ほど,ものの,に対する,ほとんど,と共に,といった,です,とも,ところ,ここ";

	# MeCab deals with UTF8 encoded dictionary.
	# Thus, better to encode $stopword to UTF8 here.
	# Note that $stopword is decoded to UTF8-flagged by 'use utf8;'.
	my @stopwords = split(/,/,encode("utf-8",$stopword));

	my $self = {
		stopwords => \@stopwords,
		mecab => Text::MeCab->new(),
	};

	$self = {%$super, %$self};
	return bless $self, $class;
}

sub parseText {
	my( $self, $text ) = @_;
	my %hashDoc;
	my $totalWordNum=0;

	# MeCab dictionary is compiled as UTF8
	# Thus need to give strings encoded with UTF8 to MeCab

	for( my $node = $self->{mecab}->parse(encode("utf-8",$text));
		$node->surface ;
		$node = $node->next ){

		next if grep { $_ eq $node->surface } @{$self->{stopwords}};

		# Decode to UTF8-flagged to compare with UTF8-flagged strings
		my @feature = split(/,/,decode("utf-8",$node->feature));
		my $baseform = $feature[6];

		next if( $baseform eq "*" );
		unless( $feature[0] eq "名詞" or\
			$feature[0] eq "動詞" or\
			$feature[0] eq "副詞" or\
			$feature[0] eq "形容詞" ){ next; }

		if (exists $hashDoc{$baseform}){
			$hashDoc{$baseform}++;
		}else{
			$hashDoc{$baseform}=1;
		}

		$totalWordNum++;
	}

	if( $totalWordNum < $self->{Args}->minTermsInPage or
		$totalWordNum > $self->{Args}->maxTermsInPage ) {
		return ("",0,0);
	} else {
		my $output =
			reduce {
				$hashDoc{$b} >= $self->{Args}->minFreqsOfTerm ?
					( defined($a) ? $a." ".$b." ".$hashDoc{$b} : $b." ".$hashDoc{$b} ) : $a
			} undef, sort{ $hashDoc{$b} <=> $hashDoc{$a} || $a cmp $b } keys %hashDoc;

		my $wordNum = keys %hashDoc;
		return ($output, $totalWordNum, $wordNum);
	}

}

1;


package ThreadWorker;

sub bowCreate {
	my $parser = shift;

	my $redis = Redis->new(server => "127.0.0.1:6379");

	for(;;){

		my $page = $parser->{PageQueue}->dequeue();
		if( $page eq $parser->{Finished} ) { last; }

		#my $recateg = qr/<title[^<>]*>($parser->{Args}->recateg)<\/title>/;
		my $recateg = "<title[^<>]*>(".$parser->{Args}->recateg.")<\/title>";
		next unless $page =~ /$recateg/;

		my $text;
		if( $page =~ /<text[^<>]*>([^<>]+)<\/text>/ ){
			$text = $1;
		}else{
			next;
		}

		my ($bofw,$totalNum,$num) = $parser->parseText($text);
		if($totalNum>0){
			$parser->writeToFile($bofw);
			$redis->zincrby('total_num', 1, int($totalNum/100)*100);
			$redis->zincrby('num', 1, int($num/100)*100);
		}

	}
}

1;

