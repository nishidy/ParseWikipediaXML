use strict;
use warnings;
use Getopt::ArgParse;
use List::Util qw/reduce/;
use Data::Dumper;
use threads;
use Thread::Queue;

my $ap = Getopt::ArgParse->new_parser(
	prog => 'ParseWikipediaXML',
	description => '',
	epilog => ''
);

$ap->add_arg('--ifwiki','-i',required=>1,help=>"Wikipedia XML file as input");
$ap->add_arg('--ifdict','-d',default=>"",help=>"Dictionary file as input ");
$ap->add_arg('--ofcont','-s',required=>1,help=>"Output file with bag-of-words of each page");
$ap->add_arg('--oftitle','-t',help=>"Output file with title of each page");
$ap->add_arg('--minw','-m',default=>1,help=>"Minimum number of words which each page should contain");
$ap->add_arg('--maxw','-x',default=>65535,help=>"Maximum number of words which each page should contain");
$ap->add_arg('--minc','-c',default=>2,help=>"Minimum number which each word should have in each page");
$ap->add_arg('--recateg','-g',default=>".*",help=>"Regular expresion which each page should match with its category title");
$ap->add_arg('--ngram','-n',default=>1, help=>"The N number for N-gram");
my $args = $ap->parse_args();

my $stopword="a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";
my @stopwords=split(/,/,$stopword);

my %hashDict;
my $fh;
if( $args->ifdict ne "" ){
	open $fh, '<', $args->ifdict or die "Cannot open $args->ifdict :$!";

	while(<$fh>){
		if( index($_,";;;") == -1 ){
			my @words = map { $_ =~ s/\s*$//; $_ } split(/\t/, $_);
			$hashDict{$words[0]} = $words[2];
		}
	}

	close $fh;
}
#warn Dumper %hashDict;


my $fout;
open $fout, '>', $args->ofcont or die "Cannot open $args->ofcont:$!";
open $fh, '<', $args->ifwiki or die "Cannot open $args->ifwiki:$!";

my $queue = new Thread::Queue;
my $outmtx :shared;
my $finish :shared = 0;

my @threads;

my $host=`uname`;
chomp($host);

my $cpus=1;
if( $host eq "Darwin" ){
	$cpus = `cpus=\$(system_profiler SPHardwareDataType|grep "Total Number of Cores:");echo \${cpus#*: }`;
}else{
	$cpus = `cat /proc/cpuinfo | grep processor | wc -l`;
	chomp($cpus)
}
$cpus = $cpus == 1 ? 1 : $cpus-1;

for(my $c=0;$c<$cpus;$c++){
	my $th = threads->create(\&bowCreate);
	push(@threads, $th);
}
#my $nthread = @threads;

my $sflag=0;
my $eflag=0;
my $page="";
while(<$fh>){
	$sflag=1 if index($_, "<page>") > -1;
	$eflag=1 if index($_, "</page>") > -1;
	$page.=$_ if $sflag;
	if($eflag){
		$queue->enqueue($page);
		$sflag=$eflag=0;
		$page="";
	}
}
$finish = 1;

$_->join() foreach @threads;

close $fout;
close $fh;

sub bowCreate {

	for(;;){

		if($finish){
			last unless $queue->pending;
		}

		# "my" creates another $page in this scope
		my $page = $queue->dequeue();

		#my $recateg = qr/<title[^<>]*>($args->recateg)<\/title>/;
		my $recateg = "<title[^<>]*>(".$args->recateg.")<\/title>";
		next unless $page =~ /$recateg/;

		my $text;
		if( $page =~ /<text[^<>]*>([^<>]+)<\/text>/ ){
			$text = $1;
		}else{
			next;
		}

		my %hashDoc;
		my $docCount=0;
		my @ngrams;
		my @words = split(/ /,$text);
		foreach my $word ( map { chomp; lc } @words ) {
			next unless $word =~ "^[a-z][a-z0-9'-]*[a-z0-9]\$";
			next if grep { $_ eq $word } @stopwords;

			my $dword;
			if(exists $hashDict{$word}){
				$dword = $hashDict{$word};
			}else{
				$dword = $word;
			}

			push(@ngrams, ($dword));
			# Note that $#ngrams returns the number of last element starting from 0
			if( $#ngrams+1 < $args->ngram ){
				next;
			}
			my $keyword = join ":", @ngrams;

			if(exists $hashDoc{$keyword}){
				$hashDoc{$keyword}++;
			}else{
				$hashDoc{$keyword}=1;
			}
			$docCount++;

			shift(@ngrams);
		}
		next if $docCount < $args->minw or $docCount > $args->maxw;

		my $output =
			reduce {
				$hashDoc{$b} >= $args->minc ?
					( defined($a) ? $a." ".$b." ".$hashDoc{$b} : $b." ".$hashDoc{$b} ) : $a
			} undef, sort{ $hashDoc{$b} <=> $hashDoc{$a} || $a cmp $b } keys %hashDoc;

		if(defined($output)){
			lock($outmtx);
			# INSERT HERE
			print $fout $output."\n";
		}

	}

}

