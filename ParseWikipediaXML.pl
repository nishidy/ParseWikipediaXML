use strict;
use warnings;
use Getopt::ArgParse;
use List::Util qw/reduce/;
use Data::Dumper;

my $ap = Getopt::ArgParse->new_parser(
	prog => 'ParseWikipediaXML',
	description => '',
	epilog => ''
);

$ap->add_arg('--ifwiki','-i',required=>1);
$ap->add_arg('--ifdict','-d',required=>1);
$ap->add_arg('--ofcont','-s',required=>1);
$ap->add_arg('--oftitle','-t');
$ap->add_arg('--minw','-m',default=>1);
$ap->add_arg('--maxw','-x',default=>65535);
$ap->add_arg('--minc','-c',default=>2);
$ap->add_arg('--recateg','-g',default=>".*");
my $args = $ap->parse_args();

my $stopword="a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your";
my @stopwords=split(/,/,$stopword);

my %hashDict;
open my $fh, '<', $args->ifdict or die "Cannot open $args->ifdict :$!";

while(<$fh>){
	if( index($_,";;;") == -1 ){
		my @words = map { $_ =~ s/\s*$//; $_ } split(/\t/, $_);
		$hashDict{$words[0]} = $words[2];
	}
}
#warn Dumper %hashDict;

close $fh;

open $fh, '<', $args->ifwiki or die "Cannot open $args->ifwiki:$!";
open OUT, '>', $args->ofcont or die "Cannot open $args->ofcont:$!";
my $sflag=0;
my $eflag=0;
my $page="";
while(<$fh>){
	$sflag=1 if index($_, "<page>") > -1;
	$eflag=1 if index($_, "</page>") > -1;
	$page.=$_ if $sflag;
	if($eflag){
		&bowCreate;
		$sflag=$eflag=0;
		$page="";
	}
}

sub bowCreate {

	#my $recateg = qr/<title[^<>]*>($args->recateg)<\/title>/;
	my $recateg = "<title[^<>]*>(".$args->recateg.")<\/title>";
	return unless $page =~ /$recateg/;

	my $text;
	if( $page =~ /<text[^<>]*>([^<>]+)<\/text>/ ){
		$text = $1;
	}else{
		return;
	}

	my %hashDoc;
	my $docCount=0;
	my @words = split(/ /,$text);
	foreach my $word ( map { chomp($_); lc($_) } @words ) {
		next unless $word =~ "^[a-zA-Z0-9][a-zA-Z0-9-]*[a-zA-Z0-9]\$";
		next if grep { $_ eq $word } @stopwords;

		my $dword;
		if(exists $hashDict{$word}){
			$dword = $hashDict{$word};
		}else{
			$dword = $word;
		}

		if(exists $hashDoc{$dword}){
			$hashDoc{$dword}++;
		}else{
			$hashDoc{$dword}=1;
		}
		$docCount++;
	}
	return if $docCount < $args->minw or $docCount > $args->maxw;

	my $output = reduce { $hashDoc{$b} >= $args->minc ? defined($a) ? $a." ".$b." ".$hashDoc{$b} : $b." ".$hashDoc{$b} : $a } undef, sort{ $hashDoc{$b} <=> $hashDoc{$a} } keys %hashDoc;
	print OUT $output."\n" if defined($output);

}

close OUT;
close $fh;

