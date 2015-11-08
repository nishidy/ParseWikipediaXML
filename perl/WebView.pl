use Plack::Builder;
use Plack::Request;
use Template;
use Redis;

sub arr_in_arr {
	my $in_arr = shift;

	my @arr_in_arr;
	my @arr;
	my $c = 0;
	for my $a (@{$in_arr}) {
		if(($c+=1)%2){
			@arr = ();
			push(@arr,$a);
		}else{
			push(@arr,$a);
			push(@arr_in_arr,[@arr]);
		}
	}

	return @arr_in_arr;
};

my $app = sub {
	my $env = shift;

	my $redis = Redis->new(server => "127.0.0.1:6379");

	my @total_num = $redis->zrevrange('total_num',0,-1,'withscores');
	my @num = $redis->zrevrange('num',0,-1,'withscores');

	@total_num = arr_in_arr(\@total_num),
	@num = arr_in_arr(\@num),

	my $pages = 0;
	#foreach my @key (@total_num) {
	#	$pages += $key[1];
	#}
	foreach my $key (@total_num) {
		$pages += ${$key}[1];
	}

	my $start_time = $redis->get('start_time');
	my $finish_time = $redis->get('finish_time');
	my $duration = $finish_time - $start_time;

	my $vars =  +{
		pages => $pages,
		duration => $duration,
		total_num => \@total_num,
		num => \@num,
	};

	my $tt = new Template;
	$tt->process('views/index.html', $vars , \my $html) or die $tt->error;

	return [ 200, [ "Content-Type" => "text/html" ], [ $html ] ];

};

builder {
	mount "/" => $app;
};

