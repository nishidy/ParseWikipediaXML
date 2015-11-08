use Plack::Builder;
use Plack::Request;
use Template;
use Redis;

my $app = sub {
	my $env = shift;

	my $redis = Redis->new(server => "127.0.0.1:6379");

	my %total_num = $redis->zrange('total_num',0,-1,'withscores');
	my %num = $redis->zrange('num',0,-1,'withscores');

	my $pages = 0;
	foreach my $key (keys(%total_num)) {
		$pages += $hash_total_num{$key};
	}

	my $start_time = $redis->get('start_time');
	my $finish_time = $redis->get('finish_time');
	my $duration = $finish_time - $start_time;

	my $vars =  +{
		pages => $pages,
		duration => $duration,
		total_num => \%total_num,
		num => \%num,
	};

	my $tt = new Template;
	$tt->process('views/index.html', $vars , \my $html) or die $tt->error;

	return [ 200, [ "Content-Type" => "text/html" ], [ $html ] ];

};

builder {
	mount "/" => $app;
};

