<style type="text/css">
<!--
table {
	table-layout: fixed;
	width: 75%;
}

th.title { width: 50%; }
td.value { width: 50%; }

-->
</style>

<table border=1>
	<tr>
		<td>Total parsed pages : {{.Pages}} pages</td>
		<td>Duration : {{.Duration}} sec</td>
		<td>Throughput (Speed to parse) : {{printf "%.2f" .Throughput}} pages/sec</td>
	</tr>
</table>

<br>

<table border=1>
	<tr class=title>
		<th>Range of total number of words in a page</th>
		<th>Count of pages</th>
	</tr>
	{{range $index, $report := .Total_num}}
	<tr class=value>
		<td>{{$report.Member}} - {{Func $report.Member}}</td>
		<td>{{$report.Score}}</td>
	</tr>
	{{end}}
</table>


<br>

<table border=1>
	<tr class=title>
		<th>Range of number of words in a page</th>
		<th>Count of pages</th>
	</tr>
	{{range $index, $report := .Num}}
	<tr class=value>
		<td>{{$report.Member}} - {{Func $report.Member}}</td>
		<td>{{$report.Score}}</td>
	</tr>
	{{end}}
</table>
