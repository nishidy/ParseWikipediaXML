-module(parseWikipediaXML).
-export([main/0]). 
-import(string,[tokens/2,to_lower/1,to_integer/1,join/2]). 
-import(lists,[sublist/3,concat/1,map/2,filter/2,nth/2,any/2,reverse/1,keyfind/3,keydelete/3]). 

%$ erl -noshell -s parseWikipediaXML main -i ../share/enwiki-test-5000 -o o1 -s init stop

main() ->
	Args=take_args(init:get_arguments(),[{c,1},{n,1}]),
	%io:format("~p.~n",[init:get_arguments()]),
	%io:format("~p.~n",[Args]),

	case keyfind(fhi,1,Args) of
		false -> error(badarg);
		{fhi,Fhi} ->
			line_concat( Args, Fhi, "" )
	end. 

take_args([{K,[_]}|_],_) when
	K=:=h ->
        io:format("-c count : Mininum number of a term~n"),
        io:format("-n ngram : N-gram supported~n"),
        io:format("-o path : Output file path~n"),
        io:format("-i path : Input file path~n"),
        io:format("-p : Spawn processes~n"),
        io:format("-h : Show this message~n");
take_args([{K,[V|_]}|T],Args) when
	K=:=c; K=:=n ->
	NewArgs =
		case keyfind(K,1,Args) of
			false -> Args;
			_ -> keydelete(K,1,Args)
		end,
	{I,_} = to_integer(V),
	take_args(T,[{K,I}|NewArgs]);
take_args([{K,[V|_]}|T],Args) when
	K=:=o ->
	case file:open(V,write) of
		{ok,Fho} ->
			take_args(T,[{K,V},{fho,Fho}|Args]);
		{error,FsReason} ->
			io:format("~p.~n",[FsReason])
	end;
take_args([{K,[V|_]}|T],Args) when
	K=:=i ->
	case file:open(V,read) of
		{ok,Fhi} ->
			take_args(T,[{K,V},{fhi,Fhi}|Args]);
		{error,FiReason} ->
			io:format("~p.~n",[FiReason])
	end;
take_args([{K,[V|_]}|T],Args) ->
	take_args(T,[{K,V}|Args]);
take_args([{K,[]}|T],Args) ->
	take_args(T,[{K,""}|Args]);
take_args([],Args) ->
	Args.

if_stopwords(Word) ->
	Stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your",
	any(fun(X)-> X==Word end,tokens(Stopwords,",")).

chomp([H|T]) ->
	case T of
		"\n" -> [H];
		[] -> [];
		_ -> [H | chomp(T)]
	end.

ngram_count([H|T],G,N) when length(G)<N ->
	ngram_count(T,[H|G],N);
ngram_count([H|T],G,N) ->
	%io:format("~p~n",[G]),
	M=join(reverse(G),":"),
	case get(M) of
		undefined -> put(M,1);
		C -> put(M,C+1)
	end,
	case T of
		[] -> "";
		_  -> ngram_count(T,[H|sublist(G,1,length(G)-1)],N)
	end;
ngram_count([],_,_) -> "".

word_count([H|T]) ->
	%%io:format("~p~p~n",[H,T]),
	case get(H) of
		undefined -> put(H,1);
		C -> put(H,C+1)
	end,
	case T of
		[] -> "";
		_  -> word_count(T)
	end;
word_count([]) -> "".

tokenizer(Word) ->
	{ok,MP}=re:compile("^[a-z0-9][a-z0-9-]*$"),
	case re:run([Word],MP) of
		nomatch -> false;
		{match,_}-> true
	end.

filter_pmap([],P,_) -> P;
filter_pmap([{_,V}|T],P,C) when V<C ->
    filter_pmap(T,P,C);
filter_pmap([{K,V}|T],P,C) ->
    filter_pmap(T,[{K,V}|P],C).

save_pmap(_,undefined) -> error(badarg);
save_pmap([{K,V}|T],Fho) ->
	if
		T == [] ->
			io:format(Fho,"~s ~p~n",[K,V]);
		true ->
			io:format(Fho,"~s ~p ",[K,V]),
			save_pmap(T,Fho)
	end;
save_pmap([],Fho) ->
	io:format(Fho,"~n",[]).

line_read(File) ->
	case file:read_line(File) of
		eof -> eof;
		{ok,Data} ->
			chomp(Data);
		{error,Reason} ->
			io:format("Error ~p.~n",[Reason]),
			exit(badarith)
	end.

line_concat(Args,File,Line) ->
	L = line_read(File),
    case L of
        eof ->
			io:format("Finish reading file.~n");
        _ ->
        	Lines = concat([Line,L,' ']),
        	%io:format("Page=~p.~n",[Page]),
            parse_text(Args, File, Lines)
    end.

parse_text(Args, File, Lines) ->
	% Regular expression '.' does not match with \n.
	% So, \n will be deleted by chomp.
	{ok,MP}=re:compile("<text[^<>]*>(.*)</text>"),
	case re:run([Lines],MP) of
        nomatch ->
            line_concat(Args, File, Lines);
		{match, Match} ->
            % Match contains index starting from 0
            % However, sublist takes index starting from 1
            {Idx, Num} = nth(1,Match),
            Text = tokens(sublist(Lines, Idx+1, Num),"\n,. "),
	        %io:format("~p~n",[tokens(sublist(Line,Idx+1,Num),"\n,. ")]),
            run_parse(Args, Text),
            line_concat(Args, File, "")
    end.

run_parse(Args, Text) ->

	{fho,Fho} = keyfind(fho,1,Args),
	{c,C} = keyfind(c,1,Args),
	{n,N} = keyfind(n,1,Args),

	WordList=
		filter(
			fun(X) -> tokenizer(X) andalso not if_stopwords(X) end,
			map(fun(X)->to_lower(X)end,Text)
		),

	case keyfind(p,1,Args) of
		false ->
            seq_parse(WordList, N),
            post_parse(Fho,C);
		_ ->
            par_parse(WordList, Fho, C)
    end.

seq_parse(WordList,N) ->
	case N of
		1 ->
			word_count( WordList );
		N ->
			ngram_count( WordList, [], N )
	end.

par_parse(WordList, Fho,C) ->
	spawn(
		fun() ->
			word_count(WordList),
            post_parse(Fho,C)
		end
	).

post_parse(Fho,C)->
    Pmap = filter_pmap(get(),[],C),
    case Pmap of
        [] -> "";
        _ -> save_pmap(Pmap,Fho)
    end,
	erase().

