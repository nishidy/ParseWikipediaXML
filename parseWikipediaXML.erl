-module(parseWikipediaXML). 
-export([main/0]). 
-import(string,[tokens/2,to_lower/1,to_integer/1]). 
-import(lists,[concat/1,map/2,filter/2,nth/2,any/2]). 

main() ->
	take_args(init:get_arguments()),
	%io:format("~p.~n",[init:get_arguments()]),
	%io:format("~p.~n",[get()]),

	case get(o) of
		undefined -> error(badarg);
		Fs ->
			case file:open(Fs,write) of
				{ok,Fho} -> put(fho,Fho);
				{error,Reaso} ->io:format("~p.~n",[Reaso])
			end
	end,

	case get(i) of
		undefined -> error(badarg);
		Fi ->
			case file:open(Fi,read) of
				{ok,Fhi} -> put(fhi,Fhi);
				{error,Reason} ->io:format("~p.~n",[Reason])
			end
	end,

	case get(fhi) of
		undefined -> error(badarg);
		File ->
			line_concat(File,"")
	end. 

take_args([{c,[V|_]}|T]) ->
	{I,_} = to_integer(V),
	put(c,I),
	take_args(T);
take_args([{K,[V|_]}|T]) ->
	put(K,V),
	take_args(T);
take_args([{K,[]}|T]) ->
	put(K,[]),
	take_args(T);
take_args([]) -> true. 

if_stopwords(Word) ->
	Stopwords = "a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your",
	any(fun(X)-> X==Word end,tokens(Stopwords,",")). 

chomp([H|T]) ->
	case T of
		"\n" -> [H];
		[] -> [];
		_ -> [H | chomp(T)]
	end. 

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

disp_processmap(_,undefined,_) -> error(badarg);
disp_processmap([{_,V}|T],Fho,C) when V<C ->
	%io:format("~p ~p.~n",[C,V]),
	if
		T == [] ->
			io:format(Fho,"~n",[]);
		true ->
			disp_processmap(T,Fho,C)
	end;
disp_processmap([{K,V}|T],Fho,C) ->
	if
		T == [] ->
			io:format(Fho,"~s ~p~n",[K,V]);
		true ->
			io:format(Fho,"~s ~p ",[K,V]),
			disp_processmap(T,Fho,C)
	end;
disp_processmap([],Fho,_) ->
	io:format(Fho,"~n",[]). 

line_concat(File,Line) ->
	%io:format("Line=~p.~n",[Line]),

	% Regular expression '.' does not match with \n.
	% So, \n will be deleted by chomp.

	{ok,MP}=re:compile("<text[^<>]*>(.*)</text>"),
	case re:run([Line],MP) of

		nomatch ->

			case file:read_line(File) of
				{ok,Data} ->
					line_concat(File,concat([Line,chomp(Data),' ']));
				eof -> 
					io:format("Finish reading file.~n");
				{error,Reason} -> 
					io:format("Error ~p.~n",[Reason]),
					exit(badarith)
			end;

		{match,Match} -> 

			% Match contains index starting from 0
			% However, sublist takes index starting from 1

			{Idx,Num} = nth(1,Match),
			%io:format("~p~n",[tokens(lists:sublist(Line,Idx+1,Num),"\n,. ")]),

			case get(u) of

				undefined ->

					word_count(
						filter(
							fun(X) -> tokenizer(X) andalso not if_stopwords(X) end,
							map(fun(X)->to_lower(X)end,tokens(lists:sublist(Line,Idx+1,Num),"\n,. "))
						)
					),
					disp_processmap(get(),get(fho),get(c)),
					erase(),
					take_args(init:get_arguments());

				_ ->
					Wordlist=
						filter(
							fun(X) -> tokenizer(X) andalso not if_stopwords(X) end,
							map(fun(X)->to_lower(X)end,tokens(lists:sublist(Line,Idx+1,Num),"\n,. "))
						),
					Fho = get(fho),
					C = get(c),
					spawn(
						fun() ->
							word_count(Wordlist),
							disp_processmap(get(),Fho,C),
							erase()
						end
					)

			end,

			case file:read_line(File) of
				{ok,Data} ->
					line_concat(File,concat([chomp(Data)," "]));
				eof -> 
					io:format("Finish reading file.~n");
				{error,Reason} -> 
					io:format("Error ~p.~n",[Reason]),
					exit(badarith)
			end

	end. 

