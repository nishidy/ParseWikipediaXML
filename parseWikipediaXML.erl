-module(parseWikipediaXML). 
-export([main/0]). 
-import(string,[tokens/2,to_lower/1,to_integer/1,join/2]). 
-import(lists,[sublist/3,concat/1,map/2,filter/2,nth/2,any/2,reverse/1,keyfind/3,keydelete/3]). 

main() ->
	Args=take_args(init:get_arguments(),[{c,1},{n,1}]),
	%io:format("~p.~n",[init:get_arguments()]),
	%io:format("~p.~n",[Args]),

	case keyfind(fhi,1,Args) of
		false -> error(badarg);
		{fhi,Fhi} ->
			line_concat(Args,Fhi,"")
	end. 

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

line_concat(Args,File,Line) ->
	%io:format("Line=~p.~n",[Line]),

	% Regular expression '.' does not match with \n.
	% So, \n will be deleted by chomp.

	{ok,MP}=re:compile("<text[^<>]*>(.*)</text>"),
	case re:run([Line],MP) of

		nomatch ->

			case file:read_line(File) of
				{ok,Data} ->
					line_concat(Args,File,concat([Line,chomp(Data),' ']));
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
			%io:format("~p~n",[tokens(sublist(Line,Idx+1,Num),"\n,. ")]),

			{fho,Fho} = keyfind(fho,1,Args),
			{c,C} = keyfind(c,1,Args),
			{n,N} = keyfind(n,1,Args),

			case keyfind(u,1,Args) of

				false ->

					case N of
						1 ->
							word_count(
								filter(
									fun(X) -> tokenizer(X) andalso not if_stopwords(X) end,
									map(fun(X)->to_lower(X)end,tokens(sublist(Line,Idx+1,Num),"\n,. "))
								)
							);

						N ->
							ngram_count(
								filter(
									fun(X) -> tokenizer(X) andalso not if_stopwords(X) end,
									map(fun(X)->to_lower(X)end,tokens(sublist(Line,Idx+1,Num),"\n,. "))
								),
								[],
								N
							)
					end,

					disp_processmap(get(),Fho,C),
					erase();

				_ ->
					Wordlist=
						filter(
							fun(X) -> tokenizer(X) andalso not if_stopwords(X) end,
							map(fun(X)->to_lower(X)end,tokens(sublist(Line,Idx+1,Num),"\n,. "))
						),
					spawn(
						fun() ->
							word_count(Wordlist),
							disp_processmap(get(),Fho,C)
						end
					)

			end,

			case file:read_line(File) of
				{ok,Data} ->
					line_concat(Args,File,concat([chomp(Data)," "]));
				eof -> 
					io:format("Finish reading file.~n");
				{error,Reason} -> 
					io:format("Error ~p.~n",[Reason]),
					exit(badarith)
			end

	end. 

