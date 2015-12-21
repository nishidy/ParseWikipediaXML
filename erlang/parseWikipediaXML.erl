-module(parseWikipediaXML).
-export([main/0,counter_daemon/2,process_wait/2]).
-import(string,[tokens/2,to_lower/1,to_integer/1,join/2]).
-import(lists,[sublist/3,concat/1,map/2,filter/2,nth/2,any/2,reverse/1,keyfind/3,keydelete/3,reverse/1,sort/2]).
-import(maps,[put/3,find/2]).

%$ erl -noshell -s parseWikipediaXML main -i ../share/enwiki-test-5000 -o o1 -s init stop

main() ->
    Args=take_args(init:get_arguments(),[{c,1},{n,1}]),
    case keyfind(h,1,Args) of
        false -> start_parse(Args);
        _ -> unit
    end.

process_wait(Num,Last) ->
    receive
        {add, _} -> process_wait(Num+1,Last);
        {done,_} when Num == 1 ->
            case Last of
                true  -> unit;
                false -> process_wait(Num-1,Last)
            end;
        {done,_} -> process_wait(Num-1,Last);
        {join,_} when Num == 0 -> unit;
        {join,_} -> process_wait(Num,true)
    end.

counter_daemon(Pages,Lines) ->
    receive
        {get, Pid} ->
            Pid ! { ok, {Pages, Lines}, self() },
            counter_daemon(Pages, Lines);
        {incr1, Pid} ->
            Newlines = Lines+1,
            Pid ! { ok, {Pages, Newlines}, self() },
            counter_daemon(Pages, Newlines);
        {incr2, Pid} ->
            Newlines = Lines+1,
            Newpages = Pages+1,
            Pid ! { ok, {Newpages, Newlines}, self() },
            counter_daemon(Newpages, Newlines);
        _ -> unit
    end.

start_parse(Args) ->

    register(cd, spawn(parseWikipediaXML, counter_daemon, [0,0] )),
    register(pw, spawn(parseWikipediaXML, process_wait, [0,false] )),

    Dict = read_dictionary(Args),
    case keyfind(fhi,1,Args) of
        false -> error(badarg);
        {fhi,Fhi} ->
            Usec = 1000000.0,
            {_,S,US} = os:timestamp(),
            line_concat(Args, Dict, Fhi, ""),
            {_,F,UF} = os:timestamp(),
            io:format(" > Read database in ~.3f sec.~n",[((F*Usec+UF)-(S*Usec+US))/Usec])
    end.

read_dictionary(Args) ->
    Usec = 1000000.0,
    {_,S,US} = os:timestamp(),
    Maps = case keyfind(fhd,1,Args) of
        false -> error(badarg);
        {fhd,Fhd} -> read_baseforms(Fhd, maps:new(), 0, 0)
    end,
    {_,F,UF} = os:timestamp(),
    io:format("~n > Read dictionary in ~.3f sec.~n",[((F*Usec+UF)-(S*Usec+US))/Usec]),
    Maps.

read_baseforms(Fhd, Maps, Loaded, Parsed) ->
    io:format(" > Read dictionary [ # word (loaded/parsed) ~.10B / ~.10B ]\r",
              [Loaded, Parsed]),
    L = line_read(Fhd),
    case L of
        eof -> Maps;
        _-> get_baseform(Fhd, Maps, L, Loaded, Parsed)
    end.

get_baseform(Fhd, Maps, L, Loaded, Parsed) ->
    Newmaps = case tokens(L,"\t") of
        [";;;"|_] -> Maps;
        [P|[B|_]] ->
            case any(fun(X)-> X==$\ end, B) of
                true -> Maps;
                false -> put(chomp(P),chomp(B),Maps)
            end;
        _ -> Maps
    end,
    if
        Newmaps == Maps -> read_baseforms(Fhd, Newmaps, Loaded, Parsed+1);
        true  -> read_baseforms(Fhd, Newmaps, Loaded+1, Parsed+1)
    end.


take_args([{K,[]}|_],Args) when
    K=:=h ->
        io:format("-c count : Mininum number of a term~n"),
        io:format("-n ngram : N-gram supported~n"),
        io:format("-o path : Output file path~n"),
        io:format("-t path : Output title path~n"),
        io:format("-i path : Input file path~n"),
        io:format("-d path : Input dictionary file path~n"),
        io:format("-p : Spawn processes~n"),
        io:format("-h : Show this message~n"),
        [{h,[]}|Args];
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
    K=:=o; K=:=t ->
    case file:open(V,write) of
        {ok,F} ->
            case K of
                o -> take_args(T,[{K,V},{fho,F}|Args]);
                t -> take_args(T,[{K,V},{fht,F}|Args])
            end;
        {error,FsReason} ->
            io:format("~p.~n",[FsReason])
    end;
take_args([{K,[V|_]}|T],Args) when
    K=:=i; K=:=d ->
    case file:open(V,read) of
        {ok,F} ->
            case K of
                i -> take_args(T,[{K,V},{fhi,F}|Args]);
                d -> take_args(T,[{K,V},{fhd,F}|Args])
            end;
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
    any(fun(X)-> X=:=Word end,tokens(Stopwords,",")).

chomp(L) ->
    Li = case reverse(L) of
        [$\ |T] -> T;
        [$\n|T] -> T;
        [] -> [];
        R -> R
    end,
    reverse(Li).

ngram_count([],_,_,Maps) -> Maps;
ngram_count([_|[]],_,_,Maps) -> Maps;
ngram_count([H|T],G,N,Maps) when length(G)<N ->
    ngram_count(T,[H|G],N,Maps);
ngram_count([H|T],G,N,Maps) ->
    %io:format("~p~n",[G]),
    M=join(reverse(G),":"),
    Newmaps = case find(M,Maps) of
        {ok,Freq} -> maps:update(M,Freq+1,Maps);
        _ -> put(M,1,Maps)
    end,
    ngram_count(T,[H|sublist(G,1,length(G)-1)],N,Newmaps).

word_count([H|T], Maps) ->
    Newmaps = case find(H,Maps) of
        {ok,Freq} -> maps:update(H,Freq+1,Maps);
        _ -> put(H,1,Maps)
    end,
    word_count(T,Newmaps);
word_count([], Maps) -> Maps.

tokenizer(Word) ->
    {ok,MP}=re:compile("^[a-z][a-z0-9-']*[a-z0-9]$"),
    case re:run([Word],MP) of
        nomatch -> false;
        {match,_}-> true
    end.

save_maps(_,undefined) -> error(badarg);
save_maps([],Fho) ->
    io:format(Fho,"~n",[]);
save_maps([{K,V}|[]],Fho) ->
    io:format(Fho,"~s ~p~n",[K,V]);
save_maps([{K,V}|T],Fho) ->
    io:format(Fho,"~s ~p ",[K,V]),
    save_maps(T,Fho).

line_read(File) ->
    case file:read_line(File) of
        eof -> eof;
        {ok,Data} -> chomp(Data);
        {error,Reason} ->
            io:format("Error ~p.~n",[Reason]),
            exit(badarith)
    end.

line_concat(Args,Dict,File,Line) ->
    L = line_read(File),
    case L of
        eof ->
            cd ! {get, self()},
            receive
                {ok, {Saved,Parsed}, _} ->
                    show_counts( Saved, Parsed ),
                    io:format("\n")
            end,
            pw ! { join, self() };
        _ ->
            Lines = concat([Line,L,' ']),
            %io:format("Page=~p.~n",[Page]),
            parse_text(Args, Dict, File, Lines)
    end.

parse_title(Lines) ->
    {ok,MP}=re:compile("<title[^<>]*>(.*)</title>"),
    case re:run([Lines],MP,[{capture,all_but_first}]) of
        nomatch -> "";
        {match, Match} ->
            {Idx, Num} = nth(1,Match),
            sublist(Lines, Idx+1, Num)
    end.

parse_text(Args, Dict, File, Lines) ->
    % Regular expression '.' does not match with \n.
    % So, \n will be deleted by chomp.
    {ok,MP}=re:compile("<text[^<>]*>(.*)</text>"),
    case re:run([Lines],MP,[{capture,all_but_first}]) of
        nomatch ->
            line_concat(Args, Dict, File, Lines);
        {match, Match} ->
            % Match contains index starting from 0
            % However, sublist takes index starting from 1
            {Idx, Num} = nth(1,Match),
            Text = tokens(sublist(Lines, Idx+1, Num),"\n,.; "),
            Title = parse_title(Lines),
            %io:format("~p~n",[tokens(sublist(Line,Idx+1,Num),"\n,. ")]),
            run_parse(Args, Dict, Text, Title),
            line_concat(Args, Dict, File, "")
    end.

run_parse(Args, Dict, Text, Title) ->

    {fho,Fho} = keyfind(fho,1,Args),
    {fht,Fht} = keyfind(fht,1,Args),
    {c,C} = keyfind(c,1,Args),
    {n,N} = keyfind(n,1,Args),

    WordList=
        filter(
            fun(X) -> tokenizer(X) andalso not if_stopwords(X) end,
            map(fun(X)->convert_word(X, Dict)end,Text)
        ),

    case keyfind(p,1,Args) of
        false ->
            Bofw = seq_parse(WordList, N),
            post_parse(Fho,Fht,Bofw,C,Title);
        _ ->
            par_parse(WordList,Fho,Fht,C,Title)
    end.

convert_word(Word,Dict) ->
    case find(to_lower(Word), Dict) of
        {ok,Baseform} -> Baseform;
        _ -> Word
    end.

seq_parse(WordList,N) ->
    case N of
        1 -> word_count(WordList, maps:new());
        N -> ngram_count(WordList, [], N, maps:new())
    end.

par_parse(WordList,Fho,Fht,C,Title) ->
    pw ! { add, self() },
    spawn(
        fun() ->
            Bofw = word_count(WordList, maps:new()),
            post_parse(Fho,Fht,Bofw,C,Title),
            pw ! { done, self() }
        end
    ).

post_parse(Fho,Fht,Bofw,C,Title)->
    B = maps:filter(fun(_,V)-> V>=C end, Bofw),
    case maps:size(B) of
        0 -> do_not_save();
        _ -> do_save(Fho,B,Fht,Title)
    end.

do_not_save() ->
    cd ! {incr1, self()},
    receive
        {ok, {Saved,Parsed}, _} ->
            show_counts( Saved, Parsed )
    end.

do_save(Fho,B,Fht,T) ->
    cd ! {incr2, self()},
    receive
        {ok, {Saved,Parsed}, _} ->
            show_counts( Saved, Parsed )
    end,
    save_maps(sort(fun({_,V1},{_,V2})-> V1>V2 end, maps:to_list(B)),Fho),
    io:format(Fht,"~s~n",[T]).

show_counts(Saved, Parsed) ->
    io:format(" > Read database [ # page (saved/parsed) ~.10B / ~.10B ]\r", [Saved, Parsed]).

