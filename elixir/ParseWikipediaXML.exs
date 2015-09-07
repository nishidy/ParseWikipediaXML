defmodule Main do

  def start(argv) do
    path = hd argv

    ppid = self()
    pid = spawn fn -> read_dict ppid end
    Process.register(pid,:dict)

    # To wait until reading dictionary finishes
    receive do
      {:finish, sender} when sender == pid ->
        IO.write "Finished reading dictionary. Let's go in 3 sec."

        :timer.sleep(1000)
        IO.write "."
        :timer.sleep(1000)
        IO.write "."
        :timer.sleep(1000)
        IO.puts ""

    after
      60_000 ->
        IO.puts "Timed out..."
        exit(1)

    end

    # To avoid conflict on stdio
    pid_ = spawn fn -> receive_output end
    Process.register(pid_, :output)

    counter_p = spawn fn -> counter ppid, 0, false end
    Process.register(counter_p, :counter)

    File.stream!(Path.absname(path),[:read],:line)
    |> Enum.reduce( "",
      fn x,t ->
        text = String.rstrip(t,?\n)
        if String.contains?(x,"</page>") do
          _ = spawn fn -> parse(text <> x) end
          send :counter, {:add}
          ""
        else
          text <> x
        end
      end
    )

    # To mark as reading WikipediaXML finished
    send :counter,{:mark}

    # This will be noticed when parsing text finishes
    receive do
      {:finish, pid} when pid == counter_p -> IO.puts "Finished."
    after
      60_000 -> IO.puts "Timed out..."
    end

  end

  def counter(ppid,cnt,mark) do
    receive do
      {:add} -> counter(ppid,cnt+1,mark)
      {:sub} when cnt == 1 and mark -> send ppid,{:finish,self()}
      {:sub} -> counter(ppid,cnt-1,mark)
      {:mark} -> counter(ppid,cnt,true)
    end
  end

  def parse(text) do
    minc = 2
    ngram_len=2

    stopwords = String.split("a,able,about,across,after,all,almost,also,am,among,an,and,any,are,as,at,be,because,been,but,by,can,cannot,could,dear,did,do,does,either,else,ever,every,for,from,get,got,had,has,have,he,her,hers,him,his,how,however,i,if,in,into,is,it,its,just,least,let,like,likely,may,me,might,most,must,my,neither,no,nor,not,of,off,often,on,only,or,other,our,own,rather,said,say,says,she,should,since,so,some,than,that,the,their,them,then,there,these,they,this,tis,to,too,twas,us,wants,was,we,were,what,when,where,which,while,who,whom,why,will,with,would,yet,you,your",",")

    Regex.run(~r/<text[^<>]*>(.*)<\/text>/,text)
    |> hd
    |> String.split
    |> Enum.map( &(String.rstrip &1,?\n))
    |> Enum.filter( &(tokenizer &1))
    |> Enum.filter( &(! Enum.any?(stopwords, fn x -> x==&1 end)))
    |> Enum.map(
      fn x ->
        send :dict, {:get, x, self()}
        receive do
          {:return, trans} -> trans
        end
      end
    )
    |> listup_ngram([],ngram_len,[])
    |> Enum.reduce( HashDict.new,
      fn x, bofw ->
        if Dict.has_key?(bofw,x),
        do: Dict.update!(bofw,x,&(&1+1)),
      else: Dict.put(bofw,x,1)
      end
    )
    |> Enum.filter( fn {_,v} -> v>=minc end)
    |> Enum.sort( fn {_,v1},{_,v2} -> v1 > v2 end)
    |> Enum.map( fn {k,v} -> k <> " " <> Integer.to_string v end)
    |> Enum.join( " ")
    |> output

    send :counter, {:sub}

  end

  def listup_ngram([h|t],ngram,len,ngram_list) when length(ngram) < len do
    listup_ngram(t,[h|ngram],len,ngram_list)
  end

  def listup_ngram([h|t],ngram,len,ngram_list) when length(ngram) == len do
    listup_ngram(
      t,
      [h|List.delete_at(ngram,len-1)],
      len,
      [Enum.join(Enum.reverse(ngram),":")|ngram_list]
    )
  end

  def listup_ngram([],_,_,ngram_list) do
    ngram_list
  end

  def output(text) do
    if String.length(text) > 0 do
      send :output, {:put, text}
    end
  end

  def receive_output do
    receive do
      {:put, text} ->
        IO.puts text
        receive_output
    end
  end

  def tokenizer(word) do
    {:ok,exp}=Regex.compile("^[a-z0-9][a-z0-9-]*$")
    Regex.match?(exp,word)
  end

  def read_dict(ppid) do
    File.stream!(Path.absname("../share/morph_english.flat"),[:read],:line)
    |> Enum.reduce( HashDict.new,
      fn line,dict ->
        terms= String.split(String.rstrip(line,?\n),["\t"," "])
        from= Enum.at(terms,0)
        trans= Enum.at(terms,3)
        Dict.put(dict,from,trans)
      end
    )
    |> return_trigger(ppid)
    |> loop
  end

  def return_trigger(dict,ppid) do
    send ppid, {:finish, self()}
    dict
  end

  def loop(dict) do
    receive do
      {:get, from, pid} ->
        send pid, {:return, Dict.get(dict,from,from)}
        loop(dict)
    end
  end

end

Main.start System.argv()
