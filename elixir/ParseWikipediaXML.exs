defmodule Main do

  def start(argv) do
    path = hd argv

    pid = spawn fn -> read_dict end
    Process.register(pid,:dict)

    File.stream!(Path.absname(path),[:read],:line)
    |> Enum.reduce( "",
      fn x,t ->
        text = String.rstrip(t,?\n)
        if String.contains?(x,"</page>"),
        do: parse(text <> x),
      else: text <> x
      end
    )
  end

  def parse(text) do
    minc = 2

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

    ""
  end

  def output(text) do
    if String.length(text) > 0,
    do: IO.puts text
  end

  def tokenizer(word) do
    {:ok,exp}=Regex.compile("^[a-z0-9][a-z0-9-]*$")
    Regex.match?(exp,word)
  end

  def read_dict do
    File.stream!(Path.absname("../share/morph_english.flat"),[:read],:line)
    |> Enum.reduce( HashDict.new,
      fn line,dict ->
        terms= String.split(String.rstrip(line,?\n),["\t"," "])
        from= Enum.at(terms,0)
        trans= Enum.at(terms,3)
        Dict.put(dict,from,trans)
      end
    )
    |> loop
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
