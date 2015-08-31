defmodule Main do

  def start(argv) do
    path = hd argv

    File.stream!(Path.absname(path),[:read],:line)
    |> Enum.reduce( "",
      fn x, text ->
        if String.contains?(x,"</page>"),
        do: parse(String.rstrip(text,?\n) <> x),
      else: String.rstrip(text,?\n) <> x
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
    |> Enum.reduce( HashDict.new,
      fn x, bofw ->
        if Dict.has_key?(bofw,x),
        do: Dict.update!(bofw,x,&(&1+1)),
      else: Dict.put(bofw,x,1)
      end
    )
    |> Enum.filter( fn {_,v} -> v>=minc end)
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

end

Main.start System.argv()
