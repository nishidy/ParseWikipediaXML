defmodule Main do

  def parse(argv) do
    minc = 2
    path = hd argv
    File.stream!(Path.absname(path),[:read],:line)
    |> Enum.map(
      fn(x) ->
        String.split(x)
        |> Enum.map( &(String.rstrip &1,?\n))
        |> Enum.filter( &(tokenizer &1))
        |> Enum.reduce( HashDict.new,
          fn x, bofw ->
            if Dict.has_key?(bofw,x),
            do: Dict.update!(bofw,x,&(&1+1)),
            else: Dict.put(bofw,x,1)
          end
        )
        |> Enum.filter( fn {_,v} -> v>minc end)
        |> Enum.map( fn {k,v} -> k <> " " <> Integer.to_string v end)
        |> Enum.join( " ")
        |> output
      end
    )
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

Main.parse System.argv()
