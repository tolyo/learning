defmodule Ch11 do
  @doc """
  Write a function that returns true if a single-quoted string contains only
  printable ASCII characters (space through tilde).
  """
  def is_printable([h|t]) when h > 31 and h < 127, do: is_printable(t)
  def is_printable([h|t]) when h < 32 or h > 126, do: false
  def is_printable([]), do: true

  @doc """
  Write an anagram?(word1, word2) that returns true if its parameters are
  anagrams.
  """
  def anagram?(word1, word2), do: Enum.sort(word1) == Enum.sort(word2)

  @doc """
  Hard) Write a function that takes a single-quoted string of the form
  number [+-*/] number and returns the result of the calculation. The indi-
  vidual numbers do not have leading plus or minus signs.
  """
  def calculate(list), do: _helper(list, 0)

  def _helper([], acc), do: acc
  def _helper([h|t], acc) when h in '0123456789', do: _helper(t, acc * 10 + h - 48)
  def _helper([?*|t], acc), do: acc * _helper(t,  0)
  def _helper([?/|t], acc), do: acc / _helper(t,  0)
  def _helper([?+|t], acc), do: acc + _helper(t,  0)
  def _helper([?-|t], acc), do: acc - _helper(t,  0)

  @doc """
    Write a function that takes a list of double-quoted strings and prints each
    on a separate line, centered in a column that has the width of the longest
    string. Make sure it works with UTF characters.
  """

  def center([h|t]=list) do
    longest_string = _get_longest_string(t, String.length(h), 0)

    Enum.each(list, fn x ->
      pad = floor((longest_string-String.length(x))/2 + String.length(x))
      IO.puts((x |> String.pad_leading(pad) |> String.pad_trailing(pad)))
    end)
  end

  defp _get_longest_string([], current, acc) when current > acc, do: current
  defp _get_longest_string([], current, acc) when current < acc, do: acc
  defp _get_longest_string([h|t], current, acc) when current > acc, do: _get_longest_string(t, String.length(h), current)
  defp _get_longest_string([h|t], current, acc) when current < acc, do: _get_longest_string(t, String.length(h), acc)

  @doc """
    Write a function to capitalize the sentences in a string. Each sentence is
    terminated by a period and a space. Right now, the case of the characters
    in the string is random.
  """
  def capitalize_sentences(sentences) do
    sentences
    |> String.split(". ")
    |> Enum.map(&(String.downcase(&1) |> String.capitalize()))
    |> Enum.join(". ")
  end

  @doc """
  Chapter 7 had an exercise about calculating sales tax on page 114. We
  now have the sales information in a file of comma-separated id , ship_to ,
  and amount values. The file looks like this:
  Write a function that reads and parses this file and then passes the result
  to the sales_tax function. Remember that the data should be formatted into
  a keyword list, and that the fields need to be the correct types (so the id
  field is an integer, and so on).
  You’ll need the library functions File.open , IO.read(file, :line) , and IO.stream(file) .
  """
  def get_orders() do
    tax_rates = [ NC: 0.075, TX: 0.08 ]

    [h|t] = File.open!("./lib/tax.txt")
      |> IO.stream(:line)
      |> Enum.into([])

    orders =
      t
      |> Enum.map(&(String.replace(&1, "\n", "")))
      |> Enum.map(&(String.replace(&1, ":", "")))
      |> Enum.map(&(String.split(&1, ",")))
      |> Enum.map(fn [a,b,c] ->
        IO.puts([a,b,c])
        {x, _} = Float.parse(c)
        {y, _} = Integer.parse(a)
        [id: y, ship_to: String.to_atom(b), net_amount: x]
      end)
    Enum.map(orders, &(Keyword.put(&1, :total_amount, &1[:net_amount] + &1[:net_amount] * (tax_rates[&1[:ship_to]] || 0))))
  end
end
