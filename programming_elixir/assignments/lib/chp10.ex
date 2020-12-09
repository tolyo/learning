defmodule Ch10 do

  @doc """
  Implement the following Enum functions using no library functions or list
  comprehensions: all? , each , filter , split , and take . You may need to use an if
  statement to implement filter . The syntax for this is
  if condition do
  expression(s)
  else
  expression(s)
  end
  """

  def all?([], _), do: true
  def all?([head|tail], func) do
    if (func.(head)) do
      all?(tail, func)
    else
      false
    end
  end

  def each([], _), do: []
  def each([head|tail], func), do: [func.(head) | each(tail, func)]

  def filter([], _), do: []
  def filter([head|tail], func) do
     if func.(head) do
       [head| filter(tail, func)]
     else
       filter(tail, func)
     end
  end

  def split(list, sep) do
    _split(list, sep, [])
  end

  def _split([head| tail], sep, acc) do
    if head === sep do
      {acc ++ [head], tail}
    else
      _split(tail, sep, acc ++ [head])
    end
  end

  def take(list, sep) do
    _take(list, sep, 0)
  end

  def _take([], sep, count), do: []
  def _take([head|tail], sep, count) do
    if sep === count do
      []
    else
      [head| _take(tail, sep, count + 1) ]
    end
  end

  def flatten([]), do: []
  def flatten([head|tail]), do: flatten(head) ++ flatten(tail)
  def flatten(head), do: [head]

  @doc """
    In the last exercise of Chapter 7, Lists and Recursion, on page 71, you
    wrote a span function. Use it and list comprehensions to return a list of
    the prime numbers from 2 to n.
  """
  def primes(n) do
    for x <- Ch7.MyList.span(2, n), Enum.all?(2..x-1, &(rem(x, &1) > 0)), do: x
  end

  @doc """
    The Pragmatic Bookshelf has offices in Texas (TX) and North Carolina
    (NC), so we have to charge sales tax on orders shipped to these states.
    The rates can be expressed as a keyword list (I wish it were that simple....):
  """
  def get_orders() do
    tax_rates = [ NC: 0.075, TX: 0.08 ]
    orders = [
      [id: 123, ship_to: :NC, net_amount: 100.00],
      [id: 124, ship_to: :OK, net_amount: 35.50],
      [id: 125, ship_to: :TX, net_amount: 24.00],
      [id: 126, ship_to: :TX, net_amount: 44.00],
      [id: 127, ship_to: :NC, net_amount: 25.00],
      [id: 128, ship_to: :MA, net_amount: 10.00],
      [id: 129, ship_to: :CA, net_amount: 102.00],
      [id: 120, ship_to: :NC, net_amount: 50.00]
    ]
    Enum.map(orders, &(Keyword.put(&1, :total_amount, &1[:net_amount] + &1[:net_amount] * (tax_rates[&1[:ship_to]] || 0))))
  end
end
