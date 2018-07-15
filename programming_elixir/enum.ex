defmodule MyEnum do

  def all?([], _) do
    true
  end

  def all?([head|tail], func) do
    if (func.(head)) do
      all?(tail, func)
    else
      false
    end
  end

  def each([head|tail], func) do
    [func.(head)|each(tail, func) ]
  end

  def each([head], func) do
    [func.(head) ]
  end

  def each([], _) do
    []
  end

  def filter([head|tail], func) do
     if func.(head) do
       [head| filter(tail, func)]
     else
       filter(tail, func)
     end
  end

  def filter([head], func) do
    if func.(head) do
      [head]
    else
      []
    end
  end

  def filter([], _) do
    []
  end

  def split(list, sep) do
    _split(list, sep, [])
  end

  def _split([head| tail], sep, acc) do
    if head === sep do
      {[ acc|[head]], tail}
    else
      _split(tail, sep, acc ++ head)
    end
  end

  def take(list, sep) do
    _take(list, sep, 0)
  end

  def _take([head|tail], sep, count) do
    if sep === count do
      []
    else
      [head| _take(tail, sep, count + 1) ]
    end
  end

  def _take([], sep, count), do: []

  def flatten([]), do: []
  def flatten([head|tail]), do: flatten(head) ++ flatten(tail)
  def flatten(head), do: [head]


  # ListsAndRecursion-7
  def span(from, from), do: [from]
  def span(from, to), do: [from|span(from + 1, to)]
  def is_prime(x) do
    all?(Enum.into(2..x-1, []), fn val -> rem(x, val) > 0 end)
  end
  def primes(n) do
    for x <- span(2, n), is_prime(x), do: x
  end

  # ListsAndRecursion-8
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
    
    Enum.map(orders, fn x ->
      rate = tax_rates[x[:ship_to]] || 0
      Keyword.put(x, :total_amount, x[:net_amount] * (1 + rate))
    end)
  end

end