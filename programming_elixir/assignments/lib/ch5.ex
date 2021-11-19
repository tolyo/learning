defmodule Ch5 do


  @doc """
    list_concat.([:a, :b], [:c, :d]) #=> [:a, :b, :c, :d]
  """
  def list_concat() do
    fn (a, b) -> a ++ b end
  end

  @spec sum :: (number, number, number -> number)
  @doc """
    sum.(1, 2, 3) #=> 6
  """
  def sum() do
    fn (a, b, c) -> a + b + c end
  end

  @doc """
    pair_tuple_to_list.( { 1234, 5678 } ) #=> [ 1234, 5678 ]
  """
  def pair_tuple_to_list() do
    fn {a, b} -> [a, b] end
  end

  @doc """
    Write a function that takes three arguments. If the first two are zero,
    return “FizzBuzz.” If the first is zero, return “Fizz.” If the second is zero,
    return “Buzz.” Otherwise return the third argument. Do not use any lan-
    guage features that we haven’t yet covered in this book.
  """
  def fizzbuzz() do
    fn
      (0, 0, _) -> "FizzBuzz"
      (0, _, _) -> "Fizz"
      (_, 0, _) -> "Buzz"
      (_, _, val) -> val
    end
  end

  @doc """
    The operator rem(a, b) returns the remainder after dividing a by b . Write a
    function that takes a single integer ( n ) and calls the function in the previ-
    ous exercise, passing it rem(n,3) , rem(n,5) , and n . Call it seven times with
    the arguments 10, 11, 12, and so on. You should get “Buzz, 11, Fizz, 13,
    14, FizzBuzz, 16.”
  """
  def rembiz() do
    fn
      n -> fizzbuzz().(rem(n,3), rem(n, 5), n)
    end
  end

  @doc """
    Write a function prefix that takes a string. It should return a new function
    that takes a second string. When that second function is called, it will
    return a string containing the first string, a space, and the second string.
  """
  def prefix do
    fn prefix ->
      fn name -> "#{prefix} #{name}" end
    end
  end

  @doc """
    Use the & notation to rewrite the following:
    – Enum.map [1,2,3,4], fn x -> x + 2 end
    – Enum.each [1,2,3,4], fn x -> IO.inspect x end
  """
  def rewrite() do

  end



end
