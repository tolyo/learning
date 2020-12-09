# defmodule Ch12 do

#   @doc """
#   Rewrite the FizzBuzz example using case .
#   """
#   def upto(n) when n > 0, do: 1..n |> Enum.map(&(fizzbuzz(&1, rem(&1, 3), rem(&1, 5)))
#   def fizzbuzz(val) do
#     case val do
#       (0, 0, _) -> "FizzBuzz"
#       (0, _, _) -> "Fizz"
#       (_, 0, _) -> "Buzz"
#       (_, _, data) -> data
#     end
#   end

#   @doc """
#   Many built-in functions have two forms. The xxx form returns the tuple
#   {:ok, data} and the xxx! form returns data on success but raises an exception
#   otherwise. However, some functions don’t have the xxx! form.
#   Write an ok! function that takes an arbitrary parameter. If the parameter
#   is the tuple {:ok, data} , return the data. Otherwise, raise an exception
#   containing information from the parameter.
#   You could use your function like this:
#   file = ok! File.open("somefile")
#   """
#   def ok!(val) do
#     case val do
#       {:ok, data} -> data
#       _ -> raise Error, message: val
#     end
#   end

# end
