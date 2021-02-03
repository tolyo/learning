defmodule FP do

  @moduledoc """
   Functional programming in Scala to Elixir
  """

  @doc """

  ## Examples

      iex> FP.fib(3)
      6
      iex> FP.fib(4)
      10

  """
  @spec fib(number) :: number
  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n), do: n + fib(n-1)

  @doc """
    Implement isSorted , which checks whether an Array[A] is sorted according to a
    given comparison function:

    def is_sorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean

    ## Examples

      iex> FP.is_sorted([1], fn (a, b) -> a < b end)
      true
      iex> FP.is_sorted([1,3,4], fn (a, b) -> a < b end)
      true
      iex> FP.is_sorted([1,3,4], fn (a, b) -> a > b end)
      false
      iex> FP.is_sorted([1,2,3,4,6,3,4], fn (a, b) -> a < b end)
      false
      iex> FP.is_sorted([1,6,3,4], fn (a, b) -> a > b end)
      false
      iex> FP.is_sorted(["a","c","d"], fn (a, b) -> a < b end)
      true
  """
  @spec is_sorted([a], ((b, b) -> boolean())) :: boolean() when a: any, b: a
  def is_sorted([], _), do: true
  def is_sorted([_head], _), do: true
  def is_sorted([head | [sub_head | _] = tail], ordered) do
    case ordered.(head, sub_head) do
      true ->
        is_sorted(tail, ordered)
      false ->
        false
    end
  end

  @doc """
    Let’s look at another example, currying, 9 which converts a function f of two arguments
    into a function of one argument that partially applies f . Here again there’s only one
    implementation that compiles. Write this implementation.

    def curry[A,B,C](f: (A, B) => C): A => (B => C)

    ## Examples

      iex> res = FP.curry(fn(x, y) -> x + y end)
      iex> res.(4).(5)
      9

  """
  @spec curry(((a, b) -> c)) :: (a1 -> (b1 -> c1)) when a: any, b: any, c: any, a1: a, b1: b, c1: c
  def curry(f) do
    fn a -> (fn b -> f.(a, b) end) end
  end

  @doc """
  Implement uncurry , which reverses the transformation of curry . Note that since =>
  associates to the right, A => (B => C) can be written as A => B => C .
  def uncurry[A,B,C](f: A => B => C): (A, B) => C

  ## Examples
    iex> res = FP.curry(fn(x, y) -> x + y end)
    iex> res.(4).(5)
    9
    iex> unres = FP.uncurry(res)
    iex> unres.(5,6)
    11

  """
  @spec uncurry((a -> (b -> c))) :: ((a1, b1) -> c1) when a: any, b: any, c: any, a1: a, b1: b, c1: c
  def uncurry(f) do
    fn (a, b) -> f.(a).(b) end
  end

  @doc """
    Implement the higher-order function that composes two functions.
    def compose[A,B,C](f: B => C, g: A => B): A => C
    ## Examples
        iex> res = FP.compose(fn(x) -> x + x end, fn(y) -> y * y end)
        iex> res.(2)
        8
  """
  @spec compose((b -> c), (a -> b1)) :: ((a1) -> c1) when a: any, a1: a, b: any, b1: b, c: any, c1: c
  def compose(f, b) do
    fn x -> f.(b.(x)) end
  end

  @doc """
    Implement the function tail for removing the first element of a List . Note that the
    function takes constant time. What are different choices you could make in your
    implementation if the List is Nil ? We’ll return to this question in the next chapter.

    ## Examples
    iex> FP.tail([1,2,3,4])
    [2,3,4]

  """
  @spec tail([any()]) :: [any()]
  def tail([_| tail]), do: tail

  @doc """
    Using the same idea, implement the function setHead for replacing the first element
    of a List with a different value.
    ## Examples
    iex> FP.set_head(5,[1,2,3,4])
    [5,2,3,4]
  """

  @spec set_head(any(), [any()]) :: [any()]
  def set_head(new_head, [_| tail]), do: [new_head | tail ]

  @doc """
    Generalize tail to the function drop , which removes the first n elements from a list.
    Note that this function takes time proportional only to the number of elements being
    dropped—we don’t need to make a copy of the entire List .
    def drop[A](l: List[A], n: Int): List[A]

    ## Examples
    iex> FP.drop([1,2,3,4], 2)
    [3,4]
    iex> FP.drop([1,2,3,4], 6)
    []
  """

  @spec drop([any()], number()) :: [any()]
  def drop([], _), do: []
  def drop(list, 0), do: list
  def drop([_ | tail], n), do: drop(tail, n-1)

  @doc """
    Implement dropWhile , which removes elements from the List prefix as long as they
    match a predicate.
    def dropWhile[A](l: List[A], f: A => Boolean): List[A]
    iex> FP.drop_while([1,2,3,4], fn x -> x > 2 end)
    [3,4]
    iex> FP.drop_while([1,2,3,4], fn x -> x > 0 end)
    [1,2,3,4]
  """
  @spec drop_while([a], ((b) -> boolean())) :: [a] when a: any, b: a
  def drop_while([], _), do: []
  def drop_while([head | tail], predicate) do
    case predicate.(head) do
      true ->
        [head | drop_while(tail, predicate)]
      false ->
        drop_while(tail, predicate)
    end
  end

  @doc """
    Not everything works out so nicely. Implement a function, init , that returns a List
    consisting of all but the last element of a List . So, given List(1,2,3,4) , init will
    return List(1,2,3) . Why can’t this function be implemented in constant time like
    tail ?
    def init[A](l: List[A]): List[A]
    iex> FP.init([1,2,3,4])
    [1,2,3]
  """
  @spec init([any()]):: [any()]
  def init(list), do: init_helper(list, [])
  defp init_helper([_head], new_list), do: Enum.reverse(new_list)
  defp init_helper([head | tail], new_list), do: init_helper(tail, [head | new_list])

  @doc """
    Implement foldRight
      def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
        as match {
          case Nil => z
          case Cons(x, xs) => f(x, foldRight(xs, z)(f))
        }
  """
  @spec fold_right([any()], any()) :: ((any(), any()) -> any()) | any()
  def fold_right(list, acc) do
     fn(f) ->
       case list do
         [] -> acc
         [head | tail] -> f.(head, fold_right(tail, acc).(f))
       end
      end
  end

  @doc """
    ## Examples
    iex> FP.sum([1,2,3])
    6
  """
  def sum(ns), do: FP.fold_right(ns, 0).(fn (x,y) -> x + y end)

  @doc """
    Can product , implemented using foldRight , immediately halt the recursion and
    return 0.0 if it encounters a 0.0 ? Why or why not? Consider how any short-circuiting
    might work if you call foldRight with a large list. This is a deeper question that we’ll
    return to in chapter 5

    No, because the higher order function has no way knowing the predicate's domain contraints.

    Compute the length of a list using foldRight .
    def length[A](as: List[A]): Int

    iex> FP.length([1,3,4,5])
    4
  """
  @spec length([any()]) :: number()
  def length(as), do: FP.fold_right(as, 0).(fn (_x, y) -> y + 1 end)

  @doc """
    Our implementation of foldRight is not tail-recursive and will result in a StackOver-
    flowError for large lists (we say it’s not stack-safe). Convince yourself that this is the
    case, and then write another general list-recursion function, foldLeft , that is
    tail-recursive, using the techniques we discussed in the previous chapter. Here is its
    signature:
    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B

    iex> FP.fold_left([1,2,3], 0).(fn (x,y) -> x + y end)
    6
  """

  @spec fold_left([any()], any()) :: ((any(), any()) -> any()) | any()
  def fold_left(list, acc) do
     fn(f) ->
       case list do
         [] -> acc
         [head | tail] -> fold_left(tail, f.(acc, head)).(f)
       end
      end
  end


  @doc """
    Write sum , product , and a function to compute the length of a list using foldLeft .
    iex> FP.suml([1,2,3])
    6
    iex> FP.productl([1,2,3])
    6
    iex> FP.lengthl([1,2,3])
    3
  """
  @spec suml([any()]) :: number()
  def suml(list), do: FP.fold_left(list, 0).(fn (acc, val) -> acc + val end)

  @spec productl([any()]) :: number()
  def productl(list), do: FP.fold_left(list, 1).(fn (acc, val) -> acc * val end)

  @spec lengthl([any()]) :: number()
  def lengthl(list), do: FP.fold_left(list, 0).(fn (acc, _val) -> acc + 1 end)

end
