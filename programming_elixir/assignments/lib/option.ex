defmodule Option do
  @moduledoc """
    sealed trait Option[+A]
    case class Some[+A](get: A) extends Option[A]
    case object None extends Option[Nothing]
  """
  @type t :: Some.t() | None.t()

  defmodule Some do
    @type t :: %Some{
      get: any()
    }

    defstruct get: nil

    def new(val), do: %Some{get: val}
  end

  def module None do
    @type t :: nil
  end

  @doc """
  trait Option[+A] {
    def map[B](f: A => B): Option[B]
    def flatMap[B](f: A => Option[B]): Option[B]
    def getOrElse[B >: A](default: => B): B
    def orElse[B >: A](ob: => Option[B]): Option[B]
    def filter(f: A => Boolean): Option[A]
  }
  Implement all of the preceding functions on Option . As you implement each function,
  try to think about what it means and in what situations you’d use it. We’ll explore when
  to use each of these functions next. Here are a few hints for solving this exercise:
   It’s fine to use pattern matching, though you should be able to implement all
  the functions besides map and getOrElse without resorting to pattern matching.
   For map and flatMap , the type signature should be enough to determine the
  implementation.
   getOrElse returns the result inside the Some case of the Option , or if the Option
  is None , returns the given default value.
   orElse returns the first Option if it’s defined; otherwise, it returns the second
  Option .
  """
  @spec map(any) :: (any -> None | Option.Some.t())
  def map(f), do: fn (a) -> case f.(a) do
      nil -> None
      val -> %Some{get: val}
    end
  end

  @spec flatmap(any) :: (any -> None | Option.Some.t())
  def flatmap(f), do: fn (a) -> case f.(a) do
      %Some{get: val} ->  %Some{get: val}
      _ -> None
    end
  end

  @spec get_or_else(any) :: (any -> any)
  def get_or_else(any), do: fn (a) ->
    case a do
      %Some{get: val} -> val
      _ -> any
    end
  end

  @spec or_else(Option.Some.t()) :: (any -> any)
  def or_else(%Some{} = b), do: fn (a) ->
    case a do
      %Some{get: val} -> val
      _ -> b.get
    end
  end

  @spec filter(any) :: (any -> None | Option.Some.t())
  def filter(f), do: fn (a) ->
    case f.(a) do
      true -> %Some{get: a}
      _ -> None
    end
  end



end
