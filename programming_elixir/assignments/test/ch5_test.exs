defmodule Ch5Test do
  use ExUnit.Case
  doctest App
  import Ch5

  test "list_concat" do
    assert list_concat().([:a, :b], [:c, :d]) == [:a, :b, :c, :d]
  end

  test "sum" do
    assert sum().(1, 2, 3) == 6
  end

  test "pair_tuple_to_list" do
    assert pair_tuple_to_list().( { 1234, 5678 } ) == [ 1234, 5678 ]
  end

  test "fizzbuz" do
    assert fizzbuzz().(0,0,1) == "FizzBuzz"
    assert fizzbuzz().(0,1,1) == "Fizz"
    assert fizzbuzz().(1,0,1) == "Buzz"
    assert fizzbuzz().(1,1,"End") == "End"
  end

  test "rembiz" do
    assert rembiz().(10) == "Buzz"
    assert rembiz().(11) == 11
    assert rembiz().(12) == "Fizz"
    assert rembiz().(13) == 13
    assert rembiz().(14) == 14
    assert rembiz().(15) == "FizzBuzz"
    assert rembiz().(16) == 16
  end

  test "prefix" do
    mrs = prefix().("Mrs")
    assert mrs.("Smith") == "Mrs Smith"
    assert prefix().("Elixir").("Rocks") == "Elixir Rocks"
  end

  test "rewrite & notation" do
    assert Enum.map [1,2,3,4], &(&1 + 2) == [3,4,5,6]
    assert Enum.each [1,2,3,4], &IO.inspect/1
  end
end
