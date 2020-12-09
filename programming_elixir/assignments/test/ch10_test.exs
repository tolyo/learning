defmodule Ch10Test do
  use ExUnit.Case
  doctest App
  import Ch10

  test "all?" do
    assert all?([1, 2, 3], &(&1 > 0)) == true
    assert all?([1, 2, 3], &(&1 < 3)) == false
  end

  test "each" do
    assert each([1, 2, 3], &(&1 > 0)) == [true, true, true]
    assert each([1, 2, 3], &(&1 < 3)) == [true, true, false]
  end

  test "filter" do
    assert filter([1, 2, 3], &(&1 > 0)) == [1,2,3]
    assert filter([1, 2, 3], &(&1 < 3)) == [1,2]
  end

  test "split" do
    assert split([1,2,3,4,5], 3) == {[1,2,3],[4,5]}
  end

  test "take" do
    assert take([1,2,3,4], 2) == [1,2]
  end

  test "flatten" do
    assert flatten([ 1, [ 2, 3, [4] ], 5, [[[6]]]]) === [1,2,3,4,5,6]
  end

  test "primes" do
    assert primes(20) == [3,5,7,11,13,17,19]
  end

  test "get_orders" do
    assert get_orders() |> List.first() === [total_amount: 107.5, id: 123, ship_to: :NC, net_amount: 100.00]
  end
end
