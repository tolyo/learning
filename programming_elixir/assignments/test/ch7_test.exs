defmodule Ch7Test do
  use ExUnit.Case
  doctest App

  test "mapsum" do
    assert Ch7.MyList.mapsum([1, 2, 3], &(&1 * &1)) == 14
  end

  test "max" do
    assert Ch7.MyList.max([1, 2, 3]) == 3
  end

  test "caesar" do
    assert Ch7.MyList.caesar('ryvkve', 13) == 'elixir'
  end

  test "span" do
    assert Ch7.MyList.span(1, 4) == [1,2,3,4]
  end

end
