defmodule Ch6Test do
  use ExUnit.Case
  doctest App
  import Ch6

  test "Times" do
    assert Times.double(4) == 8
    assert Times.triple(2) == 6
    assert Times.quadruple(4) == 16
  end

  test "sum/1" do
    assert sum(0) == 0
    assert sum(1) == 1
    assert sum(2) == 3
    assert sum(3) == 6
  end

  test "gcd/2" do
    assert gcd(2,1) == 1
    assert gcd(6,3) == 3
    assert gcd(12, 8) == 4
  end

  test "guess/2" do
    assert Chop.guess(231, 1..1000) == 231
  end

end
