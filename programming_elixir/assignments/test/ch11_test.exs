defmodule Ch11Test do
  use ExUnit.Case
  doctest App
  import Ch11

  test "is_printable" do
    assert is_printable('fdsa') == true
    assert is_printable('\t') == false
  end

  test "anagram?" do
    assert anagram?('silent', 'listen') == true
    assert anagram?('fail', 'true') == false
  end

  test "calculate" do
    assert calculate('2+2') == 4
    assert calculate('11+32') == 43
  end

  test "center" do
    # see console out
    assert center(["cat", "zebra", "elephant"]) == :ok
  end

  test "capitalize" do
    assert capitalize_sentences("oh. a DOG. woof. ") == "Oh. A dog. Woof. "
  end

  test "get_orders" do
    assert get_orders() |> List.first() === [total_amount: 107.5, id: 123, ship_to: :NC, net_amount: 100.00]
  end
end
