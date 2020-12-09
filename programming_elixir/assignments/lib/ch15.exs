defmodule Ch15 do

  @doc """
    Write a program that spawns two processes and then passes each a unique
    token (for example, “fred” and “betty”). Have them send the tokens back.
    – Is the order in which the replies are received deterministic in theory?
    In practice?
    1
    – If either answer is no, how could you make it so?
  """
  def greet() do
    receive do
      {sender, msg} ->
        IO.puts("#{inspect sender}")
        send(sender, {:ok, msg})
    end
  end

  def run do
    first = spawn_link(Ch15, :greet, [])
    second = spawn_link(Ch15, :greet, [])
    send(first, {self(), "fred"})
    receive do
      {:ok, message} ->
        IO.puts message
    end
    send(second, {self(), "betty"})
    receive do
      {:ok, message} ->
        IO.puts message
    end
  end


end

Ch15.run()
