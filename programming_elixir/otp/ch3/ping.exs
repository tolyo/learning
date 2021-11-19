# Write a program that spawns two processes. The first process, on receiving a
# ping message, should reply to the sender with a pong message. The second pro-
# cess, on receiving a pong message, should reply with a ping message.

defmodule Ping do
  def loop() do
    receive do
      {sender_pid, :ping} ->
        IO.puts("ping from #{inspect sender_pid}")
        send(sender_pid, {self(), :pong})
        loop()
      _ ->
        raise "error"
    end
  end
end

defmodule Pong do
  def loop() do
    receive do
      {pid, :pong} ->
        IO.puts("pong")
        send(pid, {self(), :ping})
        loop()
      {:receiver_pid, pid} ->
        send(pid, {self(), :ping})
        loop()
      _ ->
        raise "error"
    end
  end
end


pid = spawn(Ping, :loop, [])
pid2 = spawn(Pong, :loop, [])
send(pid2, {:receiver_pid, pid})
