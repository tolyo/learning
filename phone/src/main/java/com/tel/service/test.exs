
IO.puts("start")

send({:server, :server@anatoly}, {self(), :getphone, "+37129901427"})

IO.puts("sent")
send(self(), "hellow")
send({:server, :server@anatoly}, {self(), :getphone, "+37129901427"})

receive do
  a ->
    IO.puts(a)
end
