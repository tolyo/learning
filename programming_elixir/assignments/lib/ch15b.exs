defmodule Ch15b do
  @doc """
  Use spawn_link to start a process, and have that process send a message
  to the parent and then exit immediately. Meanwhile, sleep for 500 ms in
  the parent, then receive as many messages as are waiting. Trace what
  you receive. Does it matter that you weren’t waiting for the notification
  from the child when it exited?
  Answer: NO IT DOES NOT. Changing to spawn_monitor prevents from crashing
  """
  def sad_function() do
    receive do
      {sender, msg} ->
        send(sender, msg)
        raise "error"
    end
  end

  def run() do
    {res, _} = spawn_monitor(Ch15b, :sad_function, [])
    IO.puts(inspect res)
    send(res, {self(), "hi"})
    :timer.sleep(500)
    receive do
      msg ->
        IO.puts(inspect msg)
    after 1000 ->
      IO.puts "Nothing happened as far as I am concerned"
    end
  end
end

Ch15b.run()
