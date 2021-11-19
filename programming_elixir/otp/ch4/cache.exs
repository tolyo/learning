# Write a GenServer that can store any valid Elixir term, given a key. Here are a few
# operations to get you started:
# - Cache.write(:stooges, ["Larry", "Curly", "Moe"])
# - Cache.read(:stooges)
# - Cache.delete(:stooges)
# - Cache.clear
# - Cache.exist?(:stooges)
# Structure your program similar to how you did in this chapter. In particular, pay atten-
# tion to which of these operations should be handle_call s or handle_cast s.

defmodule Cache do
  use GenServer

  @name :cache

  ## Client API

  def start_link(_opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, [name: @name])
  end

  def write(term, value) do
    GenServer.cast(@name, {:write, term, value})
  end

  def read(term) do
    GenServer.call(@name, {:read, term})
  end

  def delete(term) do
    GenServer.cast(@name, {:delete, term})
  end

  def clear() do
    GenServer.cast(@name, {:clear})
  end

  def exist?(term) do
    GenServer.call(@name, {:exist, term})
  end

  # Server Callbacks
  # handle_calls go here

  def init(:ok) do
    {:ok, Map.new()}
  end

  def handle_cast({:write, term, value}, state) do
    new_state = case Map.has_key?(state, term) do
      true -> Map.update!(state, term, fn _x -> value end)
      false -> Map.put(state, term, value)
    end
    {:noreply, new_state}
  end

  def handle_cast({:delete, term}, state) do
    new_state = case Map.has_key?(state, term) do
      true -> Map.delete(state, term)
      false -> state
    end
    {:noreply, new_state}
  end

  def handle_call({:read, term}, _from, state) do
    {:reply, Map.get(state, term), state}
  end

  def handle_call({:exist, term} , _from, state) do
    {:reply, Map.has_key?(state, term), state}
  end
end
