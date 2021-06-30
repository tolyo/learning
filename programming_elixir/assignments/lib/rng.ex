defmodule RNG do
  use Bitwise

  @type t :: %RNG{
    seed: number()
  }

  defstruct seed: nil


  @spec next_int(number()) :: {number(), RNG.t()}
  def next_int(seed) do
    new_seed = (seed * 0x5DEECE66D + 0xB) &&& 0xFFFFFFFFFFFF
    n = new_seed >>> 16
    {n, %RNG{seed: new_seed}}
  end

  @doc """
    Write a function that uses RNG.nextInt to generate a random integer between 0 and
    Int.maxValue (inclusive). Make sure to handle the corner case when nextInt returns
    Int.MinValue , which doesn’t have a non-negative counterpart.
    def nonNegativeInt(rng: RNG): (Int, RNG)
  """
  @spec next_max(number(), number()) :: {number(), RNG.t()}
  def next_max(seed, max) do
    {n, %RNG{seed: new_seed}} = next_int(seed)
    {Integer.mod(n, max), %RNG{seed: new_seed}}
  end
end
