defmodule Cache.Server do
  use GenServer.Behaviour

  @interval 15000

  def start_link(initial_cache) do
    :gen_server.start_link {:global, :cache_server}, __MODULE__, initial_cache, []
  end

  def init(initial_cache) do
    :erlang.send_after(@interval, Kernel.self, {:clear_cache, []})
    {:ok, initial_cache}
  end

  def handle_cast({:set, backend}, cache) do
    {:noreply, [backend|cache]}
  end

  def handle_call({:bad?, backend}, _from, cache) do
    case Enum.member?(cache, backend) do
      true ->
        {:reply, :found, cache}
      false ->
        {:reply, :not_found, cache}
    end
  end

  def handle_call({:delete, backend}, _from, cache) do
    case Enum.member?(cache, backend) do
      true ->
        {:reply, {:ok, backend}, List.delete(cache, backend)}
      false ->
        {:reply, :not_found, cache}
    end
  end

  def handle_call(:get_all, _from, cache) do
    {:reply, cache, cache}
  end

  def handle_info({:clear_cache, new_cache}, cache) do
    :erlang.send_after(@interval, Kernel.self, {:clear_cache, []})
    {:noreply, new_cache}
  end

end

defmodule Cache.Client do

  def start(initial_cache \\ []) do
    Cache.Server.start_link(initial_cache)
  end

  def bad?(backend) do
    :gen_server.call({:global, :cache_server}, {:bad?, backend})
  end

  def set(backend) do
    :gen_server.cast({:global, :cache_server}, {:set, backend})
  end

  def delete(backend) do
    :gen_server.call({:global, :cache_server}, {:delete, backend})
  end

  def get_all do
    :gen_server.call({:global, :cache_server}, :get_all)
  end
end
