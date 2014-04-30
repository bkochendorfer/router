defmodule CacheTest do
  use ExUnit.Case
  alias Cache.Client

  setup do
    Client.start
    :ok
  end

  test "add bad backends to the cache" do
    Client.set("127.0.1.1")
    Client.set("127.0.1.2")

    assert Client.get_all == ["127.0.1.2", "127.0.1.1"]
  end

  test "check for bad backends" do
    assert Client.bad? "127.0.1.1" == {:found}
  end

  test "returns not found if backend is not found" do
    assert Client.bad? "127.2.1.1" == {:not_found}
  end

  test "deletes from cache" do
    assert Client.delete "127.0.1.1" == {:ok ,"127.0.1.1"}
  end

  test "does not delete things that not exist" do
    assert Client.delete "127.2.1.1" == {:not_found}
  end

  test "Clears the cache" do
    :timer.sleep(1500)
    assert Client.get_all == []
  end
end
