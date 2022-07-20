# iex --dot-iex iexnode.exs

erlnode = :"erl@127.0.0.1"
if Node.alive?() do
  ^erlnode = Node.self()
else
  {:ok, _pid} = :net_kernel.start([erlnode, :longnames])
  Node.set_cookie(:COOKIE)
end

Process.register(self(), :shell)

unless :code.is_loaded(PyTest) do
  defmodule PyTest do
    @pynode :"py@127.0.0.1"
    @my_process {:my_process, @pynode}

    IO.write("""
      ***
      *** Play with #{@pynode} node using #{inspect __MODULE__} module.
      ***
      """)

    def net_adm_ping do
      :net_adm.ping(@pynode)
    end

    def send_my_process(term \\ :hello) do
      send(@my_process, term)
    end

    def call_my_process(term \\ "hello") do
      GenServer.call(@my_process, term, 3000)
    end
  end
end


