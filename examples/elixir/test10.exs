#
# A simple Python server and an Elixir client sending to it
# Python server will reply with its own Pid, so then you know the Pid and can
# send to it directly (second send call).
#
# See the documentation: Examples page, section "Example 10"
#

IO.puts "Test10 trying to connect to py@127.0.0.1 and send some hellos"

# First we don't know destination Pid, but it registers self as 'my_process'
# It will return its own pid as val1
my_process = {:"my_process", :"py@127.0.0.1"}
IO.puts "Calling my_process via a tuple with hello"
val1 = GenServer.call(my_process, "hello")
:io.format "Result of calling by name ~p~n", [val1]

# Now that we know val1 is remote pid, we can call it again via a pid
IO.puts "Calling my_process via a pid with hello_again"
val2 = GenServer.call(val1, "hello_again")
:io.format "Result of calling again by pid ~p~n", [val2]

send(val1, "some_random_message")

