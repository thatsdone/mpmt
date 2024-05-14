defmodule Mpmt1 do

  def busy_worker(time_left) do
    IO.puts("busy_worker: started. #{time_left}")
    now = :os.system_time(:millisecond)
    busy_worker(now, time_left)
  end

  def busy_worker(current, time_left) when time_left > 0 do
    #IO.puts " busy_worker"
    now = :os.system_time(:millisecond)
    busy_worker(now, time_left - (now - current))
  end

  def busy_worker(_current, time_left) do
    IO.puts("busy_worker: expired. #{time_left}")
    IO.puts("busy_worker: expired2. #{_current}")
  end

  def mailbox() do
    IO.puts "Mpmt1: Hello world"
    IO.inspect self()
    receive do
      {:ping, pid} ->
        #:timer.sleep(500)
        IO.puts " Ping !!"
        send(pid, {:pong, pid})
        mailbox()
      {:pong, pid} ->
        #:timer.sleep(500)
        IO.puts " Pong !!"
        send(pid, {:ping, pid})
        mailbox()
    end
  end
end

duration = 5.0 * 1000
Mpmt1.busy_worker(duration)

#spawn(Mpmt1, :run, [duration])

#:timer.sleep(3000)

#pid = spawn(Mpmt1, :mailbox, [])
#IO.puts "main: Hello world"
#IO.inspect self()
#send(pid, {:ping, pid})
