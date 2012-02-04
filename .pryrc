# awesome pretty-printing
require "awesome_print"
Pry.print = proc { |output, value| output.puts value.ai }

# show the Ruby version in the prompt
Pry.prompt = [
  proc { |obj, nest_level, _| "\x01\e[33m\x02#{obj}\x01\e[m\x02:\x01\e[35m\x02#{nest_level}\x01\e[m\x02 >> " },
  proc { |obj, nest_level, _| "\x01\e[33m\x02#{obj}\x01\e[m\x02:\x01\e[35m\x02#{nest_level}\x01\e[m\x02  \x01\e[32m\x02|\x01\e[m\x02 " }
]

# vim: ft=ruby
