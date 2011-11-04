# show the Ruby version in the prompt
# stolen from https://github.com/lucapette/dotfiles/blob/master/pryrc
Pry.prompt = [
  proc { |obj, nest_level, _| "\e[33m#{obj}\e[m:\e[35m#{nest_level}\e[m >> " },
  proc { |obj, nest_level, _| "\e[33m#{obj}\e[m:\e[35m#{nest_level}\e[m  \e[32m|\e[m " }
]

# vim: ft=ruby
