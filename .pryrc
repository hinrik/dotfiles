# show the Ruby version in the prompt
# stolen from https://github.com/lucapette/dotfiles/blob/master/pryrc
require 'rainbow'
include Sickill::Rainbow
Pry.prompt = [
    proc { |obj, nest_level, _| "#{obj}".foreground(:yellow)+":"+"#{nest_level}".foreground(:magenta)+" >> ".foreground(:green) },
    proc { |obj, nest_level, _| "#{obj}".foreground(:yellow)+":"+"#{nest_level}".foreground(:magenta)+"  | ".foreground(:green) }
]

# vim: ft=ruby
