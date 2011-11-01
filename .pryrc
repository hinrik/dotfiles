# show the Ruby version in the prompt
# stolen from https://github.com/lucapette/dotfiles/blob/master/pryrc
Pry.prompt = [proc { |obj, nest_level, _| "#{RUBY_VERSION} (#{obj}):#{nest_level} > " }, proc { |obj, nest_level, _| "#{RUBY_VERSION} (#{obj}):#{nest_level} * " }]
