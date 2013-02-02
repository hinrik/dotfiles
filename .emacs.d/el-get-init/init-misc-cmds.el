; make home key go to beginning of indentation when possible
(require 'misc-cmds)
(global-set-key [home] 'beginning-or-indentation)
(substitute-key-definition 'beginning-of-line 'beginning-or-indentation global-map)
