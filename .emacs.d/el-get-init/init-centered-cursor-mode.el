; center the cursor when scrolling
(when window-system
  (and
    (require 'centered-cursor-mode)
    (global-centered-cursor-mode +1)))
