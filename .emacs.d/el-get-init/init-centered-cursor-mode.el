; center the cursor vertically when scrolling
(when (boundp 'mouse-wheel-mode)
  (and
    (require 'centered-cursor-mode)
    (global-centered-cursor-mode +1)))
