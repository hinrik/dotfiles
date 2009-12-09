# /etc/skel/.bash_logout

# This file is sourced when a login shell terminates.

# Clear the screen for security's sake.
if [ "$SHLVL" = 1 ]; then
    [ -x /usr/bin/clear_console ] && /usr/bin/clear_console -q
fi
