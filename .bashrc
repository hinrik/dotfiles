if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# away with old aliases
\unalias -a

# disable software flow control
stty -ixon

# programmable completion
if [[ -f /etc/bash_completion ]]; then
    source /etc/bash_completion
fi

# Change the window title of X terminals 
case ${TERM} in
    xterm*|rxvt*|Eterm|aterm|kterm|gnome*|interix)
        PROMPT_COMMAND='echo -ne "\033]0;${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\007"'
        ;;
    screen)
        PROMPT_COMMAND='echo -ne "\033_${USER}@${HOSTNAME%%.*}:${PWD/$HOME/~}\033\\"'
        ;;
esac

# determine if we can use colors
safe_term=${TERM//[^[:alnum:]]/?}   # sanitize TERM
match_lhs=""

[[ -f ~/.dir_colors   ]] && match_lhs="${match_lhs}$(<~/.dir_colors)"
[[ -f /etc/DIR_COLORS ]] && match_lhs="${match_lhs}$(</etc/DIR_COLORS)"
[[ -z ${match_lhs}    ]] \
    && type -P dircolors >/dev/null \
    && match_lhs=$(dircolors --print-database)

if [[ $'\n'${match_lhs} == *$'\n'"TERM "${safe_term}* ]] ; then
    has_color=1 # yay, color
else
    has_color=0
fi
unset safe_term match_lhs

# print some useful info about the current dir
# if we're inside a git working tree, print the current git branch
# if we're inside an svn working directory, print the current svn revision
# or else print the total size of all files in the directory
function dir_info() {
    local git_branch=$(git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
    if [[ -n $git_branch ]]; then
        echo $git_branch
        return 0
    fi

    local svn_rev=$(svn info 2>/dev/null | grep ^Revision | awk '{ print $2 }')
    if [[ -n $svn_rev ]]; then
        echo "r$svn_rev"
        return 0
    fi
    
    ls -Ahs|head -n1|awk '{print $2}'
}

if [[ $has_color ]]; then
    # Enable colors for ls, etc.  Prefer ~/.dir_colors #64489
    if type -P dircolors >/dev/null ; then
        if [[ -f ~/.dir_colors ]] ; then
            eval $(dircolors -b ~/.dir_colors)
        elif [[ -f /etc/DIR_COLORS ]] ; then
            eval $(dircolors -b /etc/DIR_COLORS)
        fi
    fi

    if [[ ${EUID} == 0 ]] ; then
        PS1='\[\e[1;35m\]\h\[\e[m\] \[\e[1;34m\]\W\[\e[m\] (\[\e[;33m\]$(dir_info)\[\e[m\]) \[\e[1;31m\]\$\[\e[m\] '
    else
        PS1='\[\e[1;35m\]\h\[\e[m\] \[\e[1;34m\]\W\[\e[m\] (\[\e[;33m\]$(dir_info)\[\e[m\]) \[\e[1;32m\]\$\[\e[m\] '
    fi

    alias ls='ls --group-directories-first --color=auto'
    alias grep='grep --colour=auto'
else
    PS1='\h \W ($(dir_info)) \$ '
    alias ls='ls --group-directories-first'
fi

# some nice shell options
shopt -s checkwinsize cdspell dotglob histappend nocaseglob no_empty_cmd_completion

alias perl6="~/src/rakudo/perl6"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ll="ls -lh"
alias d2u="sed 's/$//'"
alias u2d="sed 's/$//'"
alias lsofnames="lsof | awk '!/^\$/ && /\// { print \$9 }' | sort -u"
alias myip="lynx -dump -hiddenlinks=ignore -nolist http://checkip.dyndns.org:8245/ | awk '{ print \$4 }' | sed '/^$/d; s/^[ ]*//g; s/[ ]*$//g'"
alias play="mplayer -subdelay -0.700 -delay -0.200 -monitoraspect 1.65 -ao alsa:device=iec958=1" # -vf expand=1280:::::1.65 -ac hwac3,hwdts
alias scp="rsync --rsh=ssh --archive --append --human-readable --partial --progress --times"

if [[ "$TERM" == "linux" ]]; then
    conpalette tango-dark >& /dev/null
fi

# some nice less(1) options
export LESS="iMQRS"

# keep a long history without duplicates
export HISTSIZE=10000
export HISTFILESIZE=10000
export HISTCONTROL="ignoreboth"
export HISTTIMEFORMAT="%Y-%m-%d %H:%M:%S  "

# ignore some boring stuff. The " *" bit ignores all command lines
# starting with whitespace, useful to selectively avoid the history
export HISTIGNORE="ls:cd:cd ..:..*: *"

# ignore these while tab-completing
export FIGNORE="CVS:.svn:.git"

export EDITOR="vim"
export PERLDOC="-MPod::Text::Ansi"

# do an ls after every successful cd
function cd {
    builtin cd "$@" && ls
}

# recursive mkdir and cd if successful
function mkcd {
    mkdir -p "$@" && builtin cd "$@"
}

# delete untracked files/dirs
function svn_clean {
    svn status "$1" | grep '^\?' | cut -c8- | \
    while read fn
        do echo "$fn"
        rm -rf "$fn"
    done
}

# do an svn update and show the log messages since the last update.
function svn_uplog {
    local old_revision=$(svn info $@ | grep ^Revision | awk '{ print $2 }')
    local first_update=$((${old_revision} + 1))

    svn up -q $@
    local new_revision=$(svn info $@ | grep ^Revision | awk '{ print $2 }')
    if [[ ${new_revision} -gt ${old_revision} ]]
    then
            svn log -v -rHEAD:${first_update} $@
    else
            echo "No changes."
    fi
}

# make info(1) work like man(1), until I can be bothered to learn
# how to use info/emacs
function info { /usr/bin/info "$@" --subnodes -o - 2> /dev/null | less ; }
