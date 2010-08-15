test -f ~v-perlbrew/perl5/perlbrew/etc/bashrc && source ~v-perlbrew/perl5/perlbrew/etc/bashrc

cpus=$(grep -c ^processor /proc/cpuinfo)
export HARNESS_OPTIONS="j$((2*$cpus+1))"

if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

# away with old aliases
\unalias -a

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

# print some useful info about the current dir
# if we're inside a git working tree, print the current git branch
# if we're inside an svn working directory, print the current svn revision
# or else print the total size of all files in the directory
function dir_info() {
    # Test SVN first, because I'm more likely to have a svn checkout
    # inside a git repository (e.g. my ~/ in Git) than the other way
    # around.
    if test -f .svn/entries; then
        # Performance hack, calling svn info takes longer than just
        # grabbing the fourth line from .svn/entries
        local svn_rev=$(sed '4q;d' .svn/entries)
        
        if test -n $svn_rev; then
            echo "r$svn_rev"
            return 0
        fi
    fi

    if type git >&/dev/null; then
        if test -n "$(type -t __git_ps1)"; then
            # We can hopefully use __git_ps1 which comes with git's
            # bash completion support
            local git_info=$(__git_ps1 "%s")
            if test -n "$git_info"; then
                echo $git_info
                return 0
            fi
        else
            # Fall back on something dumb
            local git_info=$(git symbolic-ref HEAD 2>/dev/null | sed -e 's!refs/heads!!')
            if test -n $git_info; then
                echo $git_branch
                return 0
            fi
        fi
    fi

    
    ls -Ahs|head -n1|awk '{print $2}'
}

# Colors on a per-server basis based on a simple
# checksum. Inspired by http://geofft.mit.edu/blog/sipb/125
function hostname_color() {
    echo $((31 + $(hostname | cksum | cut -c1-3) % 6))
    return 0
}

if ls --help|grep group-directories-first >&/dev/null; then
    group_dirs=" --group-directories-first"
else
    group_dirs=
fi

# check if we support colors
if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    if [ -x /usr/bin/dircolors ]; then
        test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
        alias ls="ls$group_dirs --color=auto"

        # old versions of tree(1) don't use colors by default
        alias tree="tree -C"
    fi

    if [[ ${EUID} == 0 ]] ; then
        PS1='\[\e[1;$(hostname_color)m\]\h\[\e[m\] \[\e[1;34m\]\W\[\e[m\] (\[\e[;33m\]$(dir_info)\[\e[m\]) \[\e[1;31m\]\$\[\e[m\] '
    else
        PS1='\[\e[1;$(hostname_color)m\]\h\[\e[m\] \[\e[1;34m\]\W\[\e[m\] (\[\e[;33m\]$(dir_info)\[\e[m\]) \[\e[1;32m\]\$\[\e[m\] '
    fi

    export PERLDOC="-MPod::Text::Ansi"

    alias grep='grep --color=auto'
    alias egrep='egrep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias rgrep='rgrep --color=auto'
else
    PS1='\h \W ($(dir_info)) \$ '
    alias ls="ls$group_dirs"
fi

# some nice shell options
shopt -s checkwinsize cdspell dotglob histappend no_empty_cmd_completion

alias perl6="~/src/rakudo/perl6"
alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ll="ls -lh"
alias d2u="sed 's/$//'"
alias u2d="sed 's/$//'"
alias lsofnames="lsof | awk '!/^\$/ && /\// { print \$9 }' | sort -u"
alias myip="wget -q -O- 'http://www.moanmyip.com/' | perl -0777 -pe 's[.*<div class=\"ip\">(.*?)</div>.*][\$1\n]s'"
alias mmyip="mplayer http://moanmyip.com/output/\$(myip).mp3"

if [[ "$TERM" == "linux" ]]; then
    if type conpalette >&/dev/null; then
        conpalette tango-dark
    fi
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

# use my locally installed Perl modules where available
eval $(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib 2>/dev/null)

# do an ls after every successful cd
function cd {
    builtin cd "$@" && ls
}

# recursive mkdir and cd if successful
function mkcd {
    mkdir -p "$@" && builtin cd "$@"
}

# Sync files based on content. Useful for dynamically changing files.
function scp {
    rsync --rsh=ssh --archive --no-group --human-readable --progress "$@"
}

# Append to files based on file size. Useful large, static or append-only
# files since it skips the expensive hash check. Also retry the transfer
# if it times out.
function leech {
    cmd="rsync --rsh=ssh --append --archive --no-group --human-readable --progress"
    $cmd "$@"
    while [[ $? == 30 ]]; do sleep 5 && $cmd "$@"; done
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

# install Common Lisp packages from the command line
function asdf_install {
    sbcl --eval "(asdf:operate 'asdf:load-op :asdf-install)" --eval "(asdf-install:install :$1)" --eval "(quit)"
}

# good for links that keep dropping your ssh connections
function keepalive {
    [ -z $1 ] && interval=60 || interval=$1
    while true; do
        date
        sleep $interval
    done
}
