test -f ~v-perlbrew/perl5/perlbrew/etc/bashrc && HOME=/home/v-perlbrew source ~v-perlbrew/perl5/perlbrew/etc/bashrc
test -f ~/perl5/perlbrew/etc/bashrc && source ~/perl5/perlbrew/etc/bashrc

# use a custom Ruby installation managed by rbenv or rvm
if test -d ~/.rbenv
then
    export PATH="$PATH:$HOME/.rbenv/bin"
    eval "$(rbenv init -)"
elif test -s "$HOME/.rvm/scripts/rvm"
then
    source "$HOME/.rvm/scripts/rvm"
fi

# add to PATH if the dir exists
maybe_add_path() {
    if test -d $1
    then
        export PATH=$1:$PATH
    fi
}

# custom binaries
maybe_add_path $HOME/local/bin
maybe_add_path $HOME/src/crystal/bin

if type luarocks >&/dev/null; then
    eval $(luarocks path)
    export LUA_PATH="lib/?.lua;$LUA_PATH"
    export PATH="$HOME/.luarocks/bin:$PATH"
fi

if [[ "$TERM" == "screen.linux" ]]; then
    # older terminfo doesn't recognize screen.linux
    export TERM=screen
fi

cpus=$(grep -c ^processor /proc/cpuinfo)
export HARNESS_OPTIONS="j$((2*$cpus+1))"
alias bundle="bundle -j$cpus"

if [[ $- != *i* ]] ; then
    # Shell is non-interactive.  Be done now!
    return
fi

if [[ "$TERM" == "linux" ]]; then
    if type conpalette >&/dev/null; then
        conpalette tango-dark
    fi
fi

if [[ -z "$TMUX" && -n "$SSH_TTY" && -n "$SSH_AUTH_SOCK" ]]; then
    wrapper="$HOME/.wrap_auth_sock"
    if [[ "$SSH_AUTH_SOCK" != "$wrapper" ]]; then
        ln -sf $SSH_AUTH_SOCK $wrapper
        export SSH_AUTH_SOCK=$wrapper
    fi
fi

if [[ $(hostname --fqdn) =~ "kvmuser" && -f /etc/bookings/SERVER_ROLE ]]; then
    export this_host="$(cat /etc/bookings/SERVER_ROLE)_kvm"
else
    export this_host=$(hostname -s)
fi

# attach to or start tmux session on ssh login
if [[ $SSH_CONNECTION != "" && -z "$TMUX" && -z "$INSIDE_EMACS" ]]; then
    session=$this_host
    start_session="$HOME/.tmux.d/$session"

    if tmux has-session -t $session 2>/dev/null; then
        tmux attach -t $session
    elif test -f $start_session; then
        source $start_session
        tmux attach -t $session
    else
        tmux new -s $session
    fi
fi

# away with old aliases
\unalias -a

# bash completion
if test -z $BASH_COMPLETION
then
    if test -f /dev/shm/bash_dyncompletion
    then
        # Lightspeed from Debian bug
        # http://bugs.debian.org/cgi-bin/bugreport.cgi?bug=467231
        . /dev/shm/bash_dyncompletion
    elif test -f /etc/bash_completion
    then
        . /etc/bash_completion
    elif test -f /usr/share/bash-completion/bash_completion
    then
        . /usr/share/bash-completion/bash_completion

    fi
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

## Configure __git_ps1

# If there're untracked files, then a '%' will be shown next to the
# branch name.
export GIT_PS1_SHOWUNTRACKEDFILES=yes

# If something is stashed, then a '$' will be shown next to the branch
# name.
export GIT_PS1_SHOWSTASHSTATE=yes

# Show dirty state explicitly, way too verbose in ~/
export GIT_PS1_SHOWDIRTYSTATE=

# If you would like to see the difference between HEAD and its
# upstream, set GIT_PS1_SHOWUPSTREAM="auto".  A "<" indicates you are
# behind, ">" indicates you are ahead, and "<>" indicates you have
# diverged.
export GIT_PS1_SHOWUPSTREAM=auto

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
        #if test -n "$(type -t __git_ps1)"; then
        #    # We can hopefully use __git_ps1 which comes with git's
        #    # bash completion support
        #    local git_info=$(__git_ps1 "%s")
        #    if test -n "$git_info"; then
        #        echo $git_info
        #        return 0
        #    fi
        #else
            # Fall back on something dumb
            local git_info=$(git symbolic-ref HEAD 2>/dev/null | sed -e 's!refs/heads/!!')
            if test -n "$git_info"; then
                echo $git_info
                return 0
            fi
        #fi
    fi

    ls -Alhs | head -n1 | cut -d' ' -f2
}

if ls --help 2>&1 | grep -q group-directories-first; then
    group_dirs=" --group-directories-first"
else
    group_dirs=
fi

# check if we support colors
if type tput >/dev/null &&
    tput_colors="$(tput colors)" &&
    test -n "$tput_colors" &&
    test "$tput_colors" -gt 2
then
    if [ -x /usr/bin/dircolors ]; then
        test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
        alias ls="ls$group_dirs --color=auto -X"

        # old versions of tree(1) don't use colors by default,
        # and we want directories at the top
        alias tree="tree -C --dirsfirst"
    fi

    # Colors on a per-server basis based on a simple
    # checksum. Inspired by http://geofft.mit.edu/blog/sipb/125
    __hostname_color=$((31 + $(echo $this_host | cksum | cut -c1-3) % 6))

    if [[ ${EUID} == 0 ]] ; then
        PS1='\[\e[1;${__hostname_color}m\]$this_host\[\e[m\] \[\e[1;34m\]\W\[\e[m\] (\[\e[;33m\]$(dir_info)\[\e[m\]) \[\e[1;31m\]\$\[\e[m\] '
    else
        PS1='\[\e[1;${__hostname_color}m\]$this_host\[\e[m\] \[\e[1;34m\]\W\[\e[m\] (\[\e[;33m\]$(dir_info)\[\e[m\]) \[\e[1;32m\]\$\[\e[m\] '
    fi

    export PERLDOC="-MPod::Text::Ansi"

    alias grep='grep --color=auto'
    alias egrep='egrep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias rgrep='rgrep --color=auto'
else
    PS1='$this_host \W ($(dir_info)) \$ '
    alias ls="ls$group_dirs -X"
fi

# some nice shell options
shopt -s checkwinsize cdspell dotglob no_empty_cmd_completion

# update bash history in real time
shopt -s histappend
export PROMPT_COMMAND="history -a;$PROMPT_COMMAND"

# don't check for mail
unset MAILCHECK

alias ..="cd .."
alias ...="cd ../.."
alias ....="cd ../../.."
alias ll="ls -lhX"
alias d2u="sed 's/$//'"
alias u2d="sed 's/$//'"
alias lsofnames="lsof | awk '!/^\$/ && /\// { print \$9 }' | sort -u"
alias myip="wget -q -O- 'http://www.moanmyip.com/' | perl -0777 -pe 's[.*<div class=\"ip\">(.*?)</div>.*][\$1\n]s'"
alias mmyip="mplayer http://moanmyip.com/output/\$(myip).mp3"
alias emacs="emacs -nw"

# I want to be able to use Ctrl+s and Ctrl+C in rtorrent
stty start undef
stty stop undef

# some nice less(1) options
export LESS="iMQRS"

# keep a long history without duplicates
export HISTSIZE=100000
export HISTFILESIZE=100000
export HISTCONTROL="ignoreboth"
export HISTTIMEFORMAT="%Y-%m-%d %H:%M:%S  "
export HISTIGNORE="ls:cd:cd ..:..*:"

# ignore these while tab-completing
export FIGNORE="CVS:.svn:.git"

export EDITOR="vim"

# run Rubinius in Ruby 1.9 mode
export RBXOPT="-X19"

if test -d $HOME/.gyg
then
    export PATH=$HOME/.gyg/bin:$PATH
    export CODEARTIFACT_AUTH_TOKEN=$(gygdev --no-init tools get-codeartifact-token 2>/dev/null)
    export SCHEMAPI_LOGIN=inventory SCHEMAPI_PASSWORD=T5pQa2tuf2Qc
fi

# do an ls after every successful cd
function cd {
    builtin cd "$@" && ls >&2
}

# recursive mkdir and cd if successful
function mkcd {
    mkdir -p "$@" && builtin cd "$@"
}

# "du -h", sorted (http://www.perlmonks.org/?node_id=746356)
function duh {
    perl -e'%h=map{/.\s/;7x(ord$&&10)+$`,$_}`du -h @ARGV`;die@h{sort%h}' -- "$@"
}

# Sync files based on content. Useful for dynamically changing files.
function scp {
    rsync --rsh=ssh --archive --no-group --human-readable --progress --copy-unsafe-links "$@"
}

# Append to files based on file size. Useful for large, static or
# append-only files since it skips the expensive hash check. Also retry
# the transfer if it times out.
function leech {
    cmd="rsync --rsh=ssh --append --archive --no-group --human-readable --progress --copy-unsafe-links"
    $cmd "$@"
    while [[ $? == 30 ]]; do sleep 5 && $cmd "$@"; done
}

# a function which accepts similar arguments to scp/rsync, but uses lftp to
# transmit files over multiple parallel connections, for when your
# per-connection speed is limited. A third argument specifies the number of
# connections to use (default is 6, max is 8). Usage examples:
#
# mleech host:some/file /tmp/
# mleech local_file host:
# mleech foo host:/bar/ 8
function mleech {
    local conns=${3:-6}

    if [[ $1 =~ ^[[:alpha:]]+: && ! $2 =~ ^[[:alpha:]]+: ]]; then
        # get
        local remote=${1%%:*}
        local source_path=${1#*:}
        local dest_path=${2:-./}

        if ssh $remote "test -d $source_path"; then
            dest_path="${dest_path:-.}/"
            lftp sftp://$remote -e "glob -- mirror -c --use-pget-n=$conns $source_path $dest_path; exit"
        else
            lftp sftp://$remote -e "glob -- pget -c -n $conns $source_path -o $dest_path; exit"
        fi
    elif [[ $2 =~ ^[[:alpha:]]+: && ! $1 =~ ^[[:alpha:]]+: ]]; then
        # put
        local remote=${2%%:*}
        local source_path=${1#*:}
        local dest_path=${2#*:}

        if test -d $source_path; then
            dest_path="${dest_path:-.}/"
            lftp sftp://$remote -e "glob -- mirror -R -c --use-pget-n=$conns $source_path $dest_path; exit"
        else
            # TODO: lftp has no parallel "put" command for a single file
            lftp sftp://$remote -e "glob -- put -c $source_path -o $dest_path; exit"
        fi
    fi
}

# use rsync's bash completion for the leech/mleech commands, so we can
# tab-complete remote paths
[[ -z $BASH_COMPLETION ]] || complete -F _rsync -o nospace leech mleech

# pass through digital AC3/DTS audio to iec958/spdif output
alias dmplayer="mplayer -ao alsa:device=iec958=1 -ac hwac3,hwdts"
[[ -z $BASH_COMPLETION ]] || complete -F _mplayer dmplayer

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
function asdf-install {
    sbcl --eval "(require :asdf)" --eval "(asdf:operate 'asdf:load-op :asdf-install)" --eval "(asdf-install:install :$1)" --eval "(quit)"
}

# install a Common Lisp package with Quicklisp which is superior to asdf
function ql-install {
    [[ -f ~/quicklisp/setup.lisp ]] || ql-bootstrap
    sbcl --load ~/quicklisp/setup.lisp --eval "(ql:quickload \"$1\")" --eval "(quit)"
}

# bootstrap quicklisp
function ql-bootstrap {
    temp=$(mktemp -t quicklisp.XXXXXXXXXX)
    curl http://beta.quicklisp.org/quicklisp.lisp > $temp
    sbcl --load $temp --eval "(quicklisp-quickstart:install)" --eval "(ql:add-to-init-file)" --eval "(quit)"
    rm $temp
}

# good for links that keep dropping your ssh connections
function keepalive {
    [ -z $1 ] && interval=60 || interval=$1
    while true; do
        date
        sleep $interval
    done
}

function nixmail {
    mail -a "From: Hinrik Örn Sigurðsson <hinrik@nix.is>" -b hinrik.sig@gmail.com "$@"
}

function em {
    exec emacsclient --alternate-editor="" -c "$@"
}
