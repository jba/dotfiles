# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return


# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi


# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    #PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\w\a\]$PS1"
    PS2='>> '
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    # WARNING: enabling this can cause multi-second delays due to NFS latency
    #alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f ~/.bash_env ]; then
    . ~/.bash_env
fi


# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

################################################################
# Jba additions begin here.

# Don't exit on ^D
IGNOREEOF=100


# Display a window title.
function title {
  echo -e "\e]0;$1\a"
}

### History tweaks.

# don't put duplicate lines in the history. See bash(1) for more options
# (jba) Do put lines beginning with spaces in the history.
HISTCONTROL=ignoredups

# append to the history file, don't overwrite it
shopt -s histappend

# Store multiline commands as one line.
shopt -s cmdhist

# TAB at start of line doesn't populate every command.
shopt -s no_empty_cmd_completion


# Store each shell process's history in a different file.
HISTFILE=~/.bash_history/hist.$$ 
export HISTFILE

HISTSIZE=30000
HISTFILESIZE=1000000
HISTTIMEFORMAT='%F %T '

###


# If a path begins with $HOME, replace it with ~.
function tildify () {
  local p="${1#$HOME}"
  if [[ "$p" == "$1" ]]; then
    echo $1
  else
    echo "~$p"
  fi
}
  


function tmux_prompt() {
  if [[ $TMUX == "" ]]; then
    echo "no-tmux"
  else
    echo ""
  fi
}


### Prompt.

export BB G3

PROMPT_COMMAND='
        history -a
        GIT_BRANCH=$(git symbolic-ref HEAD 2> /dev/null | sed 's@refs/heads/@@')
        if [[ $GIT_BRANCH != "" && $(git status --porcelain) != "" ]]; then
          GIT_BRANCH=${GIT_BRANCH}'*'
        fi
'

if [ "$color_prompt" = yes ]; then
    red='\[\033[01;31m\]'
    green='\[\033[01;32m\]'
    yellow='\[\033[01;33m\]'
    blue='\[\033[01;34m\]'
    magenta='\[\033[01;35m\]'
    cyan='\[\033[01;36m\]'
    white='\[\033[00m\]'
fi
PS1='${debian_chroot:+($debian_chroot)}'$green'\u@\h'$yellow'${GIT_BRANCH:+ git:$GIT_BRANCH}'$blue' $(prompt_path)\n'$red'$(tmux_prompt)'$white'> '

unset color_prompt force_color_prompt red green yellow blue magenta cyan white

