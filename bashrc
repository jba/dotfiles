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

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f ~/.bash_env ]; then
    . ~/.bash_env
fi

if [ -f ~/.bash_functions ]; then
    . ~/.bash_functions
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
  
# Get the path below the google3 directory, or the empty
# string if we're not under google3.
function sub_google3_dir () {
  local dir=${PWD#/*google3/}
  if [[ $dir == $PWD ]]; then
    echo ""
  else
    echo $dir
  fi
}

# What the path part of the prompt should be.
function prompt_path () {
  local dir=$(sub_google3_dir)
  if [[ $dir != "" ]]; then
    echo "//$dir"
  elif [[ $(basename $PWD) == "google3" ]]; then
    echo google3
  else
    echo $(tildify $PWD)
  fi
}
  
# Get the path of the blaze-bin directory corresponding 
# to the current directory.
function blaze_bin_dir () {
  local dir=$(sub_google3_dir)
  if [[ $dir == "" ]]; then
    echo ""
  else
    local prefix=${PWD%$dir}
    echo ${prefix}blaze-bin/${dir}
  fi
}

function tmux_prompt() {
  if [[ $TMUX == "" ]]; then
    echo "no-tmux"
  else
    echo ""
  fi
}

# Find a compact display of a directory, using knowledge about citc and g4d
# aliases. In fact, the return value should be a valid argument to g4d.

function citc_client () {
  local path="$1"
  local a="${path##/google/src/cloud/$LOGNAME}"
  if [[ "$a" == "$path" || "$a" == "" ]]; then
    return 1
  else
    local b=${a##/}
    echo ${b%%/*}
  fi
}

### Prompt.

# Abbreviate some parts of the path as it appears in the prompt.
# Obsoleted by citc stuff below.
#PROMPT_COMMAND='history -w; SPWD=$(echo $PWD | sed -e /.\\\{30,\\\}/s/google3/G3/ -e s/contentads/CA/ -e s/yield_management/YM/ -e s@$HOME@~@ -e s@/google/src/cloud/jba@\$CITC@)'

export BB G3

PROMPT_COMMAND='
        history -a
        BB=$(blaze_bin_dir)
        if [[ ${PWD:(-8)} == "/google3" ]]; then G3=$PWD; else G3=${PWD%/google3/*}/google3; fi
        CITC_CLIENT=$(citc_client $PWD)
        GIT_BRANCH=$(git symbolic-ref --short HEAD 2> /dev/null || git show-ref -s HEAD 2> /dev/null)
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
PS1='${debian_chroot:+($debian_chroot)}'$green'\u@\h'$yellow'${CITC_CLIENT:+ citc:$CITC_CLIENT}${GIT_BRANCH:+ git:$GIT_BRANCH}'$blue' $(prompt_path)\n'$red'$(tmux_prompt)'$white'> '

PS2='>> '

unset color_prompt force_color_prompt red green yellow blue magenta cyan white

#PROMPT_DIRTRIM=4

#PS1=${PS1//\\w/\$SPWD}




# The next line updates PATH for the Google Cloud SDK.
source '/usr/local/google/home/jba/Downloads/google-cloud-sdk/path.bash.inc'

# The next line enables shell command completion for gcloud.
source '/usr/local/google/home/jba/Downloads/google-cloud-sdk/completion.bash.inc'
