#!/bin/bash

if [ $TERM == "dumb" ]; then
	export PS1='> '
	return
fi

export PATH=$PATH:~/bin/
export PATH=$PATH:~/.dotnet/
export PATH=~/.local/bin/:$PATH
export PATH=$PATH:~/.roswell/bin/
export PATH=$HOME/dotfiles/bin:$PATH
export GST_ID3_TAG_ENCODING='CP932'
if [ -f $HOME/.cargo/env ]; then
    source "$HOME/.cargo/env"
fi

#disable alert (i.e. at typing backspace)
gsettings set org.gnome.desktop.sound event-sounds false

if [ ! -e /tmp/mytmp ];then
    mkdir /tmp/mytmp
fi

function catc(){
    cat $1 | xsel --clipboard
}

function emacs(){
    command emacs "$@" &
}

function temacs(){
    command emacs -nw "$@"
}


alias ls='ls --color'
function cd(){
    # cdコマンド引数なしのときにホームディレクトリに移されるの嫌じゃない？
    # 私は嫌です
    command cd "$*"
    command ls --color
}

function ojd(){
    if [ -e "test" ]; then
        rm -r test
    fi
    oj d "$*"
    echo "$*" > .problem_url
}

function ojt(){
    make
    oj t
}

function ojs(){
    oj s $(cat .problem_url) main.cpp --no-open -y
}

function ojtc(){
    oj t -c 'sbcl --script main.lisp'
}
function ojsc(){
    cat header.lisp main.lisp > submit.lisp
    oj s $(cat .problem_url) submit.lisp --no-open -y
}

function workspace(){
    FILE=$HOME/Documents/etc/workspace
    if [ "$1" = "move" ];then
        select i in $(cat $FILE)
        do
            cd $i
            return
        done
    fi

    if [ "$1" = "add" ];then
        pwd >> $FILE
        return
    fi

    if [ "$1" = "del" ];then
        select i in $(cat $FILE)
        do
            tmp=$(cat $FILE)
            echo > $FILE
            for j in $tmp
            do
                if [ $i != $j ];then
                    echo $j >> $FILE
                fi
            done
            break
        done
        return
    fi
    echo move add del
}

alias ttmux='tmux new-session -A -s'

_ttmux() {
    local cur prev cword servers
    _get_comp_words_by_ref -n : cur prev cword
    servers=$(tmux ls 2> /dev/null)
    if [ $? != 0 ]; then
	servers=""
    fi

    if [ $cword != 1 ]; then
	servers=""
    fi

    servers=$(echo "$servers" | grep -oP '^\S*(?=:)')
    COMPREPLY=($(compgen -W "${servers}" -- "${cur}"))
}

complete -F _ttmux ttmux

function refresh() {
    if [ "$TMUX" != "" ]; then
        export $(tmux show-environment | grep '^DISPLAY')
    fi
}

alias rf=fd

export PS1=''
export PS1="$PS1"'\n\[\033[1;32m\]'
export PS1="$PS1"'$(x=$?;[ -n "$IN_NIX_SHELL" ] && echo "(nix) ";exit $x)'
export PS1="$PS1"'['
export PS1="$PS1"'\[\e]0;\u@\h: \w\a\]'
export PS1="$PS1"'\u@\h'
export PS1="$PS1"' ($(printf %3d $?))'
export PS1="$PS1"':\w'
export PS1="$PS1"']'
export PS1="$PS1"'\$\[\033[0m\] '


# FZF settings
export FZF_DEFAULT_COMMAND='fd --type f'

set -o vi
bind -m vi-command 'Control-l: clear-screen'
bind -m vi-insert 'Control-l: clear-screen'
