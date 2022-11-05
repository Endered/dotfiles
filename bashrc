#!/bin/bash

export PATH=$PATH:~/bin/
export PATH=$PATH:~/.dotnet/
export PATH=~/.local/bin/:$PATH
export PATH=$PATH:~/.roswell/bin/
export PATH=$HOME/dotfiles/bin:$PATH
export GST_ID3_TAG_ENCODING='CP932'
if [ -f $HOME/.cargo/env ]; then
    source "$HOME/.cargo/env"
fi

if [ "$SSH_CLIENT" = "" ]; then
    # disable middle pointer of thinkpad keyboard
    for id in `xinput list | grep 'with TrackPoint' | grep -E 'slave\ *pointer' | grep -P '(?<=id=)\d*' -o | cat`
    do
        xinput set-button-map $id 1 0 3 4 5 6 7
    done
    for id in `xinput list | grep 'ETPS/2 Elantech TrackPoint' | grep -E 'slave\ *pointer' | grep -P '(?<=id=)\d*' -o | cat`
    do
        xinput set-button-map $id 1 0 3 4 5 6 7
    done
    for id in `xinput list | grep 'ETPS/2 Elantech Touchpad' | grep -E 'slave\ *pointer' | grep -P '(?<=id=)\d*' -o | cat`
    do
        xinput disable $id
    done

    for id in `xinput list | grep 'Lenovo TrackPoint Keyboard II' | grep -E 'slave\ *pointer' | grep -P '(?<=id=)\d*' -o | cat`
    do
        xinput set-button-map $id 1 0 3 4 5 6 7
    done
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


function cd(){
    # cdコマンド引数なしのときにホームディレクトリに移されるの嫌じゃない？
    # 私は嫌です
    command cd "$*"
    ls
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
