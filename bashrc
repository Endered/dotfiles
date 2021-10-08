#!/bin/bash

export PATH=$PATH:~/bin/
export PATH=$PATH:~/.dotnet/
export PATH=~/.local/bin/:$PATH
export PATH=$PATH:~/.roswell/bin/
export GST_ID3_TAG_ENCODING='CP932'
if [ -f $HOME/.cargo/env ]; then
    source "$HOME/.cargo/env"
fi

#settings for chrome remote desktop. Because it will hide repeat key.
gsettings set org.gnome.desktop.peripherals.keyboard repeat 'false' #It is necesarry to recorver.
gsettings set org.gnome.desktop.peripherals.keyboard repeat 'true'

# disable middle pointer of thinkpad keyboard
for id in `xinput list | grep 'with TrackPoint' | grep -E 'slave\ *pointer' | grep -P '(?<=id=)\d*' -o | cat`
do
    xinput set-button-map $id 1 0 3 4 5 6 7
done

for id in `xinput list | grep 'Lenovo TrackPoint Keyboard II Mouse' | grep -E 'slave\ *pointer' | grep -P '(?<=id=)\d*' -o | cat`
do
    xinput set-button-map $id 1 0 3 4 5 6 7
done

#disable alert (i.e. at typing backspace)
gsettings set org.gnome.desktop.sound event-sounds false

# Haskell
alias ghci='stack ghci'
alias ghc='stack ghc --'
alias runghc='stack runghc --'

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


# Please download base16-shell before exec
# 'git clone https://github.com/chriskempson/base16-shell.git ~/.config/base16-shell'
# this is vim color helper
BASE16_SHELL="$HOME/.config/base16-shell/"
[ "$TERM" != "dumb" ] && \
    [ -n "$PS1" ] && \
        [ -s "$BASE16_SHELL/profile_helper.sh" ] && \
            eval "$("$BASE16_SHELL/profile_helper.sh")"

function cd(){
    if [ "$#" -ge "1" ];
    then
        command cd "$*"
    else
        command cd
    fi
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
    oj s $(cat .problem_url) submit.lisp
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
