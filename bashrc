#!/bin/bash

export PATH=$PATH:~/bin/
export PATH=$PATH:~/.dotnet/
export PATH=$PATH:~/.local/bin/
export PATH=$PATH:~/.roswell/bin/
export GST_ID3_TAG_ENCODING='CP932'
export GOOGLE_DRIVE=/run/user/1000/gvfs/google-drive:host=gmail.com,user=yy56ga10ve
xinput set-button-map 'Lenovo TrackPoint Keyboard II Mouse' 1 0 3 4 5 6 7
xinput set-button-map 'TrackPoint Keyboard II Mouse' 1 0 3 4 5 6 7
xinput set-button-map 'TPPS/2 IBM TrackPoint' 1 0 3 4 5 6 7

#settings for chrome remote desktop. Because it will hide repeat key.
gsettings set org.gnome.desktop.peripherals.keyboard repeat 'false' #It is necesarry to recorver.
gsettings set org.gnome.desktop.peripherals.keyboard repeat 'true'

#disable alert (i.e. at typing backspace)
gsettings set org.gnome.desktop.sound event-sounds false

if [ ! -e /tmp/mytmp ];then
    mkdir /tmp/mytmp
fi

function catc(){
    cat $1 | xsel --clipboard
}

function cd(){
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
    oj s $(cat .problem_url) main.cpp
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
