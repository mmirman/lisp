export PATH=/usr/local/git/bin:/opt/local/sbin:/usr/local/smlnj-110.74/bin:/opt/local/libexec/git-core:/usr/local/cuda/bin:~/.cabal/bin:/usr/local/bin:/Applications/iTerm.app/Contents/MacOS/:/Applications/Emacs.app/Contents/MacOS/bin/:$PATH:/opt/local/bin
export DYLD_LIBRARY_PATH=/usr/local/cuda/lib
export USERWM=`which xmonad`

# export PS1="\A \w \u\$ " 

COL_A="\[$(tput setaf 5)\]"
COL_B="\[$(tput setaf 3)\]"
export PS1="$COL_A\A $COL_B\w $COL_A\u$COL_B\$ \[$(tput sgr0)\]"

alias ll="ls -AlGh"
alias ls="ls -G"

alias home="cd /Users/matt"
alias doc="cd /Users/matt/Documents"

alias rayhome="cd /Users/matt/Documents/development/haskell/graphics/raytracer/src"

alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

#alias emacsStart="/Applications/Emacs.app/Contents/MacOS/Emacs --daemon"

#alias oemacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"
#alias emacs="emacsclient -nw -t"
#alias emacsStop="emacsclient -nw -t -e '(kill-emacs)'"

function bk {
    alias $1="cd $(PWD)"
    echo "alias $1=\"cd $(PWD)\"" >> ~/.bashrc
}

alias p4out="cd /Users/matt/Desktop/p4out"
alias hgraphics="cd /Users/matt/Documents/development/haskell/graphics"


alias kdsplay="cd /Users/matt/Documents/development/haskell/graphics/kdsplay"

alias etfonts="xset +fp /Users/matt/fonts"

test -r /sw/bin/init.sh && . /sw/bin/init.sh
. /sw/bin/init.sh
alias initboot="/efi/refit/enable.sh"
alias mircode="cd /Users/matt/Documents/development/haskell/mircode"
alias lambda="cd /Users/matt/Documents/development/haskell/lambda"
alias root="cd /"
alias safari="open -a /Applications/Safari.app/Contents/MacOS/Safari"
alias hdoc="haddock -h -o doc"

function hdocr {
    hdoc $1.hs
    open -a /Applications/Safari.app/Contents/MacOS/Safari "doc/$1.html"
}

export EDITOR=emacs
export SVN_EDITOR=emacs
export GIT_EDITOR=emacs

if [ -f /opt/local/etc/bash_completion ]; then
      . /opt/local/etc/bash_completion
fi
