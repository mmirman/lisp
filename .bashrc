export PATH=/usr/local/git/bin:/opt/local/sbin:/opt/local/libexec/git-core:~/.cabal/bin:/usr/local/bin:/Applications/iTerm.app/Contents/MacOS:/Applications/Emacs.app/Contents/MacOS/bin:$PATH:/opt/local/bin
export DYLD_LIBRARY_PATH=/usr/local/cuda/lib

# export PS1="\A \w \u\$ " 

COL_A="\[$(tput setaf 5)\]"
COL_B="\[$(tput setaf 3)\]"
export PS1="$COL_A\A $COL_B\w $COL_A\u$COL_B\$ \[$(tput sgr0)\]"

alias ll="ls -AlGh"
alias ls="ls -G"

alias doc="cd ~/Documents"

alias emacs="emacs -nw"

function bk {
    alias $1="cd $(PWD)"
    echo "alias $1=\"cd $(PWD)\"" >> ~/.bashrc
}

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

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi