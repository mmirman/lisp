export PATH=/usr/local/opt/coreutils/libexec/gnubin:~/.cabal/bin:~/bin:~/.install-ghc/ghc-8.0.1/bin:~/anaconda/bin/:/opt/local/bin:$PATH

export EDITOR=emacs
export SVN_EDITOR=emacs
export GIT_EDITOR=emacs

shopt -s extglob # lets try this for a while

if [ $TERM == "dumb" ];
then
    alias git="git --no-pager"
fi

COL_A="\[$(tput setaf 5)\]"
COL_B="\[$(tput setaf 3)\]"
COL_END="\[$(tput sgr0)\]"
export PS1="$COL_A\A $COL_B\W$COL_A\$ $COL_END"

if [ -f ~/.git-prompt.sh ]; then
   . ~/.git-prompt.sh
   export PROMPT_COMMAND='__git_ps1 "" "$COL_A\A $COL_B\W$COL_A\\\$ $COL_END" "$COL_B%s "'
fi

alias lo=$(which ls)
alias ls="ls -AGlh"
alias ll="ls"


alias emacs="emacs -nw"

#alias emacsStart="emacs --daemon"

#alias oemacs="emacs -nw"
#alias emacs="emacsclient -nw -t"
#alias emacsStop="emacsclient -nw -t -e '(kill-emacs)'"

ssh-add ~/.ssh/*.pem 2> /dev/null

function bk {
    alias $1="cd $(PWD)"
    echo "alias $1=\"cd $(PWD)\"" >> ~/.bashrc
}

alias initboot="/efi/refit/enable.sh"
alias hdoc="haddock -h -o doc"

function hdocr {
    hdoc $1.hs
    open -a /Applications/Safari.app/Contents/MacOS/Safari "doc/$1.html"
}

if [ -f /opt/local/etc/bash_completion ]; then
      . /opt/local/etc/bash_completion
fi

if [ -f ~/.git-completion.bash ]; then
    . ~/.git-completion.bash
fi

alias git-pull="git pull -r origin"


export JAVA_HOME=$(/usr/libexec/java_home)
export EC2_HOME=/usr/local/ec2/ec2-api-tools-1.7.5.0
export PATH=$PATH:$EC2_HOME/bin/

function start_wm {
    brew services start khd
    brew services start chunkwm
}

function stop_wm {
    brew services stop chunkwm
    brew services stop khd
}



