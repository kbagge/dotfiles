alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias -- -='cd -'

alias q=exit
alias clr=clear
alias sudo='sudo '
alias rm='rm -I'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'
alias ls='exa' # a nicer way of ls
alias cat='bat' # a nicer cat
alias wget='wget -c'

alias zip='p7zip'

autoload -U zmv


zman() {
  PAGER="less -g -s '+/^       "$1"'" man zshall;
}


### Usefull functions ###
function myip {
  curl -sq "https://icanhazip.com/" }

# To easily encrypt a document with GPG from the yubikey gpg guide linked in the yubikey notes
function secret () {
        output=~/"${1}".$(date +%s).enc
        gpg --encrypt --armor --output ${output} -r 0x0000 -r 0x0001 -r 0x0002 "${1}" && echo "${1} -> ${output}"
}
# To decrypt the above.
function reveal () {
        output=$(echo "${1}" | rev | cut -c16- | rev)
        gpg --decrypt --output ${output} "${1}" && echo "${1} -> ${output}"
}

