alias cd='pushd'
alias gl='git lg'
alias glm='git lg master..'
alias gs='git status'
alias listeners='lsof -a +c 0 -i TCP -s TCP:LISTEN'
alias ls='ls -F'
alias telldone='echo "Done.";xmessage -nearmouse done'

alias train='curl -s -d "at=&o=1&d=110&t=01%2F04%2F2013&c=1&s=AM"  http://as0.mta.info/mnr/schedules/sched_results.cfm | html2text | awk "/Departs/ {p=1} p==1"'

