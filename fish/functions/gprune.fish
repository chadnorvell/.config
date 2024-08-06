function gprune --wraps=git\ branch\ --merged\ \|\ grep\ -Ev\ \'\(^\\\*\|main\|master\)\'\ \|\ xargs\ git\ branch\ -d --description alias\ gprune=git\ branch\ --merged\ \|\ grep\ -Ev\ \'\(^\\\*\|main\|master\)\'\ \|\ xargs\ git\ branch\ -d
  git branch --merged | grep -Ev '(^\*|main|master)' | xargs git branch -d $argv
end
