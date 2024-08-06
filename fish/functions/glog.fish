function glog --wraps='git log --oneline -10 $argv' --description 'alias glog=git log --oneline -10 $argv'
  git log --oneline -10 $argv
end
