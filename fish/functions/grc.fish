function grc --wraps='git rebase --continue $argv' --description 'alias grc=git rebase --continue $argv'
  git rebase --continue $argv
end
