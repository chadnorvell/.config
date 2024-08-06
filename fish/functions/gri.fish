function gri --wraps='git rebase -i $argv' --description 'alias gri=git rebase -i $argv'
  git rebase -i $argv
end
