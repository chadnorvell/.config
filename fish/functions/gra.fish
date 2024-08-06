function gra --wraps='git rebase --abort $argv' --description 'alias gra=git rebase --abort $argv'
  git rebase --abort $argv
end
