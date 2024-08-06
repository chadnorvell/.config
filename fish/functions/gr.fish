function gr --wraps='git rebase $argv' --description 'alias gr=git rebase $argv'
  git rebase $argv
end
