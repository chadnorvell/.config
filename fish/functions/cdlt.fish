function cdls --wraps='cd $argv && eza --tree --level=2' --description 'alias cdls=cd $argv && eza --tree --level=2'
  cd $argv && eza --tree --level=2
end
