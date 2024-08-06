function cdls --wraps='cd $argv && tree -L 2' --description 'alias cdls=cd $argv && tree -L 2'
  cd $argv && tree -L 2
end
