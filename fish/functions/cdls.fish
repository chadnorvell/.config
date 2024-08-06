function cdls --wraps='cd $argv && ls -l' --description 'alias cdls=cd $argv && ls -l'
  cd $argv && ls -l
end
