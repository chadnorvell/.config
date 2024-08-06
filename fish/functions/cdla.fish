function cdla --wraps='cd $argv && ls -la' --description 'alias cdla=cd $argv && ls -la'
  cd $argv && ls -la
end
