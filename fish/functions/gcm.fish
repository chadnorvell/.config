function gcm --wraps='git commit -m $argv' --description 'alias gcm=git commit -m $argv'
  git commit -m $argv
end
