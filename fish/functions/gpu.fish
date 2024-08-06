function gpu --wraps='git pull $argv' --description 'alias gpu=git pull $argv'
  git pull $argv
end
