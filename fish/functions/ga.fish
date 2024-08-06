function ga --wraps='git add -A $argv' --description 'alias ga=git add -A $argv'
  git add -A $argv
end
