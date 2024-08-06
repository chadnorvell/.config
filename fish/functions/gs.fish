function gs --wraps='git status $argv' --description 'alias gs=git status $argv'
  git status $argv
end
