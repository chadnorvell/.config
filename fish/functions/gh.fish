function gh --wraps='git checkout $argv' --description 'alias gh=git checkout $argv'
  git checkout $argv
end
