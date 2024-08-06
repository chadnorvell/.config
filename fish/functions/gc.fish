function gc --wraps='git commit $argv' --description 'alias gc=git commit $argv'
  git commit $argv
end
