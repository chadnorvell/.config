function gup --wraps='git push $argv' --description 'alias gup=git push $argv'
  git push $argv
end
