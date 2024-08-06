function gca --wraps='git commit --amend $argv' --description 'alias gca=git commit --amend $argv'
  git commit --amend $argv
end
