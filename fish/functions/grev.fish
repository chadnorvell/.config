function grev --wraps='git commit --amend --no-edit && git push --force' --description 'alias grev=git commit --amend --no-edit && git push --force'
  git commit --amend --no-edit && git push --force
end
