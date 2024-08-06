function garev --wraps='git add -A && git commit --amend --no-edit && git push --force' --description 'alias garev=git add -A && git commit --amend --no-edit && git push --force'
  git add -A && git commit --amend --no-edit && git push --force
end
