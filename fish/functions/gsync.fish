function gsync --wraps='git checkout main && git pull' --description 'alias gsync=git checkout main && git pull'
  git checkout main && git pull
end
