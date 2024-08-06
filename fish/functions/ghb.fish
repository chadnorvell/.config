function ghb --wraps='git checkout -b $argv' --description 'alias ghb=git checkout -b $argv'
  git checkout -b $argv
end
