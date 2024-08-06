function gwip --wraps=git\ add\ -A\ \&\&\ git\ commit\ --no-verify\ -m\ \'\~\~WIP\~\~\' --description alias\ gwip=git\ add\ -A\ \&\&\ git\ commit\ --no-verify\ -m\ \'\~\~WIP\~\~\'
  git add -A && git commit --no-verify -m '~~WIP~~'
end
