function lt --wraps='eza --tree --level=$1 $argv' --description 'alias lt=eza --tree --level=$1 $argv'
  if test (count $argv) -eq 0
    eza --tree --level=2
  else if test (count $argv) -eq 1
    eza --tree --level=$argv
  else
    eza --tree --level=$argv[1] $argv[2..-1]
  end
end
