function ggg --wraps=git\ clone\ \$argv\ \&\&\ cd\ \$\(echo\ \$argv\ \|\ grep\ -o\ \'\[^/\]\*\$\'\ \|\ cut\ -d\ \'.\'\ -f\ 1\) --description alias\ ggg=git\ clone\ \$argv\ \&\&\ cd\ \$\(echo\ \$argv\ \|\ grep\ -o\ \'\[^/\]\*\$\'\ \|\ cut\ -d\ \'.\'\ -f\ 1\)
  git clone $argv && cd $(echo $argv | grep -o '[^/]*$' | cut -d '.' -f 1) $argv
end
