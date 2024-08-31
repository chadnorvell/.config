set fish_greeting

eval (keychain --eval --quiet --noask id_ed25519)

if status is-interactive
    if type -q vfox
        vfox activate fish | source
    end

    if test -d $HOME/.cargo
        fish_add_path -Pm $HOME/.cargo/bin
    end

    if type -q direnv
        direnv hook fish | source
    end
end


# BEGIN opam configuration
# This is useful if you're using opam as it adds:
#   - the correct directories to the PATH
#   - auto-completion for the opam binary
# This section can be safely removed at any time if needed.
test -r '/home/chad/.opam/opam-init/init.fish' && source '/home/chad/.opam/opam-init/init.fish' > /dev/null 2> /dev/null; or true
# END opam configuration
