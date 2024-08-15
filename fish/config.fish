set fish_greeting

eval (keychain --eval --quiet id_ed25519)

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
