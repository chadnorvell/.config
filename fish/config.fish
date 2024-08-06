set fish_greeting

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
