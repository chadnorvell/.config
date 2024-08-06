set fish_greeting

if status is-interactive
    if test -d /opt/homebrew
        fish_add_path -Pm /opt/homebrew/bin
    end

    if type -q vfox
        vfox activate fish | source
    end

    if test -d $HOME/.cargo
        fish_add_path -Pm $HOME/.cargo/bin
    end

    if test -d $HOME/.local/bin
        fish_add_path -Pm $HOME/.local/bin
    end

    if type -q direnv
        direnv hook fish | source
    end
end
