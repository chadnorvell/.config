function set_color_from_status
    if test "$status"
        printf "%s" (set_color red)
    else
        printf "%s" (set_color green)
    end
end

function fish_prompt
    set -l last_status $status

    if test $last_status -ne 0
        set prompt_char_color (set_color red)
    else
        set prompt_char_color (set_color green)
    end

    printf '%s%s%s%s%s' \
        (set_color blue) (prompt_pwd) (set_color normal) \
        (set_color magenta) (fish_git_prompt) (set_color normal) \
        "$prompt_char_color » " (set_color normal)
end
