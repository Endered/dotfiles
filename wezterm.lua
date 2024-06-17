local wezterm = require 'wezterm'

return {
    use_ime = true,
    font_size = 9.0,
    enable_tab_bar = false,
    exit_behavior = "Close",
    front_end = "OpenGL",
    check_for_updates = false,
    harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' },

    font = wezterm.font_with_fallback {
        'JetBrains Mono',
        'Noto Sans CJK JP',
    },

    color_scheme = 'Monokai (base16)',
    -- https://github.com/oneKelvinSmith/monokai-emacs
    -- reference
    --colors = {
    --    foreground = "#F8F8F2",
    --    background = "#272822",
    --    cursor_bg = "#F8F8F2",
    --    cursor_fg = "#272822",
    --    cursor_border = "#3E4451",

    --    -- the foreground color of selected text
    --    selection_fg = "#F8F8F2",
    --    -- the background color of selected text
    --    selection_bg = "#3E3D31",

    --    -- The color of the scrollbar "thumb"; the portion that represents the current viewport
    --    scrollbar_thumb = "#222222",

    --    -- The color of the split lines between panes
    --    split = "#444444",

    --    ansi = {"#272822", "#F92672", "#A6E22E", "#E6DB74", "#66D9EF", "#FD5FF0", "#A1EFE4", "#F8F8F2"},
    --    brights = {"#272822", "#F92672", "#A6E22E", "#E6DB74", "#66D9EF", "#FD5FF0", "#A1EFE4", "#F8F8F2"},

    --    -- Arbitrary colors of the palette in the range from 16 to 255
    --    indexed = {[136] = "#af8700"},

    --  -- Since: 20220319-142410-0fcdea07
    --  -- When the IME, a dead key or a leader key are being processed and are effectively
    --  -- holding input pending the result of input composition, change the cursor
    --  -- to this color to give a visual cue about the compose state.
    --},

    keys = {
        {
            key = " ",
            mods = "SHIFT",
            action = wezterm.action.SendKey { key = " ", mods = "" },
        },
    },
}
