# vim: foldmethod=marker foldlevelstart=0:

# config.load_autoconfig()

# Binds {{{

config.bind(",t", "config-cycle tabs.show never always")
config.bind(",s", "config-cycle statusbar.hide false true")

# }}}

# Tabs {{{

c.tabs.show = "never"
c.tabs.position = "left"
c.tabs.width = "10%"

# }}}

# Completion {{{

c.completion.shrink = True

# }}}

# Statusbar {{{

# }}}

# Colors {{{

## Nord {{{
nord = {
    # Polar Night
    "dark0": "#2E3440",  # darkest
    "dark1": "#3B4252",
    "dark2": "#434C5E",
    "dark3": "#4C566A",  # lightest
    # Snow Storm
    "light0": "#D8DEE9",  # darkest
    "light1": "#E5E9F0",
    "light2": "#ECEFF4",  # lightest
    # Frost
    "lime": "#8FBCBB",  # greenish
    "cyan": "#88C0D0",  # cyan
    "lblue": "#81A1C1",  # light blue
    "blue": "#5E81AC",  # blue
    # Aurora
    "red": "#BF616A",  # red
    "orange": "#D08770",  # orange
    "yellow": "#EBCB8B",  # yellow
    "green": "#A3BE8C",  # green
    "magenta": "#B48EAD",  # magenta
}
## }}}

## Defs {{{
c.colors.completion.category.bg = nord["dark0"]
c.colors.completion.item.selected.bg = nord["dark3"]
c.colors.completion.item.selected.fg = nord["light2"]
c.colors.completion.item.selected.border.bottom = nord["dark3"]
c.colors.completion.item.selected.border.top = nord["dark3"]
c.colors.completion.item.selected.match.fg = nord["blue"]
c.colors.completion.match.fg = nord["lblue"]

c.colors.completion.even.bg = nord["dark0"]
c.colors.completion.odd.bg = nord["dark0"]

c.colors.statusbar.insert.bg = nord["green"]
c.colors.statusbar.insert.fg = nord["dark0"]
c.colors.statusbar.normal.bg = nord["dark0"]
c.colors.statusbar.normal.fg = nord["light2"]
c.colors.tabs.bar.bg = nord["dark0"]
c.colors.tabs.even.bg = nord["dark0"]
c.colors.tabs.odd.bg = nord["dark0"]
c.colors.tabs.even.fg = nord["light2"]
c.colors.tabs.odd.fg = nord["light2"]
c.colors.tabs.selected.even.bg = nord["light2"]
c.colors.tabs.selected.odd.bg = nord["light2"]
c.colors.tabs.selected.even.fg = nord["dark0"]
c.colors.tabs.selected.odd.fg = nord["dark0"]

c.colors.statusbar.url.fg = nord["cyan"]
c.colors.statusbar.url.success.http.fg = nord["orange"]
c.colors.statusbar.url.success.https.fg = nord["green"]
c.colors.statusbar.url.error.fg = nord["red"]
c.colors.statusbar.url.hover.fg = nord["cyan"]
c.colors.statusbar.url.warn.fg = nord["red"]

c.colors.statusbar.command.bg = nord["dark0"]
c.colors.statusbar.command.fg = nord["light2"]

## }}}

## Commented defs {{{

# c.colors.completion.category.bg = nord["dark0"]
# c.colors.completion.category.border.bottom = nord["base03"]
# c.colors.completion.category.border.top = nord["base03"]
# c.colors.completion.category.fg = nord["base3"]
# c.colors.completion.fg = nord["base3"]
# c.colors.completion.scrollbar.bg = nord["base0"]
# c.colors.completion.scrollbar.fg = nord["base2"]
# c.colors.downloads.bar.bg = nord["base03"]
# c.colors.downloads.error.bg = nord["red"]
# c.colors.downloads.error.fg = nord["base3"]
# c.colors.downloads.start.fg = nord["base3"]
# c.colors.hints.bg = nord["violet"]
# c.colors.hints.fg = nord["base3"]
# c.colors.hints.match.fg = nord["base2"]
# c.colors.keyhint.fg = nord["base3"]
# c.colors.keyhint.suffix.fg = nord["yellow"]
# c.colors.messages.error.bg = nord["red"]
# c.colors.messages.error.border = nord["red"]
# c.colors.messages.error.fg = nord["base3"]
# c.colors.messages.info.bg = nord["base03"]
# c.colors.messages.info.border = nord["base03"]
# c.colors.messages.info.fg = nord["base3"]
# c.colors.messages.warning.bg = nord["orange"]
# c.colors.messages.warning.border = nord["orange"]
# c.colors.messages.warning.fg = nord["base3"]
# c.colors.prompts.bg = nord["base02"]
# c.colors.prompts.border = "1px solid " + nord["base3"]
# c.colors.prompts.fg = nord["base3"]
# c.colors.prompts.selected.bg = nord["base01"]
# c.colors.statusbar.caret.bg = nord["dark0"]
# c.colors.statusbar.caret.fg = nord["base3"]
# c.colors.statusbar.caret.selection.bg = nord["violet"]
# c.colors.statusbar.caret.selection.fg = nord["base3"]
# c.colors.statusbar.command.private.bg = nord["base01"]
# c.colors.statusbar.command.private.fg = nord["base3"]
# c.colors.statusbar.passthrough.bg = nord["magenta"]
# c.colors.statusbar.passthrough.fg = nord["base3"]
# c.colors.statusbar.private.bg = nord["base01"]
# c.colors.statusbar.private.fg = nord["base3"]
# c.colors.statusbar.progress.bg = nord["base3"]
# c.colors.tabs.odd.fg = nord["base2"]
# c.colors.tabs.indicator.error = nord["red"]
# c.colors.tabs.indicator.start = nord["violet"]
# c.colors.tabs.indicator.stop = nord["orange"]

## }}}

# }}}
