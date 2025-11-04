# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

import subprocess

from libqtile import bar, layout, qtile, widget, hook
from libqtile.config import Click, Drag, Group, Key, Match, Screen, ScratchPad, DropDown
from libqtile.lazy import lazy


class StaticTaskList(widget.TaskList):

    def select_window(self):
        if self.clicked:
            current_win = self.bar.screen.group.current_window
            window = self.clicked
            if window is not current_win:
                window.group.focus(window, False)
                if window.floating:
                    window.bring_to_front()
            else:
                # do not minimize window as the original class does
                pass


mod = 'mod1'
terminal = 'kitty'
wallpaper = '/usr/share/backgrounds/sr.solutions_wall.jpg'
appmenu = 'rofi -show drun -modi run,drun -theme windows11'
powermenu = 'rofi -show power-menu -modi "power-menu:rofi-power-menu --choices=lockscreen/logout/suspend/hibernate/shutdown/reboot" -theme windows11-powermenu'

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html

    # Switch between screens
    Key([mod], 'w', lazy.to_screen(0), desc='Warp focus to first screen'),
    Key([mod], 'e', lazy.to_screen(1), desc='Warp focus to second screen'),
    Key([mod], 'r', lazy.to_screen(2), desc='Warp focus to third screen'),

    # Switch between windows
    Key([mod], 'j', lazy.layout.next(), desc='Move window focus to next window'),
    Key([mod], 'k', lazy.layout.previous(), desc='Move window focus to previous window'),

    # Move windows between left/right columns or move up/down in current stack.
    Key([mod, 'shift'], 'j', lazy.layout.shuffle_down(), desc='Move window down'),
    Key([mod, 'shift'], 'k', lazy.layout.shuffle_up(), desc='Move window up'),
    Key([mod], 'Return', lazy.layout.swap_main(), desc='Move window to main pane'),

    # Grow/shrink windows
    Key([mod, 'shift'], 'l', lazy.layout.grow(), desc='Grow window'),
    Key([mod, 'shift'], 'h', lazy.layout.shrink(), desc='Shrink window'),

    # Layout shortcuts
    Key([mod], 'space', lazy.next_layout(), desc='Toggle between layouts'),
    Key([mod], 'n', lazy.layout.normalize(), desc='Reset all window sizes'),

    Key([mod], 'f', lazy.window.toggle_fullscreen(), desc='Toggle fullscreen on the focused window'),
    Key([mod], 't', lazy.window.toggle_floating(), desc='Toggle floating on the focused window'),

    Key([mod, 'shift'], 'c', lazy.window.kill(), desc='Kill focused window'),
    Key([mod, 'control'], 'r', lazy.reload_config(), desc='Reload the config'),
    Key([mod, 'control'], 'q', lazy.shutdown(), desc='Shutdown Qtile'),

    # Media shortcuts
    Key([], 'XF86AudioMute', lazy.spawn('pactl set-sink-mute 0 toggle')),
    Key([], 'XF86AudioLowerVolume', lazy.spawn('pactl set-sink-volume 0 -5%')),
    Key([], 'XF86AudioRaiseVolume', lazy.spawn('pactl set-sink-volume 0 +5%')),

    # Other shortcuts
    Key([mod, 'shift'], 'Return', lazy.spawn(terminal), desc='Launch terminal'),
    Key([mod], 'F2', lazy.spawn(appmenu), desc='Spawn a command using a prompt widget'),
    Key([], 'Super_L', lazy.spawn(appmenu), desc='Spawn a command using a prompt widget'),
    Key([mod, 'control'], 'l', lazy.spawn('light-locker-command -l'), desc='Lock screen'),
    Key([mod, 'control'], 'Delete', lazy.spawn(powermenu), desc='Show power menu'),
]

# Add key bindings to switch VTs in Wayland.
# We can't check qtile.core.name in default config as it is loaded before qtile is started
# We therefore defer the check until the key binding is run by using .when(func=...)
for vt in range(1, 8):
    keys.append(
        Key(
            ['control', 'mod1'],
            f'f{vt}',
            lazy.core.change_vt(vt).when(func=lambda: qtile.core.name == 'wayland'),
            desc=f'Switch to VT{vt}',
        )
    )


groups = [
    Group('1', label='1:web', layout='max'),
    Group('2', label='2:file'),
    Group('3', label='3:coding'),
    Group('4', label='4:shell'),
    Group('5', label='5:media', layout='max'),
    Group('6', label='6:misc'),
    Group('7', label='7:misc'),
    Group('8', label='8:misc'),
    Group('9', label='9:chat', layout='max'),
]

for group in groups:
    keys.extend(
        [
            # mod + group number = switch to group
            Key(
                [mod],
                group.name,
                lazy.group[group.name].toscreen(),
                desc=f'Switch to group {group.name}',
            ),
             # mod + shift + group number = move focused window to group
             Key([mod, 'shift'], group.name, lazy.window.togroup(group.name),
                 desc=f'move focused window to group {group.name}'),
        ]
    )

# ScratchPads
dropdown_config = dict(
    width=0.8,
    height=0.8,
    x=0.1,
    y=0.1,
    opacity=0.9,
)
groups.extend([
    ScratchPad('scratchpad', [
            DropDown('shell', 'kitty', **dropdown_config),
            DropDown('ranger', 'kitty ranger', **dropdown_config),
            DropDown('vim', 'kitty vim', **dropdown_config),
        ]),
])
keys.extend([
    Key([mod], 's', lazy.group['scratchpad'].dropdown_toggle('shell')),
    Key([mod], 'd', lazy.group['scratchpad'].dropdown_toggle('ranger')),
    Key([mod], 'v', lazy.group['scratchpad'].dropdown_toggle('vim')),
])

monad_config = {
    'new_client_position': 'before_current',
    'border_focus': 'cc0000',
    'border_width': 4
}
layouts = [
    layout.MonadTall(**monad_config),
    layout.MonadWide(**monad_config),
    layout.Max(),
    # Try more layouts by unleashing below layouts.
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    # layout.MonadTall(),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
]

widget_defaults = dict(
    font='sans',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()

bar = bar.Bar(
    [
        widget.Image(filename='~/Graphics/Arch_Linux_Icon.svg', margin=4, mouse_callbacks={
            'Button1': lazy.spawn(appmenu),
        }),
        widget.GroupBox(highlight_method='block', urgent_border='555555', inactive='ffffff', disable_drag=True, fmt='{:.1}'),
        widget.Sep(),
        StaticTaskList(highlight_method='block', title_width_method='uniform', max_title_width=300, rounded=False, urgent_border='215578'),
        # NB Systray is incompatible with Wayland, consider using StatusNotifier instead
        # widget.StatusNotifier(),
        widget.Systray(icon_size=22),
        widget.BatteryIcon(scale=1.2),
        widget.Clock(format='%a, %d.%m %H:%M'),
        widget.TextBox('⏻', fontsize=32, mouse_callbacks={
            'Button1': lazy.spawn(powermenu),
        }),
    ],
    30,
    background='555555',
    opacity=0.9,
    # border_width=[0, 0, 2, 0],  # Draw top and bottom borders
    # border_color=['ff00ff', '000000', 'ff00ff', '000000']  # Borders are magenta
)

screens = [
    Screen(
        top=bar,
        wallpaper=wallpaper,
        wallpaper_mode='stretch',
    ),
    Screen(
        #top=bar,
        wallpaper=wallpaper,
        wallpaper_mode='stretch',
    ),
]

# Drag floating layouts.
mouse = [
    Drag([mod], 'Button1', lazy.window.set_position_floating(), start=lazy.window.get_position()),
    Drag([mod], 'Button3', lazy.window.set_size_floating(), start=lazy.window.get_size(), warp_pointer=True),
    Click([mod], 'Button2', lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class='confirmreset'),  # gitk
        Match(wm_class='makebranch'),  # gitk
        Match(wm_class='maketag'),  # gitk
        Match(wm_class='ssh-askpass'),  # ssh-askpass
        Match(wm_class='xfrun4'),  # xfrun4
        Match(title='branchdialog'),  # gitk
        Match(title='pinentry'),  # GPG key password entry
    ]
)
auto_fullscreen = True
focus_on_window_activation = 'smart'
reconfigure_screens = True

# Autostart
@hook.subscribe.startup_once
def autostart():
    # Background services
    subprocess.Popen('/usr/bin/picom')
    subprocess.Popen('/usr/bin/dunst')
    subprocess.Popen('/usr/bin/light-locker')

    # Applets
    subprocess.Popen('/usr/bin/nm-applet')
    subprocess.Popen('/usr/bin/blueman-applet')
    subprocess.Popen('/opt/owncloud-client.AppDir/AppRun')
    subprocess.Popen('/usr/bin/redshift-gtk')

    # Default applications
    subprocess.Popen('/usr/bin/brave-browser')
    subprocess.Popen('/snap/bin/slack')

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# xcursor theme (string or None) and size (integer) for Wayland backend
wl_xcursor_theme = None
wl_xcursor_size = 24

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = 'LG3D'
