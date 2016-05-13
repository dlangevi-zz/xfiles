#!/bin/sh
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

if [ -s ~/.Xmodmap ]; then
    xmodmap ~/.Xmodmap
fi

xbindkeys

xautolock -time 5 -locker "gnome-screensaver-command -l" -notify 10 \
  -notifier "notify-send -t 5000 -i gtk-dialog-info 'Locking in 10 seconds'" &



xsetroot -cursor_name left_ptr &
exec xmonad
