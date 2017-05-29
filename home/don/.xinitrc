[[ -f ~/.Xresources ]] && xrdb -merge -I$HOME ~/.Xresources

if test x"$DBUS_SESSION_BUS_ADDRESS" = x""; then
 dbuslaunch=`which dbus-launch`
 if test x"$dbus-launch" != x"" -a x"$dbus-launch" != x"no"; then
  eval `$dbus-launch --sh-syntax --exit-with-session`
 fi
fi

xsetroot -cursor_name left_ptr
 
urxvtd --quiet --opendisplay --fork

feh --bg-center /usr/share/backgrounds/HAL.png

redshift &

exec xmonad
