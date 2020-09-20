#!/bin/sh
# Switch sound cards use by ALSA via ~/.asoundrc file.
# Tweak CARD and DEVICE options as appropriate.
# Some applications (e.g. Firefox) may require restart to take effect.

case $1 in
    headphones ) CARD=0; DEVICE=0;;
    hdmi ) CARD=1; DEVICE=7;;
    * ) echo "Usage: $0 {headphones | hdmi}"; exit 1;;
esac

echo "defaults.pcm.card $CARD" > ${HOME}/.asoundrc
echo "defaults.pcm.device $DEVICE" >> ${HOME}/.asoundrc
echo "defaults.ctl.card $CARD" >> ${HOME}/.asoundrc
