#!/bin/bash

# Recompiles the configurator binary and then kills existing processes and boots new one
# Output will be in ~/lamdera/configurator/screenlog.0

set -e

normal()  { tput sgr0;    }
red()     { tput setaf 1; }
green()   { tput setaf 2; }
yellow()  { tput setaf 3; }
blue()    { tput setaf 4; }
magenta() { tput setaf 5; }
cyan()    { tput setaf 6; }
white()   { tput setaf 7; }

RETURN=`pwd`

echo "---> $(blue)Rebuilding configurator...$(normal)"
cd ~/lamdera/configurator
istack install lamdera-configurator:exe:configurator

echo "---> $(red)Killing old versions of 'configurator'$(normal)"
(pgrep -fl "configurator" || true)
for processid in $(pgrep -f "configurator"); do
    kill $processid
done

sleep 2

echo "---> $(blue)Booting 'configurator' on 8080$(normal)"
LDEBUG=1 screen -dmSL "configurator-8080" ~/.local/bin/configurator

echo "---> $(green)Done$(normal)"

cd $RETURN
