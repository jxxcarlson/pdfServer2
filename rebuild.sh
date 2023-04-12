#!/bin/bash

# Recompiles the imageServer binary and then kills existing processes and boots new one
# Output will be in ~/lamdera/imageServer/screenlog.0

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

echo "---> $(blue)Rebuilding imageServer...$(normal)"
stack build
stack ghc app/Main.hs
mv app/Main imageServer

echo "---> $(red)Killing old versions of 'imageServer'$(normal)"
(pgrep -fl "imageServer" || true)
for processid in $(pgrep -f "imageServer"); do
    kill $processid
done

sleep 2

echo "---> $(blue)Booting 'imageServer' on 3001$(normal)"
LDEBUG=1 screen -dmSL "imageServer" ./imageServer

echo "---> $(green)Done$(normal)"

cd $RETURN