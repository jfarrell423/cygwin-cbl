#!/bin/bash
echo -e "== SYSTEM INFO   =="
echo -e "NAME:\t\t" `hostname`
# echo -e "OS:\t"`hostnamectl | grep "Operating System" | cut -d ' ' -f5-`
# Converted the line for use with CYGWIN
echo -e "OS:\t\t" `uname`

echo -e "== HARDWARE INFO =="
echo -e "MEMORY:\t"`grep MemTotal /proc/meminfo`

echo -e "== NETWORK INFO  =="
#echo -e "System Main IP:\t\t"`hostname -I`
echo -e "System Main IP:\n$(hostname -I | tr ' ' '\n')"


