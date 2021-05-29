#!/bin/bash
# This script clears the terminal, displays a greeting and gives information
# about currently connected users.  The two example variables are set and displayed.
clear # Clear the terminal window.
echo "                 * * * * * * * W A R N I N G * * * * * * * * * *"
echo " "
echo " This computer system is the property of the Farrell Family. It is for "
echo " authorized use only. Unauthorized or improper use of this system may result "
echo " in administrative disciplinary action and/or civil charges/criminal penalties."
echo " By continuing to use this system you indicate your awareness of and consent "
echo " to these terms and conditions of use. "
echo " "
echo " LOG OFF IMMEDIATELY if you do not agree to conditions stated in this warning."
echo " "
echo "                 * * * * * * * * * * * * * * * * * * * * * * * * "
echo "Welcome, $USER" # dollar sign is used to get content of variable.
echo "Today's date is `date`, this is week `date +"%V"`."
echo "These users are currently connected:"
who | cut -d " " -f 1 - | grep -v USER | sort -u
echo " "
echo "This is `uname -s` running on a `uname -m` processor."
echo " "
#echo "This is the uptime information:"
#uptime
echo