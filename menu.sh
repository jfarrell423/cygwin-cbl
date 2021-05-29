#!/bin/bash

 

function press_enter

{

    echo ""

    echo -n "Press Enter to continue"

    read

    clear

}

 

function today {

    echo "Today's date is: "

    date +"%A, %B %-d, %Y"

}

 

 

selection=

until [ "$selection" = "0" ]; do

    clear

    echo ""

    echo "COMMAND LINE MENU"

    echo "    ---------------------------"

    echo "1   - display free disk space"

    echo "2   - display free memory"

    echo "3   - display current date"

    echo "4   - display processes"

    echo "5   - Shut Down SSH Service"

    echo "6   - Start Up  SSH Service"

    echo "7   - other programs"

    echo "8   - Phone Numbers"

    echo "8.5 - Do not select this!"

    echo "9   - Password Database"

    echo "    ----------------------------"

    echo "0   - exit menu"

    echo ""

    echo -n "Enter selection: "

    read selection

    echo ""

    case $selection in

        1 ) df ; press_enter ;;

        2 ) free ; press_enter ;;

        3 ) today ; press_enter ;;

                4 ) ps -ef; press_enter;;

                5 ) net stop sshd; press_enter;;

                6 ) net start sshd; press_enter;;

                7 ) games;;

        8 ) utkphonwin ;;

                8.5 ) phbook ;;

        9 ) COB_PRE_LOAD=cobdes utpasssecwin ;;

        0 ) exit ;;

        * ) echo "Please enter 1, 2, or 0"; press_enter

    esac

 

done

