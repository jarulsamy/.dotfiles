#!/bin/bash

# Constatts for kik
readonly KIK_PATH="/sdcard/Kik/"
readonly REMOTE_KIK_PATH="/mnt/shareVol/2tb/Oasis/kik/"
readonly OMEGA_USERNAME="joshua"
readonly OMEGA_HOST="Omega.lan"

ext_rename(){
    # Ensure necessary arguments are given
    if [ -z "$1" ] || [ -z "$2" ] || [ -z "$3" ]
    then
        echo "";
        echo "Positional Arguements:";
        echo -e "  src ext, dest ext, path";
        echo -e "For example:";
        echo -e "  jpg txt 4head/ # changes all files with extension jpg to txt in directory 4head/";
    fi

    src_ext=."$1"
    dst_ext=."$2"
    path="$3"/

    for file in "$path"/*"$src_ext"; 
    do
        src="$file";
        dest="$path"/"$(basename "$file" "$src_ext")""$dst_ext";
        if [[ "$dest" == *"*"* ]];
        then
            continue;
        fi
        mv "$src" "$dest";

    done
    return;
}

# Moves all kik images to server and removes originals.
kik-clean(){    
    # Ensure an argument is given
    if [ -z "$1" ]
    then
        echo "Kik subpath missing!";
        return;
    fi

    # Format dest path
    dest_path="$REMOTE_KIK_PATH"/"$1";
    # Rename images to fix encoding
    ext_rename jpg png "$KIK_PATH"
    # Copy all files over to server
    rsync -progress -ap -essh "$KIK_PATH" "$OMEGA_USERNAME"@"$OMEGA_HOST":"$dest_path"/;
    # Delete files from phone
    rm -f "$KIK_PATH"*

    return;
}

update_alarm(){
    # Parameters to be adjusted
    EN_STATUS=true;
    ONLY_WEEKDAY=true;

    POSITIONAL=()
    while [[ $# -gt 0 ]]
    do
        key="$1"
        case $key in
            -t|--time)
                TIME="$2"
                shift;
                shift;
                ;;
            -e|--enable)
                EN_STATUS=true;
                shift;
                shift;
                ;;
            -d|--disable)
                EN_STATUS=false;
                shift;
                shift;
                ;;
            -ow|--only_weekday)
                ONLY_WEEKDAY=true;
                shift;
                shift;
                ;;
    esac
    done
    # Restore positional parameters
    set -- "${POSITIONAL[@]}"

    echo "TIME = ${TIME}"

    return;
}

