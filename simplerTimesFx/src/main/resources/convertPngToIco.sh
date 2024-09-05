if [ -z "$1" ] ; then
    echo "Need file argument"
    exit 1
fi

convert -density 300 -define icon:auto-resize=256,128,96,64,48,32,16 -background none "$1" "logo.ico"
