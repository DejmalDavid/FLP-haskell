#!/bin/bash
make
if [ $? == 0 ]
then
    echo ""
    echo ""
    echo ""
    ./bkg-2-cnf
else
    echo "CHYBA!!!"
fi

