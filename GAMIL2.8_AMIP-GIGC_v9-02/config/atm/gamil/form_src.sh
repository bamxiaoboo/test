#!/bin/bash

Env=$1
Srclist=$3
source $Env

touch $Srclist
cat > $Srclist << EOF
$CODEROOT/libs/shr
$CODEROOT/libs/timing
$CODEROOT/atm/GAMIL2.8_AMIP/src/physics/cam1
$CODEROOT/atm/GAMIL2.8_AMIP/src/physics/cam1/echam_cu
$CODEROOT/atm/GAMIL2.8_AMIP/src/dynamics/eul
$CODEROOT/atm/GAMIL2.8_AMIP/src/control
$CODEROOT/atm/GAMIL2.8_AMIP/src/utils
$CODEROOT/atm/GAMIL2.8_AMIP/src/advection/slt
$CODEROOT/atm/GAMIL2.8_AMIP/src/ocnsice/dom
$CODEROOT/atm/GAMIL2.8_AMIP/src/ice/csim4
$CODEROOT/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/main
$CODEROOT/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/biogeophys
$CODEROOT/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/mksrfdata
$CODEROOT/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/ecosysdyn
$CODEROOT/atm/GAMIL2.8_AMIP/src/lnd/clm2/src/riverroute
$CODEROOT/atm/GAMIL2.8_AMIP/src/couple/c_coupler
EOF

exit 0
