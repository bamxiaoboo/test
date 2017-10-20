#!/bin/bash

Env=$1
Srclist=$3
source $Env

touch $Srclist
cat > $Srclist << EOF
$CODEROOT/atm_chem/GIGC/Code.v9-02/bin
$CODEROOT/atm_chem/GIGC/Code.v9-02/doc
$CODEROOT/atm_chem/GIGC/Code.v9-02/GeosApm
$CODEROOT/atm_chem/GIGC/Code.v9-02/GeosCore
$CODEROOT/atm_chem/GIGC/Code.v9-02/GeosUtil
$CODEROOT/atm_chem/GIGC/Code.v9-02/GTMM
$CODEROOT/atm_chem/GIGC/Code.v9-02/Headers
$CODEROOT/atm_chem/GIGC/Code.v9-02/help
$CODEROOT/atm_chem/GIGC/Code.v9-02/ISOROPIA
$CODEROOT/atm_chem/GIGC/Code.v9-02/KPP/standard
$CODEROOT/atm_chem/GIGC/Code.v9-02/KPP/SOA/
$CODEROOT/atm_chem/GIGC/Code.v9-02/KPP/int/
$CODEROOT/atm_chem/GIGC/Code.v9-02/NcdfUtil
$CODEROOT/atm_chem/GIGC/Code.v9-02/obsolete
$CODEROOT/atm_chem/GIGC/Code.v9-02/obsolete/standard
$CODEROOT/atm_chem/GIGC/Code.v9-02/
EOF

exit 0
