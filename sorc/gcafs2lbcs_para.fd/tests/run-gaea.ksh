#!/bin/ksh -xa

PDY=20231005
cyc=12

gcafscyc=00
let tstepdiff=$cyc-$gcafscyc

if [ ! -s gcafs-input-$PDY ]; then
 mkdir gcafs-input-$PDY
 ln -s /gpfs/f6/bil-fire3/world-shared/AQM_Testbed/GEFS_Aerosol/gcafs.$PDY/00/*nc gcafs-input-$PDY/
fi
if [ ! -e gcafs-input-$PDY/gcafs.t${gcafscyc}z.atmf000.nc ]; then
 echo "can not find gcafs-input-$PDY/gcafs.t${gcafscyc}z.atmf000.nc"
 exit 1
fi

if [ ! -e INPUT/aqm.t${cyc}z.gfs_bndy.tile7.f000.nc ]; then
 echo " no original LBC file $INPUT/aqm.t${cyc}z.gfs_bndy.tile7.f000.nc "
 exit 1
fi
if [ ! -s OUTPUT ]; then
 mkdir -p OUTPUT
fi
# cp -pL INPUT/aqm.t${cyc}z.gfs_bndy.tile7.f???.nc OUTPUT/

# 12 hours
NUMTS=3

cat > gcafs2lbc.ini <<EOF
&control
 tstepdiff=$tstepdiff
 dtstep=6
 bndname='aothrj','aecj','aorgcj','asoil','numacc','numcor'
 mofile='gcafs-input-$PDY/gcafs.t00z.atmf','.nc'
 lbcfile='OUTPUT/aqm.t12z.gfs_bndy.tile7.f','.nc'
 topofile='/gpfs/f6/bil-fire3/scratch/Wei-ting.Hung/aqm_rundir/aqmv8p1.1_gfsv16_warmstart_canopy-off_202404/orog/C793_oro_data.tile7.halo4.nc'
&end

Species converting Factor
# Gocart ug/m3 to regional ug/m3
'dust1'    2  ## 0.2-2um diameter: assuming mean diameter is 0.3 um (volume= 0.01414x10^-18 m3) and density is 2.6x10^3 kg/m3 or 2.6x10^12 ug/m3.so 1 particle = 0.036x10^-6 ug
'aothrj'  1.0   'numacc' 27205909.
'dust2'    4  ## 2-4um
'aothrj'  0.45    'numacc'  330882.  'asoil'  0.55   'numcor'  50607.
'dust3'    2  ## 4-6um
'asoil'   1.0   'numcor' 11501.
'dust4'    2   ## 6-12um
'asoil'  0.7586   'numcor' 1437.
'bc1'      2     # kg/kg
'aecj'     1.0   'numacc' 6775815.
'bc2'  2     # kg/kg
'aecj'     1.0   'numacc' 6775815.
'oc1'  2     # kg/kg OC -> organic matter
'aorgcj'    1.0   'numacc' 6775815.
'oc2'  2
'aorgcj'  1.0   'numacc' 6775815.
EOF

srun --time=30:00 -n $NUMTS -M c6 --export='all' gcafs2lbc_para.x
