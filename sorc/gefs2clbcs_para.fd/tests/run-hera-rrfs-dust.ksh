#!/bin/ksh -xa

PDY=20230226
cyc=00

gefscyc=00
let tstepdiff=$cyc-$gefscyc

ORIGDIR=/scratch2/NCEPDEV/fv3-cam/noscrub/Youhua.Tang/aqm_parallel_glbc.fd/rrfs-sd/input-lbc/$PDY$cyc

if [ ! -s gefs-input-$PDY ]; then
 ln -s /scratch2/NCEPDEV/stmp3/Youhua.Tang/gefs.$PDY/$gefscyc/chem/sfcsig gefs-input-$PDY
fi
if [ ! -s gefs-input-$PDY/geaer.t${gefscyc}z.atmf000.nemsio ]; then
 echo "can not find gefs-input-$PDY/geaer.t${gefscyc}z.atmf000.nemsio"
 exit 1
fi

if [ ! -s $ORIGDIR/gfs_bndy.tile7.000.nc ]; then
 echo " no original LBC file $ORIGDIR/gfs_bndy.tile7.000.nc "
 exit 1
fi
if [ ! -s INPUT ]; then
 mkdir -p INPUT
fi
# cp -p $ORIGDIR/gfs_bndy.tile7.???.nc INPUT/
# 48 hours
NUMTS=9

cat > gefs2lbc-nemsio.ini <<EOF
&control
 tstepdiff=$tstepdiff
 dtstep=6 
 bndname='dust','coarsepm'
 mofile='gefs-input-$PDY/geaer.t${gefscyc}z.atmf','.nemsio'
 lbcfile='INPUT/gfs_bndy.tile7.','.nc'
 topofile='/scratch2/NCEPDEV/naqfc/Margaret.R.Marvin/rrfs/rrfsb_v0.7.1/ufs-srweather-app/regional_workflow/fix/lam/RRFS_NA_3km/C3463_oro_data.tile7.halo4.nc'
 inblend=20
&end

Species converting Factor
# Gocart ug/m3 to regional ug/m3
'dust1'    1  ## 0.2-2um diameter: assuming mean diameter is 0.3 um (volume= 0.01414x10^-18 m3) and density is 2.6x10^3 kg/m3 or 2.6x10^12 ug/m3.so 1 particle = 0.036x10^-6 ug
'dust'  1.0   
'dust2'    2  ## 2-4um
'dust'  0.714  'coarsepm'  0.286
'dust3'    1  ## 4-6um
'coarsepm'  1.0   
'dust4'    1   ## 6-12um
'coarsepm'  1.0 
'dust5'    1     # kg/kg
'coarsepm'  1.0
EOF

export  SALLOC_ACCOUNT=naqfc
export  SBATCH_ACCOUNT=naqfc
export  SLURM_QOS=debug

srun --time=30:00 -n $NUMTS gefs2lbc_para
