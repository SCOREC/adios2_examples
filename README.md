## build on scorec rhel7

```
module unuse /opt/scorec/spack/lmod/linux-rhel7-x86_64/Core
module use /opt/scorec/spack/dev/lmod/linux-rhel7-x86_64/Core
module load gcc mpich adios2 cmake/3.15.4-4m6f3eu
git clone git@github.com:SCOREC/adios2_examples.git
# or
# git clone https://github.com/SCOREC/adios2_examples.git
mkdir buildAdios2Examples
cd !$
cmake ../adios2_examples \
-DCMAKE_CXX_COMPILER=g++ \
-DCMAKE_Fortran_COMPILER=gfortran
make -j4
```
