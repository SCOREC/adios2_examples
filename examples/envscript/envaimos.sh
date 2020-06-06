module use /gpfs/u/software/dcs-spack-install/v0133gcc/lmod/linux-rhel7-ppc64le/gcc/7.4.0-1/
module load gcc/7.4.0/1
module load openmpi/3.1.4-mm5hjuq
module load \
  cmake/3.15.4-mnqjvz6 \
  adios2/2.5.0-rqsvxj4 \
  fftw/3.3.8-b2oxdb5

export CMAKE_PREFIX_PATH=$CMAKE_PREFIX_PATH:~/barn/build-kokkos3-aimos-gnu74/install
export kk=~/barn/kokkos
export OMPI_CXX=$kk/bin/nvcc_wrapper
export OMPI_CC=gcc
export OMPI_FC=gfortran
export CC=gcc
export FC=gfortran

export CUDA_DIR=/usr/local/cuda-10.2/
export PATH=$PATH:${CUDA_DIR}/bin
~
