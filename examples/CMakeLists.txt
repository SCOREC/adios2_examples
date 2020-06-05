cmake_minimum_required(VERSION 3.13)
project(wdm_coupling VERSION 0.0.1 LANGUAGES CXX C Fortran)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
#set(MPI_CXX_SKIP_MPICXX ON)
#find_package(MPI REQUIRED)

set(MPI_CXX_SKIP_MPICXX ON)
find_package(MPI REQUIRED)
foreach(lang IN ITEMS CXX C Fortran)
  get_property(tmp TARGET MPI::MPI_${lang} PROPERTY INTERFACE_COMPILE_OPTIONS)
  list(TRANSFORM tmp REPLACE "-fexceptions" "$<$<COMPILE_LANGUAGE:CUDA>:SHELL:-Xcompiler >-fexceptions")
  set_property(TARGET MPI::MPI_${lang} PROPERTY INTERFACE_COMPILE_OPTIONS "${tmp}")
  unset(tmp)
endforeach()


#adios2 adds C and Fortran depending on how it was built
find_package(ADIOS2 2.5 REQUIRED)
#find_package(Kokkos 3.0 REQUIRED)
## use pkgconfig since the fftw autoconf install produces
## broken cmake config files
## https://github.com/FFTW/fftw3/issues/130
#find_package(PkgConfig REQUIRED)

set(CMAKE_Fortran_COMPILER gfortran)

include_directories(include)

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)
set (CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/CMake ${CMAKE_MODULE_PATH} )
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

add_subdirectory(src)
add_subdirectory(app1)
