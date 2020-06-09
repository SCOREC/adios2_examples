#include <iostream>
#include <mpi.h>
#include <vector>
#include <adios2.h>

int main(&argc,char *argv[])
{
  MPI_Init(&argc,&argv);
  int rank,size;
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);
  MPI_Comm_size(MPI_COMM_WORLD, &size);
  
 




}
