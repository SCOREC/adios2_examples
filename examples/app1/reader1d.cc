#include <iostream>
#include <mpi.h>
#include <vector>
#include <adios2.h>
#include <string>

  typedef unsigned long GO;

int main(int argc,char *argv[])
{

  MPI_Init(&argc,&argv);
  int wrank,wsize;
  MPI_Comm_rank(MPI_COMM_WORLD, &wrank);
  MPI_Comm_size(MPI_COMM_WORLD, &wsize);
  
  int* myArray;
  int ndims;
  std::string filename="/gpfs/u/home/MPFS/MPFSshng/scratch/adios2example/examples/";
  std::string  bpfile,xmlfile;

  xmlfile = filename+"xmlfile.xml";
  bpfile = "writer.bp";  
  
  int npx,npy,posx,posy,offx,offy;
  int ndx,ndy,gdx,gdy;
  int mype_x,mype_y;
  MPI_Comm subcomm[2],comm,comm_x,comm_y; 
  
  int dimen[2]; 
  int rorder = 0;
  int periods[]={1,1};
  
  npx=wsize;
  npy=1;
  
  gdx=8;
  ndx=gdx/npx;
  gdy=2;
  ndy=gdy;
  
  dimen[0]=npx;
  dimen[1]=npy;
  MPI_Cart_create(MPI_COMM_WORLD,2,dimen,periods,rorder,&comm);
  for(int i=0;i<2;i++){
    int remain[2]={0,0};
    remain[i]=1;
    MPI_Cart_sub(comm,remain,&subcomm[i]);
  }
  comm_x=subcomm[0];
  comm_y=subcomm[1];
  MPI_Comm_rank(comm_x,&mype_x);
  MPI_Comm_rank(comm_y,&mype_y);

  offx=mype_x*ndx;
  offy=0;

   myArray=new int[ndx*2];
 
   unsigned long shape_dims[2] ={GO(npy*ndy),GO(npx*ndx)};
   unsigned long start_dims[2] ={GO(offy),GO(offx)};
   unsigned long count_dims[2] ={GO(ndy),GO(ndx)};

   adios2::ADIOS adios(xmlfile, comm_x); 
  
   adios2::IO io=adios.DeclareIO("reader");


   adios2::Engine bpReader=io.Open(bpfile,adios2::Mode::Read,comm_x);

   for(int i=0;i<1;i++){   
     bpReader.BeginStep();
   adios2::Variable<int> varArray =io.InquireVariable<int>("writer");
   adios2::Dims my_start({start_dims[0],start_dims[1]});
   adios2::Dims my_count({count_dims[0],count_dims[1]});

std::cout<<my_start[0]<<" "<<my_start[1]<<" "<<my_count[0]<<" "<<my_count[1]<<'\n';

//   adios2::Box<adios2::Dims> sel(my_start,my_count);


//   varArray.SetSelection(sel);
   varArray.SetSelection({my_start,my_count});


     bpReader.Get<int>(varArray,myArray,adios2::Mode::Sync);
     bpReader.EndStep();
   }   
   bpReader.Close();
   for(int i=0;i<ndx;i++){
      std::cout<<myArray[i]<<" ";
   }
//   adios2_finalize();

   MPI_Finalize();

  }








