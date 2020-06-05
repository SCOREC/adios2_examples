program helloInsituMPIWriter
    use helloInsituArgs
    use mpi
    use adios2

    implicit none
!    include "mpif.h"   

    integer(kind=8), dimension(2) :: shape_dims, start_dims, count_dims
    real, dimension(:,:), allocatable :: myArray
    integer :: wrank, wsize, rank, nproc
    integer :: ierr, i, j, step
    type(adios2_adios):: adios
    type(adios2_io):: io
    type(adios2_variable):: varArray
    type(adios2_engine):: engine
    integer :: comm, color
    character(len=256) :: filename = "/gpfs/u/home/MPFS/MPFSshng/barn/adios2_examples/examples/"
    character(len=256) :: bpfile, xmlfile
    xmlfile = trim(filename)//"xmlfile.xml"
    bpfile = trim(filename)//"myVector_f.bp"

    integer :: npx,npy,posx,posy,offx,offy,ndx,ndy,gdx,gdy

    integer :: rorder=0  
    integer :: dimen(2), periods(2)
    integer :: subcomm[2],remain[2],comm, comm_x,commm_y
    integer :: mype_x,mype_y 
    integer :: i,j

    integer :: steps=2

    ! Launch MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, wrank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, wsize, ierr)

    npx=wsize
    npy=1
 
    gdx=8
    ndx=gdx/npx
    gdy=2
    ndy=gdy

    dimen = (/npx,npy/)
    periods = (/0,0/)
    MPI_Cart_create(MPI_COMM_WORLD,3,dimen,periods,rorder,comm,ierr)
    do i=1,2
      remain = (/0,0/)
      remain[i] = 1
      MPI_Cart_sub(comm,remain,subcomm[i],ierr)
    end do

    comm_x=subcomm[1]
    comm_y=subcomm[2]
    MPI_Comm_rank(mype_x,subcomm[1],ierr)
    MPI_Comm_rank(mype_y,subcomm[2],ierr)
     
    offx = mype_x * ndx
    offy = mype_y * ndy

    ! Application variables
    allocate( myArray(gdx,ndy) )
    myArray = rank*gdx*ndy 
    do j=1,ndy
        do i=1,gdx
            myArray(i,j) = myArray(i,j) + (j-1)gdx + i-1
        end do
    end do

    ! Variable dimensions
    shape_dims = (/ npx*ndx, npy*ndy /)
    start_dims = (/ offx, offy /)
    count_dims = (/ ndx, ndy /)

    ! Create adios handler passing the communicator, config file, debug mode and error flag
    call adios2_init(adios, xmlfile, comm_y, adios2_debug_mode_on, ierr)
    ! Declare an IO process configuration inside adios, 
    ! Engine choice and parameters for 'writer' come from the config file
    call adios2_declare_io(io, adios, "writer", ierr)
    ! Defines a 2D array variable
    call adios2_define_variable(varArray, io, "myArray", adios2_type_real, &
                                2, shape_dims, start_dims, count_dims, &
                                adios2_constant_dims, ierr)

    ! Open myVector_f.bp in write mode, this launches an engine
    call adios2_open(engine, io,"bpfile", adios2_mode_write, ierr)

    do step = 1, steps
!        do j=1,ndy
!            do i=1,ndx
!                myArray(i,j) = myArray(i,j) + 0.01
!            end do
!        end do

        call adios2_begin_step(engine, ierr)
        call adios2_put(engine, varArray, myArray, ierr)
        call adios2_end_step(engine, ierr)

        ! sleep(sleeptime)
    end do

    ! Closes engine and deallocates it, becomes unreachable
    call adios2_close(engine, ierr)

    ! Deallocates adios and calls its destructor
    call adios2_finalize(adios, ierr)

    if( allocated(myArray) ) deallocate(myArray)

    call MPI_Finalize(ierr)

end program helloInsituMPIWriter
