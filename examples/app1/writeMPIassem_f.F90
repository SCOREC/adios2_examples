program helloInsituMPIWriter
    use mpi
    use adios2

    implicit none

    integer(kind=8) ::  shape_dims(0:1), start_dims(0:1), count_dims(0:1)
    integer, dimension(:), allocatable :: myArray
    integer :: wrank, wsize, nproc
    integer :: ierr, i, j, step
    type(adios2_adios):: adios
    type(adios2_io):: io
    type(adios2_variable):: varArray
    type(adios2_engine):: engine
    character(len=256) :: filename ="/gpfs/u/home/MPFS/MPFSshng/scratch/adios2example/examples/"
    character(len=256) :: bpfile, xmlfile

    integer :: npx,npy,posx,posy,offx,offy 
    integer :: ndx,ndy,gdx,gdy

    logical :: rorder= .false.  
    integer :: dimen(2)
    integer :: subcomm(0:1),comm, comm_x,comm_y
    integer :: mype_x,mype_y
    logical   :: remain(0:1),periods(0:1)  

    integer :: steps=2

    xmlfile = trim(filename)//"xmlfile.xml"
    bpfile = "writer.bp"

    ! Launch MPI
    call MPI_Init(ierr)
    call MPI_Comm_rank(MPI_COMM_WORLD, wrank, ierr)
    call MPI_Comm_size(MPI_COMM_WORLD, wsize, ierr)

    npx=wsize/2
    npy=2
 
    gdx=8
    ndx=gdx/npx
    gdy=2
    ndy=gdy/2

    dimen = (/npx,npy/)
    periods = .true.
    call MPI_Cart_create(MPI_COMM_WORLD,2,dimen,periods,rorder,comm,ierr)
    do i=1,2
      remain = .false.
      remain(i-1) = .true.
      call  MPI_Cart_sub(comm,remain,subcomm(i-1),ierr)
    end do
    comm_x=subcomm(0)
    comm_y=subcomm(1)

    call MPI_Comm_rank(subcomm(0),mype_x,ierr)

    call MPI_Comm_rank(subcomm(1),mype_y,ierr)
     
    offx = 0
    offy = mype_y 

    ! Application variables
    allocate( myArray(0:gdx-1))

    myArray = mype_y
!    myArray = wrank*ndx*ndy
!    do j=1,ndy
!        do i=1,ndx
!            myArray(i-1,j-1) = myArray(i-1,j-1) + (j-1)*gdx + i-1
!        end do
!    end do

    print*,"mype_y=",mype_y,"myArray",myArray(:)

    ! Variable dimensions
    shape_dims = (/ int(npx*ndx,8), int(npy,8) /)
    start_dims = (/ int(offx,8), int(offy,8) /)
    count_dims = (/ int(gdx,8), int(1,8) /)

!    shape_dims = (/ npx*ndx, npy*ndy /)
!    start_dims = (/ offx, offy /)
!    count_dims = (/ ndx, ndy /)



    ! Create adios handler passing the communicator, config file, debug mode and error flag
     call adios2_init(adios, xmlfile, comm_y, adios2_debug_mode_on, ierr)

    ! Declare an IO process configuration inside adios, 
    ! Engine choice and parameters for 'writer' come from the config file
    call adios2_declare_io(io, adios, "writer", ierr)
    ! Defines a 2D array variable
    call adios2_define_variable(varArray, io, "writer", adios2_type_integer4, &
                                2, shape_dims, start_dims, count_dims, &
                                adios2_constant_dims, ierr)

    ! Open myVector_f.bp in write mode, this launches an engine
    call adios2_open(engine, io,bpfile, adios2_mode_write, ierr)

    do step = 1, 1
   print*, "step=", step
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
