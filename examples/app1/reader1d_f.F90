program reader1d_f 
    use mpi
    use adios2

    implicit none

    type(adios2_adios):: adios
    type(adios2_io):: io
    type(adios2_variable):: varArray
    type(adios2_engine):: engine

    integer :: wrank, wsize, nproc
    integer, dimension(:), allocatable :: myArray
    integer :: ndims
 !   integer(kind=4), dimension(:), allocatable :: shape_dims
 !   integer(kind=4), dimension(:), allocatable :: sel_start, sel_count
    integer(kind=8) :: shape_dims(0:1),start_dims(0:1),count_dims(0:1)
    integer :: ierr
    integer :: i, j, step

    character(len=256) :: filename = "/gpfs/u/home/MPFS/MPFSshng/scratch/adios2example/examples/"
    character(len=256) :: bpfile, xmlfile

    integer :: npx,npy,posx,posy,offx,offy
    integer :: ndx,ndy,gdx,gdy

    logical :: rorder = .false.
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

    npx=wsize
    npy=1

    gdx=8
    ndx=gdx/npx
    gdy=2
    ndy=gdy

    dimen = (/npx,npy/)
    periods = (/.true.,.true./)
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

    offx = mype_x * ndx
    offy = mype_y * ndy

    allocate(myArray(0:ndx*ndy-1)) 

    shape_dims = (/ int(npx*ndx,8), int(npy*ndy,8) /)
    start_dims = (/ int(offx,8), int(offy,8) /)
    count_dims = (/ int(ndx,8), int(ndy,8) /)

!    shape_dims = (/ npx*ndx, npy*ndy /)
!    start_dims = (/ offx, offy /)
!    count_dims = (/ ndx, ndy /)



    ! Start adios2
    call adios2_init( adios, xmlfile, comm_x, adios2_debug_mode_on, ierr )

    ! Declare an IO process configuration inside adios,
    ! Engine choice and parameters for 'writer' come from the config file
    call adios2_declare_io( io, adios, 'reader', ierr )

    call adios2_open( engine, io, bpfile , adios2_mode_read, ierr)

    if( ierr == adios2_found ) then
        
        do j=1,1
            call adios2_begin_step(engine, ierr)
            if (ierr /= adios2_step_status_ok) then
                exit
            endif

            call adios2_inquire_variable( varArray, io, 'writer', ierr )

            call adios2_set_selection( varArray, 2,start_dims,count_dims, ierr )
            call adios2_get( engine, varArray, myArray,ierr )

            call adios2_end_step(engine, ierr)

     print*,"mype_x=",mype_x,"myArray",myArray(:)

        !    call print_array(myArray, sel_start, rank, step)

!            step = step + 1
!            if(step>3) then
!              stop
!            end if
        end do


        if( allocated(myArray) ) deallocate(myArray)

    else
        write(*, '("Variable myArray not found in stream! ierr=",i0)') ierr
    end if

    call adios2_close( engine, ierr )
    call adios2_finalize(adios, ierr)
    call MPI_Finalize(ierr)

contains

subroutine print_array(xy,offset,rank, step)
    implicit none
    include 'mpif.h'
    real,    dimension(:,:), intent(in) :: xy
    integer*8, dimension(2),   intent(in) :: offset
    integer,   intent(in)                 :: rank, step

    integer :: size1,size2
    integer :: i,j

    size1 = size(xy,1)
    size2 = size(xy,2)

    write (100+rank, '("rank=",i0," size=",i0,"x",i0," offsets=",i0,":",i0," step=",i0)') &
        rank, size1, size2, offset(1), offset(2), step
    write (100+rank, '(" time   row   columns ",i0,"...",i0)') offset(2), offset(2)+size2-1
    write (100+rank, '("        ",$)')
    do j=1,size2
        write (100+rank, '(i9,$)') offset(2)+j-1
    enddo
    write (100+rank, '(" ")')
    write (100+rank, '("--------------------------------------------------------------")')
    do i=1,size1
        write (100+rank, '(2i5,$)') step,offset(1)+i-1
        do j=1,size2
            write (100+rank, '(f9.2,$)') xy(i,j)
        enddo
        write (100+rank, '(" ")')
    enddo

end subroutine print_array

end program 
