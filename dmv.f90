!---------------------------------
! This program profiles the performance of a dense matrix-vector
! multiply operation for a matrix of linear dimension N.
!  This code will performa a matrix-vector multiply of the usual type:
!    y = A*x
!  but using the Compressed Sparse Row storage for the matrix.
!  The result will be compared to that of a dense matrix.

program sparseMVM
  implicit none

  ! N: the integer dimesion of the full matrix
  integer :: N
  ! A: the matrix
  double precision, allocatable :: A(:,:)
  ! x: source vector that will multiply A
  ! y: result vector
  double precision, allocatable :: x(:), y(:)
  ! arg: command line argument that will be N
  character (len=32):: arg
  integer :: status
  integer :: i,j,k,h
  double precision :: S,E,MFLOPS,NFLOPS,tot
  ! R: number of repitions for a given N
  integer :: R
  ! num: number of different N's to do
  integer :: num=100

  !call get_command_argument(1,arg,status)
  !read(arg, '(I10)') N
  do h = 1, num
     N = int(10.**(5./num*h))
     R = int(1e5/N)

     ! initMatrix fills the matrix in a pentadiagonal fashion
     call initMatrix(N)

     ! Now Initialize the source and result vectors
     allocate(x(N))
     allocate(y(N))
     y = 0.0
     x = 2.0

     ! Now time the matrix-vector multiply
     call get_walltime(S)
     do k = 1, R
        do j = 1,N
           tot = 0.0
           do i = 1, N
              tot = tot + A(i,j) * x(i)
           end do
           y(j) = tot
        end do
        call dummy(y(j))
     end do
     call get_walltime(E)
     ! Compute the mega-FLOPS!
     NFLOPS = 2.0*N*N
     MFLOPS = R*NFLOPS/(E-S)*1.d-6
     print *, N, R, (N+N+N*N)*8.*1.e-6, MFLOPS

     call destroyMatrix()
     ! Now free the source and result vectors
     deallocate(x)
     deallocate(y)

  end do

contains

  subroutine initMatrix(N)
    implicit none
    integer, intent(IN) :: N
    allocate(A(N,N))

    A = 0.0
    do j = 1, N
       do i = 1, N
         ! This could be any value.  Or random.
         A(N,N) = 2.0
       end do
    end do
  end subroutine initMatrix

  subroutine destroyMatrix()
    deallocate(A)
  end subroutine destroyMatrix

end program sparseMVM
