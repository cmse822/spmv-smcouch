!---------------------------------
! This program profiles the performance of a sparse matrix-vector
! multiply operation for a matrix of linear dimension N.
! The default version is the operate on a pentadiagonal matrix of the form:
!  | x x x 0.........|
!  | x x x x 0.......|
!  | x x x x x 0.....|
!  | 0 x x x x x 0...|
!  | ..0 x x x x x 0 |
!  | ....0 x x x x x |
!  | ......0 x x x x |
!  | ....... 0 x x x |
!  Where 'x's represent non-zero values of the matrix.
!  This code will performa a matrix-vector multiply of the usual type:
!    y = A*x
!  but using the Compressed Sparse Row storage for the matrix.
!  The result will be compared to that of a dense matrix.

program sparseMVM
  implicit none

  ! N: the integer dimesion of the full matrix
  ! N_nz : the integer number of non-zeros in matrix
  integer :: N, N_nz
  ! A: the flattened matrix containing only non-zeros
  double precision, allocatable :: A(:)
  ! row_ptr: is a integer array of pointers to the indices of the start
  !          of new rows in the flattened matrix, A.  Should be of length N.
  ! col_id: integer array of column indicies for values in A.  Length N_nz.
  integer, allocatable :: row_ptr(:), col_id(:)
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
     N = int(10.**(6./num*h))
     R = int(1e6/N)

     ! initMatrix fills the matrix in a pentadiagonal fashion
     call initMatrix(N)

     ! Now Initialize the source and result vectors
     allocate(x(N_nz))
     allocate(y(N_nz))
     y = 0.0
     x = 2.0

     ! Now time the matrix-vector multiply
     call get_walltime(S)
     do k = 1, R
        do j = 1,N
           tot = 0.0
           do i = row_ptr(j), row_ptr(j+1)
              tot = tot + A(i) * x(col_id(i))
           end do
           y(j) = tot
        end do
        call dummy(y(j))
     end do
     call get_walltime(E)
     ! Compute the mega-FLOPS!
     NFLOPS = 2.0*N_nz
     MFLOPS = R*NFLOPS/(E-S)*1.d-6
     print *, N, N_nz, R, MFLOPS

     call destroyMatrix()
     ! Now free the source and result vectors
     deallocate(x)
     deallocate(y)

  end do

contains

  subroutine initMatrix(N)
    implicit none
    integer, intent(IN) :: N
    ! Assume the worst case scenario.  There are at most 5 values per row of the
    ! matrix A.
    allocate(A(5*N))
    allocate(row_ptr(N+1))
    allocate(col_id(5*N))

    A = 0.0
    row_ptr = 0
    col_id = 0

    N_nz = 0
    ! Here, j will the index of the row of the matrix and i will be the
    ! column index
    do j = 1, N
       do i = 1, N
          if (abs(i-j) < 3) then
             N_nz = N_nz + 1
             A(N_nz) = 1.0
             col_id(N_nz) = i
             if (row_ptr(j)==0) row_ptr(j) = N_nz
          end if
       end do
    end do
    row_ptr(N+1) = N_nz
  end subroutine initMatrix

  subroutine destroyMatrix()
    deallocate(A)
    deallocate(row_ptr)
    deallocate(col_id)
  end subroutine destroyMatrix

end program sparseMVM
