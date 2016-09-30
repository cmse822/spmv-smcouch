!---------------------------------
! This program profiles the performance of a sparse matrix-vector
! multiply operation for a matrix of linear dimension N.
! The main pogram 'spmv' calls the routine timeit.
! This program using the Compressed Row Storage approach to
! sparse matrices.
! Two parameters are used:
!  N is the total length of the vectors
!  R is the number of repitition to do in the inner loop in order to maintain
!  roughly equivalent runtimes for any N.

program spmv
  implicit none
  ! R: Number of repitions
  ! N: linear dimension of matrix
  integer :: R,N
  integer :: i,j,k,h
  ! num: number of different values of N to compute
  integer, parameter :: num=100
  integer(KIND=4) :: nnzMax
  double precision :: S,E,MFLOPS,NFLOPS,tot
  integer :: nnz, nrows, row
  ! A: the unwrap matrix
  ! x: the source vector
  ! y: the result vector
  double precision, allocatable :: A(:), x(:), y(:)
  integer, allocatable :: ia(:), ja(:)

  ! Now do the matrix-vector multiply, y = A*x
  ! Main outer loop over different N's
  ! This loop's value of vector length N
  N = 2

  nrows = N*N
  nnzMax = 5*nrows
  allocate(A(nnzMax))
  allocate(x(nrows))
  allocate(y(nrows))
  allocate(ia(nrows))
  allocate(ja(nnzMax))
  ! Initialize the source vector
  x = 1.0

  ! Sparsely fill the matrix.  This is essentially a finite
  ! difference approximation to the Laplacian on a NxN mesh
  row = 0
  nnz = 0
  do i = 0, N-1
     do j = 0, N-1
        ia(row) = nnz
        if (i>0) then
           ja(nnz) = row -n
           A(nnz) = -1.0
           nnz = nnz+1
        end if
        if (j>0) then
           ja(nnz) = row - 1
           A(nnz) = -1.0
           nnz = nnz+1
        end if
        ja(nnz) = row
        A(nnz) = 4.0
        nnz = nnz+1
        if (j<n-1) then
           ja(nnz) = row + 1
           A(nnz) = -1.0
           nnz = nnz+1
        end if
        if (i<n-1) then
           ja(nnz) = row + n
           A(nnz) = -1.0
           nnz = nnz+1
        end if
        row = row+1
     end do
  end do
  ia(row) = nnz

print *, nrows, nnz
  ! get_walltime is a C routine linked in
  call get_walltime(S)
  ! inner loop over vector of length N
  do row=0, nrows-1
     tot = 0.0
     do i=ia(row), ia(row+1)
        tot = tot + A(i) * x(ja(i))
     end do
     y(row) = tot
  end do

  ! This condition call to dummy is included to avoid the obvious
  ! compiler optimization of removing the outer loop
  !call dummy(y(row))
  call get_walltime(E)
  ! Compute the mega-FLOPS performed
  NFLOPS = N*N*1.0 ! Figure out the right number!
  MFLOPS = 1*NFLOPS*8.d0/((E-S)*1.d6)
  print *, N, nnz, nrows, MFLOPS

  ! wind down
  deallocate(A)
  deallocate(x)
  deallocate(y)
  deallocate(ia)
  deallocate(ja)

end program spmv
