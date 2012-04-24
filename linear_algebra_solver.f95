!--------------------------------------------------------------------------
!  TITLE: CS260 Final Project
!  AUTHOR: Jerel Miller, Ben Makuh, Eric List
!  CLASS: CSCI260A
!  DATE WRITTEN: 4/24/2012
!  LAST REVISION: 4/24/2012
!  DESCRIPTION: A linear algebra solver for fortran final
!  VARIABLES USED:
!    NAME:           TYPE:     COMMENT:
!
!---------------------------------------------------------------------------
program linear_algebra_solver
  implicit none ! Must explicitely declare all variables

  ! Variable declarations
  integer:: choice, rows, cols
  integer, dimension(:,:), allocatable:: matrix1, matrix2, result

  ! Prompt the user
  write(*,*) 'Hello and welcome to the linear algebra solver!'
  write(*,*) 'What would you like to solve today? Enter the number for your choice'
  call printMenu(choice)

  ! Check to make sure the choice is valid before continuing
  do
    if (choice > 0 .and. choice < 5) exit

    write(*,*) 'That is an invalid choice. Please choose again.'
    call printMenu(choice)

  end do

  select case (choice)
    case (1)
      ! call determinant
    case (2)
      ! call matrixAdd
    case (3)
      ! call matrixMult
    case (4)
      ! call matrixSub 
  end select

  write(*,*) "Thank you for using MatrixSolver!"
  write(*,*) "We trust that your experience was enjoyable."
  write(*,*) "Please call 303-273-3000 if you need further assistance."

end program

! Prints the menu to the user and gets their choice
subroutine printMenu(choice)
  implicit none ! Must explicitely declare all variables

  integer, intent(out):: choice

  write(*,*) '1: Calculate a determinant of a square matrix'
  write(*,*) '2: Add two matrices together'
  write(*,*) '3: Multiply two matrices together'
  write(*,*) '4: Subtract one matrix from another'

  read(*,*) choice

end subroutine

! Get the dimensions for the matrix and allocate it
subroutine getMatrixDimensions(matrix, rows, cols, isSquare)
  implicit none ! Must explicitely declare all variables

  integer, dimension(:,:), allocatable, intent(out):: matrix
  integer, intent(out):: rows, cols
  logical, intent(in):: isSquare

  if (isSquare) then
    write(*,*) 'Please enter the number of rows and columns for the square matrix'
    read(*,*) rows

    allocate(matrix(rows, rows))
  else
    write(*,*) 'Please enter the number of rows for the matrix'
    read(*,*) rows
    write(*,*) 'Please enter the number of columns for the matrix'
    read(*,*) cols

    allocate(matrix(rows, cols))
  end if

  matrix = 0

end subroutine

! Subroutine to get the input for a matrix
subroutine getMatrixInput(matrix, rows, cols)
  implicit none !Must explicitely declare all variables

  integer, intent(in):: rows, cols
  integer, dimension(rows, cols), intent(out):: matrix
  integer:: i, j, input

  write(*,*) 'Please enter in the integers for the', rows, 'x', cols, 'matrix integers'
  do i = 0, rows
    do j=0, cols
      read(*,*) input
      matrix(i,j) = input 
    end do
  end do

end subroutine

! matrixAdd takes in two matricies and adds them together
! returns result in matrix1
subroutine matrixAdd(matrix1, matrix2, rows, cols)
  implicit none

  integer, intent(in):: rows, cols
  integer, dimension(rows,cols), intent(out):: matrix1
  integer, dimension(rows,cols), intent(in):: matrix2

  ! perform addition

end subroutine

! matrixMult takes in two matricies A=r1xc1, B=c1xc2 and multiplies them together
! returns result in resultant=r1xc2
subroutine matrixMult(matrix1, matrix2, resultant, r1, c1, c2)
  implicit none

  integer, intent(in):: r1, c1, c2
  integer, dimension(r1,c1), intent(in):: matrix1
  integer, dimension(c1,c2), intent(in):: matrix2
  integer, dimension(r1,c2), intent(out):: resultant

end subroutine






