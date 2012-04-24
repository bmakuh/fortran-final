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
  INTEGER:: choice, rows, cols
  INTEGER, DIMENSION(:,:), allocatable:: matrix1, matrix2, result

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

   



end program

! Prints the menu to the user and gets their choice
subroutine printMenu(choice)
  implicit none ! Must explicitely declare all variables

  INTEGER, INTENT(out):: choice

  write(*,*) '1 Calculate a determinate of a square matrix'
  write(*,*) '2 Add two matrices together'
  write(*,*) '3 Multiply two matrices together'
  write(*,*) '4 Subtract two matrices together'

  read(*,*) choice

end subroutine

! Get the dimensions for the matrix and allocate it
subroutine getMatrixDimensions(matrix, rows, cols, isSquare)
  implicit none ! Must explicitely declare all variables

  INTEGER, DIMENSION(:,:), allocatable, INTENT(out):: matrix
  INTEGER, INTENT(out):: rows, cols
  LOGICAL, INTENT(in):: isSquare

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

  INTEGER, INTENT(in):: rows, cols
  INTEGER, DIMENSION(rows, cols), INTENT(out):: matrix
  INTEGER:: i, j, input

  write(*,*) 'Please enter in the integers for the', rows, 'x', cols, 'matrix integers'
  do i = 0, rows
    do j=0, cols
      read(*,*) input
      matrix(i,j) = input 
    end do
  end do

end subroutine






