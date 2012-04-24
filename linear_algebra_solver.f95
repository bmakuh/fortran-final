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
  INTEGER:: choice, rows1, cols1, rows2, cols2
  INTEGER, DIMENSION(:,:), allocatable:: matrix1, matrix2, result
  CHARACTER:: run_again

  ! Prompt the user
  write(*,*)
  write(*,*) 'Hello and welcome to the linear algebra solver!'
  write(*,*) 'What would you like to solve today? Enter the number for your choice'

  do

    call printMenu(choice)
    write(*,*)
  
    ! Check to make sure the choice is valid before continuing
    do
      if (choice > 0 .and. choice < 5) exit
  
      write(*,*) 'That is an invalid choice. Please choose again.'
      call printMenu(choice)
      write(*,*)
  
    end do
  
    if (choice == 2 .or. choice == 3) then
  
      ! Get the matrix dimensions and the matrix input for matrix 1 and 2
      write(*,*) 'We will now input the dimensions for matrix 1'
      call getMatrixDimensions(rows1, cols1, .false.)
      write(*,*)
      write(*,*) 'We will now input the dimensions for matrix 2'
      call getMatrixDimensions(rows2, cols2, .false.)
  
      ! Make sure the dimensions match, otherwise get the dimensions again
      do
        if (rows1 == rows2 .and. cols1 == cols2) exit
  
        ! Tell the user that the dimensions don't match
        write(*,*)
        write(*,*) 'Matrix dimensions do not match. Please input the dimensions again.'
  
        write(*,*) 'We will now input the dimensions for matrix 1'
        call getMatrixDimensions(rows1, cols1, .false.)
        write(*,*)
        write(*,*) 'We will now input the dimensions for matrix 2'
        call getMatrixDimensions(rows2, cols2, .false.)
  
      end do
  
      ! Allocate memory for the matrices
      allocate(matrix1(rows1, cols1))
      allocate(matrix2(rows2, cols2))
      allocate(result(rows1, cols1))
  
      ! Get the input for the matrix 
      call getMatrixInput(matrix1, rows1, cols1)
      call getMatrixInput(matrix2, rows2, cols2)
  
      if (choice == 2) then
        ! Add the matrices together
        call addMatrices(rows1, cols1, matrix1, matrix2, result)
  
      else if (choice == 3) then
        ! Subtract the matrices
        call subtractMatrices(rows1, cols1, matrix1, matrix2, result)
      end if
  
      ! Print the result matrix
      call printResultMatrix(result, rows1, cols1)
  
  
      ! Deallocate memory for the matrices
      deallocate(matrix1)
      deallocate(matrix2)
      deallocate(result)
  
    end if

    write(*,*) 'Would you like to try and solve something else? (y or n)'
    read(*,*) run_again

    do 
      if (run_again .eq. 'y' .or. run_again .eq. 'Y' .or. run_again .eq. 'n' .or. run_again .eq. 'N') exit

      write(*,*) 'Invalid choice. Please reinput your choice. (y or n)'
      read(*,*) run_again
    end do

    if (run_again .eq. 'n' .or. run_again .eq. 'N') exit

  end do

   



end program

! Prints the menu to the user and gets their choice
subroutine printMenu(choice)
  implicit none ! Must explicitely declare all variables

  INTEGER, INTENT(out):: choice
  write(*,*) '--------------------------------------------'
  write(*,*) '1 Calculate a determinate of a square matrix'
  write(*,*) '2 Add two matrices together'
  write(*,*) '3 Subtract two matrices together'
  write(*,*) '4 Multiply two matrices together'
  write(*,*) '--------------------------------------------'

  read(*,*) choice

end subroutine

! Get the dimensions for the matrix
subroutine getMatrixDimensions(rows, cols, isSquare)
  implicit none ! Must explicitely declare all variables

  !INTEGER, DIMENSION(:,:), allocatable, INTENT(out):: matrix
  INTEGER, INTENT(out):: rows, cols
  LOGICAL, INTENT(in):: isSquare

  if (isSquare) then
    write(*,*) 'Please enter the number of rows and columns for the square matrix'
    read(*,*) rows

    cols = rows

    !allocate(matrix(rows, rows))
  else
    write(*,*) 'Please enter the number of rows for the matrix'
    read(*,*) rows
    write(*,*) 'Please enter the number of columns for the matrix'
    read(*,*) cols

    !allocate(matrix(rows, cols))
  end if

  !matrix = 0

end subroutine

! Subroutine to get the input for a matrix
subroutine getMatrixInput(matrix, rows, cols)
  implicit none !Must explicitely declare all variables

  INTEGER, INTENT(in):: rows, cols
  INTEGER, DIMENSION(rows, cols), INTENT(out):: matrix
  INTEGER:: i, j, input

  write(*,*)
  write(*,*) 'Please enter in the integers for the', rows, 'x', cols, 'matrix'
  do i = 1, rows
    do j = 1, cols
      write(*,*) 'Please input a value for row', i, 'and column', j
      read(*,*) input
      matrix(i,j) = input 
    end do
  end do

end subroutine

! Takes two matrices and adds them together
subroutine addMatrices(rows, cols, matrix1, matrix2, result)
  implicit none ! Must explictely declare all variables

  INTEGER, INTENT(in):: rows, cols
  INTEGER, DIMENSION(rows, cols), INTENT(in):: matrix1, matrix2
  INTEGER, DIMENSION(rows, cols), INTENT(out):: result
  INTEGER:: i, j

  ! Tell the user we are adding the results
  write(*,*)
  write(*,*) 'Adding together matrix 1 and matrix 2....'

  ! Add the matrices together
  do i = 1, rows
    do j = 1, cols
      result(i,j) = matrix1(i,j) + matrix2(i,j)
    end do  
  end do

end subroutine

! Takes two matrices and subtracts them
subroutine subtractMatrices(rows, cols, matrix1, matrix2, result)
  implicit none ! Must explictely declare all variables

  INTEGER, INTENT(in):: rows, cols
  INTEGER, DIMENSION(rows, cols), INTENT(in):: matrix1, matrix2
  INTEGER, DIMENSION(rows, cols), INTENT(out):: result
  INTEGER:: i, j

  ! Tell the user we are adding the results
  write(*,*)
  write(*,*) 'Subtracting matrix 2 from matrix 1....'

  ! Add the matrices together
  do i = 1, rows
    do j = 1, cols
      result(i,j) = matrix1(i,j) - matrix2(i,j)
    end do  
  end do

end subroutine

! Prints the results from the result matrix
subroutine printResultMatrix(result, rows, cols)
  implicit none ! Must explicitely declare all variables

  INTEGER, INTENT(in):: rows, cols
  INTEGER, DIMENSION(rows, cols), INTENT(in):: result
  INTEGER:: i, j

  write(*,*)
  write(*,*) 'Here is the result matrix'

  ! Loop through and print the results
  do i = 1, rows
    write(*,"(A1)",advance='no') '|'
    do j = 1, cols
      write(*,"(1X,I4,1X)",advance='no') result(i,j)
    end do
    write(*,*) '|'
  end do

end subroutine






