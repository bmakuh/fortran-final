!--------------------------------------------------------------------------
!  TITLE: CS260 Final Project
!  AUTHOR: Jerel Miller, Ben Makuh, Eric List
!  CLASS: CSCI260A
!  DATE WRITTEN: 4/24/2012
!  LAST REVISION: 4/24/2012
!  DESCRIPTION: A linear algebra solver for fortran final
!  VARIABLES USED:
!    NAME:           TYPE:     COMMENT:
!    choice          integer   What operation does the user want to perform?
!    determinant     integer   A function that calculates the determinant of a matrix
!    rows1           integer   stores row dimension of first matrix
!    cols1           integer   stores col dimension of first matrix
!    rows2           integer   stores row dimension of second matrix
!    cols2           integer   stores col dimension of second matrix
!    matrix1         array     the first matrix
!    matrix2         array     the second matrix (if applicable)
!    result          array     the resultant matrix
!    run_again       character does the user want to calculate something else?
!
!---------------------------------------------------------------------------
program linear_algebra_solver
  implicit none ! Must explicitely declare all variables

  ! Variable declarations
  INTEGER:: choice, rows1, cols1, rows2, cols2, int_result, determinant
  INTEGER, DIMENSION(:,:), allocatable:: matrix1, matrix2, result
  CHARACTER:: run_again

  ! Prompt the user
  write(*,*)
  write(*,*) 'Hello and welcome to the MatrixSolver!'
  write(*,*) 'What would you like to solve today? Enter the number for your choice'

  ! Loop through until the user doesn't want to do any more with the matrix solver,
  do

    call printMenu(choice)
    write(*,*)
  
    ! Check to make sure the choice is valid before continuing
    do
      if (choice > 0 .and. choice < 6) exit
  
      write(*,*) 'That is an invalid choice. Please choose again.'
      call printMenu(choice)
      write(*,*)
  
    end do

    if (choice == 1) then

      ! Prompt the user
      write(*,*) 'We will now calculate the determinant of a 2x2 or 3x3 matrix.'
  
      ! Get the dimensions
      call getMatrixDimensions(rows1, cols1, .true.)

      do
        if (rows1 < 4 .and. rows1 > 1) exit

        write(*,*) 'Sorry. We can only calculate the determinant of 2x2 and 3x3 matrices. Please reinput the dimensions.'
        call getMatrixDimensions(rows1, cols1, .true.)
        
      end do

      ! Allocate memory for the matrix
      allocate(matrix1(rows1, rows1))

      ! Get the input for the matrix
      call getMatrixInput(matrix1, rows1, rows1)

      ! Calculate the determinate
      int_result = determinant(matrix1, rows1)

      write(*,*) 'The determinant is:', int_result

      ! Deallocate the matrix
      deallocate(matrix1)
  
    ! Add or subtract a matrix
    else if (choice == 2 .or. choice == 3) then
  
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
  
        write(*,*) 'Please reinput the dimensions for matrix 1'
        call getMatrixDimensions(rows1, cols1, .false.)
        write(*,*)
        write(*,*) 'Please reinput the dimensions for matrix 2'
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
      else
        ! Subtract the matrices
        call subtractMatrices(rows1, cols1, matrix1, matrix2, result)
      end if
  
      ! Print the result matrix
      call printResultMatrix(result, rows1, cols1)
  
  
      ! Deallocate memory for the matrices
      deallocate(matrix1)
      deallocate(matrix2)
      deallocate(result)

    ! Matrix Multiplication
    else if (choice == 4) then

      ! Get the matrix dimensions and the matrix input for matrix 1 and 2
      write(*,*) 'We will now input the dimensions for matrix 1'
      call getMatrixDimensions(rows1, cols1, .false.)
      write(*,*)
      write(*,*) 'We will now input the dimensions for matrix 2'
      call getMatrixDimensions(rows2, cols2, .false.)

      ! Make sure the number of columns of matrix 1 match the number of rows of matrix 2
      do
        if (cols1 == rows2) EXIT

        write(*,*)
        write(*,*) 'Sorry. The number of columns of matrix 1 must match the number of rows of matrix 2'

        write(*,*) 'Please reinput the dimensions for matrix 1'
        call getMatrixDimensions(rows1, cols1, .false.)
        write(*,*)
        write(*,*) 'Please reinput the dimensions for matrix 2'
        call getMatrixDimensions(rows2, cols2, .false.)

      end do

      ! Allocate memory for the matrices
      allocate(matrix1(rows1, cols1))
      allocate(matrix2(rows2, cols2))
      allocate(result(rows1, cols2))

      ! Get the input for the matrix 
      call getMatrixInput(matrix1, rows1, cols1)
      call getMatrixInput(matrix2, rows2, cols2)

      ! Multiply the matrices
      call multiplyMatrices(matrix1, rows1, cols1, matrix2, rows2, cols2, result)

      ! Print the result matrix
      call printResultMatrix(result, rows1, cols2)

      ! Deallocate the matrices
      deallocate(matrix1)
      deallocate(matrix2)
      deallocate(result)

    ! Scalar multiplication
    else if (choice == 5) then

      ! Get the matrix dimensions
      call getMatrixDimensions(rows1, cols1, .false.)

      ! Allocate memory for the matrix
      allocate(matrix1(rows1, cols1))

      ! Get the input for the matrix
      call getMatrixInput(matrix1, rows1, cols1)

      ! Do the scalar multiplication
      call scalarMultiplyMatrix(matrix1, rows1, cols1)

      ! Print the result matrix
      call printResultMatrix(matrix1, rows1, cols1)

      ! Deallocate the matrix
      deallocate(matrix1)
  
    end if



    ! Ask the user if they would like to solve something else
    write(*,*) 'Would you like to try and solve something else? (y or n)'
    read(*,*) run_again

    ! Make sure the user made a valid choice
    do 
      if (run_again .eq. 'y' .or. run_again .eq. 'Y' .or. run_again .eq. 'n' .or. run_again .eq. 'N') exit

      write(*,*) 'Invalid choice. Please reinput your choice. (y or n)'
      read(*,*) run_again
    end do

    ! Exit if they are done
    if (run_again .eq. 'n' .or. run_again .eq. 'N') exit

  end do


  write(*,*) "Thank you for using MatrixSolver!"
  write(*,*) "We trust that your experience was enjoyable."
  write(*,*) "Please call 303-273-3000 if you need further assistance."

end program

! Prints the menu to the user and gets their choice
subroutine printMenu(choice)
  implicit none ! Must explicitely declare all variables

  integer, intent(out):: choice

  write(*,*) '--------------------------------------------'
  write(*,*) '1: Calculate a determinant of a 2x2 or a 3x3 matrix'
  write(*,*) '2: Add two matrices together'
  write(*,*) '3: Subtract one matrix from another'
  write(*,*) '4: Multiply two matrices together'
  write(*,*) '5: Multiply a matrix by a scalar'
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

  integer, intent(in):: rows, cols
  integer, dimension(rows, cols), intent(out):: matrix
  integer:: i, j, input

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

! Calculate the determinant of a matrix
integer function determinant(matrix, rows)
  implicit none
  
  INTEGER:: rows
  INTEGER, DIMENSION(rows, rows):: matrix
  INTEGER, DIMENSION(2, 2):: temp_matrix
  INTEGER:: int_result, determinant2x2, product, i, j, k, sum, col_num

  ! Explicitely reset this to prevent any magic numbers if this is called more than once
  sum = 0
  product = 1

  ! Tell the user we are calculating the determinate
  write(*,*)
  write(*,*) 'Now calculating the determinant....'
  write(*,*)

  ! If this is a 2x2 matrix, send it to the determinate2x2 function
  if (rows == 2) then
    determinant = determinant2x2(matrix)

  else 

    ! Loop through the 3 x 3 matrix and calculate each subdeterminant
    do i = 1, 3

      ! Loop through and get the 2x2 submatrix
      do j = 1, 2
        do k = 1, 3
          ! Skip this iteration if k = i
          if (k == i) cycle

          ! The column for the temp matrix will be k -1
          if (i == 1) then
            col_num = k - 1
          ! The column for the temp matrix will be k when k is 1 and k+1 when k is 2
          else if (i == 2) then
            if(k == 1) then
              col_num = k
            else
              col_num = k - 1
            end if
          ! The column for the temp matrix will be k
          else if (i == 3) then
            col_num = k
          end if

          ! Put the appropriate value in the temp matrix
          temp_matrix(j, col_num) = matrix(j + 1, k)
        end do
      end do

      product = matrix(1, i) * (-1)**(1 + i) * determinant2x2(temp_matrix)
      sum = sum + product

    end do

    ! The determinant is equal to the sum 
    determinant = sum

  end if

  return

end function

! Calculate the determinate of a 2x2 matrix
integer function determinant2x2(matrix)
  implicit none
  
  INTEGER, DIMENSION(2,2):: matrix
  INTEGER:: product

  determinant2x2 = (matrix(1,1) * matrix(2,2)) - (matrix(1, 2) * matrix(2, 1))

  return

end function

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

! Takes two matrices and multplies them
subroutine multiplyMatrices(matrix1, rows1, cols1, matrix2, rows2, cols2, result)
  implicit none ! Must explicitely declare all variables

  INTEGER, INTENT(in):: rows1, cols1, rows2, cols2
  INTEGER, DIMENSION(rows1, cols1), INTENT(in):: matrix1
  INTEGER, DIMENSION(rows2, cols2), INTENT(in):: matrix2
  INTEGER, DIMENSION(rows1, cols2), INTENT(out):: result
  INTEGER:: i, j, k, sum = 0

  ! Tell the user we are multiplying the results
  write(*,*)
  write(*,*) 'Multiplying matrix 1 and matrix 2....'

  ! Multiply the matrices together
  do i = 1, rows1
    do j = 1, cols2
      sum = 0
      do k = 1, cols1
        sum = sum + matrix1(i,k) * matrix2(k,j)
      end do
      result(i,j) = sum
    end do
  end do

end subroutine

subroutine scalarMultiplyMatrix(matrix, rows, cols)
  implicit none ! Must explicitely declare all variables
  
  INTEGER, INTENT(in):: rows, cols
  INTEGER, DIMENSION(rows, cols), INTENT(out):: matrix
  INTEGER:: i, j, scalar

  ! Get the scalar multiplier
  write(*,*) 'Please enter the scalar integer that you would like to multiply the matrix with'
  read(*,*) scalar

  write(*,*)
  write(*,*) 'Applying the scalar to the matrix....'

  ! Multiply the scalar with the matrix
  do i = 1, rows
    do j = 1, cols
      matrix(i,j) = scalar * matrix(i,j)
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






