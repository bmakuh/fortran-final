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

  ! Variable declarations
  INTEGER:: choice

  ! Prompt the user
  write(*,*) 'Hello and welcome to the linear algebra solver!'
  write(*,*) 'What would you like to solve today? Enter the number for your choice'
  write(*,*) '1 Calculate a determinate of a square matrix'
  write(*,*) '2 Add two matrices together'
  write(*,*) '3 Multiply two matrices together'
  write(*,*) '4 Subtract two matrices together'

  read(*,*) choice

  do
    if (choice > 0 .and. choice < 5) exit

    write(*,*) 'That is an invalid choice. Please choose again.'
    read(*,*) choice
  end do



end program