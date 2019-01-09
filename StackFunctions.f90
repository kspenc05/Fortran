
module StackFunctions

    implicit none

    type :: mazePos !The stack is an array of this type
        integer :: Xpos
        integer :: Ypos
        Character :: lastMove !Will consist of: 'E', 'W', 'N', 'S' for all four directions
    end type


    contains !All the subroutines/functions for the stack (aka 'filePath')
            !the stack being used to contain all path positions, and previous moves

    !The two functions I am using for this stack are the basic two functions of
    !Push and Pop, the stack is a sequential one, and does not require dynamic memory

    !Moves each position down one, then inserts at the top of stack

subroutine Push (Xpos, Ypos, lastMove, filePath)

    Integer, intent (in) :: Xpos, Ypos
    Character, intent (in) :: lastMove
    Integer :: i
    TYPE(mazePos) :: filePath(100)

    do i = 99, 1, -1
        filePath(i+1) = filePath(i)
    end do

    filePath(1)%Xpos = Xpos
    filePath(1)%Ypos = Ypos
    filePath(1)%lastMove = lastMove

end subroutine Push

    !overwrites the values stored at the top of the stack
    !it then moves what is stored at position (2), to the top,
    !and the values in position (3) to position (2), and so on
    !Note: will work with empty values, so long as the array somewhere is stored in memory

subroutine Pop (filePath)

    Integer :: i
    TYPE(mazePos) :: filePath(100)

    do i = 1, 99
        filePath(i) = filePath(i+1)
    end do

end subroutine Pop

   !Since the stack is a sequential array, it does not need a destroy() function,
   !since the compiler will deal with this memory

end module StackFunctions