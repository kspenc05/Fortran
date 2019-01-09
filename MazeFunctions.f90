
Module MazeFunctions

    contains !all the required functions to solve or print the maze
    
    ! SolveMaze Is called in order to find a solution to the maze
    !Constantly evaluates if it can move right, left up or down as in,
    !by evaluating "is that an avilable dot to travel to '.', or the end of
    !the maze 'e'?"
    !If the maze is solved, it then prints the correct path to the screen
    !using the letter 'K'
    !arguments: the filePath (the stack to store the correct path to), and the maze character array
    
subroutine solveMaze(filePath, maze)

    use StackFunctions
    TYPE(mazePos) :: filePath(100)
    Character :: maze(40,40)
    Character :: deadEnd
    
    do while ((maze(filePath(1)%Xpos, filePath(1)%Ypos)) /= 'e')
        
        if (maze(filePath(1)%Xpos+1, filePath(1)%Ypos) == 'e') then
            call Push(filePath(1)%Xpos+1, filePath(1)%Ypos, 'W', filePath)
            
        else if(maze(filePath(1)%Xpos-1, filePath(1)%Ypos) == 'e') then
            call Push(filePath(1)%Xpos-1, filePath(1)%Ypos, 'W', filePath)
            
        else if(maze(filePath(1)%Xpos, filePath(1)%Ypos+1) == 'e') then
            call Push(filePath(1)%Xpos, filePath(1)%Ypos+1, 'W', filePath)
        
        else if(maze(filePath(1)%Xpos, filePath(1)%Ypos-1) == 'e') then
            call Push(filePath(1)%Xpos, filePath(1)%Ypos-1, 'W', filePath)
    
        else if ((maze(filePath(1)%Xpos, filePath(1)%Ypos+1)) == '.' .AND. deadEnd /= 'R') then

            if(filePath(1)%lastMove == 'L') then  
                if (maze(filePath(1)%Xpos+1, filePath(1)%Ypos) /= '.') then
                    if (maze(filePath(1)%Xpos-1, filePath(1)%Ypos) /= '.') then
                        if (maze(filePath(1)%Xpos, filePath(1)%Ypos-1) /= '.') then
                            call fillDeadEnds(filePath, maze, 'R')
                        end if
                    end if
                end if
            end if
                
            call Push(filePath(1)%Xpos, filePath(1)%Ypos+1, 'R', filePath)
            
            if(filePath(2)%lastMove == 'L') then
                deadEnd = 'R'
                    
            else if(deadEnd == 'U' .OR. deadEnd == 'D') then
                deadEnd = ' '
            end if
            
        else if ((maze(filePath(1)%Xpos+1, filePath(1)%Ypos)) == '.' .AND. deadEnd /= 'D') then

            if(filePath(1)%lastMove == 'U') then  
                if (maze(filePath(1)%Xpos-1, filePath(1)%Ypos) /= '.') then !if it can only move down
                    if (maze(filePath(1)%Xpos, filePath(1)%Ypos+1) /= '.') then !but moved Up last
                        if (maze(filePath(1)%Xpos, filePath(1)%Ypos-1) /= '.') then
                            call fillDeadEnds(filePath, maze, 'D')
                        end if
                    end if
                end if
            end if
            
            call Push(filePath(1)%Xpos+1, filePath(1)%Ypos, 'D', filePath)
            
            if(filePath(2)%lastMove == 'U') then
                deadEnd = 'D'
                    
            else if(deadEnd == 'R' .OR. deadEnd == 'L') then
                deadEnd = ' '
            end if
            
        else if ((maze(filePath(1)%Xpos, filePath(1)%Ypos-1)) == '.' .AND. deadEnd /= 'L') then

            if(filePath(1)%lastMove == 'R') then    
                if (maze(filePath(1)%Xpos+1, filePath(1)%Ypos) /= '.') then
                    if (maze(filePath(1)%Xpos, filePath(1)%Ypos+1) /= '.') then
                        if (maze(filePath(1)%Xpos-1, filePath(1)%Ypos) /= '.') then
                            call fillDeadEnds(filePath, maze, 'L')
                        end if
                    end if
                end if
            end if
            
            call Push(filePath(1)%Xpos, filePath(1)%Ypos-1, 'L', filePath)
    
            if(deadEnd == 'D' .OR. deadEnd == 'U') then
                deadEnd = ' '
            end if
        
        else if ((maze(filePath(1)%Xpos-1, filePath(1)%Ypos)) == '.' .AND. deadEnd /= 'U') then

            if(filePath(1)%lastMove == 'D') then       
                if (maze(filePath(1)%Xpos+1, filePath(1)%Ypos) /= '.') then
                    if (maze(filePath(1)%Xpos, filePath(1)%Ypos+1) /= '.') then
                        if (maze(filePath(1)%Xpos, filePath(1)%Ypos-1) /= '.') then
                           call fillDeadEnds(filePath, maze, 'U')
                        end if
                    end if
                end if
            end if
               
            call Push(filePath(1)%Xpos-1, filepath(1)%Ypos, 'U', filePath)  
                    
            if(deadEnd == 'R' .OR. deadEnd == 'L') then
                deadEnd = ' '
            end if
        end if
    end do
    
    do while(filePath(1)%lastMove /= ' ')
        call Pop(filePath)
        maze(filePath(1)%Xpos, filePath(1)%Ypos) = 'K'
    end do
    
end subroutine solveMaze
    
    !PURPOSE: This function is used to get rid of any dead ends that may show up in the maze
    !Is typically called when all other spaces except one are not '.'s
    !is given the filePath, maze, and last move
    
subroutine fillDeadEnds(filePath, maze, move)

    use StackFunctions
    TYPE(mazePos) :: filePath(100)
    CHARACTER :: maze(40,40)
    CHARACTER, intent(in) :: move
    
    if(move == 'L' .OR. move == 'R') then
    
        !WRITE(*,*) 'I blocked a square'
        do while (maze(filePath(1)%Xpos-1, filePath(1)%Ypos) /= '.')
            if(maze(filePath(1)%Xpos+1, filePath(1)%Ypos) == '.') then
                EXIT
            end if
            maze(filePath(1)%Xpos, filePath(1)%Ypos) = 'X'
            call Pop(filePath)
        end do
        
    else if(move == 'D' .OR. move == 'U') then
    
        !WRITE(*,*) 'I blocked a square'
        do while (maze(filePath(1)%Xpos, filePath(1)%Ypos+1) /= '.')
            if(maze(filePath(1)%Xpos, filePath(1)%Ypos-1) == '.') then
                EXIT
            end if
            maze(filePath(1)%Xpos, filePath(1)%Ypos) = 'X'
            call Pop(filePath)
        end do
    end if
    
end subroutine fillDeadEnds
    
    !Fortran really needs it's own isAlpha() function, but this is not it!
    !PURPOSE: Checks if the given character is a valid maze character, is used for
    !validating characters before printing out the maze
    
integer function isValidMazeChar(char)

    use StackFunctions
    implicit none
    Integer :: i
    CHARACTER, intent (in) :: char
    CHARACTER (len = 7) :: alphabet
    
    alphabet = '*.OeoXK'
    isValidMazeChar = 0
    
    do i=1, len(alphabet)
        if(char == alphabet(i:i)) then
            isValidMazeChar = 1
            EXIT
        end if
    end do
    return
    
end function isValidMazeChar

    !PURPOSE: draws out the entire maze
    !Needs the vertical limit, and the maze array
    
subroutine drawMaze(verticalLim, maze)

    use StackFunctions
    character :: maze(40,40)
    Integer, intent(in) :: verticalLim
    Integer :: i,j
    WRITE(*,*)
    
    do i = 1, verticalLim-2 !NOTE: The verticalLim is always two longer than required
        WRITE(*,*)          !due to the way variable 'i' is used in the file read
        do j = 1, 40
            if(isValidMazeChar(maze(i,j)) == 1) then 
                WRITE(*, '(A)', advance = "no") maze(i,j) 
            end if
        end do
    end do
    
end subroutine drawMaze

end module MazeFunctions

