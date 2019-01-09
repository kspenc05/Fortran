
!Assignment 1: software for legacy systems with Michael Wirth
!Author: Kent Spence, mail: kspenc05@mail.uoguelph.ca, student number: 0872780
!Date: 07/02/2016
!PURPOSE: Program is used to solve any 40X40 or smaller text maze given from a text file
!Arguments: will require a valid text file (the user will be prompted for this)
!current bugs: if it cannot solve the maze, it will loop forever (trust me)

PROGRAM test

    use StackFunctions     
    use MazeFunctions
    TYPE(mazePos) :: filePath(100)
    CHARACTER (len = 25) :: fileName
    CHARACTER (len = 40) :: line
    CHARACTER :: maze(40,40)
    LOGICAL :: ext
    integer :: i, startX, startY, verticalLim
    i = 1
    
    WRITE(*,*) 'Enter the name of the file or filepath'
    WRITE(*,*) 'where the maze is located'
    WRITE(*,*) 'Note: maze should be smaller than 40X40'
    READ(*,*) fileName
    
    INQUIRE(file = fileName, EXIST = ext) !Checks to make sure the file DOES exist
    
    IF(EXT .EQV. .FALSE.) then
        WRITE(*,*) '<<file input error (fatal): file could not be found<<'
        return
    end if
    
    OPEN(10, file = fileName, status = 'old')
    
    DO while (IOValue .EQ. 0)
        READ(10, *, IOStat = IOValue) line

        line = trim(line)
        do j=1, len(line)
            maze(i,j) = line(j:j)
            
            if(maze(i,j) .EQ. 'O' .OR. maze(i,j) .EQ. 'o') then
                startX = i
                startY = j
            end if
        end do
        
        i = i + 1
        if(i > 40) then
            WRITE(*,*) '<<file input error (fatal): maze bigger than max size: 40X40<<'
            return
        end if
    end DO
    
    verticalLim = i
    Close(10)
    
    call drawMaze(verticalLim, maze)
    maze(startX, startY) = '.'
    call Push(startX, startY, ' ', filePath)  
    
    call solveMaze(filePath, maze)
    call drawMaze(verticalLim, maze)
    call displayMapKey()
end


!PURPOSE: tells the user how to read the outputted maze solution
subroutine displayMapKey()

    WRITE(*,*)
    WRITE(*,*) 'K : indicates the correct path'
    WRITE(*,*) '* : indicates walls of the maze'
    WRITE(*,*) 'X : indicates any dead ends'
    WRITE(*,*) '. : indicates floor of maze'

end subroutine displayMapKey





