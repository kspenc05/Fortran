all:
	gfortran StackFunctions.f90 mazeSolver.f90 MazeFunctions.f90 -fcheck=bounds -Wall 
clean:
	rm a.out *.mod 