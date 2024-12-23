!npm run bootstrap

!gfortran -c tokenizer.f90
!gfortran -o muestra parser.f90 tokenizer.f90

! suma
! = "prueba"i
! / "123" 'hola'

program parser
    use tokenizer
    implicit none

    character(len=*), parameter :: input = "1"
    integer :: cursor
    character(len=:), allocatable :: lexeme

    cursor = 1

    do
        lexeme = nextSym(input, cursor)

        if (lexeme == "EOF") then
            print *, lexeme
            exit
        end if

        if (lexeme == "ERROR") then
            exit
        end if

        print *, lexeme
    end do

end program parser
