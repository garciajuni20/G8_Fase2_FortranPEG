
module tokenizer
    implicit none

contains
    function to_lower(str) result(lower_str)
        character(len=*), intent(in) :: str
        character(len=len(str)) :: lower_str
        integer :: i
        lower_str = str
        do i = 1, len(str)
            if (iachar(str(i:i)) >= iachar('A') .and. iachar(str(i:i)) <= iachar('Z')) then
                lower_str(i:i) = achar(iachar(str(i:i)) + 32)
            end if
        end do
    end function to_lower
    function nextSym(input, cursor) result(lexeme)
        character(len=*), intent(in) :: input
        integer, intent(inout) :: cursor
        character(len=:), allocatable :: lexeme
        character(len=:), allocatable :: entrada_anterior
        integer :: i

        if (cursor > len(input)) then
            allocate( character(len=3) :: lexeme )
            lexeme = "EOF"
            return
        end if

        
	if ("atacar" == input(cursor:cursor + 5)) then 
	    allocate(character(len=6) :: lexeme)
	    allocate(character(len=6) :: entrada_anterior)
	    lexeme = "atacar"
	    entrada_anterior = lexeme
	    cursor = cursor + 6 
		if (":" == input(cursor:cursor + 0)) then 
	    	deallocate(lexeme)
	    	allocate(character(len=7) :: lexeme)
	    	lexeme = entrada_anterior // input(cursor:cursor + 0)
	    	deallocate(entrada_anterior)
	    	entrada_anterior = lexeme
	    	cursor = cursor + 1
    		if (input(cursor:cursor) == Char(32)) then
    	    	deallocate(lexeme)
    	    	allocate(character(len=1) :: lexeme)
    	    	lexeme = entrada_anterior // input(cursor:cursor)
    	    	deallocate(entrada_anterior)
    	    	entrada_anterior = lexeme
    	    	cursor = cursor + 1
				return
			end if
		end if
	end if

    if (to_lower(input(cursor:cursor)) >= 'a' .and. to_lower(input(cursor:cursor)) <= 'j') then
        allocate(character(len=1) :: lexeme)  ! Reservar espacio para el lexema
        lexeme = input(cursor:cursor)        ! Asignar el carácter al lexema
        lexeme = lexeme // " - " // "columna"
        cursor = cursor + 1                  ! Avanzar el cursor
        return
    end if
                

    	if (input(cursor:cursor) == Char(48)) then
    	    allocate(character(len=1) :: lexeme)  ! Reservar espacio para el lexema
    	    lexeme = input(cursor:cursor)        ! Asignar el carácter al lexema
    	    if (.not. allocated(entrada_anterior)) then
    	        allocate(character(len=len(lexeme)) :: entrada_anterior)
    	    end if
    	    entrada_anterior = lexeme
    	    cursor = cursor + 1                  ! Avanzar el cursor
                    
    		if (input(cursor:cursor) >= '1' .and. input(cursor:cursor) <= '9') then
    	    	deallocate(lexeme)
    	    	allocate(character(len=1) :: lexeme)
    	    	lexeme = entrada_anterior // input(cursor:cursor)
    	    	lexeme = lexeme // " - " // "fila"
    	    	deallocate(entrada_anterior)
    	    	entrada_anterior = lexeme
    	    	cursor = cursor + 1
			return
		end if
	end if

    if ("10" == input(cursor:cursor + 1)) then !Foo
        allocate(character(len=2) :: lexeme)
        lexeme = input(cursor:cursor + 1)
        lexeme = lexeme // " - " // "fila"
        cursor = cursor + 2
        return
    end if

        print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
        lexeme = "ERROR"
    end function nextSym
end module tokenizer

module parser
    use tokenizer
    implicit none

contains

    subroutine parse(input)
        character(len=:), intent(inout), allocatable :: input
        character(len=100) :: lexeme
        integer :: cursor

        lexeme = ""
        cursor = 1

        do while (lexeme /= "EOF" .and. lexeme /= "ERROR")
            lexeme = nextSym(input, cursor)
            print *, lexeme
        end do
    end subroutine parse
end module parser

program test
	use parser
	implicit none
	character(len=100) :: filename
	character(len=:), allocatable :: input
	integer :: u, len
	logical :: exists

	if (command_argument_count() == 0) then
		print *, "error: no input file"
		stop
	end if

	call get_command_argument(1, filename)

	inquire(file=filename, exist=exists, size=len)
	if (exists) then
		open (1, file=filename, status='old', action='read', access='stream', form='unformatted')
		allocate (character(len=len) :: input)
        read (1) input
		call parse(input)
	else
		print *, "error: file is not present"
		stop
	end if

	close(u)
end program test