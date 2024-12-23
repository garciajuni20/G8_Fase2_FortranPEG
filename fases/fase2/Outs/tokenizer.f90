
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
        character(len=:), allocatable :: lexeme_accumulated
        logical :: cicloActivo
        integer :: cursorAux
        integer :: i

        if (cursor > len(input)) then
            allocate( character(len=3) :: lexeme )
            lexeme = "EOF"
            return
        end if

        
                        cicloActivo = .true.
                        cursorAux = cursor  
                        allocate(character(len=0) :: lexeme_accumulated)  
                        do while (cicloActivo)	
                            if ( "ar" == input(cursor:cursor + 1) ) then
                                cursor = cursor + 2
                                lexeme_accumulated = lexeme_accumulated // "ar"
                            else
                                cicloActivo = .false.
                            end if
                        end do
                        if (len(lexeme_accumulated) > 0) then
                            allocate(character(len=len(lexeme_accumulated)) :: lexeme)
                            lexeme = lexeme_accumulated
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