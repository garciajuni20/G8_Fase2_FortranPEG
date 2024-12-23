module e
if (cursor <= len(input)) then
    do while (cursor <= len(input) .and. &
              (input(cursor:cursor) == ' ' .or. &
               input(cursor:cursor) == char(9) .or. &
               input(cursor:cursor) == char(10)))
        select case (input(cursor:cursor))
            case (' ')
                lexeme = "whitespace - _"
            case (char(9))
                lexeme = "whitespace - \\t"
            case (char(10))
                lexeme = "whitespace - \\n"
        end select
        cursor = cursor + 1
    end do
end if
end module e