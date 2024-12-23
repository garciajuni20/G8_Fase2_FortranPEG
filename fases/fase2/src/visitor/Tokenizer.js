import Visitor from './Visitor.js';
import { Rango,String,Clase,Character } from './CST.js';
import { CondicionalStrings, LiteralPor } from './Utilidades.js';
export default class Tokenizer extends Visitor {
    generateTokenizer(grammar) {
        return `
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
        character(len=:), allocatable :: lexemeAux
        logical :: cicloActivo
        integer :: cursorAux
        integer :: i

        if (cursor > len(input)) then
            allocate( character(len=3) :: lexeme )
            lexeme = "EOF"
            return
        end if

        ${grammar.map((produccion) => produccion.accept(this)).join('\n')}

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
end program test`;
    }

    visitProducciones(node) {
        try {
            const aliasString = node.alias
            .map(subArray => subArray[1]) 
            .join(''); 
            
            node.alias = aliasString;
            node.expr.alias = node.alias;
        } catch (error) {
            // No existe un alias
            node.alias = node.id;
            node.expr.alias = node.alias;
        }
        return node.expr.accept(this);
    }
    visitOpciones(node) {
        let prueba = node
        // console.log(node.exprs);
        node.exprs.map((node) => node.alias = prueba.alias);
        // node.expr[].alias = node.alias;
        return node.exprs.map((node) => node.accept(this)).join('\n');
    }
    visitUnion(node) {
        let fortran = "";
        if (node.exprs.length == 1){  // Si solo hay una expresión en la gramática pero solo literales
            if (node.exprs[0].expr instanceof String){
                let condicional;
                //switch para el manejo de ?*+
                switch(node.exprs[0].qty){
                    case "*":
                        let texto = LiteralPor(node.exprs[0]) + `
                            else
                                cicloActivo = .false.
                            end if
                        end do
                        if (len(lexemeAux) > 0) then
                            allocate(character(len=len(lexemeAux) + len(" - Literal")) :: lexeme)
                            lexeme = trim(lexemeAux) // " - Literal"
                            return
                        end if`
                        return texto;

                case "+":
                    condicional = CondicionalStrings(node.exprs[0]);
                        return `
                            cursorAux = cursor  
                            cicloActivo = .true.
                            allocate(character(len=0) :: lexemeAux)  
                            if ( ${condicional} ) then
                                cursor = cursor + ${node.exprs[0].expr.val.length}
                                lexemeAux = "${node.exprs[0].expr.val}"
                            else
                                print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
                                allocate(character(len=5) :: lexeme)
                                lexeme = "ERROR"
                                return
                            end if
                            do while (cicloActivo)	
                                if ( ${condicional} ) then
                                    cursor = cursor + ${node.exprs[0].expr.val.length}
                                    lexemeAux = lexemeAux // "${node.exprs[0].expr.val}"
                                else
                                    cicloActivo = .false.
                                end if
                            end do
                            if (len(lexemeAux) > 0) then
                                allocate(character(len=len(lexemeAux)) :: lexeme)
                                lexeme = lexemeAux
                                return
                            end if
                        `
                case "?":
                    condicional = CondicionalStrings(node.exprs[0]);
                    return `
                        cursorAux = cursor  
                        allocate(character(len=0) :: lexemeAux)  
                        if ( ${condicional} ) then
                            cursor = cursor + ${node.exprs[0].expr.val.length}
                            lexemeAux = "${node.exprs[0].expr.val}"
                        end if
                    `
                default:
                    const exprVal = node.exprs[0].expr.val;
                    const exprLength = exprVal.length;
                    const cursorSlice = `input(cursor:cursor + ${exprLength - 1})`;
                    const allocation = `
                        allocate(character(len=${exprLength}) :: lexeme)
                        lexeme = ${cursorSlice}
                        lexeme = lexeme // " - " // "${node.alias}"
                        cursor = cursor + ${exprLength}`;
                
                    condicional = CondicionalStrings(node.exprs[0]);
                
                    return `
                    if (${condicional}) then !Foo
                        ${allocation}
                        return
                    end if`;
                }
                 }else if (node.exprs[0].expr instanceof Clase)     {
            for(const expr of node.exprs[0].expr.chars) {
                var condicion = ''
                if(expr instanceof Rango) {
                    let bot = expr.bottom;
                    let top = expr.top;
                    if (node.exprs[0].expr.isCase == 'i') {
                        // Case insensitive
                        condicion += `${condicion !== '' ? ' .or. ' : ''}to_lower(input(cursor:cursor)) >= '${bot}' .and. to_lower(input(cursor:cursor)) <= '${top}'`
                    } else {
                        // Case sensitive
                        condicion += `${condicion !== '' ? ' .or. ' : ''}input(cursor:cursor) >= '${bot}' .and. input(cursor:cursor) <= '${top}'`
                    }
                } else { // Character
                    if (node.exprs[0].expr.isCase == 'i') {
                        // Case insensitive
                        condicion += `${condicion !== '' ? ' .or. ' : ''}to_lower(input(cursor:cursor)) == Char(${expr.text.charCodeAt(0)})`
                    } else {
                        // Case sensitive
                        condicion += `${condicion !== '' ? ' .or. ' : ''}input(cursor:cursor) == Char(${expr.text.charCodeAt(0)})`
                    }
                }
            }
            console.log(node.exprs[0].qty);
            switch(node.exprs[0].qty){
                case "*":
                    return `
                    cursorAux = cursor    
                    cicloActivo = .true.
                    allocate(character(len=0) :: lexemeAux) 
                    do while (cicloActivo)	 
                        if (${condicion}) then
                            lexemeAux = lexemeAux // input(cursor:cursor) 
                            cursor = cursor + 1   
                            else
                                cicloActivo = .false.
                            end if
                        end do
                        if (len(lexemeAux) > 0) then
                            allocate(character(len=1) :: lexeme)  
                            lexeme = lexemeAux
                            lexeme = lexeme // " - " // "${node.alias}"
                            cursor = cursor + 1                  ! Avanzar el cursor
                            return
                        end if
                        `
                case "+":
                    return `
                    cursorAux = cursor    
                    cicloActivo = .true.
                    allocate(character(len=0) :: lexemeAux) 
                    if ( ${condicional} ) then
                                lexemeAux = lexemeAux // input(cursor:cursor) 
                                cursor = cursor + 1 
                            else
                                print *, "error lexico en col ", cursor, ', "'//input(cursor:cursor)//'"'
                                allocate(character(len=5) :: lexeme)
                                lexeme = "ERROR"
                                return
                            end if
                    do while (cicloActivo)	 
                        if (${condicion}) then
                            lexemeAux = lexemeAux // input(cursor:cursor) 
                            cursor = cursor + 1   
                            else
                                cicloActivo = .false.
                            end if
                        end do
                        if (len(lexemeAux) > 0) then
                            allocate(character(len=1) :: lexeme)  
                            lexeme = lexemeAux
                            lexeme = lexeme // " - " // "${node.alias}"
                            cursor = cursor + 1                  ! Avanzar el cursor
                            return
                        end if
                        `      
                default: 
                return ` 
                if (${condicion}) then
                    allocate(character(len=1) :: lexeme)  ! Reservar espacio para el lexema
                    lexeme = input(cursor:cursor)        ! Asignar el carácter al lexema
                    lexeme = lexeme // " - " // "${node.alias}"
                    cursor = cursor + 1                  ! Avanzar el cursor
                    return
                end if`    
            }
        }
    }

        let repeticiones = 0;
        let total_nodos = node.exprs.length;
        let longitud = 0;
        node.exprs.forEach(element => {
            const tabuladores = "\t".repeat(repeticiones);
            if (element.expr instanceof String) {
                const exprVal = element.expr.val;
                const exprLength = exprVal.length;
                const inputSlice = `input(cursor:cursor + ${exprLength - 1})`;
                const caseInsensitiveCheck = element.expr.isCase === 'i' 
                    ? `to_lower("${exprVal}") == to_lower(${inputSlice})`
                    : `"${exprVal}" == ${inputSlice}`;
        
                // Genera la base del bloque Fortran
                let fortranBase = `
        ${tabuladores}if (${caseInsensitiveCheck}) then`;
        
                if (repeticiones === 0) {
                    // Primera repetición
                    fortran += `${fortranBase}
        \t${tabuladores}allocate(character(len=${exprLength}) :: lexeme)
        \t${tabuladores}allocate(character(len=${exprLength}) :: entrada_anterior)
        \t${tabuladores}lexeme = ${inputSlice}
        \t${tabuladores}entrada_anterior = lexeme
        \t${tabuladores}cursor = cursor + ${exprLength}`;
                    longitud = exprLength;
        
                } else {
                    // Repeticiones intermedias y última
                    longitud += exprLength;
                    const isLastRepetition = repeticiones === total_nodos - 1;
                    const aliasAppend = isLastRepetition ? ` // " - " // "${node.alias}"` : "";
        
                    fortran += `${fortranBase}
        \t${tabuladores}deallocate(lexeme)
        \t${tabuladores}allocate(character(len=${longitud}) :: lexeme)
        \t${tabuladores}lexeme = entrada_anterior // ${inputSlice}${aliasAppend}
        \t${tabuladores}deallocate(entrada_anterior)
        \t${tabuladores}entrada_anterior = lexeme
        \t${tabuladores}cursor = cursor + ${exprLength}`;
                }
        
                repeticiones++;
            }else if (element.expr instanceof Clase){
            var condicion = ''
            for(const expr of element.expr.chars) {
                if(expr instanceof Rango) {
                    let bot = expr.bottom;
                    let top = expr.top;
                    if (element.expr.isCase == 'i') {
                        // console.log("Case insensitive");
                        if (bot < top){
                            condicion += (condicion !== '' ? ' .or. ' : '') + `to_lower(input(cursor:cursor)) >= '${bot}' .and. to_lower(input(cursor:cursor)) <= '${top}'`
                        }
                    } else {
                        // console.log("Case sensitive");
                        if (bot < top){
                            condicion += (condicion !== '' ? ' .or. ' : '') + `input(cursor:cursor) >= '${bot}' .and. input(cursor:cursor) <= '${top}'`
                        }
                    }
                } else if(expr instanceof Character) { // Character
                    if (element.expr.isCase == 'i') {
                        // console.log("Case insensitive");
                        condicion += (condicion !== '' ? ' .or. ' : '') + `to_lower(input(cursor:cursor)) == Char(${expr.text.charCodeAt(0)})`
                    } else {
                        // console.log("Case sensitive");
                        condicion += (condicion !== '' ? ' .or. ' : '') + `input(cursor:cursor) == Char(${expr.text.charCodeAt(0)})`
                    }
                }
                if (repeticiones == 0){
                    // Primera Repeticion
                    fortran += `
    \t${tabuladores}if (${condicion}) then
    \t${tabuladores}    allocate(character(len=1) :: lexeme)  ! Reservar espacio para el lexema
    \t${tabuladores}    lexeme = input(cursor:cursor)        ! Asignar el carácter al lexema
    \t${tabuladores}    if (.not. allocated(entrada_anterior)) then
    \t${tabuladores}        allocate(character(len=len(lexeme)) :: entrada_anterior)
    \t${tabuladores}    end if
    \t${tabuladores}    entrada_anterior = lexeme
    \t${tabuladores}    cursor = cursor + 1                  ! Avanzar el cursor
                    `
                    repeticiones++;
                    // Ultima Repeticion
                }else if (repeticiones == total_nodos - 1) {
                    // console.log("Ultima Repeticion");
                    fortran += `
    \t${tabuladores}if (${condicion}) then
    \t${tabuladores}    deallocate(lexeme)
    \t${tabuladores}    allocate(character(len=1) :: lexeme)
    \t${tabuladores}    lexeme = entrada_anterior // input(cursor:cursor)
    \t${tabuladores}    lexeme = lexeme // " - " // "${node.alias}"
    \t${tabuladores}    deallocate(entrada_anterior)
    \t${tabuladores}    entrada_anterior = lexeme
    \t${tabuladores}    cursor = cursor + 1`
                    repeticiones++;
                    // console.log("Ultima Repeticion 2");
                } else {
                    // Repeticiones intermedias
                    fortran += `
    \t${tabuladores}if (${condicion}) then
    \t${tabuladores}    deallocate(lexeme)
    \t${tabuladores}    allocate(character(len=1) :: lexeme)
    \t${tabuladores}    lexeme = entrada_anterior // input(cursor:cursor)
    \t${tabuladores}    deallocate(entrada_anterior)
    \t${tabuladores}    entrada_anterior = lexeme
    \t${tabuladores}    cursor = cursor + 1`
                    repeticiones++;
                }
            }
        }
        });
        for (let i = repeticiones; i > 0; i--) {
            let tabuladores = "\t".repeat(i);
            if (i >= repeticiones){
                fortran += `
    \t${tabuladores}return`;
            }else{
            fortran += `
\t${tabuladores}end if`;
            }
        }
        let tabuladores = "\t".repeat(repeticiones);
                fortran += `
\tend if`;       
        return fortran;
    }
    visitExpresion(node) {
        return node.expr.accept(this);
    }

    visitString(node) {
    }

    generateCaracteres(chars) {
        if (chars.length === 0) return '';
        return `
if (findloc([${chars
    .map((char) => `"${char}"`)
    .join(', ')}], input(i:i), 1) > 0) then
    lexeme = input(cursor:i)
    cursor = i + 1
    return
end if`;
    }

    visitClase(node) {
        return `
i = cursor
    ${this.generateCaracteres(
        node.chars.filter((node) => typeof node === 'string')
    )}
    ${node.chars
        .filter((node) => node instanceof Rango)
        .map((range) => range.accept(this))
        .join('\n')}
        `;
    }

    visitRango(node) {
        return `
if (input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}") then
    lexeme = input(cursor:i)
    cursor = i + 1
    return
end if
    `;
    }

    visitCharacter(node) {
    //     return `
    // if (input(i:i) >= "${node.bottom}" .and. input(i:i) <= "${node.top}") then
    //     lexeme = input(cursor:i)
    //     cursor = i + 1
    //     return
    // end if
    //     `;
    }
}