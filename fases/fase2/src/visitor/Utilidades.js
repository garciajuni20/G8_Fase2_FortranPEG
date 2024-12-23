
export function CondicionalStrSencilla(node) {
    if (!node.exprs || !node.exprs[0] || !node.exprs[0].expr) {
        throw new Error("El nodo no tiene el formato esperado.");
    }

    const exprVal = node.exprs[0].expr.val;
    const exprLength = exprVal.length;
    const cursorSlice = `input(cursor:cursor + ${exprLength - 1})`;

    if (node.exprs[0].expr.isCase === 'i') {
        return `to_lower("${exprVal}") == to_lower(${cursorSlice})`;
    }

    return `"${exprVal}" == ${cursorSlice}`;
}
export function LiteralPor(node){
    let condicional = CondicionalStrSencilla(node);
    return `
    ! * en literales
    ejecuta_ciclo = .true.
    start_cursor = cursor  
    allocate(character(len=0) :: lexeme_accumulated)  
    do while (ejecuta_ciclo)	
        if ( ${condicional} ) then
            cursor = cursor + ${node.exprs[0].expr.val.length}
            lexeme_accumulated = lexeme_accumulated // "${node.exprs[0].expr.val}"
        else
            ejecuta_ciclo = .false.
        end if
    end do
    if (len(lexeme_accumulated) > 0) then
        allocate(character(len=len(lexeme_accumulated)) :: lexeme)
        lexeme = lexeme_accumulated
        return
    end if
`    
}