
export function CondicionalStrings(exprNode) {
    const exprVal = exprNode.expr.val;
    const exprLength = exprVal.length;
    const cursorSlice = `input(cursor:cursor + ${exprLength - 1})`;

    if (exprNode.expr.isCase === 'i') {
        return `to_lower("${exprVal}") == to_lower(${cursorSlice})`;
    }

    return `"${exprVal}" == ${cursorSlice}`;
}


export function LiteralPor(exprNode) {
    const condicional = CondicionalStrings(exprNode);
    return `
    cursorAux = cursor    
    cicloActivo = .true.
    allocate(character(len=0) :: lexemeAux)  
    do while (cicloActivo)	
        if ( ${condicional} ) then
            cursor = cursor + ${exprNode.expr.val.length}
            lexemeAux = lexemeAux // "${exprNode.expr.val}"

`
}