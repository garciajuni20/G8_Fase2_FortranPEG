const nodes = {
    Producciones: ['id', 'expr', 'alias'],
    Opciones: ['exprs', 'alias'],
    Union: ['exprs', 'alias'],
    Expresion: ['expr', 'label', 'qty'],
    
    String: ['val', 'isCase'],
    Clase: ['chars', 'isCase'],
    Rango: ['bottom', 'top'],
};

export default nodes;
