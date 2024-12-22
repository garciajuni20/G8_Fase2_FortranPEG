/** @type {{[node: string]: {[arg: string]: string}}} */
const nodes = {
    Producciones: { id: 'string', expr: 'Opciones', alias: '?string' },
    Opciones: ['exprs'],
    Union: ['exprs'],
    Expresiones: ["expr"],
    Varios: ['val'],
    Corchetes: ["expr"],
    Literales:["literales"],
    Expresion: ['id', 'label', ,'expr', 'qty'],
    String: ['val', 'isCase'],
    Clase: ['chars', 'isCase'],
    Rango: ['bottom', 'top'],
    Identificador: ['id'],
    Numero: ['val'],
    Caracter: ['char'],
    Punto: [],
    Fin: [],
    FinLinea:['data'],
    
};

export default nodes;