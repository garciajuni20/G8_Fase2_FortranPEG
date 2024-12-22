{{
    
    // let identificadores = []

    // import { identificadores } from '../index.js'

    import { ids, usos} from '../index.js'
    import { ErrorReglas } from './error.js';
    import { errores } from '../index.js'
    import * as n from '../../fase2/src/lib/CST.js';
}}

gramatica = _ producciones:producciones+ _ {

    let duplicados = ids.filter((item, index) => ids.indexOf(item) !== index);
    if (duplicados.length > 0) {
        errores.push(new ErrorReglas("Regla duplicada: " + duplicados[0]));
    }

    // Validar que todos los usos están en ids
    let noEncontrados = usos.filter(item => !ids.includes(item));
    if (noEncontrados.length > 0) {
        errores.push(new ErrorReglas("Regla no encontrada: " + noEncontrados[0]));
    }
    return producciones;
}

producciones = _ id:identificador _ alias:(literales)? _ "=" _ expr:opciones (_";")? {
    ids.push(id);
    return new n.Producciones(id, expr, alias);
  }

opciones = inicio:union resto:(_ "/" _ @union)* {
    return new n.Opciones([inicio, ...resto]);
  }

union = inicio:expresion resto:(_ @expresion !(_ literales? _ "=") )* {
    return new n.Union([inicio, ...resto]);
  }

expresion = ("@")? _ id:(identificador _ ":")?_ label:varios? _ expr:expresiones _ qty:([?+*]/conteo)?{
    return new n.Expresion(id, label, expr,qty);
  }

//ERRORES ENCONTRADOS: podia venir @pluck:@"expresion"  o 
/*expresion  = (etiqueta/varios)? _ expresiones _ ([?+*]/conteo)?

etiqueta = ("@")? _ id:identificador _ ":" (varios)?

 varios = ("!"/"$"/"@"/"&")*/

varios = val:("!"/"&"/"$"){return new n.Varios(val);  }

expresiones  =  expr:identificador {return new n.Expresiones(expr);  }
                / expr:$literales isCase:"i"?{return new n.String(expr.replace(/['"]/g, ''), isCase);  }
                / "(" _ expr:opciones _ ")"{return new n.Expresiones(expr);  }
                / expr:corchetes "i"?{return new n.Expresiones(expr);  }
                / "."
                / "!."

// conteo = "|" parteconteo _ (_ delimitador )? _ "|"

conteo = "|" _ (numero / id:identificador) _ "|"
        / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "|"
        / "|" _ (numero / id:identificador)? _ "," _ opciones _ "|"
        / "|" _ (numero / id:identificador)? _ ".." _ (numero / id2:identificador)? _ "," _ opciones _ "|"

// parteconteo = identificador
//             / [0-9]? _ ".." _ [0-9]?
// 			/ [0-9]

// delimitador =  "," _ expresion

// Regla principal que analiza corchetes con contenido
corchetes
    = "[" contenido:(rango / texto)+ "]" {
        return new n.Corchetes(contenido);
    }

// Regla para validar un rango como [A-Z]
rango
    = inicio:caracter "-" fin:caracter {
        if (inicio.char.charCodeAt(0) > fin.char.charCodeAt(0)) {
            throw new Error(`Rango inválido: [${inicio}-${fin}]`);
        }
        return new n.Rango(inicio, fin); 
    }

// Regla para caracteres individuales
caracter
    = data:[a-zA-Z0-9_ ] { return new n.Caracter(data); }



/* GRAMATICAS ANTERIORES, DAN ERROR AL TRATAR DE RECONOCER EJ: [abc0-3], reconocimiento esperado: [a,b,c,1,2,3]
                                                                  Salida que se obtiene: [a,b,c,1,-,3]

 contenido
   = elementos:(corchete / texto)+ {
      return new n.Contenido(elementos);
  }

 corchete
    = "[" contenido "]" 
*/

// Coincide con cualquier contenido que no incluya "]"

texto
    = val:[^\[\]]{
        return new n.String(val);
    }

literales = '"' val:stringDobleComilla* '"' 
            / "'" stringSimpleComilla* "'"

stringDobleComilla =!('"' / "\\" / finLinea) .{
         return text();
    } / "\\" escape 
                    //(se quitaron porque peggyjs no acepta cadenas con multilinea) igual no funcionaba xd
                    // / continuacionLinea

stringSimpleComilla = !("'" / "\\" / finLinea) . { 
        return this.text(); 
    }
                    / "\\" escape { 
        return this.text(); 
    }
                    //(se quitaron porque peggyjs no acepta cadenas con multilinea) igual no funcionaba xd
                    // / continuacionLinea

//(se quitaron porque peggyjs no acepta cadenas con multilinea) igual no funcionaba xd
// continuacionLinea = "\\" secuenciaFinLinea

continuacionLinea = "\\" secuenciaFinLinea

finLinea = data:[\n\r\u2028\u2029]{return new n.FinLinea(data); }

escape = "'"
        / '"'
        / "\\"
        / "b"
        / "f"
        / "n"
        / "r"
        / "t"
        / "v"
        / "u"

secuenciaFinLinea = "\r\n" / "\n" / "\r" / "\u2028" / "\u2029"

// literales = 
//     "\"" [^"]* "\""
//     / "'" [^']* "'"
    

numero = val:[0-9]+{
    return new n.Numero(val);
  }

identificador = id:[_a-z]i[_a-z0-9]i* {return new n.Identificador(id); }

_ = (Comentarios /[ \t\n\r])*


Comentarios = 
    "//" [^\n]* 
    / "/*" (!"*/" .)* "*/"
