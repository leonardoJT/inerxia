/*---------------------------------------------------------------------------*
 ARCHIVO     : p-genval.p                                                    *
 DESCRIPCION : Procedimiento que recorre las estructura de la B.D. creando un*
               archivo con el nombre de un incluido para ser utilizado en los*
               triggers de escritura, cuando es generado con ERWIN           *
-----------------------------------------------------------------------------*/

FOR EACH _File WHERE NOT _File._File-Name BEGINS "_" NO-LOCK:
    OUTPUT TO VALUE(".\valida\W_" + _File._File-Name + ".v").
    OUTPUT CLOSE.
END.
