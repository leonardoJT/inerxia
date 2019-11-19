DEFINE TEMP-TABLE tt LIKE mov_contable.
DEFINE VAR cont AS INTEGER.

INPUT FROM c:\INFO_Fodun\Leonardo\DocumentoAjuste.txt.
REPEAT:
    CREATE tt.
    IMPORT tt.
END.

/*FIND FIRST comprobantes WHERE comprobantes.agencia = 1
                          AND comprobantes.comprobante = 4 NO-ERROR.
comprobantes.secuencia = comprobantes.secuencia + 1.*/

FOR EACH tt NO-LOCK:
    CREATE mov_contable.
    BUFFER-COPY tt TO mov_contable.

    /*mov_contable.num_documento = comprobantes.secuencia.*/
END.
