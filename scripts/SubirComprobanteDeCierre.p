DEFINE TEMP-TABLE tt LIKE mov_contable.
DEFINE VAR cont AS INTEGER.

INPUT FROM c:\INFO_Fodun\ComprobanteCierreCarteraDiciembre.txt.
REPEAT:
    CREATE tt.
    IMPORT tt.
END.

DO cont = 1 TO 4:
    FOR EACH tt WHERE tt.agencia = cont BREAK BY tt.num_documento:
        IF FIRST-OF(tt.num_documento) THEN DO:
            FIND FIRST comprobantes WHERE comprobantes.agencia = cont
                                      AND comprobantes.comprobante = 20 NO-ERROR.
            comprobantes.secuencia = comprobantes.secuencia + 1.
        END.

        FIND FIRST comprobantes WHERE comprobantes.agencia = cont
                                  AND comprobantes.comprobante = 20 NO-ERROR.
        
        tt.num_documento = comprobantes.secuencia.

        CREATE mov_contable.
        BUFFER-COPY tt TO mov_contable.

        mov_contable.num_documento = comprobantes.secuencia.
    END.
END.
