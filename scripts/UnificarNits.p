DEFINE TEMP-TABLE reg
    FIELD agencia AS INTEGER
    FIELD nit AS CHARACTER FORMAT "X(20)"
    FIELD nit2 AS CHARACTER FORMAT "X(20)"
    FIELD cuenta AS CHARACTER
    FIELD db AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD cr AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99"
    FIELD saldo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".

DEFINE VAR cedula AS CHARACTER.
DEFINE VAR cedula2 AS CHARACTER.
DEFINE VAR cont AS INTEGER.
DEFINE VAR saldoInicial AS DECIMAL.
DEFINE VAR seq AS INTEGER.

INPUT FROM C:\INFO_Fodun\Leonardo\Nits1.txt.

REPEAT:
    IMPORT cedula.

    cedula2 = "".
    cont = 0.

    FIND FIRST ahorros WHERE ahorros.nit = cedula
                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        MESSAGE "Ahorros" cedula
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    FIND FIRST creditos WHERE creditos.nit = cedula
                         AND creditos.estado = 2 NO-LOCK NO-ERROR.
    IF AVAILABLE creditos THEN DO:
        MESSAGE "creditos" cedula
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN.
    END.

    FOR EACH clientes WHERE clientes.nit <> cedula
                        AND INDEX(clientes.nit,cedula) = 1
                        AND clientes.estado = 1 NO-LOCK:
        cedula2 = clientes.nit.

        /*cont = cont + 1.

        IF cont = 2 THEN
            MESSAGE cedula
                VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
    END.

    /*IF cedula2 = "" THEN
        MESSAGE cedula
            VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    FOR EACH agencias NO-LOCK:
        FOR EACH anexos WHERE anexos.agencia = agencias.agencia
                          AND anexos.nit = cedula
                          AND anexos.ano = 2012 BREAK BY anexos.cuenta:
            IF FIRST-OF(anexos.cuenta) THEN DO:
                saldoInicial = 0.

                CREATE reg.
                reg.agencia = agencias.agencia.
                reg.nit = cedula.
                reg.nit2 = cedula2.
                reg.cuenta = anexos.cuenta.
            END.

            saldoInicial = saldoInicial + anexos.sdo_inicial.

            DO cont = 1 TO MONTH(TODAY):
                reg.db = reg.db + anexos.db[cont].
                reg.cr = reg.cr + anexos.cr[cont].
            END.

            IF LAST-OF(anexos.cuenta) THEN DO:
                IF reg.db - reg.cr = 0 THEN
                    DELETE reg.
                ELSE
                    reg.saldo = reg.db - reg.cr.
            END.
        END.
    END.
END.

FOR EACH reg NO-LOCK BREAK BY reg.agencia:
    IF FIRST-OF(reg.agencia) THEN DO:
        FIND FIRST comprobantes WHERE comprobantes.agencia = reg.agencia
                                  AND comprobantes.comprobante = 4 NO-ERROR.
        IF AVAILABLE comprobantes THEN DO:
            comprobantes.secuencia = comprobantes.secuencia + 1.
            seq = comprobantes.secuencia.
        END.
    END.

    CREATE mov_contable.
    mov_contable.agencia = reg.agencia.
    mov_contable.comprobante = 4.
    mov_contable.num_documento = seq.
    mov_contable.cuenta = reg.cuenta.
    mov_contable.cen_costos = 999.
    mov_contable.nit = reg.nit.
    mov_contable.doc_referencia = STRING(seq).
    mov_contable.fec_contable = TODAY.
    mov_contable.comentario = "Traslado por unificación de Nit".
    mov_contable.usuario = "2305".
    mov_contable.estacion = "5".
    mov_contable.fec_grabacion = TODAY.
    mov_contable.hora = TIME.

    IF reg.saldo > 0 THEN
        mov_contable.cr = reg.saldo.
    ELSE
        mov_contable.db = reg.saldo * -1.

   CREATE mov_contable.
   mov_contable.agencia = reg.agencia.
   mov_contable.comprobante = 4.
   mov_contable.num_documento = seq.
   mov_contable.cuenta = reg.cuenta.
   mov_contable.cen_costos = 999.
   mov_contable.nit = reg.nit2.
   mov_contable.doc_referencia = STRING(seq).
   mov_contable.fec_contable = TODAY.
   mov_contable.comentario = "Traslado por unificación de Nit".
   mov_contable.usuario = "2305".
   mov_contable.estacion = "5".
   mov_contable.fec_grabacion = TODAY.
   mov_contable.hora = TIME.

   IF reg.saldo > 0 THEN
       mov_contable.db = reg.saldo.
   ELSE
       mov_contable.cr = reg.saldo * -1.

   FIND FIRST clientes WHERE clientes.nit = reg.nit NO-ERROR.
   IF AVAILABLE clientes THEN DO:
       clientes.estado = 2.
       clientes.fec_retiro = TODAY.
       clientes.cod_retiro = 22.
   END.
END.
