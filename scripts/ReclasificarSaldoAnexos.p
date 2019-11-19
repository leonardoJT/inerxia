DEFINE TEMP-TABLE movCont LIKE mov_contable.
DEFINE VAR pSecuencia AS INTEGER.
DEFINE VAR saldoTotal AS DECIMAL.
DEFINE VAR saldoAnexo AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR vAgencia AS INTEGER INITIAL 4.

FIND FIRST comprobantes WHERE comprobantes.comprobante = 4
                          AND comprobantes.agencia = vAgencia NO-ERROR.
IF AVAILABLE comprobantes THEN DO:
    /*comprobantes.secuencia = 3.*/
    comprobantes.secuencia = comprobantes.secuencia + 1.
    pSecuencia = comprobantes.secuencia.
END.

FOR EACH anexos WHERE anexos.cuenta = "24959550"
                  AND anexos.agencia = vAgencia
                  AND anexos.ano = 2015
                  AND anexos.nit <> "800112808" NO-LOCK:
    saldoAnexo = anexos.sdo_inicial.

    DO cont = 1 TO MONTH(TODAY):
        saldoAnexo = saldoAnexo + anexos.cr[cont] - anexos.db[cont].
    END.

    IF saldoAnexo <> 0 THEN DO:
        saldoTotal = saldoTotal + saldoAnexo.

        CREATE movCont.
        movCont.agencia = anexos.agencia.
        movCont.cuenta = anexos.cuenta.
        movCont.comentario = "ReclasificaciónDeSaldo".
        movCont.nit = anexos.nit.
    
        IF saldoAnexo > 0 THEN
            movCont.db = saldoAnexo.
        ELSE
            movCont.cr = saldoAnexo * -1.

        movCont.cen_costos = 999.

        movCont.destino = anexos.agencia.
        movCont.comprobante = 4.
        movCont.num_documento = pSecuencia.
        movCont.Doc_referencia = STRING(pSecuencia).
        movCont.Fec_Contable = TODAY.
        movCont.Fec_Grabacion = TODAY.
        movCont.Usuario = "2305".
        movCont.Estacion = "005".
    END.
END.

CREATE movCont.
movCont.agencia = vAgencia.
movCont.cuenta = "24959550".
movCont.comentario = "ReclasificaciónDeSaldo".
movCont.nit = "800112808".

IF saldoTotal > 0 THEN
    movCont.cr = saldoTotal.
ELSE
    movCont.db = saldoTotal * -1.

movCont.cen_costos = 999.
movCont.destino = 1.
movCont.comprobante = 4.
movCont.num_documento = pSecuencia.
movCont.Doc_referencia = STRING(pSecuencia).
movCont.Fec_Contable = TODAY.
movCont.Fec_Grabacion = TODAY.
movCont.Usuario = "2305".
movCont.Estacion = "005".

FOR EACH movCont NO-LOCK:
    CREATE mov_contable.
    BUFFER-COPY movCont TO mov_contable.
END.

MESSAGE "Hecho!"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
