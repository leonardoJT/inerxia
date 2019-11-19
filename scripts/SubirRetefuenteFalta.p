DISABLE TRIGGERS FOR LOAD OF anexos.
DISABLE TRIGGERS FOR LOAD OF sal_cuenta.

DEFINE VAR fec AS DATE.
DEFINE VAR ag AS INTEGER.
DEFINE VAR mes AS INTEGER.

DEFINE TEMP-TABLE retes
    FIELD cuenta AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD clave AS INTEGER
    FIELD producto AS CHARACTER
    FIELD valor AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD db AS DECIMAL FORMAT "->>>,>>>,>>9.99"
    FIELD cr AS DECIMAL FORMAT "->>>,>>>,>>9.99".

INPUT FROM "c:\INFO_fodun\Leonardo\Nueva Informacion\24453502_Palmira_Febrero.csv".
REPEAT :
    CREATE retes.
    IMPORT DELIMITER ";" retes.

    IF retes.valor > 0 THEN
        retes.db = valor.
    ELSE
        retes.cr = valor * -1.

    /*DISPLAY retes WITH 1 COL.*/
END.
INPUT CLOSE.

MESSAGE "Inicia"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

ag = 4.
fec = 02/28/2011.
mes = 2.

FOR EACH retes NO-LOCK:
    FIND FIRST clientes WHERE clientes.nit = retes.nit NO-LOCK NO-ERROR.
    IF NOT AVAILABLE clientes THEN DO:
        CREATE clientes.
        clientes.nit = retes.nit.
        clientes.nombre = retes.nombre.
        clientes.tipo_cliente = 23.
    END.

    /* Anexos */
    FIND FIRST anexos WHERE anexos.nit = retes.nit
                        AND anexos.cuenta = retes.cuenta
                        AND anexos.cen_costos = 999
                        AND anexos.agencia = ag
                        AND anexos.ano = 2011 NO-ERROR.
    IF NOT AVAILABLE anexos THEN DO:
        CREATE anexos.
        anexos.nit = retes.nit.
        anexos.cuenta = retes.cuenta.
        anexos.cen_costos = 999.
        anexos.agencia = ag.
        anexos.ano = 2011.
    END.

    anexos.db[mes] = anexos.db[mes] + retes.db.
    anexos.cr[mes] = anexos.cr[mes] + retes.cr.

    /* Anexos13 */
    FIND FIRST anexos13 WHERE anexos13.nit = retes.nit
                          AND anexos13.cuenta = retes.cuenta
                          AND anexos13.cen_costos = 999
                          AND anexos13.agencia = ag
                          AND anexos13.ano = 2011 NO-ERROR.
    IF NOT AVAILABLE anexos13 THEN DO:
        CREATE anexos13.
        anexos13.nit = retes.nit.
        anexos13.cuenta = retes.cuenta.
        anexos13.cen_costos = 999.
        anexos13.agencia = ag.
        anexos13.ano = 2011.
    END.

    anexos13.db[mes] = anexos13.db[mes] + retes.db.
    anexos13.cr[mes] = anexos13.cr[mes] + retes.cr.

    CREATE mov_contable.
    mov_contable.agencia = ag.
    mov_contable.comprobante = 20.
    mov_contable.num_documento = 310111.
    mov_contable.cuenta = retes.cuenta.
    mov_contable.cen_costos = 999.
    mov_contable.nit = retes.nit.
    mov_contable.doc_referencia = retes.producto.
    mov_contable.fec_contable = fec.
    mov_contable.comentario = "RevRetefeunte Rendimientos Financieros".
    mov_contable.usuario = "2305".
    mov_contable.estacion = "5".
    mov_contable.db = retes.db.
    mov_contable.cr = retes.cr.
    mov_contable.fec_grabacion = TODAY.
    mov_contable.hora = TIME.
END.

MESSAGE "Finaliza"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

