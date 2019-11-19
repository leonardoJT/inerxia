DEFINE TEMP-TABLE ttact
    FIELD agencia AS INTEGER
    FIELD tipoActivo AS INTEGER
    FIELD cuentaActivo AS CHARACTER
    FIELD nombre AS CHARACTER
    FIELD marca AS CHARACTER
    FIELD serial AS CHARACTER
    FIELD fechaCompra AS CHARACTER
    FIELD factura AS CHARACTER
    FIELD nitProveedor AS CHARACTER
    FIELD valor AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99"
    FIELD cenCostos AS INTEGER
    FIELD depreciable AS LOGICAL
    FIELD meses AS INTEGER
    FIELD depreciado AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99"
    FIELD saldoActual AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9.99".

DEFINE TEMP-TABLE tt_agencias
    FIELD cod_agencia AS INTEGER
    FIELD prefijo AS CHARACTER.

FOR EACH agencias NO-LOCK:
    CREATE tt_agencias.
    tt_agencias.cod_agencia = agencias.agencia.
    tt_agencias.prefijo = SUBSTRING(agencias.nombre,1,3).
END.


INPUT FROM d:\Leonardo\activosFijos.csv.
REPEAT :
    CREATE ttact.
    IMPORT DELIMITER ";" ttact NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        DELETE ttact.
END.
INPUT CLOSE.

/*FOR EACH ttact NO-LOCK:
    DISPLAY ttAct WITH 1 COL.
END.*/

FOR EACH ttact NO-LOCK:
    CREATE activosFijos.

    activosFijos.agencia = ttact.agencia.
    
    FIND FIRST cfg_activosFijos WHERE cfg_activosFijos.tipoActivo = ttAct.tipoActivo NO-ERROR.
    IF AVAILABLE cfg_activosFijos THEN DO:
        FIND FIRST tt_Agencias WHERE tt_agencias.cod_agencia = ttAct.agencia NO-LOCK NO-ERROR.

        activosFijos.idActivo = tt_agencias.prefijo + STRING(ttAct.cenCostos,"999") + STRING(ttact.tipoActivo,"99") + STRING(cfg_ActivosFijos.consecutivo + 1,"99999").
        cfg_ActivosFijos.consecutivo = cfg_ActivosFijos.consecutivo + 1.
    END.

    activosFijos.nombre = ttact.nombre.
    activosFijos.marca = ttact.marca.
    activosFijos.serial = ttact.serial.
    activosFijos.tipoActivo = ttact.tipoActivo.

    IF LENGTH(ttAct.fechaCompra) = 8 THEN
        activosFijos.fechaCompra = DATE(INTEGER(SUBSTRING(ttact.fechaCompra,5,2)),INTEGER(SUBSTRING(ttact.fechaCompra,7,2)),INTEGER(SUBSTRING(ttact.fechaCompra,1,4))).
    ELSE
        activosFijos.fechaCompra = DATE(INTEGER(SUBSTRING(ttact.fechaCompra,4,2)),INTEGER(SUBSTRING(ttact.fechaCompra,1,2)),INTEGER(SUBSTRING(ttact.fechaCompra,7,4))) NO-ERROR.

    IF ERROR-STATUS:ERROR THEN
        DISPLAY ttact WITH 1 COL.

    activosFijos.numFactura = ttact.factura.
    activosFijos.nitProveedor = ttact.nitProveedor.
    activosFijos.depreciable = ttact.depreciable.
    activosFijos.mesesDepreciar = ttact.meses.
    activosFijos.valorCompra = ttact.valor.
    activosFijos.valorDepreciado = ttact.depreciado.
    activosFijos.valorActual = ttact.saldoActual.
    activosFijos.contabilizado = YES.
    activosFijos.estado = 1.
    activosFijos.cen_costos = ttAct.cenCostos.
END.


