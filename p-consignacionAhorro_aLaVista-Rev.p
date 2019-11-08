DEFINE INPUT PARAMETER pAgencia AS INTEGER.
DEFINE INPUT PARAMETER pCodAhorroAlaVista AS INTEGER.
DEFINE INPUT PARAMETER pNit AS CHARACTER.
DEFINE INPUT PARAMETER pNitTrans AS CHARACTER.
DEFINE INPUT PARAMETER pNumCuenta AS CHARACTER.
DEFINE INPUT PARAMETER pValEfectivo AS DECIMAL.
DEFINE INPUT PARAMETER pValCheque AS DECIMAL.
DEFINE INPUT PARAMETER pCodComprobante AS INTEGER.
DEFINE INPUT PARAMETER pNumDocumento AS INTEGER.
DEFINE INPUT PARAMETER pDescripcion AS CHARACTER.
DEFINE INPUT PARAMETER pNroAuditoria AS CHARACTER.
DEFINE INPUT PARAMETER pNroCheque AS CHARACTER.
DEFINE INPUT PARAMETER pCodigoBanco AS CHARACTER.
DEFINE INPUT PARAMETER pTerceroTransaccion AS CHARACTER.
DEFINE INPUT PARAMETER pUsuario AS CHARACTER.
DEFINE INPUT PARAMETER pProcesoAtendido AS LOGICAL.
DEFINE OUTPUT PARAMETER pResult AS LOGICAL.

{Incluido/Variable.I "SHARED"}

DEFINE VAR vCuenta AS CHARACTER.
DEFINE VAR vCuentaSyA AS CHARACTER.

FIND FIRST ahorros WHERE ahorros.nit = pNit
                     AND ahorros.cod_ahorro = pCodAhorroAlaVista
                     AND ahorros.cue_ahorros = pNumCuenta
                     AND ahorros.agencia = pAgencia NO-ERROR.
IF AVAILABLE ahorros THEN DO:
    /* Captura de configuración contable para la contabilidad */
    FIND FIRST cortoLargo WHERE cortolargo.agencia = ahorros.agencia
                            AND cortoLargo.clase_Producto = 1
                            AND cortoLargo.cod_Producto = Ahorros.cod_ahorro NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cortoLargo THEN DO:
        IF pProcesoAtendido = TRUE THEN
            MESSAGE "No se encuentra configuración contable para el producto de Ahorro a la Vista." SKIP
                    "Revise la configuración."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN ERROR.
    END.

    vCuenta = CortoLargo.Cta_AsoAd.
    vCuentaSyA = CortoLargo.Cta_SyA.

    FIND FIRST cuentas WHERE cuentas.cuenta = vCuenta NO-LOCK NO-ERROR.
    /* ----------------------- */

    ahorros.sdo_disponible = ahorros.sdo_disponible - pValEfectivo.
    ahorros.sdo_canje = ahorros.sdo_canje - pValCheque.
    ahorros.fec_ultTrans = TODAY.

    CREATE mov_ahorros.
    Mov_Ahorros.Agencia = ahorros.agencia.
    mov_Ahorros.Age_Destino = ahorros.agencia.
    mov_ahorros.Age_Fuente = w_agencia.
    Mov_Ahorros.Cedula_Transac = pNitTrans.
    Mov_Ahorros.Cod_Ahorro = ahorros.cod_ahorro.
    Mov_Ahorros.Cod_Operacion = 010102001.
    Mov_Ahorros.Cpte = pCodComprobante.
    Mov_Ahorros.Cue_Ahorros = ahorros.cue_ahorros.
    Mov_Ahorros.Descrip = "Rev-" + pDescripcion.
    Mov_Ahorros.Fecha = TODAY.
    Mov_Ahorros.Hora = TIME.
    Mov_Ahorros.Nit = ahorros.nit.
    Mov_Ahorros.NomApell_Trans = pTerceroTransaccion.
    mov_ahorros.nro_auditoria = pNroAuditoria.
    Mov_Ahorros.Num_Documento = STRING(pNumDocumento).
    Mov_Ahorros.Sdo_Disponible = ahorros.sdo_disponible.
    Mov_Ahorros.Usuario = pUsuario.
    Mov_Ahorros.Val_Cheque = pValCheque.
    Mov_Ahorros.Val_Efectivo = pValEfectivo.

    IF pValCheque > 0 THEN DO:
        mov_ahorros.cod_operacion = 010101002.
        
        FIND FIRST che_transito WHERE Che_Transito.Agencia = ahorros.agencia
                                  AND Che_Transito.Cheque = pNroCheque
                                  AND Che_Transito.Cod_Compensa = INTEGER(pCodigoBanco)
                                  AND Che_Transito.Cod_Producto = ahorros.cod_ahorro
                                  AND Che_Transito.Num_Cuenta = ahorros.cue_ahorros
                                  AND Che_Transito.Tip_Producto = '1'
                                  AND che_transito.estado = 1
                                  AND Che_Transito.Valor = pValCheque NO-ERROR.
        IF AVAILABLE che_transito THEN
            che_transito.estado = 4.
    END.

    CREATE mov_contable.
    Mov_Contable.agencia = ahorros.agencia.
    Mov_Contable.Cen_Costos = 999.
    Mov_Contable.Comentario = pDescripcion.
    Mov_Contable.Comprobante = pCodComprobante.
    Mov_Contable.db = pValEfectivo + pValCheque.
    Mov_Contable.Cuenta = vCuenta.
    Mov_Contable.Doc_Referencia = ahorros.cue_ahorros.
    Mov_Contable.Enlace = ahorros.cue_ahorros.
    Mov_Contable.Fec_Contable = TODAY.
    Mov_Contable.Fec_Grabacion = TODAY.
    Mov_Contable.Hora = TIME.
    ASSIGN Mov_Contable.Nit = ahorros.nit WHEN cuentas.id_nit = TRUE.
    Mov_Contable.Num_Documento = pNumDocumento.
    Mov_Contable.Usuario = pUsuario.

    pResult = TRUE.
END.
