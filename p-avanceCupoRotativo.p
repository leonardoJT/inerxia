DEFINE INPUT PARAMETER pRowIdCredito AS ROWID.
DEFINE INPUT PARAMETER pValorEfectivo AS DECIMAL.
DEFINE INPUT PARAMETER pValorCheque AS DECIMAL.
DEFINE INPUT PARAMETER pComision AS DECIMAL.
DEFINE INPUT PARAMETER pComprobante AS INTEGER.
DEFINE INPUT PARAMETER pDescripcion AS CHARACTER.
DEFINE INPUT PARAMETER pNumDocumento AS INTEGER.
DEFINE INPUT PARAMETER pAgenciaDelUsuario AS INTEGER.
DEFINE INPUT PARAMETER pUsuario AS CHARACTER.

DEFINE VAR vCuentaCompensacion AS CHARACTER INITIAL "24459550".
DEFINE VAR vTasacredito AS DECIMAL.
DEFINE VAR vCuentaContableCredito AS CHARACTER.

FIND FIRST creditos WHERE ROWID(creditos) = pRowIdCredito NO-ERROR.

creditos.Sdo_capital = Creditos.Sdo_capital + pValorEfectivo + pValorCheque.

CREATE Mov_Creditos.
RUN movCreditos.
Mov_Creditos.Val_Cheque = pValorCheque.
Mov_Creditos.Val_Efectivo = pValorEfectivo.

IF pComision > 0 THEN DO:
    creditos.sdo_capital = creditos.sdo_capital + pComision.
    CREATE Mov_Creditos.
    RUN movCreditos.
    Mov_Creditos.Val_Efectivo = pComision.
END.

/* Se analiza si es necesario realizar el cambio de tasa */
FIND FIRST pro_creditos WHERE pro_credito.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
IF AVAILABLE(pro_creditos) THEN
    FIND FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa  NO-LOCK NO-ERROR.

IF AVAILABLE(indicadores) THEN DO:
    vTasaCredito = (((EXP((indicadores.tasa / 100) + 1,1 / 12)) - 1) * 100) * 12.

    IF STRING(vTasaCredito,">>9.99") <> STRING(creditos.tasa,">>9.99") THEN DO:
        CREATE Mov_Creditos.
        RUN movCreditos.
        Mov_Creditos.Cod_Operacion = 999999999.
        Mov_Creditos.Descrip = "Cambio de Tasa " + STRING(creditos.tasa,">>9.99") + "-->" + STRING(vTasaCredito,">>9.99").

        creditos.tasa = vTasaCredito.
    END.
END.

CREATE mov_contable.
Mov_Contable.agencia = creditos.agencia.
Mov_Contable.Comentario = pDescripcion.
Mov_Contable.Comprobante = pComprobante.

RUN cuentaContableCredito.

Mov_Contable.Cuenta = vCuentaContablecredito.
Mov_Contable.Db = pValorEfectivo + pValorCheque + pComision.
Mov_Contable.Destino = pAgenciaDelUsuario.
Mov_Contable.Doc_Referencia = STRING(creditos.num_credito).
Mov_Contable.Fec_Contable = TODAY.
Mov_Contable.Fec_Grabacion = TODAY.
Mov_Contable.Hora = TIME.
Mov_Contable.Nit = creditos.nit.
Mov_Contable.Num_Documento = pNumDocumento.
Mov_Contable.Usuario = pUsuario.


PROCEDURE movCreditos:
    Mov_Creditos.agencia = creditos.agencia.
    Mov_Creditos.Cod_Credito = creditos.cod_credito.
    Mov_Creditos.Cod_Operacion = 020102001.
    Mov_Creditos.Cpte = pComprobante.
    Mov_Creditos.Descrip = pDescripcion.
    Mov_Creditos.Fecha = TODAY.
    Mov_Creditos.Hora = TIME.
    Mov_Creditos.Nit = creditos.nit.
    Mov_Creditos.Num_Credito = credito.num_credito.
    Mov_Creditos.Num_Documento = STRING(pNumDocumento).
    Mov_Creditos.Ofi_Destino = pAgenciaDelUsuario.
    Mov_Creditos.Ofi_Fuente = creditos.agencia.
    Mov_Creditos.Pagare = creditos.pagare.
    Mov_Creditos.Sdo_Capital = creditos.sdo_capital.
    Mov_Creditos.Usuario = pUsuario.

END PROCEDURE.

PROCEDURE cuentaContableCredito:
    FIND FIRST CortoLargo WHERE CortoLargo.Agencia = Creditos.Agencia
                            AND CortoLargo.Clase_Producto = 2
                            AND CortoLargo.Cod_Producto = Creditos.Cod_Credito NO-LOCK NO-ERROR.

    IF creditos.FOR_pago = 2 THEN
        vCuentaContableCredito = CortoLargo.Cta_AsoAd.
    ELSE
        vCuentaContableCredito = CortoLargo.Cta_NoaAd.

END PROCEDURE
