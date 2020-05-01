DEFINE INPUT PARAMETER pCanal AS CHARACTER.
DEFINE INPUT PARAMETER pClienteId AS CHARACTER.
DEFINE INPUT PARAMETER pCreditoId AS INTEGER.
DEFINE INPUT PARAMETER pDescripcion AS CHARACTER.
DEFINE INPUT PARAMETER pMonto AS DECIMAL.
DEFINE OUTPUT PARAMETER pError AS LOGICAL.

DEFINE VAR vTasa AS DECIMAL.

FIND FIRST pro_creditos WHERE pro_credito.cod_credito = 123 NO-LOCK NO-ERROR.
IF AVAILABLE pro_creditos THEN
    FIND FIRST indicadores WHERE Indicadores.indicador = pro_creditos.cod_tasa NO-LOCK NO-ERROR.

IF AVAILABLE(indicadores) THEN
    vTasa = ((EXP((indicadores.tasa / 100) + 1,1 / 12)) - 1 ) * 100.
ELSE DO:
    pError = TRUE.
    RETURN.
END.

CREATE utilizacionesRotativo.
utilizacionesRotativo.canal = pCanal.
utilizacionesRotativo.cliente_id = pClienteId.
utilizacionesRotativo.credito_id = pCreditoId.

FIND FIRST cfg_tarjetaDB NO-LOCK NO-ERROR.
IF AVAILABLE cfg_tarjetaDB THEN
    utilizacionesRotativo.cuotas = cfg_tarjetaDB.plazoCupo.
ELSE
    utilizacionesRotativo.cuotas = 1.

utilizacionesRotativo.cuotas_restantes = utilizacionesRotativo.cuotas.
utilizacionesRotativo.descripcion = pDescripcion.
utilizacionesRotativo.estado = 1.
utilizacionesRotativo.fec_cancelacion = ?.
utilizacionesRotativo.fec_utilizacion = TODAY.
utilizacionesRotativo.hora_utilizacion = STRING(TIME,"HH:MM").
utilizacionesRotativo.monto = pMonto.
utilizacionesRotativo.saldo = pMonto.
utilizacionesRotativo.tasa = vTasa.

RELEASE utilizacionesRotativo.
