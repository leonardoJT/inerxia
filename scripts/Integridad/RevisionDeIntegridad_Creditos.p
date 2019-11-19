DEFINE VAR sumaProducto AS DECIMAL INITIAL 0.
DEFINE VAR sumaContable AS DECIMAL INITIAL 0.
DEFINE VAR vFecha AS DATE.
DEFINE VAR vAgencia AS INTEGER.
DEFINE VAR fechaIni AS DATE.
DEFINE VAR fechaFin AS DATE.

DEFINE TEMP-TABLE cuentasCred
    FIELD cuenta AS CHARACTER.

FOR EACH cortoLargo WHERE agencia = 1
                      AND clase_producto = 2 NO-LOCK:
    FIND FIRST cuentasCred WHERE cuentasCred.cuenta = CortoLargo.Cta_AsoAd NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasCred THEN DO: 
        CREATE cuentasCred.
        cuentasCred.cuenta = CortoLargo.Cta_AsoAd.
    END.

    FIND FIRST cuentasCred WHERE cuentasCred.cuenta = CortoLargo.Cta_NoaAd NO-LOCK NO-ERROR.
    IF NOT AVAILABLE cuentasCred THEN DO: 
        CREATE cuentasCred.
        cuentasCred.cuenta = CortoLargo.Cta_NoaAd.
    END.
END.

/* Interes
- 020101002
- 020102006
*/

vAgencia = 2.
fechaIni = 08/01/2018.
fechaFin = 08/31/2018.

OUTPUT TO d:\leonardo\integridad.csv.
DO vFecha = fechaFin TO fechaIni BY -1:
    FOR EACH mov_creditos WHERE mov_creditos.agencia = vAgencia
                            /*AND mov_creditos.cod_credito = 17*/
                            AND mov_creditos.fecha = vFecha
                            /*AND (mov_creditos.cod_operacion = 020101001 OR
                                 (mov_creditos.cod_operacion = 020102001 AND INDEX(mov_creditos.descrip,"mora") > 0))*/
                            AND (mov_creditos.cod_operacion = 020101001 OR mov_creditos.cod_operacion = 020102001) NO-LOCK:
        sumaProducto = sumaProducto + mov_creditos.val_efectivo + mov_creditos.val_cheque.
    END.

    FOR EACH cuentasCred NO-LOCK:
        FOR EACH mov_contable WHERE mov_contable.agencia = vAgencia
                                AND mov_contable.cuenta = cuentasCred.cuenta
                                AND mov_contable.fec_contable = vFecha
                                AND mov_contable.comprobante <> 20 NO-LOCK:
            sumaContable = sumaContable + mov_contable.cr + mov_contable.db.
        END.
    END.

    EXPORT DELIMITER ";" vfecha sumaProducto - sumaContable.

    IF sumaProducto - sumaContable <> 0 THEN
        MESSAGE vFecha sumaProducto - sumaContable vFecha
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    sumaProducto = 0.
    sumaContable = 0.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
