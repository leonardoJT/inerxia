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

vAgencia = 3.
fechaIni = 08/01/2018.
fechaFin = 08/31/2018.

OUTPUT TO d:\leonardo\integridad.csv.
DO vFecha = fechaIni TO fechaFin:
    FOR EACH mov_creditos WHERE mov_creditos.agencia = vAgencia
                            /*AND mov_creditos.cod_credito = 17*/
                            AND mov_creditos.fecha = vFecha
                            /*AND mov_creditos.cod_operacion <> 999999999*/
                            AND mov_creditos.cpte <> 20 NO-LOCK:
        sumaProducto = sumaProducto + mov_creditos.val_efectivo + mov_creditos.val_cheque.
    END.

    FOR EACH mov_contable WHERE mov_contable.agencia = vAgencia
                            AND mov_contable.fec_contable = vFecha
                            AND (SUBSTRING(mov_contable.cuenta,1,4) = "1411" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "1412" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "1441" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "1442" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "1443" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "1469" OR
                                 SUBSTRING(mov_contable.cuenta,1,4) = "1470")
                            AND mov_contable.comprobante <> 20 NO-LOCK:
        sumaContable = sumaContable + mov_contable.cr + mov_contable.db.
    END.

    EXPORT DELIMITER ";" vfecha sumaProducto - sumaContable.

    IF sumaProducto - sumaContable <> 0 THEN
        MESSAGE vFecha sumaProducto - sumaContable
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    sumaProducto = 0.
    sumaContable = 0.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
