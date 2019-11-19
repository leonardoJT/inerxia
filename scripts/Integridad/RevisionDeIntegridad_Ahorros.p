DEFINE VAR sumaProducto AS DECIMAL INITIAL 0.
DEFINE VAR sumaContable AS DECIMAL INITIAL 0.
DEFINE VAR vFecha AS DATE.
DEFINE VAR vAgencia AS INTEGER.
DEFINE VAR fechaIni AS DATE.
DEFINE VAR fechaFin AS DATE.

vAgencia = 4.
fechaIni = 02/01/2017.
fechaFin = 02/28/2017.

OUTPUT TO d:\leonardo\integridad.csv.
DO vFecha = fechaFin TO fechaIni BY -1:
    FOR EACH mov_ahorros WHERE mov_ahorros.agencia = vAgencia
                           AND mov_ahorro.cod_ahorro = 4
                           AND mov_ahorros.fecha = vFecha NO-LOCK:
        IF mov_ahorros.descrip = "RetFuente X Liq.Interés" THEN
            sumaProducto = sumaProducto - (mov_ahorros.val_efectivo + mov_ahorros.val_cheque).
        ELSE
            sumaProducto = sumaProducto + mov_ahorros.val_efectivo + mov_ahorros.val_cheque.
    END.

    FOR EACH mov_contable WHERE mov_contable.agencia = vAgencia
                            AND mov_contable.cuenta = "21050501"
                            AND mov_contable.fec_contable = vFecha NO-LOCK:
        IF mov_contable.comentario = "RetFuente X Liq.Interés" THEN
            sumaContable = sumaContable - (mov_contable.cr + mov_contable.db).
        ELSE
            sumaContable = sumaContable + mov_contable.cr + mov_contable.db.
    END.

    /*MESSAGE vfecha sumaProducto - sumaContable
        VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

    /*DISPLAY vfecha sumaProducto - sumaContable.*/

    EXPORT DELIMITER ";" vfecha sumaProducto - sumaContable.

    sumaProducto = 0.
    sumaContable = 0.
END.
OUTPUT CLOSE.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
