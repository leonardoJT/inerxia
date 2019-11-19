DEFINE VAR sumSaldos AS DECIMAL.
DEFINE VAR sumInteres AS DECIMAL.
DEFINE VAR cont AS INTEGER.
DEFINE VAR vAgencia AS INTEGER INITIAL 2.

DEFINE TEMP-TABLE tt
    FIELD cuenta AS CHARACTER
    FIELD sdoProd AS DECIMAL
    FIELD sdoContable AS DECIMAL
    FIELD db AS DECIMAL
    FIELD cr AS DECIMAL.

FOR EACH rep_creditos WHERE fecCorte = 08/31/2018
                        AND agencia = vAgencia NO-LOCK BREAK BY cta_contable:
    IF FIRST-OF(cta_contable) THEN DO:
        sumSaldos = 0.
        sumInteres = 0.
    END.

    sumSaldos = sumSaldos + rep_creditos.sdo_capital.
    sumInteres = sumInteres + rep_creditos.INT_corrientes.

    IF LAST-OF(cta_contable) THEN DO:
        CREATE tt.
        tt.cuenta = cta_contable.
        tt.sdoProd = sumSaldos.
    END.
END.

OUTPUT TO d:\Leonardo\clasif.csv.
FOR EACH tt:
    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = vAgencia
                          AND sal_cuenta.cuenta = tt.cuenta
                           AND sal_cuenta.ano = 2018 NO-LOCK:
        tt.sdoContable = tt.sdoContable + sal_cuenta.sal_ini.

        DO cont = 1 TO 8:
            tt.sdoContable = tt.sdoContable + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
        END.
    END.

    IF tt.sdoProd - tt.sdoContable > 0 THEN
        tt.db = tt.sdoProd - tt.sdoContable.

    IF tt.sdoProd - tt.sdoContable < 0 THEN
        tt.cr = tt.sdoContable - tt.sdoProd.

    EXPORT DELIMITER ";" tt.

    /*CREATE mov_contable.
    Mov_Contable.agencia = vAgencia.
    Mov_Contable.Cen_Costos = 999.
    Mov_Contable.Comentario = "ReclasificaciónCapital".
    Mov_Contable.Comprobante = 20.
    Mov_Contable.Cr = tt.cr.
    Mov_Contable.Cuenta = tt.cuenta.
    Mov_Contable.Db = tt.db.
    Mov_Contable.Estacion = "000005".
    Mov_Contable.Fec_Contable = 08/31/2018.
    Mov_Contable.Fec_Grabacion = today.
    Mov_Contable.Hora = time.
    Mov_Contable.Num_Documento = 6069.
    Mov_Contable.Usuario = "desarrollo".*/

END.
OUTPUT CLOSE.
