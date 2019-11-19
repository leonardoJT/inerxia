DISABLE TRIGGERS FOR LOAD OF mov_contable.

DEFINE VAR CuentaOrigen AS CHARACTER INITIAL "14711025".
DEFINE VAR CuentaDestino AS CHARACTER INITIAL "14715001".
DEFINE VAR vAgencia AS INTEGER INITIAL 1.

DEFINE VAR vSec AS INTEGER.

DEFINE VAR cont AS INTEGER.
DEFINE VAR vSdo AS DECIMAL.

DEFINE TEMP-TABLE tt
    FIELD cc AS INTEGER
    FIELD sdo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".


FOR EACH agencias /*WHERE agencias.agencia = vAgencia*/ NO-LOCK:
    FIND FIRST comprobantes WHERE comprobantes.comprobante = 4
                              AND comprobantes.agencia = agencias.agencia NO-ERROR.

    comprobantes.secuencia = comprobantes.secuencia + 1.
    vSec = comprobantes.secuencia.


    FOR EACH sal_cuenta WHERE sal_cuenta.agencia = agencias.agencia
                          AND sal_cuenta.cuenta = cuentaOrigen
                          AND sal_cuenta.ano = 2018 NO-LOCK:
        vSdo = sal_cuenta.sal_inicial.

        DO cont = 1 TO 12:
            vSdo = vSdo + sal_cuenta.db[cont] - sal_cuenta.cr[cont].
        END.

        IF vSdo <> 0 THEN DO:
            CREATE tt.
            tt.cc = sal_cuenta.cen_costos.
            tt.sdo = vSdo.

            CREATE mov_contable.
            mov_contable.agencia = agencias.agencia.
            mov_contable.cuenta = CuentaOrigen.
            mov_contable.comentario = "Reclasificación".
            mov_contable.cr = vSdo.
            mov_contable.cen_costos = sal_cuenta.cen_costos.
            mov_contable.destino = agencias.agencia.
            mov_contable.comprobante = 4.
            mov_contable.num_documento = vSec.
            mov_contable.Doc_referencia = STRING(vSec).
            mov_contable.Fec_Contable = TODAY.
            mov_contable.Fec_Grabacion = TODAY.
            mov_contable.Usuario = "desarrollo".
            mov_contable.Estacion = "005".

            CREATE mov_contable.
            mov_contable.agencia = agencias.agencia.
            mov_contable.cuenta = CuentaDestino.
            mov_contable.comentario = "Reclasificación".
            mov_contable.db = vSdo.
            mov_contable.cen_costos = sal_cuenta.cen_costos.
            mov_contable.destino = agencias.agencia.
            mov_contable.comprobante = 4.
            mov_contable.num_documento = vSec.
            mov_contable.Doc_referencia = STRING(vSec).
            mov_contable.Fec_Contable = TODAY.
            mov_contable.Fec_Grabacion = TODAY.
            mov_contable.Usuario = "desarrollo".
            mov_contable.Estacion = "005".
        END.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
