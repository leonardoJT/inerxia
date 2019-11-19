DISABLE TRIGGERS FOR LOAD OF mov_contable.

DEFINE VAR CuentaOrigen AS CHARACTER INITIAL "16300501".
DEFINE VAR CuentaDestino AS CHARACTER INITIAL "16309503".
DEFINE VAR vAgencia AS INTEGER INITIAL 1.

DEFINE VAR vSec AS INTEGER.

DEFINE VAR cont AS INTEGER.
DEFINE VAR vSdo AS DECIMAL.

DEFINE TEMP-TABLE tt
    FIELD nit AS CHARACTER
    FIELD cc AS INTEGER
    FIELD sdo AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99".


FOR EACH agencias /*WHERE agencias.agencia = vAgencia*/ NO-LOCK:
    FIND FIRST comprobantes WHERE comprobantes.comprobante = 4
                              AND comprobantes.agencia = agencias.agencia NO-ERROR.

    comprobantes.secuencia = comprobantes.secuencia + 1.
    vSec = comprobantes.secuencia.


    FOR EACH anexos WHERE anexos.agencia = agencias.agencia
                      AND anexos.cuenta = cuentaOrigen
                      AND anexos.ano = 2017 NO-LOCK:
        vSdo = anexos.sdo_inicial.

        DO cont = 1 TO 12:
            vSdo = vSdo + anexos.db[cont] - anexos.cr[cont].
        END.

        IF vSdo <> 0 THEN DO:
            CREATE tt.
            tt.nit = anexos.nit.
            tt.cc = anexos.cen_costos.
            tt.sdo = vSdo.

            CREATE mov_contable.
            mov_contable.agencia = agencias.agencia.
            mov_contable.cuenta = CuentaOrigen.
            mov_contable.comentario = "Reclasificación".
            mov_contable.cr = vSdo.
            mov_contable.cen_costos = anexos.cen_costos.
            mov_contable.destino = agencias.agencia.
            mov_contable.comprobante = 4.
            mov_contable.num_documento = vSec.
            mov_contable.Doc_referencia = STRING(vSec).
            mov_contable.Fec_Contable = 12/31/2017.
            mov_contable.Fec_Grabacion = 12/31/2017.
            mov_contable.Usuario = "desarrollo".
            mov_contable.Estacion = "005".
            mov_contable.nit = anexos.nit.

            CREATE mov_contable.
            mov_contable.agencia = agencias.agencia.
            mov_contable.cuenta = CuentaDestino.
            mov_contable.comentario = "Reclasificación".
            mov_contable.db = vSdo.
            mov_contable.cen_costos = anexos.cen_costos.
            mov_contable.destino = agencias.agencia.
            mov_contable.comprobante = 4.
            mov_contable.num_documento = vSec.
            mov_contable.Doc_referencia = STRING(vSec).
            mov_contable.Fec_Contable = 12/31/2017.
            mov_contable.Fec_Grabacion = 12/31/2017.
            mov_contable.Usuario = "desarrollo".
            mov_contable.Estacion = "005".
            mov_contable.nit = anexos.nit.
        END.
    END.
END.

MESSAGE "Fin"
    VIEW-AS ALERT-BOX INFO BUTTONS OK.
