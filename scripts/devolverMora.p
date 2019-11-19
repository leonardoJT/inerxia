DEFINE TEMP-TABLE cambio
    FIELD nit AS CHARACTER
    FIELD cod_credito AS INTEGER
    FIELD num_credito AS INTEGER.

DEFINE VAR moraNom AS CHARACTER INITIAL "1655180204".
DEFINE VAR ingNom AS CHARACTER INITIAL "41851054".
DEFINE VAR moraCaj AS CHARACTER INITIAL "1655180204".
DEFINE VAR ingCaj AS CHARACTER INITIAL "41851089".

DEFINE VAR vSec AS INTEGER.

INPUT FROM D:\LEONARDO\cambiofECHA.CSV.
REPEAT:
    CREATE cambio.
    IMPORT DELIMITER ";" cambio.
END.
INPUT CLOSE.

FIND FIRST comprobantes WHERE comprobantes.agencia = 1
                          AND comprobantes.comprobante = 9.
comprobantes.secuencia = comprobantes.secuencia + 1.
vsec = comprobantes.secuencia.

OUTPUT TO d:\leonardo\reversaMora_2_32.csv.
FOR EACH cambio WHERE cambio.cod_credito = 32 NO-LOCK:
    FIND FIRST creditos WHERE creditos.nit = cambio.nit
                          AND creditos.cod_credito = cambio.cod_credito
                          AND creditos.num_credito = cambio.num_credito
                          AND creditos.estado = 2 NO-ERROR.
    IF AVAILABLE creditos THEN DO:
        creditos.fec_pago = ADD-INTERVAL(creditos.fec_pago,1,"months").

        FOR EACH control_Pagos WHERE control_Pagos.nit = creditos.nit
                                 AND control_Pagos.num_credito = creditos.num_credito
                                 AND control_Pagos.fec_vcto <= 03/31/2015:
            CONTROL_pagos.id_PdoMes = 2.
        END.

        IF creditos.INT_morCobrar > 0 THEN DO:
            CREATE mov_contable.
            ASSIGN mov_contable.agencia = 1
                   mov_contable.comentario = "ReversionMora"
                   mov_contable.nit = creditos.nit
                   mov_contable.cr = creditos.INT_morCobrar
                   mov_contable.cen_costos = 999
                   Mov_Contable.Destino = creditos.agencia
                   Mov_Contable.Comprobante = comprobantes.comprobante
                   Mov_Contable.Num_Documento = vsec
                   Mov_contable.Doc_referencia = STRING(creditos.num_credito)
                   Mov_Contable.Fec_Contable = TODAY
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Usuario = "2305"
                   Mov_Contable.Estacion = "005".

            IF creditos.FOR_pago = 2 THEN
                mov_contable.cuenta = moraNom.
            ELSE
                mov_contable.cuenta = moraCaj.


            CREATE mov_contable.
            ASSIGN mov_contable.agencia = 1
                   mov_contable.comentario = "ReversionMora"
                   mov_contable.nit = creditos.nit
                   mov_contable.db = creditos.INT_morCobrar
                   mov_contable.cen_costos = 999
                   Mov_Contable.Destino = creditos.agencia
                   Mov_Contable.Comprobante = comprobantes.comprobante
                   Mov_Contable.Num_Documento = vsec
                   Mov_contable.Doc_referencia = STRING(creditos.num_credito)
                   Mov_Contable.Fec_Contable = TODAY
                   Mov_Contable.Fec_Grabacion = TODAY
                   Mov_Contable.Usuario = "2305"
                   Mov_Contable.Estacion = "005".

            IF creditos.FOR_pago = 2 THEN
                mov_contable.cuenta = ingNom.
            ELSE
                mov_contable.cuenta = ingCaj.

            EXPORT DELIMITER ";" creditos.nit creditos.num_credito creditos.INT_morCobrar.

            creditos.INT_morCobrar = 0.
        END.
    END.
END.
OUTPUT CLOSE.
