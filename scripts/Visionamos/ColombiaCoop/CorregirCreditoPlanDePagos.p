DEFINE VAR vTime AS INTEGER.
DEFINE VAR vToday AS DATE INITIAL 04/30/2016.
DEF VAR W_FecTra AS DATE FORMAT "99/99/9999".
DEF VAR W_NroDias AS INT INIT 0.
DEFINE VAR p_monto AS DECIMAL.
DEF VAR W_CuoTra AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_SdoCapTra AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_PerTra AS INT INIT 0.
DEFINE VAR p_plazo AS INTEGER.
DEFINE VAR p_fecha AS DATE.
DEFINE VAR i AS INTEGER.
DEFINE VAR p_tasa AS DECIMAL.
DEFINE VAR p_nit AS CHARACTER.
DEFINE VAR p_nroDcto AS INTEGER.
DEF VAR W_NroPer AS INT INIT 0.

DEFINE TEMP-TABLE creditos
    FIELD nit AS CHARACTER
    FIELD num_credito AS INTEGER
    FIELD sdo_Capital AS DECIMAL
    FIELD tasa AS DECIMAL
    FIELD per_pago AS INTEGER
    FIELD cuota AS DECIMAL.

DEFINE TEMP-TABLE creditos2
    FIELD nit AS CHARACTER
    FIELD num_credito AS INTEGER
    FIELD fec_pagoCh AS CHARACTER
    FIELD cuo_pagadas AS DECIMAL
    FIELD cuo_pendientes AS DECIMAL
    FIELD plazo AS INTEGER
    FIELD fec_Pago AS DATE.

DEF TEMP-TABLE W_TabAmor
    FIELD W_Periodo AS INT INIT 0
    FIELD W_Fecha AS DATE FORMAT "99/99/9999"
    FIELD W_FecIniP AS DATE
    FIELD W_Cuota AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_AboCap AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_AboInt AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_SdoCap AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".

DEF TEMP-TABLE W_TabExt
    FIELD W_Fecha AS DATE FORMAT "99/99/9999"
    FIELD W_PlazExt AS INTEGER
    FIELD W_Cuota AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".

DEFINE TEMP-TABLE controlPagos
    FIELD nit AS CHARACTER
    FIELD num_credito AS INTEGER
    FIELD nro_cuota AS INTEGER
    FIELD cuota_capital AS DECIMAL
    FIELD cuota_interes AS DECIMAL
    FIELD INT_mora AS DECIMAL
    FIELD dias_mora AS INTEGER
    FIELD fec_ini AS CHARACTER
    FIELD fec_pago AS CHARACTER
    FIELD d1 AS INTEGER
    FIELD d2 AS INTEGER
    FIELD d3 AS INTEGER
    FIELD d4 AS INTEGER
    FIELD d5 AS INTEGER
    FIELD d6 AS INTEGER
    FIELD d7 AS INTEGER
    FIELD d8 AS INTEGER
    FIELD cuota AS DECIMAL.
    
INPUT FROM d:\leonardo\creditos.csv.
REPEAT:
    CREATE creditos.
    IMPORT DELIMITER ";" creditos.
END.
INPUT CLOSE.

INPUT FROM d:\leonardo\creditos2.csv.
REPEAT:
    CREATE creditos2.
    IMPORT DELIMITER ";" creditos2.

    creditos2.fec_pago = DATE(integer(SUBSTRING(creditos2.fec_pagoCh,5,2)),integer(SUBSTRING(creditos2.fec_pagoCh,7,2)),integer(SUBSTRING(creditos2.fec_pagoCh,1,4))).
    creditos2.plazo = creditos2.cuo_pagadas + creditos2.cuo_pendientes.
    creditos2.cuo_pagadas = ROUND(creditos2.cuo_pagadas,0).
    creditos2.cuo_pendientes = ROUND(creditos2.cuo_pendientes,0).
END.
INPUT CLOSE.


vTime = TIME.

/*RUN MENU.p.*/

FOR EACH creditos NO-LOCK:
    EMPTY TEMP-TABLE W_TabAmor.
    EMPTY TEMP-TABLE W_TabExt.

    RUN pControlPagos(INPUT creditos.nit,
                      INPUT creditos.num_credito).

    /*FOR EACH controlPagos WHERE CONTROLPagos.nit = creditos.nit
                            AND CONTROLPagos.num_credito = creditos.num_credito NO-LOCK BY controlPagos.nro_cuota:
        DISPLAY controlPagos WITH 1 COL.
    END.*/

    /*RETURN.*/
END.

OUTPUT TO d:\leonardo\planPagos.csv.
FOR EACH controlPagos NO-LOCK:
    EXPORT DELIMITER ";" controlPagos.
END.
OUTPUT CLOSE.

MESSAGE "Terminó" STRING(TIME - vTime,"HH:MM:SS")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.


PROCEDURE pControlPagos:
    DEFINE INPUT PARAMETER nitCredito AS CHARACTER.
    DEFINE input PARAMETER numeroCredito AS INTEGER.

    DEFINE VAR p_gracia AS INTEGER.
    DEFINE VAR P_VrSoli AS DECIMAL.
    DEFINE VAR deuda AS DECIMAL.
    DEF VAR W_VlrPte AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
    DEF VAR W_TvrPte AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
    DEFI VAR INT_MGracia AS DECIMAL INIT 0.
    DEF VAR W_NMeses AS INT INIT 0 NO-UNDO.
    DEF VAR W_Primero AS LOG INIT YES.
    DEFINE VAR flagFecPago AS LOGICAL.
    DEFINE VAR flagProy AS LOGICAL.
    DEFINE VAR vIntCorriente AS DECIMAL.
    DEFINE VAR vIntCausado AS DECIMAL.
    
    FIND FIRST creditos2 WHERE creditos2.nit = nitCredito
                           AND creditos2.num_credito = numeroCredito NO-ERROR.
    IF NOT AVAILABLE creditos2 THEN
        RETURN.

    flagfecPago = FALSE.
    flagProy = FALSE.

    /* 1. Borro los registros de PlanPagos y ControlPagos */
    FOR EACH controlPagos WHERE controlPagos.nit = creditos.nit
                            AND controlPagos.num_credito = creditos.num_credito:
        DELETE controlPagos.
    END.

    p_monto = creditos.sdo_capital.
    p_plazo = creditos2.plazo.
    p_fecha = creditos2.fec_pago.
    p_gracia = 0.
    p_nit = creditos.nit.
    p_nroDcto = Creditos.Num_Credito.

    RUN HallarPeriodo.

    p_tasa = creditos.tasa / (W_NroPer * 100).

    /* 2 - Inicia la creación de los registros */
    RUN P-Amortizacion.

END PROCEDURE.


PROCEDURE P-Amortizacion:
    DEFINE VAR fecIni AS DATE.

    fecIni = vToday.
    
    /* 3 - Proceso de creación de los registros de ControlPagos */
    CREATE W_TabAmor.
    ASSIGN W_FecTra = creditos2.fec_pago - w_nroDias
           W_TabAmor.W_AboInt = 0       
           W_TabAmor.W_Cuota = 0
           W_TabAmor.W_AboCap = 0
           W_TabAmor.W_SdoCap = P_Monto
           W_CuoTra = creditos.cuota
           W_SdoCapTra = P_Monto.

    DO W_PerTra = creditos2.cuo_pagadas TO P_Plazo BY 1:
        IF W_PerTra > 0 THEN DO:
            IF w_PerTra > 1 THEN
                RUN CambiarFecha.

            IF MONTH(w_fecTra) = 3 AND DAY(w_fecTra) = 28 THEN
                w_fecTra = w_fecTra + 3.

            W_TabAmor.W_Fecha = W_FecTra.
        END.

        W_TabAmor.W_Cuota = creditos.cuota.

        W_TabAmor.W_Periodo = W_PerTra.

        IF (W_PerTra = 0) THEN DO:
            ASSIGN W_TabAmor.W_Cuota = 0
                   W_TabAmor.W_Fecha = P_Fecha.
        END.
        ELSE DO:
            RUN P-AmortCuotaFija.
        END.

        IF W_PerTra < P_Plazo THEN
            CREATE W_TabAmor.

        I = I + 1.
    END.

    fecIni = vToday - 1.

    FOR EACH W_TabAmor:
        RUN Crear_PLan(INPUT W_Periodo,
                       INPUT fecIni,
                       INPUT W_Fecha,
                       INPUT W_Cuota,
                       INPUT W_Abocap,
                       INPUT W_AboInt,
                       INPUT W_SdoCap).
        
        fecIni = w_fecha.
    END.
END PROCEDURE.

PROCEDURE P-AmortCuotaFija:
    DEF VAR W_SdoAnt AS DECIMAL.
    DEF VAR W_Interes AS DECIMAL.
    DEFI VAR W_CuoExT LIKE Creditos.Cuota INIT 0.

    W_CuoExT = creditos.cuota.

    IF W_SdoCapTra GT 0 THEN DO:
            W_TabAmor.W_AboInt = ROUND(W_SdoCapTra * P_Tasa,0).
    END.
    ELSE
        IF W_SdoCapTra GT 0 THEN
            W_TabAmor.W_AboInt = ROUND((W_SdoCapTra - W_CuoExT) * P_Tasa,0).

    ASSIGN W_SdoAnt = W_SdoCapTra
           W_Interes = W_TabAmor.W_AboInt
           W_TabAmor.W_Cuota = W_CuoExT.

    IF W_SdoCapTra GT 0 THEN
        ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_Cuota - (W_TabAmor.W_AboInt)
               W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap
               W_TabAmor.W_SdoCap = W_SdoCapTra.

    /* Ajuste a la última cuota - 27/05/2010 - */
    IF W_PerTra = P_Plazo AND W_TabAmor.W_SdoCap <> 0 THEN DO:
        IF W_TabAmor.W_SdoCap < 0 THEN
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap - ABS(W_TabAmor.W_SdoCap)
                   /*W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + ABS(W_TabAmor.W_SdoCap)*/.
        ELSE
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap
                   /*W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - W_TabAmor.W_SdoCap*/.

        W_TabAmor.W_Cuota = W_TabAmor.W_AboCap + W_TabAmor.W_AboInt.
    END.
END PROCEDURE.

PROCEDURE CambiarFecha:
    RUN D:\SFG\Desarrollo\Obj\Halla_FecVcto.R (INPUT creditos2.fec_pago,
                                               INPUT W_NroDias,
                                               INPUT W_FecTra,
                                               OUTPUT W_FecTra).

END PROCEDURE.


PROCEDURE Crear_Plan:
    /*--------------------*/
    DEF INPUT PARAMETER W_Periodo AS INT FORMAT "99".
    DEF INPUT PARAMETER W_FecAnt AS DATE.
    DEF INPUT PARAMETER W_Fecha AS DATE.
    DEF INPUT PARAMETER W_Cuota LIKE Creditos.Cuota.
    DEF INPUT PARAMETER W_Abocap LIKE Creditos.Sdo_Capital.
    DEF INPUT PARAMETER W_AboInt LIKE Creditos.Sdo_Capital.
    DEF INPUT PARAMETER W_SdoCap LIKE Creditos.Sdo_Capital.

    IF W_Periodo EQ 0 THEN
        RETURN.

    CREATE controlPagos.
    ASSIGN controlPagos.Cuota = W_Cuota
           controlPagos.fec_ini = STRING(w_fecAnt + 1,"99/99/9999")
           controlPagos.Fec_pago = STRING(W_Fecha,"99/99/9999")
           controlPagos.Nit = P_Nit
           controlPagos.Nro_Cuota = W_Periodo
           controlPagos.Num_Credito = P_NroDcto
           controlPagos.cuota_capital = W_AboCap
           controlPagos.cuota_interes = W_AboInt.

END PROCEDURE.



PROCEDURE hallarPeriodo:
    CASE creditos.per_pago:
        WHEN 1 THEN
            ASSIGN W_NroDias = 1
                   W_NroPer = 360.

        WHEN 7 THEN
            ASSIGN W_NroDias = 7
                   W_NroPer = 52.

        WHEN 10 THEN
            ASSIGN W_NroDias = 10
                   W_NroPer = 36.

        WHEN 15 THEN
            ASSIGN W_NroDias = 15
                   W_NroPer = 24.

        WHEN 30 THEN
            ASSIGN W_NroDias = 30
                   W_NroPer = 12.

        WHEN 60 THEN
            ASSIGN W_NroDias = 60
                   W_NroPer = 6.

        WHEN 90 THEN
            ASSIGN W_NroDias = 90
                   W_NroPer = 4.

        WHEN 120 THEN
            ASSIGN W_NroDias = 120
                   W_NroPer = 3.

        WHEN 180 THEN
            ASSIGN W_NroDias = 180
                   W_NroPer = 2.

        WHEN 360 THEN
            ASSIGN W_NroDias = 360
                   W_NroPer = 1.
    END CASE.

END PROCEDURE.

