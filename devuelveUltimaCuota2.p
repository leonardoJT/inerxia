DEFINE INPUT PARAMETER P_Monto AS DECIMAL.
DEFINE INPUT PARAMETER P_Plazo AS INTEGER.
DEFINE INPUT-OUTPUT PARAMETER P_Cuota AS DECIMAL.
DEFINE INPUT PARAMETER P_Fecha1 AS DATE.
DEFINE INPUT PARAMETER P_Fecha AS DATE.
DEFINE INPUT PARAMETER P_Tasa AS DECIMAL.
DEFINE INPUT PARAMETER P_PerDed AS INTEGER.
DEFINE INPUT PARAMETER P_CodPro AS INTEGER.

DEFINE TEMP-TABLE Temp_Extra
    FIELD Nro_Cuota AS INTEGER FORMAT "9999"
    FIELD Vr_CuoExtra AS DECIMAL FORMAT "->>>>,>>>,>>9.99"
    FIELD Fec_Vcto AS DATE FORMAT "99/99/9999"
    FIELD Estado AS INTEGER.

DEFINE INPUT PARAMETER TABLE FOR Temp_Extra.

DEFINE VAR W_NMeses AS INTEGER.
DEFINE SHARED VAR W_ManFin AS HANDLE.
DEFINE VAR nroCuota AS INTEGER.
DEFINE VAR W_NroDias AS INTEGER.
DEFINE VAR W_SdoCapTra AS DECIMAL.
DEFINE VAR fecPago AS DATE.
DEFINE VAR W_NroPer AS INTEGER.
DEFINE VAR abonoInteres AS INTEGER.
DEFINE VAR P_NomPer AS CHARACTER.
DEFINE VAR fechaAnterior AS DATE.
DEFINE VAR vCuota AS INTEGER.

RUN HallarPeriodo IN W_ManFin (INPUT P_Perded,
                               INPUT P_Plazo,
                               OUTPUT W_NroDias,
                               OUTPUT W_NMeses,
                               OUTPUT W_NroPer,
                               OUTPUT P_NomPer).

P_Cuota = ((P_Tasa * EXP((P_Tasa + 1) , (P_Plazo - 1)) / (EXP((P_Tasa + 1) , (P_Plazo - 1)) - 1)) * (P_Monto * (1 + P_Tasa) + ((p_fecha - p_fecha1 - W_NroDias) * ((P_Monto * P_Tasa) / W_NroDias)))) / ((1 + (P_Tasa * EXP((P_Tasa + 1) , (P_Plazo - 1)) / (EXP((P_Tasa + 1) , (P_Plazo - 1)) - 1)))).

PROCEDURE rutina:
    fecPago = P_Fecha.
    fechaAnterior = p_fecha.
    W_SdoCapTra = P_Monto.

    DO nroCuota = 0 TO P_Plazo BY 1:
        IF nroCuota > 0 THEN DO:
            IF nroCuota > 1 THEN
                RUN Halla_FecVcto.R (INPUT P_Fecha,
                                     INPUT W_NroDias,
                                     INPUT fecPago,
                                     OUTPUT fecPago).

            fechaAnterior = fecPago.
        END.

        vCuota = P_Cuota.

        IF (nroCuota = 0) THEN DO:
            fechaAnterior = P_Fecha1.
            vCuota = 0.
        END.
        ELSE
            RUN P-AmortCuotaFija.

        IF W_SdoCapTra = 0 THEN
            LEAVE.
    END.

    RUN Deduc_Cre.
END.

PROCEDURE P-AmortCuotaFija:
    DEFINE VAR W_SdoAnt AS DECIMAL.
    
    /* Suma la cuota extra en caso que exista */
    FIND FIRST Temp_Extra WHERE Temp_Extra.fec_vcto > fechaAnterior
                        AND Temp_Extra.fec_vcto <= fecPago
                        AND Temp_Extra.fec_vcto > ADD-INTERVAL(fecPago,-1,"months") NO-LOCK NO-ERROR.
    IF AVAILABLE Temp_Extra THEN
        vCuota = p_cuota + Temp_Extra.Vr_CuoExtra.

    IF W_SdoCapTra > 0 THEN DO:
        IF nroCuota = 1 THEN
            abonoInteres = ((W_SdoCapTra * P_Tasa) / W_NroDias) * (p_fecha - p_fecha1).
        ELSE
            abonoInteres = (W_SdoCapTra * P_Tasa).
    END.

    W_SdoAnt = W_SdoCapTra.
    W_SdoCapTra = W_SdoCapTra - (vCuota - abonoInteres).
    
    /* Ajuste al plan de pagos por finalizar antes */
    IF W_SdoCapTra < 0 THEN DO:
        vCuota = abonoInteres + W_SdoAnt.
        W_SdoCapTra = 0.
    END.

    IF nroCuota = P_Plazo AND W_SdoCapTra <> 0 THEN DO:
        IF W_SdoCapTra < 0 THEN
            abonoInteres = abonoInteres + ABS(W_sdoCapTra).
        ELSE
            vCuota = abonoInteres + W_SdoAnt.

        W_SdoCapTra = 0.
    END.

END PROCEDURE.

PROCEDURE Deduc_Cre:
    DEFINE VAR W_TotVal AS DECIMAL.
    DEFINE VAR W_Vdd AS DECIMAL.
    DEFINE VAR W_Ndd AS INTEGER.
    DEFINE VAR W_Porcent AS DECIMAL.

    W_TotVal = 0.

    FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito = P_CodPro NO-LOCK NO-ERROR.
    IF AVAILABLE(Pro_Creditos) THEN DO:
        DO W_Ndd = 1 TO 10:
            IF Pro_Creditos.Deducible[W_Ndd] > "0000" THEN DO:
                FIND FIRST Deducible WHERE Deducible.Cod_Deducible = Pro_Creditos.Deducible[W_Ndd]
                                       AND Deducible.Estado = 1
                                       AND Deducible.Tip_Deducible = 1 NO-LOCK NO-ERROR.
                IF AVAILABLE(Deducible) THEN DO:
                    W_Vdd = Deducible.Valor.
                    W_Porcent = 0.

                    IF Deducible.Cla_Deducible = 1 THEN DO:
                        W_Porcent = Deducible.Valor.

                        W_Vdd = ROUND ((p_monto * Deducible.Valor) / 100 ,0).
                    END.

                    W_TotVal  = W_TotVal + W_Vdd.
                END.
            END.
        END.
    END.

END PROCEDURE.
