DEFINE INPUT PARAMETER P_Monto AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE INPUT PARAMETER P_Plazo AS INTEGER FORMAT "->>>>9".
DEFINE INPUT PARAMETER P_Cuota AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE INPUT PARAMETER P_Fecha1 AS DATE.
DEFINE INPUT PARAMETER P_Fecha AS DATE.
DEFINE INPUT PARAMETER P_Tasa AS DECIMAL FORMAT "999.9999999".
DEFINE INPUT PARAMETER P_PerDed AS INTEGER.
DEFINE INPUT PARAMETER P_Sistema AS INTEGER.
DEFINE INPUT PARAMETER P_Nit AS CHARACTER.
DEFINE INPUT PARAMETER P_Nombre AS CHARACTER FORMAT "X(40)".
DEFINE INPUT PARAMETER P_CodPro AS INTEGER.
DEFINE INPUT PARAMETER P_NomPro AS CHARACTER FORMAT "X(40)".
DEFINE INPUT PARAMETER P_TipArc AS CHARACTER.
DEFINE INPUT PARAMETER P_NroDcto AS INTEGER.
DEFINE INPUT PARAMETER P_NomStma AS CHARACTER FORMAT "X(40)".
DEFINE INPUT PARAMETER P_VrSoli AS DECIMAL.

{INCLUIDO\VARIABLE.I "SHARED"}

DEFINE VAR W_NMeses AS INTEGER.

DEFINE VAR W_NomUsu AS CHARACTER.
DEFINE VAR W_PerTra AS INTEGER.
DEFINE VAR W_NroDias AS INTEGER.
DEFINE VAR W_SdoCapTra AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR W_FecTra AS DATE FORMAT "99/99/9999".
DEFINE VAR W_TasTra AS CHARACTER FORMAT "X(40)".
DEFINE VAR W_MonTra AS CHARACTER FORMAT "X(20)".
DEFINE VAR W_Anualidad AS CHARACTER FORMAT "X(20)".
DEFINE VAR W_NroPer AS INTEGER.
DEFINE VAR I AS INTEGER.
DEFINE VAR W_TitP AS CHARACTER FORMAT "X(11)" INITIAL "Solicitud :".
DEFINE VAR interesPendiente AS DECIMAL.

DEFINE TEMP-TABLE W_TabAmor
    FIELD W_Periodo AS INTEGER
    FIELD W_Fecha AS DATE FORMAT "99/99/9999"
    FIELD W_CuoEx AS DECIMAL FORMAT "->>>>,>>>,>>>,>>9"
    FIELD W_Cuota AS DEC INIT 0 FORMAT "->>>>,>>>,>>>,>>9"
    FIELD W_AboCap AS DEC INIT 0 FORMAT "->>>>,>>>,>>>,>>9"
    FIELD W_AboInt AS DEC INIT 0 FORMAT "->>>>,>>>,>>>,>>9"
    FIELD W_SdoCap AS DEC INIT 0 FORMAT "->>>>,>>>,>>>,>>9"
    FIELD descripcion AS CHARACTER FORMAT "X(30)".

DEFINE VAR W_TotInt AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR W_TotCap AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9". 
DEFINE VAR vNumCredito AS INTEGER.
DEFINE VAR P_NomPer AS CHARACTER.
DEFINE VAR fechaAnterior AS DATE.
DEFINE VAR tea AS DECIMAL.
DEFINE VAR tea_Chr AS CHARACTER.

FIND FIRST creditos WHERE creditos.nit = p_Nit
                      AND creditos.num_solicitud = P_NroDcto NO-LOCK NO-ERROR.
IF AVAILABLE creditos THEN
    vNumCredito = creditos.num_credito.

RUN HallarPeriodo IN W_Manfin (INPUT P_Perded,
                               INPUT P_Plazo,
                               OUTPUT W_NroDias,
                               OUTPUT W_NMeses,
                               OUTPUT W_NroPer,
                               OUTPUT P_NomPer).

IF p_codPro = 108 OR p_codPro = 113 OR p_codPro = 114 THEN
    RUN NVEF IN w_manfin (INPUT p_tasa * 360,
                          INPUT 360,
                          OUTPUT tea).
ELSE
    RUN NVEF IN w_manfin (INPUT p_tasa * w_NroPer,
                          INPUT w_nroPer,
                          OUTPUT tea).

RUN P-Amortizacion.

PROCEDURE P-Amortizacion:
    DEFINE VAR Linea AS CHARACTER FORMAT "X(120)".
    DEFINE VAR Raya AS CHAR INIT "-".
    DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(40)".
    
    W_Cliente = P_Nit + " - " + P_Nombre.

    IF (P_TipArc = "A") THEN
        W_TitP = "Asesoría  :".

    DEFINE FRAME F-Encabezado HEADER
        W_Nom_Entidad                   AT 18 FORMAT "X(30)"
        "Página:"                       AT 70 PAGE-NUMBER FORMAT ">>9"
        "Amortizaciones del préstamo"   AT 28 SKIP(1)
        "---------------------------------------------------------------------------------------------------" AT 1
        "Agencia        :" AT 1
        STRING(W_Agencia,"999") + " - " + W_Nom_Agencia AT 18
        "Asesor         :" AT 61
        W_NomUsu           AT 78
        "---------------------------------------------------------------------------------------------------" AT 1
        "Titular   :"               AT 1
        W_Cliente                   AT 13
        W_TitP                      AT 1
        STRING(P_NroDcto)           AT 13
        "Producto            :"     AT 56
        P_NomPro                    AT 78 
        "Monto     :"               AT 1
        W_MonTra                    AT 13 FORMAT "X(20)"
        "Tasa Efectiva Anual :"     AT 56
        tea_Chr                     AT 78 FORMAT "X(10)"
        "Plazo     :"               AT 1
        TRIM(STRING(P_Plazo))       AT 13
        "Tasa de Liquidación :"     AT 56
        W_TasTra                    AT 78 FORMAT "X(50)"
        "Sistema   :"               AT 1
        P_NomStma                   AT 13 FORMAT "X(35)"
        "Cuota               :"     AT 56
        W_Anualidad                 AT 78 FORMAT "X(20)"
        "---------------------------------------------------------------------------------------------------" AT 1
        "Nro"            AT  3
        "Fecha"          AT  9
        "Cuota Extra"    AT 21
        "Vr.Cuota Total" AT 36
        "Abono Interés"  AT 54
        "Abono Capital"  AT 71
        "Saldo Capital"  AT 88
        Linea            AT  1  FORMAT "X(100)"
    WITH 1 DOWN WIDTH 140 USE-TEXT FRAME F-Encabezado PAGE-TOP STREAM-IO NO-BOX NO-LABEL.

    DEFINE FRAME F-Amortizacion
        W_TabAmor.W_Periodo AT 1 FORMAT ">>>9"
        W_TabAmor.W_Fecha   AT 6 FORMAT "99/99/99"
        W_TabAmor.W_CuoEx   AT 15
        W_TabAmor.W_Cuota   AT 33
        W_TabAmor.W_AboInt  AT 50
        W_TabAmor.W_AboCap  AT 67
        W_TabAmor.W_SdoCap  AT 84
        W_TabAmor.descripcion FORMAT "X(30)"
        WITH DOWN WIDTH 140 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

    DEFINE FRAME F-Detalle1
        WITH DOWN WIDTH 140 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

    DEFINE FRAME F-Detalle2
        WITH DOWN WIDTH 140 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

    DEFINE FRAME F-PiePagina HEADER 
        "FECHA:"        AT 2
        TODAY           AT 10 FORMAT "99/99/9999"
        W_Nom_Agencia   AT 28 FORMAT "X(30)"
        "HORA:"         AT 70 STRING(TIME,"HH:MM AM")
        WITH DOWN WIDTH 140 NO-BOX FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO.

    FIND FIRST Usuarios WHERE Usuarios.Usuario = W_Usuario
                          AND Usuarios.Agencia = W_Agencia NO-LOCK NO-ERROR.
    IF AVAILABLE(Usuarios) THEN
        W_NomUsu = Usuarios.Nombre.

    ASSIGN Linea = FILL(Raya,120)
           W_MonTra = TRIM(STRING(P_Monto,"->>,>>>,>>>,>>>,>>9")).

    W_Anualidad = TRIM(STRING(P_Cuota,"->>,>>>,>>>,>>>,>>9")).
    W_TasTra = TRIM(STRING(P_Tasa * 100,">>9.99") + "% " + P_NomPer + " Vencido").
    tea_Chr = TRIM(STRING(tea * 100,">>9.99") + "% ").

    IF p_codPro = 108 OR p_codPro = 113 OR p_codPro = 114 THEN
        W_TasTra = TRIM(STRING(P_Tasa * 100 * 365 / W_NroPer,">>9.99") + "% " + P_NomPer + " Vencido").
    
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-PiePagina.

    W_TotInt = 0.
    W_TotCap = 0.
    W_FecTra = P_Fecha.
    
    CREATE W_TabAmor.
    W_TabAmor.W_AboInt = 0.
    W_TabAmor.W_Cuota = 0.
    W_TabAmor.W_AboCap = 0.
    W_TabAmor.W_SdoCap = P_Monto.
    W_TabAmor.descripcion = "Desembolso inicial".

    W_SdoCapTra = P_Monto.
    fechaAnterior = p_fecha.

    DO W_PerTra = 0 TO P_Plazo BY 1:
        IF P_Sistema <> 2 AND W_PerTra > 0 THEN DO:
            IF W_PerTra > 1 THEN
                RUN CambiarFecha.

            W_TabAmor.W_Fecha = W_FecTra.
        END.

        W_TabAmor.W_Cuota = P_Cuota.
        W_TabAmor.W_Periodo = W_PerTra.

        IF (W_PerTra = 0) THEN DO:
            W_TabAmor.W_Cuota = 0.
            W_TabAmor.W_Fecha = P_Fecha1.
        END.
        ELSE DO:
            IF P_Sistema = 1 THEN
                RUN P-AmortCuotaFija.
            ELSE DO:
                W_NroDias = (P_Plazo * W_NroDias).
                W_TabAmor.W_Fecha = W_FecTra.
                W_TabAmor.W_AboInt = P_Cuota - P_Monto.
                W_TotInt = W_TabAmor.W_AboInt.
                W_TabAmor.W_AboCap = P_Monto.
                W_TabAmor.W_Fecha = W_FecTra.
                W_TotCap = P_Monto.
                W_TabAmor.W_SdoCap = 0.
                
                LEAVE.
            END.
        END.

        ASSIGN W_TotInt = W_TotInt + W_TabAmor.W_AboInt
               W_TotCap = W_TotCap + W_TabAmor.W_AboCap.

        IF W_PerTra <= P_Plazo THEN
            CREATE W_TabAmor.

        I = I + 1.
    END.

    IF AVAILABLE credito THEN DO:
        FIND FIRST amortizacion WHERE amortizacion.nit = p_nit
                                  AND amortizacion.num_credito = vNumCredito NO-LOCK NO-ERROR.
        IF AVAILABLE amortizacion THEN DO:
            W_TotInt = 0.
            W_TotCap = 0.

            EMPTY TEMP-TABLE W_TabAmor.

            fechaAnterior = creditos.fec_pagAnti.
            
            FOR EACH amortizacion WHERE amortizacion.nit = p_nit
                                    AND amortizacion.num_credito = vNumCredito NO-LOCK BY amortizacion.fec_pago:

                CREATE W_TabAmor.
                W_TabAmor.W_Periodo = amortizacion.nro_cuota.
                W_TabAmor.W_Fecha = amortizacion.fec_pago.
                W_TabAmor.W_Cuota = amortizacion.cuota.
                W_TabAmor.W_AboCap = amortizacion.cuota_k.
                W_TabAmor.W_AboInt = amortizacion.cuota_i.
                W_TabAmor.W_SdoCap = amortizacion.sdo_capital.
                W_TabAmor.descripcion = amortizacion.novedad.

                /* Ajuste para controlar que no se muestren negativos al final del plan */
                IF W_TabAmor.W_SdoCap < 0 THEN DO:
                    W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap.
                    W_TabAmor.W_SdoCap = 0.
                    W_TabAmor.W_Cuota = W_TabAmor.W_AboCap + W_TabAmor.W_AboInt.
                END.

                FIND FIRST Extras WHERE Extras.Nit = P_Nit
                                    AND Extras.Num_Solicitud = P_NroDcto
                                    AND extras.fec_vcto > fechaAnterior
                                    AND extras.fec_vcto <= amortizacion.fec_pago
                                    AND extras.fec_vcto > ADD-INTERVAL(amortizacion.fec_pago,-1,"months") NO-LOCK NO-ERROR.
                IF AVAILABLE extras THEN
                    W_TabAmor.W_CuoEx = Extras.Vr_CuoExtra.

                W_TotInt = W_TotInt + W_TabAmor.W_AboInt.
                W_TotCap = W_TotCap + W_TabAmor.W_AboCap.

                fechaAnterior = amortizacion.fec_pago.

                IF W_TabAmor.W_SdoCap = 0 THEN
                    LEAVE.
            END.
        END.
    END.

    IF W_TotInt <> 0 THEN DO:
        FOR EACH W_TabAmor NO-LOCK WITH FRAME F-Amortizacion BY W_TabAmor.w_fecha
                                                             BY W_TabAmor.w_periodo:
            DISPLAY W_Periodo W_TabAmor.W_Fecha W_CuoEx W_TabAmor.W_Cuota W_AboCap W_AboInt W_SdoCap W_TabAmor.descripcion
                WITH DOWN WIDTH 140 USE-TEXT STREAM-IO NO-LABELS.
        END.

        DISPLAY W_TotInt AT 47 FORMAT "->>,>>>,>>>,>>>,>>9"
             W_TotCap AT 66 FORMAT "->,>>>,>>>,>>>,>>9"
            WITH DOWN WIDTH 140 USE-TEXT STREAM-IO NO-LABELS.

        RUN Deduc_Cre.
    END.

    PAGE.

END PROCEDURE.


PROCEDURE P-AmortCuotaFija:
    DEF VAR W_SdoAnt LIKE Creditos.Monto.
    DEF VAR W_CuoyExt LIKE Creditos.Cuota.

    W_CuoyExt = P_Cuota.

    /* Suma la cuota extra en caso que exista */
    /*IF w_perTra <> 1 THEN DO:
        FIND FIRST Extras WHERE Extras.Nit = P_Nit
                            AND Extras.Num_Solicitud = P_NroDcto
                            AND extras.fec_vcto > fechaAnterior
                            AND extras.fec_vcto <= w_fecTra
                            AND extras.fec_vcto > ADD-INTERVAL(w_fecTra,-1,"months") NO-LOCK NO-ERROR.
        IF AVAILABLE extras THEN
            W_TabAmor.W_CuoEx = Extras.Vr_CuoExtra.
    END.
    ELSE DO:
        /* Para la cuota 1 */
        FIND FIRST Extras WHERE Extras.Nit = P_Nit
                        AND Extras.Num_Solicitud = P_NroDcto
                        AND extras.fec_vcto <= w_fecTra NO-LOCK NO-ERROR.
        IF AVAILABLE extras THEN
            W_TabAmor.W_CuoEx = Extras.Vr_CuoExtra.
    END.*/

    FIND FIRST Extras WHERE Extras.Nit = P_Nit
                        AND Extras.Num_Solicitud = P_NroDcto
                        AND extras.nro_cuota = w_perTra NO-LOCK NO-ERROR.
    IF AVAILABLE extras THEN
        W_TabAmor.W_CuoEx = Extras.Vr_CuoExtra.


    
    IF W_SdoCapTra GT 0 THEN DO:
        IF w_pertra = 1 THEN
            W_TabAmor.W_AboInt = ((W_SdoCapTra * P_Tasa) / W_NroDias) * (p_fecha - p_fecha1).
        ELSE
            W_TabAmor.W_AboInt = (W_SdoCapTra * P_Tasa).

        interesPendiente = interesPendiente + W_TabAmor.W_AboInt.
        W_TabAmor.W_AboInt = interesPendiente.
        
        IF W_TabAmor.W_AboInt > W_CuoyExt + W_TabAmor.W_CuoEx THEN
            W_TabAmor.W_AboInt = W_CuoyExt + W_TabAmor.W_CuoEx.

        interesPendiente = interesPendiente - W_TabAmor.W_AboInt.
    END.
    ELSE
        IF W_SdoCapTra GT 0 THEN
            W_TabAmor.W_AboInt = ((W_SdoCapTra - W_CuoyExT) * P_Tasa).

    W_SdoAnt = W_SdoCapTra.
    W_TabAmor.W_Cuota = W_CuoyExt + W_TabAmor.W_CuoEx.
    W_TabAmor.W_AboCap = W_TabAmor.W_Cuota - W_TabAmor.W_AboInt.
    W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap.
    W_TabAmor.W_SdoCap = W_SdoCapTra.

    /* Ajuste al plan de pagos por finalizar antes */
    IF W_TabAmor.W_SdoCap < 0 THEN DO:
        W_TabAmor.w_cuota = W_TabAmor.W_AboInt + W_SdoAnt.
        W_TabAmor.W_AboCap = W_SdoAnt.
        W_SdoCapTra = 0.
        W_TabAmor.W_SdoCap = 0.
    END.

    IF W_PerTra = P_Plazo AND W_TabAmor.W_SdoCap <> 0 THEN DO:
        IF W_TabAmor.W_SdoCap < 0 THEN
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap - ABS(W_TabAmor.W_SdoCap)
                   W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + ABS(W_TabAmor.W_SdoCap).
        ELSE DO: 
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap.
                   /*W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - W_TabAmor.W_SdoCap*/
                   W_TabAmor.w_cuota = W_TabAmor.W_AboInt + W_TabAmor.W_AboCap.
        END.
        W_TabAmor.W_SdoCap = 0.
    END.

    fechaAnterior = W_TabAmor.w_fecha.

END PROCEDURE.                         
  
PROCEDURE CambiarFecha:
    RUN Halla_FecVcto.R (INPUT P_Fecha,W_NroDias,W_FecTra,OUTPUT W_FecTra).
END PROCEDURE.

PROCEDURE Deduc_Cre:
   DEF VAR W_TotVal  LIKE Deducible.Valor INIT 0.
   DEF VAR W_Vdd     LIKE Deducible.Valor.
   DEF VAR W_Ndd     AS INT FORMAT "99".
   DEF VAR W_Porcent LIKE Deducible.Valor.
   DEF VAR Linea1    AS CHA FORMAT "X(120)" INIT "".
   
   DISPLAY SKIP(2) Linea1 AT 3 FORMAT "X(100)" WITH WIDTH 132 USE-TEXT STREAM-IO NO-LABELS.
   DISPLAY "DEDUCIBLES:"  AT 3 "DESCRIPCION" AT 23 "VALOR DEDUCIBLE    % APLICADO" AT 55 SKIP
   WITH WIDTH 140 USE-TEXT STREAM-IO NO-LABELS.

   ASSIGN W_TotVal       = 0.
   FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ P_CodPro NO-LOCK NO-ERROR.
   IF AVAIL(Pro_Creditos) THEN DO W_Ndd = 1 TO 10:
      IF Pro_Creditos.Deducible[W_Ndd] GT "0000" THEN DO:
         FIND Deducible WHERE Deducible.Cod_Deducible EQ Pro_Creditos.Deducible[W_Ndd] 
                          AND Deducible.Estado        EQ 1 
                          AND Deducible.Tip_Deducible EQ 1 NO-LOCK NO-ERROR.
         IF AVAIL(Deducible) THEN DO:
            ASSIGN W_Vdd = Deducible.Valor W_Porcent = 0.
            IF Deducible.Cla_Deducible = 1 THEN DO:
               W_Porcent = Deducible.Valor.
               
               ASSIGN W_Vdd = ROUND ((P_VrSoli * Deducible.Valor) / 100 ,0).
               
            END.
            ASSIGN W_TotVal  = W_TotVal + W_Vdd.
            RUN Imp_Deducibles (INPUT W_Porcent, W_Vdd).
         END.
      END.
   END.
END PROCEDURE.

PROCEDURE Imp_Deducibles:
  DEF INPUT PARAMETER P_Porcent    LIKE Deducible.Valor.
  DEF INPUT PARAMETER P_VlrPorc    LIKE Deducible.Valor.
  /*nvo egom*/
  IF P_Porcent EQ 0 THEN
     DISPLAY  Deducible.Nom_Deducible AT 23 FORMAT "X(30)"
              P_VlrPorc               AT 55 FORMAT ">>>,>>>,>>>,>>9" 
              P_Porcent               AT 75 FORMAT "%>>>9.9999" 
          WITH FRAME fnopor WIDTH 150  NO-LABELS.
  ELSE
     DISPLAY  Deducible.Nom_Deducible AT 23 FORMAT "X(30)"
              P_VlrPorc  * 100        AT 55 FORMAT ">>>,>>>,>>>,>>9" 
              P_Porcent               AT 75 FORMAT "%>>>9.9999" 
          WITH WIDTH 150 FRAME F-Detalle3 NO-LABELS.
END PROCEDURE.
