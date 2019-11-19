DEFINE INPUT PARAMETER P_Monto AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE INPUT PARAMETER P_Plazo AS INTEGER FORMAT "->>>>9".
DEFINE INPUT PARAMETER P_Cuota AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE INPUT PARAMETER P_TotExt AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE INPUT PARAMETER P_Fecha1 AS DATE.
DEFINE INPUT PARAMETER P_Fecha AS DATE.
DEFINE INPUT PARAMETER P_Tasa AS DECIMAL FORMAT "999.9999999".
DEFINE INPUT PARAMETER P_Razon AS INTEGER.
DEFINE INPUT PARAMETER P_Gracia AS INTEGER.
DEFINE INPUT PARAMETER P_PerDed AS INTEGER.
DEFINE INPUT PARAMETER P_TipInt AS INTEGER.
DEFINE INPUT PARAMETER P_Sistema AS INTEGER.
DEFINE INPUT PARAMETER P_Nit AS CHARACTER.
DEFINE INPUT PARAMETER P_Nombre AS CHARACTER FORMAT "X(40)".
DEFINE INPUT PARAMETER P_CodPro AS INTEGER.
DEFINE INPUT PARAMETER P_NomPro AS CHARACTER FORMAT "X(40)". 
DEFINE INPUT PARAMETER P_TipArc AS CHARACTER.
DEFINE INPUT PARAMETER P_NroDcto AS INTEGER.
DEFINE INPUT PARAMETER P_NomStma AS CHARACTER FORMAT "X(40)".
DEFINE INPUT PARAMETER P_NomPer AS CHARACTER FORMAT "X(14)".
DEFINE INPUT PARAMETER P_RForma AS INTEGER.
DEFINE INPUT PARAMETER P_VrSoli AS DECIMAL.

DEFINE VAR W_NMeses AS INTEGER.

DEFINE SHARED VAR W_ManFin AS HANDLE.
DEFINE SHARED VAR W_Nom_Entidad AS CHARACTER.
DEFINE SHARED VAR W_Nom_Agencia AS CHARACTER.
DEFINE SHARED VAR W_Usuario AS CHARACTER.
DEFINE SHARED VAR W_Agencia AS INTEGER.

/* oakley */

DEF VAR W_VlrInteres LIKE Creditos.Cuota INIT 0.
DEF VAR W_NomUsu LIKE Usuarios.Nombre.
DEF VAR W_PerTra AS INT INIT 0.
DEF VAR W_NroDias AS INT INIT 0. 
DEF VAR W_SdoCapTra AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".  
DEF VAR W_CuoTra AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_VlrPte AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_TvrPte AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_FecTra AS DATE FORMAT "99/99/9999".
DEF VAR W_VlrExt AS DEC INIT 0.
DEF VAR W_TasTra AS CHAR FORMAT "X(40)" INIT "".
DEF VAR W_MonTra AS CHAR FORMAT "X(20)" INIT "".
DEF VAR W_RazTra AS CHAR FORMAT "X(20)" INIT "".
DEF VAR W_Anualidad AS CHAR FORMAT "X(20)" INIT "".
DEF VAR W_NroPer AS INT INIT 0.
DEF VAR I AS INT INIT 0.
DEF VAR W_TitP AS CHAR FORMAT "X(16)" INIT "Nro.Solicitud  :".

DEF TEMP-TABLE W_TabAmor
    FIELD W_Periodo AS INT INIT 0
    FIELD W_Fecha AS DATE FORMAT "99/99/9999"
    FIELD W_CuoEx AS DEC INIT 0 FORMAT "->>>>,>>>,>>>,>>9"
    FIELD W_Cuota AS DEC INIT 0 FORMAT "->>>>,>>>,>>>,>>9"
    FIELD W_AboCap AS DEC INIT 0 FORMAT "->>>>,>>>,>>>,>>9"
    FIELD W_AboInt AS DEC INIT 0 FORMAT "->>>>,>>>,>>>,>>9"
    FIELD W_SdoCap AS DEC INIT 0 FORMAT "->>>>,>>>,>>>,>>9".

DEF TEMP-TABLE W_TabExt
    FIELD W_Fecha AS DATE FORMAT "99/99/9999"
    FIELD W_PlazExt LIKE Solicitud.Plazo
    FIELD W_Cuota AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".

DEF VAR W_TotInt AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9".
DEF VAR W_TotCap AS DEC INIT 0 FORMAT "->>>,>>>,>>>,>>>,>>9". 
DEF VAR W_TDeduc LIKE Pro_Creditos.Deducible.
DEF VAR W_VDeduc LIKE Solicitud.Deducible.
DEF VAR W_IDeduc LIKE Solicitud.Id_Adicionales.
DEF VAR W_MDeduc LIKE Solicitud.Monto.


RUN HallarPeriodo IN W_ManFin (INPUT P_Perded,
                               INPUT P_Plazo,
                               OUTPUT W_NroDias,
                               OUTPUT W_NMeses,
                               OUTPUT W_NroPer,
                               OUTPUT P_NomPer).

/*IF p_fecha = p_Fecha1 THEN
    p_Fecha = p_Fecha + W_NroDias.*/

RUN P-Amortizacion.

PROCEDURE P-Amortizacion:
    DEF VAR Linea AS CHAR FORMAT "X(120)" INIT "".
    DEF VAR Raya AS CHAR INIT "-".
    DEF VAR W_Cliente AS CHAR FORMAT "X(40)".

    /* oakley */

    W_Cliente = P_Nit + " - " + P_Nombre.
    
    IF (P_TipArc = "A") THEN
        W_TitP = "Nro. Asesoría:".

    DEF FRAME F-Encabezado HEADER
        W_Nom_Entidad   AT 18 FORMAT "X(30)"
        "PAGINA:"       AT 70 PAGE-NUMBER FORMAT ">>>9"
        "AMORTIZACIONES DEL PRESTAMO" AT 28 SKIP(1)
        "---------------------------------------------------------------------------------------------------" AT 1
        "Agencia        :" AT 1
        STRING(W_Agencia,"999") + " - " + W_Nom_Agencia AT 18
        "Asesor         :" AT 61
        W_NomUsu           AT 78
        "---------------------------------------------------------------------------------------------------" AT 1
        "Nit            :" AT 1
        W_Cliente          AT 18
        W_TitP             AT 1
        P_NroDcto          AT 18
        "Producto       :" AT 61
        P_NomPro           AT 78 
        "Monto          :" AT 1
        W_MonTra           AT 18 FORMAT "X(20)"
        "Tasa           :" AT 61
        W_TasTra           AT 78 FORMAT "X(50)"
        "Plazo          :" AT 1
        P_Plazo            AT 18
        "Cuota          :" AT 61
        W_Anualidad        AT 78 FORMAT "X(20)"
        "Stma.Utilizado :" AT 1
        P_NomStma          AT 18 FORMAT "X(35)"
        "Período Gracia :" AT 1
        P_Gracia           AT 18 FORMAT ">>,>>>" SKIP(1)
        "---------------------------------------------------------------------------------------------------" AT 1
        "Nro"            AT  3
        "Fecha"          AT  9
        "Cuota Extra"    AT 21
        "Vr.Cuota Total" AT 36
        "Abono Interés"  AT 54
        "Abono Capital"  AT 71
        "Saldo Capital"  AT 88
        Linea            AT  1  FORMAT "X(100)"
    WITH 1 DOWN WIDTH 132 USE-TEXT FRAME F-Encabezado PAGE-TOP STREAM-IO NO-BOX NO-LABEL.

    DEF FRAME F-Amortizacion
        W_TabAmor.W_Periodo   AT  1  FORMAT ">>>9"
        W_TabAmor.W_Fecha     AT  6  FORMAT "99/99/99"
        W_TabAmor.W_CuoEx     AT 15
        W_TabAmor.W_Cuota     AT 33
        W_TabAmor.W_AboInt    AT 50
        W_TabAmor.W_AboCap    AT 67
        W_TabAmor.W_SdoCap    AT 84
    WITH DOWN WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

    DEF FRAME F-Detalle1
    WITH DOWN WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

    DEF FRAME F-Detalle2
    WITH DOWN WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

    DEF FRAME F-PiePagina HEADER 
        "FECHA:"   AT 2
        TODAY      AT 10 FORMAT "99/99/9999"
        W_Nom_Agencia AT 28 FORMAT "X(30)"
        "HORA:"    AT 70 STRING(TIME,"HH:MM AM")
    WITH DOWN WIDTH 132 NO-BOX FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO.

    FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario
                    AND Usuarios.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
    IF AVAIL(Usuarios) THEN
        ASSIGN W_NomUsu = Usuarios.Nombre.

    ASSIGN Linea    = FILL(Raya,120)
           W_MonTra = TRIM(STRING(P_Monto,"->>,>>>,>>>,>>>,>>9")).
    
    IF P_Sistema <> 8 THEN
        W_RazTra = TRIM(STRING(P_Razon,"->>,>>>,>>>,>>>,>>9.99")).
    ELSE
        W_RazTra = " ".
        
    W_Anualidad = TRIM(STRING(P_Cuota,"->>,>>>,>>>,>>>,>>9")).
    /************************************************************************************************/


    IF P_TipInt = 1 THEN DO:
        W_TasTra = TRIM(STRING(P_Tasa * 100,">>9.99") + "% " + P_NomPer + " Vencido").

        IF p_codPro = 108 OR p_codPro = 113 OR p_codPro = 114 THEN
            W_TasTra = TRIM(STRING(P_Tasa * 100 * W_NroDias,">>9.99") + "% " + P_NomPer + " Vencido").
    END.
    ELSE
        W_TasTra = TRIM(STRING(P_Tasa * 100,">>9.99") + "% " + P_NomPer + " Anticipado").

    VIEW FRAME F-Encabezado.
    VIEW FRAME F-PiePagina.

    CREATE W_TabAmor.
    ASSIGN W_TotInt = 0
           W_TotCap = 0
           W_FecTra = P_Fecha
           W_TabAmor.W_AboInt = 0
           W_TabAmor.W_Cuota = 0
           W_TabAmor.W_AboCap = 0
           W_TabAmor.W_SdoCap = P_Monto
           W_CuoTra = P_Cuota
           W_SdoCapTra = P_Monto.



    DO W_PerTra = 0 TO P_Plazo BY 1:
        IF P_Sistema <> 2 AND W_PerTra > 0 THEN DO:
            IF W_PerTra > 1 THEN
                RUN CambiarFecha.
            W_TabAmor.W_Fecha = W_FecTra .
        END.
        
        IF P_Sistema = 1 AND P_TotExt > 0 THEN DO:
            W_VlrExt = 0.
            RUN AsentarExtras.
        END.

        IF P_Sistema EQ 1 OR P_Sistema EQ 2 THEN
            ASSIGN W_TabAmor.W_Cuota = P_Cuota.
        ELSE
            IF P_Sistema EQ 3 THEN
                ASSIGN W_TabAmor.W_Cuota = P_Monto * P_Tasa.

        ASSIGN W_TabAmor.W_Periodo = W_PerTra.

        IF (W_PerTra = 0) THEN DO:
            ASSIGN W_TabAmor.W_Cuota = 0
                   W_TabAmor.W_Fecha = P_Fecha1.

            IF P_TipInt = 2 THEN
                ASSIGN W_TabAmor.W_AboInt = P_Monto * P_Tasa.

        END.
        ELSE DO:
            IF P_Sistema EQ 1 THEN
                RUN P-AmortCuotaFija.
            ELSE
                IF P_Sistema EQ 2 THEN DO:
                    W_NroDias = (P_Plazo * W_NroDias).
                    RUN CambiarFecha.
                    W_TabAmor.W_Fecha = W_FecTra.

                    IF P_TipInt = 1 THEN
                        ASSIGN W_TabAmor.W_AboInt = P_Cuota - P_Monto
                               W_TotInt = W_TabAmor.W_AboInt.

                    ASSIGN W_TabAmor.W_AboCap = P_Monto
                           W_TabAmor.W_Fecha = W_FecTra
                           W_TotCap = P_Monto
                           W_TabAmor.W_SdoCap = 0
                           W_SdoCapTra = 0.

                    IF W_SdoCapTra = 0 THEN
                        LEAVE.
                END.
                ELSE
                    IF P_Sistema EQ 3 THEN DO:
                        ASSIGN W_TabAmor.W_AboInt = P_Monto * P_Tasa.

                        IF P_Plazo = W_PerTra THEN DO:
                            IF P_TipInt = 1 THEN
                                ASSIGN W_TabAmor.W_Cuota = W_TabAmor.W_Cuota + P_Monto.
                            ELSE
                                ASSIGN W_TabAmor.W_Cuota = P_Monto
                                       W_TabAmor.W_AboInt = 0.

                            ASSIGN W_TabAmor.W_AboCap = P_Monto
                                   W_TabAmor.W_SdoCap = 0
                                   W_SdoCapTra = 0.
                        END.
                    END.
                    ELSE
                        IF P_Sistema EQ 4 THEN
                            RUN P-AmortCuotaConstante.
                        ELSE
                            IF P_Sistema GE 5 AND P_Sistema LE 7 THEN
                                RUN P-AmortizacionGradiente.
                            ELSE
                                IF P_Sistema EQ 8 THEN
                                    RUN P-AmortPeriodoGracia.
                                ELSE
                                    IF P_Sistema EQ 9 THEN DO:
                                        ASSIGN W_TabAmor.W_AboCap = (P_Monto / P_Plazo)
                                               W_SdoCapTra = (W_SdoCapTra - W_TabAmor.W_AboCap)
                                               W_TabAmor.W_SdoCap = W_SdoCapTra.

                                        IF W_PerTra = 1 AND P_TipInt = 1 THEN
                                            ASSIGN W_TabAmor.W_Cuota = (P_Monto / P_Plazo) + (P_Monto * P_Tasa)
                                                   W_TabAmor.W_AboInt = (W_TabAmor.W_Cuota - W_TabAmor.W_AboCap).
                                        ELSE
                                            ASSIGN W_TabAmor.W_Cuota = (P_Monto / P_Plazo).
                                    END.
        END.

        ASSIGN W_TotInt = W_TotInt + W_TabAmor.W_AboInt
               W_TotCap = W_TotCap + W_TabAmor.W_AboCap.

        IF W_PerTra <= P_Plazo THEN
            CREATE W_TabAmor.
        I = I + 1.
    END.


    IF W_TotInt <> 0 THEN DO:
        FOR EACH W_TabAmor WITH FRAME F-Amortizacion:
            DISP W_Periodo W_Fecha W_CuoEx W_Cuota W_AboCap W_AboInt W_SdoCap
                WITH DOWN WIDTH 132 USE-TEXT STREAM-IO NO-LABELS.
        END.

        DISP W_TotInt AT 47 FORMAT "->>,>>>,>>>,>>>,>>9"
             W_TotCap AT 66 FORMAT "->,>>>,>>>,>>>,>>9"
            WITH DOWN WIDTH 132 USE-TEXT STREAM-IO NO-LABELS.

        RUN Deduc_Cre.
    END.

    PAGE.
END PROCEDURE.

/*comentar el procedimiento*/
PROCEDURE AsentarExtras:
    IF P_Sistema = 1 AND P_TotExt > 0 THEN
        IF (P_TipArc = "A") THEN.
        ELSE DO:
            CREATE W_TabExt.
            FOR EACH Extras WHERE Extras.Nit EQ P_Nit
                              AND Extras.Cod_credito EQ P_CodPro
                              AND Extras.Num_Solicitud EQ P_NroDcto
                              AND Extras.Nro_Cuota EQ W_PerTra
                              AND Extras.Estado EQ 1
                              AND Extras.Vr_CuoExtra GT 0 NO-LOCK:
                ASSIGN W_VlrExt = W_VlrExt + Extras.Vr_CuoExtra
                       W_TabExt.W_PlazExt = Extras.Nro_Cuota.
            END.

            ASSIGN W_TabExt.W_Fecha = W_FecTra
                   W_TabExt.W_Cuota = W_VlrExt.
        END.
END PROCEDURE.

PROCEDURE P-AmortCuotaConstante:
    W_TabAmor.W_AboCap = (P_Monto / P_Plazo).

    IF P_TipInt = 1 THEN
        RUN HCCC IN W_ManFin (INPUT P_Monto,
                              INPUT P_Plazo,
                              INPUT W_PerTra,
                              INPUT P_Tasa,
                              OUTPUT W_TabAmor.W_Cuota).
    ELSE
        W_TabAmor.W_Cuota = ((W_SdoCapTra - W_TabAmor.W_AboCap) * P_Tasa) + W_TabAmor.W_AboCap.

    ASSIGN W_TabAmor.W_AboInt = W_TabAmor.W_Cuota - W_TabAmor.W_AboCap
           W_SdoCapTra        = W_SdoCapTra - W_TabAmor.W_AboCap
           W_TabAmor.W_SdoCap = W_SdoCapTra.
    IF W_PerTra = P_Plazo AND W_SdoCapTra <> 0 THEN DO: 
       IF W_SdoCapTra < 0
          THEN W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - ABS(W_SdoCapTra).
          ELSE W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + W_SdoCapTra.
       W_TabAmor.W_SdoCap = 0.
    END.
  END PROCEDURE.     
  
PROCEDURE P-AmortCuotaFija:
    DEF VAR W_SdoAnt LIKE Creditos.Monto.
    DEF VAR W_Interes LIKE Creditos.Monto.
    DEF VAR W_CuoyExt LIKE Creditos.Cuota.

    W_CuoyExt = P_Cuota.

    /* Suma la cuota extra en caso que exista */
    IF P_TotExt GT 0 THEN DO:
        FIND FIRST Extras WHERE Extras.Nit EQ P_Nit
                            AND Extras.Num_Solicitud EQ P_NroDcto
                            AND Extras.Nro_Cuota EQ W_TabAmor.W_Periodo
                            AND Extras.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL(Extras) THEN
            ASSIGN W_CuoyExt = P_Cuota + Extras.Vr_CuoExtra
                   W_TabAmor.W_CuoEx = Extras.Vr_CuoExtra.
    END.


    IF P_TipInt = 1 AND W_SdoCapTra GT 0 THEN DO:
        IF w_pertra = 1 THEN
            W_TabAmor.W_AboInt = ((W_SdoCapTra * P_Tasa) / W_NroDias) * (p_fecha - p_fecha1).
        ELSE
            W_TabAmor.W_AboInt = (W_SdoCapTra * P_Tasa).
    END.
    ELSE
        IF W_SdoCapTra GT 0 THEN
            W_TabAmor.W_AboInt = ((W_SdoCapTra - W_CuoyExT) * P_Tasa).

/*
    IF P_TipInt = 1 THEN
        W_TabAmor.W_AboInt = W_SdoCapTra * P_Tasa.
    ELSE
        W_TabAmor.W_AboInt = (W_SdoCapTra - W_CuoyExt) * P_Tasa.*/

    ASSIGN W_SdoAnt = W_SdoCapTra
           W_Interes = W_TabAmor.W_AboInt
           W_TabAmor.W_Cuota = W_CuoyExt
           W_TabAmor.W_AboCap = W_TabAmor.W_Cuota - W_TabAmor.W_AboInt
           W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap
           W_TabAmor.W_SdoCap = W_SdoCapTra.

    /* Ajuste al plan de pagos por fianlizar antes */
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
        ELSE
        DO: 
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap.
                   /*W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - W_TabAmor.W_SdoCap*/
                   W_TabAmor.w_cuota = W_TabAmor.W_AboInt + W_TabAmor.W_AboCap.
        END.
        W_TabAmor.W_SdoCap = 0.
    END.
END PROCEDURE.                         
  
  PROCEDURE P-AmortizacionGradiente:
    DEF VAR W_Dia    AS INT INIT 0.
    IF W_PerTra  > 1 AND P_Sistema <> 7 THEN DO:
       IF P_Sistema EQ 5 THEN       
          W_CuoTra  = W_CuoTra + P_Razon.  /* P_Razon está malo, le entra el número de dias de la primera cuota, arreglar si se usa este proc */
       ELSE
       IF P_Sistema EQ 6 THEN W_CuoTra  = W_CuoTra * P_Razon.
    END. 
    ASSIGN W_TabAmor.W_Cuota  = W_CuoTra
           W_TabAmor.W_AboInt = W_SdoCapTra * P_Tasa
           W_TabAmor.W_AboCap = W_TabAmor.W_Cuota - W_TabAmor.W_AboInt
           W_SdoCapTra        = W_SdoCapTra - W_TabAmor.W_AboCap
           W_TabAmor.W_SdoCap = W_SdoCapTra.
    
    /*IF W_PerTra = P_Plazo AND W_SdoCapTra <> 0 THEN DO: 
       IF W_SdoCapTra < 0 THEN 
          ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap - ABS(W_SdoCapTra)
                 W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + ABS(W_SdoCapTra).
       ELSE ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_SdoCapTra
                   W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - W_SdoCapTra.
       W_TabAmor.W_SdoCap = 0.
    END.*/

    IF (P_Sistema = 7) THEN DO:
        IF (P_PerDed = 1 OR P_PerDed = 2 OR P_PerDed = 3) THEN DO:
              W_Dia = ((YEAR(W_FecTra) - YEAR(P_Fecha)) * 360 + 
                      (MONTH(W_FecTra) - MONTH(P_Fecha)) *  30).
              IF W_Dia > 0 AND W_Dia MODULO 360 = 0 AND I MODULO W_NroPer = 0
                 THEN W_CuoTra  = W_CuoTra * P_Razon.
        END.
        ELSE DO:        
              W_Dia = ((YEAR(W_FecTra) - YEAR(P_Fecha)) * 360 + 
                      (MONTH(W_FecTra) - MONTH(P_Fecha)) *  30).
              IF W_Dia > 0 AND W_Dia MODULO 360 = 0 THEN W_CuoTra  = W_CuoTra * P_Razon.
        END.     
    END.
  END PROCEDURE.
  
  PROCEDURE P-AmortPeriodoGracia:
    IF P_Gracia = W_PerTra THEN DO:
       RUN HFDP IN W_ManFin (INPUT P_Monto,INPUT P_Tasa,INPUT P_Gracia,OUTPUT W_SdoCapTra).
       W_TotInt = W_SdoCapTra - P_Monto.
       P_Monto  = W_SdoCapTra.
       RETURN.
    END.                   
    ELSE
    IF P_Gracia > W_PerTra THEN RETURN.
    W_TabAmor.W_Cuota = P_Cuota.
    RUN HCCF IN W_ManFin (INPUT P_Monto,INPUT P_Cuota,INPUT W_PerTra - P_Gracia,INPUT P_Tasa,OUTPUT W_TabAmor.W_AboCap).
    ASSIGN W_TabAmor.W_AboInt = W_TabAmor.W_Cuota  - W_TabAmor.W_AboCap
           W_SdoCapTra        = W_SdoCapTra - W_TabAmor.W_AboCap
           W_TabAmor.W_SdoCap = W_SdoCapTra.
    IF W_PerTra = P_Plazo  AND W_SdoCapTra <> 0 THEN DO: 
       IF W_TabAmor.W_SdoCap < 0 THEN 
          ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap - ABS(W_TabAmor.W_SdoCap)
                 W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + ABS(W_TabAmor.W_SdoCap).
       ELSE ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap
                   W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - W_TabAmor.W_SdoCap.
       W_TabAmor.W_SdoCap = 0.
    END.
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
   
   DISP SKIP(2) Linea1 AT 3 FORMAT "X(100)" WITH WIDTH 132 USE-TEXT STREAM-IO NO-LABELS.
   DISP "DEDUCIBLES:"  AT 3 "DESCRIPCION" AT 23 "VALOR DEDUCIBLE    % APLICADO" AT 55 SKIP
   WITH WIDTH 132 USE-TEXT STREAM-IO NO-LABELS.

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
               IF W_IDeduc EQ 4
                  THEN ASSIGN W_Vdd = ROUND ((P_VrSoli  * Deducible.Valor) / 100 ,0).
                  ELSE ASSIGN W_Vdd = ROUND ((P_VrSoli * Deducible.Valor) / 100 ,0).
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
     DISP  Deducible.Nom_Deducible AT 23 FORMAT "X(30)"
              P_VlrPorc               AT 55 FORMAT ">>>,>>>,>>>,>>9" 
              P_Porcent               AT 75 FORMAT "%>>>9.9999" 
          WITH FRAME fnopor WIDTH 150  NO-LABELS.
  ELSE
     DISP  Deducible.Nom_Deducible AT 23 FORMAT "X(30)"
              P_VlrPorc  * 100        AT 55 FORMAT ">>>,>>>,>>>,>>9" 
              P_Porcent               AT 75 FORMAT "%>>>9.9999" 
          WITH WIDTH 150 FRAME F-Detalle3 NO-LABELS.
END PROCEDURE.
