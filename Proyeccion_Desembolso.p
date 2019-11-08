DEFINE INPUT PARAMETER P_Age AS INTEGER.
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

/* oakley */

DEFINE INPUT PARAMETER P_NomPro AS CHARACTER FORMAT "X(40)".
DEFINE INPUT PARAMETER P_TipArc AS CHARACTER.
DEFINE INPUT PARAMETER P_NroDcto AS INTEGER.
DEFINE INPUT PARAMETER P_NomStma AS CHARACTER FORMAT "X(40)".
DEFINE INPUT PARAMETER P_NomPer AS CHARACTER FORMAT "X(14)".
DEFINE INPUT PARAMETER P_RForma AS INTEGER.
DEFINE INPUT PARAMETER P_VrSoli AS DECIMAL.

DEFINE SHARED VAR W_ManFin AS HANDLE.
DEFINE SHARED VAR W_Nom_Entidad AS CHARACTER.
DEFINE SHARED VAR W_Nom_Agencia AS CHARACTER.
DEFINE SHARED VAR W_Usuario AS CHARACTER.
DEFINE SHARED VAR W_Agencia AS INTEGER.

DEFINE VAR W_Ubicacion AS CHARACTER FORMAT "X(40)".
DEFINE VAR W_NomUsu AS CHARACTER.
DEFINE VAR W_PerTra AS INTEGER.
DEFINE VAR W_NroDias AS INTEGER.
DEFINE VAR W_SdoCapTra AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR W_CuoTra AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR W_FecTra AS DATE FORMAT "99/99/9999".
DEFINE VAR W_TasTra AS CHARACTER FORMAT "X(40)".
DEFINE VAR W_MonTra AS CHARACTER FORMAT "X(20)".
DEFINE VAR W_RazTra AS CHARACTER FORMAT "X(20)".
DEFINE VAR W_Anualidad AS CHARACTER FORMAT "X(20)".
DEFINE VAR W_NroPer AS INTEGER.
DEFINE VAR i AS INTEGER.
DEFINE VAR W_TitP AS CHARACTER FORMAT "X(16)" INITIAL "Nro.Solicitud  :".
DEFINE VAR INT_MGracia AS DECIMAL.
DEFINE VAR W_TotGra AS DECIMAL.
DEFINE VAR W_NMeses AS INTEGER.
DEFINE VAR W_Primero AS LOGICAL INITIAL YES.
DEFINE VAR W_FecIni AS DATE.

DEFINE TEMP-TABLE W_TabAmor
    FIELD W_Periodo AS INTEGER
    FIELD W_Fecha AS DATE FORMAT "99/99/9999"
    FIELD W_FecIniP AS DATE
    FIELD W_Cuota AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_AboCap AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_AboInt AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9"
    FIELD W_SdoCap AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".

DEFINE VAR W_TotInt AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR W_TotCap AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".

RUN RUTFINAN.R PERSISTENT SET W_MANFIN.

RUN HallarPeriodo IN W_ManFin (INPUT P_Perded,
                               INPUT P_Plazo,
                               OUTPUT W_NroDias,
                               OUTPUT W_NMeses,
                               OUTPUT W_NroPer,
                               OUTPUT P_NomPer).

RUN P-Amortizacion.

PROCEDURE P-Amortizacion:
    DEF VAR Linea AS CHAR FORMAT "X(120)" INIT "".
    DEF VAR Raya AS CHAR INIT "-".
    DEF VAR W_Cliente AS CHAR FORMAT "X(40)".

    W_Cliente = P_Nit + " - " + P_Nombre.
    W_Ubicacion = STRING(W_Agencia,"999") + " - " + W_Nom_Agencia.

    IF (P_TipArc = "A") THEN
        W_TitP = "Nro. Asesoría:".

    DEF FRAME F-Encabezado HEADER
        W_Nom_Entidad   AT 28 FORMAT "X(30)"
        "PAGINA:"       AT 70 PAGE-NUMBER FORMAT ">>>9" 
        "AMORTIZACIONES DEL PRESTAMO" AT 28 SKIP(1)
        "---------------------------------------------------------------------------------------------------" AT 1
        "Agencia        :" AT 1
        W_Ubicacion        AT 18
        "Asesor         :" AT 61
        W_NomUsu           AT 78
        "---------------------------------------------------------------------------------------------------" AT 1
        "Nit            :" AT 1
        W_Cliente         AT 18
        W_TitP            AT 1
        P_NroDcto         AT 18
        "Producto       :" AT 61
        P_NomPro          AT 78 
        "Monto          :" AT 1
        W_MonTra          AT 18 FORMAT "X(20)"
        "Tasa           :" AT 61
        W_TasTra          AT 78 FORMAT "X(50)"
        "Plazo          :" AT 1
        P_Plazo           AT 18
        "Cuota          :" AT 61
        W_Anualidad       AT 78 FORMAT "X(20)"
        "Stma.Utilizado :" AT 1
        P_NomStma         AT 18 FORMAT "X(35)"
        "Razón          :" AT 61
        W_RazTra          AT 78 /*FORMAT "X(20)"*/
        "Período Gracia :" AT 1
        P_Gracia          AT 18 FORMAT ">>,>>>" SKIP(1)
        "---------------------------------------------------------------------------------------------------" AT 1
    WITH 1 DOWN WIDTH 132 USE-TEXT FRAME F-Encabezado PAGE-TOP STREAM-IO NO-BOX NO-LABEL.

    DEF FRAME F-Detalle1 WITH DOWN WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.
    DEF FRAME F-Detalle2 WITH DOWN WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.
    
    DEF FRAME F-PiePagina HEADER
        "FECHA:"   AT 2
        TODAY     AT 10 FORMAT "99/99/9999"
        W_Nom_Agencia AT 28 FORMAT "X(30)"
        "HORA:"    AT 70 STRING(TIME,"HH:MM AM")
    WITH DOWN WIDTH 132 NO-BOX FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO.

    FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario
                    AND Usuarios.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
    IF AVAIL(Usuarios) THEN
        W_NomUsu = Usuarios.Nombre.

    ASSIGN Linea = FILL(Raya,120)
           W_MonTra = TRIM(STRING(P_Monto,"->>,>>>,>>>,>>>,>>9")).

    IF P_Sistema <> 8 THEN
        W_RazTra = TRIM(STRING(P_Razon,"->>,>>>,>>>,>>>,>>9.99")).
    ELSE
        W_RazTra = " ".

    W_Anualidad = TRIM(STRING(P_Cuota,"->>,>>>,>>>,>>>,>>9")).

    IF P_TipInt = 1 THEN
        W_TasTra = TRIM(STRING(P_Tasa * 100,">>9.99") + "% " + P_NomPer + " Vencido").
    ELSE
        W_TasTra = TRIM(STRING(P_Tasa * 100,">>9.99") + "% " + P_NomPer + " Anticipado").

    VIEW FRAME F-Encabezado.
    VIEW FRAME F-PiePagina.

    FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ P_CodPro NO-LOCK NO-ERROR.
    IF AVAIL(Pro_Creditos) AND Pro_Creditos.Id_PerGracia AND Dia_Gracia GT 0 THEN
        ASSIGN INT_MGracia = ROUND((((P_Monto * P_Tasa) / W_NroDias) * Dia_Gracia) / P_Plazo,0).

    FIND FIRST Creditos WHERE Creditos.Nit EQ P_Nit
                          AND Creditos.Num_Credito EQ P_NroDcto NO-LOCK NO-ERROR.

    CREATE W_TabAmor.
    ASSIGN W_TotInt = 0
           W_TotCap = 0
           W_FecTra = P_Fecha
           W_FecIni = P_Fecha
           W_TabAmor.W_AboInt = 0       
           W_TabAmor.W_Cuota = 0
           W_TabAmor.W_AboCap = 0
           W_TabAmor.W_SdoCap = P_Monto
           W_CuoTra = P_Cuota
           W_SdoCapTra = P_Monto.

    IF DAY(W_FecIni) EQ 31 THEN
        ASSIGN W_FecIni = W_FecIni - 1
               W_FecTra = W_FecIni.

    DO W_PerTra = 0 TO P_Plazo BY 1:                    /* No cambia la primera fecha william 10-12-2008 */
        IF P_Sistema <> 2 AND W_PerTra > 0 THEN DO:
            W_TabAmor.W_FecIniP = W_FecTra.
            IF W_PerTra > 1 THEN
               RUN CambiarFecha.

            W_TabAmor.W_Fecha = W_FecTra.
        END.

        IF P_Sistema EQ 1 OR P_Sistema EQ 2 THEN
            W_TabAmor.W_Cuota = P_Cuota.
        ELSE
            IF P_Sistema EQ 3 THEN
                W_TabAmor.W_Cuota = P_Monto * P_Tasa.

        ASSIGN W_TabAmor.W_Periodo = W_PerTra.

        IF (W_PerTra = 0) THEN DO:
            ASSIGN W_TabAmor.W_Cuota = 0
                   W_TabAmor.W_Fecha = P_Fecha1.

            IF P_TipInt = 2 THEN
                ASSIGN W_TabAmor.W_AboInt = P_Monto * P_Tasa.
        END.
        ELSE DO:
            IF P_Sistema EQ 1 THEN RUN
                P-AmortCuotaFija.
            ELSE
                IF P_Sistema EQ 2 THEN DO:
                    W_NroDias = (P_Plazo * W_NroDias).

                    IF W_PerTra NE 1 THEN
                        W_TabAmor.W_FecIniP = W_FecTra. /* ojo nh */

                    /*RUN CambiarFecha.*/

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
                                ASSIGN W_TabAmor.W_Cuota = P_Monto W_TabAmor.W_AboInt = 0.

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
                                            W_TabAmor.W_Cuota = (P_Monto / P_Plazo).
                                    END.
        END.

        ASSIGN W_TotInt = W_TotInt + W_TabAmor.W_AboInt
               W_TotGra = W_TotGra + INT_MGracia WHEN W_PerTra NE 0
               W_TotCap = W_TotCap + W_TabAmor.W_AboCap.

        IF W_PerTra < P_Plazo THEN
            CREATE W_TabAmor.

        I = I + 1.
    END.

    FIND FIRST amortizacion WHERE amortizacion.nit = p_nit
                              AND amortizacion.num_credito = P_NroDcto NO-LOCK NO-ERROR.
    IF NOT AVAILABLE amortizacion THEN DO:
        FOR EACH W_TabAmor:   /* WITH FRAME F-Amortizacion:*/
            RUN Crear_PLan(INPUT W_TabAmor.W_Periodo,
                           INPUT W_TabAmor.W_FecIniP,
                           INPUT W_TabAmor.W_Fecha,
                           INPUT W_TabAmor.W_Cuota,
                           INPUT W_TabAmor.W_Abocap,
                           INPUT W_TabAmor.W_AboInt,
                           INPUT W_TabAmor.W_SdoCap).

            DISP W_TabAmor.W_Periodo     /*AT  3*/   FORMAT ">>>9"              LABEL "PdoN"
                 W_TabAmor.W_Fecha       /*AT 13*/   FORMAT "99/99/9999"        LABEL "Vencimiento"
                 W_TabAmor.W_Cuota       /*AT 25*/   FORMAT "->>,>>>,>>>,>>9"   LABEL "Vr.Cuota Total"
                 INT_MGracia   /*AT 41*/   FORMAT "->>,>>>,>>>,>>9"   LABEL "Int-Pdo.Gracia" WHEN W_TabAmor.W_Periodo GT 0
                 W_TabAmor.W_AboInt      /*AT 57*/   FORMAT "->>,>>>,>>>,>>9"   LABEL "Abono Interés"
                 W_TabAmor.W_AboCap      /*AT 73*/   FORMAT "->>,>>>,>>>,>>9"   LABEL "Abono Capital"
                 W_TabAmor.W_SdoCap      /*AT 89*/   FORMAT "->>,>>>,>>>,>>9"   LABEL "Saldo Capital"
                WITH DOWN WIDTH 132 FRAME F-DetallCU USE-TEXT STREAM-IO NO-LABELS NO-BOX.
        END.

        IF W_TotInt <> 0 THEN DO:
            DISP W_TotGra AT 32 FORMAT "->>,>>>,>>>,>>9"
                 W_TotInt AT 48 FORMAT "->>,>>>,>>>,>>9"
                 W_TotCap AT 64 FORMAT "->>,>>>,>>>,>>9"
                WITH DOWN WIDTH 132 USE-TEXT STREAM-IO NO-LABELS.
        END.
        
        PAGE.
    END.
    ELSE DO:
        W_TotInt = 0.
        W_TotCap = 0.

        FOR EACH amortizacion WHERE amortizacion.nit = p_nit
                                AND amortizacion.num_credito = P_NroDcto NO-LOCK BY amortizacion.fec_pago:
            DISPLAY amortizacion.nro_cuota      FORMAT ">>>9"               LABEL "PdoN"
                    amortizacion.fec_pago       FORMAT "99/99/9999"         LABEL "Vencimiento"
                    amortizacion.cuota          FORMAT "->>,>>>,>>>,>>9"    LABEL "Vr.Cuota Total"
                    0                           FORMAT "->>,>>>,>>>,>>9"    LABEL "Int-Pdo.Gracia"
                    amortizacion.cuota_i        FORMAT "->>,>>>,>>>,>>9"    LABEL "Abono Interés" 
                    amortizacion.cuota_k        FORMAT "->>,>>>,>>>,>>9"    LABEL "Abono Capital"
                    amortizacion.sdo_capital    FORMAT "->>,>>>,>>>,>>9"    LABEL "Saldo Capital"
                WITH DOWN WIDTH 132 FRAME F-Amortizacion USE-TEXT STREAM-IO NO-LABELS NO-BOX.

            W_TotInt = W_TotInt + amortizacion.cuota_i.
            W_TotCap = W_TotCap + amortizacion.cuota_k.
        END.

        DISPLAY 0           AT 32   FORMAT "->>,>>>,>>>,>>9"
                W_TotInt    AT 48   FORMAT "->>,>>>,>>>,>>9"
                W_TotCap    AT 64   FORMAT "->>,>>>,>>>,>>9"
            WITH DOWN WIDTH 132 USE-TEXT STREAM-IO NO-LABELS.
    
        PAGE.
    END.

END PROCEDURE.

PROCEDURE P-AmortCuotaConstante:
    W_TabAmor.W_AboCap = (P_Monto / P_Plazo).
    IF P_TipInt = 1
       THEN RUN HCCC IN W_ManFin (INPUT P_Monto,INPUT P_Plazo,INPUT W_PerTra,INPUT P_Tasa,OUTPUT W_TabAmor.W_Cuota).
       ELSE W_TabAmor.W_Cuota = ((W_SdoCapTra - W_TabAmor.W_AboCap) * P_Tasa) + W_TabAmor.W_AboCap.
    ASSIGN W_TabAmor.W_AboInt = W_TabAmor.W_Cuota  - W_TabAmor.W_AboCap
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
    DEFI VAR W_CuoExT LIKE Creditos.Cuota INIT 0.

    W_CuoExT = P_Cuota.

    IF P_TotExt GT 0 THEN DO:
        FIND FIRST Extras WHERE Extras.Nit EQ P_Nit
                            AND Extras.Num_Solicitud EQ Creditos.Num_Solicitud
                            AND extras.cod_credito = creditos.cod_credito
                            AND Extras.Nro_Cuota EQ W_TabAmor.W_Periodo
                            AND Extras.Estado EQ 1 NO-LOCK NO-ERROR.
        IF AVAIL(Extras) THEN
            W_CuoExT = P_Cuota + Extras.Vr_CuoExtra.
    END.

    IF P_TipInt = 1 AND W_SdoCapTra GT 0 THEN
    DO:
        IF w_pertra = 1 THEN
        DO:
            W_TabAmor.W_AboInt = ((W_SdoCapTra * P_Tasa) / W_NroDias) * (p_fecha - p_fecha1).
        END.
        ELSE
        DO:
            W_TabAmor.W_AboInt = (W_SdoCapTra * P_Tasa).
        END.
    END.
    ELSE
        IF W_SdoCapTra GT 0 THEN
            W_TabAmor.W_AboInt = ((W_SdoCapTra - W_CuoExT) * P_Tasa).
/*
    ASSIGN W_SdoAnt = W_SdoCapTra
           W_Interes = W_TabAmor.W_AboInt
           W_TabAmor.W_Cuota = W_CuoExT.

    IF W_SdoCapTra GT 0 THEN
        ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_Cuota - (W_TabAmor.W_AboInt + INT_MGracia)
               W_SdoCapTra = W_SdoCapTra - W_TabAmor.W_AboCap
               W_TabAmor.W_SdoCap = W_SdoCapTra.
*/

/******************************/

    ASSIGN W_SdoAnt = W_SdoCapTra
           W_Interes = W_TabAmor.W_AboInt
           W_TabAmor.W_Cuota = W_CuoExt
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

/******************************/


    /* Ajuste a la última cuota - 27/05/2010 - */
    IF W_PerTra = P_Plazo AND W_TabAmor.W_SdoCap <> 0 THEN DO:
        IF W_TabAmor.W_SdoCap < 0 THEN
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap - ABS(W_TabAmor.W_SdoCap)
                   W_TabAmor.W_AboInt = W_TabAmor.W_AboInt + ABS(W_TabAmor.W_SdoCap).
        ELSE
            ASSIGN W_TabAmor.W_AboCap = W_TabAmor.W_AboCap + W_TabAmor.W_SdoCap
                   W_TabAmor.W_AboInt = W_TabAmor.W_AboInt - W_TabAmor.W_SdoCap.
        W_TabAmor.W_SdoCap = 0.   /******************************/
    END.

END PROCEDURE.
  
  PROCEDURE P-AmortizacionGradiente:
    DEF VAR W_Dia    AS INT INIT 0.
    IF W_PerTra  > 1 AND P_Sistema <> 7 THEN DO:
       IF P_Sistema EQ 5 THEN W_CuoTra  = W_CuoTra + P_Razon.
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
            W_Dia = ((YEAR(W_FecTra) - YEAR(P_Fecha)) * 360 + (MONTH(W_FecTra) - MONTH(P_Fecha)) *  30).
            IF W_Dia > 0 AND W_Dia MODULO 360 = 0 AND I MODULO W_NroPer = 0 THEN ASSIGN W_CuoTra  = W_CuoTra * P_Razon.
        END.
        ELSE DO:        
            W_Dia = ((YEAR(W_FecTra) - YEAR(P_Fecha)) * 360 + (MONTH(W_FecTra) - MONTH(P_Fecha)) *  30).
            IF W_Dia > 0 AND W_Dia MODULO 360 = 0  THEN ASSIGN W_CuoTra  = W_CuoTra * P_Razon.
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
    ELSE IF P_Gracia > W_PerTra THEN
            RETURN.
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
  /*--------------------             
    Marzo 24/06 GAER, Comentariado se cambio por El Run*/
    RUN Halla_FecVcto.R (INPUT W_FecIni,W_NroDias,W_FecTra,OUTPUT W_FecTra).
  END PROCEDURE.

PROCEDURE Deduc_Cre:
   DEF VAR W_TotVal        LIKE Deducible.Valor INIT 0.
   DEF VAR W_Vdd           LIKE Deducible.Valor.
   DEF VAR W_Ndd           AS INT FORMAT "99".
   DEF VAR W_Porcent       LIKE Deducible.Valor.
   DEF VAR Linea1          AS CHAR FORMAT "X(120)" INIT "".
   
   DISP SKIP(2) Linea1 AT 3 FORMAT "X(100)" WITH WIDTH 132 USE-TEXT STREAM-IO NO-LABELS.
   DISP "DEDUCIBLES:"  AT 3 "DESCRIPCION" AT 23 "VALOR DEDUCIBLE    % APLICADO" AT 55 SKIP WITH WIDTH 132 USE-TEXT STREAM-IO NO-LABELS.
   
   W_TotVal = 0.
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
   
   DISP SKIP(1) "TOTAL DEDUCIBLES => " AT 3 W_TotVal * 100 AT 55 FORMAT ">>>,>>>,>>>,>>9" SKIP WITH FRAME F-Detalle5 NO-LABELS.

END PROCEDURE.

PROCEDURE Imp_Deducibles:
  DEF INPUT PARAMETER P_Porcent    LIKE Deducible.Valor.
  DEF INPUT PARAMETER P_VlrPorc    LIKE Deducible.Valor.

  DISP Deducible.Nom_Deducible AT 23 FORMAT "X(30)"
       P_VlrPorc   * 100       AT 55 FORMAT ">>>,>>>,>>>,>>9" 
       P_Porcent               AT 75 FORMAT "%>>>9.9999"
      WITH WIDTH 150 FRAME F-Detalle3 NO-LABELS.

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

    IF W_Primero THEN
        W_FecAnt = TODAY.

    IF P_TipInt EQ 1 AND W_Periodo EQ 0 THEN
        RETURN.

    /*CREATE PlanPagos.
    ASSIGN PlanPagos.Agencia = p_age    /* se cambio jjmp 23102007  W_Agencia*/
           PlanPAgos.Nit = P_Nit
           PlanPagos.Num_Credito = P_NroDcto
           PlanPagos.Cod_Credito = P_CodPro
           PlanPagos.Fec_Ini = W_FecAnt
           PlanPagos.Fec_Vcto = W_Fecha
           PlanPagos.Fec_ProxPago = W_Fecha
           PlanPagos.Nro_Cuota = W_Periodo
           PlanPagos.Cuota = W_Cuota
           PlanPagos.Tasa = P_Tasa
           PlanPagos.Plazo = P_Plazo
           PlanPagos.Monto = P_Monto
           PlanPagos.Id_PdoMes = 0.

    CREATE CONTROL_pagos.
    ASSIGN control_pagos.Agencia = p_age
           control_pagos.Cod_Credito = P_CodPro
           control_pagos.Cuota = W_Cuota
           control_pagos.Fec_Inic = W_FecAnt
           control_pagos.Fec_Vcto = W_Fecha
           control_pagos.Nit = P_Nit
           control_pagos.Nro_Cuota = W_Periodo
           control_pagos.Num_Credito = P_NroDcto
           control_pagos.Id_PdoMes = 0
           control_pagos.Plazo = P_Plazo
           control_pagos.Tasa = P_Tasa.*/

    IF W_Primero THEN
        ASSIGN /*PlanPagos.Id_PdoMes = 1
               control_pagos.Id_PdoMes = 1
               PlanPagos.Fec_Ini = TODAY
               control_pagos.Fec_Inic = TODAY
               PlanPagos.Fec_ProxPago = W_Fecha
               CONTROL_pagos.fec_Vcto = w_fecha*/
               W_Primero = NO.

    /*CREATE amortizacion.
    amortizacion.cuota = w_cuota.
    amortizacion.cuota_i = W_aboInt.
    amortizacion.cuota_k = W_AboCap.
    amortizacion.fec_pago = w_fecha.
    amortizacion.nit = P_Nit.
    amortizacion.novedad = "Desembolso Inicial".
    amortizacion.nro_cuota = w_periodo.
    amortizacion.num_credito = P_NroDcto.
    amortizacion.sdo_capital = W_SdoCap.

    IF w_periodo = 1 THEN
        amortizacion.novedad = "Desembolso inicial".*/

END PROCEDURE.
