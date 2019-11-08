DEFI TEMP-TABLE CopCre LIKE Creditos.

DEFI TEMP-TABLE TabMes
     FIELD Ano AS INTEG FORM "9999"
     FIELD Mes AS INTEG FORM "99"
     FIELD Pdo AS CHAR  FORM "X(12)"
     FIELD Vlr LIKE Creditos.Sdo_Capital INIT 0.

DEFI VAR TVlr LIKE Creditos.Sdo_Capital INIT 0.

DEFI VAR AnoC AS INTEG FORM "9999".
DEFI VAR MesC AS INTEG FORM "99".
DEFI VAR Pdo  AS CHAR  FORM "X(12)".

DEFI TEMP-TABLE TabMesI
     FIELD Ano AS INTEG FORM "9999"
     FIELD Mes AS INTEG FORM "99"
     FIELD Vlr LIKE Creditos.Sdo_Capital INIT 0.

OUTPUT TO C:\INFO_Utrahuilca\RecPxMeses.Txt.

FOR EACH Creditos WHERE Creditos.Sdo_Capital GT 0 AND Creditos.Estado EQ 2 NO-LOCK:
    FOR EACH Planpagos WHERE PlanPagos.Agencia     EQ Creditos.Agencia    
                        AND PlanPagos.Nit          EQ Creditos.Nit        
                        AND PlanPagos.Cod_Credito  EQ Creditos.Cod_Credito
                        AND PlanPagos.Num_Credito  EQ Creditos.Num_Credito 
                        AND PlanPagos.Id_PdoMes    LE 1 NO-LOCK
                            BY PlanPagos.Nro_Cuota:
        

        IF  PlanPagos.Fec_Vcto LE (TODAY + 31) THEN
            ASSIGN AnoC = YEAR(TODAY  + 28) 
                   MesC = MONTH(TODAY + 28)
                   Pdo  = "MES".
        ELSE IF PlanPagos.Fec_Vcto LE (TODAY + 360) THEN
            ASSIGN AnoC = YEAR(PlanPagos.Fec_Vcto)
                   MesC = MONTH(PlanPagos.Fec_Vcto)
                   Pdo  = "MES".
        ELSE IF PlanPagos.Fec_Vcto LE (TODAY + 540) THEN
            ASSIGN AnoC = YEAR(TODAY  + 500)
                   MesC = MONTH(TODAY + 500)
                   Pdo  = "13 Hasta 18".
        ELSE IF PlanPagos.Fec_Vcto LE (TODAY + 730) THEN
            ASSIGN AnoC = YEAR(TODAY  + 700)
                   MesC = MONTH(TODAY + 700)
                   Pdo  = "19 Hasta 24".
        ELSE 
            ASSIGN AnoC = YEAR(TODAY  + 1000)
                   MesC = MONTH(TODAY + 1000)
                   Pdo  = "Más de 24".
        
        FIND FIRST TabMes WHERE TabMes.Ano EQ AnoC 
                            AND TabMes.Mes EQ MesC NO-ERROR.
        IF NOT AVAIL(TabMes) THEN DO:
           CREATE TabMes.
           ASSIGN TabMes.Ano = AnoC
                  TabMes.Mes = MesC
                  TabMes.Pdo = pdo.
        END.

        ASSIGN TabMes.Vlr = TabMes.Vlr + Creditos.Cuota.
    END.
END.

DISPLAY "COOPERATIVA UTRAHUILCA       Recaudo-Cartera Esperado Prox.Meses"
       SKIP(1)
    WITH WIDTH 100 FRAME f2 NO-LABELS USE-TEXT STREAM-IO NO-BOX.

FOR EACH TabMes BY TabMes.Ano BY TabMes.Mes:
    TVlr = TVlr + TabMes.Vlr.    

    DISPLAY TabMes.Mes   LABEL "Mes"
            TabMes.Ano   LABEL "Año"
            tabMes.Pdo   LABEL "Rango"
            TabMes.Vlr   LABEL "Vr.Prox.Recaudo"
        WITH DOWN WIDTH 80 FRAME f1 NO-LABELS USE-TEXT STREAM-IO NO-BOX.
END.

DISPLAY SKIP
        "  TOTAL                    ----------------" SKIP
        "                     "
        Tvlr WITH WIDTH 100 FRAME f3 NO-LABELS USE-TEXT STREAM-IO NO-BOX.

OUTPUT CLOSE.
