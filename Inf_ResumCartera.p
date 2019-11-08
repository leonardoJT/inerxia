/* Programa Inf_ResumCartera.P.
   Resumen cartera por tipo-gtìa y edad*/
DEFI VAR Tot1 LIKE Creditos.Sdo_capital EXTENT 9 INIT 0 FORM "->>>>,>>>,>>>,>>9".   
DEFI VAR Tot2 LIKE Creditos.Sdo_capital EXTENT 9 INIT 0 FORM "->>>>,>>>,>>>,>>9".
DEFI VAR Tot3 LIKE Creditos.Sdo_capital EXTENT 9 INIT 0 FORM "->>>>,>>>,>>>,>>9".

DEFI VAR Nro  AS INTEG FORM "9999999"   EXTENT 3 INIT 0.
   
DEFI TEMP-TABLE CopCreditos LIKE Creditos
     FIELD Gtia AS CHAR FORM "X(2)"
     FIELD PagX AS CHAR FORM "X(2)"
     FIELD TotGar LIKE Garantias.Val_Bien INIT 0.

SESSION:SET-WAIT-STATE("General").

FOR EACH Creditos WHERE Sdo_Capital GT 0 NO-LOCK:
    CREATE CopCreditos.
    BUFFER-COPY Creditos TO CopCreditos.

    ASSIGN PagX = "SL"
           Gtia = "NA".

    IF Creditos.FOR_pago EQ 2 THEN
       PagX = "CL".

    FOR EACH Garantias WHERE Garantias.Agencia     EQ Creditos.Agencia
                         AND Garantias.Cod_credito EQ Creditos.Cod_credito NO-LOCK:
        IF  Garantias.Num_credito EQ Creditos.Num_credito
        AND Garantias.Estado      EQ 1
        AND Garantias.Val_Bien    GT 0 THEN
            ASSIGN TotGar = TotGar + Garantias.Val_Bien
                   Gtia   = "AD".
    END.
END.

OUTPUT TO C:\INFO_utrahuilca\ResumCartera_032005.Txt.
DISPLAY "             COOPERATIVA UTRAHUILCA - CLASIFICACIÒN CARTERA CONSOLIDADO"
        SKIP(2)
    WITH WIDTH 100.

FOR EACH CopCreditos BREAK BY CopCreditos.Tip_credito BY Gtia BY PagX BY CopCreditos.Categoria:
    IF FIRST-OF(PagX) THEN 
       RUN Tit1.

    ASSIGN Nro [1] = Nro [1] + 1
           Nro [2] = Nro [2] + 1
           Nro [3] = Nro [3] + 1
           Tot1[1] = Tot1[1] + CopCreditos.Sdo_capital
           Tot2[1] = Tot2[1] + CopCreditos.Sdo_capital
           Tot3[1] = Tot3[1] + CopCreditos.Sdo_capital
           Tot1[2] = Tot1[2] + CopCreditos.INT_Corrientes
           Tot2[2] = Tot2[2] + CopCreditos.INT_Corrientes
           Tot3[2] = Tot3[2] + CopCreditos.INT_Corrientes
           Tot1[3] = Tot1[3] + CopCreditos.Polizas
           Tot2[3] = Tot2[3] + CopCreditos.Polizas
           Tot3[3] = Tot3[3] + CopCreditos.Polizas
           Tot1[4] = Tot1[4] + CopCreditos.Costas
           Tot2[4] = Tot2[4] + CopCreditos.Costas
           Tot3[4] = Tot3[4] + CopCreditos.Costas
           Tot1[5] = Tot1[5] + CopCreditos.Provision
           Tot2[5] = Tot2[5] + CopCreditos.Provision
           Tot3[5] = Tot3[5] + CopCreditos.Provision
           Tot1[6] = Tot1[6] + CopCreditos.INT_DifCobro
           Tot2[6] = Tot2[6] + CopCreditos.INT_DifCobro
           Tot3[6] = Tot3[6] + CopCreditos.INT_DifCobro
           Tot1[8] = Tot1[8] + CopCreditos.INT_Anticip
           Tot2[8] = Tot2[8] + CopCreditos.INT_Anticip
           Tot3[8] = Tot3[8] + CopCreditos.INT_Anticip
           Tot1[7] = Tot1[7] + TotGar
           Tot2[7] = Tot2[7] + TotGar
           Tot3[7] = Tot3[7] + TotGar
           Tot1[9] = Tot1[9] + CopCreditos.INT_morcobrar
           Tot2[9] = Tot2[9] + CopCreditos.INT_morcobrar
           Tot3[9] = Tot3[9] + CopCreditos.INT_morcobrar.


    IF LAST-OF(CopCreditos.Categoria) THEN DO:
       DISPLAY CopCreditos.Categoria     LABEL "Calif."
               Nro [1]                   LABEL "T.Nros"
               Tot1[1]                   LABEL "Sdos-Capital"
               Tot1[2]                   LABEL "Int-Corrientes"
               Tot1[3]                   LABEL "Polizas"
               Tot1[4]                   LABEL "Costas-Jud"
               Tot1[5]                   LABEL "Provisión"
               Tot1[6]                   LABEL "Int-Nocausado"
               Tot1[7]                   LABEL "Garantia-Adm"
               Tot1[8]                   LABEL "Int-Anticipado"
               Tot1[9]                   LABEL "Int-Mor-Cobrar"
               SKIP(0)
           WITH DOWN WIDTH 170 FRAME FDetalle NO-BOX NO-LABELS STREAM-IO USE-TEXT.
       ASSIGN Nro [1] = 0
              Tot1[1] = 0
              Tot1[2] = 0
              Tot1[3] = 0
              Tot1[4] = 0
              Tot1[5] = 0
              Tot1[6] = 0
              Tot1[7] = 0
              Tot1[8] = 0
              Tot1[9] = 0.
    END.

    IF LAST-OF(CopCreditos.PagX) THEN DO:                                    
       RUN Tot1.
       ASSIGN Nro [2] = 0                                                         
              Tot2[1] = 0                                                         
              Tot2[2] = 0                                                         
              Tot2[3] = 0                                                         
              Tot2[4] = 0                                                         
              Tot2[5] = 0                                                         
              Tot2[6] = 0                                                         
              Tot2[7] = 0
              Tot2[8] = 0
              Tot2[9] = 0.                                                        
    END.                                                                          
END.

RUN TotalAgencia.

OUTPUT CLOSE.

ASSIGN Tot1 = 0
       Tot2 = 0
       Tot3 = 0          
       Nro  = 0.

OUTPUT TO C:\INFO_Utrahuilca\XOfiResumCartera_032005.Txt.
FOR EACH CopCreditos BREAK BY CopCreditos.Agencia BY CopCreditos.Tip_credito 
                           BY Gtia                BY PagX                    BY CopCreditos.Categoria:
    IF FIRST-OF(CopCreditos.Agencia) THEN
       RUN TitAgenc.

    IF FIRST-OF(PagX) THEN 
       RUN Tit1.

    ASSIGN Nro [1] = Nro [1] + 1
           Nro [2] = Nro [2] + 1
           Nro [3] = Nro [3] + 1
           Tot1[1] = Tot1[1] + CopCreditos.Sdo_capital
           Tot2[1] = Tot2[1] + CopCreditos.Sdo_capital
           Tot3[1] = Tot3[1] + CopCreditos.Sdo_capital
           Tot1[2] = Tot1[2] + CopCreditos.INT_Corrientes
           Tot2[2] = Tot2[2] + CopCreditos.INT_Corrientes
           Tot3[2] = Tot3[2] + CopCreditos.INT_Corrientes
           Tot1[3] = Tot1[3] + CopCreditos.Polizas
           Tot2[3] = Tot2[3] + CopCreditos.Polizas
           Tot3[3] = Tot3[3] + CopCreditos.Polizas
           Tot1[4] = Tot1[4] + CopCreditos.Costas
           Tot2[4] = Tot2[4] + CopCreditos.Costas
           Tot3[4] = Tot3[4] + CopCreditos.Costas
           Tot1[5] = Tot1[5] + CopCreditos.Provision
           Tot2[5] = Tot2[5] + CopCreditos.Provision
           Tot3[5] = Tot3[5] + CopCreditos.Provision
           Tot1[6] = Tot1[6] + CopCreditos.INT_DifCobro
           Tot2[6] = Tot2[6] + CopCreditos.INT_DifCobro
           Tot3[6] = Tot3[6] + CopCreditos.INT_DifCobro
           Tot1[8] = Tot1[8] + CopCreditos.INT_Anticip
           Tot2[8] = Tot2[8] + CopCreditos.INT_Anticip
           Tot3[8] = Tot3[8] + CopCreditos.INT_Anticip
           Tot1[7] = Tot1[7] + TotGar
           Tot2[7] = Tot2[7] + TotGar
           Tot3[7] = Tot3[7] + TotGar
           Tot1[9] = Tot1[9] + CopCreditos.INT_morcobrar
           Tot2[9] = Tot2[9] + CopCreditos.INT_morcobrar
           Tot3[9] = Tot3[9] + CopCreditos.INT_morcobrar.

    IF LAST-OF(CopCreditos.Categoria) THEN DO:
       DISPLAY CopCreditos.Categoria     LABEL "Calif."
               Nro [1]                   LABEL "T.Nros"
               Tot1[1]                   LABEL "Sdos-Capital"
               Tot1[2]                   LABEL "Int-Corrientes"
               Tot1[3]                   LABEL "Polizas"
               Tot1[4]                   LABEL "Costas-Jud"
               Tot1[5]                   LABEL "Provisión"
               Tot1[6]                   LABEL "Int-Nocausado"
               Tot1[7]                   LABEL "Garantia-Adm"
               Tot1[8]                   LABEL "Int-Anticipado"
               Tot1[9]                   LABEL "Int-Mor-Cobrar"
               SKIP(0)
           WITH DOWN WIDTH 170 FRAME FDetalleAg NO-BOX NO-LABELS STREAM-IO USE-TEXT.
       ASSIGN Nro [1] = 0
              Tot1[1] = 0
              Tot1[2] = 0
              Tot1[3] = 0
              Tot1[4] = 0
              Tot1[5] = 0
              Tot1[6] = 0
              Tot1[7] = 0
              Tot1[8] = 0
              Tot1[9] = 0.
    END.

    IF LAST-OF(CopCreditos.PagX) THEN DO:                                    
       RUN Tot1.
       ASSIGN Nro [2] = 0                                                         
              Tot2[1] = 0                                                         
              Tot2[2] = 0                                                         
              Tot2[3] = 0                                                         
              Tot2[4] = 0                                                         
              Tot2[5] = 0                                                         
              Tot2[6] = 0                                                         
              Tot2[7] = 0
              Tot2[8] = 0
              Tot2[9] = 0.                                                        
    END. 

    IF LAST-OF(CopCreditos.Agencia) THEN
       RUN TotalAgencia.
END.

PROCEDURE TotalAgencia:
  DISPLAY   SKIP(2)
          "TOTAL"                                                            
           Nro [3]                                      
           Tot3[1]                                      
           Tot3[2]                                      
           Tot3[3]                                      
           Tot3[4]                                      
           Tot3[5]                                      
           Tot3[6]                                      
           Tot3[7]                                      
           Tot3[8]
           Tot3[9]
       WITH DOWN WIDTH 170 FRAME FTot3 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  ASSIGN Nro [3] = 0
         Tot3[1] = 0
         Tot3[2] = 0
         Tot3[3] = 0
         Tot3[4] = 0
         Tot3[5] = 0
         Tot3[6] = 0
         Tot3[7] = 0
         Tot3[8] = 0
         Tot3[9] = 0.
END PROCE.

OUTPUT CLOSE.

SESSION:SET-WAIT-STATE("").

PROCEDURE TitAgenc:
  FIND FIRST Agencias WHERE Agencias.Agencia EQ CopCreditos.Agencia NO-LOCK NO-ERROR.
  DISPLAY SKIP(2)
          "COOPERATIVA UTRAHUILCA - CLASIFICACIÒN CARTERA  AGENCIA : "
          Agencias.Nombre  FORM "X(25)" NO-LABELS
      WITH WIDTH 150.
END PROCE.

PROCEDURE Tit1:
   DEFI VAR NomPdcto AS CHAR FORM "X(50)" INIT "CONSUMO".

   IF CopCreditos.Tip_credito EQ 2 THEN
      NomPdcto = "COMERCIAL".
   ELSE IF CopCreditos.Tip_credito EQ 4 THEN
      NomPdcto = "MICROCREDITO".

   IF Gtia EQ "AD" THEN
      NomPdcto = NomPdcto + " Garantía-Admisible".
   ELSE 
      NomPdcto = NomPdcto + " Otras Garantías".

   IF PagX EQ "CL" THEN
      NomPdcto = NomPdcto + "  -  Con Libranza".
   ELSE 
      NomPdcto = NomPdcto + "  -  Sin Libranza".

   DISPLAY SKIP(1)
           NomPdcto NO-LABELS
           SKIP(0).            
END PROCE.

PROCEDURE Tot1:
   DISPLAY "SubTot"                                                            
           Nro [2]                                      
           Tot2[1]                                      
           Tot2[2]                                      
           Tot2[3]                                      
           Tot2[4]                                      
           Tot2[5]                                      
           Tot2[6]                                      
           Tot2[7]                                      
           Tot2[8]
           Tot2[9]
           SKIP(0)
       WITH DOWN WIDTH 170 FRAME FTot1 NO-BOX NO-LABELS STREAM-IO USE-TEXT. 

END PROCE.
