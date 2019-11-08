/*--------------------------------------------------------------------------
 ARCHIVO    : P-libros.P 
 DESCRIPCION: Permite la generación del Balance de Prueba y Libro Mayor
 AUTOR      : ""
 FECHA      : 
 PROPIEDAD  : REDECOOP
 MODIFICACIONES:
        11/05/1998 DFV  Se evitó la lectura a la tabla de Sal_Cuentas para ob
                        tener el saldo inicial, calculándolo con el saldo ac-
                        tual y su movimiento.
        30/06/1998 GAER Se corrigieron Totales de movtos DEB-CRED de Nivel 1
                        y Generales. (Quedaron sumando por Ctas Mayores).
        19/08/1998 GAER Se modificó Total ingresos por Totales cuentas de
                        Resultado y se adicionó Resultado del Ejercicio en "BP".
                        Se agregó a Ruticon.P opción 13 cuentas para "BP".
---------------------------------------------------------------------------*/                  


  DEFINE INPUT PARAMETER P_Tipo AS CHARACTER.
  
  {incluido\variable.i "SHARED"}
  {incluido\VarCon.i   "SHARED"}.
  DEFINE VARIABLE W_Titulo AS CHAR INITIAL "Balance de Prueba".
  DEFINE VARIABLE W_Tamano AS INTEGER INITIAL 1.               
  DEFINE VARIABLE W_sdoant AS CHAR INITIAL "" FORMAT "X(40)".  
  DEFINE VARIABLE W_sdoact AS CHAR INITIAL "" FORMAT "X(40)".
  DEFINE VARIABLE W_sdodeb AS CHAR INITIAL "" FORMAT "X(40)".
  DEFINE VARIABLE W_sdocre AS CHAR INITIAL "" FORMAT "X(40)".
  DEFINE VARIABLE W_AnoAnt AS INTEGER INITIAL 0.
  DEFINE VARIABLE W_MesAnt AS INTEGER INITIAL 0.
  DEFINE VARIABLE W_AnoAct AS INTEGER INITIAL 0.
  DEFINE VARIABLE W_MesAct AS INTEGER INITIAL 0.
  DEFINE VARIABLE W_Estado AS LOGICAL.    
  DEFINE VARIABLE W_PanIni AS WIDGET-HANDLE.
  DEFINE FRAME F-Encabezado
   HEADER
    W_Titulo                       AT  50 FORMAT "X(20)"
    "PAGINA:"                      AT 110 PAGE-NUMBER FORMAT ">>>9"
    W_NomMes                       AT  50 FORMAT "X(20)"
    "agencia: " + W_NomOfi         AT   3 FORMAT "X(50)"
    "Centro de Costo: " + W_NomCen AT   3 FORMAT "X(50)"
    W_Raya1                        AT   1 FORMAT "X(123)"
    "CUENTA"                       AT   3
    "NOMBRE CUENTA"                AT  18
    "  SALDO ANTERIOR"             AT  50
    "          DEBITO"             AT  69
    "         CREDITO"             AT  88
    "    SALDO ACTUAL"             AT 107
    W_Raya2                        AT   1 FORMAT "X(123)"
  WITH DOWN WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.

  DEFINE FRAME F-PiePagina
   HEADER 
     "Fecha:"      AT  5 
     TODAY         AT 13
     W_Nom_agencia AT 48 FORMAT "X(30)"
     "HORA:"    AT 110 STRING(TIME,"HH:MM AM")
  WITH DOWN WIDTH 132 FRAME F-PiePagina PAGE-BOTTOM USE-TEXT STREAM-IO.
    
  DEFINE FRAME F-detalle               
    Cuentas.Cuenta AT   3
    Cuentas.Nombre AT  18 FORMAT "X(30)"
    W_SdoAnt       AT  50 FORMAT "X(18)"
    W_SdoDeb       AT  69 FORMAT "X(18)"
    W_SdoCre       AT  88 FORMAT "X(18)" 
    W_SdoAct       AT 107 FORMAT "X(18)"
  WITH DOWN WIDTH 132 USE-TEXT NO-BOX.
    
  IF P_Tipo = "LM" THEN
     ASSIGN W_Titulo  = "  Libro Mayor"
            W_Destino = "LMay".
  ELSE
     W_Destino = "BPba".
 
  RUN FormarNombre IN W_ManCon (INPUT-OUTPUT W_Destino).
 
  DO WHILE TRUE:  
    IF P_Tipo EQ "BP" THEN
       RUN P-DatGrl IN W_ManCon (INPUT 14, OUTPUT W_Validacion, INPUT-OUTPUT W_Destino, INPUT W_Titulo).
    ELSE    
       RUN P-DatGrl IN W_ManCon (INPUT 2,  OUTPUT W_Validacion, INPUT-OUTPUT W_Destino, INPUT W_Titulo).
    
    IF W_Validacion = -1 THEN DO:
       ASSIGN W_Estado = SESSION:SET-WAIT-STATE("").
       RETURN.
    END.
    PAUSE 1 NO-MESSAGE. /* No Limpia los datos de la pantalla de captura */

    ASSIGN W_Estado = SESSION:SET-WAIT-STATE("GENERAL").

    IF W_Nivel LT 1 THEN
       ASSIGN W_Tamano = (W_Nivel * 2) - 2.

    RUN NombreMes IN W_Manija (INPUT W_FecIni, OUTPUT W_NomMes).

    ASSIGN W_MesAct = MONTH(W_FecIni)
           W_AnoAct = YEAR(W_FecIni)
           W_MesAnt = W_MesAct - 1
           W_AnoAnt = W_AnoAct.
           
    IF W_MesAnt LE 0 THEN 
       ASSIGN W_MesAnt = 12
              W_AnoAnt = W_AnoAnt - 1.

    OUTPUT TO VALUE(W_Destino) NO-ECHO PAGE-SIZE 81.
    
    IF P_Tipo NE "BP" THEN
       ASSIGN W_CtaIni   = ""
              W_CtaFin   = "99999999999999".

    FOR EACH Cuentas FIELDS(Cuentas.Cuenta Cuentas.Nombre Cuentas.Id_Cuenta Cuentas.Tipo Cuentas.Nivel
                            Cuentas.Naturaleza)
        WHERE Cuentas.Cuenta GE W_CtaIni
        AND   Cuentas.Cuenta LE W_CtaFin
        NO-LOCK BREAK BY Cuentas.Id_Cuenta BY Cuentas.Cuenta:

      IF FIRST-OF(Cuentas.Id_Cuenta) THEN 
         W_NomCta = Cuentas.Nombre. 
 
      IF (    P_Tipo EQ "LM" 
          AND Cuentas.Tipo EQ 1 
          AND Cuentas.Nivel LE 3)
      OR (    Cuentas.Nivel LE W_nivel 
          AND P_Tipo EQ "BP") THEN DO:  
          VIEW FRAME F-Encabezado.
          VIEW FRAME F-PiePagina.

          RUN HallarSdoMayor IN W_Manija (INPUT W_OfiIni, INPUT W_OfiFin, INPUT W_CenIni, INPUT W_CenFin,
                                          INPUT Cuentas.Cuenta, INPUT W_AnoAct, INPUT W_MesAct,
                                          INPUT Cuentas.Tipo, OUTPUT VlrDeb, OUTPUT VlrCre, OUTPUT W_SaldoAct).

          IF Cuentas.Naturaleza = "DB" THEN
             ASSIGN W_SaldoAnt = VlrCre + W_SaldoAct - VlrDeb.
          ELSE
             ASSIGN W_SaldoAnt = VlrDeb + W_SaldoAct - VlrCre.
               
          IF W_SaldoAnt NE 0
          OR W_SaldoAct NE 0
          OR VlrDeb     NE 0
          OR VlrCre     NE 0 THEN DO:
              IF ( (   Cuentas.Nivel EQ 1 
                    OR Cuentas.Nivel EQ 2)
                  AND Cuentas.Tipo EQ 1) 
              OR (    P_Tipo EQ "BP" 
                  AND (   Cuentas.Nivel EQ 1 
                       OR Cuentas.Nivel EQ 2)) THEN
                  DOWN 1 WITH FRAME F-Detalle.  

              RUN Decodificar IN W_Manija (INPUT W_SaldoAnt, INPUT " ", INPUT 16, INPUT 0, 
                                           INPUT W_RForma, OUTPUT W_SdoAnt).
              RUN Decodificar IN W_Manija (INPUT W_SaldoAct, INPUT " ", INPUT 16, INPUT 0, 
                                           INPUT W_RForma, OUTPUT W_SdoAct).
              RUN Decodificar IN W_Manija (INPUT VlrDeb, INPUT " ", INPUT 16, INPUT 0, INPUT W_Rforma, 
                                           OUTPUT W_SdoDeb).
              RUN Decodificar IN W_Manija (INPUT VlrCre, INPUT " ", INPUT 16, INPUT 0, INPUT W_RForma,
                                           OUTPUT W_SdoCre).
                                           
              IF (    Cuentas.Tipo  EQ 1
                  AND Cuentas.Nivel EQ 1 
                  AND P_Tipo        EQ "BP") THEN
                 ASSIGN TotDctoDeb = TotDctodeb + VlrDeb
                        TotDctoCre = TotDctoCre + VlrCre.
              ELSE DO:
                  IF  P_Tipo EQ "LM"
                  AND Cuentas.Nivel EQ 1 THEN
                     ASSIGN TotDctoDeb = TotDctodeb + VlrDeb
                            TotDctoCre = TotDctoCre + VlrCre.
                  ELSE
                  IF  P_Tipo EQ "LM" 
                  AND Cuentas.Nivel EQ 2 THEN
                      DOWN 1 WITH FRAME F-Detalle.
              END.
              DISPLAY Cuentas.Cuenta Cuentas.Nombre W_SdoAnt W_SdoDeb W_SdoCre W_SdoAct 
                     WITH FRAME F-detalle USE-TEXT STREAM-IO NO-LABELS.
          END.
      END.
      IF LAST-OF(Cuentas.Id_Cuenta) THEN DO: 
         IF P_Tipo NE "LM" THEN DO:
            IF Cuentas.Id_Cuenta EQ 4 THEN DO:
               RUN TotalLibros (INPUT TotDctoDeb,INPUT TotDctoCre,
                                INPUT "                TOTALES CUENTAS DE RESULTADO").
               IF TotDctoDeb - TotDctoCre GE 0 THEN
                  RUN TotalLibros (INPUT 0,INPUT TotDctoDeb - TotDctoCre,
                                   INPUT "                RESULTADO DEL EJERCICIO").
               ELSE
                  RUN TotalLibros (INPUT TotDctoCre - TotDctoDeb,INPUT 0,
                                   INPUT "                RESULTADO DEL EJERCICIO").
            END.
            ELSE
               RUN TotalLibros (INPUT TotDctoDeb,INPUT TotDctoCre,
                             INPUT "              Totales de " + W_NomCta).
         END.
      ASSIGN TotGralDeb = TotGraldeb + TotDctoDeb
             TotGralCre = TotGralCre + TotDctoCre
             TotDctoDeb = 0
             TotDctoCre = 0.
      END. 
      IF LAST(Cuentas.Id_Cuenta) THEN             
         RUN TotalLibros(INPUT TotGralDeb,INPUT TotGralCre, INPUT "").
    END.                   
    IF LINE-COUNTER < PAGE-SIZE THEN            
       VIEW FRAME F-PiePagina.        
    
    OUTPUT CLOSE.
    ASSIGN W_Estado = SESSION:SET-WAIT-STATE("").
    RUN PantallaImpresora IN W_ManCon (INPUT 2).
  END. /* REPEAT */

PROCEDURE TotalLibros:
    DEFINE INPUT PARAMETER W_Deb     AS DECIMAL.
    DEFINE INPUT PARAMETER W_Cre     AS DECIMAL.
    DEFINE INPUT PARAMETER W_Mensaje AS CHARACTER FORMAT "X(60)".
    
    DEFINE FRAME F-totales
      W_Raya1   AT  2 FORMAT "X(120)" SKIP
      W_Mensaje AT  2
      W_SdoDeb  AT 69 FORMAT "X(18)"
      W_SdoCre  AT 88 FORMAT "X(18)"
      W_Raya2   AT  2 FORMAT "X(120)" SKIP
      WITH DOWN WIDTH 132 FRAME F-Totales NO-BOX USE-TEXT NO-LABEL. 
            
    IF  W_Deb EQ 0
    AND W_Cre EQ 0 THEN
        RETURN.
    
    RUN Decodificar IN W_Manija (INPUT W_Deb, INPUT " ", INPUT 16, INPUT 0, INPUT W_RForma, OUTPUT W_SdoDeb).
    RUN Decodificar IN W_Manija (INPUT W_Cre, INPUT " ", INPUT 16, INPUT 0, INPUT W_RForma, OUTPUT W_SdoCre).

    IF TRIM(W_Mensaje) EQ "" THEN
      DISPLAY "T O T A L E S" @ W_Mensaje W_Raya1 W_Raya2 W_SdoDeb W_SdoCre
            WITH FRAME F-totales USE-TEXT STREAM-IO.
    ELSE   
      DISPLAY W_Mensaje W_SdoDeb W_SdoCre 
            WITH FRAME F-totales USE-TEXT STREAM-IO.      
END PROCEDURE.
