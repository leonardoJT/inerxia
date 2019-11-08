    DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Movimiento Contable" NO-UNDO.

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza NO-UNDO.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza NO-UNDO.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES NO-UNDO.
  DEFINE VARIABLE W_Com        AS LOGICAL INITIAL NO NO-UNDO.

  DEFINE VARIABLE Nom_Cliente  AS CHARACTER FORMAT "X(18)" NO-UNDO.

/* incluido de Pantalla con parametros */
{incluido/Pantalla_Validacion2.i}

/*inicializacion de variables */
/*   W_Ok = Cmb_Agencia:ADD-LAST("000 - Todas las Agencias").
    FOR EACH Agencias NO-LOCK:
      W_Ok = Cmb_Agencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
      IF W_Agencia EQ Agencias.Agencia THEN
         Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
    END.
        
    FIND FIRST Entidad WHERE Entidad.Nombre EQ W_Nom_Entidad NO-LOCK NO-ERROR.
    IF AVAILABLE(Entidad) THEN
       IF NOT Entidad.Id_CenCosto THEN ASSIGN L_CC = NO.
    ELSE MESSAGE "No encontro entidad" VIEW-AS ALERT-BOX.
    ASSIGN W_Ok = Cmb_CenCost:ADD-LAST("000 - Entidad No Maneja Cen.Costos")
           Cmb_CenCost:SCREEN-VALUE IN FRAME F_Valida = "000 - Entidad No Maneja Cen.Costos".
    IF L_CC THEN DO:
       W_Ok = Cmb_CenCost:ADD-LAST("000 - Todos los Centros de Costo").
       FOR EACH Cen_Costos WHERE Cen_Costos.Agencia EQ W_Agencia NO-LOCK:
          W_Ok = Cmb_CenCost:ADD-LAST(STRING(Cen_Costos.Cen_Costos,"999") + Cen_Costos.Nombre).
       END.
       Cmb_CenCost:SCREEN-VALUE IN FRAME F_Valida = "000 - Todos los Centros de Costo".
    END.
    W_Ok = Cmb_Comprob:ADD-LAST("00 - Todos los Comprobantes").
    FOR EACH Comprobantes WHERE Comprobantes.Estado EQ 1:
        W_Ok = Cmb_Comprob:ADD-LAST(STRING(Comprobantes.Comprobante,"99") + " - " + Comprobantes.Nombre).
    END.
    Cmb_Comprob:SCREEN-VALUE IN FRAME F_Valida = "00 - Todos los Comprobantes".

    ASSIGN W_Usuario1:SCREEN-VALUE = STRING(0)
           W_Usuario2:SCREEN-VALUE = STRING(999)
           W_NomUsuario1:SCREEN-VALUE = "Todos los Usuarios"
           W_NomUsuario2:SCREEN-VALUE = "Todos los Usuarios"
           W_Fec1:SCREEN-VALUE = STRING(TODAY)
           W_Fec2:SCREEN-VALUE = STRING(TODAY)
           Cmb_Nivel:SCREEN-VALUE = STRING(8).*/
    DEF VAR cusi AS CHAR NO-UNDO.
    DEF VAR cusf AS CHAR NO-UNDO.
/*fin inicializacion de variables*/
  ON RETURN TAB.
  
  VIEW W_Pantalla ACTIVE-WINDOW.

  RELEASE usuarios.
  
  ON CHOOSE OF Btn_Informacion 
  DO:
    RUN W-InfDia.R NO-ERROR.
  END.  

  ON 'value-changed':U OF Cmb_Agencia
  DO:
    IF L_CC THEN DO:
       Cmb_CenCost:LIST-ITEMS = "".
       W_Ok = Cmb_CenCost:ADD-LAST("000 - Todos los Centros de Costo").
       FOR EACH Cen_Costos WHERE Cen_Costos.Agencia EQ W_Agencia NO-LOCK:
          W_Ok = Cmb_CenCost:ADD-LAST(STRING(Cen_Costos.Cen_Costos,"999") + Cen_Costos.Nombre).
       END.
       Cmb_CenCost:SCREEN-VALUE IN FRAME F_Valida = "000 - Todos los Centros de Costo".
    END.
  END.

  ON 'leave':U OF W_Usuario1
  DO:
     IF W_Usuario1:SCREEN-VALUE NE "0" THEN DO:
        FIND Usuarios WHERE Usuarios.Agencia EQ integer(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3)) AND
                            Usuarios.Usuario EQ W_Usuario1:SCREEN-VALUE NO-LOCK NO-ERROR.
        W_NomUsuario1:SCREEN-VALUE = "A partir de este Numero".
        IF AVAILABLE(Usuarios) THEN
           W_NomUsuario1:SCREEN-VALUE = Usuarios.Nombre.
     END.
     ELSE W_NomUsuario1:SCREEN-VALUE = "Consolidado".
  END.

  ON 'leave':U OF W_Usuario2
  DO:
     IF W_Usuario2:SCREEN-VALUE NE "999" THEN DO:
        FIND Usuarios WHERE Usuarios.Agencia EQ integer(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3)) AND
                            Usuarios.Usuario EQ W_Usuario2:SCREEN-VALUE NO-LOCK NO-ERROR.
        W_NomUsuario2:SCREEN-VALUE = "Hasta este Numero".
        IF AVAILABLE(Usuarios) THEN
           W_NomUsuario2:SCREEN-VALUE = Usuarios.Nombre.
     END.
     ELSE W_NomUsuario2:SCREEN-VALUE = "Consolidado".
  END.

  ON 'mouse-select-dblclick':U OF W_Cuenta1
  DO:
     DEFINE VAR W_Cta LIKE Cuentas.Cuenta NO-UNDO.
     DEFINE VAR W_Nom LIKE Cuentas.Nombre NO-UNDO.
     IF W_Cuenta1:SCREEN-VALUE EQ "" THEN
        RUN Busca_Cuenta (INPUT W_Cuenta1:SCREEN-VALUE, OUTPUT W_Cta, OUTPUT W_Nom).
     ASSIGN W_Cuenta1:SCREEN-VALUE = W_Cta
            W_NomCuenta1:SCREEN-VALUE = W_Nom.
  END.

  ON 'leave':U OF  W_Cuenta1
  DO:
     IF W_Cuenta1:SCREEN-VALUE EQ "" THEN
        ASSIGN W_Cuenta1:SCREEN-VALUE = "0"
               W_NomCuenta1:SCREEN-VALUE = "Todas las Cuentas".
  END.

  ON 'mouse-select-dblclick':U OF W_Cuenta2
  DO:
     DEFINE VAR W_Cta LIKE Cuentas.Cuenta NO-UNDO.
     DEFINE VAR W_Nom LIKE Cuentas.Nombre NO-UNDO.
     IF W_Cuenta2:SCREEN-VALUE EQ "" THEN
        RUN Busca_Cuenta (INPUT W_Cuenta2:SCREEN-VALUE, OUTPUT W_Cta, OUTPUT W_Nom).
     ASSIGN W_Cuenta2:SCREEN-VALUE = W_Cta
            W_NomCuenta2:SCREEN-VALUE = W_Nom.
  END.

  ON 'leave':U OF  W_Cuenta2
  DO:
     IF W_Cuenta2:SCREEN-VALUE EQ "" THEN
        ASSIGN W_Cuenta2:SCREEN-VALUE = "99999999999999"
               W_NomCuenta2:SCREEN-VALUE = "Todas las Cuentas".
  END.

  ON 'leave':U OF Cmb_Nivel
  DO:
     APPLY 'entry' TO Btn_Imprimir IN FRAME F_Valida.
     RETURN NO-APPLY.
  END.

  ON 'entry':U OF Btn_Imprimir
  DO:
    ON RETURN RETURN.
  END.

  ON 'leave':U OF Btn_Imprimir
  DO:
     APPLY 'focus' TO Cmb_Agencia.
  END.

  ON 'Entry':U OF W_Usuario1 DO:
      /*Disdponible*/
  END.

  ON 'Entry':U OF W_Fec1
  DO:
      APPLY "FOCUS" TO SELF.
      RELEASE usuarios.
     /* APPLY "Entry" TO W_Usuario1.
      RETURN NO-APPLY.*/
  END.

  ON CHOOSE OF Btn_TotCta DO:
     ASSIGN W_TotCta = TRUE.
     APPLY "CHOOSE" TO Btn_Imprimir.
  END.

  ON CHOOSE OF Btn_TotComp DO:
     ASSIGN W_TotComp = TRUE.
     APPLY "CHOOSE" TO Btn_Imprimir.
  END.


  ON CHOOSE OF Btn_Imprimir
  DO:
     ASSIGN W_CC1 = INTEGER(SUBSTRING(Cmb_CenCost:SCREEN-VALUE,1,3))
            W_CC2 = INTEGER(SUBSTRING(Cmb_CenCost:SCREEN-VALUE,1,3))
            W_AG1 = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
            W_AG2 = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
            W_CB1 = INTEGER(SUBSTRING(Cmb_Comprob:SCREEN-VALUE,1,3))
            W_CB2 = INTEGER(SUBSTRING(Cmb_Comprob:SCREEN-VALUE,1,3)).
     IF SUBSTRING(Cmb_CenCost:SCREEN-VALUE,1,3) EQ "000" THEN
        ASSIGN W_CC1 = 0
               W_CC2 = 999.
     IF SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3) EQ "000" THEN
        ASSIGN W_AG1 = 0
               W_AG2 = 999.
     IF SUBSTRING(Cmb_Comprob:SCREEN-VALUE,1,3) EQ "000" THEN
        ASSIGN W_CB1 = 0
               W_CB2 = 999.
     ASSIGN FRAME F_Valida W_Fec1    W_Fec2
                           W_Cuenta1 W_Cuenta2
                           W_Nit1    W_Nit2
                           W_Usuario1 W_Usuario2
                           W_Porcentaje Cmb_Nivel W_Base.
     IF W_Cuenta1 EQ "" AND W_Cuenta2 EQ "" THEN
        ASSIGN W_Cuenta1 = "0" W_Cuenta2 = "99999999999999".
     IF W_Nit1 EQ "" AND W_Nit2 EQ "" THEN
        ASSIGN W_Nit1 = " " W_Nit2 = "zzzzzzzzzzzz".
     
     RUN Proceso_Imprimir.
     ASSIGN W_TotCta = FALSE.
     ON RETURN TAB.
  END.
   
RUN Habilita_Deshabilita.

RELEASE usuarios.

APPLY "ENTRY" TO W_Fec1.
/*RETURN NO-APPLY.*/
 /*MESSAGE " VERIFICA TX " SKIP
         " VALOR : " THIS-PROCEDURE:TRANSACTION
     VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
WAIT-FOR CHOOSE OF Btn_Salir FOCUS W_Fec1.    /*Cmb_Agencia.*/

RELEASE usuarios.


  ON CURSOR-DOWN CURSOR-DOWN.
  ON RETURN RETURN.
  HIDE FRAME F_Valida NO-PAUSE IN WINDOW W_Pantalla.
  DELETE WIDGET W_Pantalla.
  
RELEASE usuarios.

/* fin incluido Pantalla parametros */

  PROCEDURE Habilita_Deshabilita:
  /* En este procedimiento se habilitan o deshabilitan las variables
     a pedir en pantalla segun el informe que se vaya a ejecutar. */
      ENABLE ALL WITH FRAME F_Valida IN WINDOW W_Pantalla.
      DISABLE /*W_Cuenta1      W_Cuenta2*/
              W_NomUsuario1  W_NomUsuario2          
              W_NomCuenta1   W_NomCuenta2         
              /*W_Nit1*/         W_NomNit1            
              /*W_Nit2*/         W_NomNit2 Cmb_Nivel
              W_Base         W_Porcentaje WITH FRAME F_Valida.
      IF NOT L_CC THEN /* valida si la entidad maneja centros de costos */
         DISABLE Cmb_CenCost WITH FRAME F_Valida.
  END PROCEDURE.

  PROCEDURE Proceso_Imprimir:
      DEFINE VAR Listado AS CHARACTER INITIAL "" NO-UNDO.
      Listado = W_Pathspl + "MDia" + STRING(W_Usuario,"X(4)") + STRING(TIME,"999999") + ".Txt".
     {incluido\IMPRIMIR_informe.I "listado"}.
  END PROCEDURE.

  PROCEDURE ProcesoImprimir:
     {Incluido\RepEncabezado.i}
      DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "" NO-UNDO.
      DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "" NO-UNDO.
      IF W_Usuario2 = "999" THEN w_usuario2 = "9999".
      IF W_Fec1 NE W_Fec2 AND W_Fec1 LE W_Fec2 THEN
         W_Reporte = "REPORTE   : MOVIMIENTO CONTABLE - del " + STRING(W_Fec1) + " al " + STRING(W_Fec2) + " - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(25)")
                    + " - "+ STRING(TIME,"hh:mm am") .
      ELSE
         W_Reporte = "REPORTE   : MOVIMIENTO CONTABLE - Al " + STRING(W_Fec2) + " - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(25)")
                    + " - "+ STRING(TIME,"hh:mm am").

/*                             1         2         3         4         5         6         7         8         9         10        11        12
                      123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789*/
      W_EncColumna = "AGE NUM.DOC  CUENTA        COMENTARIO           NIT            NOMBRE CLIENTE      DOC.REF          DEBITO          CREDITO   USU".
      DEFINE VAR  TT_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
      DEFINE VAR  TC_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
      DEFINE VAR  TC_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
      
      DEFINE VAR  W_Cta AS CHARACTER FORMAT "X(14)" NO-UNDO.
      DEFINE VAR  W_Nom AS CHARACTER FORMAT "X(20)" NO-UNDO.
      DEFINE FRAME F_Mov

      WITH DOWN FRAME F_Mov WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

      VIEW FRAME F-Encabezado.
      VIEW FRAME f-ftr.
      IF W_TotCta THEN DO:
         IF INTEGER(W_Usuario1) = INTEGER(W_Usuario2) THEN
                W_EncColumna = "        Resumen Detallado - Movimiento por Cuenta   Usuario "  + trim(W_Usuario1).
         ELSE
                W_EncColumna = "        Resumen Detallado - Movimiento por Cuenta   de Usuario " + trim(W_Usuario1) + " al " + trim(W_Usuario2).
         RUN TotxCta.
      END.
      ELSE IF W_TotComp THEN DO:
         IF INTEGER(W_Usuario1) = INTEGER(W_Usuario2) THEN
                W_EncColumna = "        Resumen Totalizado por Cuenta y Comprobante   Usuario "  + trim(W_Usuario1).
         ELSE
                W_EncColumna = "        Resumen Totalizado por Cuenta y Comprobante   de Usuario " + trim(W_Usuario1) + " al " + trim(W_Usuario2).
         RUN TotxComp.
      END.
      ELSE FOR EACH Mov_Contable WHERE
               Mov_Contable.Agencia      GE  W_Ag1      AND
               Mov_Contable.Agencia      LE  W_Ag2      AND
               Mov_Contable.Cen_Costos   GE  W_CC1      AND
               Mov_Contable.Cen_Costos   LE  W_CC2      AND
               Mov_Contable.Comprobante  GE  W_CB1      AND
               Mov_Contable.Comprobante  LE  W_CB2      AND
               Mov_Contable.Fec_Contable GE  W_Fec1     AND
               Mov_Contable.Fec_Contable LE  W_Fec2     AND
               Mov_Contable.Cuenta       GE  W_Cuenta1  AND
               Mov_Contable.Cuenta       LE  W_Cuenta2  AND
               (IF  Mov_Contable.Nit     EQ ? 
                    THEN TRUE 
                    ELSE (Mov_Contable.Nit GE  W_Nit1 AND
                            Mov_Contable.Nit        LE  W_Nit2)) AND
               INTEGER(Mov_Contable.Usuario) GE  INTEGER(W_Usuario1) AND
               INTEGER(Mov_Contable.Usuario) LE  INTEGER(W_Usuario2) NO-LOCK
               BREAK 
                    BY Mov_Contable.Comprobante 
                    BY Mov_Contable.Fec_Contable 
                    BY Mov_Contable.Agencia 
                    BY Mov_Contable.Num_Documento:
          IF FIRST-OF(Mov_Contable.Comprobante) THEN DO:
              DISPLAY "Comprobante: " AT 1
                      Mov_Contable.Comprobante AT 15
              WITH FRAME F_Com WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
          END.
          IF FIRST-OF(Mov_Contable.Fec_Contable) THEN DO:
              DISPLAY SKIP(1) "Fecha Contable: " AT 1
                      Mov_Contable.Fec_Contable AT 18 SKIP(1)
              WITH FRAME F_Fec WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
          END.
          Nom_Cliente = "".
          FIND Clientes WHERE Clientes.Nit EQ Mov_Contable.Nit NO-LOCK NO-ERROR.
          IF AVAILABLE Clientes THEN
             Nom_Cliente = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
          DISPLAY Mov_Contable.Agencia        AT 1   FORMAT "999"
                  Mov_Contable.Num_Documento  AT 5   FORMAT "9999999"
                  Mov_Contable.Cuenta         AT 13  FORMAT "X(14)"
                  Mov_Contable.Comentario     AT 28  FORMAT "X(20)" 
/*                  Mov_Contable.Cen_Costos     AT 46  FORMAT "999"*/
                  Mov_Contable.Nit            AT 49  FORMAT "X(14)" 
                  Nom_Cliente                 AT 64  FORMAT "X(19)"
                  Mov_Contable.Doc_Referencia AT 84  FORMAT "X(7)" 
                  Mov_Contable.Db             AT 94  FORMAT ">>,>>>,>>>,>>9"
                  Mov_Contable.Cr             AT 110 FORMAT ">>,>>>,>>>,>>9"
                  Mov_Contable.Usuario        AT 127 FORMAT "X(4)"
          WITH DOWN FRAME F_Otro WIDTH 150 USE-TEXT NO-BOX STREAM-IO NO-UNDERLINE NO-LABELS.
          
          ASSIGN TT_Db = TT_Db + Mov_Contable.Db
                 TT_Cr = TT_Cr + Mov_Contable.Cr
                 TC_Db = TC_Db + Mov_Contable.Db
                 TC_Cr = TC_Cr + Mov_Contable.Cr.
          IF LAST-OF(Mov_Contable.Num_Documento) THEN DO:
              DISPLAY "____________________________________" AT 90 SKIP(1)
                      TT_Db AT 94
                      TT_Cr AT 110 SKIP(0)
              WITH DOWN FRAME F_Tot WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.    
              ASSIGN TT_Db = 0 TT_Cr = 0.
          END.
          IF LAST-OF(Mov_Contable.Comprobante) THEN DO:
              DISPLAY "Total Comprobante: "                  AT 40
                      Mov_Contable.Comprobante               AT 60
                      "____________________________________" AT 90 SKIP(1)
                      TC_Db AT 94
                      TC_Cr AT 110 SKIP(1)
              WITH DOWN FRAME F_TotCom WIDTH 150 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.    
              ASSIGN TC_Db = 0 TC_Cr = 0.
          END.
      END. 
      DISPLAY
      SKIP(3)
       "    _______________________           _____________________________           _________________________" SKIP
       "    Elaborado                         Revisado                                Aprobado                 " 
          WITH DOWN FRAME F_TotGen WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

      PAGE.
    OUTPUT CLOSE.
  END PROCEDURE.


  PROCEDURE TotXCta:
    DEFINE VAR  TT_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TT_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TC_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR  TC_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TG_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR  TG_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    
    IF W_Usuario2 = "999" THEN w_usuario2 = "9999".
    cusi = string(INTEGER(w_usuario1),"zzzz").
    cusf = string(INTEGER(w_usuario2),"zzzz").
        
    FOR EACH Mov_Contable no-lock
        WHERE
            Mov_Contable.Agencia = W_Ag1
        AND (IF W_CC1 = 0 THEN TRUE ELSE Mov_Contable.Cen_Costos = W_CC1)
        AND (IF W_CB1 = 0 THEN TRUE ELSE Mov_Contable.Comprobante = W_CB1)
        AND (Mov_Contable.Fec_Contable >=  W_Fec1 AND Mov_Contable.Fec_Contable <=  W_Fec2)
        AND (Mov_Contable.Cuenta >= W_Cuenta1 AND Mov_Contable.Cuenta <= W_Cuenta2)
        
        AND (IF W_Usuario1 = "0" AND W_Usuario2 = "9999" 
                THEN TRUE 
                ELSE (Mov_Contable.Usuario >=  cusi AND Mov_Contable.Usuario <=  cusf))
                BREAK 
            BY Mov_Contable.Cuenta
            BY Mov_Contable.Comprobante
            BY Mov_Contable.Num_Documento
            BY Mov_Contable.Agencia
            BY Mov_Contable.Fec_Contable : 
        
/*    FOR EACH Mov_Contable WHERE
               Mov_Contable.Agencia      GE  W_Ag1      AND
               Mov_Contable.Agencia      LE  W_Ag2      AND
               Mov_Contable.Cen_Costos   GE  W_CC1      AND
               Mov_Contable.Cen_Costos   LE  W_CC2      AND
               Mov_Contable.Comprobante  GE  W_CB1      AND
               Mov_Contable.Comprobante  LE  W_CB2      AND
               Mov_Contable.Fec_Contable GE  W_Fec1     AND
               Mov_Contable.Fec_Contable LE  W_Fec2     AND
               Mov_Contable.Cuenta       GE  W_Cuenta1  AND
               Mov_Contable.Cuenta       LE  W_Cuenta2  AND
               INTEGER(Mov_Contable.Usuario) GE  INTEGER(W_Usuario1) AND
               INTEGER(Mov_Contable.Usuario) LE  INTEGER(W_Usuario2) NO-LOCK
               BREAK BY Mov_Contable.Cuenta
                     BY Mov_Contable.Comprobante
                     BY Mov_Contable.Num_Documento
                     BY Mov_Contable.Agencia
                     BY Mov_Contable.Fec_Contable : */
          IF FIRST-OF(Mov_Contable.Cuenta) THEN DO:
             FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta NO-LOCK NO-ERROR.
             DISPLAY "Cuenta: "           AT 1
                      Mov_Contable.Cuenta AT 10
                      Cuentas.Nombre      AT 25 WHEN AVAIL(Cuentas)
              WITH FRAME F_Com WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
          END.  

          DISPLAY Mov_Contable.Agencia           FORMAT "999" LABEL "Ag."
                  Mov_Contable.Fec_Contab        FORMAT "99/99/99" LABEL "Fecha"
                  Mov_Contable.Comprob           LABEL "CP"
                  Mov_Contable.Num_Documento     FORMAT "999999999" LABEL "Documento"
                  Mov_Contable.Cuenta            FORMAT "X(14)"     LABEL "Cta-Contable"
                  Mov_Contable.Comentario        FORMAT "X(20)"     LABEL "Descripción Tx"
                  Mov_Contable.Nit               FORMAT "X(12)"     LABEL "Ced./ Nit"
                  Mov_Contable.Usuario           FORMAT "X(4)"      LABEL "Usu."
                  Mov_Contable.Doc_Referencia    FORMAT "X(10)"     LABEL "Docto-Ref."
                  Mov_Contable.Db                FORMAT ">>,>>>,>>>,>>9" LABEL "Valor Débitos"
                  Mov_Contable.Cr                FORMAT ">>>>,>>>,>>>,>>9" LABEL "Valor Créditos"
                  
            WITH DOWN FRAME F_Mov1 WIDTH 140 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

          ASSIGN TT_Db = TT_Db + Mov_Contable.Db
                 TT_Cr = TT_Cr + Mov_Contable.Cr
                 TC_Db = TC_Db + Mov_Contable.Db
                 TC_Cr = TC_Cr + Mov_Contable.Cr
                 TG_Db = TG_Db + Mov_Contable.Db
                 TG_Cr = TG_Cr + Mov_Contable.Cr.

          IF LAST-OF(Mov_Contable.Comprobante) THEN DO:
              DISPLAY "                      Total Comprobante: "                 
                      Mov_Contable.Comprobante              
                      "                                          __________________ ________________" SKIP
             "                                                                         "
                  "                "    
                  TC_Db 
                  " "
                      TC_Cr  SKIP(1)
              WITH DOWN FRAME F_TotCom WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
              ASSIGN TC_Db = 0 TC_Cr = 0.
          END.

          IF LAST-OF(Mov_Contable.Cuenta) THEN DO:
              DISPLAY "                                                         "
                      "                                "
                      "--------------    -------------" SKIP
                      "Total Cuenta " 
                      Mov_Contable.Cuenta
                      Cuentas.Nombre WHEN AVAIL(cuentas)
                      "                    "
                      TT_Db
                      " "
                      TT_Cr  SKIP(2)
              WITH DOWN FRAME F_Tot WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
              ASSIGN TT_Db = 0 TT_Cr = 0.
          END.
     END.  /*Fin For Each*/

     DISPLAY "                                          "
             "                      ----------------    ----------------" SKIP
             "                                                TOTAL GENERAL   " 
             TG_Db
         "      "
             TG_Cr  
          WITH DOWN FRAME F_TotGen WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

     ASSIGN TG_Db = 0
            TG_Cr = 0.

  END PROCE.

PROCEDURE TotXComp:
    DEFINE VAR  TA_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TA_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TK_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR  TK_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TC_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR  TC_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TG_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR  TG_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  TU_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.  
    DEFINE VAR  TU_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9" NO-UNDO.
    DEFINE VAR  tit_c  AS CHARACTER FORM "X(40)" NO-UNDO.
    IF W_Usuario2 = "999" THEN w_usuario2 = "9999".
    cusi = string(INTEGER(w_usuario1),"zzzz").
    cusf = string(INTEGER(w_usuario2),"zzzz").
    
    FOR EACH Mov_Contable no-lock
        WHERE
            Mov_Contable.Agencia = W_Ag1
        AND (IF W_CC1 = 0 THEN TRUE ELSE Mov_Contable.Cen_Costos = W_CC1)
        AND (IF W_CB1 = 0 THEN TRUE ELSE Mov_Contable.Comprobante = W_CB1)
        AND (Mov_Contable.Fec_Contable >=  W_Fec1 AND Mov_Contable.Fec_Contable <=  W_Fec2)
        AND (Mov_Contable.Cuenta >= W_Cuenta1 AND Mov_Contable.Cuenta <= W_Cuenta2)
        
        AND (IF W_Usuario1 = "0" AND W_Usuario2 = "9999" 
                THEN TRUE 
                ELSE (Mov_Contable.Usuario >=  cusi AND Mov_Contable.Usuario <=  cusf))
        BREAK BY Mov_Contable.Agencia
              BY Mov_contable.Cuenta
              BY Mov_Contable.Comprobante
              BY Mov_Contable.Fec_Contable 
              BY Mov_contable.Usuario:
/*            
    FOR EACH Mov_Contable WHERE
               Mov_Contable.Agencia      GE  W_Ag1      AND
               Mov_Contable.Agencia      LE  W_Ag2      AND
               Mov_Contable.Cen_Costos   GE  W_CC1      AND
               Mov_Contable.Cen_Costos   LE  W_CC2      AND
               Mov_Contable.Comprobante  GE  W_CB1      AND
               Mov_Contable.Comprobante  LE  W_CB2      AND
               Mov_Contable.Fec_Contable GE  W_Fec1     AND
               Mov_Contable.Fec_Contable LE  W_Fec2     AND
               Mov_Contable.Cuenta       GE  W_Cuenta1  AND
               Mov_Contable.Cuenta       LE  W_Cuenta2  AND
               INTEGER(Mov_Contable.Usuario) GE  INTEGER(W_Usuario1) AND
               INTEGER(Mov_Contable.Usuario) LE  INTEGER(W_Usuario2) NO-LOCK
               BREAK BY Mov_Contable.Agencia
                     BY Mov_contable.Cuenta
                     BY Mov_Contable.Comprobante
                     BY Mov_Contable.Fec_Contable 
                     BY Mov_contable.Usuario: */
          
        
        ASSIGN TC_Db = TC_Db + Mov_Contable.Db
               TC_Cr = TC_Cr + Mov_Contable.Cr
               TK_Db = TK_Db + Mov_Contable.Db
               TK_Cr = TK_Cr + Mov_Contable.Cr
               TG_Db = TG_Db + Mov_Contable.Db
               TG_Cr = TG_Cr + Mov_Contable.Cr
               TU_Db = TU_Db + Mov_Contable.Db
               TU_Cr = TU_Cr + Mov_Contable.Cr
               TA_Db = TA_Db + Mov_Contable.Db
               TA_Cr = TA_Cr + Mov_Contable.Cr.


        IF FIRST-OF(Mov_Contable.Agencia)  THEN DO:
            FIND FIRST agencias WHERE agencias.agencia EQ Mov_contable.agencia NO-LOCK NO-ERROR.
            DISPLAY  Mov_Contable.Agencia           FORMAT "999"  
                     "- "
                     Agencias.nombre      WHEN AVAIL(Agencias)          FORMAT "X(25)" SKIP(1)
                     WITH DOWN FRAME F_TotCom1 WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
        END.

        IF FIRST-OF(Mov_Contable.Cuenta) THEN DO:
           FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ Mov_Contable.Cuenta NO-LOCK NO-ERROR.
           DISPLAY "Cuenta: "       AT 5
                    Mov_Contable.Cuenta AT 15
                    Cuentas.Nombre      AT 30 WHEN AVAIL(Cuentas) SKIP(1)
                    WITH DOWN FRAME F_TotCom2 WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
        END.

        IF LAST-OF(Mov_Contable.Usuario) THEN DO:
            FIND FIRST comprobantes WHERE comprobantes.agencia     EQ Mov_Contable.agencia AND 
                                          comprobantes.comprobante EQ Mov_contable.comprobante NO-LOCK NO-ERROR.
             DISPLAY "        "
                     Mov_Contable.Comprobante "-" Comprobantes.nombre FORMAT "X(29)" WHEN AVAIL(Comprobantes)  LABEL "Comprobante"
                     Mov_Contable.Fec_Contab        FORMAT "99/99/99" LABEL "Fecha"
                     "  "
                     Mov_Contable.Usuario           FORMAT "X(4)"     LABEL "Usu."
                     "  "
                     TU_Db                          FORMAT ">>>>,>>>,>>>,>>9" LABEL "Valor Débitos"
                     "    "
                     TU_Cr                          FORMAT ">>>>,>>>,>>>,>>9" LABEL "Valor Créditos" SKIP(0) 
                     WITH DOWN FRAME F_TotCom3 WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
             ASSIGN TU_Db = 0  TU_Cr = 0.

        END.  

        IF LAST-OF(Mov_Contable.comprobante) THEN DO:
           DISPLAY
               "                                                      "
               "        -----------------     -----------------" SKIP(0)  
               "           Total Comprobante  " Mov_contable.Comprobante " - " Comprobantes.nombre FORMAT "X(25)" 
               TC_Db                          FORMAT ">>>>,>>>,>>>,>>9"  
               "    "
               TC_Cr                          FORMAT ">>>>,>>>,>>>,>>9" SKIP(1)
               WITH DOWN FRAME F_TotCom4 WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
           ASSIGN TC_Db = 0 TC_Cr = 0.
        END.

        IF LAST-OF(Mov_Contable.Cuenta) THEN DO:
           DISPLAY "                                                      "
                   "        -----------------     -----------------" SKIP(0)  
                   "Total por la cuenta " 
                    Mov_Contable.Cuenta 
                   Cuentas.Nombre   FORMAT "X(27)" WHEN AVAIL(Cuentas)
                   TK_Db                          FORMAT ">>>>,>>>,>>>,>>9"  
                   "    "
                   TK_Cr                          FORMAT ">>>>,>>>,>>>,>>9" SKIP(2)
                   WITH DOWN FRAME F_TotCom5 WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
           ASSIGN TK_Db = 0  TK_Cr = 0.
        END.
        IF LAST-OF(Mov_Contable.Agencia) THEN DO:
           DISPLAY
               "                                                      "
               "       -----------------     -----------------" SKIP(1)  
               "              Total Agencia "  Mov_contable.agencia " - " Agencias.nombre FORMAT "X(25)" 
               TA_Db                          FORMAT ">>>>,>>>,>>>,>>9"  
               "    "
               TA_Cr                          FORMAT ">>>>,>>>,>>>,>>9" SKIP(1)
               WITH DOWN FRAME F_TotCom6 WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.    
           ASSIGN TA_Db = 0 TA_Cr = 0.
        END.
        
     END.  /*Fin For Each*/

     DISPLAY "                                          "
             "                      ----------------    ----------------" SKIP
             "                                                TOTAL GENERAL   " 
             TG_Db
             "      "
             TG_Cr  
          WITH DOWN FRAME F_TotGen WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.

     ASSIGN TG_Db = 0
            TG_Cr = 0.

  END PROCE.


  /***********************/
  PROCEDURE Imprimir_Excel:
      {Incluido\Def_Excel.i}
       /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
       E_NumFila = 1.
       E_NumColumn = 11.
       E_Fila      = "003" + "Age"          
                   + "003" + "Cbt"      
                   + "007" + "NumDoct"
                   + "014" + "Cuenta        "         
                   + "017" + "Comentario       "     
                   + "003" + "CCo"       
                   + "014" + "Nit           "            
                   + "010" + " DocRefere" 
                   + "014" + "Debito        "    
                   + "014" + "Credito       "    
                   + "004" + "Usua".        

       RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

      /* launch Excel so it is visible to the user */
      chExcelApp:Visible = TRUE.

      /* create a new Workbook */
      chWorkbook = chExcelApp:Workbooks:Add().

      /* get the active Worksheet */
      chWorkSheet = chExcelApp:Sheets:Item(1).

       DEFI VAR W_IdCC AS CHARACTER FORMAT "X(2)".
        
       IF W_Usuario2 = "999" THEN w_usuario2 = "9999".
       cusi = string(INTEGER(w_usuario1),"zzzz").
       cusf = string(INTEGER(w_usuario2),"zzzz").

       FOR EACH Mov_Contable no-lock
           WHERE
               Mov_Contable.Agencia = W_Ag1
           AND (IF W_CC1 = 0 THEN TRUE ELSE Mov_Contable.Cen_Costos = W_CC1)
           AND (IF W_CB1 = 0 THEN TRUE ELSE Mov_Contable.Comprobante = W_CB1)
           AND (Mov_Contable.Fec_Contable >=  W_Fec1 AND Mov_Contable.Fec_Contable <=  W_Fec2)
           AND (Mov_Contable.Cuenta >= W_Cuenta1 AND Mov_Contable.Cuenta <= W_Cuenta2)
           
           AND (IF W_Usuario1 = "0" AND W_Usuario2 = "9999" 
                   THEN TRUE 
                   ELSE (Mov_Contable.Usuario >=  cusi AND Mov_Contable.Usuario <=  cusf))
           BREAK 
               BY Mov_Contable.Comprobante 
               BY Mov_Contable.Fec_Contable 
               BY Mov_Contable.Agencia 
               BY Mov_Contable.Num_Documento:
        
/*        
       FOR EACH Mov_Contable WHERE
                Mov_Contable.Agencia      GE  W_Ag1      AND
                Mov_Contable.Agencia      LE  W_Ag2      AND
                Mov_Contable.Cen_Costos   GE  W_CC1      AND
                Mov_Contable.Cen_Costos   LE  W_CC2      AND
                Mov_Contable.Comprobante  GE  W_CB1      AND
                Mov_Contable.Comprobante  LE  W_CB2      AND
                Mov_Contable.Fec_Contable GE  W_Fec1     AND
                Mov_Contable.Fec_Contable LE  W_Fec2     AND
                INTEGER(Mov_Contable.Usuario) GE  INTEGER(W_Usuario1) AND
                INTEGER(Mov_Contable.Usuario) LE  INTEGER(W_Usuario2) NO-LOCK
                BREAK 
                    BY Mov_Contable.Comprobante 
                    BY Mov_Contable.Fec_Contable 
                    BY Mov_Contable.Agencia 
                    BY Mov_Contable.Num_Documento:
*/                        
            E_Fila2     = "".
            E_Fila2     = "003" + STRING(Mov_Contable.Agencia,"999")
                        + "003" + STRING(Mov_Contable.Comprobante,"999")
                        + "007" + STRING(Mov_Contable.Num_Documento,"9999999")
                        + "014" + STRING(Mov_Contable.Cuenta,"X(14)")
                        + "017" + STRING(Mov_Contable.Comentario,"X(20)")
                        + "003" + STRING(Mov_Contable.Cen_Costos,"999")
                        + "014" + STRING(Mov_Contable.Nit,"X(14)")
                        + "010" + STRING(Mov_Contable.Doc_Referencia,"X(10)")
                        + "014" + STRING(Mov_Contable.Db,">>,>>>,>>>,>>9")
                        + "014" + STRING(Mov_Contable.Cr,">>,>>>,>>>,>>9")
                        + "004" + STRING(Mov_Contable.Usuario,"X(4)").
            {Incluido\imprimir_Excel.i}
  END PROCEDURE.
                                           
/*          DISPLAY Mov_Contable.Agencia        AT 1   FORMAT "999"
                  Mov_Contable.Num_Documento  AT 5   FORMAT "9999999"
                  Mov_Contable.Cuenta         AT 13  FORMAT "X(14)"
                  Mov_Contable.Comentario     AT 28  FORMAT "X(17)"
/*                  Mov_Contable.Cen_Costos     AT 46  FORMAT "999"*/
                  Mov_Contable.Nit            AT 46  FORMAT "X(14)" /*50*/
                  Nom_Cliente                 AT 61  FORMAT "X(10)"
                  Mov_Contable.Doc_Referencia AT 74  FORMAT "X(7)" /*"X(10)" 71*/
                  Mov_Contable.Db             AT 84  FORMAT ">>,>>>,>>>,>>9"
                  Mov_Contable.Cr             AT 100 FORMAT ">>,>>>,>>>,>>9"
                  Mov_Contable.Usuario        AT 117 FORMAT "X(4)"
          WITH DOWN FRAME F_Mov WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
*/
