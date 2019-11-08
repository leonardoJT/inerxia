  DEFINE VARIABLE W_Informe AS CHARACTER FORMAT "X(50)" INITIAL "Sucursales y Agencias".

{incluido/Variable.i "SHARED"}.
{incluido/Varcon.i "SHARED"}.

  DEFINE VARIABLE W_Naturaleza LIKE Cuentas.Naturaleza.
  DEFINE VARIABLE W_CtrNat     LIKE Cuentas.Ctr_Naturaleza.
  DEFINE VARIABLE L_CC         AS LOGICAL INITIAL YES.
  DEFINE VARIABLE W_Com        AS LOGICAL INITIAL NO.

  DEFINE VARIABLE W_NomNit AS CHARACTER FORMAT "X(40)".
  /* Nuevo*/
  DEFINE TEMP-TABLE tempmov
     FIELD Nit            LIKE mov_contable.Nit
     FIELD Cuenta         LIKE mov_contable.Cuenta
     FIELD Fec_Contable   LIKE mov_contable.fec_contable
     FIELD Agencia        LIKE mov_contable.Agencia
     FIELD Cen_costos     LIKE mov_contable.cen_costos
     FIELD Comprobante    LIKE mov_contable.Comprobante
     FIELD Usuario        LIKE mov_contable.Usuario
     FIELD Num_Documento  LIKE mov_contable.num_documento
     FIELD Destino        LIKE mov_contable.destino
     FIELD Doc_Referencia LIKE mov_contable.doc_referencia
     FIELD Comentario     LIKE mov_contable.comentario
     FIELD Db             LIKE mov_contable.Db
     FIELD Cr             LIKE mov_contable.Cr
     INDEX x3 nit cuenta fec_contable agencia.    
  /***********************/

/* incluido de Pantalla con parametros */
{incluido/Pantalla_Validacion2.i}

ON RETURN TAB.
  
  VIEW W_Pantalla ACTIVE-WINDOW.
  
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
     DEFINE VAR W_Cta LIKE Cuentas.Cuenta.
     DEFINE VAR W_Nom LIKE Cuentas.Nombre.
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
     DEFINE VAR W_Cta LIKE Cuentas.Cuenta.
     DEFINE VAR W_Nom LIKE Cuentas.Nombre.
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

  ON CHOOSE OF Btn_Imprimir
  DO:

     ASSIGN W_CC1 = INTEGER(SUBSTRING(Cmb_CenCost:SCREEN-VALUE,1,3))
            W_CC2 = INTEGER(SUBSTRING(Cmb_CenCost:SCREEN-VALUE,1,3))
            W_AG1 = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
            W_AG2 = INTEGER(SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3))
            W_CB1 = INTEGER(SUBSTRING(Cmb_Comprob:SCREEN-VALUE,1,2))
            W_CB2 = INTEGER(SUBSTRING(Cmb_Comprob:SCREEN-VALUE,1,2)).
     IF SUBSTRING(Cmb_CenCost:SCREEN-VALUE,1,3) EQ "000" THEN
        ASSIGN W_CC1 = 0
               W_CC2 = 999.
     IF SUBSTRING(Cmb_Agencia:SCREEN-VALUE,1,3) EQ "000" THEN
        ASSIGN W_AG1 = 0
               W_AG2 = 999.
     IF SUBSTRING(Cmb_Comprob:SCREEN-VALUE,1,2) EQ "00" THEN
        ASSIGN W_CB1 = 0
               W_CB2 = 99.
     ASSIGN FRAME F_Valida W_Fec1    W_Fec2
                           W_Cuenta1 W_Cuenta2
                           W_Nit1    W_Nit2
                           W_Usuario1 W_Usuario2
                           W_Porcentaje Cmb_Nivel W_Base.
     IF W_Cuenta1 EQ "" AND W_Cuenta2 EQ "" THEN
        ASSIGN W_Cuenta1 = "0" W_Cuenta2 = "99999999999999".
     IF W_Nit1 EQ "" AND W_Nit2 EQ "" THEN
        ASSIGN W_Nit1 = "0" W_Nit2 = "99999999999999".
     RUN Proceso_Imprimir.
     ON RETURN TAB.
  END.
   
RUN Habilita_Deshabilita.
WAIT-FOR CHOOSE OF Btn_Salir FOCUS Cmb_Agencia.
  ON CURSOR-DOWN CURSOR-DOWN.
  ON RETURN RETURN.
  HIDE FRAME F_Valida NO-PAUSE IN WINDOW W_Pantalla.
  DELETE WIDGET W_Pantalla.
  
  
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
      DEFINE VAR Listado AS CHARACTER INITIAL "".
      Listado = w_Pathspl + "Auxiliar.lst".
     {incluido\IMPRIMIR.I "listado"}.
  END PROCEDURE.

  PROCEDURE ProcesoImprimir:
     /* Nuevo*/
      DEFINE VARIABLE viconta   AS INTEGER NO-UNDO.
      DEFINE VARIABLE viageini  AS INTEGER INITIAL 1  NO-UNDO.
      DEFINE VARIABLE viagefin  AS INTEGER INITIAL 55 NO-UNDO.
      DEFINE VARIABLE viAge1   AS INTEGER NO-UNDO.
      DEFINE VARIABLE viAge2   AS INTEGER NO-UNDO.
      DEFINE VARIABLE viCen1   AS INTEGER NO-UNDO.
      DEFINE VARIABLE viCen2   AS INTEGER NO-UNDO.
      DEFINE VARIABLE viCpt1   AS INTEGER NO-UNDO.
      DEFINE VARIABLE viCpt2   AS INTEGER NO-UNDO.
      DEFINE VARIABLE visi     AS INTEGER INITIAL 0 NO-UNDO.
      DEFINE VARIABLE vccuenta AS CHARACTER NO-UNDO.
        
     EMPTY TEMP-TABLE TempMov.       
     MESSAGE "Desea Sacar solo los totales de las cuentas?"
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice AS LOGICAL.
     /* Nuevo */
     ASSIGN viAge1 = W_Ag1
            viAge2 = W_Ag2.
        
     IF (viAge1 EQ 0 AND viAge2 EQ 999) THEN DO:
        FIND LAST agencias NO-LOCK  NO-ERROR.
        IF AVAILABLE(agencias) THEN 
           ASSIGN viageini = 1
                  viagefin = agencias.agencia.
    END.
    ELSE 
        ASSIGN viageini = viAge1
               viagefin = viAge1.
            
    DO viconta = viageini TO viagefin:
       FOR EACH Mov_Contable
           WHERE Mov_Contable.Agencia      EQ viconta AND
                 Mov_Contable.Fec_Contable GE W_Fec1  AND Mov_Contable.Fec_Contable LE W_Fec2 NO-LOCK:
           CREATE TempMov.
           UPDATE TempMov.Nit              =  mov_contable.nit
                  TempMov.Cuenta           =  mov_contable.cuenta
                  TempMov.Fec_Contable     =  mov_contable.fec_contable
                  TempMov.Agencia          =  mov_contable.agencia
                  TempMov.Cen_Costos       =  mov_contable.cen_costos
                  TempMov.Comprobante      =  mov_contable.comprobante
                  TempMov.Usuario          =  mov_contable.usuario
                  TempMov.Num_Documento    =  mov_contable.num_documento
                  TempMov.Destino          =  mov_contable.Destino
                  TempMov.Doc_Referencia   =  Mov_Contable.Doc_Referencia
                  TempMov.Comentario       =  mov_contable.comentario
                  TempMov.Db               =  mov_contable.db
                  TempMov.Cr               =  mov_contable.cr.
       END.
    END.
    /******************/        
            
        
     {Incluido\RepEncabezado.i}
      DEFINE VARIABLE W_EstadoInf   AS CHARACTER FORMAT "X(8)" INITIAL "".
      DEFINE VARIABLE Nom_Cencosto  AS CHARACTER FORMAT "X(2)" INITIAL "".
      W_Reporte   = "REPORTE   : SUCURSALES Y AGENCIAS - Agencia: " + STRING(Cmb_Agencia:SCREEN-VALUE IN FRAME F_Valida,"X(25)")
                    + " - "+ STRING(TIME,"hh:mm am").
/*                1         2         3         4         5         6         7         8         9         10        11        12
         123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789*/
      W_EncColumna = "              COM NUM.DOC   C.C DOC.REF      COMENTARIO                             DEBITO          CREDITO          USU".
      DEFINE VAR  TT_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
      DEFINE VAR  TT_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
      DEFINE VAR  TC_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
      DEFINE VAR  TC_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
      
      DEFINE VAR  TTT_Db  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
      DEFINE VAR  TTT_Cr  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".


      DEFINE VAR  W_Cta AS CHARACTER FORMAT "X(14)".
      DEFINE VAR  W_Nom AS CHARACTER FORMAT "X(20)".
      DEFINE FRAME F_Mov

      WITH DOWN FRAME F_Mov WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.

      VIEW FRAME F-Encabezado.
      VIEW FRAME f-ftr.
      FOR EACH TempMov WHERE
               TempMov.Agencia      GE  W_Ag1      AND
               TempMov.Agencia      LE  W_Ag2      AND
               TempMov.Cen_Costos   GE  W_CC1      AND
               TempMov.Cen_Costos   LE  W_CC2      AND
               TempMov.Comprobante  GE  W_CB1      AND
               TempMov.Comprobante  LE  W_CB2      AND
               TempMov.Fec_Contable GE  W_Fec1     AND
               TempMov.Fec_Contable LE  W_Fec2     AND
               TempMov.Cuenta       GE  W_Cuenta1  AND
               TempMov.Cuenta       LE  W_Cuenta2  AND
               TempMov.Nit          GE  W_Nit1     AND
               TempMov.Nit          LE  W_Nit2     AND
               INTEGER(TempMov.Usuario) GE  INTEGER(W_Usuario1) AND
               INTEGER(TempMov.Usuario) LE  INTEGER(W_Usuario2) NO-LOCK
               BREAK BY TempMov.Nit 
                     BY TempMov.Cuenta
                     BY TempMov.Fec_Contable
                     BY TempMov.Agencia:
          IF FIRST-OF(TempMov.Nit) THEN DO:
              FIND Clientes WHERE Clientes.Nit EQ TempMov.Nit NO-LOCK NO-ERROR.
              W_NomNit = "No Existe Cliente: " + STRING(TempMov.Nit).
              IF AVAILABLE Clientes THEN 
                 W_NomNit = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
              DISPLAY TempMov.Nit AT 1 FORMAT "X(14)" 
                      W_NomNit         AT 20
                      "------------------------------------------------------------------------------------------------------------------------" AT 1
              WITH FRAME F_Nit WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS STREAM-IO.
          END.
          IF FIRST-OF(TempMov.Cuenta) THEN DO:
             W_NomCta = "Cta No Existe".
             FIND Cuentas WHERE Cuentas.Cuenta EQ TempMov.Cuenta NO-LOCK NO-ERROR.
              IF AVAILABLE Cuentas THEN 
                 W_NomCta = Cuentas.Nombre.
        /*      DISPLAY SKIP(1)
                      "__________________________________________________________________________________________________________" AT 14
                      "Cuenta : "         AT 14 
                      Mov_Contable.Cuenta AT 25 FORMAT "X(14)" 
                      W_NomCta            AT 45 FORMAT "X(40)"
                      "----------------------------------------------------------------------------------------------------------" AT 14
              WITH FRAME F_Cta WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.*/
             
          END.
          IF NOT choice THEN
            DISPLAY TempMov.Fec_Contable   AT 2   FORMAT "99/99/9999"
                    TempMov.Comprobante    AT 14  FORMAT "99"
                    TempMov.Num_Documento  AT 18  FORMAT "9999999"
                    TempMov.Destino        AT 28  FORMAT "999"
                    TempMov.Doc_Referencia AT 32  FORMAT "X(10)"
                    TempMov.Comentario     AT 45  FORMAT "X(35)"
                    TempMov.Db             AT 84  FORMAT ">>,>>>,>>>,>>9"
                    TempMov.Cr             AT 100 FORMAT ">>,>>>,>>>,>>9"
                    TempMov.Usuario        AT 117 FORMAT "X(4)"
            WITH DOWN FRAME F_Mov WIDTH 132 NO-BOX USE-TEXT NO-UNDERLINE NO-LABELS.
          ASSIGN TT_Db = TT_Db + TempMov.Db
                 TT_Cr = TT_Cr + TempMov.Cr
                 TC_Db = TC_Db + TempMov.Db
                 TC_Cr = TC_Cr + TempMov.Cr
                 TTT_Db = TTT_Db + TempMov.Db
                 TTT_Cr = TTT_Cr + TempMov.Cr.
          IF LAST-OF(TempMov.Cuenta) THEN DO:
              IF NOT choice THEN
                 DISPLAY SKIP(1)
                        /*"__________________________________________________________________________________________________________" AT 14*/
                        "TotCuenta:"        AT 14 
                        TempMov.Cuenta AT 25 FORMAT "X(14)" 
                        W_NomCta            AT 45 FORMAT "X(35)"
                        TC_Db               AT 84  FORMAT ">>,>>>,>>>,>>9"
                        TC_Cr               AT 100 FORMAT ">>,>>>,>>>,>>9"
                        "----------------------------------------------------------------------------------------------------------" AT 14
                WITH FRAME F_TCta WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
              ELSE
                 DISPLAY SKIP(1)
                        "TotCuenta:"        AT 14 
                        TempMov.Cuenta AT 25 FORMAT "X(14)" 
                        W_NomCta            AT 45 FORMAT "X(35)"
                        TC_Db               AT 84  FORMAT ">>,>>>,>>>,>>9"
                        TC_Cr               AT 100 FORMAT ">>,>>>,>>>,>>9"
                WITH FRAME F_TCta2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
              ASSIGN TC_Cr = 0 TC_Db = 0.
          END.
          IF LAST-OF(TempMov.Nit) THEN DO:
              DISPLAY SKIP(1)
                      "__________________________________________________________________________________________________________" AT 14
                      "TotNit:"        AT 14 
                      TempMov.Nit    AT 25  FORMAT "X(14)" 
                      W_NomNit            AT 45  FORMAT "X(35)"
                      TT_Db               AT 84  FORMAT ">>,>>>,>>>,>>9"
                      TT_Cr               AT 100 FORMAT ">>,>>>,>>>,>>9" SKIP(2)
              WITH FRAME F_TNit WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
              ASSIGN TT_Cr = 0 TT_Db = 0.
          END.
          
      END.  
      DISPLAY SKIP(1)
              "__________________________________________________________________________________________________________" AT 14
              "Total :"        AT 14 
              TTT_Db               AT 84  FORMAT ">>,>>>,>>>,>>9"
              TTT_Cr               AT 100   FORMAT ">>,>>>,>>>,>>9" SKIP(2)
      WITH FRAME F_TTT WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
      ASSIGN TTT_Cr = 0 TTT_Db = 0.

      PAGE.
    OUTPUT CLOSE.
  END PROCEDURE.

  PROCEDURE Imprimir_Excel:
      MESSAGE "Opción no disponible" VIEW-AS ALERT-BOX INFORMATION.
/*      {Incluido\Def_Excel.i}
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
                BREAK BY Mov_Contable.Comprobante BY Mov_Contable.Fec_Contable BY Mov_Contable.Agencia BY Mov_Contable.Num_Documento:
            E_Fila2     = "".
            E_Fila2     = "003" + STRING(Mov_Contable.Agencia,"999")
                        + "003" + STRING(Mov_Contable.Comprobante,"999")
                        + "007" + STRING(Mov_Contable.Num_Documento,"9999999")
                        + "014" + STRING(Mov_Contable.Cuenta,"X(14)")
                        + "017" + STRING(Mov_Contable.Comentario,"X(17)")
                        + "003" + STRING(Mov_Contable.Cen_Costos,"999")
                        + "014" + STRING(Mov_Contable.Nit,"X(14)")
                        + "010" + STRING(Mov_Contable.Doc_Referencia,"X(10)")
                        + "014" + STRING(Mov_Contable.Db,">>,>>>,>>>,>>9")
                        + "014" + STRING(Mov_Contable.Cr,">>,>>>,>>>,>>9")
                        + "004" + STRING(Mov_Contable.Usuario,"X(4)").
            {Incluido\imprimir_Excel.i}*/
  END PROCEDURE.
                                           
