&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-ProxVctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-ProxVctos 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

  {Incluido\VARIABLE.I "SHARED"}

  DEFI TEMP-TABLE TMes
     FIELD Tipo AS   CHAR  FORM "X"
     FIELD TA   AS   INTEG FORM "9"
     FIELD AMes AS   INTEG FORM "999999"
     FIELD SdoK LIKE Ahorros.Sdo_Dispon INIT 0 FORM "->>>>>,>>>,>>>,>>9"
     FIELD SdoI LIKE Ahorros.Sdo_Dispon INIT 0 FORM "->>>>>,>>>,>>>,>>9"
     FIELD Nro  AS   INTEG FORM "9999999"
     FIELD Tasa LIKE Ahorros.Tasa
     FIELD Banda AS INTEG FORM 9.

  DEFI VAR W_ProrRen AS LOG INIT FALSE.
  DEFI VAR W_CedAho  LIKE Ahorros.Nit.
  DEFI VAR FecAho    LIKE Ahorros.Fec_UltTrans.
  DEFI VAR K      AS INTEGER.
  DEFI VAR AnoIni AS INTEG FORM "9999".
  DEFI VAR MesIni AS INTEG FORM "99".
  DEFI VAR AnoFin AS INTEG FORM "9999".
  DEFI VAR MesFin AS INTEG FORM "99".
  DEFI VAR FecIni AS INTEG FORM "999999".
  DEFI VAR FecFin AS INTEG FORM "999999".
  DEFI VAR FecCor AS INTEG FORM "999999".
  DEFI VAR I_Tasa LIKE Ahorros.Tasa.
  DEFI VAR LiqInt LIKE Ahorros.Sdo_Dispon.
  DEFI VAR W_RowidTMes AS ROWID.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Proc

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS FecCorte Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS m1 m2 Prc FecCorte 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-ProxVctos AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "Procesar" 
     SIZE 12 BY 1.15
     BGCOLOR 8 .

DEFINE VARIABLE FecCorte AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE m1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1.15
     BGCOLOR 7 FGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE m2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Prc AS DECIMAL FORMAT "999999":U INITIAL 0 
     LABEL "No.Registros procesados" 
     VIEW-AS FILL-IN 
     SIZE 10 BY 1
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Proc
     m1 AT ROW 1.35 COL 1 COLON-ALIGNED NO-LABEL
     m2 AT ROW 2.62 COL 1 COLON-ALIGNED NO-LABEL
     Prc AT ROW 3.96 COL 58 COLON-ALIGNED
     FecCorte AT ROW 5.88 COL 46 NO-LABEL
     Btn_Done AT ROW 8.54 COL 60
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 5.31 COL 46
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.72 BY 10.65.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-ProxVctos ASSIGN
         HIDDEN             = YES
         TITLE              = "Próximos Vencimientos por Bandas, Prog.W-ProxVctos.W"
         HEIGHT             = 10.65
         WIDTH              = 76.72
         MAX-HEIGHT         = 16
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 16
         VIRTUAL-WIDTH      = 80
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-ProxVctos
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Proc
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN FecCorte IN FRAME F_Proc
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN m1 IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN m2 IN FRAME F_Proc
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Prc IN FRAME F_Proc
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProxVctos)
THEN W-ProxVctos:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Proc
/* Query rebuild information for FRAME F_Proc
     _Query            is NOT OPENED
*/  /* FRAME F_Proc */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-ProxVctos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProxVctos W-ProxVctos
ON END-ERROR OF W-ProxVctos /* Próximos Vencimientos por Bandas, Prog.W-ProxVctos.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProxVctos W-ProxVctos
ON WINDOW-CLOSE OF W-ProxVctos /* Próximos Vencimientos por Bandas, Prog.W-ProxVctos.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-ProxVctos
ON CHOOSE OF Btn_Done IN FRAME F_Proc /* Procesar */
DO:
  SESSION:SET-WAIT-STATE("GENERAL").
  RUN Procesar.
  SESSION:SET-WAIT-STATE("").

  MESSAGE "El Informe fue Generado en la carpeta C:\Info_Cooprudea\ProxVctos + La FechaCorte."
      VIEW-AS ALERT-BOX INFO BUTTONS OK.

  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-ProxVctos
ON ENTRY OF Btn_Done IN FRAME F_Proc /* Procesar */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FecCorte
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FecCorte W-ProxVctos
ON ENTRY OF FecCorte IN FRAME F_Proc
DO:
  on return tab.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FecCorte W-ProxVctos
ON LEAVE OF FecCorte IN FRAME F_Proc
DO:
  ASSIGN FecCorte.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-ProxVctos 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cdats_Permanentes W-ProxVctos 
PROCEDURE Cdats_Permanentes :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR W_RowIdCdat AS ROWID.

  ASSIGN W_RowIdCdat = ROWID(Ahorros).

  FOR EACH Ahorros WHERE Ahorros.Nit        EQ W_CedAho 
                    AND  Ahorros.Tip_Ahorro EQ 3 NO-LOCK:
      IF  Ahorros.Estado  NE 1 
      AND ROWID(Ahorros)  NE W_RowIdCdat
      AND (FecAho - Ahorros.Fec_UltTran) GT 0 
      AND (FecAho - Ahorros.Fec_UltTran) LE 730 THEN DO:   /*730 dias para abrir el nuevo(FecAho)*/
          ASSIGN W_ProrRen = TRUE.
          LEAVE.
      END.
  END.

  FIND Ahorros WHERE ROWID(Ahorros) EQ W_RowIdCdat NO-LOCK NO-ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-ProxVctos  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Delete the WINDOW we created */
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProxVctos)
  THEN DELETE WIDGET W-ProxVctos.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-ProxVctos  _DEFAULT-ENABLE
PROCEDURE enable_UI :
/*------------------------------------------------------------------------------
  Purpose:     ENABLE the User Interface
  Parameters:  <none>
  Notes:       Here we display/view/enable the widgets in the
               user-interface.  In addition, OPEN all queries
               associated with each FRAME and BROWSE.
               These statements here are based on the "Other 
               Settings" section of the widget Property Sheets.
------------------------------------------------------------------------------*/
  DISPLAY m1 m2 Prc FecCorte 
      WITH FRAME F_Proc IN WINDOW W-ProxVctos.
  ENABLE FecCorte Btn_Done 
      WITH FRAME F_Proc IN WINDOW W-ProxVctos.
  {&OPEN-BROWSERS-IN-QUERY-F_Proc}
  VIEW W-ProxVctos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Halla_TempVcto W-ProxVctos 
PROCEDURE Halla_TempVcto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF FecCor GT FecIni AND SUBSTRING(STRING(FecCor),5,2) GT "12" THEN
     ASSIGN FecCor = FecCor + 1000
            FecCor = FecCor - 12.  


  IF FecCor GT FecFin THEN                                                                  
     FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                                 
                       AND TMes.Ta   EQ 5                            
                       AND TMes.AMes EQ FecFin + 1 NO-ERROR.                                
  ELSE DO:                                                                                  
     IF FecCor LT FecIni THEN                                                               
        FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                              
                          AND TMes.Ta   EQ 5                         
                          AND TMes.AMes EQ FecIni NO-ERROR.                                 
     ELSE                                                                                   
        FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                              
                          AND TMes.Ta   EQ 5                         
                          AND TMes.AMes EQ FecCor NO-ERROR.                                 
  END.                                                                                      
      
  IF NOT AVAIL(Tmes) THEN DO:                                                               
     CREATE TMes.                                                                           
     ASSIGN TMes.Tipo = "A"                                                                 
            TMes.Ta   = 5                                            
            TMes.AMes = FecCor.                                                             
                                                                                                
     IF FecCor GT FecFin THEN                                                        
        ASSIGN TMes.AMes = FecFin + 1.                                                      
     ELSE IF FecCor LT FecIni THEN                                                          
        ASSIGN TMes.AMes = FecIni.                                                          
  END.

  ASSIGN LiqInt      = ((Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) * (Ahorros.Tasa / 12)) / 100
         TMes.Nro    = TMes.Nro  + 1
         TMes.Tasa   = TMes.Tasa + Ahorros.Tasa 
         TMes.SdoI   = TMes.SdoI + LiqInt
         W_RowidTMes = ROWID(TMes).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_ProxVctos W-ProxVctos 
PROCEDURE Imp_ProxVctos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR T1    AS DEC FORM ">>>,>>>,>>>,>>9" INIT 0 EXTENT 2. 
  DEFI VAR T2    AS DEC FORM ">>>,>>>,>>>,>>9" INIT 0 EXTENT 2. 
  DEFI VAR W_Mes AS CHAR FORM "99".
  DEFI VAR W_Ano AS CHAR FORM "9999".

  DEFI VAR TT  AS DEC FORM "9999999999999.9999999".
  DEFI VAR TN  AS INTEG FORM "999999999".
  DEFI VAR TTT AS DEC FORM "9999999999999.9999999".
  DEFI VAR TTN AS INTEG FORM "999999999".

  FOR EACH TMes:
      W_Mes = SUBSTRING(STRING(TMes.AMes),5,2).
      IF W_Mes GT "12" THEN
         ASSIGN TMes.Banda = (TMes.AMes - FecIni) + 1
                W_Mes      = STRING((INTEG(W_Mes) - 12),"99")
                W_Ano      = STRING(INTEG(SUBSTRING(STRING(TMes.AMes),1,4)) + 1)
                TMes.AMes  = INTEG(STRING(W_Ano,"9999") + STRING(W_Mes,"99")).
      ELSE IF INTEG(SUBSTRING(STRING(TMes.AMes),1,4)) GT AnoIni THEN
         TMes.Banda = (TMes.AMes - 88 - FecIni) + 1.
      ELSE   
         TMes.Banda = (TMes.AMes - FecIni) + 1.

      IF TMes.Banda LE 1 OR TMes.Banda EQ ? THEN 
         TMes.Banda = 1.
      ELSE IF TMes.Banda EQ 2 THEN
         TMes.Banda = 2.
      ELSE IF TMes.Banda EQ 3 THEN
         TMes.Banda = 3.
      ELSE IF TMes.Banda LE 6 THEN
         TMes.Banda = 4.
      ELSE IF TMes.Banda LE 9 THEN
         TMes.Banda = 5.
      ELSE IF TMes.Banda LE 12 THEN
         TMes.Banda = 6.
      ELSE IF TMes.Banda GT 12 THEN
         TMes.Banda = 7.
  END.

  DISPLAY "   COOLANTA    -   Próximos Vencimientos de Ahorros y Créditos    Desde " +
          STRING(FecCorte,"99/99/9999") FORM "X(120)" SKIP(1)
       WITH DOWN WIDTH 250 FRAME Tit1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  /*El TMes.Ta = 3,(cdats) fue reemplazado por el 5*/
  FOR EACH TMes WHERE TMes.Tipo EQ "A" AND TMes.Ta NE 3 BREAK BY TMes.Tipo BY TA BY AMes:
      IF FIRST-OF(TMes.TA) THEN
         RUN Tit_ProxVcto.
               
      DISPLAY TMes.Tipo   LABEL "T"
              TMes.TA     LABEL "C"
              TMes.AMes   LABEL "Año-Mes Vcto"
              TMes.SdoK   LABEL "Valores de Capital"
              TMes.SdoI   LABEL "Vlrs. de Intereses"
              TMes.Nro    LABEL "No.Vctos"
              TMes.Tasa / TMes.Nro   LABEL "Tasa Promedio" FORM "->99.999"
         WITH DOWN WIDTH 250 FRAME Det1 NO-BOX NO-LABELS STREAM-IO USE-TEXT.

      ASSIGN T1[1] = T1[1] + TMes.SdoK
             T1[2] = T1[2] + TMes.SdoI
             TT    = TT + (TMes.Tasa / TMes.Nro)
             TN    = TN + 1.

      IF LAST-OF(TMes.TA) THEN DO:
         DISPLAY "                               "
                 T1[1]
                 T1[2]
                 "       "
                 TT / TN FORM "->99.999"
                 SKIP(2) WITH NO-LABELS.

         ASSIGN T1[1] = 0
                T1[2] = 0
                TT    = 0
                TN    = 0.
      END.
  END.

  ASSIGN T1[1] = 0
         T1[2] = 0
         TT    = 0
         TN    = 0.

  /*Continua el Resumen por bandas*/
  DISPLAY SKIP(2)
            "   Resumen Bandas de Ahorros(Contractuales)"
            SKIP(1)
         WITH DOWN WIDTH 250 FRAME Tit1BC NO-BOX NO-LABELS STREAM-IO USE-TEXT. /*Solo Contractuales*/

    FOR EACH TMes WHERE TMes.Tipo EQ "A" AND TMes.Ta EQ 2 BREAK BY TMes.Banda:
        ASSIGN T1[1] = T1[1] + TMes.SdoK
               T1[2] = T1[2] + TMes.SdoI
               T2[1] = T2[1] + TMes.SdoK
               T2[2] = T2[2] + TMes.SdoI
               TT    = TT    + (TMes.Tasa / TMes.Nro)
               TTT   = TTT   + (TMes.Tasa / TMes.Nro)
               TN    = TN    + 1
               TTN   = TTN   + 1.

        IF LAST-OF(TMes.Banda) THEN DO:
           DISPLAY TMes.Tipo   LABEL "T"
                   TMes.Banda  LABEL "Banda"
                   T1[1]       LABEL "Valores de Capital"
                   T1[2]       LABEL "Vlrs. de Intereses"
                   TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
              WITH DOWN WIDTH 250 FRAME Det1BC NO-BOX NO-LABELS STREAM-IO USE-TEXT.
           ASSIGN T1[1] = 0 
                  T1[2] = 0 
                  TT    = 0 
                  TN    = 0.
        END.
    END.

    DISPLAY SKIP(1)
            "Tot.Ahorros" 
            T2[1] 
            " "
            T2[2]
            "    "
            TTT / TTN FORM "->99.999"
        WITH DOWN WIDTH 250 FRAME Tit1TBC NO-BOX NO-LABELS STREAM-IO USE-TEXT. 

    ASSIGN T1[1] = 0 
           T1[2] = 0 
           TT    = 0 
           TN    = 0
           T2[1] = 0 
           T2[2] = 0 
           TTT   = 0 
           TTN   = 0.


    DISPLAY SKIP(2)
              "   Resumen Bandas de Ahorros(A-TNO CDATs)"
              SKIP(1)
           WITH DOWN WIDTH 250 FRAME Tit1BA NO-BOX NO-LABELS STREAM-IO USE-TEXT. /*Solo A-Termino*/

      FOR EACH TMes WHERE TMes.Tipo EQ "A" AND TMes.Ta EQ 5 BREAK BY TMes.Banda:
          ASSIGN T1[1] = T1[1] + TMes.SdoK
                 T1[2] = T1[2] + TMes.SdoI
                 T2[1] = T2[1] + TMes.SdoK
                 T2[2] = T2[2] + TMes.SdoI
                 TT    = TT    + (TMes.Tasa / TMes.Nro)
                 TTT   = TTT   + (TMes.Tasa / TMes.Nro)
                 TN    = TN    + 1
                 TTN   = TTN   + 1.

          IF LAST-OF(TMes.Banda) THEN DO:
             DISPLAY TMes.Tipo   LABEL "T"
                     TMes.Banda  LABEL "Banda"
                     T1[1]       LABEL "Valores de Capital"
                     T1[2]       LABEL "Vlrs. de Intereses"
                     TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
                WITH DOWN WIDTH 250 FRAME Det1BA NO-BOX NO-LABELS STREAM-IO USE-TEXT.
             ASSIGN T1[1] = 0 
                    T1[2] = 0 
                    TT    = 0 
                    TN    = 0.
          END.
      END.

      DISPLAY SKIP(1)
              "Tot.Ahorros" 
              T2[1] 
              " "
              T2[2]
              "    "
              TTT / TTN FORM "->99.999"
          WITH DOWN WIDTH 250 FRAME Tit1TBA NO-BOX NO-LABELS STREAM-IO USE-TEXT. 

      ASSIGN T1[1] = 0 
             T1[2] = 0 
             TT    = 0 
             TN    = 0
             T2[1] = 0 
             T2[2] = 0 
             TTT   = 0 
             TTN   = 0.


  DISPLAY SKIP(2)
          "   Resumen Bandas de Ahorros. (Contractuales + A-Termino)"
          SKIP(1)
       WITH DOWN WIDTH 250 FRAME Tit1B NO-BOX NO-LABELS STREAM-IO USE-TEXT. /*Solo Contractuales y A-Termino*/

  FOR EACH TMes WHERE TMes.Tipo EQ "A" AND (TMes.Ta EQ 2 OR 
                                            TMes.Ta EQ 5) BREAK BY TMes.Banda:
      ASSIGN T1[1] = T1[1] + TMes.SdoK
             T1[2] = T1[2] + TMes.SdoI
             T2[1] = T2[1] + TMes.SdoK
             T2[2] = T2[2] + TMes.SdoI
             TT    = TT    + (TMes.Tasa / TMes.Nro)
             TTT   = TTT   + (TMes.Tasa / TMes.Nro)
             TN    = TN    + 1
             TTN   = TTN   + 1.

      IF LAST-OF(TMes.Banda) THEN DO:
         DISPLAY TMes.Tipo   LABEL "T"
                 TMes.Banda  LABEL "Banda"
                 T1[1]       LABEL "Valores de Capital"
                 T1[2]       LABEL "Vlrs. de Intereses"
                 TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
            WITH DOWN WIDTH 250 FRAME Det1B NO-BOX NO-LABELS STREAM-IO USE-TEXT.
         ASSIGN T1[1] = 0 
                T1[2] = 0 
                TT    = 0 
                TN    = 0.
      END.
  END.

  DISPLAY SKIP(1)
          "Tot.Ahorros" 
          T2[1] 
          " "
          T2[2]
          "    "
          TTT / TTN FORM "->99.999"
      WITH DOWN WIDTH 250 FRAME Tit1TB NO-BOX NO-LABELS STREAM-IO USE-TEXT. 
 
  ASSIGN T1[1] = 0 
         T1[2] = 0 
         TT    = 0 
         TN    = 0
         T2[1] = 0 
         T2[2] = 0 
         TTT   = 0 
         TTN   = 0.
    
  /*Créditos Vctos 12 meses*/   
  DISPLAY SKIP(2)
          "     CREDITOS"
          SKIP(1)
      WITH DOWN WIDTH 250 FRAME Tit2bCCCRB NO-BOX NO-LABELS STREAM-IO USE-TEXT.  
    
  FOR EACH TMes WHERE TMes.Tipo EQ "C" BREAK BY AMes:
      DISPLAY TMes.Tipo   LABEL "T"
              TMes.TA     LABEL "C"
              TMes.AMes   LABEL "Año-Mes Vcto"
              TMes.SdoK   LABEL "Valores de Capital"
              TMes.SdoI   LABEL "Vlrs. de Intereses"
              TMes.Nro    LABEL "No.Vctos"
              TMes.Tasa / TMes.Nro   LABEL "Tasa Promedio" FORM "->99.999"
         WITH DOWN WIDTH 250 FRAME Det1Cr NO-BOX NO-LABELS STREAM-IO USE-TEXT.

      ASSIGN T1[1] = T1[1] + TMes.SdoK
             T1[2] = T1[2] + TMes.SdoI
             TT    = TT + (TMes.Tasa / TMes.Nro)
             TN    = TN + 1.      
  END.

  DISPLAY "                               "
          T1[1]                  
          T1[2]                         
          "       "                     
          TT / TN FORM "->99.999"       
          SKIP(2) WITH NO-LABELS.       

  ASSIGN T1[1] = 0 
         T1[2] = 0        
         TT    = 0        
         TN    = 0.        

  DISPLAY SKIP(2)
          "   Resumen Bandas de Créditos."
          SKIP(1)
      WITH DOWN WIDTH 250 FRAME Tit2bCC NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TMes WHERE TMes.Tipo EQ "C" BREAK BY TMes.Banda:
       ASSIGN T1[1] = T1[1] + TMes.SdoK
              T1[2] = T1[2] + TMes.SdoI
              T2[1] = T2[1] + TMes.SdoK
              T2[2] = T2[2] + TMes.SdoI
              TT    = TT    + (TMes.Tasa / TMes.Nro)
              TN    = TN    + 1
              TTT   = TTT   + (TMes.Tasa / TMes.Nro)
              TTN   = TTN   + 1.

      IF LAST-OF(TMes.Banda) THEN DO:
         DISPLAY TMes.Tipo   LABEL "T"
                 TMes.Banda  LABEL "Banda"
                 T1[1]       LABEL "Valores de Capital"
                 T1[2]       LABEL "Vlrs. de Intereses"
                 TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
            WITH DOWN WIDTH 250 FRAME Det1BCC NO-BOX NO-LABELS STREAM-IO USE-TEXT.
         ASSIGN T1[1] = 0 
                T1[2] = 0 
                TT    = 0 
                TN    = 0.
      END.
  END.

  DISPLAY SKIP(1)
          "Tot.Creditos" 
          T2[1]  
          " "
          T2[2]
          "   "
          TTT / TTN FORM "->99.999"
      WITH DOWN WIDTH 250 FRAME Tit1TBCrC NO-BOX NO-LABELS STREAM-IO USE-TEXT. 
 
  ASSIGN T1[1] = 0 
         T1[2] = 0 
         TT    = 0 
         TN    = 0
         T2[1] = 0 
         T2[2] = 0 
         TTT   = 0 
         TTN   = 0.

  DISPLAY SKIP(2)
          "   Resumen Bandas de Créditos(Por Tipo-Credito CONSUMO)."
          SKIP(1)
      WITH DOWN WIDTH 250 FRAME Tit2bCCC NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TMes WHERE TMes.Tipo EQ "C" AND TMes.TA EQ 1 BREAK BY TMes.Banda:
       ASSIGN T1[1] = T1[1] + TMes.SdoK
              T1[2] = T1[2] + TMes.SdoI
              T2[1] = T2[1] + TMes.SdoK
              T2[2] = T2[2] + TMes.SdoI
              TT    = TT    + (TMes.Tasa / TMes.Nro)
              TN    = TN    + 1
              TTT   = TTT   + (TMes.Tasa / TMes.Nro)
              TTN   = TTN   + 1.

      IF LAST-OF(TMes.Banda) THEN DO:
         DISPLAY TMes.Tipo   LABEL "T"
                 TMes.Banda  LABEL "Banda"
                 T1[1]       LABEL "Valores de Capital"
                 T1[2]       LABEL "Vlrs. de Intereses"
                 TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
            WITH DOWN WIDTH 250 FRAME Det1BCCC NO-BOX NO-LABELS STREAM-IO USE-TEXT.
         ASSIGN T1[1] = 0 
                T1[2] = 0 
                TT    = 0 
                TN    = 0.
      END.
  END.

  DISPLAY SKIP(1)
          "Tot.Creditos" 
          T2[1]  
          " "
          T2[2]
          "   "
          TTT / TTN FORM "->99.999"
      WITH DOWN WIDTH 250 FRAME Tit1TBCrCC NO-BOX NO-LABELS STREAM-IO USE-TEXT. 
 
  ASSIGN T1[1] = 0 
         T1[2] = 0 
         TT    = 0 
         TN    = 0
         T2[1] = 0 
         T2[2] = 0 
         TTT   = 0 
         TTN   = 0.
    
  DISPLAY SKIP(2)
          "   Resumen Bandas de Créditos(Por Tipo-Credito COMERCIAL)."
          SKIP(1)
      WITH DOWN WIDTH 250 FRAME Tit2bCCCc NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TMes WHERE TMes.Tipo EQ "C" AND TMes.TA EQ 2 BREAK BY TMes.Banda:
       ASSIGN T1[1] = T1[1] + TMes.SdoK
              T1[2] = T1[2] + TMes.SdoI
              T2[1] = T2[1] + TMes.SdoK
              T2[2] = T2[2] + TMes.SdoI
              TT    = TT    + (TMes.Tasa / TMes.Nro)
              TN    = TN    + 1
              TTT   = TTT   + (TMes.Tasa / TMes.Nro)
              TTN   = TTN   + 1.

      IF LAST-OF(TMes.Banda) THEN DO:
         DISPLAY TMes.Tipo   LABEL "T"
                 TMes.Banda  LABEL "Banda"
                 T1[1]       LABEL "Valores de Capital"
                 T1[2]       LABEL "Vlrs. de Intereses"
                 TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
            WITH DOWN WIDTH 250 FRAME Det1BCCCc NO-BOX NO-LABELS STREAM-IO USE-TEXT.
         ASSIGN T1[1] = 0 
                T1[2] = 0 
                TT    = 0 
                TN    = 0.
      END.
  END.

  DISPLAY SKIP(1)
          "Tot.Creditos" 
          T2[1]  
          " "
          T2[2]
          "   "
          TTT / TTN FORM "->99.999"
      WITH DOWN WIDTH 250 FRAME Tit1TBCrCCc NO-BOX NO-LABELS STREAM-IO USE-TEXT. 
 
  ASSIGN T1[1] = 0 
         T1[2] = 0 
         TT    = 0 
         TN    = 0
         T2[1] = 0 
         T2[2] = 0 
         TTT   = 0 
         TTN   = 0.
    
  DISPLAY SKIP(2)
          "   Resumen Bandas de Créditos(Por Tipo-Credito HIPOTECARIO-VVDA)."
          SKIP(1)
      WITH DOWN WIDTH 250 FRAME Tit2bCCV NO-BOX NO-LABELS STREAM-IO USE-TEXT.

  FOR EACH TMes WHERE TMes.Tipo EQ "C" AND TMes.TA EQ 3 BREAK BY TMes.Banda:
       ASSIGN T1[1] = T1[1] + TMes.SdoK
              T1[2] = T1[2] + TMes.SdoI
              T2[1] = T2[1] + TMes.SdoK
              T2[2] = T2[2] + TMes.SdoI
              TT    = TT    + (TMes.Tasa / TMes.Nro)
              TN    = TN    + 1
              TTT   = TTT   + (TMes.Tasa / TMes.Nro)
              TTN   = TTN   + 1.

      IF LAST-OF(TMes.Banda) THEN DO:
         DISPLAY TMes.Tipo   LABEL "T"
                 TMes.Banda  LABEL "Banda"
                 T1[1]       LABEL "Valores de Capital"
                 T1[2]       LABEL "Vlrs. de Intereses"
                 TT / TN     LABEL "Tasa Promedio" FORM "->99.999"
            WITH DOWN WIDTH 250 FRAME Det1BCCV NO-BOX NO-LABELS STREAM-IO USE-TEXT.
         ASSIGN T1[1] = 0 
                T1[2] = 0 
                TT    = 0 
                TN    = 0.
      END.
  END.

  DISPLAY SKIP(1)
          "Tot.Creditos" 
          T2[1]  
          " "
          T2[2]
          "   "
          TTT / TTN FORM "->99.999"
      WITH DOWN WIDTH 250 FRAME Tit1TBCrCV NO-BOX NO-LABELS STREAM-IO USE-TEXT. 
 
  ASSIGN T1[1] = 0 
         T1[2] = 0 
         TT    = 0 
         TN    = 0
         T2[1] = 0 
         T2[2] = 0 
         TTT   = 0 
         TTN   = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesar W-ProxVctos 
PROCEDURE Procesar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFI VAR W_ArchVcto AS CHAR FORM "X(35)".

   ASSIGN W_ArchVcto = "C:\info_cooprudea\ProxVctos-" + STRING(FecCorte,"999999") + ".Txt".
   DISPLAY "Generando Archivo Próximos Vencimientos..." @ m1 m2 WITH FRAME F_Proc.
   OUTPUT TO VALUE(W_ArchVcto).
   RUN ProxVctos.
   OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProxVctos W-ProxVctos 
PROCEDURE ProxVctos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH TMes: DELETE TMes. END.
  
  ASSIGN AnoIni = YEAR(FecCorte)
         MesIni = MONTH(FecCorte) + 1
         AnoFin = AnoIni + 1
         MesFin = MesIni.

  IF MesIni GT 12 THEN
     ASSIGN AnoIni = AnoIni + 1
            MesIni = 1.

  ASSIGN FecFin = INTEG(STRING(AnoFin,"9999") + STRING(MesFin,"99"))
         FecIni = INTEG(STRING(AnoIni,"9999") + STRING(MesIni,"99")).

  FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado  EQ 1 NO-LOCK
                    BREAK BY Pro_Ahorros.Tip_Ahorro BY Pro_Ahorros.Cod_Ahorro:
      IF Pro_Ahorros.Tip_Ahorro EQ 1 OR Pro_Ahorros.Tip_Ahorro EQ 4 THEN DO:
         FIND FIRST TMes WHERE TMes.Tipo EQ "A"                     
                           AND TMes.Ta   EQ Pro_Ahorros.Tip_Ahorro    
                           AND TMes.AMes EQ FecIni NO-ERROR.            
         IF NOT AVAIL(Tmes) THEN DO:                                    
            CREATE TMes.                                                 
            ASSIGN TMes.Tipo = "A"                                      
                   TMes.Ta   = Pro_Ahorros.Tip_Ahorro                 
                   TMes.AMes = FecIni.                                  
         END.                                                           
      END.

      FOR EACH Ahorros WHERE Ahorros.Cod_Ahorro EQ Pro_Ahorros.Cod_Ahorro
                       AND Ahorros.Estado       EQ 1 NO-LOCK:
          ASSIGN Prc = Prc + 1
                 Prc:SCREEN-VALUE IN FRAME F_Proc = STRING(Prc).

          IF Pro_Ahorros.Tip_Ahorro EQ 1 OR Pro_Ahorros.Tip_Ahorro EQ 4 THEN DO:          
             ASSIGN TMes.SdoK = TMes.SdoK + (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)
                    TMes.Nro  = TMes.Nro  + 1.

             IF Pro_Ahorros.Tip_Ahorro EQ 1 THEN DO:
                RUN Halla_TasaInd.   /*Al final de este mismo procedimiento*/
                TMes.Tasa = TMes.Tasa + (I_Tasa * 365).
             END.
          END.
          ELSE DO:
             ASSIGN FecCor = INTEG(STRING(YEAR(Ahorros.Fec_Vencimiento),"9999") + 
                                      STRING(MONTH(Ahorros.Fec_Vencimiento),"99")).
             IF FecCor GT FecFin THEN
                FIND FIRST TMes WHERE TMes.Tipo EQ "A"
                                  AND TMes.Ta   EQ Pro_Ahorros.Tip_Ahorro
                                  AND TMes.AMes EQ FecFin + 1 NO-ERROR.
             ELSE DO:
                IF FecCor LT FecIni THEN  
                   FIND FIRST TMes WHERE TMes.Tipo EQ "A"
                                     AND TMes.Ta   EQ Pro_Ahorros.Tip_Ahorro
                                     AND TMes.AMes EQ FecIni NO-ERROR.
                ELSE
                   FIND FIRST TMes WHERE TMes.Tipo EQ "A"
                                     AND TMes.Ta   EQ Pro_Ahorros.Tip_Ahorro
                                     AND TMes.AMes EQ FecCor NO-ERROR.
             END.
             IF NOT AVAIL(Tmes) THEN DO:
                CREATE TMes.
                ASSIGN TMes.Tipo = "A"                     
                       TMes.Ta   = Pro_Ahorros.Tip_Ahorro
                       TMes.AMes = FecCor.

                IF FecCor GT FecFin THEN
                   ASSIGN TMes.AMes = FecFin + 1.
                ELSE IF FecCor LT FecIni THEN
                   ASSIGN TMes.AMes = FecIni.
             END.
             
             ASSIGN TMes.SdoK = TMes.SdoK + (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)
                    TMes.Nro  = TMes.Nro  + 1.

             IF Pro_Ahorros.Tip_Ahorro EQ 2 THEN 
                ASSIGN TMes.SdoI = TMes.SdoI + Ahorros.INT_Pagar
                       TMes.Tasa = TMes.Tasa + Ahorros.Tasa.
             ELSE DO:
                ASSIGN LiqInt    = ((Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) * (Ahorros.Tasa / 12)) / 100
                       TMes.Tasa = TMes.Tasa + Ahorros.Tasa 
                       TMes.SdoI = TMes.SdoI + LiqInt.      
             END.             
          END.         
      END.
  END.

  RUN ProxVctos_ATno.   /*De los Cdats con base en las Prorrogas y Renovaciones*/

  RUN ProxVctos_Creditos.  /*Halla los prox.vctos de Créditos*/

END PROCEDURE.

PROCEDURE Halla_TasaInd:
    DEFI VAR I_TasAux LIKE Ahorros.Tasa.

    FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Ahorros.Indicador
                              AND   Indicadores.Estado EQ 1 NO-LOCK NO-ERROR.
    IF AVAILABLE(Indicadores) THEN DO:
       ASSIGN I_Tasa = Indicadores.Tasa.
       IF Indicadores.Rangos EQ TRUE THEN DO:
          FIND FIRST Ran_Intereses WHERE      Ran_Intereses.Indicador EQ Indicadores.Indicador
                                      AND   Ran_Intereses.Val_Inicial LE Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
                                      AND   Ran_Intereses.Val_Final   GE Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje
                                      AND   Ran_Intereses.Pla_Inicial LE 9999
                                      AND   Ran_Intereses.Pla_Final   GE 9999
                                      AND   Ran_Interes.Estado        EQ 1 NO-LOCK NO-ERROR.
          IF AVAILABLE(Ran_Intereses) THEN 
             ASSIGN I_Tasa = I_Tasa + Ran_Intereses.Puntos_Asoc.

          RUN EFNV IN W_ManFin (INPUT (I_Tasa / 100),INPUT 365,OUTPUT I_TasAux).
          ASSIGN I_Tasa = I_TasAux * 100.
       END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProxVctos_ATno W-ProxVctos 
PROCEDURE ProxVctos_ATno :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR FecContVcto LIKE Ahorros.Fec_ProLiqui.

  FOR EACH Ahorros WHERE Ahorros.Tip_Ahorro EQ 3 
                     AND Ahorros.Estado     EQ 1  NO-LOCK BY Ahorros.Nit:
      ASSIGN FecCor = INTEG(STRING(YEAR(Ahorros.Fec_ProLiqui),"9999") +                   
                            STRING(MONTH(Ahorros.Fec_ProLiqui),"99"))
             Prc    = Prc + 1
             Prc:SCREEN-VALUE IN FRAME F_Proc = STRING(Prc).

      /*Control de liq.int.x c/mes*/
      IF  Ahorros.Per_Liquid GE 4 AND Ahorros.Per_Liquid LE 8
      AND Plazo GT 1 THEN DO:
          IF  Ahorros.Fec_Vencimiento GT FecCorte  
          AND (Ahorros.Fec_Vencimiento - FecCorte) GE 31 THEN DO: 
             IF Ahorros.Fec_ProLiqui + 2 GT Ahorros.Fec_Vencimiento THEN. /*Prox vcto con ult.intereses lo controla el código sgte.*/
             ELSE DO:
                 ASSIGN FecContVcto = Ahorros.Fec_ProLiqui.
                 IF Ahorros.Per_Liquid EQ 4 THEN DO K = 1 TO Plazo / 30:
                    RUN Halla_TempVcto.
                    ASSIGN FecCor      = FecCor + 1
                           FecContVcto = FecContVcto + 31.
                    IF FecContVcto GE Ahorros.Fec_Vencimiento THEN
                       LEAVE.                    
                 END.
                 ELSE IF Ahorros.Per_Liquid EQ 5 THEN DO K = 1 TO Plazo / 30:
                    RUN Halla_TempVcto.
                    ASSIGN FecCor      = FecCor + 2
                           FecContVcto = FecContVcto + 61.
                    IF FecContVcto GE Ahorros.Fec_Vencimiento THEN
                       LEAVE.                    
                 END.
                 ELSE IF Ahorros.Per_Liquid EQ 6 THEN DO K = 1 TO Plazo / 30:
                    RUN Halla_TempVcto.
                    ASSIGN FecCor      = FecCor + 3
                           FecContVcto = FecContVcto + 92.
                    IF FecContVcto GE Ahorros.Fec_Vencimiento THEN
                       LEAVE.                 
                 END.
                 ELSE IF Ahorros.Per_Liquid EQ 8 THEN DO K = 1 TO Plazo / 30:
                    RUN Halla_TempVcto.
                    ASSIGN FecCor      = FecCor + 6
                           FecContVcto = FecContVcto + 184.
                    IF FecContVcto GE Ahorros.Fec_Vencimiento THEN
                       LEAVE.          
                 END.
             END.
         END.
         /*de lo contrario: Prox vcto con ult.intereses lo controla el código sgte.*/
      END.
      /*de lo contrario: Prox vcto con ult.intereses lo controla el código sgte.*/

      ASSIGN FecCor = INTEG(STRING(YEAR(Ahorros.Fec_Vencimiento),"9999") +                   
                      STRING(MONTH(Ahorros.Fec_Vencimiento),"99"))
             W_ProrRen = FALSE.

      IF Ahorros.Fec_Prorroga NE ? AND (W_Fecha - Ahorros.Fec_Prorroga) GT 3 THEN
         W_ProrRen = TRUE.
      ELSE DO:
         ASSIGN W_CedAho = Ahorros.Nit
                FecAho   = Ahorros.Fec_Apertura.
         RUN Cdats_Permanentes.   /*Halla los Cdats Renovados en Otros*/    
      END.

      /*Fecha para Los intereses en el vcto normal*/
      IF FecCor GT FecFin THEN                                                                  
         FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                                 
                           AND TMes.Ta   EQ 5                            
                           AND TMes.AMes EQ FecFin + 1 NO-ERROR.                                
      ELSE DO:                                                                                  
         IF FecCor LT FecIni THEN                                                               
            FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                              
                              AND TMes.Ta   EQ 5                         
                              AND TMes.AMes EQ FecIni NO-ERROR.                                 
         ELSE                                                                                   
            FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                              
                              AND TMes.Ta   EQ 5                         
                              AND TMes.AMes EQ FecCor NO-ERROR.                                 
      END.                                                                                      
      IF NOT AVAIL(Tmes) THEN DO:                                                               
         CREATE TMes.                                                                           
         ASSIGN TMes.Tipo = "A"                                                                 
                TMes.Ta   = 5                                            
                TMes.AMes = FecCor.                                                             
                                                                                                
         IF FecCor GT FecFin THEN                                                        
            ASSIGN TMes.AMes = FecFin + 1.                                                      
         ELSE IF FecCor LT FecIni THEN                                                          
            ASSIGN TMes.AMes = FecIni.                                                          
      END.

      /*Los intereses en el vcto normal*/
      ASSIGN LiqInt   = ((Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) * (Ahorros.Tasa / 12)) / 100
             TMes.Nro = TMes.Nro  + 1
             TMes.Tasa = TMes.Tasa + Ahorros.Tasa 
             TMes.SdoI = TMes.SdoI + LiqInt.       

      IF NOT W_ProrRen THEN DO:  /*No Son prorrogas Ni Renovaciones, K en el vcto normal*/
         ASSIGN TMes.SdoK = TMes.SdoK + (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje).
         NEXT.
      END.

      /*Continua K solo para Renovac.y Prorrogas en el Ultimo Mes*/
      ASSIGN FecCor = FecFin + 1.

      FIND FIRST TMes WHERE TMes.Tipo EQ "A"                                                 
                           AND TMes.Ta   EQ 5                            
                           AND TMes.AMes EQ FecFin + 1 NO-ERROR.                                
      IF NOT AVAIL(Tmes) THEN DO:                                                               
         CREATE TMes.                                                                           
         ASSIGN TMes.Tipo = "A"                                                                 
                TMes.Ta   = 5                                            
                TMes.AMes = FecFin    + 1
                TMes.Nro  = TMes.Nro  + 1
                TMes.Tasa = TMes.Tasa + (Ahorros.Tasa / 12).
      END.                                                                                      
                                                                                                
      ASSIGN TMes.SdoK = TMes.SdoK + (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje).
                                                                      
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProxVctos_Creditos W-ProxVctos 
PROCEDURE ProxVctos_Creditos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR FecCre AS INTEG FORM "999999".
  DEFI VAR W_EPag AS INTEG FORM "99".
  DEFI VAR SdoK   LIKE Creditos.Sdo_Capital.
  DEFI VAR IntP   LIKE Creditos.Sdo_Capital.
  DEFI VAR K      AS INTEG FORM "99999".

  ASSIGN FecFin = FecIni + 12.

  FIND FIRST Indicadores WHERE Indicadores.Indicador EQ 1 AND estado EQ 1 NO-LOCK NO-ERROR.

  FOR EACH Creditos WHERE Creditos.Estado         EQ 2 
                      AND Creditos.Sdo_Capital    GT 0 NO-LOCK:
      ASSIGN W_EPag = 4.33  /*Inicia Semanal*/
             Prc    = Prc + 1
             Prc:SCREEN-VALUE IN FRAME F_Proc = STRING(Prc).

      IF Creditos.Per_Pago EQ 4 THEN
         ASSIGN W_EPag  = 1.

      FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Creditos.Cod_Credito NO-LOCK NO-ERROR.

      FIND FIRST TMes WHERE TMes.Tipo EQ "C"                     
                        AND TMes.Ta   EQ Pro_Creditos.Tip_Credito    
                        AND TMes.AMes EQ FecIni - 1 NO-ERROR.            
      IF NOT AVAIL(Tmes) THEN DO:                                    
         CREATE TMes.                                                 
         ASSIGN TMes.Tipo = "C"                                      
                TMes.Ta   = Pro_Creditos.Tip_Credito WHEN AVAIL(Pro_Creditos)              
                TMes.AMes = FecIni - 1
                TMes.Nro  = TMes.Nro  + 1
                TMes.SdoI = TMes.SdoI + (Creditos.Int_Corrientes +  Creditos.Int_MorCobrar).
         IF Creditos.Tasa LE Indicadores.Tasa THEN
            TMes.Tasa = TMes.Tasa + Creditos.Tasa.
         ELSE 
            TMes.Tasa = TMes.Tasa + Indicadores.Tasa.
      END. 

      ASSIGN SdoK   = Creditos.Sdo_Capital
             FecCre = FecIni - 1.

      DO K = 1 TO Creditos.Plazo:  
         IF SdoK GT 0 THEN DO:
            IF K GT 12 THEN 
               FIND FIRST TMes WHERE TMes.Tipo EQ "C"                     
                                 AND TMes.Ta   EQ Pro_Creditos.Tip_Credito    
                                 AND TMes.AMes EQ FecCre + 13 NO-ERROR.  
            ELSE
               FIND FIRST TMes WHERE TMes.Tipo EQ "C"                     
                                 AND TMes.Ta   EQ Pro_Creditos.Tip_Credito    
                                 AND TMes.AMes EQ FecCre + K NO-ERROR.
            IF NOT AVAIL(Tmes) THEN DO:                                    
               CREATE TMes.                                                 
               ASSIGN TMes.Tipo = "C"                                      
                      TMes.Ta   = Pro_Creditos.Tip_Credito                 
                      TMes.AMes = FecCre + K.
               IF K GT 12 THEN
                  TMes.AMes = FecCre + 13.
            END.

            ASSIGN TMes.Nro  = TMes.Nro  + 1
                   TMes.Tasa = TMes.Tasa + Creditos.Tasa 
                   IntP      = SdoK * ((Creditos.Tasa / 12) / 100)
                   TMes.SdoI = TMes.SdoI + IntP.

            IF SdoK - ((Creditos.Cuota * W_EPag) - IntP) GE 0 THEN DO:
               ASSIGN SdoK      = SdoK      - ((Creditos.Cuota * W_EPag) - IntP)
                      TMes.SdoK = TMes.SdoK + ((Creditos.Cuota * W_EPag) - IntP).

               IF K EQ Creditos.Plazo AND SdoK GT 0 THEN DO:
                  ASSIGN TMes.SdoK = TMes.SdoK + SdoK
                         SdoK      = 0.
                  LEAVE.
               END.
            END.
            ELSE DO:
               ASSIGN TMes.SdoK = TMes.SdoK + SdoK
                      SdoK      = 0.
               LEAVE.
            END.
         END.
         ELSE 
            LEAVE.
      END.
  END.

  RUN Imp_ProxVctos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Tit_ProxVcto W-ProxVctos 
PROCEDURE Tit_ProxVcto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF TMes.Tipo = "C" THEN
         DISPLAY "       CREDITOS"
                 SKIP (0) WITH FRAME F1 NO-LABELS.
  ELSE DO:
      IF TMes.TA EQ 1 THEN
         DISPLAY "   AHORROS A LA VISTA" 
                 SKIP (0) WITH FRAME F2 NO-LABELS.
      ELSE IF TMes.TA EQ 2 THEN
         DISPLAY "   AHORROS CONTRACTUALES" 
                 SKIP (0) WITH FRAME F3 NO-LABELS.
      ELSE IF TMes.TA EQ 3 THEN
         DISPLAY "   AHORROS A TERMINO" 
                 SKIP (0) WITH FRAME F4 NO-LABELS.
      ELSE IF TMes.TA EQ 4 THEN
         DISPLAY "   APORTES"
                 SKIP (0) WITH FRAME F5 NO-LABELS.
      ELSE IF TMes.TA EQ 5 THEN
         DISPLAY "A-TERMINO (Prórrogas y Renovaciones)"
                 SKIP (0) WITH WIDTH 80 FRAME F6 NO-LABELS.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

