&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
DEFI VAR Tfei LIKE   Mov_Contable.Fec_Contable INIT TODAY NO-UNDO. 
DEFI VAR Tfef LIKE   Mov_Contable.Fec_Contable INIT TODAY NO-UNDO.
DEFI VAR tmes AS INTEGER NO-UNDO.
DEFI VAR tano AS INTEGER NO-UNDO.
DEFI VAR Tsalida AS CHAR NO-UNDO.
ASSIGN tmes = MONTH(Tfei).
ASSIGN tano = YEAR(Tfei).
ASSIGN Tsalida = "c:\aldosmes.txt".



/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

    {incluido/Variable.i "SHARED"}.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-208 Btn_Done FILL-IN-1 FILL-IN-2 ~
FILL-IN-4 BUTTON-1 BUTTON-11 BUTTON-2 
&Scoped-Define DISPLAYED-OBJECTS FILL-IN-1 FILL-IN-2 FILL-IN-4 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 6 BY 1.5 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-1 
     LABEL "Ejecutar Informe Movto Vs Saldos" 
     SIZE 41 BY 1.12
     BGCOLOR 17 .

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U NO-FOCUS
     LABEL "Button 11" 
     SIZE 5 BY 1.08.

DEFINE BUTTON BUTTON-2 
     LABEL "Documentos Descuadrados" 
     SIZE 42 BY 1.12.

DEFINE BUTTON BUTTON-208 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 208" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE FILL-IN-1 AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicio" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 TOOLTIP "Ingrese Fecha de Inicio" NO-UNDO.

DEFINE VARIABLE FILL-IN-2 AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 TOOLTIP "Fecha Final" NO-UNDO.

DEFINE VARIABLE FILL-IN-4 AS CHARACTER FORMAT "X(256)":U INITIAL "c:~\info_juriscoop~\veri_saldos.txt" 
     LABEL "Archivo de Salida" 
     VIEW-AS FILL-IN 
     SIZE 85.72 BY 1 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-208 AT ROW 1.27 COL 106 WIDGET-ID 16
     Btn_Done AT ROW 2.88 COL 109 WIDGET-ID 20
     FILL-IN-1 AT ROW 3.69 COL 16 COLON-ALIGNED WIDGET-ID 2
     FILL-IN-2 AT ROW 4.92 COL 16.14 COLON-ALIGNED WIDGET-ID 6
     FILL-IN-4 AT ROW 6.27 COL 16.29 COLON-ALIGNED WIDGET-ID 10
     BUTTON-1 AT ROW 12.81 COL 4 WIDGET-ID 4
     BUTTON-11 AT ROW 4.77 COL 109 WIDGET-ID 18
     BUTTON-2 AT ROW 12.85 COL 53 WIDGET-ID 12
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 116 BY 12.96
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta de Movimientos(Diarios)  Vs Saldos (Mes)"
         HEIGHT             = 12.96
         WIDTH              = 116
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
ASSIGN 
       FRAME fMain:PRIVATE-DATA     = 
                "00) Identificación Cliente".

ASSIGN 
       FILL-IN-1:AUTO-RESIZE IN FRAME fMain      = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de Movimientos(Diarios)  Vs Saldos (Mes) */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de Movimientos(Diarios)  Vs Saldos (Mes) */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done wWin
ON CHOOSE OF Btn_Done IN FRAME fMain
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME fMain /* Ejecutar Informe Movto Vs Saldos */
DO:
/* Compara saldos de movto vs saldos cuentas */

SESSION:SET-WAIT("General").


DEFI VAR Tdb  LIKE Mov_Contable.Db NO-UNDO.
DEFI VAR Tcr  LIKE Mov_Contable.Cr NO-UNDO.
DEFI VAR Tcta LIKE cuentas.cuenta.
DEFI VAR tsde LIKE Sal_Cuenta.Db[1].
DEFI VAR tscr LIKE Sal_Cuenta.Cr[1].
DEFI VAR tfei AS DATE.
DEFI VAR tfef AS DATE.
DEFI VAR tmes AS INTEGER.
DEFI VAR tano AS INTEGER.
DEFI VAR salida AS CHAR.
DEFI VAR ruta   AS CHAR INIT "c:\info_juriscoop\".
/* Define Variables Totales */
DEFI VAR Tgdbm  LIKE Mov_Contable.Db.
DEFI VAR Tgcrm  LIKE Mov_Contable.Cr.
DEFI VAR Tgdbs  LIKE Mov_Contable.Db.
DEFI VAR Tgcrs  LIKE Mov_Contable.Cr.
DEFI VAR Tgdbd  LIKE Mov_Contable.Db.
DEFI VAR Tgcrd  LIKE Mov_Contable.Cr.

/*--------------------------------------*/


/* ASSIGN salida = "c:\tmp\salida\enero2008.txt.".  */
ASSIGN tfei = DATE (FILL-IN-1:SCREEN-VALUE).
ASSIGN tfef = DATE (FILL-IN-2:SCREEN-VALUE).
ASSIGN tmes = MONTH(tfei).
ASSIGN tano = YEAR(tfei).
OUTPUT TO VALUE(FILL-IN-4:SCREEN-VALUE).
EXPORT DELIMITER ";"  
"CUENTA"   
"DB_MOVTO"  
"CR_MOVTO"  
"DB_SALDOS"
"CR_SAlDOS" 
"DBM-DBS"
"CRM-CRS" .
/* lucho */

FOR EACH Mov_contable fields(fec_contab cuenta db cr ) NO-LOCK
    WHERE 
        Mov_contable.fec_contab GE tfei
    AND Mov_contable.fec_contab LE tfef 
    AND Mov_contable.cuenta NE " " 
    BREAK 
        BY Mov_contable.cuenta :
    ASSIGN Tdb = Tdb + Mov_Contable.Db 
           Tcr = Tcr +  Mov_Contable.Cr
           Tcta = Mov_contable.cuenta.
    IF LAST-OF(Mov_contable.cuenta) THEN DO:
       FOR FIRST Sal_Cuenta fields(db cr cuenta) WHERE  Sal_Cuenta.Ano = tano 
                AND Sal_Cuenta.cuenta = Tcta NO-LOCK :
                 ASSIGN tsde = tsde + Sal_Cuenta.Db[tmes]
                        tscr = tscr + Sal_Cuenta.Cr[tmes].
       END. 
       EXPORT DELIMITER ";"  
       Tcta 
       tdb  
       tcr  
       tsde
       tscr
       (tdb - tsde)
       (tcr - tscr).
       
       ASSIGN Tgdbm  = Tgdbm + Tdb.
       ASSIGN Tgcrm  = Tgcrm + tcr.
       ASSIGN Tgdbs  = Tgdbs + tsde.
       ASSIGN Tgcrs  = Tgcrs + tscr.
       ASSIGN Tgdbd  = Tgdbd + (tdb - tsde).
       ASSIGN Tgcrd  = Tgcrd + tcr - tscr.
       Tdb = 0.
       tcr = 0.
       tsde = 0.
       tscr = 0.
    END.
END.
EXPORT DELIMITER ";"  
    "Totales" 
    Tgdbm  
    Tgcrm  
    Tgdbs
    Tgcrs
    Tgdbd
    Tgcrd.

OUTPUT  CLOSE.
SESSION:SET-WAIT("").
MESSAGE "Consulta Finalizada  " 
                " El archivo de Salida queda en  "
          FILL-IN-4:SCREEN-VALUE.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Documentos Descuadrados */
DO:
/*     DEBUGGER:INITIATE().  */
/*     DEBUGGER:SET-BREAK(). */
    SESSION:SET-WAIT("General").
    ASSIGN tfei = DATE (FILL-IN-1:SCREEN-VALUE).
    ASSIGN tfef = DATE (FILL-IN-2:SCREEN-VALUE).
    ASSIGN tmes = MONTH(tfei).
    ASSIGN tano = YEAR(tfei).
    OUTPUT TO VALUE(FILL-IN-4:SCREEN-VALUE).
    EXPORT DELIMITER ";"  
    "AGENCIA"   
    "FECHA"  
    "COMPROBANTE"  
    "DOCUMENTO"
    "DIFERENCIA". 

    DEFI VAR TCpte LIKE Mov_Contable.Db.
    FOR EACH Mov_contable WHERE fec_contab GE tfei
                            AND fec_contab LE tfef 
                            AND Mov_contable.cuenta NE " " NO-LOCK
             BREAK BY Agencia BY comprob BY num_docum:
        ASSIGN Tcpte = Tcpte + (Mov_Contable.Db - Mov_Contable.Cr).

        IF LAST-OF(num_docum) THEN DO:
           IF Tcpte NE 0 THEN
              EXPORT DELIMITER ";"
              Agencia  
              Fec_contab 
              Comproban  
              Num_docum 
              tcpte.
           Tcpte = 0.
        END.
    END.
    OUTPUT  CLOSE.
    SESSION:SET-WAIT("").
    MESSAGE "Consulta Finalizada  " 
                    " El archivo de Salida queda en  "
              FILL-IN-4:SCREEN-VALUE.

  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-208
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-208 wWin
ON CHOOSE OF BUTTON-208 IN FRAME fMain /* Button 208 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY FILL-IN-1 FILL-IN-2 FILL-IN-4 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE BUTTON-208 Btn_Done FILL-IN-1 FILL-IN-2 FILL-IN-4 BUTTON-1 BUTTON-11 
         BUTTON-2 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        FILL-IN-1:SCREEN-VALUE = STRING(TODAY).
        FILL-IN-2:SCREEN-VALUE = STRING(TODAY).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

