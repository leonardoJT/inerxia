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

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}


{incluido\igetfecha.i}
{incluido\Variable.i "SHARED"}

DEFINE VARIABLE vcFiltro AS CHARACTER INITIAL "" NO-UNDO.
DEFINE VARIABLE vlEstado AS CHARACTER INITIAL "?" NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-12 RECT-13 RECT-8 BUTTON-143 FFecIni ~
FFecFin wop_inf Btn_Imp FInsIni FInsFin Btn_Excel FAgeIni FAgeFin ~
BUTTON-exitF FUsuIni FUsuFin REstado Fdet 
&Scoped-Define DISPLAYED-OBJECTS FFecIni FFecFin wop_inf FInsIni FInsFin ~
FAgeIni FAgeFin FUsuIni FUsuFin REstado Fdet 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dmov_inssipla AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-titulo  NO-FOCUS
     LABEL "" 
     SIZE 42 BY 1.15.

DEFINE BUTTON Btn_Excel 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Excel" 
     SIZE 6 BY 1.5
     FONT 8.

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 6 BY 1.5
     FONT 8.

DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 6 BY 1.5.

DEFINE BUTTON BUTTON-exitF 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 163" 
     SIZE 6 BY 1.5.

DEFINE VARIABLE FAgeFin AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "A" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FAgeIni AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fdet AS LOGICAL FORMAT "Detalle/Resumen":U INITIAL NO 
     LABEL "Det./Res." 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FFecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FFecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fec. Trans." 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FInsFin AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "A" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FInsIni AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Instancia" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FUsuFin AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "A" 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FUsuIni AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Usu. Gest." 
     VIEW-AS FILL-IN 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE REstado AS CHARACTER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Activo", "TRUE",
"Inactivo", "FALSE",
"Todos", "?"
     SIZE 16 BY 1.88 TOOLTIP "Seleccione el informe" NO-UNDO.

DEFINE VARIABLE wop_inf AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Gestión SIPLA", 1,
"Clientes Con Actualiz. Datos", 2,
"Lista de Usuarios SIPLA", 3
     SIZE 29 BY 6.46 TOOLTIP "Seleccione el informe" NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 59 BY 10.5
     BGCOLOR 0 FGCOLOR 15 .

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 1.62.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 32 BY 10.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BTN-titulo AT ROW 1.5 COL 33 WIDGET-ID 4
     BUTTON-143 AT ROW 4.5 COL 97 WIDGET-ID 68
     FFecIni AT ROW 5.85 COL 46 COLON-ALIGNED WIDGET-ID 28
     FFecFin AT ROW 5.88 COL 73 COLON-ALIGNED WIDGET-ID 30
     wop_inf AT ROW 6.38 COL 3 HELP
          "Seleccione el informe" NO-LABEL WIDGET-ID 16
     Btn_Imp AT ROW 6.38 COL 97 WIDGET-ID 2
     FInsIni AT ROW 7.19 COL 46 COLON-ALIGNED WIDGET-ID 38
     FInsFin AT ROW 7.19 COL 73 COLON-ALIGNED WIDGET-ID 36
     Btn_Excel AT ROW 8 COL 97 WIDGET-ID 56
     FAgeIni AT ROW 8.54 COL 46 COLON-ALIGNED WIDGET-ID 42
     FAgeFin AT ROW 8.54 COL 73 COLON-ALIGNED WIDGET-ID 40
     BUTTON-exitF AT ROW 9.62 COL 97 WIDGET-ID 50
     FUsuIni AT ROW 9.88 COL 46 COLON-ALIGNED WIDGET-ID 46
     FUsuFin AT ROW 9.88 COL 73 COLON-ALIGNED WIDGET-ID 44
     REstado AT ROW 11.27 COL 48 HELP
          "Seleccione el informe" NO-LABEL WIDGET-ID 62
     Fdet AT ROW 13.38 COL 46 COLON-ALIGNED HELP
          "Enviar Detalle o Resumen" WIDGET-ID 26
     "Estado:" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 11.23 COL 40.43 WIDGET-ID 66
     "FILTROS" VIEW-AS TEXT
          SIZE 8.43 BY .62 AT ROW 3.69 COL 60.29 WIDGET-ID 24
          FGCOLOR 0 FONT 7
     "REPORTES DE SIPLA" VIEW-AS TEXT
          SIZE 19.86 BY .62 AT ROW 1.77 COL 44.14 WIDGET-ID 14
          BGCOLOR 11 FGCOLOR 0 FONT 7
     "REPORTES" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 3.69 COL 12.57 WIDGET-ID 12
          FGCOLOR 0 FONT 7
     RECT-12 AT ROW 4.5 COL 35 WIDGET-ID 6
     RECT-13 AT ROW 1.27 COL 32 WIDGET-ID 8
     RECT-8 AT ROW 4.5 COL 2 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 104.57 BY 16.54 WIDGET-ID 100.


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
         TITLE              = "SFG - Informes SIPLA"
         HEIGHT             = 16.54
         WIDTH              = 104.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
/* SETTINGS FOR BUTTON BTN-titulo IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Informes SIPLA */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Informes SIPLA */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Excel wWin
ON CHOOSE OF Btn_Excel IN FRAME fMain /* Excel */
DO:
    DEFINE VARIABLE viCnt AS INTEGER INITIAL 0 NO-UNDO.

    ASSIGN vcFiltro = "((Fecha_Transaccion >= " + FFecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " AND " +
                      "Fecha_Transaccion <= " + FFecFin:SCREEN-VALUE + ") OR (" + FFecIni:SCREEN-VALUE + " EQ ? AND " + FFecFin:SCREEN-VALUE + " EQ ?)) AND " + 
                      "((Agencia >= " + FAgeIni:SCREEN-VALUE + " AND " + 
                      "Agencia <= " + FAgeFin:SCREEN-VALUE + ") OR (" + FAgeIni:SCREEN-VALUE + " = 0 AND " + FAgeFin:SCREEN-VALUE + " = 0)) AND " +
                      "((Instancia >= " + FInsIni:SCREEN-VALUE + " AND " + 
                      "Instancia <= " + FInsFin:SCREEN-VALUE + ") OR (" + FInsIni:SCREEN-VALUE + " = 0 AND " + FInsFin:SCREEN-VALUE + " = 0)) AND " +
                      "((usuGestiona >= '" + FUsuIni:SCREEN-VALUE + "' AND " + 
                      "usuGestiona <= '" + FUsuFin:SCREEN-VALUE + "') OR (" + FUsuIni:SCREEN-VALUE + " = 0 AND " + FUsuFin:SCREEN-VALUE + " = 0)) AND " +
                      "(estado = LOGICAL(" + REstado:SCREEN-VALUE + ") OR LOGICAL(" + REstado:SCREEN-VALUE + ") = ?) AND " +
                      " (TRUE)".

    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dmov_inssipla,
     INPUT vcFiltro /* CHARACTER */).
    DYNAMIC-FUNCTION('openQuery':U IN h_dmov_inssipla).

    RUN fetchLast IN h_dmov_inssipla.
    ASSIGN viCnt = DYNAMIC-FUNCTION('getLastRowNum':U IN h_dmov_inssipla).

/*     RUN exportData IN h_dmov_inssipla */
/*     ( INPUT "Excel" /* CHARACTER */,  */
/*       INPUT " " /* CHARACTER */,      */
/*       INPUT YES /* LOGICAL */,        */
/*       INPUT YES /* LOGICAL */,        */
/*       INPUT viCnt /* INTEGER */).     */

    MESSAGE "Se va a generar archivo Excel con " SKIP
        viCnt " registros." SKIP
        "Esto puede tardar unos segundos." SKIP
        "Desea continuar?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO 
        TITLE "Generar a Excel" UPDATE vlgenerar AS LOGICAL.

    IF vlgenerar THEN 
        RUN transferToExcel IN h_dmov_inssipla
        ( INPUT " " /* CHARACTER */,
          INPUT YES /* LOGICAL */,
          INPUT YES /* LOGICAL */,
          INPUT viCnt /* INTEGER */).
    ELSE
        RETURN.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp wWin
ON CHOOSE OF Btn_Imp IN FRAME fMain /* Button 149 */
DO:
    CASE wop_inf:
        WHEN 1 THEN
            RUN wimprime.w ("prMov_InsSipla.p", "Gestión SIPLA",
                           IF FInsIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FInsIni:SCREEN-VALUE,
                           IF FInsFin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FInsFin:SCREEN-VALUE,
                           IF FAgeIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FAgeIni:SCREEN-VALUE,
                           IF FAgeFin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FAgeFin:SCREEN-VALUE,
                           FUsuIni:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                           FUsuFin:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                           REstado:SCREEN-VALUE IN FRAME {&FRAME-NAME}, 
                           "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           DATE(FFecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                           DATE(FFecFin:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                           ?, ?,
                           ?, ?,
                           ?, ?,
                           ?, ?,
                           ?, ?).
        WHEN 2 THEN
            RUN wimprime.w ("prClientesActualizacion.p", "Actualización Datos",
                           IF FAgeIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FAgeIni:SCREEN-VALUE,
                           IF FAgeFin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FAgeFin:SCREEN-VALUE,
                           IF REstado:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "TRUE" THEN "1" ELSE
                               IF REstado:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "FALSE" THEN "2"
                                   ELSE "0",
                           "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           DATE(FFecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                           DATE(FFecFin:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                           ?, ?,
                           ?, ?,
                           ?, ?,
                           ?, ?,
                           ?, ?).
        WHEN 3 THEN
            RUN wimprime.w ("prSiplaListaUsuarios.p", "Lista USuarios SIPLA",
                           IF FInsIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FInsIni:SCREEN-VALUE,
                           IF FInsFin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FInsFin:SCREEN-VALUE,
                           IF FAgeIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FAgeIni:SCREEN-VALUE,
                           IF FAgeFin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FAgeFin:SCREEN-VALUE,
                           FUsuIni:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                           FUsuFin:SCREEN-VALUE IN FRAME {&FRAME-NAME},
                           IF REstado:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "TRUE" THEN "1" ELSE (IF REstado:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "FALSE" THEN "2" ELSE "0"), 
                           "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           ?, ?,  /*Date*/
                           ?, ?,  /*Date*/
                           ?, ?,  /*Date*/
                           ?, ?,  /*Date*/
                           ?, ?,  /*Logical*/
                           ?, ?). /*Logical*/
        OTHERWISE DO:
               MESSAGE "Debe seleccionar un reporte y" SKIP
                       "definir los parámetros de filtro...".
               APPLY "CLOSE":U TO THIS-PROCEDURE.
        END.
    END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-143
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-143 wWin
ON CHOOSE OF BUTTON-143 IN FRAME fMain /* Button 143 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-exitF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-exitF wWin
ON CHOOSE OF BUTTON-exitF IN FRAME fMain /* Button 163 */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FFecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FFecFin wWin
ON LEAVE OF FFecFin IN FRAME fMain /* Hasta */
DO:
/*     IF DATE(SELF:SCREEN-VALUE) < DATE(FFecIni:SCREEN-VALUE) */
/*         OR SELF:SCREEN-VALUE EQ "" THEN DO:                 */
    IF FFecIni:SCREEN-VALUE NE "" AND (DATE(SELF:SCREEN-VALUE) < DATE(FFecIni:SCREEN-VALUE))
        THEN DO:
            MESSAGE "La fecha final no es válida"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FFecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FFecIni wWin
ON LEAVE OF FFecIni IN FRAME fMain /* Fec. Trans. */
DO:
/*     IF SELF:SCREEN-VALUE = "" THEN DO:         */
/*         MESSAGE "Debe colocar fecha Inicial"   */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*         RETURN NO-APPLY.                       */
/*     END.                                       */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME REstado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL REstado wWin
ON VALUE-CHANGED OF REstado IN FRAME fMain
DO:
    ASSIGN REstado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wop_inf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wop_inf wWin
ON VALUE-CHANGED OF wop_inf IN FRAME fMain
DO:
    ASSIGN wop_inf.
    CASE wop_inf:
        WHEN 1 THEN DO:
            ASSIGN 
                FFecIni:HIDDEN      = FALSE
                FFecFin:HIDDEN      = FALSE
                FInsIni:HIDDEN      = FALSE
                FInsFin:HIDDEN      = FALSE
                FUsuIni:HIDDEN      = FALSE
                FUsuFin:HIDDEN      = FALSE
                FFecIni:LABEL       = "Fec. Trans."
/*                 REstado:HIDDEN      = FALSE */
                Fdet:HIDDEN         = FALSE.
        END.
        WHEN 2 THEN DO:
            ASSIGN                 
                FFecIni:HIDDEN      = FALSE
                FFecFin:HIDDEN      = FALSE
                FInsIni:HIDDEN      = TRUE
                FInsFin:HIDDEN      = TRUE
                FUsuIni:HIDDEN      = TRUE
                FUsuFin:HIDDEN      = TRUE
                FFecIni:LABEL       = "Fec. Act."
/*                 REstado:HIDDEN      = TRUE */
                Fdet:HIDDEN         = TRUE.
        END.
        WHEN 3 THEN DO:
            ASSIGN 
                FFecIni:HIDDEN      = TRUE
                FFecFin:HIDDEN      = TRUE
                Fdet:HIDDEN         = TRUE.
/*                 REstado:HIDDEN      = FALSE. */
        END.
    END CASE.
    ASSIGN wop_inf.
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
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dmov_inssipla.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedmov_inssiplaOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dmov_inssipla ).
       RUN repositionObject IN h_dmov_inssipla ( 1.54 , 14.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       /* Adjust the tab order of the smart objects. */
    END. /* Page 0 */

  END CASE.

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
  DISPLAY FFecIni FFecFin wop_inf FInsIni FInsFin FAgeIni FAgeFin FUsuIni 
          FUsuFin REstado Fdet 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-12 RECT-13 RECT-8 BUTTON-143 FFecIni FFecFin wop_inf Btn_Imp 
         FInsIni FInsFin Btn_Excel FAgeIni FAgeFin BUTTON-exitF FUsuIni FUsuFin 
         REstado Fdet 
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



  ASSIGN FFecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(dia_ini_mes(TODAY))
      FFecFin:SCREEN-VALUE = STRING(dia_fin_mes(TODAY))
      REstado:SCREEN-VALUE = "?".


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

