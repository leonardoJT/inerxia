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
DEFINE VARIABLE viAgeIni AS INTEGER INITIAL 0 NO-UNDO.
DEFINE VARIABLE viAgeFin AS INTEGER INITIAL 0 NO-UNDO.

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
&Scoped-Define ENABLED-OBJECTS RECT-12 RECT-13 RECT-8 BUTTON-143 ~
RS-Opciones Btn_Imp FAgeIni FAgeFin Btn_Excel FFecIni FFecFin BUTTON-exitF ~
FCptIni FCptFin FCtaIni FCtaFin 
&Scoped-Define DISPLAYED-OBJECTS RS-Opciones FAgeIni FAgeFin FFecIni ~
FFecFin FCptIni FCptFin FCtaIni FCtaFin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dmov_contable AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-titulo  NO-FOCUS
     LABEL "" 
     SIZE 42 BY 1.15.

DEFINE BUTTON Btn_Excel 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Excel" 
     SIZE 10 BY 1.58
     FONT 8.

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 10 BY 1.35
     FONT 8.

DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-exitF 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 163" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE FAgeFin AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "A" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FAgeIni AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 7 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FCptFin AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "A" 
     VIEW-AS FILL-IN 
     SIZE 7.14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FCptIni AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Comprobante" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 7.72 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FCtaFin AS CHARACTER FORMAT "X(12)":U 
     LABEL "Cta. Final" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FCtaIni AS CHARACTER FORMAT "X(12)":U 
     LABEL "Cta. Inicial" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FFecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE FFecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN NATIVE 
     SIZE 16 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RS-Opciones AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Comprobantes Descuadrados", 1,
"Consistencia", 2
     SIZE 30 BY 4.85 TOOLTIP "Seleccione el informe" NO-UNDO.

DEFINE RECTANGLE RECT-12
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 10.5
     BGCOLOR 0 FGCOLOR 15 .

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 44 BY 1.62.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 10.5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BTN-titulo AT ROW 1.5 COL 33 WIDGET-ID 4
     BUTTON-143 AT ROW 4.5 COL 97 WIDGET-ID 68
     RS-Opciones AT ROW 5.31 COL 2 HELP
          "Seleccione el informe" NO-LABEL WIDGET-ID 16
     Btn_Imp AT ROW 6.12 COL 97 WIDGET-ID 2
     FAgeIni AT ROW 6.46 COL 46.29 COLON-ALIGNED WIDGET-ID 42
     FAgeFin AT ROW 6.46 COL 74.86 COLON-ALIGNED WIDGET-ID 40
     Btn_Excel AT ROW 7.5 COL 97 WIDGET-ID 56
     FFecIni AT ROW 7.81 COL 46.29 COLON-ALIGNED WIDGET-ID 28
     FFecFin AT ROW 7.85 COL 74.86 COLON-ALIGNED WIDGET-ID 30
     BUTTON-exitF AT ROW 9.12 COL 97 WIDGET-ID 50
     FCptIni AT ROW 9.15 COL 46.29 COLON-ALIGNED WIDGET-ID 38
     FCptFin AT ROW 9.15 COL 74.86 COLON-ALIGNED WIDGET-ID 36
     FCtaIni AT ROW 10.69 COL 46 COLON-ALIGNED WIDGET-ID 70
     FCtaFin AT ROW 10.69 COL 75 COLON-ALIGNED WIDGET-ID 74
     "REPORTES" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 3.69 COL 13 WIDGET-ID 12
          FGCOLOR 0 FONT 7
     "REPORTES DE CONTABILIDAD" VIEW-AS TEXT
          SIZE 29.86 BY .62 AT ROW 1.77 COL 39 WIDGET-ID 14
          BGCOLOR 11 FGCOLOR 0 FONT 7
     "FILTROS" VIEW-AS TEXT
          SIZE 8.43 BY .62 AT ROW 3.69 COL 59 WIDGET-ID 24
          FGCOLOR 0 FONT 7
     RECT-12 AT ROW 4.5 COL 34 WIDGET-ID 6
     RECT-13 AT ROW 1.27 COL 32 WIDGET-ID 8
     RECT-8 AT ROW 4.5 COL 1 WIDGET-ID 10
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.14 ROW 1
         SIZE 107 BY 16.54
         BGCOLOR 17  WIDGET-ID 100.


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
         TITLE              = "SFG - Reportes de Contabilidad"
         HEIGHT             = 16.54
         WIDTH              = 107
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
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
ON END-ERROR OF wWin /* SFG - Reportes de Contabilidad */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Reportes de Contabilidad */
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
    RUN ValidaFiltros.
    IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "Verifique los Filtros de Nuevo..."
          VIEW-AS ALERT-BOX ERROR.
       RETURN.
    END.
    /***Nuevo****/
/*     IF FAgeIni:SCREEN-VALUE EQ "0" AND FAgeFin:SCREEN-VALUE EQ "0" THEN       */
/*        ASSIGN viAgeIni = 0                                                    */
/*               viAgeFin = 99.                                                  */
/*     ELSE                                                                      */
/*        IF INTEGER(FAgeIni:SCREEN-VALUE) LE INTEGER(FAgeFin:SCREEN-VALUE) THEN */
/*           ASSIGN viAgeIni = INTEGER(FAgeIni:SCREEN-VALUE)                     */
/*                  viAgeFin = INTEGER(FAgeFin:SCREEN-VALUE).                    */
/*        ELSE DO:                                                               */
/*            MESSAGE "Agencia Inicial Deber ser Menor a la Agencia Final.."     */
/*                VIEW-AS ALERT-BOX.                                             */
/*            RETURN.                                                            */
/*        END.                                                                   */
    /****************/
    ASSIGN vcFiltro = "((Comprobante >= " + FCptIni:SCREEN-VALUE + " AND " + "Comprobante <= " + FCptFin:SCREEN-VALUE + ") OR
                        (" + FCptIni:SCREEN-VALUE + " = 0 AND " + FCptFin:SCREEN-VALUE + " = 0)) AND " +
                      "((Fec_Contable >= " + FFecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} + " AND " + "Fec_Contable <= " + FFecFin:SCREEN-VALUE + ") OR
                        (" + FFecIni:SCREEN-VALUE + " EQ ? AND " + FFecFin:SCREEN-VALUE + " EQ ?)) AND " + 
                      "(Agencia >= " + STRING(viAgeIni) + " AND " + "Agencia <= " + STRING(viAgeFin) + ")".

/*                          OR (" + STRING(viAgeIni) + " = 0 AND " + STRING(viAgeFin) + " = 99))". */

/*                       "((Agencia >= " + FAgeIni:SCREEN-VALUE + " AND " +                                                                      */
/*                       "Agencia <= " + FAgeFin:SCREEN-VALUE + ") OR (" + FAgeIni:SCREEN-VALUE + " = 0 AND " + FAgeFin:SCREEN-VALUE + " = 0))". */

    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dMov_Contable,
    INPUT vcFiltro /* CHARACTER */).
    MESSAGE vcFiltro
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    DYNAMIC-FUNCTION('openQuery':U IN h_dMov_Contable).

    RUN fetchLast IN h_dMov_Contable.
    ASSIGN viCnt = DYNAMIC-FUNCTION('getLastRowNum':U IN h_dMov_Contable).

    MESSAGE "Se va a generar archivo Excel con " SKIP
        viCnt " registros." SKIP
        "Esto puede tardar unos segundos." SKIP
        "Desea continuar?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO 
        TITLE "Generar a Excel" UPDATE vlgenerar AS LOGICAL.

    IF vlgenerar THEN 
        RUN transferToExcel IN h_dMov_Contable
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
    CASE RS-Opciones:
        WHEN 1 THEN DO:
            RUN ValidaFiltros. /* NO-ERROR.*/
            IF ERROR-STATUS:ERROR THEN DO:
               MESSAGE "Verifique los Filtros de Nuevo..."
                  VIEW-AS ALERT-BOX.
               RETURN.
            END.
            RUN wimprime.w ("prCptesDescuadrados.p", "Comprobantes CONTABLES",
                           IF FCptIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FCptIni:SCREEN-VALUE, /* 20 Campos Char o Numéricos*/
                           IF FCptFin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FCptFin:SCREEN-VALUE,
                           TRIM(STRING(viAgeIni)),
                           TRIM(STRING(viAgeFin)),
/*                            IF FAgeIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FAgeIni:SCREEN-VALUE, */
/*                            IF FAgeFin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FAgeFin:SCREEN-VALUE, */
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           "", "",
                           DATE(FFecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}), /* 8 Campos Fechas - 4 Fechas*/
                           DATE(FFecFin:SCREEN-VALUE IN FRAME {&FRAME-NAME}),
                           ?, ?,
                           ?, ?,
                           ?, ?,
                           ?, ?,  /* 4 Campos Lógicos*/
                           ?, ?).
        END.
        WHEN 2 THEN
            "".
/*             RUN wimprime.w ("prClientesActualizacion.p", "Actualización Datos",                                                                                                   */
/*                            IF FAgeIni:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FAgeIni:SCREEN-VALUE,                                                                */
/*                            IF FAgeFin:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "" THEN "0" ELSE FAgeFin:SCREEN-VALUE,                                                                */
/*                            IF REstado:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "TRUE" THEN "1" ELSE (IF REstado:SCREEN-VALUE IN FRAME {&FRAME-NAME} EQ "FALSE" THEN "2" ELSE "0"),  */
/*                            "",                                                                                                                                                    */
/*                            "", "",                                                                                                                                                */
/*                            "", "",                                                                                                                                                */
/*                            "", "",                                                                                                                                                */
/*                            "", "",                                                                                                                                                */
/*                            DATE(FFecIni:SCREEN-VALUE IN FRAME {&FRAME-NAME}),                                                                                                     */
/*                            DATE(FFecFin:SCREEN-VALUE IN FRAME {&FRAME-NAME}),                                                                                                     */
/*                            ?, ?,                                                                                                                                                  */
/*                            ?, ?,                                                                                                                                                  */
/*                            ?, ?).                                                                                                                                                 */
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


&Scoped-define SELF-NAME FAgeFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FAgeFin wWin
ON LEAVE OF FAgeFin IN FRAME fMain /* A */
DO:
/*  ASSIGN FAgeIni                                                                      */
/*         FAgeFin.                                                                     */
/*  MESSAGE "Ini: " FAgeIni SKIP                                                        */
/*          "Fin: " FAgeFin                                                             */
/*      VIEW-AS ALERT-BOX INFO BUTTONS OK.                                              */
/*  IF (FAgeIni:SCREEN-VALUE NE "" AND (FAgeIni:SCREEN-VALUE GT FAgeFin:SCREEN-VALUE))  */
/*      THEN DO:                                                                        */
/*          MESSAGE "La Agencia Inicial Debe ser Menor ó Igual a la Agencia Final"      */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                                      */
/*          RETURN NO-APPLY.                                                            */
/*      END.                                                                            */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FCptFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FCptFin wWin
ON LEAVE OF FCptFin IN FRAME fMain /* A */
DO:
/*  ASSIGN FCptIni.                                                                     */
/*  IF (FCptIni:SCREEN-VALUE NE "" AND (FCptIni:SCREEN-VALUE GT FCptFin:SCREEN-VALUE))  */
/*      THEN DO:                                                                        */
/*          MESSAGE "El Cpte.Inicial Debe ser Menor ó Igual al Cpte.Final"              */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                                      */
/*          RETURN NO-APPLY.                                                            */
/*  END.                                                                                */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FFecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FFecFin wWin
ON LEAVE OF FFecFin IN FRAME fMain /* Fecha Final */
DO:
 IF FFecIni:SCREEN-VALUE NE "" AND (DATE(SELF:SCREEN-VALUE) LT DATE(FFecIni:SCREEN-VALUE))
     THEN DO:
         MESSAGE "La Fecha Inicial Debe ser Menor ó Igual a la Fecha Final ó Errada."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN NO-APPLY.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FFecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FFecIni wWin
ON LEAVE OF FFecIni IN FRAME fMain /* Fecha Inicial */
DO:
/*     IF SELF:SCREEN-VALUE = "" THEN DO:         */
/*         MESSAGE "Debe colocar fecha Inicial"   */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK. */
/*         RETURN NO-APPLY.                       */
/*     END.                                       */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-Opciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-Opciones wWin
ON VALUE-CHANGED OF RS-Opciones IN FRAME fMain
DO:
    ASSIGN RS-Opciones.
    CASE RS-Opciones:
        WHEN 1 THEN DO:
            ASSIGN 
                FCptIni:HIDDEN      = FALSE
                FCptFin:HIDDEN      = FALSE
                FCtaIni:HIDDEN      = TRUE
                FCtaFin:HIDDEN      = TRUE
                FFecIni:LABEL       = "Fec. Trans.".
        END.
        WHEN 2 THEN DO:
            ASSIGN 
                FCptIni:HIDDEN      = TRUE
                FCptFin:HIDDEN      = TRUE
                FCtaIni:HIDDEN      = FALSE
                FCtaFin:HIDDEN      = FALSE
                FFecIni:LABEL       = "Fec. Act.".
        END.
    END CASE.
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
             INPUT  'dmov_contable.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedmov_contableOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dmov_contable ).
       RUN repositionObject IN h_dmov_contable ( 1.54 , 22.00 ) NO-ERROR.
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
  DISPLAY RS-Opciones FAgeIni FAgeFin FFecIni FFecFin FCptIni FCptFin FCtaIni 
          FCtaFin 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-12 RECT-13 RECT-8 BUTTON-143 RS-Opciones Btn_Imp FAgeIni FAgeFin 
         Btn_Excel FFecIni FFecFin BUTTON-exitF FCptIni FCptFin FCtaIni FCtaFin 
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
         FFecFin:SCREEN-VALUE = STRING(dia_fin_mes(TODAY)).
  APPLY "VALUE-CHANGED":U TO RS-Opciones.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidaFiltros wWin 
PROCEDURE ValidaFiltros :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/* Agencias*/
ASSIGN viAgeIni = 0  
       viAgeFin = 99.

IF INTEGER(FAgeIni:SCREEN-VALUE IN FRAME {&frame-name}) LE 
   INTEGER(FAgeFin:SCREEN-VALUE IN FRAME {&frame-name}) THEN
   ASSIGN viAgeIni = INTEGER(FAgeIni:SCREEN-VALUE)
          viAgeFin = INTEGER(FAgeFin:SCREEN-VALUE).
ELSE DO:
    MESSAGE "La Agencia Inicial Deber ser Menor ó Igual a la Agencia Final.."
        VIEW-AS ALERT-BOX.
    RETURN ERROR.
END.
/* Fechas*/
IF FFecIni:SCREEN-VALUE NE "" AND FFecIni:SCREEN-VALUE NE "" THEN 
   IF DATE(FFecIni:SCREEN-VALUE) GT DATE(FFecFin:SCREEN-VALUE) THEN DO:
      MESSAGE "La Fecha Inicial Deber ser Memor ó Igual a la Fecha Final.."
          VIEW-AS ALERT-BOX.
      RETURN ERROR.
   END.
/* Comprobantes*/
IF FCptIni:SCREEN-VALUE NE "" AND FCptFin:SCREEN-VALUE NE "" THEN
   IF INTEGER(FCptIni:SCREEN-VALUE) GT INTEGER(FCptFin:SCREEN-VALUE) THEN DO:
      MESSAGE "El Comprobante Inicial Deber ser Menor ó Igual al Comprobante Final.."
          VIEW-AS ALERT-BOX.
      RETURN ERROR.
   END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

