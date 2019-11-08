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
{incluido/Variable.i "SHARED"}.
DEF VAR hSper001 AS HANDLE NO-UNDO.
RUN SuperSuper001.p PERSISTENT SET hSper001.
SESSION:ADD-SUPER-PROCEDURE(hSper001).

DEF TEMP-TABLE tFrmtos NO-UNDO
    FIELD tFCdgo AS INTEGER 
    FIELD tFDscrpcion AS CHAR
    FIELD tFFncion AS CHAR.

CREATE tFrmtos.
ASSIGN  tFrmtos.tFCdgo      = 338
        tFrmtos.tFDscrpcion = "Desagregado de Sectorización de Principales Operaciones"
        tFrmtos.tFFncion = "f338".

/* DEF VAR chExcel AS COM-HANDLE NO-UNDO. */
DEF VAR chExcelApplication  AS COM-HANDLE.
DEF VAR chWorksheet         AS COM-HANDLE.
DEF VAR chWorkbook          AS COM-HANDLE.
DEF VAR chPage              AS COM-HANDLE.
DEF VAR chExcel             AS COM-HANDLE.

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
&Scoped-Define ENABLED-OBJECTS RECT-290 RECT-291 RECT-292 cmbPriodcdad ~
daFchaIncial daFchaFnal daFchaCrte c2Slccion BUTTON-166 Btn_Imp ~
Btn_Ejecutar btnCntnuar cSlcciondos BtnFrmtos BUTTON-14 BUTTON-11 
&Scoped-Define DISPLAYED-OBJECTS cmbPriodcdad daFchaIncial daFchaFnal ~
daFchaCrte c2Slccion cSlcciondos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD f338 wWin 
FUNCTION f338 RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFchaaaaammdd wWin 
FUNCTION fFchaaaaammdd RETURNS INTEGER
  (dafcha AS DATE /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD ftrim wWin 
FUNCTION ftrim RETURNS CHARACTER
  (c AS CHAR)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnCntnuar 
     LABEL "Continuar" 
     SIZE 10 BY 1.62 TOOLTIP "Continuar".

DEFINE BUTTON BtnFrmtos 
     LABEL "Formatos" 
     SIZE 10 BY 1.62 TOOLTIP "Configuración De Formatos".

DEFINE BUTTON Btn_Ejecutar 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Ejecutar" 
     SIZE 10 BY 1.62 TOOLTIP "Envía Repositorio Directamente A Excel".

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Opciones De Impresión".

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-14 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 14" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-166 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 166" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE cmbPriodcdad AS CHARACTER FORMAT "X(256)":U INITIAL "0" 
     LABEL "Periodicidad" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 28.29 BY 1 TOOLTIP "Periodicidad" NO-UNDO.

DEFINE VARIABLE daFchaCrte AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Fecha De Generación Del Informe" NO-UNDO.

DEFINE VARIABLE daFchaFnal AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Fecha Final Lectura De Los Datos" NO-UNDO.

DEFINE VARIABLE daFchaIncial AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 TOOLTIP "Fecha Inicial Lectura De Los Datos" NO-UNDO.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE RECTANGLE RECT-291
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 114 BY 2.96.

DEFINE RECTANGLE RECT-292
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 47 BY 2.15.

DEFINE VARIABLE c2Slccion AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 99 BY 6.19
     FONT 8 NO-UNDO.

DEFINE VARIABLE cSlcciondos AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "","*" 
     SIZE 99 BY 9.96 TOOLTIP "Seleccionados"
     FONT 8 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     cmbPriodcdad AT ROW 2 COL 13 COLON-ALIGNED WIDGET-ID 66
     daFchaIncial AT ROW 2.35 COL 65 COLON-ALIGNED NO-LABEL WIDGET-ID 82
     daFchaFnal AT ROW 2.35 COL 80 COLON-ALIGNED NO-LABEL WIDGET-ID 84
     daFchaCrte AT ROW 2.35 COL 95 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     c2Slccion AT ROW 4.23 COL 2 NO-LABEL WIDGET-ID 68
     BUTTON-166 AT ROW 4.77 COL 104 WIDGET-ID 62
     Btn_Imp AT ROW 6.38 COL 104 WIDGET-ID 54
     Btn_Ejecutar AT ROW 8 COL 104 WIDGET-ID 56
     btnCntnuar AT ROW 10.15 COL 104 WIDGET-ID 76
     cSlcciondos AT ROW 10.69 COL 2 NO-LABEL WIDGET-ID 74
     BtnFrmtos AT ROW 13.38 COL 104 HELP
          "Configuración De Formatos" WIDGET-ID 80
     BUTTON-14 AT ROW 15.27 COL 105 WIDGET-ID 60
     BUTTON-11 AT ROW 17.15 COL 107 WIDGET-ID 58
     "Fechas:" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 1.27 COL 67 WIDGET-ID 88
     "Inicial" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.81 COL 67 WIDGET-ID 90
     "Final" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 1.81 COL 82 WIDGET-ID 92
     "Generación Informe" VIEW-AS TEXT
          SIZE 17 BY .62 AT ROW 1.81 COL 95 WIDGET-ID 94
     RECT-290 AT ROW 4.5 COL 103 WIDGET-ID 64
     RECT-291 AT ROW 1 COL 1 WIDGET-ID 70
     RECT-292 AT ROW 1.38 COL 66 WIDGET-ID 86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 146 BY 26.38
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Formatos Financieros"
         HEIGHT             = 19.77
         WIDTH              = 113.14
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
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Formatos Financieros */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Formatos Financieros */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnCntnuar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnCntnuar wWin
ON CHOOSE OF btnCntnuar IN FRAME fMain /* Continuar */
DO:
    DO WITH FRAME {&FRAME-NAME}:
        ASSIGN dafchaincial dafchafnal dafchacrte.
        IF NOT (dafchaincial <= daFchaFnal AND  dafchaincial <= dafchacrte AND dafchaIncial <> ? AND dafchafnal <> ?) 
        THEN DO:
            MESSAGE "ERROR: Rango De Fechas Incorrecto"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN NO-APPLY.
        END.
    END.
/*     RUN ExcelAbre. */
    RUN pFrmtos.
/*     RUN ExcelCierra. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnFrmtos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnFrmtos wWin
ON CHOOSE OF BtnFrmtos IN FRAME fMain /* Formatos */
DO:
    RUN wsuperformato.w.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 wWin
ON CHOOSE OF BUTTON-14 IN FRAME fMain /* Button 14 */
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


&Scoped-define SELF-NAME BUTTON-166
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-166 wWin
ON CHOOSE OF BUTTON-166 IN FRAME fMain /* Button 166 */
DO:
    RUN W-InfDia.w NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME c2Slccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL c2Slccion wWin
ON VALUE-CHANGED OF c2Slccion IN FRAME fMain
DO:
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        i = LOOKUP(self:screen-value,SELF:LIST-ITEM-PAIRS,",").
        c = ENTRY(i - 1,SELF:LIST-ITEM-PAIRS,",") + "," +
            ENTRY(i,SELF:LIST-ITEM-PAIRS,",").
        i = LOOKUP(self:screen-value,cSlcciondos:LIST-ITEM-PAIRS,",").
        IF i = ? OR i = 0
        THEN DO:
            cSlcciondos:LIST-ITEM-PAIRS = cSlcciondos:LIST-ITEM-PAIRS + "," + c.
        END.
        ELSE DO:
            c = cSlcciondos:LIST-ITEM-PAIRS.
            ENTRY(i,c,",") = "".
            ENTRY(i - 1,c,",") = "".
            c = ftrim(c).
            c = TRIM(c,",").
            cSlcciondos:LIST-ITEM-PAIRS = c.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cmbPriodcdad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cmbPriodcdad wWin
ON VALUE-CHANGED OF cmbPriodcdad IN FRAME fMain /* Periodicidad */
DO:
    DEF VAR i AS INTEGER NO-UNDO.
    i = LOOKUP(SELF:SCREEN-VALUE,cmbPriodcdad:PRIVATE-DATA,CHR(1)).
    IF i = 0 THEN RETURN NO-APPLY.
    c2Slccion:LIST-ITEM-PAIRS = ENTRY(i,c2Slccion:PRIVATE-DATA,CHR(1)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME daFchaFnal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL daFchaFnal wWin
ON LEAVE OF daFchaFnal IN FRAME fMain
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME daFchaIncial
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL daFchaIncial wWin
ON LEAVE OF daFchaIncial IN FRAME fMain
DO:
    ASSIGN {&SELF-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

ON 'leave':U OF daFchaCrte, daFchaFnal, daFchaIncial
DO:
    ASSIGN {&SELF-NAME}.
    RETURN.
END.

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
  DISPLAY cmbPriodcdad daFchaIncial daFchaFnal daFchaCrte c2Slccion cSlcciondos 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-290 RECT-291 RECT-292 cmbPriodcdad daFchaIncial daFchaFnal 
         daFchaCrte c2Slccion BUTTON-166 Btn_Imp Btn_Ejecutar btnCntnuar 
         cSlcciondos BtnFrmtos BUTTON-14 BUTTON-11 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelAbre wWin 
PROCEDURE ExcelAbre :
/*------------------------------------------------------------------------------
  Purpose: ABRE EXCEL
  Parameters:  <none>
  Notes:     InfrmeScringAExcel  
  LOG: CREADO, 3 MAR 2008, ING. Edilberto Mariño Moya
------------------------------------------------------------------------------*/

    DEF INPUT PARAMETER cFrmto AS CHAR NO-UNDO.
    DEF VAR i AS INTEGER NO-UNDO.    
    DEF VAR iSheetNumber        AS INTEGER INITIAL 1.
    DEF VAR cRngo AS char NO-UNDO.
    DEF VAR cRngo1 AS char NO-UNDO.
    DEF VAR cvlor AS CHAR NO-UNDO.
    DEF VAR sw AS LOGICAL NO-UNDO.
    DEF VAR deTSocioDemo AS DECIMAL NO-UNDO.
    CREATE "Excel.Application" chExcelApplication CONNECT NO-ERROR.
    IF NOT VALID-HANDLE(chExcelApplication) THEN CREATE "Excel.Application" chExcelApplication .
    IF NOT VALID-HANDLE(chExcelApplication)
    THEN DO:
        MESSAGE  "ERROR: Abriendo Excel".
        RETURN.
    END.
    chExcelApplication:VISIBLE = FALSE.
    chExcelApplication:screenupdating = FALSE.


    /* las siguientes líneas son mientras se desarrolla */
    chExcelApplication:VISIBLE = TRUE.
    chExcelApplication:screenupdating = TRUE.
    /* FIN las siguientes líneas son mientras se desarrolla */


    chWorkSheet = chExcelApplication:workbooks:add().
    chWorkSheet = chExcelApplication:Sheets:Item(iSheetNumber).
    chWorkSheet = chExcelApplication:workbooks:item(1):worksheets:add().
    chWorkSheet:activate().
    chWorkSheet:Name = "Formato" + cFrmto + "_" + string(fFchaaaaammdd(daFchaCrte)).

    chExcel = chExcelApplication.
    RUN DNEElHdeExelSUPER IN hSper001(chExcel).
/**************************************************************************************************************************************/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelCierra wWin 
PROCEDURE ExcelCierra :
/*------------------------------------------------------------------------------
  Purpose: CIERRA EXCEL
  Parameters:  <none>
  Notes:     InfrmeScringAExcel  
  LOG: CREADO, 3 MAR 2008, ING. Edilberto Mariño Moya
------------------------------------------------------------------------------*/


/**************************************************************************************************************************************/
    DEF INPUT PARAMETER cFrmto AS CHAR NO-UNDO.
    chExcelApplication:VISIBLE = TRUE.
    chExcelApplication:screenupdating = TRUE.
    chExcelApplication:displayalerts = FALSE.
    chExcelApplication:ActiveWorkbook:SaveAs(w_pathspl + "Formato" + cFrmto + "_" + string(fFchaaaaammdd(daFchaCrte)),1,"","",FALSE,FALSE,,).
    RELEASE OBJECT chWorksheet.
    RELEASE OBJECT chWorkBook   NO-ERROR.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExcelTrmna wWin 
PROCEDURE ExcelTrmna :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    chExcelApplication:QUIT(). 
/*     RELEASE OBJECT chWorksheet.           */
/*     RELEASE OBJECT chWorkBook   NO-ERROR. */
    RELEASE OBJECT chExcelApplication.

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
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR c1 AS CHAR NO-UNDO.
    RUN SUPER.
    /* Code placed here will execute AFTER standard behavior.    */
    DO WITH FRAME {&FRAME-NAME}:
        daFchaCrte = DATE(MONTH(TODAY),1,YEAR(TODAY)) - 1.
        daFchaIncial = DATE(MONTH(daFchaCrte),1,YEAR(daFchaCrte)).
        daFchaFnal = daFchaCrte.
        DISPLAY daFchaCrte  dafchaIncial daFchaFnal.
        cmbPriodcdad:PRIVATE-DATA = "".
        c2Slccion:PRIVATE-DATA = "".
        FOR EACH SuperFormatoC NO-LOCK
            BREAK
                BY SuperFormatoC.periodicidad:
            IF FIRST-OF(SuperFormatoC.periodicidad)
            THEN do:
                cmbPriodcdad:PRIVATE-DATA = cmbPriodcdad:PRIVATE-DATA + SuperFormatoC.periodicidad + CHR(1).
                c1 = c1 + SuperFormatoC.periodicidad + "," + SuperFormatoC.periodicidad + ",".
                c = "".
            END.
            c = c + "[" + SuperFormatoC.Codigo + "] " + replace(SuperFormatoC.Nombre,","," ") + "," + SuperFormatoC.Codigo + ",".

            IF LAST-OF(SuperFormatoC.periodicidad)
            THEN do:
                c2Slccion:PRIVATE-DATA = c2Slccion:PRIVATE-DATA + trim(c,",") + CHR(1).
            END.
        END.
        c1 = TRIM(c1,",").
        cmbPriodcdad:LIST-ITEM-PAIRS = c1.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pFrmtos wWin 
PROCEDURE pFrmtos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR c AS CHAR NO-UNDO.

    /* break */

    SESSION:SET-WAIT("general").
    DO WITH FRAME {&FRAME-NAME}:
        RUN FchaDeCrteSUPER IN hSper001 (string(dafchaIncial) + chr(1) + string(daFchaFnal) + CHR(1) + string(daFchaCrte)).
        DO i = 1 TO NUM-ENTRIES(cSlcciondos:LIST-ITEM-PAIRS,","):
            IF i MOD 2 = 0 
            THEN DO:
                IF NOT trim(ENTRY(i ,cSlcciondos:LIST-ITEM-PAIRS,",")) = "*" 
                THEN DO:
                    c = ENTRY(i ,cSlcciondos:LIST-ITEM-PAIRS,",").
                    RUN ExcelAbre(c).
                    RUN pFrmtoActual IN hSper001(c).
                    DYNAMIC-FUNCTION('f' + c).
                    RUN ExcelCierra(c).
                    /* f338(). /* FORMATO SUPER 338 */*/
                END.
            END. /* IF i MOD 2 = 0  */
        END. /* DO i = 1 TO NUM-ENTRIES(cSlcciondos:LIST-ITEM-PAIRS,","): */
    END. /*     DO WITH FRAME {&FRAME-NAME}: */
    SESSION:SET-WAIT("").
    BELL.
    RUN ExcelTrmna.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION f338 wWin 
FUNCTION f338 RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN super().

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFchaaaaammdd wWin 
FUNCTION fFchaaaaammdd RETURNS INTEGER
  (dafcha AS DATE /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN YEAR(daFcha) * 10000 + MONTH(dafcha) * 100 + DAY(dafcha).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION ftrim wWin 
FUNCTION ftrim RETURNS CHARACTER
  (c AS CHAR) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR i AS INTEGER NO-UNDO.
    DEF VAR c1 AS CHAR NO-UNDO.
    DO i = 1 TO NUM-ENTRIES(c,","):
        IF NOT trim(ENTRY(i,c,",")) = "" 
        THEN c1 = c1 + ENTRY(i,c,",") + ",".
    END.
  RETURN trim(c1,",").

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

