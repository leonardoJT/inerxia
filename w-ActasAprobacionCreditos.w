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
      DEFINE VAR W_Puntero  AS ROWID NO-UNDO.
      DEFINE VAR W_Ok       AS LOGICAL NO-UNDO.
      DEFINE VAR W_Intentos AS INTEGER NO-UNDO.
      DEFINE VAR W_NitAnt   LIKE Clientes.Nit NO-UNDO.
      DEFINE VAR W_Inf      AS CHARACTER FORMAT "X(10)" NO-UNDO.
DEF TEMP-TABLE tUsuario NO-UNDO
    FIELD usuario AS CHAR
    FIELD nmbre AS CHAR
    INDEX pk usuario.

    DEF VAR t AS CHAR NO-UNDO EXTENT 4.
    t[1] = "Propiedad ".
    t[2] = "Vehículo ".
    t[3] = "Inversión ".
    t[4] = "No Admisible ".
DEF VAR cEstdo AS CHAR NO-UNDO EXTENT 3.
    cEstdo[2] = "APROBADO".
    cEstdo[3] = "RECHAZADO".

DEFINE VARIABLE chWordApplication AS COM-HANDLE NO-UNDO.
DEF VAR cTdos AS CHAR NO-UNDO.

DEF VAR tpoGrntia AS CHAR NO-UNDO EXTENT 3.
tpoGrntia[1] = "Personal". 
tpoGrntia[2] = "Admisible".    
tpoGrntia[3] = "No Admisible".

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
&Scoped-Define ENABLED-OBJECTS BUTTON-209 BUTTON-11 RECT-1 RECT-2 RECT-3 ~
RECT-323 RECT-324 daFchaActa BUTTON-2 daFchai daFchaF iEstdo lDfntva ~
BUTTON-208 cUsuarios cEncbzdo Btn_Termina cEstmntos cFrmas 
&Scoped-Define DISPLAYED-OBJECTS daFchaActa daFchai daFchaF iEstdo lDfntva ~
cUsuarios cEncbzdo cEstmntos cFrmas 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fFrmas wWin 
FUNCTION fFrmas RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fGrntias wWin 
FUNCTION fGrntias RETURNS CHARACTER
  (iSlctud AS INTEGER /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fNoDscncdo wWin 
FUNCTION fNoDscncdo RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fUsuario wWin 
FUNCTION fUsuario RETURNS LOGICAL
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Termina 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62
     FONT 4.

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U NO-FOCUS
     LABEL "Button 11" 
     SIZE 5 BY 1.08.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U NO-FOCUS
     LABEL "Button 2" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-208 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 208" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-209  NO-FOCUS FLAT-BUTTON
     LABEL "INICIA" 
     SIZE 15 BY 1.12 TOOLTIP "Inicializa El Texto Del Encabezado".

DEFINE VARIABLE cEncbzdo AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 48 BY 6.73 NO-UNDO.

DEFINE VARIABLE daFchaActa AS DATE FORMAT "99/99/99":U 
     LABEL "Acta" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 TOOLTIP "Fecha Acta" NO-UNDO.

DEFINE VARIABLE daFchaF AS DATE FORMAT "99/99/99":U 
     LABEL "Final" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 TOOLTIP "Fecha Final" NO-UNDO.

DEFINE VARIABLE daFchai AS DATE FORMAT "99/99/99":U 
     LABEL "Inicial" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1 TOOLTIP "Fecha Inicial" NO-UNDO.

DEFINE VARIABLE iEstdo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL EXPAND 
     RADIO-BUTTONS 
          "Aprobada", 2,
"Negada", 3,
"Todas", 0
     SIZE 35 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 72 BY 1.62 TOOLTIP "Rango De Tiempo".

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 14 BY 1.62.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 111.72 BY .27.

DEFINE RECTANGLE RECT-323
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 18 BY 1.62.

DEFINE RECTANGLE RECT-324
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   ROUNDED 
     SIZE 50 BY 8.35.

DEFINE VARIABLE cEstmntos AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     LIST-ITEMS "Comité De Crédito","Consejo De Administración","Gerente General","Director De Otorgamiento","Director De Otorgamiento Y Analista","Analista" 
     SIZE 52 BY 7.54 TOOLTIP "Estamentos" NO-UNDO.

DEFINE VARIABLE cFrmas AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "GULFRAN ANTONIO AVILEZ (Gerente General)","GULFRAN ANTONIO AVILEZ (Gerente General)",
                     "BEATRIZ ELENA MUNERA (Subgerente)","BEATRIZ ELENA MUNERA (Subgerente)",
                     "PAULA MONTOYA GOMEZ (Presidente)","PAULA MONTOYA GOMEZ (Presidente)",
                     "DORIS ELENA IDARRAGA MARTINEZ (Director Encargado)","DORIS ELENA IDARRAGA MARTINEZ (Director Encargado)",
                     "JOSE POMPILIO RUIZ (Integrante)","JOSE POMPILIO RUIZ (Integrante)",
                     "NESTOR JARAMILLO HERNANDEZ (Integrante)","NESTOR JARAMILLO HERNANDEZ (Integrante)",
                     "LILIAM PEREZ MUÑETON (Integrante)","LILIAM PEREZ MUÑETON (Integrante)",
                     "DIEGO GRISALES TIRADO (Analista)","DIEGO GRISALES TIRADO (Analista)" 
     SIZE 52 BY 7.54 TOOLTIP "Los firmantes" NO-UNDO.

DEFINE VARIABLE cUsuarios AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS " Todos","*" 
     SIZE 48 BY 7 TOOLTIP "Usuarios" NO-UNDO.

DEFINE VARIABLE lDfntva AS LOGICAL INITIAL no 
     LABEL "Definitiva" 
     VIEW-AS TOGGLE-BOX
     SIZE 12 BY .77 TOOLTIP "Definitiva O Borrador" NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-209 AT ROW 10.42 COL 18.57 WIDGET-ID 64
     BUTTON-11 AT ROW 8.27 COL 104.86 WIDGET-ID 24
     daFchaActa AT ROW 1.27 COL 9 COLON-ALIGNED WIDGET-ID 58
     BUTTON-2 AT ROW 4.5 COL 100.86 WIDGET-ID 26
     daFchai AT ROW 1.27 COL 28 COLON-ALIGNED WIDGET-ID 2
     daFchaF AT ROW 1.27 COL 45 COLON-ALIGNED WIDGET-ID 4
     iEstdo AT ROW 1.27 COL 59 NO-LABEL WIDGET-ID 36
     lDfntva AT ROW 1.42 COL 96 WIDGET-ID 6
     BUTTON-208 AT ROW 2.88 COL 100.86 WIDGET-ID 22
     cUsuarios AT ROW 3.58 COL 51 NO-LABEL WIDGET-ID 10
     cEncbzdo AT ROW 3.69 COL 2 NO-LABEL WIDGET-ID 62
     Btn_Termina AT ROW 6.38 COL 102.86 HELP
          "Termina la consulta del Plan Contable" WIDGET-ID 28
     cEstmntos AT ROW 13.12 COL 2 NO-LABEL WIDGET-ID 52
     cFrmas AT ROW 13.12 COL 55 NO-LABEL WIDGET-ID 56
     "ESTAMENTOS" VIEW-AS TEXT
          SIZE 14 BY .62 AT ROW 12.46 COL 21 WIDGET-ID 54
     "USUARIOS DISPONIBLES" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 2.88 COL 62.57 WIDGET-ID 16
     "FIRMAS" VIEW-AS TEXT
          SIZE 8 BY .62 AT ROW 12.46 COL 78.14 WIDGET-ID 46
     RECT-1 AT ROW 1 COL 23 WIDGET-ID 12
     RECT-2 AT ROW 1 COL 95 WIDGET-ID 14
     RECT-3 AT ROW 2.62 COL 1.29 WIDGET-ID 18
     RECT-323 AT ROW 1 COL 5 WIDGET-ID 60
     RECT-324 AT ROW 3.42 COL 1 WIDGET-ID 66
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 19.77
         BGCOLOR 17  WIDGET-ID 100.


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
         TITLE              = "ACTAS APROBACION CREDITO"
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
ASSIGN 
       FRAME fMain:PRIVATE-DATA     = 
                "00) Identificación Cliente".

ASSIGN 
       cEncbzdo:RETURN-INSERTED IN FRAME fMain  = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* ACTAS APROBACION CREDITO */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* ACTAS APROBACION CREDITO */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Termina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Termina wWin
ON CHOOSE OF Btn_Termina IN FRAME fMain /* Terminar Consulta */
DO:
    APPLY "CLOSE":U TO THIS-PROCEDURE.
    RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME fMain /* Button 2 */
DO:
    /*
    W_Inf = "Cliente".     
    DEFINE VAR Listado AS CHARACTER INITIAL "" NO-UNDO.
    Listado = W_Pathspl + "Lst_Relaciones.lst".
    {incluido/imprimir.i "Listado"}.
   */
    RUN pCmbiaFcha.
    RUN pCmbiaFI.
    RUN pCmbiaFF.
    RUN pCmciaEstmnto.
    SESSION:SET-WAIT("general").
    CREATE "Word.application" chWordApplication.
    
    chWordApplication:VISIBLE=FALSE.
    chWordApplication:Documents:ADD().
    
    chWordApplication:Selection:FONT:bold = 9999998.
    chWordApplication:Selection:font:SIZE = 14.
    chWordApplication:Selection:ParagraphFormat:Alignment = 1.
    chWordApplication:Selection:typetext("ACTA NO. " + (IF LDfntva:SCREEN-VALUE = "yes" THEN string(NEXT-VALUE(sec_ActaAprbcion)) ELSE "PARA REVISION")).
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.

    /*
    chWordApplication:Selection:ParagraphFormat:Alignment = 0.
    chWordApplication:Selection:typetext("REUNION ORDINARIA").
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    */

    chWordApplication:Selection:FONT:bold = 9999998.
    chWordApplication:Selection:font:SIZE = 12.
    chWordApplication:Selection:ParagraphFormat:Alignment = 3.
    chWordApplication:Selection:typetext(cEncbzdo:SCREEN-VALUE).
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    /*
    chWordApplication:Selection:FONT:bold = 9999998.
    chWordApplication:Selection:font:SIZE = 14.
    chWordApplication:Selection:ParagraphFormat:Alignment = 0.
    chWordApplication:Selection:typetext("ORDEN DEL DIA").
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    */
    /*
    chWordApplication:Selection:FONT:bold = 9999998.
    chWordApplication:Selection:font:SIZE = 12.
    chWordApplication:Selection:ParagraphFormat:Alignment = 3.
    chWordApplication:Selection:typetext("Presentación de solicitudes de crédito, análisis y toma de decisión.").
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.

    chWordApplication:Selection:FONT:bold = 9999998.
    chWordApplication:Selection:font:SIZE = 12.
    chWordApplication:Selection:ParagraphFormat:Alignment = 3.
    chWordApplication:Selection:FONT:UNDERLINE = 1.
    chWordApplication:Selection:typetext("Solicitudes").
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.

    
    
    chWordApplication:Selection:FONT:bold = 9999998.
    chWordApplication:Selection:font:SIZE = 12.
    chWordApplication:Selection:ParagraphFormat:Alignment = 3.
    chWordApplication:Selection:FONT:UNDERLINE = 0.
    */
    RUN pActas.    
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    chWordApplication:Selection:FONT:bold = 9999998.
    chWordApplication:Selection:font:SIZE = 12.
    chWordApplication:Selection:ParagraphFormat:Alignment = 3.
    chWordApplication:Selection:FONT:UNDERLINE = 0.
    chWordApplication:Selection:typetext("Siendo las 2:00 p.m. se da por terminada la reunión.").
    chWordApplication:selection:TypeParagraph.
    chWordApplication:selection:TypeParagraph.
    
    fFrmas(cFrmas:SCREEN-VALUE).
    chWordApplication:VISIBLE=TRUE.
    
    chWordApplication:Quit().
    
    RELEASE OBJECT chWordApplication.
  
    SESSION:SET-WAIT("").

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-208
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-208 wWin
ON CHOOSE OF BUTTON-208 IN FRAME fMain /* Button 208 */
DO:
  RUN W-InfDia.w NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-209
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-209 wWin
ON CHOOSE OF BUTTON-209 IN FRAME fMain /* INICIA */
DO:
    RUN pInciaEncbzdo.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cEstmntos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cEstmntos wWin
ON MOUSE-SELECT-CLICK OF cEstmntos IN FRAME fMain
DO:                   
    DO WITH FRAME  {&FRAME-NAME}:
        cEncbzdo:SCREEN-VALUE = REPLACE(cEncbzdo:SCREEN-VALUE,"%ESTAMENTO%", SELF:SCREEN-VALUE).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME cUsuarios
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL cUsuarios wWin
ON VALUE-CHANGED OF cUsuarios IN FRAME fMain
DO:
    /*
    DEF VAR i AS INTEGER NO-UNDO.
    DO WITH FRAME  {&FRAME-NAME}:
        DO i = 1 TO NUM-ENTRIES(cUsuarios:SCREEN-VALUE,","):
            CASE ENTRY(i,cUsuarios:SCREEN-VALUE,","):
                WHEN "256" OR WHEN "446" OR WHEN "445" OR WHEN "289" OR WHEN "251" OR WHEN "592"
                THEN DO:
                    cfrmas:SCREEN-VALUE = "LUIS FERNANDO BERNAL GALOFRE      ________________________________" + CHR(10) +
                            "Director De Crédito".
                END.
                WHEN "589"
                THEN DO:
                    RUN pfirmas.
                END.
                WHEN "591"
                THEN DO:
                    
                END.
                WHEN "590" OR WHEN "588"
                THEN DO:
                    cfrmas:SCREEN-VALUE = 
                        "FABIO CHAVARRO GONZALEZ               ________________________________" + CHR(10) +
                        "Gerente General JURISCOOP" + CHR(10) +
                        "JORGE ENRIQUE VERA ARAMBULA     ________________________________" + CHR(10) +
                        "Gerente Financiero" + CHR(10) +
                        "HECTOR RAUL RUIZ VELANDIA             ________________________________" + CHR(10) +
                        "Gerente De Riesgo".
                END.
            END CASE.
        END.
    END.
    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME daFchaActa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL daFchaActa wWin
ON LEAVE OF daFchaActa IN FRAME fMain /* Acta */
DO:
    IF date(SELF:SCREEN-VALUE) = ? 
    THEN DO:
        MESSAGE "Fecha No Factible"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
    END.
    ASSIGN {&SELF-NAME}.
    RUN pCmbiaFcha.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME daFchaF
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL daFchaF wWin
ON LEAVE OF daFchaF IN FRAME fMain /* Final */
DO:
  RUN pCmbiaFF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME daFchai
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL daFchai wWin
ON LEAVE OF daFchai IN FRAME fMain /* Inicial */
DO:
    RUN pCmbiaFI.  
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
  DISPLAY daFchaActa daFchai daFchaF iEstdo lDfntva cUsuarios cEncbzdo cEstmntos 
          cFrmas 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE BUTTON-209 BUTTON-11 RECT-1 RECT-2 RECT-3 RECT-323 RECT-324 daFchaActa 
         BUTTON-2 daFchai daFchaF iEstdo lDfntva BUTTON-208 cUsuarios cEncbzdo 
         Btn_Termina cEstmntos cFrmas 
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
    fUsuario().
    DO WITH FRAME  {&FRAME-NAME}:
        dafchai     = TODAY.
        dafchaf     = TODAY.
        daFchaActa  = TODAY.
        iEstdo = 2.
        DISPLAY dafchai dafchaf iEstdo daFchaActa.            
        cUsuarios:SCREEN-VALUE = entry(2,cUsuarios:LIST-ITEM-PAIRS,",").
        RUN pInciaEncbzdo.
        RUN pCmbiaFcha.
        RUN pCmbiaFI.
        RUN pCmbiaFF.
    END.
    RUN pfirmas.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pActas wWin 
PROCEDURE pActas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR cl AS CHAR NO-UNDO.
    DEF VAR hrngo AS COM-HANDLE NO-UNDO.
    DEF VAR hTbla AS COM-HANDLE NO-UNDO.
    DO WITH FRAME  {&FRAME-NAME}:
        EMPTY TEMP-TABLE tusuario.
        DEF VAR i AS INTEGER NO-UNDO.
        FOR EACH usuarios FIELDS(usuario) NO-LOCK
            WHERE
                IF cusuarios:SCREEN-VALUE = "*"
                THEN CAN-DO(cTdos,usuarios.usuario)
                ELSE CAN-DO(cusuarios:SCREEN-VALUE,usuarios.usuario):
            CREATE tusuario.
            assign  tusuario.usuario    =   usuarios.usuario
                    tusuario.Nmbre      =   usuarios.nombre.
        END.
        cl = "ASOCIADO| SECC.|VALOR|LINEA|FORMA PAGO|PLA.|GARANTIA|DECISION".
        chWordApplication:Selection:FONT:bold = 9999998.
        chWordApplication:Selection:font:SIZE = 8.
        chWordApplication:Selection:ParagraphFormat:Alignment = 1.
        chWordApplication:Selection:FONT:UNDERLINE = 0.
        chWordApplication:Selection:typetext(cl).
        chWordApplication:selection:TypeParagraph.
        chWordApplication:Selection:FONT:bold = 9999998.


        FOR EACH tusuario NO-LOCK,
            EACH mov_instancia NO-LOCK
            WHERE
                mov_instancia.usuario = tusuario.usuario
            AND mov_instancia.estado = TRUE /* solo las procesadas */
            AND mov_instancia.fec_retiro >= DATE(daFchai:SCREEN-VALUE)
            AND mov_instancia.fec_retiro <= DATE(dafchaf:SCREEN-VALUE)
            BREAK
                BY tusuario.usuario:
            FOR FIRST solicitud NO-LOCK
                WHERE
                    solicitud.num_solicitud = mov_instancia.num_solicitud
                AND (IF integer(iEstdo:SCREEN-VALUE) = 0 THEN TRUE ELSE solicitud.estado = integer(iEstdo:SCREEN-VALUE))
                AND solicitud.estado >= 2 AND solicitud.estado <= 3,
                FIRST clientes NO-LOCK
                    WHERE
                        clientes.nit = solicitud.nit,
                FIRST agencias NO-LOCK
                    WHERE
                        agencias.agencia = solicitud.agencia,
                FIRST pro_creditos NO-LOCK
                    WHERE
                        pro_creditos.cod_credito = solicitud.cod_credito:
                cl = trim(clientes.nombre) + " " + TRIM(clientes.apellido1) + " " + TRIM(clientes.apellido2) + "|" +
                    agencias.nombre + "|" +
                    string(Solicitud.Monto,"$>>>,>>>,>>9") + "|" +
                    pro_creditos.nom_producto + "|" +
                    (IF solicitud.FOR_pago = 1 THEN "CAJA" ELSE (IF solicitud.FOR_pago = 2 THEN "NOMINA" ELSE "")) + "|" +
                    STRING((solicitud.plazo / 30)) + "|" +
                    fGrntias(solicitud.num_solicitud) + "|" +
                    cEstdo[solicitud.estado].
                chWordApplication:Selection:font:SIZE = 8.
                chWordApplication:Selection:ParagraphFormat:Alignment = 1.
                chWordApplication:Selection:FONT:UNDERLINE = 0.
                chWordApplication:Selection:typetext(cl).
                chWordApplication:selection:TypeParagraph.
            END.
        END.
    
    END.
    chWordApplication:Selection:FONT:bold = 9999998.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCmbiaFcha wWin 
PROCEDURE pCmbiaFcha :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        cEncbzdo:SCREEN-VALUE = REPLACE(cEncbzdo:SCREEN-VALUE,"%FECHAACTA%", STRING(daFchaActa)).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCmbiaFF wWin 
PROCEDURE pCmbiaFF :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        cEncbzdo:SCREEN-VALUE = REPLACE(cEncbzdo:SCREEN-VALUE,"%FF%", daFchaf:SCREEN-VALUE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCmbiaFI wWin 
PROCEDURE pCmbiaFI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        cEncbzdo:SCREEN-VALUE = REPLACE(cEncbzdo:SCREEN-VALUE,"%FI%", daFchai:SCREEN-VALUE).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pCmciaEstmnto wWin 
PROCEDURE pCmciaEstmnto :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        cEncbzdo:SCREEN-VALUE = REPLACE(cEncbzdo:SCREEN-VALUE,"%ESTAMENTO%", cEstmntos:SCREEN-VALUE).
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pfirmas wWin 
PROCEDURE pfirmas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
    DO WITH FRAME   {&FRAME-NAME}:
        cfrmas:SCREEN-VALUE = 
            "FABIO CHAVARRO GONZALEZ               ________________________________" + CHR(10) +
            "Gerente General JURISCOOP" + CHR(10) +
            "JORGE ENRIQUE VERA ARAMBULA     ________________________________" + CHR(10) +
            "Gerente Financiero" + CHR(10) +
            "HECTOR RAUL RUIZ VELANDIA             ________________________________" + CHR(10) +
            "Gerente De Riesgo" + CHR(10) +
            "LUIS FERNANDO BERNAL GALOFRE      ________________________________" + CHR(10) +
            "Director De Crédito" + CHR(10) +
            "PRESIDENTE DEL CONSEJO O DELEGADO     ________________________________" + CHR(10) +
            "INTEGRANTE DEL CONSEJO".
    END.
*/    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pInciaEncbzdo wWin 
PROCEDURE pInciaEncbzdo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DO WITH FRAME {&FRAME-NAME}:
        cEncbzdo:SCREEN-VALUE = "Siendo las 9:00 a.m. del día %FECHAACTA% se reunió el %ESTAMENTO%, para:" + CHR(10) + CHR(10).
        cEncbzdo:SCREEN-VALUE = cEncbzdo:SCREEN-VALUE  + "ORDEN DEL DIA" + CHR(10) + CHR(10).
        cEncbzdo:SCREEN-VALUE = cEncbzdo:SCREEN-VALUE  + "Presentación de solicitudes de crédito, análisis y toma de decisión del %FI% a %FF% " + chr(10) + CHR(10).
        cEncbzdo:SCREEN-VALUE = cEncbzdo:SCREEN-VALUE  + "Solicitudes" + CHR(10).
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fFrmas wWin 
FUNCTION fFrmas RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    DEF VAR i AS INTEGER NO-UNDO.
    DO i = 1 TO NUM-ENTRIES(c,","):
        chWordApplication:Selection:FONT:bold = 9999998.
        chWordApplication:Selection:ParagraphFormat:Alignment = 0.
        chWordApplication:Selection:typetext(entry(1,ENTRY(i,c,","),"(")). 
        chWordApplication:selection:TypeParagraph.

        chWordApplication:Selection:FONT:bold = 9999998.
        chWordApplication:Selection:ParagraphFormat:Alignment = 0.
        chWordApplication:Selection:typetext(entry(1,entry(2,ENTRY(i,c,","),"("),")")). 
        chWordApplication:selection:TypeParagraph.
        chWordApplication:selection:TypeParagraph.
    END.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fGrntias wWin 
FUNCTION fGrntias RETURNS CHARACTER
  (iSlctud AS INTEGER /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR c AS CHAR NO-UNDO.
    DEF VAR cPgre AS CHAR NO-UNDO.
    FOR EACH garantias NO-LOCK
        WHERE
            garantias.num_solicitud = iSlctud:
        cPgre = fNoDscncdo(garantia.pagare).
        cPgre = TRIM((IF cPgre = "" THEN cPgre ELSE "Pagare ") + cPgre).
        c = c + " " + tpoGrntia[garantias.tipo_garantia].
        c = c + cPgre + " " +
                t[garantia.tipo_garantia] + 
                fNoDscncdo(garantia.descripcion_bien) + " " +
                STRING(garantia.val_bien," Valor Bien $>>>,>>>,>>9").
    END.
    FOR FIRST relaciones NO-LOCK
        WHERE
            relaciones.nit = solicitud.nit
        AND relaciones.clase_producto = 2
        AND relaciones.cod_relacion = 11
        AND relaciones.Cuenta = string(iSlctud):
        c = c + " PERSONAL ".
    END.
    /* CUANDO EL CREDITO YA HA SIDO DESEMBOLSADO */
    FOR FIRST creditos NO-LOCK
        WHERE
            creditos.num_solicitud = iSlctud:
        FOR FIRST relaciones NO-LOCK
            WHERE
                relaciones.nit = solicitud.nit
            AND relaciones.clase_producto = 2
            AND relaciones.cod_relacion = 11
            AND relaciones.Cuenta = string(creditos.num_credito):
            c = c + " PERSONAL ".
        END.

    END.
  RETURN trim(c).   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fNoDscncdo wWin 
FUNCTION fNoDscncdo RETURNS CHARACTER
  (c AS CHAR /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN IF c = ? THEN "" ELSE TRIM(c).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fUsuario wWin 
FUNCTION fUsuario RETURNS LOGICAL
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEF VAR cLsta AS CHAR NO-UNDO.    
    DO WITH FRAME  {&FRAME-NAME}:
         cLsta = " Todos,*". 
         cTdos = "".
        FOR EACH cfg_instancias NO-LOCK
            WHERE
                cfg_instancias.instancia = 50
            AND cfg_instancias.agencia = 4,
            EACH instancias NO-LOCK
                WHERE
                    instancias.instancia = cfg_instancias.instancia,
            EACH usuarios NO-LOCK
                WHERE
                    usuarios.usuario = cfg_instancias.usuario
            BREAK
                BY usuarios.usuario:
            IF FIRST-OF(usuario.usuario)
            THEN DO:
                cLsta = cLsta +
                       "," + usuarios.nombre + "," + usuarios.usuario.
                cTdos = cTdos +
                       "," + usuarios.nombre + "," + usuarios.usuario.
            END.
        END.
        cUsuarios:LIST-ITEM-PAIRS = cLsta.
    END.
    RETURN FALSE.   /* Function return value. */
END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

