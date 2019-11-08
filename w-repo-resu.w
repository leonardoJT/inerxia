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
&Scoped-Define ENABLED-OBJECTS deTtalPrdda ed-agncia ed-lnea ed-AgnciaLnea ~
ed-LneaAgncia 
&Scoped-Define DISPLAYED-OBJECTS deTtalPrdda ed-agncia ed-lnea ~
ed-AgnciaLnea ed-LneaAgncia 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAgncia wWin 
FUNCTION fAgncia RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fAgnciaLnea wWin 
FUNCTION fAgnciaLnea RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD flnea wWin 
FUNCTION flnea RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD flneaAgncia wWin 
FUNCTION flneaAgncia RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD fTtal wWin 
FUNCTION fTtal RETURNS CHARACTER
  ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-ed-agncia 
       MENU-ITEM m_copia-Agncia LABEL "copia"         .

DEFINE MENU POPUP-MENU-ed-AgnciaLnea 
       MENU-ITEM m_copia-AgnciaLnea LABEL "copia"         .

DEFINE MENU POPUP-MENU-ed-lnea 
       MENU-ITEM m_copia-Lnea   LABEL "copia"         .

DEFINE MENU POPUP-MENU-ed-LneaAgncia 
       MENU-ITEM m_copia-LneaAgncia LABEL "copia"         .


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Termina 
     IMAGE-UP FILE "imagenes\volver":U NO-FOCUS
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62
     FONT 4.

DEFINE VARIABLE ed-agncia AS CHARACTER 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 70 BY 16.96 TOOLTIP "Resumen Por Agencias"
     FONT 5 NO-UNDO.

DEFINE VARIABLE ed-AgnciaLnea AS CHARACTER 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 70 BY 16.96 TOOLTIP "Resumen Por Agencia/Línea"
     FONT 5 NO-UNDO.

DEFINE VARIABLE ed-lnea AS CHARACTER 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 70 BY 16.96 TOOLTIP "Resumen Por Líneas"
     FONT 5 NO-UNDO.

DEFINE VARIABLE ed-LneaAgncia AS CHARACTER 
     VIEW-AS EDITOR LARGE NO-BOX
     SIZE 70 BY 16.96 TOOLTIP "Resumen Por Línea/Agencia"
     FONT 5 NO-UNDO.

DEFINE VARIABLE deTtalPrdda AS DECIMAL FORMAT "***,***,***,**9":U INITIAL 0 
     LABEL "Total Pérdida Esperada" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 TOOLTIP "Total Pérdida Esperada" NO-UNDO.

DEFINE RECTANGLE RECT-314
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 2.15.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Btn_Termina AT ROW 4.5 COL 106 HELP
          "Termina la consulta del Plan Contable" WIDGET-ID 28
     deTtalPrdda AT ROW 1.27 COL 56.86 COLON-ALIGNED WIDGET-ID 30
     ed-agncia AT ROW 3.69 COL 22 NO-LABEL WIDGET-ID 32
     ed-lnea AT ROW 3.69 COL 22 NO-LABEL WIDGET-ID 34
     ed-AgnciaLnea AT ROW 3.69 COL 22 NO-LABEL WIDGET-ID 36
     ed-LneaAgncia AT ROW 3.69 COL 22 NO-LABEL WIDGET-ID 38
     RECT-314 AT ROW 4.23 COL 105 WIDGET-ID 26
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 5 WIDGET-ID 100.


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
         TITLE              = "REPOSITORIO - Pérdida Esperada"
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
         BGCOLOR            = 17
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 5
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
   FRAME-NAME Size-to-Fit                                               */
ASSIGN 
       FRAME fMain:SCROLLABLE       = FALSE.

/* SETTINGS FOR BUTTON Btn_Termina IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       Btn_Termina:HIDDEN IN FRAME fMain           = TRUE.

ASSIGN 
       deTtalPrdda:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       ed-agncia:POPUP-MENU IN FRAME fMain       = MENU POPUP-MENU-ed-agncia:HANDLE
       ed-agncia:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       ed-AgnciaLnea:POPUP-MENU IN FRAME fMain       = MENU POPUP-MENU-ed-AgnciaLnea:HANDLE
       ed-AgnciaLnea:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       ed-lnea:POPUP-MENU IN FRAME fMain       = MENU POPUP-MENU-ed-lnea:HANDLE
       ed-lnea:READ-ONLY IN FRAME fMain        = TRUE.

ASSIGN 
       ed-LneaAgncia:POPUP-MENU IN FRAME fMain       = MENU POPUP-MENU-ed-LneaAgncia:HANDLE
       ed-LneaAgncia:READ-ONLY IN FRAME fMain        = TRUE.

/* SETTINGS FOR RECTANGLE RECT-314 IN FRAME fMain
   NO-ENABLE                                                            */
ASSIGN 
       RECT-314:HIDDEN IN FRAME fMain           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* REPOSITORIO - Pérdida Esperada */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* REPOSITORIO - Pérdida Esperada */
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


&Scoped-define SELF-NAME m_copia-Agncia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_copia-Agncia wWin
ON CHOOSE OF MENU-ITEM m_copia-Agncia /* copia */
DO:
    DEF VAR i AS INTEGER NO-UNDO.
    OUTPUT TO "clipboard".
    DO WITH FRAME {&FRAME-NAME}:
        DO i = 1 TO NUM-ENTRIES(ed-agncia:SCREEN-VALUE,CHR(10)):
            PUT UNFORMATTE entry(i,ed-agncia:SCREEN-VALUE,CHR(10)) SKIP.
        END.
    END.
    OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_copia-AgnciaLnea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_copia-AgnciaLnea wWin
ON CHOOSE OF MENU-ITEM m_copia-AgnciaLnea /* copia */
DO:
    DEF VAR i AS INTEGER NO-UNDO.
    OUTPUT TO "clipboard".
    DO WITH FRAME {&FRAME-NAME}:
        DO i = 1 TO NUM-ENTRIES(ed-AgnciaLnea:SCREEN-VALUE,CHR(10)):
            PUT UNFORMATTE entry(i,ed-AgnciaLnea:SCREEN-VALUE,CHR(10)) SKIP.
        END.
    END.
    OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_copia-Lnea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_copia-Lnea wWin
ON CHOOSE OF MENU-ITEM m_copia-Lnea /* copia */
DO:
    DEF VAR i AS INTEGER NO-UNDO.
    OUTPUT TO "clipboard".
    DO WITH FRAME {&FRAME-NAME}:
        DO i = 1 TO NUM-ENTRIES(ed-lnea:SCREEN-VALUE,CHR(10)):
            PUT UNFORMATTE entry(i,ed-lnea:SCREEN-VALUE,CHR(10)) SKIP.
        END.
    END.
    OUTPUT CLOSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_copia-LneaAgncia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_copia-LneaAgncia wWin
ON CHOOSE OF MENU-ITEM m_copia-LneaAgncia /* copia */
DO:
    DEF VAR i AS INTEGER NO-UNDO.
    OUTPUT TO "clipboard".
    DO WITH FRAME {&FRAME-NAME}:
        DO i = 1 TO NUM-ENTRIES(ed-LneaAgncia:SCREEN-VALUE,CHR(10)):
            PUT UNFORMATTE entry(i,ed-LneaAgncia:SCREEN-VALUE,CHR(10)) SKIP.
        END.
    END.
    OUTPUT CLOSE.
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
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Agencias|Líneas|Agencia/Línea|Línea/Agencia' + 'FolderTabWidth0FolderFont5HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 2.35 , 22.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 1.12 , 70.00 ) NO-ERROR.

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_folder ,
             deTtalPrdda:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE changePage wWin 
PROCEDURE changePage :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR iPgna AS INTEGER NO-UNDO.
  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */
    ipgna = DYNAMIC-FUNCTION('getCurrentPage':U).
    DO WITH FRAME  {&FRAME-NAME}:
        ed-agncia:HIDDEN        = NOT ipgna = 1.
        ed-lnea:HIDDEN          = NOT ipgna = 2.
        ed-AgnciaLnea:HIDDEN    = NOT ipgna = 3.
        ed-LneaAgncia:HIDDEN    = NOT ipgna = 4.
    END.
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
  DISPLAY deTtalPrdda ed-agncia ed-lnea ed-AgnciaLnea ed-LneaAgncia 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE deTtalPrdda ed-agncia ed-lnea ed-AgnciaLnea ed-LneaAgncia 
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
        ed-agncia:SCREEN-VALUE = "RESUMEN POR AGENCIAS".
        ed-agncia:FONT = 2.
        ed-lnea:SCREEN-VALUE = "RESUMEN POR LINEAS".
        ed-lnea:FONT = 2.
        ed-Agncialnea:SCREEN-VALUE = "RESUMEN POR AGENCIA/LINEA".
        ed-Agncialnea:FONT = 2.
        ed-lneaAgncia:SCREEN-VALUE = "RESUMEN POR LINEA/AGENCIA".
        ed-lneaAgncia:FONT = 2.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pMuestraTtalPrdda wWin 
PROCEDURE pMuestraTtalPrdda :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DO WITH FRAME {&FRAME-NAME}:
        deTtalPrdda:SCREEN-VALUE = c.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRsmenAgnciaLnea wWin 
PROCEDURE pRsmenAgnciaLnea :
/*------------------------------------------------------------------------------
  Purpose:   pRsmenAgnciaLnea  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DO WITH FRAME  {&FRAME-NAME}:
        ed-AgnciaLnea:SCREEN-VALUE = c.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRsmenAgncias wWin 
PROCEDURE pRsmenAgncias :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DO WITH FRAME  {&FRAME-NAME}:
        ed-agncia:SCREEN-VALUE = c.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRsmenLneaAgncia wWin 
PROCEDURE pRsmenLneaAgncia :
/*------------------------------------------------------------------------------
  Purpose:   pRsmenLneaAgncia  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DO WITH FRAME  {&FRAME-NAME}:
        ed-LneaAgncia:SCREEN-VALUE = c.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE pRsmenLneas wWin 
PROCEDURE pRsmenLneas :
/*------------------------------------------------------------------------------
  Purpose:   pRsmenLneas  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    DO WITH FRAME  {&FRAME-NAME}:
        ed-lnea:SCREEN-VALUE = c.
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAgncia wWin 
FUNCTION fAgncia RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN  ed-agncia:SCREEN-VALUE IN FRAME {&FRAME-NAME} + fttal().

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fAgnciaLnea wWin 
FUNCTION fAgnciaLnea RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN  ed-agnciaLnea:SCREEN-VALUE IN FRAME {&FRAME-NAME} + fttal().

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION flnea wWin 
FUNCTION flnea RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN  ed-lnea:SCREEN-VALUE IN FRAME {&FRAME-NAME} + fttal().

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION flneaAgncia wWin 
FUNCTION flneaAgncia RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

    RETURN  ed-lneaagncia:SCREEN-VALUE IN FRAME {&FRAME-NAME} + fttal().

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION fTtal wWin 
FUNCTION fTtal RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  chr(10) + "TOTAL PERDIDA ESPERADA               " +
      deTtalPrdda:SCREEN-VALUE IN FRAME {&FRAME-NAME} + CHR(10).

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

