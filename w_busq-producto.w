&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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

DEF VAR W_Ok           AS LOG NO-UNDO.

/*DEFINE OUTPUT PARAMETER P_Cla_Pro  AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER P_Tip_pro  AS INT NO-UNDO. 
DEFINE OUTPUT PARAMETER P_Cod-pro  AS INT NO-UNDO.
  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Producto

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Saro Pro_Creditos

/* Definitions for FRAME F_Producto                                     */
&Scoped-define FIELDS-IN-QUERY-F_Producto Saro.Clase_Producto 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Producto Saro.Clase_Producto 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Producto Saro
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Producto Saro
&Scoped-define QUERY-STRING-F_Producto FOR EACH Saro SHARE-LOCK, ~
      EACH Pro_Creditos WHERE TRUE /* Join to Saro incomplete */ SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Producto OPEN QUERY F_Producto FOR EACH Saro SHARE-LOCK, ~
      EACH Pro_Creditos WHERE TRUE /* Join to Saro incomplete */ SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Producto Saro Pro_Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-F_Producto Saro
&Scoped-define SECOND-TABLE-IN-QUERY-F_Producto Pro_Creditos


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Saro.Clase_Producto 
&Scoped-define ENABLED-TABLES Saro
&Scoped-define FIRST-ENABLED-TABLE Saro
&Scoped-Define ENABLED-OBJECTS Tip_Credito Cmb_Productos Btn_OutProductos ~
Btn_Salir_Producto 
&Scoped-Define DISPLAYED-FIELDS Saro.Clase_Producto 
&Scoped-define DISPLAYED-TABLES Saro
&Scoped-define FIRST-DISPLAYED-TABLE Saro
&Scoped-Define DISPLAYED-OBJECTS Tip_Credito Cmb_Productos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OutProductos 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "x" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_Salir_Producto 
     LABEL "Salir sin escoger" 
     SIZE 25 BY 1.12.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(70)":U 
     LABEL "Producto" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tip_Credito LIKE Pro_Creditos.Tip_Credito
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Consumo", 1,
"Comercial", 2,
"Hipotecario", 3,
"Microcrédito", 4
     SIZE 52 BY 1.08 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F_Producto FOR 
      Saro, 
      Pro_Creditos SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Producto
     Saro.Clase_Producto AT ROW 1.19 COL 3 HELP
          "Clase de producto de Ahorro o Crédito" NO-LABEL WIDGET-ID 8
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ahorros", 1,
"Credito", 2
          SIZE 52 BY 1.08
     Tip_Credito AT ROW 2.27 COL 3 HELP
          "Tipo de Producto de Crédito" NO-LABEL WIDGET-ID 14
     Cmb_Productos AT ROW 3.62 COL 14 COLON-ALIGNED WIDGET-ID 12
     Btn_OutProductos AT ROW 4.96 COL 48 WIDGET-ID 2
     Btn_Salir_Producto AT ROW 5.04 COL 12 WIDGET-ID 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 58.86 BY 6.62
         BGCOLOR 17 FONT 5
         TITLE "Productos" WIDGET-ID 100.


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
         TITLE              = ""
         HEIGHT             = 6.62
         WIDTH              = 58.86
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
/* SETTINGS FOR FRAME F_Producto
   FRAME-NAME                                                           */
/* SETTINGS FOR RADIO-SET Saro.Clase_Producto IN FRAME F_Producto
   EXP-HELP                                                             */
/* SETTINGS FOR RADIO-SET Tip_Credito IN FRAME F_Producto
   LIKE = bdcentral.Pro_Creditos. EXP-HELP                              */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Producto
/* Query rebuild information for FRAME F_Producto
     _TblList          = "bdcentral.Saro,bdcentral.Pro_Creditos WHERE bdcentral.Saro ..."
     _Query            is OPENED
*/  /* FRAME F_Producto */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_OutProductos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutProductos wWin
ON CHOOSE OF Btn_OutProductos IN FRAME F_Producto /* x */
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


    /*
  DO WITH FRAME F_Producto:
     /*Nom_Producto:SCREEN-VALUE IN FRAME F_Sarouno = Cmb_Productos:SCREEN-VALUE.*/
  END.
    
  HIDE FRAME F_Producto.
      */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir_Producto wWin
ON CHOOSE OF Btn_Salir_Producto IN FRAME F_Producto /* Salir sin escoger */
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


&Scoped-define SELF-NAME Saro.Clase_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Saro.Clase_Producto wWin
ON VALUE-CHANGED OF Saro.Clase_Producto IN FRAME F_Producto /* Clase de Producto */
DO:
    DO WITH FRAME F_Producto:
        Cmb_Productos:LIST-ITEMS = "".
        DISABLE Tip_Credito.

        IF INT(Saro.Clase_Producto:SCREEN-VALUE) = 1 THEN DO:
            
            FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado      EQ 1 NO-LOCK:
                W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).
                IF AVAILABLE Saro AND Saro.Cod_Producto = Pro_Ahorros.Cod_Ahorro THEN
                Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
            END.
            DISABLE Tip_Credito.
        END.
        ELSE DO:
            ENABLE Tip_Credito.
        END.

    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON VALUE-CHANGED OF Cmb_Productos IN FRAME F_Producto /* Producto */
DO:
  APPLY "CHOOSE" TO Btn_OutProductos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tip_Credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tip_Credito wWin
ON VALUE-CHANGED OF Tip_Credito IN FRAME F_Producto
DO:
    DO WITH FRAME F_Producto:
        Cmb_Productos:LIST-ITEMS = "".
    
        IF INT(Saro.Clase_Producto:SCREEN-VALUE) = 2 THEN DO:
            ENABLE Tip_Credito.
            FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ INT(Tip_Credito:SCREEN-VALUE) AND
                                        Pro_Creditos.Estado  EQ 1 NO-LOCK:
                W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
                IF AVAILABLE Saro AND Saro.Cod_Producto EQ Pro_Creditos.Cod_Credito THEN
                    Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
            END.
        END.
    END.
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

  {&OPEN-QUERY-F_Producto}
  GET FIRST F_Producto.
  DISPLAY Tip_Credito Cmb_Productos 
      WITH FRAME F_Producto IN WINDOW wWin.
  IF AVAILABLE Saro THEN 
    DISPLAY Saro.Clase_Producto 
      WITH FRAME F_Producto IN WINDOW wWin.
  ENABLE Saro.Clase_Producto Tip_Credito Cmb_Productos Btn_OutProductos 
         Btn_Salir_Producto 
      WITH FRAME F_Producto IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Producto}
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

