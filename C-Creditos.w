&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral       PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

DEFINE INPUT  PARAMETER W_Nit LIKE Creditos.Nit.
DEFINE OUTPUT PARAMETER W_Age LIKE Creditos.Agencia.
DEFINE OUTPUT PARAMETER W_Pro LIKE Creditos.Cod_Credito.
DEFINE OUTPUT PARAMETER W_NitW LIKE Creditos.Nit.
DEFINE OUTPUT PARAMETER W_Cue LIKE Creditos.Num_Credito.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME B_Creditos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Creditos

/* Definitions for BROWSE B_Creditos                                    */
&Scoped-define FIELDS-IN-QUERY-B_Creditos Creditos.Agencia Creditos.Cod_Credito Creditos.Num_Credito Creditos.Nit Creditos.Sdo_Capital Creditos.Fec_Desembolso Creditos.Fec_Pago   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Creditos   
&Scoped-define SELF-NAME B_Creditos
&Scoped-define QUERY-STRING-B_Creditos FOR EACH Creditos WHERE     Creditos.Nit EQ W_Nit AND Creditos.Estado EQ 2 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Creditos OPEN QUERY {&SELF-NAME} FOR EACH Creditos WHERE     Creditos.Nit EQ W_Nit AND Creditos.Estado EQ 2 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Creditos Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-B_Creditos Creditos


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-B_Creditos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS B_Creditos Btn_Salir R_Busca W_Busca ~
RECT-282 
&Scoped-Define DISPLAYED-OBJECTS R_Busca W_Busca 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 96" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE W_Busca AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 21 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Busca AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Nit", 1,
"Num.Credito", 2,
"Todos", 3
     SIZE 33 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 57 BY 1.62.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_Creditos FOR 
      Creditos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Creditos wWin _FREEFORM
  QUERY B_Creditos NO-LOCK DISPLAY
      Creditos.Agencia COLUMN-LABEL "Age" FORMAT "999":U 
      Creditos.Cod_Credito COLUMN-LABEL "Pdto" FORMAT "999":U
      Creditos.Num_Credito FORMAT "999999999":U LABEL "Num.Credito"
      Creditos.Nit FORMAT "X(12)":U
      Creditos.Sdo_Capital FORMAT "->>>>>,>>>,>>>,>>9.99":U LABEL "Saldo Capital"
      Creditos.Fec_Desembolso   COLUMN-LABEL "Fec-Desemb"
      Creditos.Fec_Pago         COLUMN-LABEL "Prox.Pago"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH SEPARATORS SIZE 75 BY 4.85
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .5.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     B_Creditos AT ROW 1.27 COL 2
     Btn_Salir AT ROW 6.38 COL 60
     R_Busca AT ROW 6.65 COL 3 NO-LABEL
     W_Busca AT ROW 6.65 COL 35 COLON-ALIGNED NO-LABEL
     RECT-282 AT ROW 6.38 COL 2
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 76.72 BY 7.38
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta de Creditos"
         HEIGHT             = 7.38
         WIDTH              = 76.72
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

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/desktop.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/desktop.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
                                                                        */
/* BROWSE-TAB B_Creditos 1 fMain */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Creditos
/* Query rebuild information for BROWSE B_Creditos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Creditos WHERE
    Creditos.Nit EQ W_Nit AND Creditos.Estado EQ 2 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Creditos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _Query            is NOT OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Consulta de Creditos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Consulta de Creditos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME fMain /* Button 96 */
DO:
  IF AVAIL(Creditos) THEN                 
     ASSIGN W_Age  = Creditos.Agencia
            W_Pro  = Creditos.Cod_Credito
            W_Cue  = Creditos.Num_Credito
            W_NitW = Creditos.Nit.
            
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.                    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Creditos
&Scoped-define SELF-NAME B_Creditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Creditos wWin
ON MOUSE-SELECT-DBLCLICK OF B_Creditos IN FRAME fMain
DO:
  IF AVAIL(Creditos) THEN                 
     ASSIGN W_Age  = Creditos.Agencia
            W_Pro  = Creditos.Cod_Credito
            W_Cue  = Creditos.Num_Credito
            W_NitW = Creditos.Nit.

  APPLY "choose" TO Btn_Salir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME R_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Busca wWin
ON VALUE-CHANGED OF R_Busca IN FRAME fMain
DO:
  ASSIGN R_Busca.

  IF SELF:SCREEN-VALUE EQ "3" THEN DO:
     CLOSE QUERY B_Creditos.
     OPEN QUERY B_Creditos FOR EACH Creditos WHERE Creditos.Estado EQ 2 NO-LOCK INDEXED-REPOSITION.
     APPLY "ENTRY" TO Btn_Salir.
  END.
  ELSE   
    APPLY "ENTRY" TO W_Busca.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Busca
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Busca wWin
ON LEAVE OF W_Busca IN FRAME fMain
DO:
  ASSIGN FRAME {&FRAME-NAME} R_Busca W_Busca.
  
  IF W_Busca GT " " THEN DO:  
     CLOSE QUERY B_Creditos.

     CASE R_Busca:                                                                                          
       WHEN 1 THEN                                                                                          
          OPEN QUERY B_Creditos FOR EACH Creditos WHERE                                                     
              Creditos.Nit EQ W_Busca AND Creditos.Estado EQ 2 NO-LOCK INDEXED-REPOSITION.                  
       WHEN 2 THEN                                                                                          
          OPEN QUERY B_Creditos FOR EACH Creditos WHERE                                                     
              Creditos.Num_Credito EQ int(DECIMAL(W_Busca)) AND Creditos.Estado EQ 2 NO-LOCK INDEXED-REPOSITION. 
     END CASE.                                                                                              
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
  DISPLAY R_Busca W_Busca 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE B_Creditos Btn_Salir R_Busca W_Busca RECT-282 
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

  APPLY "ENTRY" TO R_Busca IN FRAME FMain.
 /* RETURN NO-APPLY.*/

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

