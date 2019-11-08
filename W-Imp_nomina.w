&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
{Incluido\VARIABLE.I "SHARED"}

DEFINE TEMP-TABLE Tmp
    FIELD CCuenta AS CHARACTER FORMAT "X(10)"
    FIELD DocRef AS CHARACTER FORMAT "X(6)"
    FIELD CNit AS CHARACTER FORMAT "X(11)"
    FIELD Coment AS CHARACTER FORMAT "X(28)"
    FIELD Natural AS CHARACTER FORMAT "X"
    FIELD Cvalordb AS DECIMAL FORMAT "9999999999999999.99"
    FIELD Cvalorcr AS DECIMAL FORMAT "9999999999999999.99".

DEFINE VAR TDeb AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TCre AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VARIABLE procname AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_Doc BUTTON-3 Btn_Done 
&Scoped-Define DISPLAYED-OBJECTS W_Doc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Done DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-3 
     LABEL "Leer Archivo Plano" 
     SIZE 31 BY 1.12.

DEFINE VARIABLE W_Doc AS INTEGER FORMAT "99999999":U INITIAL 0 
     LABEL "Número de documento a contabilizar" 
     VIEW-AS FILL-IN 
     SIZE 11 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_Doc AT ROW 1.54 COL 40 COLON-ALIGNED
     BUTTON-3 AT ROW 9.35 COL 5
     Btn_Done AT ROW 9.35 COL 44
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 80 BY 17
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON Btn_Done.


/* *********************** Procedure Settings ************************ */

/* oakley */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Importar Archivo plano de Nómina"
         HEIGHT             = 9.88
         WIDTH              = 63
         MAX-HEIGHT         = 17
         MAX-WIDTH          = 80
         VIRTUAL-HEIGHT     = 17
         VIRTUAL-WIDTH      = 80
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Importar Archivo plano de Nómina */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Importar Archivo plano de Nómina */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done W-Win
ON CHOOSE OF Btn_Done IN FRAME F-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 W-Win
ON CHOOSE OF BUTTON-3 IN FRAME F-Main /* Leer Archivo Plano */
DO:
  ASSIGN FRAME F-MAIN W_Doc.
  
  SYSTEM-DIALOG GET-FILE procname
        TITLE      "Choose Procedure to Run ..."
        FILTERS    "Source Files (*.txt)"   "*.txt",
                   "R-code Files (*.prn)"   "*.prn"
        INITIAL-DIR "C:\sicobel\"
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
  RUN _SetCurs.r .     
    IF OKpressed = TRUE THEN
        RUN Generar_Movimiento.
 RUN _SetCurs.r .
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  DISPLAY W_Doc 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE W_Doc BUTTON-3 Btn_Done 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_Movimiento W-Win 
PROCEDURE Generar_Movimiento :
INPUT FROM VALUE(Procname).
REPEAT:
   CREATE Tmp.
   IMPORT DELIMITER "," tmp.
END.
INPUT CLOSE.

FOR EACH Tmp WHERE Tmp.CCuenta NE "":
  FIND Clientes WHERE clientes.Nit EQ TRIM(Tmp.CNit) NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Terceros) THEN
   DO:
     MESSAGE "El Nit: " Tmp.Cnit "No existe. Rectifique!" VIEW-AS ALERT-BOX.
   END.
  FIND Cuentas WHERE Cuentas.Cuenta EQ Tmp.CCuenta NO-LOCK NO-ERROR.
  IF NOT AVAILABLE(Cuentas) THEN DO:
     MESSAGE "La Cuenta: " Tmp.CCuenta " No Existe"
              SKIP(1)
              "Desea Cancelar el Proceso?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "" UPDATE choice AS LOGICAL.
     IF CHOICE THEN quit.
  END.
  IF Tmp.Natural EQ "1" THEN
     TDeb = TDeb + Tmp.CValordb.
  ELSE
     TCre = TCre + Tmp.CValorcr.
END.

IF TCre NE TDeb THEN DO:
     MESSAGE "Valor Credito: " TCre " Diferente al Debito: " TDeb
              SKIP(1)
              "Desea Cancelar el Proceso?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "" UPDATE choice2 AS LOGICAL.
     IF CHOICE2 THEN quit.
END.

MESSAGE "Valor Credito: " TCre SKIP(1)
        "Valor Debito : " TDeb SKIP(1)
        "Desea Realizar el Movimiento Automatico de Nomina?" 
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
                    TITLE "" UPDATE choice3 AS LOGICAL.
     IF NOT CHOICE3 THEN quit.

FOR EACH Tmp WHERE Tmp.CCuenta NE "":
 CREATE Mov_Contable.
 ASSIGN Mov_Contable.Agencia         = 1
        Mov_Contable.Cen_Costos      = 999
        Mov_Contable.Cuenta          = Tmp.CCuenta
        Mov_Contable.Comprobante     = 3
        Mov_Contable.Fec_Contable    = TODAY /*DATE(STRING(Tmp.Dia) + "/" + STRING(Tmp.Mes) + "/" + STRING(Tmp.Ano))*/
        Mov_Contable.Fec_Grabacion   = TODAY /*DATE(STRING(Tmp.Dia) + "/" + STRING(Tmp.Mes) + "/" + STRING(Tmp.Ano))*/
        Mov_Contable.Num_Documento   = W_Doc
        Mov_Contable.Doc_Referencia  = Tmp.DocRef
        Mov_Contable.Nit             = TRIM(Tmp.CNit)
        Mov_Contable.Comentario      = Tmp.Coment
        Mov_Contable.db           = Tmp.CValordb
        Mov_Contable.cr           = Tmp.CValorcr
        Mov_Contable.Usuario         = "35". 
  /*IF Tmp.Natural EQ "1" THEN
     Mov_Contable.Naturaleza = "DB".
  ELSE
     Mov_Contable.Naturaleza = "CR".*/

END.

MESSAGE "Contabilización Realizada"
    VIEW-AS ALERT-BOX.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

