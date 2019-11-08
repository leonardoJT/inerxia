&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME

&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 

/* oakley */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{incluido\Variable.i "SHARED"}

DEFINE VARIABLE W_Rpta AS LOGICAL.
DEFINE VARIABLE wcr AS CHARACTER FORMAT "X(14)".
DEFINE VARIABLE W_error AS LOGICAL.

DEFINE VAR W_Cbt AS INTEGER INITIAL 18.
DEFINE VAR P_Cuenta AS CHARACTER.
DEFINE VAR P_Nombre AS CHARACTER.
DEFINE VAR P_NatTra AS CHARACTER.
DEFINE VAR P_CtrNat AS LOGICAL.
DEFINE VAR zsuma AS DECIMAL.
DEFINE VAR W_Age AS INTEGER.

DEFINE TEMP-TABLE tmp
    FIELD tage AS INTEGER
    FIELD tpro AS INTEGER
    FIELD tcue AS CHARACTER
    FIELD tnit AS CHARACTER
    FIELD tcuo AS DECIMAL
    FIELD tdoc AS INTEGER
    INDEX IdxNit tnit tcue.

DEFINE TEMP-TABLE TmpMov LIKE Mov_Contable
    INDEX X1 AGENCIA CUENTA DB
    INDEX x2 agencia cuenta cr.



DEFINE VAR TDeb AS DECIMAL FORMAT ">>>,>>>,>>>,>>9". 
DEFINE VAR TCre AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VARIABLE procname AS CHARACTER NO-UNDO.
DEFINE VARIABLE OKpressed AS LOGICAL INITIAL TRUE.

DEFINE VAR W_TX AS CHARACTER FORMAT "X(25)".
DEFINE VAR W_OK AS LOGICAL.

DEFINE VAR W_OKTmp   AS LOGICAL INITIAL YES.
DEFINE VAR zswsalida AS LOGICAL INITIAL NO.
DEFINE TEMP-TABLE tmpincons
    FIELD tdanger AS LOGICAL INITIAL FALSE
    FIELD terror  AS CHARACTER FORMAT "X(80)".


DEFI TEMP-TABLE TempCtas                        
        FIELD Agen   LIKE Ahorros.Agencia                                               
        FIELD Pto    LIKE Ahorros.Cod_Ahorro                    
        FIELD CtaPro LIKE Cuentas.Cuenta                        
        FIELD CtaIng LIKE Cuentas.Cuenta                        
        FIELD CtaLiq LIKE Cuentas.Cuenta                        
        FIELD IntAnt LIKE Cuentas.Cuenta                        
        FIELD IntMor LIKE Cuentas.Cuenta                        
        FIELD DifCoD LIKE Cuentas.Cuenta                        
        FIELD DifCoH LIKE Cuentas.Cuenta                        
        FIELD CtaPol LIKE Cuentas.Cuenta                        
        FIELD CtaHon LIKE Cuentas.Cuenta                        
        FIELD CtaCos LIKE Cuentas.Cuenta                        
        FIELD Oper   LIKE Liqui_Int.Cod_Operacion                       
        FIELD CtaSyA LIKE Cuentas.Cuenta                        
        INDEX x3 agen  pto .

DEFINE VAR ctadev AS CHAR FORMAT "X(14)".
DEFINE VAR agedev AS INT.
DEFINE VAR agedes AS INT.
DEFINE VAR w_doc   LIKE mov_contable.num_documento.
DEFINE VAR w_doc2  LIKE mov_contable.num_documento.
DEFINE VARIABLE registro AS CHARACTER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Clientes

/* Definitions for FRAME F-Main                                         */
&Scoped-define QUERY-STRING-F-Main FOR EACH Clientes SHARE-LOCK
&Scoped-define OPEN-QUERY-F-Main OPEN QUERY F-Main FOR EACH Clientes SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F-Main Clientes
&Scoped-define FIRST-TABLE-IN-QUERY-F-Main Clientes


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-4 wcomenta wdb wNit BUTTON-3 Btn_Done ~
RECT-322 
&Scoped-Define DISPLAYED-OBJECTS wcomenta wdb wNit T-consolidar T-trasage ~
NomCueDB W_NomTitular 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Preliminar 
     LABEL "Revisar comprobante sin contabilizar" 
     SIZE 43 BY 1.12.

DEFINE BUTTON Btn_Conta 
     LABEL "Contabilizar" 
     SIZE 43 BY 1.12.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE BUTTON BUTTON-3 
     LABEL "Leer Archivo Plano" 
     SIZE 43 BY 1.12.

DEFINE BUTTON BUTTON-4 
     LABEL "ImpPrueba" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE NomCueDB AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 42 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE wcomenta AS CHARACTER FORMAT "X(70)":U 
     LABEL "Descripcion" 
     VIEW-AS FILL-IN 
     SIZE 62.14 BY .92
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE wdb AS CHARACTER FORMAT "X(16)":U 
     LABEL "Cta Db" 
     VIEW-AS FILL-IN 
     SIZE 19.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE wNit LIKE Clientes.Nit
     LABEL "Nit Empresa" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 49.72 BY 1.35.

DEFINE VARIABLE T-consolidar AS LOGICAL INITIAL yes 
     LABEL "Consolidar Cuenta Contrapartida" 
     VIEW-AS TOGGLE-BOX
     SIZE 31.86 BY .54 NO-UNDO.

DEFINE VARIABLE T-trasage AS LOGICAL INITIAL no 
     LABEL "Llevar a Dirección General" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .54 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F-Main FOR 
      Clientes SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-4 AT ROW 9.62 COL 61 WIDGET-ID 34
     wcomenta AT ROW 3.77 COL 10.86 COLON-ALIGNED WIDGET-ID 6
     wdb AT ROW 4.88 COL 10.72 COLON-ALIGNED WIDGET-ID 10
     wNit AT ROW 6.88 COL 11.43 COLON-ALIGNED HELP
          "Número documento de identificación" WIDGET-ID 24
          LABEL "Nit Empresa"
          BGCOLOR 15 
     T-consolidar AT ROW 8 COL 13 WIDGET-ID 8
     T-trasage AT ROW 8.77 COL 13 WIDGET-ID 18
     BUTTON-3 AT ROW 10.69 COL 4
     Btn-Preliminar AT ROW 12.12 COL 4
     Btn_Conta AT ROW 13.65 COL 4
     Btn_Done AT ROW 12.85 COL 66
     NomCueDB AT ROW 4.85 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 20
     W_NomTitular AT ROW 6.92 COL 27 COLON-ALIGNED NO-LABEL WIDGET-ID 26
     "    Comprobante Cargue de NóminaxConvenio: (18  )" VIEW-AS TEXT
          SIZE 48.57 BY 1.08 AT ROW 1.81 COL 13
          BGCOLOR 18 FGCOLOR 15 
     "ver 22" VIEW-AS TEXT
          SIZE 6 BY 1.04 AT ROW 1.81 COL 72 WIDGET-ID 32
     RECT-322 AT ROW 1.65 COL 12.29
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 82.29 BY 14.65
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON Btn_Done.


/* *********************** Procedure Settings ************************ */

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
         TITLE              = "Cargue de Archivo NóminaxConvenio"
         HEIGHT             = 14.77
         WIDTH              = 82.43
         MAX-HEIGHT         = 37.5
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 37.5
         VIRTUAL-WIDTH      = 182.86
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
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR BUTTON Btn-Preliminar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Conta IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCueDB IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-consolidar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR TOGGLE-BOX T-trasage IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN wNit IN FRAME F-Main
   LIKE = bdcentral.Clientes.Nit EXP-LABEL EXP-SIZE                     */
/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       W_NomTitular:READ-ONLY IN FRAME F-Main        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _TblList          = "bdcentral.Clientes"
     _Query            is OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* Cargue de Archivo NóminaxConvenio */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* Cargue de Archivo NóminaxConvenio */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Preliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Preliminar W-Win
ON CHOOSE OF Btn-Preliminar IN FRAME F-Main /* Revisar comprobante sin contabilizar */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Usuar.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
 /* {Imprimir.I "listado"}  */
      IF TDeb LE 0  THEN 
      ENABLE Btn_Conta          
          WITH FRAME F-main.
    ELSE
      MESSAGE " Btn_conta: " TDeb VIEW-AS ALERT-BOX BUTTON OK.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Conta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Conta W-Win
ON CHOOSE OF Btn_Conta IN FRAME F-Main /* Contabilizar */
DO:
  IF NOT zswsalida THEN DO:
      RUN _setCurs.r("Wait").
       RUN Generar_Movimiento.
       MESSAGE w_fecha "--" W_Doc2 "----" w_cbt  VIEW-AS ALERT-BOX BUTTON OK.
     RUN _setCurs.r("Arrow").
  END.
  ELSE
     MESSAGE "Corrija Primero las inconsistencias e intente de nuevo!!"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
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
    w_error = FALSE.
  IF WNIT:SCREEN-VALUE IN FRAME f-main = ""  THEN DO:
    MESSAGE "Debe colocar el Nit del Convenio por Nomina"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
    APPLY "Entry" TO wnit.
    RETURN NO-APPLY.
  END.
  MESSAGE "El archivo debe tener : oficina ; producto ; cuenta ; nit ; valor " VIEW-AS ALERT-BOX.
  FOR EACH Tmp: DELETE Tmp. END.
  SYSTEM-DIALOG GET-FILE procname
        TITLE      "Choose Procedure to Run ..."
        FILTERS    "Source Files (*.txt)"   "*.txt",
                   "R-code Files (*.prn)"   "*.prn"
        INITIAL-DIR "C:\info_utrahuilca\"
        MUST-EXIST
        USE-FILENAME
        UPDATE OKpressed.
  RUN _SetCurs.r ("WAIT").     
    IF OKpressed = TRUE THEN
        RUN Generar_Temporal.
    
    IF w_error THEN DO:
        DISABLE Btn-Preliminar WITH FRAME f-main.
        RETURN NO-APPLY.
    END.
  RUN _SetCurs.r ("ARROW").
 END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 W-Win
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* ImpPrueba */
DO:
  RUN impcomp.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wdb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wdb W-Win
ON LEAVE OF wdb IN FRAME F-Main /* Cta Db */
DO:
  ASSIGN FRAME F-main wdb.
  FIND Cuentas WHERE Cuentas.Cuenta EQ wdb NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN DO:
      ASSIGN  NomCueDB = Cuentas.Nombre.
      DISPLAY NomCueDB WITH FRAME F-main.
  END.
  ELSE DO:
    RUN C-Cuentas.r (  OUTPUT P_Cuenta, OUTPUT P_Nombre, OUTPUT P_NatTra,
                       OUTPUT P_CtrNat, INPUT  1).
    IF P_Cuenta NE "" THEN DO:
        ASSIGN SELF:SCREEN-VALUE = P_Cuenta
               NomCueDB:SCREEN-VALUE = P_Nombre.
    END.
  END.
/*  IF wdb <> '246095' THEN  solo afecta contable*/
    IF  wdb <> '273025' THEN
     IF wdb <> '21050505' THEN DO:
      MESSAGE "Solo se aceptan 3 cuentas contables" SKIP
          "273025 - 21050505 " VIEW-AS ALERT-BOX.
      RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME wNit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wNit W-Win
ON LEAVE OF wNit IN FRAME F-Main /* Nit Empresa */
DO:
   ASSIGN WNit
          W_NomTitular:SCREEN-VALUE = "".

   FIND FIRST Clientes WHERE Clientes.Nit         EQ WNit
                         AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.
   IF NOT AVAIL(Clientes) THEN DO:                                                                    
      RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                        OUTPUT WNit, OUTPUT W_NomTitular, OUTPUT W_NomTitular, OUTPUT W_Age).       
                                                                                                    
      FIND FIRST Clientes WHERE Clientes.Nit         EQ WNit
                            AND Clientes.Estado      EQ 1 NO-LOCK NO-ERROR.                            
      IF AVAIL(Clientes) THEN                                                                         
         ASSIGN WNit:SCREEN-VALUE = WNit                                                      
                W_NomTitular:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +   
                                        " " + TRIM(Clientes.Nombre).                                  
      ELSE                                                                                            
         MESSAGE "El Nit debe existir Activo en Clientes." VIEW-AS ALERT-BOX.                                 
   END.                                                                                               
   ELSE DO: 
       ASSIGN W_NomTitular:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +    
                                       " " + TRIM(Clientes.Nombre).                                                                                                             
       IF Clientes.Fec_fallecido NE ? THEN 
          MESSAGE "Este cliente aparece como FALLECIDO fecha: " clientes.Fec_fallecido
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wNit W-Win
ON MOUSE-SELECT-DBLCLICK OF wNit IN FRAME F-Main /* Nit Empresa */
DO:
   RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                     OUTPUT WNit, OUTPUT W_NomTitular, OUTPUT W_NomTitular, OUTPUT W_Age).

   ASSIGN WNit:SCREEN-VALUE  = WNit.
   APPLY "Entry" TO SELF.
   RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wNit W-Win
ON SELECTION OF wNit IN FRAME F-Main /* Nit Empresa */
DO:
  DISABLE Btn-Preliminar Btn_Conta WITH FRAME f_main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */
/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
  /* Al inicio del  programa y una sola vez */
  DO WITH FRAME F-Main:     
     ASSIGN wcomentA:SCREEN-VALUE IN FRAME f-main = "Cargue de NóminaxConvenio - "
            Wdb:SCREEN-VALUE IN FRAME f-main = "273025"
            t-consolidar:SCREEN-VALUE = "yes"
            T-trasage:SCREEN-VALUE    = "no".
     FIND FIRST cuentas WHERE cuentas.cuenta = Wdb:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(cuentas) THEN nomcueDB:SCREEN-VALUE IN FRAME f-main = cuentas.nombre.
     /*FIND FIRST cuentas WHERE cuentas.cuenta = wctatras:SCREEN-VALUE NO-LOCK NO-ERROR. 
     IF AVAILABLE(cuentas) THEN nomctatras:SCREEN-VALUE IN FRAME f-main = cuentas.nombre. */
     ASSIGN wcomenta wdb  wnit t-consolidar t-trasage
            nomcueDB wcr = "21050505".
  END.

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

  {&OPEN-QUERY-F-Main}
  GET FIRST F-Main.
  DISPLAY wcomenta wdb wNit T-consolidar T-trasage NomCueDB W_NomTitular 
      WITH FRAME F-Main IN WINDOW W-Win.
  ENABLE BUTTON-4 wcomenta wdb wNit BUTTON-3 Btn_Done RECT-322 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_Movimiento W-Win 
PROCEDURE Generar_Movimiento :
ASSIGN FRAME F-Main
    wdb
    wcomenta
    wnit.

DEFINE VAR wsumaCR AS DECIMAL.
DEFINE VAR tsuma AS DECIMAL.
DEFINE VAR indi AS INTEGER.
DEFINE VAR ages AS INTEGER.

IF zsuma <= 0 THEN DO:
    MESSAGE "No se puede grabar un movimiento si el débito y el crédito no son iguales" SKIP
            "consulte el informe sin contabilizar!."
        VIEW-AS ALERT-BOX.

    RETURN ERROR.
END.

DEFINE VAR zconta AS LOGICAL.

Grabar:
DO TRANSACTION ON ERROR UNDO Grabar:
    IF w_agencia <> 0 THEN DO:
        FIND FIRST Comprobantes WHERE Comprobantes.Comprobante = W_Cbt
                                  AND Comprobantes.Agencia = w_agencia
                                  AND Comprobantes.Estado = 1 NO-ERROR.
        IF NOT AVAILABLE(comprobantes) THEN DO:
            RELEASE comprobantes.

            MESSAGE "No se encontro comprobante" SKIP(0)
                    "Comprobante" W_Cbt SKIP(0)
                    "Agencia" w_agencia SKIP(0)
                    "Estado" 1
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            RETURN ERROR.
        END.
        ELSE
            ASSIGN Comprobantes.Secuencia = Comprobantes.Secuencia + 1
                   W_Doc2 = Comprobantes.Secuencia.
    END.

    FOR EACH Tmp WHERE Tmp.tage <> 0:
        FIND FIRST ahorros WHERE ahorros.cod_ahorro = tmp.tpro
                             AND ahorros.cue_ahorros = tmp.tcue
                             AND ahorros.agencia = tmp.tage
                             AND ahorros.nit = tmp.tnit
                             AND ahorros.estado = 1
                             AND tmp.tpro <> 16 NO-ERROR.
        IF AVAILABLE(ahorros) THEN DO:
            ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + tmp.Tcuo
                   Ahorros.Fec_UltTransaccion = today.

            FIND FIRST tarjetas WHERE tarjetas.nit = ahorros.nit
                                  AND tarjetas.estado = '01'
                                  AND tarjetas.cue_ahorros = ahorros.cue_ahorros NO-LOCK NO-ERROR.
            IF AVAILABLE tarjetas THEN DO:
                FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.

                CREATE reportarVisionamos.
                ASSIGN reportarVisionamos.fecha = w_fecha
                       reportarVisionamos.agencia = w_agencia
                       reportarVisionamos.nit = ahorros.nit
                       reportarVisionamos.numCuenta = ahorros.cue_ahorros
                       reportarVisionamos.tarjeta = tarjetas.tarjetaDB
                       reportarVisionamos.tipoCuenta = 'AH'
                       reportarVisionamos.tipoDoc = clientes.tipo_identificacion
                       reportarVisionamos.valor = tmp.Tcuo
                       reportarVisionamos.usuario = w_usuario
                       reportarVisionamos.estado = 1.
                       reportarVisionamos.tipoTrans = 'C'.
               
                RELEASE reportarVisionamos.
            END.

            CREATE Mov_Ahorros.
            ASSIGN Mov_Ahorros.Agencia = ahorros.agencia
                   Mov_Ahorros.Age_Destino = Ahorros.agencia
                   Mov_Ahorros.Age_Fuente = w_agencia
                   Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
                   Mov_Ahorros.Cod_Operacion = 010101001
                   Mov_Ahorros.Cpte = W_Cbt
                   Mov_Ahorros.Cue_Ahorros = ahorros.cue_ahorros
                   Mov_Ahorros.Descrip = wcomenta
                   Mov_Ahorros.Fecha = TODAY
                   Mov_Ahorros.Hora = TIME
                   Mov_Ahorros.Nit = Ahorros.nit
                   Mov_Ahorros.Num_Documento = TRIM(STRING(w_Doc2))
                   Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_disponible
                   Mov_Ahorros.Usuario = w_usuario
                   Mov_Ahorros.Val_Efectivo = tmp.tcuo
                   mov_ahorros.Cedula_Trans = Ahorros.nit.

            FIND FIRST cortolargo WHERE cortolargo.cod_producto = tmp.tpro
                                    AND cortolargo.agencia = tmp.tage
                                    AND cortolargo.clase_producto = 1
                                    AND tmp.tpro <> 16 NO-ERROR.
            IF AVAILABLE (cortolargo) THEN DO:
                wcr = cortolargo.cta_NoaAd.

                CREATE Mov_Contable.
                ASSIGN mov_contable.Agencia = tmp.tage
                       mov_contable.Cen_Costos = 999
                       mov_contable.Cuenta = Wcr
                       mov_contable.Comprobante = W_Cbt
                       mov_contable.Fec_Contable = W_Fecha
                       mov_contable.Fec_Grabacion = W_Fecha
                       mov_contable.Num_Documento = w_Doc2
                       mov_contable.destino = w_agencia
                       mov_contable.Doc_Referencia = Tmp.Tcue
                       mov_contable.Nit = tmp.tnit
                       mov_contable.hora = TIME
                       mov_contable.Comentario = wcomenta
                       mov_contable.Usuario = w_usuario
                       mov_contable.Estacion = W_Estacion
                       mov_contable.CR = tmp.tcuo.

                wsumacr = wsumacr + tmp.tcuo.
                zconta = TRUE.

                IF w_agencia <> tmp.tage THEN
                    indi = 99.
            END.
        END.

        IF tmp.tpro <> 16 THEN DO:
            ASSIGN wcr = "24650505".

            CREATE Mov_Contable.
            ASSIGN mov_contable.Agencia = agedev
                   mov_contable.Cen_Costos = 999
                   mov_contable.Cuenta = Wcr
                   mov_contable.Comprobante = W_Cbt
                   mov_contable.Fec_Contable = W_Fecha
                   mov_contable.Fec_Grabacion = W_Fecha
                   mov_contable.Num_Documento = w_Doc2
                   mov_contable.destino = w_agencia
                   mov_contable.Doc_Referencia = Tmp.Tcue
                   mov_contable.Nit = tmp.tnit
                   mov_contable.Comentario = wcomenta
                   mov_contable.Estacion = W_Estacion
                   mov_contable.hora = TIME
                   mov_contable.Usuario = w_usuario
                   mov_contable.CR = tmp.tcuo.

            wsumacr = wsumacr + tmp.tcuo.
            zconta = TRUE.
        END.
    END.

    /* descuento a la empresa */
    IF zconta THEN DO:
        CREATE mov_contable.
        Mov_Contable.Agencia = agedev.
        Mov_Contable.Cen_Costos = 999.
        Mov_Contable.Cuenta = Wdb.
        Mov_Contable.Comprobante = W_Cbt.
        Mov_Contable.Fec_Contable = W_Fecha.
        Mov_Contable.Fec_Grabacion = W_Fecha.
        Mov_Contable.Num_Documento = W_doc2.
        Mov_contable.destino = w_agencia.
        Mov_Contable.Nit = wnit.
        Mov_Contable.Comentario = wcomenta.
        mov_contable.hora = TIME.
        mov_contable.Estacion = W_Estacion.
        Mov_Contable.Usuario = W_Usuario.
        Mov_Contable.DB = wsumacr.

        IF wdb <> "21050505" THEN
            Mov_Contable.Doc_Referencia = STRING(W_doc2).
        ELSE
            Mov_Contable.Doc_Referencia = ctadev.

        FIND FIRST ahorros WHERE ahorros.cod_ahorro = 1
                             AND ahorros.cue_ahorros = ctadev
                             AND ahorros.agencia = agedev
                             AND ahorros.tip_ahorro = 1
                             AND ahorros.estado = 1 NO-ERROR.
        IF AVAILABLE(ahorros) THEN DO:
            ASSIGN Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible - wsumacr
                   Ahorros.Fec_UltTransaccion = today.

            FIND FIRST tarjetas WHERE tarjetas.nit = ahorros.nit
                                  AND tarjetas.estado = '01'
                                  AND tarjetas.cue_ahorros = ahorros.cue_ahorros NO-LOCK NO-ERROR.
            IF AVAILABLE tarjetas THEN DO:
                FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.

                CREATE reportarVisionamos.
                ASSIGN reportarVisionamos.fecha = w_fecha
                       reportarVisionamos.agencia = w_agencia
                       reportarVisionamos.nit = ahorros.nit
                       reportarVisionamos.numCuenta = ahorros.cue_ahorros
                       reportarVisionamos.tarjeta = tarjetas.tarjetaDB
                       reportarVisionamos.tipoCuenta = 'AH'
                       reportarVisionamos.tipoDoc = clientes.tipo_identificacion
                       reportarVisionamos.valor = wsumacr
                       reportarVisionamos.usuario = w_usuario
                       reportarVisionamos.estado = 1.
                       reportarVisionamos.tipoTrans = 'R'.
               
                RELEASE reportarVisionamos.
            END.

            CREATE Mov_Ahorros.
            ASSIGN Mov_Ahorros.Agencia = agedev
                   Mov_Ahorros.Age_Destino = Ahorros.agencia
                   Mov_Ahorros.Age_Fuente = w_agencia
                   Mov_Ahorros.Cod_Ahorro = Ahorros.Cod_Ahorro
                   Mov_Ahorros.Cod_Operacion = 010102001
                   Mov_Ahorros.Cpte = W_Cbt
                   Mov_Ahorros.Cue_Ahorros = ahorros.cue_ahorros
                   Mov_Ahorros.Descrip = wcomenta
                   Mov_Ahorros.Fecha = TODAY
                   Mov_Ahorros.Hora = TIME
                   Mov_Ahorros.Nit = Ahorros.nit
                   Mov_Ahorros.Num_Documento = TRIM(STRING(w_Doc2))
                   Mov_Ahorros.Sdo_Disponible = Ahorros.Sdo_disponible
                   Mov_Ahorros.Usuario = w_usuario
                   Mov_Ahorros.Val_Efectivo = wsumacr
                   mov_ahorros.Cedula_Trans = Ahorros.nit.
        END.
        ELSE DO:
            MESSAGE "No se encontro Cuenta! o no tiene el estado requerido"
                VIEW-AS ALERT-BOX.

            RETURN NO-APPLY.
        END.
    END.

    IF indi = 99 THEN do:
        repeat ages = 1 to 14:
            if ages = w_agencia or ages = 6 THEN
                ages = ages + 1.

            tsuma = 0.

            for each tmp where tmp.tage = ages:
                tsuma = tsuma + tmp.tcuo.
            end.

            if tsuma > 0 then DO:
                CREATE mov_contable.
                ASSIGN mov_contable.Agencia = w_agencia
                       mov_contable.Cuenta = '27059599'
                       mov_contable.Nit = string(ages)
                       mov_contable.Fec_Contable = W_Fecha
                       mov_contable.Comentario = STRING (w_agencia) + "- Sucursales Y Agencias"
                       mov_contable.Usuario = W_Usuario
                       mov_contable.doc_Referencia = wnit
                       mov_contable.Cen_Costos = 999
                       mov_contable.Destino = w_agencia
                       mov_contable.Comprobante = W_Cbt
                       mov_contable.Num_Documento = W_doc2
                       mov_contable.Fec_Grabacion = TODAY
                       mov_contable.Hora = TIME
                       mov_contable.Estacion = W_Estacion
                       mov_contable.cr = tsuma.

                CREATE mov_contable.
                ASSIGN mov_contable.Agencia = ages
                       mov_contable.Cuenta = '27059599'
                       mov_contable.Nit = STRING(W_Agencia)
                       mov_contable.Fec_Contable = W_Fecha
                       mov_contable.Comentario = string(ages) + "- Sucursales Y Agencias"
                       mov_contable.Usuario = W_Usuario
                       mov_contable.doc_Referencia = wnit
                       mov_contable.Cen_Costos = 999
                       mov_contable.Destino = W_Agencia
                       mov_contable.Comprobante = W_Cbt
                       mov_contable.Num_Documento = W_doc2
                       mov_contable.Fec_Grabacion = TODAY
                       mov_contable.Hora = TIME
                       mov_contable.Estacion = W_Estacion
                       mov_contable.Db = tsuma.
            END.
        END.
    END.

    MESSAGE "Contabilización Realizada"
        VIEW-AS ALERT-BOX.

    FIND CURRENT Comprobantes NO-LOCK NO-ERROR.
END.

DISABLE btn_Conta WITH FRAME f-main.
/*DISABLE Btn-Preliminar WITH FRAME f-main.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_Temporal W-Win 
PROCEDURE Generar_Temporal :
DEFINE VAR  wcon_linea   AS INTEGER INITIAL 1.
DEFINE VAR liserror AS CHARACTER INITIAL "".
DEFINE VAR tvalor  AS DECIMAL INITIAL 0.
DEFINE VAR cont AS INT INITIAL 1.

FOR EACH tmpincons:
  DELETE tmpincons.
END.

EMPTY TEMP-TABLE Tempctas.
EMPTY TEMP-TABLE TmpMov.
ASSIGN
    zsuma = 0
    agedev = 0.
OUTPUT TO VALUE ("c:\Info_utrahuilca\liserror.txt").
INPUT FROM VALUE(Procname).
REPEAT:
   CREATE Tmp.
   IMPORT DELIMITER ";" tmp EXCEPT  tdoc.
     /* MESSAGE tmp.Tnit tmp.tcue tmp.tcuo  VIEW-AS ALERT-BOX. */
   IF trim(Tmp.Tnit) = " " OR trim(Tmp.Tcue) = " " THEN DO:
      DELETE Tmp.
      NEXT.
   END.     
   FIND FIRST Clientes WHERE Clientes.Nit EQ TRIM(Tmp.TNit) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE(Clientes) THEN DO:
      CREATE tmpincons.
      ASSIGN tmpincons.Terror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cedula/Nit : " + string(Tmp.Tnit,"X(12)") + " No Encontrada "
             tmpincons.Tdanger =  TRUE
           liserror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Nit : " + string(Tmp.Tnit,"X(12)") + " No Encontrada en clientes"
             agedes = clientes.agencia.
   
      END.    
 /*  MESSAGE tmp.Tnit tmp.tcue tmp.tcuo  VIEW-AS ALERT-BOX. */
 
   FIND FIRST Ahorros  WHERE Ahorros.nit         = Tmp.Tnit AND 
                             Ahorros.agencia     = Tmp.Tage AND
                             Ahorros.cod_ahorro  = tmp.tpro  AND 
                             INT(Ahorros.cue_ahorro)  = INT(tmp.tcue)   AND
                             (Ahorros.estado = 1  OR ahorros.sdo_disponible GT 0)
                             NO-LOCK NO-ERROR.
         IF NOT AVAILABLE(Ahorros) THEN DO:
                FIND FIRST Ahorros  WHERE Ahorros.nit         = Tmp.Tnit AND 
                             Ahorros.agencia     = Tmp.Tage AND
                             Ahorros.tip_ahorro  = 1  AND 
                             (Ahorros.estado = 1  OR ahorros.sdo_disponible GT 0)
                             NO-LOCK NO-ERROR.
                    IF AVAILABLE(ahorros) THEN DO:
                    CREATE tmpincons.
                    ASSIGN tmpincons.Terror  = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cuenta : " + string(Tmp.Tcue,"X(12)") + 
                        " Nit : " + string(Tmp.Tnit,"X(12)") + "poseen otra cuenta diferente"
                    tmpincons.Tdanger =  TRUE
                    liserror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cuenta : " + string(Tmp.Tcue,"X(12)") + " Nit : " + string(Tmp.Tnit,"X(12)") + " Tiene otra cta dif a la registrada".                  
                    DISABLE Btn-Preliminar WITH FRAME f-main. 
                    END.
                    ELSE DO:
                    ASSIGN tmp.tpro = 16
                    tmp.tcue = substring(string(tmp.tnit),1,8).
                    CREATE tmpincons.
                    ASSIGN tmpincons.Terror  = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cuenta : " + string(Tmp.Tcue,"X(12)") + 
                        " Nit : " + string(Tmp.Tnit,"X(12)") + " No Encontrada"
                   /*  tmpincons.Tdanger =  TRUE*/
                    liserror = "Linea " + STRING(wcon_linea,">>>,>>9") + " Cuenta : " + string(Tmp.Tcue,"X(12)") + " Nit : " + string(Tmp.Tnit,"X(12)") + " No Encontrada".                   
                    /* DISABLE Btn-Preliminar WITH FRAME f-main.  */                      
                    END.
        END. 
 ELSE
      ASSIGN Tmp.Tage = ahorros.agencia
             Tmp.Tpro = ahorros.cod_ahorro
             tmp.tcue = ahorros.cue_ahorro
             Tmp.Tnit = ahorros.nit.
          
   IF Tmp.Tcuo LE 0  THEN DO:
      CREATE tmpincons.
      ASSIGN tmpincons.Terror  = "Linea " + STRING(wcon_linea,">>>,>>9") + "Valor menor a cero,  C.C. " + TRIM(Tmp.Tnit)  + "  Valor:" + STRING(Tmp.Tcuo,"->>,>>>,>>9.99")
             tmpincons.Tdanger =  TRUE.
   END.
   
END.
OUTPUT CLOSE.
 DOS SILENT  wordpad c:\info_utrahuilca\listerror.txt.
FOR EACH tmp:
       ASSIGN wcon_linea = wcon_linea + 1
          zsuma = zsuma + Tmp.Tcuo.
END.

IF wcon_linea GT 1 THEN
   ENABLE btn-Preliminar WITH FRAME f-main.
ELSE
   DISABLE Btn-Preliminar WITH FRAME f-main.
IF zsuma LE 0 THEN DO:
   CREATE tmpincons.
   ASSIGN tmpincons.Terror = "Total Nónima Valor No Válido: " + STRING(Tdeb,"->>,>>>,>>>,>>9.99")
          tmpincons.Tdanger =  TRUE.
END.
INPUT CLOSE.

/*CAge/CNit/CCue/CNat/Cval/CCom*/

ASSIGN wcon_linea = 0.
FOR EACH Tmpincons:
  IF tmpincons.Tdanger THEN DO: 
     ASSIGN wcon_linea = wcon_linea + 1
            zswsalida  = TRUE.
  END.
END.
  

/**/
DO WITH FRAME f-main:
IF wdb:SCREEN-VALUE NE ? OR NomCueDB:SCREEN-VALUE NE "" THEN DO:
    IF w_NomTitular:SCREEN-VALUE NE "" THEN DO:
        ASSIGN wdb = TRIM(wdb:SCREEN-VALUE).
        IF wdb = "273025"  THEN DO:
            ASSIGN tvalor = 0
                agedev  = w_agencia.
              FOR EACH Anexos WHERE   anexos.agencia > 0  AND 
                                    anexos.cen_costo = 999 AND   
                                    anexos.cuenta = wdb AND 
                                   anexos.nit = wNit:SCREEN-VALUE  AND 
                                  anexos.ano= YEAR(TODAY) NO-LOCK :
                                REPEAT cont = 1 TO 12 :
                                 tvalor = tvalor + anexos.cr[(cont)] - anexos.db[ (cont) ].
                                 ASSIGN tvalor = tvalor + anexos.sdo_Inicial.
                END.            END.
               
           MESSAGE  zsuma  " > " tvalor  VIEW-AS ALERT-BOX BUTTON OK.
           IF zsuma GT  tvalor THEN DO:
           MESSAGE "No hay fondos suficientes"  zsuma VIEW-AS ALERT-BOX BUTTON OK. 
           w_error = TRUE.
           RETURN NO-APPLY.
           END.
        END.
        ELSE  DO:
           IF wdb = "21050505" THEN DO:
                FIND ahorros WHERE  ahorros.tip_ahorro = 1 AND ahorros.cod_ahorro = 1 AND  ahorros.sdo_disponible GE zsuma
                    AND ahorros.estado = 1 AND ahorros.nit = wnit:SCREEN-VALUE NO-LOCK NO-ERROR.
                IF NOT AVAILABLE(ahorros) THEN DO:
                    MESSAGE "No hay fondos suficientes o no existe cuenta"  zsuma 
                        VIEW-AS ALERT-BOX BUTTON OK. 
                   w_error =TRUE.
                   RETURN.
                END.
                ELSE 
                    ASSIGN ctadev = ahorros.cue_ahorros
                         agedev  = ahorros.agencia.
                /* voy */

            END.
        END.

    END.
    ELSE DO:
        MESSAGE "Favor digite un numero de identificacion existente" VIEW-AS ALERT-BOX INFORMATION.
       w_error =TRUE.
       RETURN.
    END.
END.
    ELSE DO:
        MESSAGE "favor digite la cuenta contable" VIEW-AS ALERT-BOX INFORMATION.
        w_error =TRUE.
        RETURN.
    END.

END.

/**/
IF zswsalida OR w_error THEN DO:
   MESSAGE "no se permite la actualización de datos " SKIP(0)
           "se encontraron " + STRING(wcon_linea,">>>,>>9") " registros con error" zswsalida w_error 
            VIEW-AS ALERT-BOX. /* QUESTION BUTTONS YES-NO
                    TITLE "" UPDATE choice AS LOGICAL. IF CHOICE THEN quit.*/
   DISABLE btn_Conta  WITH FRAME f-main.   
END.
ELSE  DO:
  MESSAGE " Sin Errores! -" STRING(agedev) "--"  VIEW-AS ALERT-BOX BUTTON OK.

     
END.
       
    RUN VALIDACION.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE impcomp W-Win 
PROCEDURE impcomp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*CAge/CNit/CCue/CNat/Cval/CCom*/
    ASSIGN FRAME f-main t-consolidar t-trasage wdb  wnit wcomenta.
    DEFINE VAR WswAge AS LOGICAL INITIAL FALSE.
    DEFINE VAR wmsg   AS CHARACTER FORMAT "X(80)".
    DEFINE VAR IDeb   LIKE Mov_Contable.CR.
    DEFINE VAR ICre   LIKE Mov_Contable.CR.
    DEFINE VAR TIDeb  LIKE Mov_Contable.CR.
    DEFINE VAR TICre  LIKE Mov_Contable.CR.
    DEFINE VAR INom   AS CHARACTER FORMAT "X(15)".
    DEFINE VAR wage   AS CHARACTER FORMAT "X(25)".
    DEFINE VAR fuen AS  CHARACTER FORMAT "x(12)" INITIAL "".
    zsuma = 0.                      
{INCLUIDO\RepEncabezado.I}.    
    IF zswsalida THEN DO:
      W_Reporte    = "REPORTE   : INCONSISTENCIAS IMPORTACION   - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = "DETALLE DEL ERROR ENCONTRADO EN EL ARCHIVO PLANO - VALIDE LA LINEA DEL ARCHIVO PLANO ".
      VIEW FRAME F-Encabezado.
      VIEW FRAME F-Ftr.
      FOR EACH Tmpincons:
         DISPLAY Tmpincons.Terror WITH FRAME F-ERR USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS. 
      END.
      PUT " " SKIP(2).
      DISABLE Btn_Conta WITH FRAME F-main.
      PAGE.
    END.
 /*   ELSE
      ENABLE Btn_Conta WITH FRAME F-main. */

  W_Reporte    = "REPORTE   : PRUEBA CONTABILIZACION NOMINA x CONVENIO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGE CUE-AHORRO       CUENTA         NIT              NOMBRE                         DEBITO         CREDITO".
  wmsg = "Descuadre en Agencias: ".
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  DEFINE VAR WAdb AS DECIMAL INITIAL 0.
  DEFINE VAR WAcr AS DECIMAL INITIAL 0.
  DEFINE VAR wlin AS INTEGER INITIAL 0.

    FOR EACH Mov_Contable WHERE Mov_Contable.Comprobante   EQ 18
                          AND Mov_Contable.Num_Documento EQ  w_doc2
                          AND Mov_Contable.Fec_Contable  EQ TODAY NO-LOCK
                              BREAK BY Mov_Contable.Agencia BY Mov_Contable.Cuenta.
  
    wlin = wlin + 1.
   
    FIND FIRST clientes WHERE clientes.nit = mov_contable.nit NO-LOCK NO-ERROR.
    IF AVAILABLE(clientes) THEN  Inom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE Inom = "No Encontro Nombre".
    ASSIGN ICre = mov_contable.cr  zsuma = zsuma + mov_contable.cr.
    DISPLAY mov_contable.agencia    AT 1   FORMAT "999"
            string(mov_contable.doc_referencia)    AT 5   FORMAT "X(12)"
            mov_contable.Cuenta         AT 22  FORMAT "X(12)"
            mov_contable.Nit    AT 38  FORMAT "X(12)"
            INom        AT 54  FORMAT "X(15)"
            IDeb        AT 76  FORMAT "->>>,>>>,>>9.99"
            mov_contable.cr    AT 92  FORMAT "->>>,>>>,>>9.99"
    WITH FRAME F-mov USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
    IF NOT T-consolidar THEN DO:
       
    END.
  END.
  FIND FIRST agencia WHERE agencias.agencia = mov_contable.agencia NO-LOCK NO-ERROR.
  IF AVAILABLE(agencias) THEN wage = agencias.nombre.
  ELSE wage = " No identificada".
  ASSIGN IdeB = 0.00  Icre = 0.00.

  IF t-consolidar THEN DO:

                Inom = substring(w_nomtitular:SCREEN-VALUE,1,15).
                DISPLAY  w_agencia   AT 1  FORMAT "999"
                 " "        AT 5   FORMAT "X(12)"
                 wdb        AT 22  FORMAT "X(12)"
                 wnit       AT 38  FORMAT "X(12)"
                 INom       AT 54  FORMAT "X(15)"              
                 zsuma      AT 76  FORMAT "->>>,>>>,>>9.99"
                 Icre       AT 92  FORMAT "->>>,>>>,>>9.99"
         WITH FRAME F-mov4 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
   
  END.
  WAcr = WAdb.

  DISPLAY "Total Nomina : "  AT 10
                "-------------------------------------------" AT 70
                WAdb         AT 70  FORMAT "->>,>>>,>>>,>>9.99" 
                WAcr         AT 92 FORMAT "->>,>>>,>>>,>>9.99" 
        WITH FRAME F-Totage USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
        PUT " " SKIP(2).
  
  PAGE.

  IF zsuma = 0 THEN btn_Conta:VISIBLE = TRUE.
  IF wswage THEN DO: 
     DISABLE Btn_Conta WITH FRAME F-main.
     MESSAGE wmsg
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_NumCbt W-Win 
PROCEDURE Llenar_NumCbt :
/*
FIND Comprobantes WHERE 
     Comprobantes.Agencia EQ Tmp.Cage AND
     Comprobantes.Comprobante EQ W_Cbt NO-LOCK NO-ERROR.
IF AVAILABLE Comprobantes THEN DO:
   IF Comprobantes.Id_Consecutivo LT 3 THEN DO:
       W_Doc:SCREEN-VALUE = "00000000".
       DISABLE W_Doc WITH FRAME F-Main.
   END.
   ELSE 
      ENABLE W_Doc WITH FRAME F-Main.
END. */
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Win 
PROCEDURE local-initialize :
/*ASSIGN Cmb_Agencia:LIST-ITEMS IN FRAME {&FRAME-NAME} = "".
  FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK
                       BY Agencia.Agencia:
      ASSIGN W_TX = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre
             W_OK = Cmb_Agencia:ADD-LAST(W_TX).
      IF W_Agencia EQ Agencias.Agencia THEN
         Cmb_Agencia:SCREEN-VALUE = STRING(Agencias.Agencia, "999") + "-" + Agencias.Nombre.
  END. 

  RUN Llenar_NumCBT.
*/
  DISABLE Btn_Conta WITH FRAME F-main.
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  APPLY "Leave" TO BUTTON-3 IN FRAME f-main.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Win 
PROCEDURE ProcesoImprimir :
/*CAge/CNit/CCue/CNat/Cval/CCom*/
    ASSIGN FRAME f-main t-consolidar t-trasage wdb  wnit wcomenta.
    DEFINE VAR WswAge AS LOGICAL INITIAL FALSE.
    DEFINE VAR wmsg   AS CHARACTER FORMAT "X(80)".
    DEFINE VAR IDeb   LIKE Mov_Contable.CR.
    DEFINE VAR ICre   LIKE Mov_Contable.CR.
    DEFINE VAR TIDeb  LIKE Mov_Contable.CR.
    DEFINE VAR TICre  LIKE Mov_Contable.CR.
    DEFINE VAR INom   AS CHARACTER FORMAT "X(15)".
    DEFINE VAR wage   AS CHARACTER FORMAT "X(25)".
    DEFINE VAR fuen AS  CHARACTER FORMAT "x(12)" INITIAL "".
    zsuma = 0.                      
{INCLUIDO\RepEncabezado.I}.    
    IF zswsalida THEN DO:
      W_Reporte    = "REPORTE   : INCONSISTENCIAS IMPORTACION   - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
      W_EncColumna = "DETALLE DEL ERROR ENCONTRADO EN EL ARCHIVO PLANO - VALIDE LA LINEA DEL ARCHIVO PLANO ".
      VIEW FRAME F-Encabezado.
      VIEW FRAME F-Ftr.
      FOR EACH Tmpincons:
         DISPLAY Tmpincons.Terror WITH FRAME F-ERR USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS. 
      END.
      PUT " " SKIP(2).
      DISABLE Btn_Conta WITH FRAME F-main.
      PAGE.
    END.
 /*   ELSE
      ENABLE Btn_Conta WITH FRAME F-main. */

  W_Reporte    = "REPORTE   : PRUEBA CONTABILIZACION NOMINA x CONVENIO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGE CUE-AHORRO       CUENTA         NIT              NOMBRE                         DEBITO         CREDITO".
  wmsg = "Descuadre en Agencias: ".
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  DEFINE VAR WAdb AS DECIMAL INITIAL 0.
  DEFINE VAR WAcr AS DECIMAL INITIAL 0.
  DEFINE VAR wlin AS INTEGER INITIAL 0.
  FOR EACH Tmp WHERE tmp.tnit NE "" BREAK BY Tmp.Tage:
    wlin = wlin + 1.
    IF Tmp.Tcuo NE 0 THEN
       ASSIGN WAdb  = WAdb + Tmp.Tcuo   IDeb = 0  ICre = 0.

    /* por incluir */
      FIND FIRST cortolargo  WHERE cortolargo.cod_producto = tmp.tpro AND cortolargo.agencia = tmp.tage  AND tmp.tpro NE 16 NO-ERROR. 
        IF AVAILABLE (cortolargo) THEN 
            ASSIGN wcr = cortolargo.cta_NoaAd.
        ELSE 
            IF tmp.tpro EQ 16 AND w_agencia = tmp.tage THEN
            ASSIGN wcr = "24650505".
      
        FIND FIRST cortolargo  WHERE cortolargo.cod_producto = tmp.tpro AND cortolargo.agencia = w_agencia AND w_agencia <> tmp.tage 
            AND tmp.tpro NE 16 NO-ERROR. 
        IF AVAILABLE (cortolargo) THEN 
            ASSIGN fuen = cortolargo.cta_SYA.
        ELSE
            IF tmp.tpro EQ 16 AND w_agencia <> tmp.tage THEN
               ASSIGN wcr = "24650505".

    FIND FIRST clientes WHERE clientes.nit = Tmp.Tnit NO-LOCK NO-ERROR.
    IF AVAILABLE(clientes) THEN  Inom = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    ELSE Inom = "No Encontro Nombre".
    ASSIGN ICre = Tmp.Tcuo  zsuma = zsuma + Tmp.Tcuo.
    DISPLAY Tmp.Tage    AT 1   FORMAT "999"
            string(tmp.tpro) + "-" + Tmp.TCue    AT 5   FORMAT "X(12)"
            wCR         AT 22  FORMAT "X(12)"
            Tmp.TNit    AT 38  FORMAT "X(12)"
            INom        AT 54  FORMAT "X(15)"
            IDeb        AT 76  FORMAT "->>>,>>>,>>9.99"
            Tmp.Tcuo    AT 92  FORMAT "->>>,>>>,>>9.99"
    WITH FRAME F-mov USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
    IF NOT T-consolidar THEN DO:
       /*  Inom = substring(w_nomtitular:SCREEN-VALUE,1,15).
       IF NOT T-trasage THEN 
           DISPLAY Tmp.Tage     AT 1   FORMAT "999"
                   " "          AT 5   FORMAT "X(12)"
                   wDB          AT 22  FORMAT "X(12)"
                   wnit         AT 38  FORMAT "X(12)"
                   Inom         AT 54  FORMAT "X(15)"
                   Tmp.Tcuo     AT 76  FORMAT "->>>,>>>,>>9.99"
                   0.00         AT 92  FORMAT "->>>,>>>,>>9.99"
            WITH FRAME F-mov2 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
        ELSE DO:
            FIND FIRST agencias WHERE agencias.agencia = tmp.tage NO-LOCK NO-ERROR.
            IF AVAILABLE(agencias) THEN  Inom = agencias.nombre.
            ELSE Inom = "No Encontro Nombre".
             IF w_agencia <> tmp.tage THEN DO:
            DISPLAY Tmp.Tage    AT 1   FORMAT "999"
                    " "         AT 5   FORMAT "X(12)"
                    wctatras    AT 22  FORMAT "X(12)"
                    w_agencia   AT 38  FORMAT "999"
                    INom        AT 54  FORMAT "X(15)"
                    Tmp.Tcuo    AT 76  FORMAT "->>>,>>>,>>9.99"
                    0.00        AT 92  FORMAT "->>>,>>>,>>9.99"
            WITH FRAME F-mov12 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
            
        
            FIND FIRST agencias WHERE agencias.agencia = tmp.tage NO-LOCK NO-ERROR.
            IF AVAILABLE(agencias) THEN  Inom = agencias.nombre.
            ELSE Inom = "No Encontro Nombre".
            DISPLAY tmp.tage    AT 1   FORMAT "999"
                     " "        AT 5   FORMAT "X(12)"
                     wctatras   AT 22  FORMAT "X(12)"
                    w_agencia   AT 38  FORMAT "999"
                     INom       AT 54  FORMAT "X(15)"
                     0.00       AT 76  FORMAT "->>>,>>>,>>9.99"
                     Tmp.Tcuo   AT 92  FORMAT "->>>,>>>,>>9.99"
             WITH FRAME F-mov13 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
             ASSIGN WAdb  = WAdb + Tmp.Tcuo   IDeb = 0  ICre = 0
                    zsuma = zsuma + Tmp.Tcuo.
             Inom = substring(w_nomtitular:SCREEN-VALUE,1,15).
             DISPLAY tmp.tage   AT 1   FORMAT "999"
                      " "       AT 5   FORMAT "X(12)"
                      wdb       AT 22  FORMAT "X(12)"
                      wnit      AT 38  FORMAT "X(12)"
                      INom      AT 54  FORMAT "X(15)"
                      Tmp.Tcuo  AT 76  FORMAT "->>>,>>>,>>9.99"
                      0.00      AT 92  FORMAT "->>>,>>>,>>9.99"
             WITH FRAME F-mov14 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
            END.

        END. */
    END.
  END.
  FIND FIRST agencia WHERE agencias.agencia = Tmp.Tage NO-LOCK NO-ERROR.
  IF AVAILABLE(agencias) THEN wage = agencias.nombre.
  ELSE wage = " No identificada".
  ASSIGN IdeB = 0.00  Icre = 0.00.

  IF t-consolidar THEN DO:
     /* IF T-trasage THEN DO: 
            FIND FIRST agencias WHERE agencias.agencia = w_agencia NO-LOCK NO-ERROR.
            IF AVAILABLE(agencias) THEN  Inom = agencias.nombre.
         ELSE Inom = "No Encontro Nombre".
           
              IF w_agencia <> tmp.tage THEN DO:
           DISPLAY w_agencia    AT 1   FORMAT "999"
                 " "            AT 5   FORMAT "X(12)"
                 fuen           AT 22  FORMAT "X(12)"
                 " "            AT 38  FORMAT "X(12)"
                 Inom           AT 54  FORMAT "X(15)"
                 zsuma          AT 76  FORMAT "->>>,>>>,>>9.99"
                 Icre           AT 92  FORMAT "->>>,>>>,>>9.99"
         WITH FRAME F-mov2 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.


                DISPLAY tmp.tage   AT 1  FORMAT "999"
                 " "         AT 5   FORMAT "X(12)"
                 wctatras    AT 22  FORMAT "X(12)"
                /* STRING(Tmp.Tage,"999") AT 38  FORMAT "X(12)" */
                 ""          AT 38  FORMAT "X(12)"
                 wage        AT 54  FORMAT "X(15)"
                 Ideb        AT 76  FORMAT "->>>,>>>,>>9.99"
                 zsuma       AT 92 FORMAT "->>>,>>>,>>9.99"
                WITH FRAME F-mov3 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
                ASSIGN WAdb  = WAdb + zsuma. 
         */
                Inom = substring(w_nomtitular:SCREEN-VALUE,1,15).
                DISPLAY  w_agencia   AT 1  FORMAT "999"
                 " "        AT 5   FORMAT "X(12)"
                 wdb        AT 22  FORMAT "X(12)"
                 wnit       AT 38  FORMAT "X(12)"
                 INom       AT 54  FORMAT "X(15)"              
                 zsuma      AT 76  FORMAT "->>>,>>>,>>9.99"
                 Icre       AT 92  FORMAT "->>>,>>>,>>9.99"
         WITH FRAME F-mov4 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
    /*  END.
   ELSE DO: 
         Inom = substring(w_nomtitular:SCREEN-VALUE,1,15).
         DISPLAY Tmp.Tage     AT 1   FORMAT "999"
                 " "          AT 5   FORMAT "X(12)"
                 wDB          AT 22  FORMAT "X(12)"
                 wnit         AT 38  FORMAT "X(12)"
                 Inom         AT 54  FORMAT "X(15)"
                 zsuma        AT 76  FORMAT "->>>,>>>,>>9.99"
                 Icre         AT 92  FORMAT "->>>,>>>,>>9.99"
         WITH FRAME F-mov6 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
      END. */
  END.
  WAcr = WAdb.

  DISPLAY "Total Nomina : "  AT 10
                "-------------------------------------------" AT 70
                WAdb         AT 70  FORMAT "->>,>>>,>>>,>>9.99" 
                WAcr         AT 92 FORMAT "->>,>>>,>>>,>>9.99" 
        WITH FRAME F-Totage USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
        PUT " " SKIP(2).
  
  PAGE.

  IF zsuma = 0 THEN btn_Conta:VISIBLE = TRUE.
  IF wswage THEN DO: 
     DISABLE Btn_Conta WITH FRAME F-main.
     MESSAGE wmsg
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rutina_Imprimir W-Win 
PROCEDURE Rutina_Imprimir :
DEFINE INPUT PARAMETER P_CompAct       LIKE Comprobantes.comprobante.
    DEFINE INPUT PARAMETER P_OfiOrigen     LIKE Agencias.Agencia.
    DEFINE INPUT PARAMETER P_DocIni        LIKE Comprobantes.Secuencia.
    DEFINE INPUT PARAMETER P_DocFin        LIKE Comprobantes.Secuencia.
    FIND Comprobantes WHERE Comprobantes.Comprobante EQ P_CompAct 
                        AND Comprobantes.Agencia     EQ P_OfiOrigen
                    NO-LOCK NO-ERROR.        
    IF AVAILABLE Comprobantes THEN DO:
       FIND Formatos WHERE Formatos.Agencia     EQ P_OfiOrigen
                       AND Formatos.Cod_Formato EQ Comprobantes.Cod_formato 
                   NO-LOCK NO-ERROR.
       IF AVAILABLE(Formatos) THEN
          RUN VALUE(Formatos.Nom_Proceso) (INPUT P_CompAct,
                                           INPUT P_DocIni, INPUT P_DocFin,
                                           INPUT P_OfiOrigen).
       ELSE
         RUN MostrarMensaje IN W_Manija (INPUT 345, OUTPUT W_Rpta).
    END.
    ELSE 
      RUN MostrarMensaje IN W_Manija (INPUT 268, OUTPUT W_Rpta).
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

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Clientes"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validacion W-Win 
PROCEDURE validacion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado EQ 1 NO-LOCK                                                            
                          BY Pro_Ahorros.Cod_Ahorro:                                                            
      FOR EACH CortoLargo WHERE CortoLargo.Clase_Producto EQ 1                                                          
                            AND CortoLargo.Cod_Producto   EQ Pro_Ahorros.Cod_Ahorro                                                             
                            AND CortoLargo.Plazo_Inicial  GE 0 NO-LOCK                                                          
               BREAK BY CortoLargo.Agencia BY CortoLargo.Cod_Producto BY CortoLargo.Plazo_Inicial:                                                              
          IF FIRST-OF(CortoLargo.Cod_Producto) THEN DO:                                                         
             FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_AsoAd                                                            
                                  AND Cuentas.Tipo   EQ 2                                                               
                                  AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.                                                             
             IF AVAIL(Cuentas) THEN                                                             
                FIND FIRST Cuentas WHERE Cuentas.Cuenta EQ CortoLargo.Cta_SyA                                                           
                                     AND Cuentas.Tipo   EQ 2                                                            
                                     AND Cuentas.Estado EQ 1 NO-LOCK NO-ERROR.                                                          
                                                                
             IF NOT AVAIL(Cuentas) THEN DO:                                                             
                MESSAGE "En CortoLargo.Cta_AsoAd y CortoLargo.Cta_SyA deben existir Activas en Cuentas..." SKIP                                                         
                        "Para el Pro_Ahorros.Cod_Ahorro : " Pro_Ahorros.Cod_Ahorro   SKIP                                                               
                        "De la Agencia : "                  CortoLargo.Agencia                                                          
                        VIEW-AS ALERT-BOX ERROR.                                                                
                RETURN ERROR.                                                           
             END.                                                               
                                                                
             CREATE TempCtas.                                                           
             ASSIGN TempCtas.Age    = CortoLargo.Agencia                                                                
                    TempCtas.Pto    = CortoLargo.Cod_Producto                                                           
                    TempCtas.CtaPro = CortoLargo.Cta_AsoAd                                                              
                    TempCtas.CtaSyA = CortoLargo.Cta_SyA.                                                               
          END.                                                          
      END.                                                              
  END.                                                          

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

