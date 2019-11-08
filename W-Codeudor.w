&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
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
{INCLUIDO\VARIABLE.I "SHARED"}
/* Local Variable Definitions ---                                       */
DEFINE VAR P_Nit      LIKE Clientes.Nit.
DEFINE VAR P_Nombre   LIKE Clientes.Nombre.
DEFINE VAR P_Apellido AS CHARACTER FORMAT "X(25)".
DEFINE VAR P_AgeCli   LIKE Agencias.Agencia.


DEFINE TEMP-TABLE TCRE
    FIELD TCodCre LIKE Creditos.Cod_Credito
    FIELD TNumSol LIKE Creditos.Num_Solicitud
    FIELD TNumCre LIKE Creditos.Num_Credito
    FIELD TPagCre LIKE Creditos.Pagare
    FIELD TSdoCap LIKE Creditos.Sdo_Capital.
    
DEFINE TEMP-TABLE TCod
    FIELD TAgeCod LIKE Agencias.Agencia
    FIELD TNitCod LIKE Clientes.Nit
    FIELD TNomCod AS CHARACTER FORMAT "X(40)".

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
&Scoped-define BROWSE-NAME BCod

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TCod TCre

/* Definitions for BROWSE BCod                                          */
&Scoped-define FIELDS-IN-QUERY-BCod TCod.TNitCod TCod.TNomCod   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BCod   
&Scoped-define SELF-NAME BCod
&Scoped-define QUERY-STRING-BCod FOR EACH TCod
&Scoped-define OPEN-QUERY-BCod OPEN QUERY {&SELF-NAME} FOR EACH TCod.
&Scoped-define TABLES-IN-QUERY-BCod TCod
&Scoped-define FIRST-TABLE-IN-QUERY-BCod TCod


/* Definitions for BROWSE BCre                                          */
&Scoped-define FIELDS-IN-QUERY-BCre Tcre.TNumCre Tcre.TSdoCap   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BCre   
&Scoped-define SELF-NAME BCre
&Scoped-define QUERY-STRING-BCre FOR EACH TCre
&Scoped-define OPEN-QUERY-BCre OPEN QUERY {&SELF-NAME} FOR EACH TCre.
&Scoped-define TABLES-IN-QUERY-BCre TCre
&Scoped-define FIRST-TABLE-IN-QUERY-BCre TCre


/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BCod}~
    ~{&OPEN-QUERY-BCre}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS WNitDeu RTipo BCre BCod Btn_Crear ~
Btn_Inactivar BtnDone RECT-302 
&Scoped-Define DISPLAYED-OBJECTS WNitDeu WNomDeu RTipo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 21 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON Btn_Crear 
     LABEL "Crear Nuevo Codeudor" 
     SIZE 21 BY 1.12.

DEFINE BUTTON Btn_Inactivar 
     LABEL "Inactivar Codeudor" 
     SIZE 21 BY 1.12.

DEFINE VARIABLE WNitDeu AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit del Deudor" 
     VIEW-AS FILL-IN 
     SIZE 16 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomDeu AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS FILL-IN 
     SIZE 55 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RTipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 20 BY .81
     FONT 5 NO-UNDO.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 22 BY 1.35.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BCod FOR 
      TCod SCROLLING.

DEFINE QUERY BCre FOR 
      TCre SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BCod
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BCod wWin _FREEFORM
  QUERY BCod DISPLAY
      TCod.TNitCod
 TCod.TNomCod COLUMN-LABEL "Nombre del Codeudor"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 55 BY 7.27
         BGCOLOR 15 FONT 4 EXPANDABLE.

DEFINE BROWSE BCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BCre wWin _FREEFORM
  QUERY BCre DISPLAY
      Tcre.TNumCre COLUMN-LABEL "Num.Credito"
 Tcre.TSdoCap
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 26 BY 7.27
         BGCOLOR 15 FONT 4 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     WNitDeu AT ROW 1.54 COL 15 COLON-ALIGNED
     WNomDeu AT ROW 1.54 COL 31 COLON-ALIGNED NO-LABEL
     RTipo AT ROW 1.54 COL 92 NO-LABEL
     BCre AT ROW 3.69 COL 6
     BCod AT ROW 3.69 COL 34
     Btn_Crear AT ROW 3.69 COL 92
     Btn_Inactivar AT ROW 5.04 COL 92
     BtnDone AT ROW 9.35 COL 92
     RECT-302 AT ROW 1.27 COL 91
     "CréditosVigentes del Cliente" VIEW-AS TEXT
          SIZE 25 BY .81 AT ROW 2.62 COL 6
          FGCOLOR 7 FONT 5
     "Codeudores del Crédito" VIEW-AS TEXT
          SIZE 21 BY .81 AT ROW 2.62 COL 34
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 10.23
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.


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
         TITLE              = "Mantenimiento de Codeudores"
         HEIGHT             = 10.23
         WIDTH              = 113.72
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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
                                                                        */
/* BROWSE-TAB BCre RTipo fMain */
/* BROWSE-TAB BCod BCre fMain */
/* SETTINGS FOR FILL-IN WNomDeu IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BCod
/* Query rebuild information for BROWSE BCod
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCod.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BCod */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BCre
/* Query rebuild information for BROWSE BCre
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCre.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BCre */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Mantenimiento de Codeudores */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Mantenimiento de Codeudores */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BCre
&Scoped-define SELF-NAME BCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCre wWin
ON VALUE-CHANGED OF BCre IN FRAME fMain
DO:
  RUN ActBrwCod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain /* Salir */
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


&Scoped-define SELF-NAME Btn_Crear
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Crear wWin
ON CHOOSE OF Btn_Crear IN FRAME fMain /* Crear Nuevo Codeudor */
DO:
  ASSIGN FRAME Fmain WNitDeu.
  RUN C-Clientes.R(INPUT 2,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
  RUN CrearCodeudor.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Inactivar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Inactivar wWin
ON CHOOSE OF Btn_Inactivar IN FRAME fMain /* Inactivar Codeudor */
DO:
  ASSIGN FRAME fmain Rtipo WNitDeu.
  FIND Relaciones WHERE 
       Relaciones.Clase_Producto  EQ 2      AND
       Relaciones.Cod_Relacion    EQ 11     AND
       Relaciones.Nit_Relacion    EQ TCod.TNitCod AND
       Relaciones.Estado          EQ RTipo  AND
       INTEGER(Relaciones.Cuenta) EQ INTEGER(TCre.TNumCre) AND
       Relaciones.Nit             EQ WNitDeu NO-ERROR.
  IF AVAILABLE Relaciones THEN DO:
      ASSIGN Relaciones.Estado = 2
             Relaciones.Fec_Inactividad = W_Fecha.
      RUN P-GraLog IN W_Manija (INPUT "GAR: INACTIVA Codeudor: " + TCod.TNitCod + " a Credito:" 
          + STRING(Relaciones.Cuenta) + ", Usuario: " + STRING(W_Usuario)).
  END.
  ELSE MESSAGE "HOLA" TCod.TNitCod:SCREEN-VALUE IN BROWSE BCod 
               WNITDEU 
               TCre.TNumCre:SCREEN-VALUE IN BROWSE BCre.
  RUN ActBrwCod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RTipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RTipo wWin
ON VALUE-CHANGED OF RTipo IN FRAME fMain
DO:
  ASSIGN FRAME fmain Rtipo.
  IF RTipo EQ 2 THEN DISABLE Btn_Crear Btn_Inactivar WITH FRAME fmain.
  ELSE ENABLE Btn_Crear Btn_Inactivar WITH FRAME fmain.
  RUN ActBrwCod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME WNitDeu
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WNitDeu wWin
ON LEAVE OF WNitDeu IN FRAME fMain /* Nit del Deudor */
DO:
  ASSIGN FRAME fmain Rtipo Wnitdeu.
  FOR EACH Tcre: DELETE TCre. END. FOR EACH TCod: DELETE TCod. END.
  FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Clientes OR SELF:SCREEN-VALUE EQ "" THEN DO:
    RUN C-Clientes.R(INPUT 2,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
    ASSIGN WNomDeu:SCREEN-VALUE = P_Nombre + " " + P_Apellido
           SELF:SCREEN-VALUE   = P_Nit.
    FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  ELSE 
    WNomDeu:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
  FOR EACH Creditos WHERE Creditos.Nit EQ Clientes.Nit AND Creditos.Estado EQ 2 NO-LOCK:
      CREATE TCre.
      ASSIGN TCre.TNumCre = Creditos.Num_Credito
             TCre.TNumSol = Creditos.Num_Solicitud
             TCre.TSdoCap = Creditos.Sdo_Capital.
  END.
  OPEN QUERY BCre FOR EACH TCre.
  FIND FIRST TCre NO-ERROR.
  IF AVAILABLE TCre THEN
     RUN ActBrwCod.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BCod
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ActBrwCod wWin 
PROCEDURE ActBrwCod :
FOR EACH TCod: DELETE TCod. END.
FOR EACH Relaciones WHERE 
           Relaciones.Clase_Producto  EQ 2      AND
           Relaciones.Cod_Relacion    EQ 11     AND
           Relaciones.Nit_Relacion    NE ""     AND
           Relaciones.Estado          EQ RTipo  AND
           INTEGER(Relaciones.Cuenta) EQ INTEGER(TCre.TNumCre:SCREEN-VALUE IN BROWSE BCre) AND
           Relaciones.Nit             EQ WNitDeu NO-LOCK:
   FIND Clientes WHERE Clientes.Nit EQ Relaciones.Nit_Relacion NO-LOCK NO-ERROR.
   IF AVAILABLE Clientes THEN DO:
       CREATE TCod.
       ASSIGN TCod.TNitCod = Relaciones.Nit_Relacion
              TCod.TAgeCod = Clientes.Agencia
              TCod.TNitCod = Clientes.Nit
              TCod.TNomCod = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
   END.
END.
  OPEN QUERY BCod FOR EACH TCod.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CrearCodeudor wWin 
PROCEDURE CrearCodeudor :
ASSIGN FRAME Fmain WNitDeu.
  FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN DO:
     IF NOT Clientes.Id_PuedeCodeudar THEN DO:
        MESSAGE "Este Cliente no puede codeudar un crédito" SKIP
                "según su configuración en clientes!" VIEW-AS ALERT-BOX.
        RETURN ERROR.
     END.
     FIND Relaciones WHERE 
          Relaciones.Cod_Relacion    EQ 11 AND
          Relaciones.Clase_Producto  EQ 2 AND
          INTEGER(Relaciones.Cuenta) EQ INTEGER(TCre.TNumCre:SCREEN-VALUE IN BROWSE BCre) AND
          Relaciones.Nit             EQ WNitDeu AND
          Relaciones.Nit_Relacion    EQ P_Nit  AND 
          Relaciones.Estado          EQ 1 NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Relaciones THEN DO:
         IF P_Nit EQ ""  THEN DO:
            MESSAGE "No se puede crear un codeudor con nit en blanco" VIEW-AS ALERT-BOX.
            RETURN ERROR.
         END.
         CREATE Relaciones.
         ASSIGN Relaciones.Nit            = WNitDeu:SCREEN-VALUE
                Relaciones.Nit_Relacion   = P_Nit
                Relaciones.Cod_Relacion   = 11
                Relaciones.Clase_Producto = 2
                Relaciones.Fec_Ingreso    = W_Fecha
                /*Relaciones.Cod_Producto   = TCre.TCodCre*/
                Relaciones.Cuenta         = STRING(TCre.TNumCre:SCREEN-VALUE IN BROWSE BCre)
                Relaciones.Descripcion    = "Creado fuera de proceso"
                Relaciones.Usuario        = W_Usuario.
         RUN P-GraLog IN W_Manija (INPUT "GAR: CREA Codeudor: " + P_Nit + " a Credito:" 
             + STRING(Relaciones.Cuenta) + ", Usuario: " + STRING(W_Usuario)).
     END.
     ELSE DO:
         MESSAGE "Ya existe un codeudor con este nit" SKIP
                 "No se crea el codeudor!" VIEW-AS ALERT-BOX.

     END.
  END.
    
  RUN ActBrwCod.

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
  DISPLAY WNitDeu WNomDeu RTipo 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE WNitDeu RTipo BCre BCod Btn_Crear Btn_Inactivar BtnDone RECT-302 
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

