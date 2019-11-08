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
 {Incluido/Variable.I "SHARED"}
/* Local Variable Definitions ---                                       */

  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR W_EstAnterior AS CHARACTER FORMAT "X(30)".

  DEFINE VAR W_Age  LIKE Ahorros.Agencia.
  DEFINE VAR W_Pro  LIKE Ahorros.Cod_Ahorro.
  DEFINE VAR W_Nit  LIKE Ahorros.Nit.
  DEFINE VAR W_Cue  LIKE Ahorros.Cue_Ahorros.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Cambio

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Ahorros

/* Definitions for FRAME F_Cambio                                       */
&Scoped-define FIELDS-IN-QUERY-F_Cambio Ahorros.Cue_Ahorros Ahorros.Nit 
&Scoped-define QUERY-STRING-F_Cambio FOR EACH Ahorros SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Cambio OPEN QUERY F_Cambio FOR EACH Ahorros SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Cambio Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-F_Cambio Ahorros


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-59 BUTTON-66 Cmb_Estados BUTTON-157 ~
E_Estado BUTTON-63 BUTTON-64 
&Scoped-Define DISPLAYED-FIELDS Ahorros.Cue_Ahorros Ahorros.Nit 
&Scoped-define DISPLAYED-TABLES Ahorros
&Scoped-define FIRST-DISPLAYED-TABLE Ahorros
&Scoped-Define DISPLAYED-OBJECTS W_NomAgencia W_NomProducto NomCliente ~
Cmb_Estados E_Estado 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Grabar 
     LABEL "Cambiar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-157 
     LABEL "Cancelar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-59 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 59" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-63 
     LABEL "Salir" 
     SIZE 11 BY 1.38.

DEFINE BUTTON BUTTON-64 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 64" 
     SIZE 5 BY 1.12.

DEFINE BUTTON BUTTON-66 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 66" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE Cmb_Estados AS CHARACTER FORMAT "X(256)":U 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE E_Estado AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 52 BY 5.38
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE NomCliente AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_NomAgencia AS CHARACTER FORMAT "X(30)":U 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomProducto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Producto" 
     VIEW-AS FILL-IN 
     SIZE 43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F_Cambio FOR 
      Ahorros SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Cambio
     W_NomAgencia AT ROW 1.27 COL 11 COLON-ALIGNED
     BUTTON-59 AT ROW 1.27 COL 60
     W_NomProducto AT ROW 2.35 COL 11 COLON-ALIGNED
     BUTTON-66 AT ROW 2.88 COL 60
     Ahorros.Cue_Ahorros AT ROW 3.42 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     Ahorros.Nit AT ROW 4.5 COL 11 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 13 BY .81
          BGCOLOR 15 
     NomCliente AT ROW 4.5 COL 24 COLON-ALIGNED NO-LABEL
     Btn_Grabar AT ROW 5.31 COL 60
     Cmb_Estados AT ROW 5.58 COL 11 COLON-ALIGNED
     BUTTON-157 AT ROW 6.92 COL 60
     E_Estado AT ROW 7.73 COL 3 NO-LABEL
     BUTTON-63 AT ROW 10.15 COL 60
     BUTTON-64 AT ROW 12.04 COL 63
     "Descripción del Cambio" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 6.92 COL 3
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 72.29 BY 12.38
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
         TITLE              = "Cambio de Estados a las Cuentas de Ahorro"
         HEIGHT             = 12.38
         WIDTH              = 72.29
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
/* SETTINGS FOR FRAME F_Cambio
                                                                        */
/* SETTINGS FOR BUTTON Btn_Grabar IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Cue_Ahorros IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Ahorros.Nit IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCliente IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomAgencia IN FRAME F_Cambio
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomProducto IN FRAME F_Cambio
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cambio
/* Query rebuild information for FRAME F_Cambio
     _TblList          = "bdCentral.Ahorros"
     _Query            is OPENED
*/  /* FRAME F_Cambio */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Cambio de Estados a las Cuentas de Ahorro */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Cambio de Estados a las Cuentas de Ahorro */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar wWin
ON CHOOSE OF Btn_Grabar IN FRAME F_Cambio /* Cambiar */
DO:
  IF E_Estado:SCREEN-VALUE = "" THEN DO:
     MESSAGE "Se debe describir el por que se hace" SKIP
             "el cambio del estado. por razones de" SKIP
             "seguridad y control, Escriba su motivo" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO E_Estado.
     RETURN NO-APPLY.
  END.
  IF Ahorros.Detalle_Estado EQ INTEGER(SUBSTRING(Cmb_Estados:SCREEN-VALUE,1,2)) THEN DO:
     MESSAGE "No se esta efectuando ningun cambio" SKIP
             "de estado, rectifique cambiando el estado" VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Cmb_Estados.
     RETURN NO-APPLY.
  END.

  DISABLE Btn_Grabar WITH FRAME F_Cambio.
  RUN Grabar_HV.
  FIND CURRENT Hoja_Vida NO-LOCK NO-ERROR.

  FIND CURRENT Ahorros NO-ERROR.
  RUN Grabar_Ahorro.
  FIND CURRENT Ahorros NO-LOCK NO-ERROR.

  RUN Inicializar_Pantalla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-157
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-157 wWin
ON CHOOSE OF BUTTON-157 IN FRAME F_Cambio /* Cancelar */
DO:
  RUN Inicializar_Pantalla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-63
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-63 wWin
ON CHOOSE OF BUTTON-63 IN FRAME F_Cambio /* Salir */
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


&Scoped-define SELF-NAME BUTTON-66
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-66 wWin
ON CHOOSE OF BUTTON-66 IN FRAME F_Cambio /* Button 66 */
DO:
  RUN C-Ahorros.r (INPUT "", 
                   OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue). 
  FIND Ahorros WHERE Ahorros.Agencia      EQ W_Age AND
                     Ahorros.Cod_Ahorro   EQ W_Pro AND
                     Ahorros.Cue_Ahorros  EQ W_Cue AND
                     Ahorros.Nit          EQ W_Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Ahorros THEN DO:
     RUN Inicializar_Pantalla.
     ENABLE Cmb_Estados Btn_Grabar WITH FRAME F_Cambio. 
     FIND FIRST Pro_Ahorros WHERE Pro_Ahorros.Cod_ahorro EQ W_Pro NO-LOCK NO-ERROR.
     IF AVAILABLE Pro_Ahorros THEN DO:
        W_NomProducto:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"99") + " - " + Pro_Ahorros.Nom_Producto.
        FIND Agencias WHERE Agencias.Agencia EQ W_Age NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
          W_NomAgencia:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
        FIND Clientes WHERE Clientes.Nit EQ W_Nit NO-LOCK NO-ERROR.
        IF AVAILABLE Clientes THEN
           NomCliente:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
        ELSE
           NomCliente:SCREEN-VALUE = "Cliente No Encontrado".
        ASSIGN Cmb_Estados:SCREEN-VALUE = Cmb_Estados:ENTRY(Ahorros.Detalle_Estado)
               W_EstAnterior = Cmb_Estados:SCREEN-VALUE
               Ahorros.Cue_Ahorros:SCREEN-VALUE = Ahorros.Cue_Ahorros
               Ahorros.Nit:SCREEN-VALUE = Ahorros.Nit
               E_Estado:SCREEN-VALUE = "".
        
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

  {&OPEN-QUERY-F_Cambio}
  GET FIRST F_Cambio.
  DISPLAY W_NomAgencia W_NomProducto NomCliente Cmb_Estados E_Estado 
      WITH FRAME F_Cambio IN WINDOW wWin.
  IF AVAILABLE Ahorros THEN 
    DISPLAY Ahorros.Cue_Ahorros Ahorros.Nit 
      WITH FRAME F_Cambio IN WINDOW wWin.
  ENABLE BUTTON-59 BUTTON-66 Cmb_Estados BUTTON-157 E_Estado BUTTON-63 
         BUTTON-64 
      WITH FRAME F_Cambio IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cambio}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Ahorro wWin 
PROCEDURE Grabar_Ahorro :
IF AVAILABLE Ahorros THEN
   Ahorros.Detalle_Estado = INTEGER(SUBSTRING(Cmb_Estados:SCREEN-VALUE IN FRAME F_Cambio,1,2)).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_HV wWin 
PROCEDURE Grabar_HV :
CREATE Hoja_Vida.
 ASSIGN Hoja_Vida.Tipo       = 21 
        Hoja_Vida.Codigo     = INTEGER(SUBSTRING(Cmb_Estados:SCREEN-VALUE IN FRAME F_Cambio,1,2))  
        Hoja_Vida.DoctoRefer = INTEGER(Ahorros.Cue_Ahorros)
        Hoja_Vida.Nit        = Ahorros.Nit:SCREEN-VALUE
        Hoja_Vida.Usuario    = W_Usuario
        Hoja_Vida.Fec_Grabacion = TODAY
        Hoja_Vida.Hora_Grabacion = TIME
        Hoja_Vida.Observacion = 
          E_Estado:SCREEN-VALUE + " - Estado Anterior: " + W_EstAnterior. 
        Hoja_Vida.Asunto_Cumplido = YES.
        
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Pantalla wWin 
PROCEDURE Inicializar_Pantalla :
ASSIGN W_NomAgencia:SCREEN-VALUE IN FRAME F_Cambio = ""
       W_NomProducto:SCREEN-VALUE = ""
       Ahorros.Cue_Ahorros:SCREEN-VALUE = ""
       Ahorros.Nit:SCREEN-VALUE = ""
       Cmb_Estados:SCREEN-VALUE = Cmb_Estados:ENTRY(1)
       Cmb_Estados:SENSITIVE = NO
       E_Estado:SCREEN-VALUE = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FOR EACH Varios WHERE Varios.Tipo EQ 21 NO-LOCK:
     W_Ok = Cmb_Estados:ADD-LAST(STRING(Varios.Codigo,"99") + " - " + Varios.Descripcion) IN FRAME F_Cambio.
  END.
  RUN SUPER.
  RUN Inicializar_Pantalla.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

