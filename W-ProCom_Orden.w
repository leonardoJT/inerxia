&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Wwin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{INCLUIDO\VARIABLE.I "SHARED"}
DEFINE INPUT PARAMETER P_NomIns AS CHARACTER FORMAT "X(50)".
DEFINE INPUT PARAMETER P_CodOpe LIKE Operacion.Cod_Operacion.
DEFINE INPUT PARAMETER P_Progra AS INTEGER FORMAT "9".


DEFINE VAR W_Ok AS LOGICAL.

DEFINE VARIABLE W_NumSeq   AS INTEGER FORMAT "9999999".

DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
DEFINE VARIABLE P_TipDoc   LIKE Clientes.Tipo_Identificacion.

DEFINE VAR W_NitPro LIKE Clientes.Nit.
DEFINE VAR W_NomPro AS CHARACTER FORMAT "X(80)".
DEFINE VAR W_TelPro LIKE Clientes.Tel_Residencia.
DEFINE VAR W_DirPro LIKE Clientes.DIR_Residencia.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Orden

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Act_Fijo

/* Definitions for FRAME F_Orden                                        */
&Scoped-define QUERY-STRING-F_Orden FOR EACH Act_Fijo SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Orden OPEN QUERY F_Orden FOR EACH Act_Fijo SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Orden Act_Fijo
&Scoped-define FIRST-TABLE-IN-QUERY-F_Orden Act_Fijo


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Descripcion Btn_pro Btn_Aut Btn_Grabar ~
Btn_Salir 
&Scoped-Define DISPLAYED-OBJECTS Descripcion WProveedor WAutoriza 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR Wwin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Aut 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 2" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Grabar 
     LABEL "Imprimir Orden" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_pro 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 1" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 15 BY 1.62.

DEFINE VARIABLE Descripcion AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL LARGE
     SIZE 88 BY 11.58
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE WAutoriza AS CHARACTER FORMAT "X(80)":U 
     LABEL "Autorizador de la Compra" 
     VIEW-AS FILL-IN 
     SIZE 54 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WProveedor AS CHARACTER FORMAT "X(80)":U 
     LABEL "Proveedor" 
     VIEW-AS FILL-IN 
     SIZE 54 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F_Orden FOR 
      Act_Fijo SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Orden
     Descripcion AT ROW 2.35 COL 5 NO-LABEL
     WProveedor AT ROW 14.46 COL 34 COLON-ALIGNED
     Btn_pro AT ROW 14.46 COL 90
     WAutoriza AT ROW 15.54 COL 34 COLON-ALIGNED
     Btn_Aut AT ROW 15.54 COL 90
     Btn_Grabar AT ROW 17.15 COL 57
     Btn_Salir AT ROW 17.15 COL 73
     "Descripción de la Compra" VIEW-AS TEXT
          SIZE 23 BY .81 AT ROW 1.54 COL 5
          FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 97.57 BY 18.15
         BGCOLOR 17 FONT 4.


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
  CREATE WINDOW Wwin ASSIGN
         HIDDEN             = YES
         TITLE              = "Orden de Compra de Activos Fijos"
         COLUMN             = 1.72
         ROW                = 3.96
         HEIGHT             = 18.15
         WIDTH              = 97.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         ALWAYS-ON-TOP      = yes
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB Wwin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW Wwin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Orden
                                                                        */
/* SETTINGS FOR FILL-IN WAutoriza IN FRAME F_Orden
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WProveedor IN FRAME F_Orden
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
THEN Wwin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Orden
/* Query rebuild information for FRAME F_Orden
     _TblList          = "bdCentral.Act_Fijo"
     _Query            is OPENED
*/  /* FRAME F_Orden */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME Wwin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON END-ERROR OF Wwin /* Orden de Compra de Activos Fijos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Wwin Wwin
ON WINDOW-CLOSE OF Wwin /* Orden de Compra de Activos Fijos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Aut
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aut Wwin
ON CHOOSE OF Btn_Aut IN FRAME F_Orden /* Button 2 */
DO:
 DO WITH FRAME {&FRAME-NAME}:
    RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
    ASSIGN WAutoriza:SCREEN-VALUE    = P_Nit + " - " + TRIM(P_Nombre) + " " + P_Apellido.
    FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-ERROR.
    IF AVAILABLE Clientes THEN DO:
       FIND FIRST Usuarios WHERE Usuarios.Nit EQ Clientes.Nit AND Usuarios.Estado EQ 1 NO-ERROR.
       IF NOT AVAILABLE Usuarios THEN DO:
          MESSAGE "La persona escogida no es usario del sistema" SKIP
                  "las ordenes solo pueden ser autorizadas por" SKIP
                  "por personas inscritas a la cooperativa" VIEW-AS ALERT-BOX INFORMATION.
          WAutoriza:SCREEN-VALUE = "".
       END.
    END.
 END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Grabar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Grabar Wwin
ON CHOOSE OF Btn_Grabar IN FRAME F_Orden /* Imprimir Orden */
DO:
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN FRAME F_Orden WAutoriza WProveedor Descripcion.
  IF Descripcion:SCREEN-VALUE EQ "" THEN DO:
     MESSAGE "No se ha descrito que se ordena comprar" SKIP
             "digite los items en el editor de" SKIP
             "descripción de compra!" VIEW-AS ALERT-BOX WARNING.
     APPLY "entry" TO Descripcion.
     RETURN NO-APPLY.
  END.
  IF WProveedor:SCREEN-VALUE EQ "" THEN DO:
     MESSAGE "Debe escogerse un proveedor para la compra" VIEW-AS ALERT-BOX ERROR.
     APPLY "choose" TO Btn_Pro.
     RETURN NO-APPLY.
  END.
  IF W_DirPro EQ "" OR W_TelPro EQ "" THEN DO:
     MESSAGE "Para poder realizar la orden, el proveedor debe" SKIP
             "tener matriculada una direccion comercial y" SKIP
             "un telefono de ubicación" SKIP(1)
             "Actualize esta información y proceda a imprimir" SKIP
             "la orden de compra" VIEW-AS ALERT-BOX ERROR.
     APPLY "choose" TO Btn_Pro.
     RETURN NO-APPLY.
  END.
  IF WAutoriza:SCREEN-VALUE EQ "" THEN DO:
     MESSAGE "Debe escogerse la persona que autoriza la compra" VIEW-AS ALERT-BOX INFORMATION.
     APPLY "choose" TO Btn_Aut.
     RETURN NO-APPLY.
  END.
    DEFINE VAR Listado AS CHARACTER INITIAL "".
    Listado = W_Pathspl + "Orden.lst".
    {incluido/imprimir.i "Listado"}.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_pro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_pro Wwin
ON CHOOSE OF Btn_pro IN FRAME F_Orden /* Button 1 */
DO:
 DO WITH FRAME {&FRAME-NAME}:
    RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
    ASSIGN WProveedor:SCREEN-VALUE    = P_Nit + " - " + TRIM(P_Nombre) + " " + P_Apellido.
    FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-ERROR.
    IF AVAILABLE Clientes THEN
       ASSIGN W_NitPro = Clientes.Nit
              W_NomPro = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
              W_TelPro = Clientes.Tel_Comercial
              W_DirPro = Clientes.DIR_Comercial.
              
 END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir Wwin
ON CHOOSE OF Btn_Salir IN FRAME F_Orden /* Btn_Salir */
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


&Scoped-define SELF-NAME Descripcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Descripcion Wwin
ON ENTRY OF Descripcion IN FRAME F_Orden
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Descripcion Wwin
ON LEAVE OF Descripcion IN FRAME F_Orden
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Wwin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects Wwin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI Wwin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(Wwin)
  THEN DELETE WIDGET Wwin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI Wwin  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-F_Orden}
  GET FIRST F_Orden.
  DISPLAY Descripcion WProveedor WAutoriza 
      WITH FRAME F_Orden IN WINDOW Wwin.
  ENABLE Descripcion Btn_pro Btn_Aut Btn_Grabar Btn_Salir 
      WITH FRAME F_Orden IN WINDOW Wwin.
  {&OPEN-BROWSERS-IN-QUERY-F_Orden}
  VIEW Wwin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject Wwin 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject Wwin 
PROCEDURE initializeObject :
RUN SUPER.
 
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir Wwin 
PROCEDURE ProcesoImprimir :
DEFINE VAR Linea AS CHARACTER FORMAT "X(86)" INITIAL "".
  DEFINE VAR Raya AS CHARACTER INITIAL "_".
  DEFINE VAR NumOrd AS DECIMAL FORMAT "9999999999".

  NumOrd = NEXT-VALUE(Sec_OrdCompra).
     
    DEFINE FRAME F-Encabezado
     HEADER
      "ORDEN DE COMPRA NUMERO : "             AT 1
      NumOrd                                  AT 30 FORMAT "9999999999" 
      "Pagina: "             AT 68 PAGE-NUMBER FORMAT ">>>9" SKIP(1)
      "Fecha : "             AT 1
      TODAY                  AT 8 FORMAT "99/99/9999" SKIP(1)
     WITH DOWN WIDTH 100 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.

    DEFINE FRAME f-ftr
      HEADER 
      "Firma Autorizador______________________________________" AT 1
      WAutoriza               AT 15 SKIP(1)
      W_Nom_Agencia    AT 28 FORMAT "X(30)"
      "Hora: "         AT 72 STRING(TIME,"HH:MM:SS")
    WITH DOWN WIDTH 100 FRAME f-ftr PAGE-BOTTOM USE-TEXT STREAM-IO. 

    Linea = FILL(Raya,50).
    VIEW FRAME F-Encabezado.
    DISPLAY "Se autoriza la compra de los siguientes items al proveedor:" AT 1 SKIP(1)
            "Nit Proveedor      : " AT 1
            W_NitPro                AT 25
            "Proveedor          : " AT 1
            W_NomPro                AT 25
            "Dir.Comercial      : " AT 1
            W_DirPro                AT 25
            "Tel.Comercial      : " AT 1
            W_TelPro                AT 25 SKIP(5)
    WITH FRAME F_Pro WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.          
    
    DISPLAY Descripcion VIEW-AS EDITOR SIZE 90 BY 4 WITH FRAME FEnc1 NO-LABELS WIDTH 132.
    VIEW FRAME f-ftr.
    PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

