&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME W-Logs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Logs 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

   DEFINE SHARED VAR W_Nom_Agencia    AS   CHARACTER FORMAT "X(60)".
   DEFINE SHARED VAR W_Nom_Entidad    AS   CHARACTER FORMAT "X(60)".
   DEFINE SHARED VAR W_Usuario        LIKE Usuarios.Usuario.
   DEFINE SHARED VAR W_Estacion       LIKE Estaciones.Estacion.
   DEFINE SHARED VAR W_Manija         AS   HANDLE.
   DEFINE SHARED VAR W_PathSpl        LIKE Entidad.Dir_Spl.
   DEFINE VAR W_Rpta                  AS LOGICAL.
   DEFINE VAR W_TamOfi                AS INTEGER INITIAL 0.
   DEFINE VAR W_OfiTra                LIKE Agencias.Agencia INITIAL 0.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-52 Btn_Imprimir W_RConsulta ~
T_Mensajes Btn_Salir Btn_Ayuda IMAGE-5 RECT-221 
&Scoped-Define DISPLAYED-OBJECTS W_RConsulta W_OfiIni W_OfiFin W_FecIni ~
W_FecFin W_UsuIni W_UsuFin T_Mensajes 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 W_OfiIni W_OfiFin W_FecIni W_FecFin W_UsuIni W_UsuFin 
&Scoped-define List-2 W_FecIni W_FecFin 
&Scoped-define List-3 W_UsuIni 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Logs AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda".

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "Imprimir" 
     SIZE 9 BY 1.62 TOOLTIP "Muestra la interface de salida de información (Reportes)"
     FONT 4.

DEFINE BUTTON Btn_Salir DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.62 TOOLTIP "Sale del programa de Consulta de Logs"
     BGCOLOR 8 FONT 4.

DEFINE BUTTON BUTTON-122 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 122" 
     SIZE 9 BY 1.62.

DEFINE BUTTON BUTTON-52 
     IMAGE-UP FILE "imagenes\informacion":U
     LABEL "Button 52" 
     SIZE 9 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y mensajes del usuario activo".

DEFINE VARIABLE W_FecFin AS DATE FORMAT "99/99/9999" 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE W_FecIni AS DATE FORMAT "99/99/9999" 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FONT 4.

DEFINE VARIABLE W_OfiFin AS INTEGER FORMAT ">>9":U INITIAL 999 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_OfiIni AS INTEGER FORMAT ">>9":U INITIAL 0 
     LABEL "Agencia" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_UsuFin AS CHARACTER FORMAT "X(4)":U 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_UsuIni AS CHARACTER FORMAT "X(4)" 
     LABEL "Usuario" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15 FONT 4.

DEFINE IMAGE IMAGE-5
     FILENAME "imagenes\dog":U
     SIZE 5 BY 1.35.

DEFINE VARIABLE W_RConsulta AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "General", 1,
"Personalizada", 2
     SIZE 23 BY .81
     BGCOLOR 17 FONT 4 NO-UNDO.

DEFINE RECTANGLE RECT-221
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 11 BY 5.38.

DEFINE VARIABLE T_Mensajes AS LOGICAL INITIAL no 
     LABEL "Sacar en el reporte solamente los mensajes" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .77
     FONT 5 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-52 AT ROW 2.35 COL 101
     Btn_Imprimir AT ROW 3.96 COL 101 HELP
          "Permite imprimir la auditoria"
     BUTTON-122 AT ROW 5.58 COL 101
     W_RConsulta AT ROW 9.88 COL 45 HELP
          "Seleccione el tipo de consulta" NO-LABEL
     W_OfiIni AT ROW 12.04 COL 38 COLON-ALIGNED HELP
          "Ingrese la Agencia inicial para la Auditoria"
     W_OfiFin AT ROW 12.04 COL 61 COLON-ALIGNED HELP
          "Ingrese la Agencia final para la Auditoría"
     W_FecIni AT ROW 13.12 COL 38 COLON-ALIGNED HELP
          "Ingrese la fecha inicial para la auditoria"
     W_FecFin AT ROW 13.12 COL 61 COLON-ALIGNED HELP
          "Ingrese la fecha final para la auditoria"
     W_UsuIni AT ROW 14.19 COL 38 COLON-ALIGNED HELP
          "Ingrese el usuario inicial para la auditoria"
     W_UsuFin AT ROW 14.19 COL 61 COLON-ALIGNED HELP
          "Ingrese el usuario final para la auditoria"
     T_Mensajes AT ROW 15.54 COL 36
     Btn_Salir AT ROW 18.5 COL 100 HELP
          "Permite salir de la Auditoria"
     Btn_Ayuda AT ROW 20.38 COL 103 HELP
          "Permite obtener ayuda de la pantalla"
     IMAGE-5 AT ROW 8.54 COL 38
     RECT-221 AT ROW 2.08 COL 100
     "Tipo de Consulta a Realizar" VIEW-AS TEXT
          SIZE 25 BY .81 AT ROW 8.81 COL 44
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Rango Inicial" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 11.23 COL 41
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Rango Final" VIEW-AS TEXT
          SIZE 11 BY .81 AT ROW 11.23 COL 64
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 21.27
         BGCOLOR 17 FONT 4.


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
  CREATE WINDOW W-Logs ASSIGN
         HIDDEN             = YES
         TITLE              = "Consulta de Logs"
         HEIGHT             = 21.27
         WIDTH              = 113.14
         MAX-HEIGHT         = 21.27
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.27
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         FONT               = 1
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Logs 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Logs
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
/* SETTINGS FOR BUTTON BUTTON-122 IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecFin IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN W_FecIni IN FRAME F-Main
   NO-ENABLE 1 2                                                        */
/* SETTINGS FOR FILL-IN W_OfiFin IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_OfiIni IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_UsuFin IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN W_UsuIni IN FRAME F-Main
   NO-ENABLE 1 3                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Logs)
THEN W-Logs:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Logs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Logs W-Logs
ON END-ERROR OF W-Logs /* Consulta de Logs */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Logs W-Logs
ON WINDOW-CLOSE OF W-Logs /* Consulta de Logs */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Logs
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "ayudas\utilidad" CONTEXT 4.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-Logs
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Imprimir */
DO:
  DEFINE VAR listado AS CHAR FORMAT "X(60)" INITIAL "".
  ASSIGN FRAME F-MAIN T_Mensajes.
  Listado = W_PathSpl + "L_Logs.Lst".
  {incluido/Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir W-Logs
ON CHOOSE OF Btn_Salir IN FRAME F-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-52
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-52 W-Logs
ON CHOOSE OF BUTTON-52 IN FRAME F-Main /* Button 52 */
DO:
  RUN W-InfDia.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecFin W-Logs
ON LEAVE OF W_FecFin IN FRAME F-Main /* Fecha */
DO:
  ASSIGN W_FecFin.
  IF W_FecFin > TODAY OR W_FecIni > W_FecFin THEN
     ASSIGN W_FecFin = TODAY
            W_FecFin:SCREEN-VALUE = STRING(TODAY,"99/99/9999").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecIni W-Logs
ON LEAVE OF W_FecIni IN FRAME F-Main /* Fecha */
DO:
  ASSIGN W_FecIni.
  IF W_FecIni > TODAY THEN
     ASSIGN W_FecIni = TODAY
            W_FecIni:SCREEN-VALUE = STRING(TODAY,"99/99/9999").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_OfiFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OfiFin W-Logs
ON LEAVE OF W_OfiFin IN FRAME F-Main /* Agencia */
DO:
  ASSIGN W_OfiFin.  
  IF W_OfiFin < W_OfiIni THEN
     ASSIGN W_OfiFin = W_OfiIni
            W_OfiFin:SCREEN-VALUE = STRING(W_OfiIni).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_OfiIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_OfiIni W-Logs
ON LEAVE OF W_OfiIni IN FRAME F-Main /* Agencia */
DO:
  ASSIGN W_OfiIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_RConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_RConsulta W-Logs
ON MOUSE-SELECT-CLICK OF W_RConsulta IN FRAME F-Main
DO:
  APPLY "VALUE-CHANGED" TO W_Rconsulta.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_RConsulta W-Logs
ON VALUE-CHANGED OF W_RConsulta IN FRAME F-Main
OR RETURN OF W_RConsulta DO:
 DO WITH FRAME {&FRAME-NAME}: 
    ASSIGN W_Rconsulta
           W_OfiIni = 0
           W_OfiFin = 999
           W_FecIni = 01/01/1997
           W_FecFin = TODAY
           W_UsuIni = "   0"
           W_UsuFin = "9999".
    DISPLAY W_OfiIni W_OfiFin W_FecIni W_FecFin W_UsuIni W_UsuFin.
    CASE W_Rconsulta:
      WHEN 1 THEN
        DISABLE {&List-1}.
      WHEN 2 THEN       
       DO:
         ENABLE {&List-1} WITH FRAME {&FRAME-NAME}.
         APPLY "ENTRY" TO W_OfiIni.
         RETURN NO-APPLY.
       END.
    END CASE.    
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_UsuFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_UsuFin W-Logs
ON LEAVE OF W_UsuFin IN FRAME F-Main /* Usuario */
DO:
  ASSIGN W_UsuFin.
  IF W_UsuIni > W_UsuFin THEN
     ASSIGN W_UsuFin = W_UsuIni
            W_UsuFin:SCREEN-VALUE = W_UsuIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_UsuIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_UsuIni W-Logs
ON LEAVE OF W_UsuIni IN FRAME F-Main /* Usuario */
OR RETURN OF W_UsuIni DO:
  ASSIGN W_UsuIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Logs 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Logs  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Logs  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Logs  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Logs)
  THEN DELETE WIDGET W-Logs.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Logs  _DEFAULT-ENABLE
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
  DISPLAY W_RConsulta W_OfiIni W_OfiFin W_FecIni W_FecFin W_UsuIni W_UsuFin 
          T_Mensajes 
      WITH FRAME F-Main IN WINDOW W-Logs.
  ENABLE BUTTON-52 Btn_Imprimir W_RConsulta T_Mensajes Btn_Salir Btn_Ayuda 
         IMAGE-5 RECT-221 
      WITH FRAME F-Main IN WINDOW W-Logs.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Logs.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Logs 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Logs 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
------------------------------------------------------------------------------*/
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
  APPLY "VALUE-CHANGED" TO W_RConsulta IN FRAME {&FRAME-NAME}.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Logs 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
  DEFINE FRAME F-Usuarios WITH USE-TEXT FRAME F-Usuarios.
  DEFINE FRAME F-Logs
    Logs.Fecha       AT 2   VIEW-AS TEXT
    HoraEnvio        AT 14
    Logs.Observacion AT 24  VIEW-AS TEXT FORMAT "X(50)"
    FechaLee         AT 76
    HoraLee          AT 88
     SKIP
  WITH DOWN WIDTH 132.

    W_Reporte    = "REPORTE   : CONSULTA DE LOGS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = " FECHA       HR.GRABA  MENSAJE                                             FEC.LEIDO  HR.LEIDO".

    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.
    FOR EACH Logs WHERE W_OfiIni <= Logs.Agencia AND W_OfiFin GE Logs.Agencia AND
                        W_FecIni <= Logs.Fecha   AND W_FecFin GE Logs.Fecha   AND
                        W_UsuIni <= Logs.Usuario AND W_UsuFin GE Logs.Usuario NO-LOCK
                  BREAK BY Logs.Usuario BY Logs.Fecha:
      ASSIGN W_NomUsu = ""
             W_UsuTra = "".
      IF FIRST-OF(Logs.Usuario) THEN
       DO:
         FIND Usuarios WHERE Usuarios.Usuario = Logs.Usuario NO-LOCK NO-ERROR.
         IF AVAILABLE(usuarios) THEN
            ASSIGN W_UsuEncabe = "Agencia : " + STRING(Usuarios.Agencia) + 
                                 " / Usuario : " + TRIM(Usuarios.Usuario) + " - " + Usuarios.nombre.
         ELSE   
            ASSIGN W_UsuEncabe = "Inconsistencia en Usuario : " + Logs.Usuario.
         DISPLAY 
           W_UsuEncabe AT 2
           WITH WIDTH 132 FRAME F-Usuarios USE-TEXT STREAM-IO NO-LABELS.
         ASSIGN W_UsuTra = Logs.Usuario.
       END.
      IF T_Mensajes AND Logs.IdRegistro NE 1 THEN NEXT. 
      IF Logs.IdRegistro EQ 1 THEN
       DO:
         IF Logs.Estado THEN
            ASSIGN FechaLee  = ""
                   HoraLee   = "".
         ELSE
            ASSIGN FechaLee  = STRING(Logs.FechaL)
                   HoraLee   = STRING(Logs.HoraL,"hh:mm am").
         HoraEnvio = STRING(Logs.HoraE,"hh:mm am").
       END.
      ELSE
         ASSIGN HoraEnvio = ""
                HoraLee   = ""
                FechaLee  = "".
      DISPLAY Logs.Fecha  
              HoraEnvio
              Logs.Observacion
              FechaLee
              HoraLee 
              WITH WIDTH 132 FRAME F-Logs USE-TEXT STREAM-IO NO-LABELS.
    END.
    PAGE.
    OUTPUT CLOSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Logs  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Logs 
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

