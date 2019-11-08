&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

{Incluido/Variable.I "SHARED"}

DEFINE VAR W_Ok AS LOGICAL NO-UNDO.
DEFINE VAR W_Cta AS CHARACTER.
DEFINE VAR W_NomPC AS CHARACTER.
DEFINE VAR W_Nat AS CHARACTER.
DEFINE VAR W_Ctr AS CHARACTER.
DEFINE VARIABLE TextoLC AS LONGCHAR   NO-UNDO.
DEF VAR W_Sarlaft AS LOG INIT FALSE.
DEFINE VARIABLE hQuery AS HANDLE NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-MAIN
&Scoped-define BROWSE-NAME brwListas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES cfg_listasSarlaft cfg_contenidolistas

/* Definitions for BROWSE brwListas                                     */
&Scoped-define FIELDS-IN-QUERY-brwListas cfg_listasSarlaft.Lista ~
cfg_listasSarlaft.Tipo cfg_listasSarlaft.Estado cfg_listasSarlaft.Usuario ~
cfg_listasSarlaft.Fecha_modificacion 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwListas 
&Scoped-define QUERY-STRING-brwListas FOR EACH cfg_listasSarlaft SHARE-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwListas OPEN QUERY brwListas FOR EACH cfg_listasSarlaft SHARE-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwListas cfg_listasSarlaft
&Scoped-define FIRST-TABLE-IN-QUERY-brwListas cfg_listasSarlaft


/* Definitions for BROWSE brw_lineas                                    */
&Scoped-define FIELDS-IN-QUERY-brw_lineas cfg_contenidolistas.Linea 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw_lineas cfg_contenidolistas.Linea 
&Scoped-define ENABLED-TABLES-IN-QUERY-brw_lineas cfg_contenidolistas
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-brw_lineas cfg_contenidolistas
&Scoped-define QUERY-STRING-brw_lineas FOR EACH cfg_contenidolistas ~
      WHERE FALSE NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw_lineas OPEN QUERY brw_lineas FOR EACH cfg_contenidolistas ~
      WHERE FALSE NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw_lineas cfg_contenidolistas
&Scoped-define FIRST-TABLE-IN-QUERY-brw_lineas cfg_contenidolistas


/* Definitions for FRAME F-MAIN                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-MAIN ~
    ~{&OPEN-QUERY-brwListas}

/* Definitions for FRAME F_Lista                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Lista ~
    ~{&OPEN-QUERY-brw_lineas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwListas Radio_Activo Nombre_fill ~
Btn_Deshacer Btn-Guardar Btn-Eliminar Tipo_fill Btn-Salvar Btn_Done ~
Btn-Editar RECT-325 
&Scoped-Define DISPLAYED-OBJECTS Radio_Activo Nombre_fill Tipo_fill 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn-Editar 
     IMAGE-UP FILE "imagenes/modify.jpg":U
     LABEL "btnagregar 2" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn-Eliminar 
     IMAGE-UP FILE "D:/SFG/Desarrollo/Obj/imagenes/eliminar.jpg":U
     LABEL "Eliminar" 
     SIZE 8 BY 2.15.

DEFINE BUTTON Btn-Guardar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "" 
     SIZE 8 BY 1.88.

DEFINE BUTTON Btn-Salvar 
     IMAGE-UP FILE "imagenes/agregar.jpg":U
     LABEL "Salvar" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn_Deshacer 
     IMAGE-UP FILE "D:/SFG/Desarrollo/Prog/Imagenes/deshacer.bmp":U
     LABEL "Cancelar" 
     SIZE 8 BY 1.88.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE VARIABLE Tipo_fill AS CHARACTER FORMAT "x(25)" 
     LABEL "Tipo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Restrictiva","RES",
                     "No restrictiva","NORES"
     DROP-DOWN-LIST
     SIZE 42 BY 1 NO-UNDO.

DEFINE VARIABLE Nombre_fill AS CHARACTER FORMAT "X(15)":U 
     LABEL "Nombre de la lista" 
     VIEW-AS FILL-IN 
     SIZE 42 BY 1.08 NO-UNDO.

DEFINE VARIABLE Radio_Activo AS LOGICAL 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activo", yes,
"Inactivo", no
     SIZE 23.14 BY 1.12 NO-UNDO.

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 132.14 BY 20.19.

DEFINE BUTTON Btn_Archivo 
     IMAGE-UP FILE "D:/SFG/Desarrollo/Obj/imagenes/folder06.ico":U
     LABEL "Button 159" 
     SIZE 7 BY 1.35.

DEFINE VARIABLE Estado AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Activo",         1,
                     "Inactivo",         2
     DROP-DOWN-LIST
     SIZE 16 BY .92 NO-UNDO.

DEFINE VARIABLE Archivo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 35.72 BY 1 NO-UNDO.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE FILL-IN-1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 14 BY 1 NO-UNDO.

DEFINE VARIABLE ListaAEditar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Lista" 
      VIEW-AS TEXT 
     SIZE 31.86 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwListas FOR 
      cfg_listasSarlaft SCROLLING.

DEFINE QUERY brw_lineas FOR 
      cfg_contenidolistas SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwListas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwListas C-Win _STRUCTURED
  QUERY brwListas SHARE-LOCK NO-WAIT DISPLAY
      cfg_listasSarlaft.Lista FORMAT "x(30)":U WIDTH 34.43
      cfg_listasSarlaft.Tipo FORMAT "x(8)":U WIDTH 7.86
      cfg_listasSarlaft.Estado FORMAT "->,>>>,>>9":U WIDTH 23.29
      cfg_listasSarlaft.Usuario FORMAT "x(60)":U WIDTH 33.43
      cfg_listasSarlaft.Fecha_modificacion FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 124 BY 15.35 FIT-LAST-COLUMN.

DEFINE BROWSE brw_lineas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw_lineas C-Win _STRUCTURED
  QUERY brw_lineas NO-LOCK DISPLAY
      cfg_contenidolistas.Linea FORMAT "x(500)":U
  ENABLE
      cfg_contenidolistas.Linea
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 121 BY 14.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-MAIN
     brwListas AT ROW 6.92 COL 6 WIDGET-ID 200
     Radio_Activo AT ROW 5 COL 75 NO-LABEL WIDGET-ID 154
     Nombre_fill AT ROW 3.42 COL 29 COLON-ALIGNED WIDGET-ID 148
     Btn_Deshacer AT ROW 7.5 COL 136 WIDGET-ID 146
     Btn-Guardar AT ROW 3.15 COL 136 WIDGET-ID 144
     Btn-Eliminar AT ROW 9.65 COL 136 WIDGET-ID 142
     Tipo_fill AT ROW 5.04 COL 29 COLON-ALIGNED WIDGET-ID 140
     Btn-Salvar AT ROW 3.08 COL 136 WIDGET-ID 42
     Btn_Done AT ROW 23.08 COL 137 WIDGET-ID 38
     Btn-Editar AT ROW 5.27 COL 136 WIDGET-ID 16
     "               Administración de la configuración para listas restrictivas SARLAFT" VIEW-AS TEXT
          SIZE 120.72 BY 1.08 AT ROW 1.46 COL 2 WIDGET-ID 8
          BGCOLOR 3 
     RECT-325 AT ROW 2.88 COL 1.86 WIDGET-ID 14
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 144.72 BY 25.08 WIDGET-ID 100.

DEFINE FRAME F_Lista
     FILL-IN-1 AT ROW 1 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 166
     Btn_Archivo AT ROW 1.85 COL 81 WIDGET-ID 158
     Buscar AT ROW 2 COL 50.29 COLON-ALIGNED WIDGET-ID 164
     Archivo AT ROW 2.08 COL 88 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     Estado AT ROW 2.35 COL 110 COLON-ALIGNED WIDGET-ID 170
     brw_lineas AT ROW 3.69 COL 4 WIDGET-ID 400
     ListaAEditar AT ROW 2.08 COL 7.72 COLON-ALIGNED WIDGET-ID 162
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 2.88
         SIZE 132 BY 20.19
         TITLE "Lista SARLAFT" WIDGET-ID 300.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Configuración"
         HEIGHT             = 24.73
         WIDTH              = 144.72
         MAX-HEIGHT         = 32.35
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 32.35
         VIRTUAL-WIDTH      = 194.86
         SHOW-IN-TASKBAR    = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Lista:FRAME = FRAME F-MAIN:HANDLE.

/* SETTINGS FOR FRAME F-MAIN
   FRAME-NAME Custom                                                    */
/* BROWSE-TAB brwListas 1 F-MAIN */
/* SETTINGS FOR FRAME F_Lista
   NOT-VISIBLE                                                          */
/* BROWSE-TAB brw_lineas Estado F_Lista */
ASSIGN 
       FRAME F_Lista:HIDDEN           = TRUE.

ASSIGN 
       Archivo:READ-ONLY IN FRAME F_Lista        = TRUE.

ASSIGN 
       ListaAEditar:READ-ONLY IN FRAME F_Lista        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwListas
/* Query rebuild information for BROWSE brwListas
     _TblList          = "bdcentral.cfg_listasSarlaft"
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _FldNameList[1]   > bdcentral.cfg_listasSarlaft.Lista
"cfg_listasSarlaft.Lista" ? ? "character" ? ? ? ? ? ? no ? no no "34.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > bdcentral.cfg_listasSarlaft.Tipo
"cfg_listasSarlaft.Tipo" ? ? "character" ? ? ? ? ? ? no ? no no "7.86" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > bdcentral.cfg_listasSarlaft.Estado
"cfg_listasSarlaft.Estado" ? ? "logical" ? ? ? ? ? ? no ? no no "23.29" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > bdcentral.cfg_listasSarlaft.Usuario
"cfg_listasSarlaft.Usuario" ? ? "character" ? ? ? ? ? ? no ? no no "33.43" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   = bdcentral.cfg_listasSarlaft.Fecha_modificacion
     _Query            is OPENED
*/  /* BROWSE brwListas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brw_lineas
/* Query rebuild information for BROWSE brw_lineas
     _TblList          = "bdcentral.cfg_contenidolistas"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "FALSE"
     _FldNameList[1]   > bdcentral.cfg_contenidolistas.Linea
"cfg_contenidolistas.Linea" ? "x(500)" "character" ? ? ? ? ? ? yes ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brw_lineas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Configuración */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Configuración */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Editar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Editar C-Win
ON CHOOSE OF Btn-Editar IN FRAME F-MAIN /* btnagregar 2 */
DO:
  APPLY "Mouse-Select-Click" TO brwListas.
  IF AVAIL(cfg_listasSarlaft) THEN DO:
     ASSIGN Nombre_fill:SCREEN-VALUE = STRING(cfg_listasSarlaft.lista)
            Tipo_fill:SCREEN-VALUE = STRING(cfg_listasSarlaft.tipo)
            Radio_Activo:SCREEN-VALUE = STRING(cfg_listasSarlaft.Estado)
     Btn-Salvar:HIDDEN IN FRAME F-MAIN            = TRUE
     Btn-Guardar:HIDDEN IN FRAME F-MAIN           = FALSE. 
  END.
  ELSE MESSAGE "No existe el registro seleccionado para modificar" SKIP
               VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Eliminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Eliminar C-Win
ON CHOOSE OF Btn-Eliminar IN FRAME F-MAIN /* Eliminar */
DO:
  IF FRAME F_Lista:HIDDEN = TRUE THEN DO:
      APPLY "Mouse-Select-Click" TO brwListas.
      IF AVAIL(cfg_listasSarlaft) THEN DO:
          MESSAGE "¿Desea eliminar la lista seleccionada?" SKIP
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar eliminación"
             UPDATE W_RptaRet AS LOGICAL.
          IF NOT W_RptaRet THEN RETURN. 
          DELETE cfg_listasSarlaft.
          OPEN QUERY brwListas FOR EACH cfg_listasSarlaft SHARE-LOCK INDEXED-REPOSITION.
      END.
  END.
  ELSE DO:
      APPLY "Mouse-Select-Click" TO brw_lineas.
      IF AVAIL(cfg_contenidoListas) THEN DO:
          MESSAGE "¿Desea eliminar la línea seleccionada?" SKIP
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar eliminación"
             UPDATE W_RptaRet2 AS LOGICAL.
          IF NOT W_RptaRet2 THEN RETURN. 
          DELETE cfg_contenidoListas.
          hQuery:QUERY-PREPARE("FOR EACH cfg_contenidoListas NO-LOCK WHERE cfg_contenidoListas.IDLista = " + quoter(STRING(ROWID(cfg_listasSarlaft)))).
          hQuery:QUERY-OPEN().
      END.
      ELSE MESSAGE "No existe el registro seleccionado para eliminarlo" SKIP
               VIEW-AS ALERT-BOX ERROR.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Guardar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Guardar C-Win
ON CHOOSE OF Btn-Guardar IN FRAME F-MAIN
DO:
    IF AVAIL(cfg_listasSarlaft) THEN DO:
        RUN Validar_campos.
        MESSAGE "¿Desea cambiar el registro seleccionado?" SKIP
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmación de cambio"
         UPDATE W_RptaRet AS LOGICAL.
        IF NOT W_RptaRet THEN RETURN.
        ASSIGN cfg_listasSarlaft.lista = STRING(nombre_fill:SCREEN-VALUE)
                cfg_listasSarlaft.tipo = STRING(Tipo_fill:SCREEN-VALUE)
                cfg_listasSarlaft.estado = integer(radio_activo:SCREEN-VALUE)
                cfg_listasSarlaft.fecha_modificacion = TODAY
                cfg_listasSarlaft.usuario = W_Usuario.

       ASSIGN Btn-Guardar:HIDDEN IN FRAME F-MAIN           = TRUE.
       ASSIGN Btn-Salvar:HIDDEN IN FRAME F-MAIN            = FALSE.
       RUN Inicializar_Variables.
       OPEN QUERY brwListas FOR EACH cfg_listasSarlaft SHARE-LOCK INDEXED-REPOSITION.
    END.
    ELSE MESSAGE "No existe el registro seleccionado para modificar" SKIP
               VIEW-AS ALERT-BOX ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Salvar C-Win
ON CHOOSE OF Btn-Salvar IN FRAME F-MAIN /* Salvar */
DO: 
    RUN Validar_campos.
    MESSAGE "¿Desea guardar el registro ingresado?" SKIP
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar registro"
             UPDATE W_RptaRet AS LOGICAL.
    IF NOT W_RptaRet THEN RETURN.
    
    CREATE cfg_listasSarlaft.
   
    ASSIGN cfg_listasSarlaft.lista = STRING(Nombre_fill:SCREEN-VALUE)
           cfg_listasSarlaft.tipo = STRING(Tipo_fill:SCREEN-VALUE)
           cfg_listasSarlaft.estado = INTEGER(Radio_Activo:SCREEN-VALUE)
           cfg_listasSarlaft.fecha_modificacion = TODAY
           cfg_listasSarlaft.usuario = W_Usuario.

    INPUT CLOSE.

    ASSIGN Nombre_fill:SCREEN-VALUE = "".

    OPEN QUERY brwListas FOR EACH cfg_listasSarlaft SHARE-LOCK INDEXED-REPOSITION.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Lista
&Scoped-define SELF-NAME Btn_Archivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Archivo C-Win
ON CHOOSE OF Btn_Archivo IN FRAME F_Lista /* Button 159 */
DO:
  DEFINE VAR Archivo_nombre AS CHARACTER FORMAT "x(80)".
  SYSTEM-DIALOG GET-FILE Archivo_nombre
     TITLE      "Escoja el archivo que contiene la información ..."
     MUST-EXIST
     USE-FILENAME.
     
  IF Archivo_nombre <> "" THEN Archivo:SCREEN-VALUE = Archivo_nombre.
  ELSE RETURN.

  MESSAGE "¿Desea reemplazar el contenido de la lista con los datos del archivo elegido?" SKIP
             VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar carga"
             UPDATE W_RptaRet AS LOGICAL.
     IF NOT W_RptaRet THEN RETURN. 

  DELETE FROM cfg_contenidoListas where cfg_contenidoListas.IDLista = STRING(ROWID(cfg_listasSarlaft)).
  DEFINE VARIABLE cLine AS CHARACTER NO-UNDO.

  INPUT FROM VALUE(Archivo_nombre).

  REPEAT :   
    IMPORT UNFORMAT cLine.   
    CREATE cfg_contenidolistas.
    cfg_contenidolistas.IDLista = string(rowId(cfg_listasSarlaft)).
    cfg_contenidolistas.linea = cLine.
  END.
  hQuery = QUERY brw_lineas:HANDLE.
  hQuery:QUERY-PREPARE("FOR EACH cfg_contenidoListas NO-LOCK WHERE cfg_contenidoListas.IDLista = " + quoter(STRING(ROWID(cfg_listasSarlaft)))).
  hQuery:QUERY-OPEN().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-MAIN
&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer C-Win
ON CHOOSE OF Btn_Deshacer IN FRAME F-MAIN /* Cancelar */
DO:
  RUN Inicializar_Variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME F-MAIN /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Lista
&Scoped-define SELF-NAME Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Buscar C-Win
ON ANY-KEY OF Buscar IN FRAME F_Lista /* Buscar */
DO:
  IF LASTKEY = KEYCODE("GO") OR LASTKEY = KEYCODE("RETURN")
      THEN DO:
        FIND NEXT cfg_contenidoListas WHERE cfg_contenidoListas.linea MATCHES '*' + Buscar:SCREEN-VALUE + '*' AND string(ROWID(cfg_listassarlaft))=cfg_contenidoListas.idlista.
        hQUERY:REPOSITION-TO-ROWID(ROWID(cfg_contenidoListas)).
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FILL-IN-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FILL-IN-1 C-Win
ON ANY-KEY OF FILL-IN-1 IN FRAME F_Lista
DO:
  IF LASTKEY = KEYCODE("GO") OR LASTKEY = KEYCODE("RETURN")
      THEN DO:
        RUN validarUsuarioSarlaft.R (INPUT FILL-IN-1:SCREEN-VALUE, OUTPUT W_Sarlaft) NO-ERROR.
      END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-MAIN
&Scoped-define BROWSE-NAME brwListas
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
    RUN enable_UI.

    brwListas:SENSITIVE = TRUE.
    ASSIGN 
       Btn-Guardar:HIDDEN IN FRAME F-MAIN           = TRUE. 

    DEFINE VAR NombreLista AS CHARACTER INIT "Lista no encontrada".

    ON MOUSE-SELECT-DBLCLICK OF brwListas
    DO:
        IF AVAILABLE cfg_listasSarlaft THEN DO:
            ASSIGN FRAME F_Lista:HIDDEN           = FALSE.
            NombreLista = cfg_listasSarlaft.lista.
            DISABLE Btn-Salvar WITH FRAME F-MAIN.
            DISABLE Btn-Guardar WITH FRAME F-MAIN.
            DISABLE Btn-Editar WITH FRAME F-MAIN.
            ASSIGN ListaAEditar:SCREEN-VALUE = NombreLista.
            hQuery = QUERY brw_lineas:HANDLE.
            hQuery:QUERY-PREPARE("FOR EACH cfg_contenidoListas NO-LOCK WHERE cfg_contenidoListas.IDLista = " + quoter(STRING(ROWID(cfg_listasSarlaft)))).
            hQuery:QUERY-OPEN().
            ASSIGN NombreLista = "Lista no encontrada".
        END.
    END.

    IF NOT THIS-PROCEDURE:PERSISTENT THEN
        WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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
  DISPLAY Radio_Activo Nombre_fill Tipo_fill 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  ENABLE brwListas Radio_Activo Nombre_fill Btn_Deshacer Btn-Guardar 
         Btn-Eliminar Tipo_fill Btn-Salvar Btn_Done Btn-Editar RECT-325 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-MAIN}
  DISPLAY FILL-IN-1 Buscar Archivo Estado ListaAEditar 
      WITH FRAME F_Lista IN WINDOW C-Win.
  ENABLE FILL-IN-1 Btn_Archivo Buscar Archivo Estado brw_lineas ListaAEditar 
      WITH FRAME F_Lista IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Lista}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
VIEW FRAME F-Main.

ASSIGN 
       FRAME F_Lista:HIDDEN           = TRUE.

DO WITH FRAME f-main:
     ASSIGN Nombre_fill:SCREEN-VALUE = ""
           Tipo_fill:SCREEN-VALUE = "RES".
     ASSIGN Btn-Guardar:HIDDEN IN FRAME F-MAIN           = TRUE.
     ASSIGN Btn-Salvar:HIDDEN IN FRAME F-MAIN            = FALSE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_campos C-Win 
PROCEDURE Validar_campos :
DO WITH FRAME f-main:
        IF Nombre_fill:SCREEN-VALUE = "" OR Tipo_fill:SCREEN-VALUE = " " THEN DO:
            MESSAGE "Debe ingresar nombre y tipo de lista."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN ERROR.
        END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

