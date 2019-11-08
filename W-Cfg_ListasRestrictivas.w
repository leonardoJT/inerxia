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
DEFINE VAR Archivotxt AS CHARACTER.

DEFINE VAR NombreLista AS CHARACTER INIT "Lista no encontrada".

DEFINE VAR esOficialDeCumplimiento AS LOGICAL.

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
&Scoped-define ENABLED-FIELDS-IN-QUERY-brw_lineas 
&Scoped-define QUERY-STRING-brw_lineas FOR EACH cfg_contenidolistas ~
      WHERE FALSE SHARE-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brw_lineas OPEN QUERY brw_lineas FOR EACH cfg_contenidolistas ~
      WHERE FALSE SHARE-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brw_lineas cfg_contenidolistas
&Scoped-define FIRST-TABLE-IN-QUERY-brw_lineas cfg_contenidolistas


/* Definitions for FRAME F-MAIN                                         */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F-MAIN ~
    ~{&OPEN-QUERY-brwListas}

/* Definitions for FRAME F_Lista                                        */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Lista ~
    ~{&OPEN-QUERY-brw_lineas}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS brwListas Nombre_fill Btn_Deshacer ~
Btn-Guardar Tipo_fill Estado Btn-Salvar Btn_Done Btn-Editar RECT-325 
&Scoped-Define DISPLAYED-OBJECTS Nombre_fill Tipo_fill Estado 

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

DEFINE BUTTON Btn-Guardar 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "" 
     SIZE 8 BY 1.88.

DEFINE BUTTON Btn-Salvar 
     IMAGE-UP FILE "imagenes/agregar.jpg":U
     LABEL "Salvar" 
     SIZE 8 BY 1.92.

DEFINE BUTTON Btn_Deshacer 
     IMAGE-UP FILE "imagenes/deshacer.bmp":U
     LABEL "Cancelar" 
     SIZE 8 BY 1.88.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.92
     BGCOLOR 8 .

DEFINE VARIABLE Estado AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Estado" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "Activo",1,
                     "Inactivo",2
     DROP-DOWN-LIST
     SIZE 26 BY 1 NO-UNDO.

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

DEFINE RECTANGLE RECT-325
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 132.14 BY 20.19.

DEFINE BUTTON Btn_Archivo 
     IMAGE-UP FILE "imagenes/folder06.ico":U
     LABEL "Button 159" 
     SIZE 7 BY 1.35.

DEFINE BUTTON descargar 
     IMAGE-UP FILE "Imagenes/disque.jpg":U
     LABEL "Button 1" 
     SIZE 6 BY 1.38.

DEFINE VARIABLE Archivo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY 1 NO-UNDO.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(256)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 19 BY 1 NO-UNDO.

DEFINE VARIABLE ClientesValidados AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Clientes validados" 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY 1 NO-UNDO.

DEFINE VARIABLE ContadorRegistros AS INTEGER FORMAT "->,>>>,>>9":U INITIAL 0 
     LABEL "Líneas cargadas" 
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
      cfg_listasSarlaft.Estado FORMAT "9":U WIDTH 23.29
      cfg_listasSarlaft.Usuario FORMAT "x(60)":U WIDTH 33.43
      cfg_listasSarlaft.Fecha_modificacion FORMAT "99/99/99":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 124 BY 15.35 FIT-LAST-COLUMN.

DEFINE BROWSE brw_lineas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brw_lineas C-Win _STRUCTURED
  QUERY brw_lineas SHARE-LOCK NO-WAIT DISPLAY
      cfg_contenidolistas.Linea FORMAT "x(500)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SIZE 121 BY 14.81 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-MAIN
     brwListas AT ROW 7.19 COL 5.86 WIDGET-ID 200
     Nombre_fill AT ROW 3.42 COL 29 COLON-ALIGNED WIDGET-ID 148
     Btn_Deshacer AT ROW 7.5 COL 136 WIDGET-ID 146
     Btn-Guardar AT ROW 3.15 COL 136 WIDGET-ID 144
     Tipo_fill AT ROW 5.04 COL 29 COLON-ALIGNED WIDGET-ID 140
     Estado AT ROW 4.23 COL 91 COLON-ALIGNED WIDGET-ID 170
     Btn-Salvar AT ROW 3.08 COL 136 WIDGET-ID 42
     Btn_Done AT ROW 23.35 COL 136 WIDGET-ID 38
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
     ContadorRegistros AT ROW 1.54 COL 107.14 COLON-ALIGNED WIDGET-ID 168
     descargar AT ROW 2.73 COL 77.43 WIDGET-ID 172
     Btn_Archivo AT ROW 2.81 COL 34.72 WIDGET-ID 158
     Archivo AT ROW 3 COL 40 COLON-ALIGNED NO-LABEL WIDGET-ID 152
     ClientesValidados AT ROW 3 COL 107.72 COLON-ALIGNED WIDGET-ID 170
     Buscar AT ROW 3.08 COL 9.57 COLON-ALIGNED WIDGET-ID 164
     brw_lineas AT ROW 4.54 COL 4 WIDGET-ID 400
     ListaAEditar AT ROW 1.85 COL 7.72 COLON-ALIGNED WIDGET-ID 162
     "Descargar lista" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 1.69 COL 73.29 WIDGET-ID 174
     "Cargar lista desde archivo de texto" VIEW-AS TEXT
          SIZE 34 BY 1.08 AT ROW 1.58 COL 36 WIDGET-ID 166
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
/* BROWSE-TAB brw_lineas Buscar F_Lista */
ASSIGN 
       FRAME F_Lista:HIDDEN           = TRUE.

ASSIGN 
       Archivo:READ-ONLY IN FRAME F_Lista        = TRUE.

ASSIGN 
       ClientesValidados:HIDDEN IN FRAME F_Lista           = TRUE
       ClientesValidados:READ-ONLY IN FRAME F_Lista        = TRUE.

ASSIGN 
       ContadorRegistros:HIDDEN IN FRAME F_Lista           = TRUE
       ContadorRegistros:READ-ONLY IN FRAME F_Lista        = TRUE.

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
     _Options          = "SHARE-LOCK INDEXED-REPOSITION"
     _Where[1]         = "FALSE"
     _FldNameList[1]   > bdcentral.cfg_contenidolistas.Linea
"cfg_contenidolistas.Linea" ? "x(500)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
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


&Scoped-define BROWSE-NAME brwListas
&Scoped-define SELF-NAME brwListas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL brwListas C-Win
ON MOUSE-SELECT-DBLCLICK OF brwListas IN FRAME F-MAIN
DO:
    IF AVAILABLE cfg_listasSarlaft THEN DO:
        FRAME F_Lista:HIDDEN = FALSE.
        NombreLista = cfg_listasSarlaft.lista.
        Btn-Editar:HIDDEN IN FRAME F-MAIN = TRUE.
        Btn-Salvar:HIDDEN IN FRAME F-MAIN = TRUE.
        Btn-Guardar:HIDDEN IN FRAME F-MAIN = TRUE.
        ListaAEditar:SCREEN-VALUE = NombreLista.
        hQuery = QUERY brw_lineas:HANDLE.
        hQuery:QUERY-PREPARE("FOR EACH cfg_contenidoListas SHARE-LOCK WHERE cfg_contenidoListas.IDLista = " + quoter(STRING(ROWID(cfg_listasSarlaft)))).
        hQuery:QUERY-OPEN().
        NombreLista = "Lista no encontrada".
    END.
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
            Nombre_fill:READ-ONLY IN FRAME F-MAIN            = TRUE
            Tipo_fill:SCREEN-VALUE = STRING(cfg_listasSarlaft.tipo)
            Estado:SCREEN-VALUE = STRING(cfg_listasSarlaft.Estado)
            Btn-Salvar:HIDDEN IN FRAME F-MAIN            = TRUE
            Btn-Guardar:HIDDEN IN FRAME F-MAIN           = FALSE. 
  END.
  ELSE MESSAGE "No existe el registro seleccionado para modificar" SKIP
               VIEW-AS ALERT-BOX ERROR.
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
                cfg_listasSarlaft.estado = integer(Estado:SCREEN-VALUE)
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
           cfg_listasSarlaft.estado = INTEGER(Estado:SCREEN-VALUE)
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
    DEFINE VAR cLine AS CHARACTER.

    /* Únicamente el Usuario configurado como Oficial de Cumplimiento tiene los privilegios para realizar las cargas de listas restrictivas */
    IF esOficialDeCumplimiento = FALSE THEN DO:
        MESSAGE "Usted no tiene los provilegios para realizar" SKIP
                "esta operación. Por favor, comuníquese con" SKIP
                "el Oficial de Cumplimiento."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RETURN NO-APPLY.
    END.
    /* ----------------------- */

    SYSTEM-DIALOG GET-FILE Archivo_nombre
        TITLE "Escoja el archivo que contiene la información ..."
        MUST-EXIST
        USE-FILENAME.

    IF Archivo_nombre <> "" THEN
        Archivo:SCREEN-VALUE = Archivo_nombre.
    ELSE
        RETURN.

    FIND FIRST cfg_contenidoListas where cfg_contenidoListas.IDLista = STRING(ROWID(cfg_listasSarlaft)) NO-LOCK NO-ERROR.
    IF AVAILABLE cfg_contenidoListas THEN DO:
        MESSAGE "¿Desea reemplazar el contenido de la lista con los datos del archivo elegido?" SKIP
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar carga" UPDATE W_RptaRet AS LOGICAL.

        IF NOT W_RptaRet THEN
            RETURN.
        ELSE DO:
            FOR EACH cfg_contenidoListas where cfg_contenidoListas.IDLista = STRING(ROWID(cfg_listasSarlaft)):
                DELETE cfg_contenidoListas.
            END.
        END.
    END.

    INPUT FROM VALUE(Archivo_nombre).
    REPEAT:
        IMPORT UNFORMAT cLine.
        CREATE cfg_contenidolistas.
        cfg_contenidolistas.IDLista = string(rowId(cfg_listasSarlaft)).
        cfg_contenidolistas.linea = cLine.
        ContadorRegistros:SCREEN-VALUE = STRING(INTEGER(ContadorRegistros:SCREEN-VALUE) + 1).
    END.

    MESSAGE "La Lista Restrictiva fue cargada exitosamente!"
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    hQuery = QUERY brw_lineas:HANDLE.
    hQuery:QUERY-PREPARE("FOR EACH cfg_contenidoListas SHARE-LOCK WHERE cfg_contenidoListas.IDLista = " + quoter(STRING(ROWID(cfg_listasSarlaft)))).
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
    IF LASTKEY = KEYCODE("GO") OR LASTKEY = KEYCODE("RETURN") THEN DO:
        FIND NEXT cfg_contenidoListas WHERE cfg_contenidoListas.linea MATCHES '*' + Buscar:SCREEN-VALUE + '*'
                                        AND STRING(ROWID(cfg_listassarlaft)) = cfg_contenidoListas.idlista.
        
        hQUERY:REPOSITION-TO-ROWID(ROWID(cfg_contenidoListas)).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME descargar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL descargar C-Win
ON CHOOSE OF descargar IN FRAME F_Lista /* Button 1 */
DO:
    SYSTEM-DIALOG GET-FILE Archivotxt
        TITLE      "Guardar archivo como"
        FILTERS    "Archivos txt (*.txt)"   "*.*",
                   "Archivos csv (*.csv)"   "*.csv"
        DEFAULT-EXTENSION "txt"
        SAVE-AS
     USE-FILENAME.

    IF Archivotxt <> "" THEN DO: 
        OUTPUT TO VALUE(Archivotxt).
            FOR EACH cfg_contenidoListas WHERE cfg_contenidoListas.IDLista = STRING(ROWID(cfg_listasSarlaft)) NO-LOCK:
                EXPORT cfg_contenidoListas.linea.
            END.
        OUTPUT CLOSE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-MAIN
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
    Btn-Guardar:HIDDEN IN FRAME F-MAIN = TRUE.

    /* Validamos si el Usuario està configurado como Oficial de Cumplimiento */
    FIND FIRST cfg_sarlaft NO-LOCK NO-ERROR.
    IF AVAILABLE cfg_sarlaft THEN DO:
        IF cfg_sarlaft.usuarioOficialDeCumplimiento = w_usuario THEN
            esOficialDeCumplimiento = TRUE.
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
  DISPLAY Nombre_fill Tipo_fill Estado 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  ENABLE brwListas Nombre_fill Btn_Deshacer Btn-Guardar Tipo_fill Estado 
         Btn-Salvar Btn_Done Btn-Editar RECT-325 
      WITH FRAME F-MAIN IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-MAIN}
  DISPLAY ContadorRegistros Archivo ClientesValidados Buscar ListaAEditar 
      WITH FRAME F_Lista IN WINDOW C-Win.
  ENABLE ContadorRegistros descargar Btn_Archivo Archivo ClientesValidados 
         Buscar brw_lineas ListaAEditar 
      WITH FRAME F_Lista IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F_Lista}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
VIEW FRAME F-Main.

ASSIGN FRAME F_Lista:HIDDEN           = TRUE
    ListaAEditar:SCREEN-VALUE = ""
    Archivo:SCREEN-VALUE = "".
DO WITH FRAME f-main:
     ASSIGN Nombre_fill:SCREEN-VALUE = ""
           Tipo_fill:SCREEN-VALUE = "RES"
           Nombre_fill:READ-ONLY IN FRAME F-MAIN            = FALSE.
     ASSIGN Btn-Guardar:HIDDEN IN FRAME F-MAIN           = TRUE.
     ASSIGN Btn-Salvar:HIDDEN IN FRAME F-MAIN            = FALSE.
     ASSIGN Btn-Editar:HIDDEN IN FRAME F-MAIN            = FALSE.
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

