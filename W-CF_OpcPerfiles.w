&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-progra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-progra 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */


{incluido\variable.i SHARED}
{incluido\varcon.i SHARED}

/* Variables de trabajo                                                */

   DEFINE VARIABLE W_Status     AS LOGICAL.
   DEFINE VARIABLE W_PrimeraVez AS LOGICAL.
   DEFINE VARIABLE W_Item       AS CHARACTER.
   DEFINE VARIABLE W_Programa LIKE Programas.Programa.
   DEFINE VARIABLE W_NivelItem  AS INTEGER.
   DEFINE VARIABLE W_NivelPadre AS INTEGER.   
   DEFINE VARIABLE W_Posicion   AS INTEGER                  NO-UNDO.
   DEFINE VARIABLE W_GrupoAux LIKE Grupos.Grupo             NO-UNDO.
   DEFINE VARIABLE I            AS INTEGER                  NO-UNDO.
   DEFINE VARIABLE Listado      AS CHARACTER INITIAL "l-planti.Lst".
   DEFINE VARIABLE W_Consulta   AS LOGICAL INITIAL NO.
   DEFINE VARIABLE W_Rowid         AS ROWID.
   

/**/
   DEFINE VAR W_NomUsu                LIKE Usuarios.Nombre FORMAT "X(30)" INITIAL "".
   DEFINE VAR W_NomUsuEnc             LIKE Usuarios.Nombre FORMAT "X(30)" INITIAL "".
   DEFINE VAR W_NomEstEnc             LIKE Estacion.Descripcion FORMAT "X(30)" INITIAL "".
   DEFINE VAR W_CodUsu                LIKE Usuarios.Usuario.
   DEFINE VAR W_FechaHora AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_Reporte   AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_EncColumna  AS CHARACTER FORMAT "X(80)".
   DEFINE VAR W_IdReporta AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_IdEstacion AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_Ubicacion AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_PiePagina AS CHARACTER FORMAT "X(60)".
   DEFINE VAR W_UsuEncabe AS CHARACTER FORMAT "X(60)".


  DEFINE VAR W_Linea     AS CHARACTER FORMAT "X(130)" INITIAL "".
  DEFINE VAR W_Linea2    AS CHARACTER FORMAT "X(130)" INITIAL "".

/*  DEFINE VAR W_Raya2     AS CHARACTER INITIAL "-".*/
  DEFINE VAR W_UsuTra    AS CHAR FORMAT "X(4)" INITIAL "".
  DEFINE VAR HoraEnvio   AS CHARACTER FORMAT "X(8)".
  DEFINE VAR HoraLee     AS CHARACTER FORMAT "X(8)".
  DEFINE VAR FechaLee    AS CHARACTER FORMAT "X(10)".

/**/

   DEFINE BUFFER Tmp_Pro_Grupo FOR Pro_Grupo.
   DEFINE TEMP-TABLE Tmp
          FIELD Nivel       LIKE Pro_Grupo.Nivel
          FIELD Origen_Rama LIKE Pro_Grupo.Origen_Rama
          FIELD Abuelo      LIKE Pro_Grupo.Padre
          FIELD Padre       LIKE Pro_Grupo.Padre
          FIELD Orden       LIKE Pro_Grupo.Orden
          FIELD Programa    LIKE Pro_Grupo.Programa
          FIELD Raya        LIKE Pro_Grupo.Raya
   INDEX Ind_ProGru IS PRIMARY UNIQUE Nivel
                                      Origen_Rama
                                      Abuelo
                                      Padre
                                      Orden.
   Listado = W_PathSpl + listado.
   
   DEFINE FRAME F_Imprime 
       W_Item AT 03 FORMAT "X(70)"
      WITH DOWN NO-LABELS NO-BOX STREAM-IO WIDTH 100.

   DEFINE TEMP-TABLE Temporal LIKE Pro_Grupo.
   DEFINE TEMP-TABLE Tmp_Ex
       FIELD Campo1 AS CHARACTER FORMAT "X(50)"
       FIELD Campo2 AS CHARACTER FORMAT "X(40)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Grupos

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 Grupos.Grupo Grupos.Nombre ~
Grupos.Prioridad 
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1 
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH Grupos ~
      WHERE Grupos.Estado = 1 AND Grupos.Grupo <> W_Grupo NO-LOCK
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY BROWSE-1 FOR EACH Grupos ~
      WHERE Grupos.Estado = 1 AND Grupos.Grupo <> W_Grupo NO-LOCK.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 Grupos
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 Grupos


/* Definitions for FRAME FRAME-A                                        */

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Programas.Proceso Programas.Tipo ~
Programas.Descripcion Programas.Id_Procesos Programas.Programa ~
Programas.Nom_Fisico Programas.Ejecutable 
&Scoped-define ENABLED-TABLES Programas
&Scoped-define FIRST-ENABLED-TABLE Programas
&Scoped-Define ENABLED-OBJECTS W_Ejecutable SELECT-1 W_StrEjecuta ~
W_StrNoEjecuta W_StrTodos Btn_Add Btn_del Btn_Rul Btn_Up Btn_Bck Btn_Dwn ~
Btn_Fwd SELECT-2 BUTTON-44 Btn_Imprimir Btn_Consulta Btn_Salva Btn_Borrar ~
Btn_Cancela Btn_Copia Btn_Salir Btn_Ayuda W_Grupo W_Nombre IMAGE-4 RECT-125 ~
RECT-129 RECT-230 
&Scoped-Define DISPLAYED-FIELDS Programas.Proceso Programas.Tipo ~
Programas.Descripcion Programas.Id_Procesos Programas.Programa ~
Programas.Nom_Fisico Programas.Ejecutable 
&Scoped-define DISPLAYED-TABLES Programas
&Scoped-define FIRST-DISPLAYED-TABLE Programas
&Scoped-Define DISPLAYED-OBJECTS W_Ejecutable SELECT-1 W_StrEjecuta ~
W_StrNoEjecuta W_StrTodos SELECT-2 W_Grupo W_Nombre 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Programas.Proceso Programas.Tipo ~
Programas.Descripcion Programas.Id_Procesos Programas.Programa ~
Programas.Nom_Fisico Programas.Ejecutable 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-progra AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-Btn_Imprimir 
       MENU-ITEM m_Todos        LABEL "Todos"         
       MENU-ITEM m_Activo       LABEL "Activo"        .


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Add 
     IMAGE-UP FILE "Imagenes\btn_fwd":U
     LABEL "&Adicionar" 
     SIZE 14 BY 1.35 TOOLTIP "Adiciona la Opción a la Lista de Asignación".

DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Accesa la Ayuda para la Ventana".

DEFINE BUTTON Btn_Bck 
     IMAGE-UP FILE "Imagenes\fizq":U
     LABEL "Mueve A&tras" 
     SIZE 5 BY 1 TOOLTIP "A la Opción Seleccionada se le Suprime la Dependencia de la Anterior".

DEFINE BUTTON Btn_Borrar 
     LABEL "&Borrar" 
     SIZE 10 BY 1.35 TOOLTIP "Borra Todas las Opciones Asignadas".

DEFINE BUTTON Btn_Cancela 
     LABEL "Ca&ncelar" 
     SIZE 10 BY 1.35 TOOLTIP "Cancela Cualquier Cambio no Guardado".

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes\lupa":U
     LABEL "&Consultar" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla activa".

DEFINE BUTTON Btn_Copia 
     LABEL "Co&piar" 
     SIZE 10 BY 1.35 TOOLTIP "Copia Todas las Opciones del Grupo hacia Otros".

DEFINE BUTTON Btn_del 
     IMAGE-UP FILE "Imagenes\btn_bck":U
     LABEL "&Eliminar" 
     SIZE 14 BY 1.35 TOOLTIP "Elimina la Opción de la Lista de Asignación".

DEFINE BUTTON Btn_Dwn 
     IMAGE-UP FILE "Imagenes\faba":U
     LABEL "Mueve Aba&jo" 
     SIZE 4 BY 1.35 TOOLTIP "Mueve hacia Abajo la Opción Seleccionada".

DEFINE BUTTON Btn_Fwd 
     IMAGE-UP FILE "Imagenes\fder":U
     LABEL "Mueve A&delante" 
     SIZE 5 BY 1 TOOLTIP "La Opción Seleccionada pasa a ser Dependiente de la Opción Anterior".

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "&Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra la interface de salida de información (Reportes)".

DEFINE BUTTON Btn_Rul 
     LABEL "Separador" 
     SIZE 14 BY 1 TOOLTIP "Genera una Lína de Separación de Opciones".

DEFINE BUTTON Btn_Salir 
     LABEL "Sa&lir" 
     SIZE 10 BY 1.35 TOOLTIP "Sale de la Ventana".

DEFINE BUTTON Btn_Salva 
     LABEL "&Salvar" 
     SIZE 10 BY 1.35 TOOLTIP "Guarda las Opciones Asignadas".

DEFINE BUTTON Btn_Up 
     IMAGE-UP FILE "Imagenes\farr":U
     LABEL "Mueve A&rriba" 
     SIZE 4 BY 1.35 TOOLTIP "Mueve hacia Arriba la Opción Seleccionada".

DEFINE BUTTON BUTTON-44 
     IMAGE-UP FILE "imagenes\informacion":U
     LABEL "Button 44" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra la pantalla de Información de Sesión y mensajes del usuario activo".

DEFINE VARIABLE W_Grupo AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Grupo" 
      VIEW-AS TEXT 
     SIZE 4 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_Nombre AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 25 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE IMAGE IMAGE-4
     FILENAME "imagenes\grupo":U
     SIZE 14 BY 2.96.

DEFINE VARIABLE W_Ejecutable AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todas", 1,
"Ejecutables", 2,
"No Ejecutables", 3
     SIZE 30.86 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-125
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 7.27.

DEFINE RECTANGLE RECT-129
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 95 BY 5.27.

DEFINE RECTANGLE RECT-230
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.

DEFINE VARIABLE SELECT-1 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 40 BY 12.65 TOOLTIP "Opciones de Menú Disponibles para Asignar"
     BGCOLOR 15 FGCOLOR 0 FONT 5 NO-UNDO.

DEFINE VARIABLE SELECT-2 AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 37 BY 12.65 TOOLTIP "Opciones de Menú Asignadas"
     BGCOLOR 15 FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE W_StrEjecuta AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 36.57 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_StrNoEjecuta AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 36.57 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W_StrTodos AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT SCROLLBAR-VERTICAL 
     SIZE 37 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE BUTTON Btn_Acepta 
     LABEL "Aceptar" 
     SIZE 9 BY 1 TOOLTIP "Confirma la Copia de la Opción Seleccionada".

DEFINE BUTTON Btn_CancelaCp 
     LABEL "Cancelar" 
     SIZE 9 BY 1 TOOLTIP "Cancela la Selección".

DEFINE BUTTON Btn_SaleCp 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "Salir" 
     SIZE 9 BY 1.54 TOOLTIP "Regresa a la Ventana Principal".

DEFINE VARIABLE RADIO-SET-1 AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todos los Grupos", 1,
"Seleccionados", 2
     SIZE 20 BY 2.15 TOOLTIP "Selección de la Copia"
     FONT 5 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      Grupos
    FIELDS(Grupos.Grupo
      Grupos.Nombre
      Grupos.Prioridad) SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-progra _STRUCTURED
  QUERY BROWSE-1 DISPLAY
      Grupos.Grupo COLUMN-LABEL "Código" FORMAT "99":U
      Grupos.Nombre FORMAT "X(30)":U
      Grupos.Prioridad FORMAT "9":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 36.72 BY 8.38
         BGCOLOR 15 FONT 4 TOOLTIP "Lista de Grupos a los que se le puede Copiar".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     W_Ejecutable AT ROW 7.73 COL 4 NO-LABEL
     SELECT-1 AT ROW 8.54 COL 4 NO-LABEL
     W_StrEjecuta AT ROW 8.54 COL 4.43 NO-LABEL
     W_StrNoEjecuta AT ROW 9.27 COL 4.43 NO-LABEL
     W_StrTodos AT ROW 9.96 COL 4 NO-LABEL
     Programas.Proceso AT ROW 5.77 COL 8.86 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Clientes", 1,
"Taquilla", 2,
"Procesos", 3,
"Cierres", 4
          SIZE 38 BY .81
          BGCOLOR 17 FGCOLOR 0 
     Btn_Add AT ROW 12.85 COL 45
     Btn_del AT ROW 14.19 COL 45
     Btn_Rul AT ROW 15.54 COL 45
     Programas.Tipo AT ROW 2.35 COL 48
          VIEW-AS TOGGLE-BOX
          SIZE 11 BY .69
          BGCOLOR 17 FGCOLOR 0 FONT 4
     Programas.Descripcion AT ROW 3.42 COL 48 NO-LABEL
          VIEW-AS EDITOR SCROLLBAR-VERTICAL
          SIZE 47 BY 2.69
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Btn_Up AT ROW 17.15 COL 50
     Btn_Bck AT ROW 18.5 COL 47
     Btn_Dwn AT ROW 19.58 COL 50
     Btn_Fwd AT ROW 18.5 COL 52
     Programas.Id_Procesos AT ROW 2.35 COL 59
          VIEW-AS TOGGLE-BOX
          SIZE 12.29 BY .69
          BGCOLOR 17 FGCOLOR 0 
     SELECT-2 AT ROW 8.54 COL 60 NO-LABEL
     BUTTON-44 AT ROW 1.54 COL 102
     Btn_Imprimir AT ROW 3.15 COL 102 HELP
          "Imprime Reporte de Grupos"
     Btn_Consulta AT ROW 4.77 COL 102 HELP
          "Permite la Selección del Grupo"
     Btn_Salva AT ROW 12.31 COL 102
     Btn_Borrar AT ROW 13.65 COL 102
     Btn_Cancela AT ROW 15 COL 102
     Btn_Copia AT ROW 16.35 COL 102
     Btn_Salir AT ROW 17.69 COL 102
     Btn_Ayuda AT ROW 19.58 COL 105 HELP
          "Accesa la Ayuda para la Ventana"
     W_Grupo AT ROW 2.12 COL 14 COLON-ALIGNED
     W_Nombre AT ROW 2.12 COL 18 COLON-ALIGNED NO-LABEL
     Programas.Programa AT ROW 3.04 COL 14 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 29 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Programas.Nom_Fisico AT ROW 3.96 COL 14 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 29 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     Programas.Ejecutable AT ROW 4.88 COL 14 COLON-ALIGNED
           VIEW-AS TEXT 
          SIZE 29 BY .81
          BGCOLOR 18 FGCOLOR 15 FONT 5
     IMAGE-4 AT ROW 8.81 COL 45
     RECT-125 AT ROW 12.04 COL 101
     RECT-129 AT ROW 1.65 COL 4
     RECT-230 AT ROW 1.27 COL 101
     "  Información General del Grupo y sus Programas" VIEW-AS TEXT
          SIZE 43 BY .81 AT ROW 1.27 COL 6
          FGCOLOR 7 FONT 5
     "Programas y opciones disponibles" VIEW-AS TEXT
          SIZE 31 BY .81 AT ROW 6.92 COL 4
          BGCOLOR 17 FGCOLOR 7 FONT 5
     "Menu configurado" VIEW-AS TEXT
          SIZE 16 BY .69 AT ROW 7.73 COL 60
          BGCOLOR 17 FGCOLOR 7 FONT 5
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114 BY 20.92
         BGCOLOR 17 FONT 4.

DEFINE FRAME FRAME-A
     BROWSE-1 AT ROW 1.27 COL 2
     Btn_Acepta AT ROW 9.88 COL 30
     RADIO-SET-1 AT ROW 10.96 COL 6 NO-LABEL
     Btn_CancelaCp AT ROW 10.96 COL 30
     Btn_SaleCp AT ROW 12.31 COL 30
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 60 ROW 7.46
         SIZE 39 BY 14.27
         BGCOLOR 17 FONT 4
         TITLE "Copiado de menu a otros grupos".


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
  CREATE WINDOW W-progra ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de Opciones y Procedimientos para los Perfiles del Sistema"
         HEIGHT             = 20.92
         WIDTH              = 113.57
         MAX-HEIGHT         = 21.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.35
         VIRTUAL-WIDTH      = 114.29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-progra 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-progra
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME FRAME-A:FRAME = FRAME F-Main:HANDLE.

/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE L-To-R,COLUMNS                                           */
ASSIGN 
       Btn_Imprimir:POPUP-MENU IN FRAME F-Main       = MENU POPUP-MENU-Btn_Imprimir:HANDLE.

/* SETTINGS FOR EDITOR Programas.Descripcion IN FRAME F-Main
   1                                                                    */
ASSIGN 
       Programas.Descripcion:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN Programas.Ejecutable IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX Programas.Id_Procesos IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN Programas.Nom_Fisico IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR RADIO-SET Programas.Proceso IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FILL-IN Programas.Programa IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX Programas.Tipo IN FRAME F-Main
   1                                                                    */
/* SETTINGS FOR FRAME FRAME-A
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-1 1 FRAME-A */
ASSIGN 
       FRAME FRAME-A:HIDDEN           = TRUE
       FRAME FRAME-A:MOVABLE          = TRUE.

ASSIGN 
       BROWSE-1:SEPARATOR-FGCOLOR IN FRAME FRAME-A      = 12.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-progra)
THEN W-progra:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _TblList          = "bdcentral.Grupos"
     _TblOptList       = "USED"
     _Where[1]         = "Grupos.Estado = 1 AND Grupos.Grupo <> W_Grupo"
     _FldNameList[1]   > bdcentral.Grupos.Grupo
"Grupos.Grupo" "Código" ? "integer" ? ? ? ? ? ? no ? no no ? yes no no "U" "" ""
     _FldNameList[2]   = bdcentral.Grupos.Nombre
     _FldNameList[3]   = bdcentral.Grupos.Prioridad
     _Query            is NOT OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-progra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-progra W-progra
ON END-ERROR OF W-progra /* SFG - Configuración de Opciones y Procedimientos para los Perfiles del Sistema */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-progra W-progra
ON WINDOW-CLOSE OF W-progra /* SFG - Configuración de Opciones y Procedimientos para los Perfiles del Sistema */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-progra
ON VALUE-CHANGED OF BROWSE-1 IN FRAME FRAME-A
DO:
  IF RADIO-SET-1 EQ 1 THEN
     ASSIGN W_Status = {&BROWSE-NAME}:DESELECT-ROWS().  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Acepta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Acepta W-progra
ON CHOOSE OF Btn_Acepta IN FRAME FRAME-A /* Aceptar */
DO:
  IF RADIO-SET-1 EQ 1 THEN DO:
     RUN MostrarMensaje IN W_Manija (INPUT 112, OUTPUT W_Status).
     IF NOT W_Status THEN RETURN NO-APPLY.
  END.
  
  SESSION:SET-WAIT-STATE("GENERAL").
  IF RADIO-SET-1 EQ 1 THEN
     FOR EACH Grupos WHERE Grupos.Estado EQ 1 
                       AND Grupos.Grupo  NE W_Grupo NO-LOCK:
         ASSIGN W_GrupoAux = Grupos.Grupo.
         RUN Copia_Grupo.                       
     END.
  ELSE DO:
      DO I = 1 TO BROWSE-1:NUM-SELECTED-ROWS:
         W_Status = BROWSE-1:FETCH-SELECTED-ROW(i).
         GET CURRENT BROWSE-1 NO-LOCK.
         ASSIGN W_GrupoAux = Grupos.Grupo.
         RUN Copia_Grupo.
      END.
  END.
  ASSIGN W_Status = {&BROWSE-NAME}:DESELECT-ROWS().
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Add
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Add W-progra
ON CHOOSE OF Btn_Add IN FRAME F-Main /* Adicionar */
DO:
   IF SELECT-1:SCREEN-VALUE EQ ? THEN
      RETURN NO-APPLY.

   IF NOT AVAILABLE Programas THEN
      APPLY "VALUE-CHANGED":U TO SELECT-1.
      
   ASSIGN W_Item     = Programas.Opcion + FILL(" ", 70) + "| " + STRING(Programas.Programa) + " | 1"
          W_Posicion = SELECT-2:LOOKUP(SELECT-2:SCREEN-VALUE).

   IF W_Posicion EQ ? THEN
      ASSIGN W_Status = SELECT-2:INSERT(W_Item, 1).
   ELSE   
      ASSIGN W_Status = SELECT-2:INSERT(W_Item, W_Posicion + 1).

   ASSIGN SELECT-2:SCREEN-VALUE = W_Item.
   RUN Borra_Item (SELECT-1:HANDLE, SELECT-1:SCREEN-VALUE).
   APPLY "VALUE-CHANGED":U TO SELECT-1.
   APPLY "VALUE-CHANGED":U TO SELECT-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-progra
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "AYUDAS\CONFIGUR" CONTEXT 6.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Bck
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Bck W-progra
ON CHOOSE OF Btn_Bck IN FRAME F-Main /* Mueve Atras */
DO:
  IF SELECT-2:SCREEN-VALUE EQ ? THEN
     RETURN NO-APPLY.

  ASSIGN W_NivelItem = INTEGER(ENTRY(3,SELECT-2:SCREEN-VALUE, "|"))
         W_Posicion  = SELECT-2:LOOKUP(SELECT-2:SCREEN-VALUE).

  IF W_NivelItem EQ 1
  OR W_Posicion  EQ 1 THEN
     RETURN NO-APPLY.

  ASSIGN W_NivelItem = W_NivelItem - 1
         W_Posicion  = R-INDEX(SELECT-2:SCREEN-VALUE, "|") + 1
         W_Item      = SUBSTRING(SELECT-2:SCREEN-VALUE, 4, W_Posicion - 3) + STRING(W_NivelItem)
         W_Status    = SELECT-2:REPLACE(W_Item, SELECT-2:SCREEN-VALUE)
         SELECT-2:SCREEN-VALUE = W_Item.
  APPLY "VALUE-CHANGED":U TO SELECT-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Borrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Borrar W-progra
ON CHOOSE OF Btn_Borrar IN FRAME F-Main /* Borrar */
DO:
  IF W_Prioridad = 1 THEN DO:
      RUN MostrarMensaje IN W_Manija(INPUT 295,OUTPUT W_Rpta).
      RETURN NO-APPLY.
  END.
  ELSE
  IF W_Prioridad = 2 THEN DO:
     RUN MostrarMensaje IN W_Manija(INPUT 296,OUTPUT W_Rpta).
     RETURN NO-APPLY.
  END.
  ELSE
  IF W_Prioridad = 3 THEN DO:
     RUN MostrarMensaje IN W_Manija(INPUT 297,OUTPUT W_Rpta).
     RETURN NO-APPLY.
  END. 

  IF SELECT-2:NUM-ITEMS EQ 0 THEN
     RETURN NO-APPLY.

  FIND FIRST Tmp_Pro_Grupo WHERE Tmp_Pro_Grupo.Grupo EQ W_Grupo NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Tmp_Pro_Grupo THEN
     RETURN NO-APPLY.

  RUN MostrarMensaje IN W_Manija (INPUT 5, OUTPUT W_Status).
  IF NOT W_Status THEN RETURN NO-APPLY.

  SESSION:SET-WAIT-STATE("GENERAL"). 
  RUN Borra_Plantilla.
  RUN Inicializa.
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancela
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancela W-progra
ON CHOOSE OF Btn_Cancela IN FRAME F-Main /* Cancelar */
DO:
  ASSIGN W_Ejecutable:SCREEN-VALUE = STRING(1)
         W_Ejecutable
         W_PrimeraVez = TRUE.
  RUN Inicializa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME Btn_CancelaCp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_CancelaCp W-progra
ON CHOOSE OF Btn_CancelaCp IN FRAME FRAME-A /* Cancelar */
DO:
  ASSIGN W_Status = {&BROWSE-NAME}:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-progra
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consultar */
DO:
  ASSIGN W_Rowid = ?.
  RUN c-perfiles.r (INPUT-OUTPUT W_Rowid).
  IF W_Rowid NE ? THEN DO: 
     FIND Grupos WHERE ROWID(Grupos) EQ W_Rowid NO-LOCK.
     IF W_Grupo EQ Grupos.Grupo THEN
        RETURN.
     ASSIGN W_Grupo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Grupos.Grupo, "99")
            W_Grupo
            W_Nombre:SCREEN-VALUE = Grupos.Nombre
            W_Ejecutable:SCREEN-VALUE = STRING(1)
            W_Ejecutable
            W_PrimeraVez = TRUE.
     RUN Inicializa.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Copia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Copia W-progra
ON CHOOSE OF Btn_Copia IN FRAME F-Main /* Copiar */
DO:
  IF SELECT-2:NUM-ITEMS EQ 0 THEN
     RETURN NO-APPLY.

  FIND FIRST Tmp_Pro_Grupo WHERE Tmp_Pro_Grupo.Grupo EQ W_Grupo NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Tmp_Pro_Grupo THEN
     RETURN NO-APPLY.
     
  RUN Deshabilita_Objetos.
  {&OPEN-QUERY-BROWSE-1}
  ASSIGN FRAME FRAME-A:HIDDEN    = FALSE
         FRAME FRAME-A:VISIBLE   = TRUE.
         FRAME FRAME-A:SENSITIVE = TRUE.
         
  APPLY "VALUE-CHANGED":U TO RADIO-SET-1 IN FRAME FRAME-A.
  APPLY "ENTRY":U TO RADIO-SET-1 IN FRAME FRAME-A.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_del
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_del W-progra
ON CHOOSE OF Btn_del IN FRAME F-Main /* Eliminar */
DO:
  IF SELECT-2:SCREEN-VALUE EQ ? THEN
     RETURN NO-APPLY.

  IF LOOKUP("RAYA", SELECT-2:SCREEN-VALUE, " ") LE 0 THEN DO:
     ASSIGN W_Programa = INTEGER(ENTRY(2,SELECT-2:SCREEN-VALUE, "|")).
     FIND Programas WHERE Programas.Program EQ W_Programa NO-LOCK NO-ERROR.
     ASSIGN W_Item   = Programas.Opcion + FILL(" ", 70) + "| " + STRING(Programas.Programa).
     IF INTEGER(W_Ejecutable:SCREEN-VALUE) EQ 1 THEN
        ASSIGN W_Status = SELECT-1:ADD-LAST(W_Item)
               SELECT-1:SCREEN-VALUE = W_Item.
     IF Programas.Tipo THEN DO:
        IF INTEGER(W_Ejecutable:SCREEN-VALUE) EQ 2 THEN
           ASSIGN W_Status = SELECT-1:ADD-LAST(W_Item)
                  SELECT-1:SCREEN-VALUE = W_Item.
        ELSE
           IF INTEGER(W_Ejecutable:SCREEN-VALUE) EQ 3 THEN
              ASSIGN W_Status = W_StrEjecuta:ADD-LAST(W_Item).
     END.
     ELSE DO:
        IF INTEGER(W_Ejecutable:SCREEN-VALUE) EQ 3 THEN
           ASSIGN W_Status = SELECT-1:ADD-LAST(W_Item)
                  SELECT-1:SCREEN-VALUE = W_Item.
        ELSE
           IF INTEGER(W_Ejecutable:SCREEN-VALUE) EQ 2 THEN
              ASSIGN W_Status = W_StrNoEjecuta:ADD-LAST(W_Item).
     END.
  END.
  RUN Borra_Item (SELECT-2:HANDLE, SELECT-2:SCREEN-VALUE).  
  APPLY "VALUE-CHANGED":U TO SELECT-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Dwn
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Dwn W-progra
ON CHOOSE OF Btn_Dwn IN FRAME F-Main /* Mueve Abajo */
DO:
  IF SELECT-2:SCREEN-VALUE EQ ? THEN
     RETURN NO-APPLY.

  ASSIGN W_Posicion = SELECT-2:LOOKUP(SELECT-2:SCREEN-VALUE).
  IF W_Posicion EQ SELECT-2:NUM-ITEMS THEN
     RETURN NO-APPLY.
     
  ASSIGN W_Item = SELECT-2:SCREEN-VALUE.
  RUN Borra_Item (SELECT-2:HANDLE, SELECT-2:SCREEN-VALUE).
  ASSIGN W_Status              = SELECT-2:INSERT(W_Item, W_Posicion + 1)
         SELECT-2:SCREEN-VALUE = W_Item.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Fwd
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Fwd W-progra
ON CHOOSE OF Btn_Fwd IN FRAME F-Main /* Mueve Adelante */
DO:
  IF SELECT-2:SCREEN-VALUE EQ ? THEN
     RETURN NO-APPLY.

  ASSIGN W_NivelItem  = INTEGER(ENTRY(3,SELECT-2:SCREEN-VALUE, "|"))
         W_Posicion   = SELECT-2:LOOKUP(SELECT-2:SCREEN-VALUE)
         W_NivelPadre = INTEGER(ENTRY(3,SELECT-2:ENTRY(W_Posicion - 1), "|"))
         W_Programa   = INTEGER(ENTRY(2,SELECT-2:ENTRY(W_Posicion - 1), "|")). /* valida sobre el Padre */

  IF W_NivelItem GE 6 
  OR W_Posicion  EQ 1 
  OR (W_NivelItem - W_NivelPadre) EQ 1 THEN
       RETURN NO-APPLY.
  
  IF  LOOKUP("RAYA", SELECT-2:ENTRY(W_Posicion - 1), " ") GT 0 
  AND (W_NivelItem - W_NivelPadre) GE 0 THEN
      RETURN NO-APPLY.

  IF  LOOKUP("RAYA", SELECT-2:ENTRY(W_Posicion - 1), " ") LE 0 THEN DO:
      FIND Programas WHERE Programas.Program EQ W_Programa NO-LOCK NO-ERROR.
      IF  Programa.Tipo 
      AND (W_NivelItem - W_NivelPadre) GE 0 THEN DO:
           IF  Programas.Ejecutable NE "w-taquil.r"
           AND Programas.Ejecutable NE "w-sercli.r" THEN
               RETURN NO-APPLY.
      END.
  END.

  ASSIGN W_NivelItem = W_NivelItem + 1
         W_Posicion  = R-INDEX(SELECT-2:SCREEN-VALUE, "|") + 1
         W_Item      = SUBSTRING(SELECT-2:SCREEN-VALUE, 1, W_Posicion) + STRING(W_NivelItem)
/*         W_Item      = "-  " + W_Item*/
         W_Item      = ">  " + W_Item
         W_Status    = SELECT-2:REPLACE(W_Item, SELECT-2:SCREEN-VALUE)
         SELECT-2:SCREEN-VALUE = W_Item.
  APPLY "VALUE-CHANGED":U TO SELECT-2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-progra
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Imprimir */
DO:
  ASSIGN W_GrupoAux = W_Grupo.
  {incluido/Imprimir.i "Listado" 2}
  ASSIGN W_Grupo = W_GrupoAux.
  RUN Inicializa.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Rul
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Rul W-progra
ON CHOOSE OF Btn_Rul IN FRAME F-Main /* Separador */
DO:
  IF SELECT-2:SCREEN-VALUE EQ ? THEN
     RETURN NO-APPLY.

  ASSIGN W_NivelItem = INTEGER(ENTRY(3,SELECT-2:SCREEN-VALUE, "|"))
         W_Posicion  = SELECT-2:LOOKUP(SELECT-2:SCREEN-VALUE).

  IF W_NivelItem EQ 1
  OR W_Posicion  EQ 1 THEN
     RETURN NO-APPLY.

  ASSIGN W_Item   = FILL(">  ", W_NivelItem - 1) + "RAYA " + FILL(" ", (70 - (3 * (W_NivelItem - 1)))) + "| " + "" + " | " + STRING(W_NivelItem)
         W_Status = SELECT-2:INSERT(W_Item, W_Posicion + 1)
         SELECT-2:SCREEN-VALUE = W_Item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME Btn_SaleCp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SaleCp W-progra
ON CHOOSE OF Btn_SaleCp IN FRAME FRAME-A /* Salir */
DO:
  CLOSE QUERY {&BROWSE-NAME}.
  ASSIGN FRAME FRAME-A:HIDDEN  = TRUE
         FRAME FRAME-A:VISIBLE = FALSE.
  RUN Habilita_Objetos.  
  RUN Inicializa.       
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir W-progra
ON CHOOSE OF Btn_Salir IN FRAME F-Main /* Salir */
DO:
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salva
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salva W-progra
ON CHOOSE OF Btn_Salva IN FRAME F-Main /* Salvar */
DO:
  IF W_Prioridad = 1 THEN DO:
     RUN MostrarMensaje IN W_Manija(INPUT 295,OUTPUT W_Rpta).
     RETURN NO-APPLY.
  END.

  IF SELECT-2:NUM-ITEMS EQ 0 THEN
     RETURN NO-APPLY.
 
  SESSION:SET-WAIT-STATE("GENERAL").
  RUN Asignar.
  SESSION:SET-WAIT-STATE("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Up
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Up W-progra
ON CHOOSE OF Btn_Up IN FRAME F-Main /* Mueve Arriba */
DO:
  IF SELECT-2:SCREEN-VALUE EQ ? THEN
     RETURN NO-APPLY.

  ASSIGN W_Posicion  = SELECT-2:LOOKUP(SELECT-2:SCREEN-VALUE)
         W_NivelItem = INTEGER(ENTRY(3,SELECT-2:SCREEN-VALUE, "|")).

  IF W_Posicion EQ 1 
  OR (    W_Posicion  EQ 2
      AND W_NivelItem NE 1) THEN
     RETURN NO-APPLY.
  
  ASSIGN W_Item = SELECT-2:SCREEN-VALUE.
  RUN Borra_Item (SELECT-2:HANDLE, SELECT-2:SCREEN-VALUE).
  ASSIGN W_Status              = SELECT-2:INSERT(W_Item, W_Posicion - 1)
         SELECT-2:SCREEN-VALUE = W_Item.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-44
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-44 W-progra
ON CHOOSE OF BUTTON-44 IN FRAME F-Main /* Button 44 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Activo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Activo W-progra
ON CHOOSE OF MENU-ITEM m_Activo /* Activo */
DO:
  ASSIGN W_Consulta = YES
         W_GrupoAux = W_Grupo.
  {incluido/Imprimir.i "Listado" 2}
  ASSIGN W_Grupo = W_GrupoAux.
  RUN Inicializa.
  ASSIGN W_Consulta = NO.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Todos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Todos W-progra
ON CHOOSE OF MENU-ITEM m_Todos /* Todos */
DO:
  APPLY "CHOOSE":U TO Btn_Imprimir IN FRAME {&FRAME-NAME}.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FRAME-A
&Scoped-define SELF-NAME RADIO-SET-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RADIO-SET-1 W-progra
ON VALUE-CHANGED OF RADIO-SET-1 IN FRAME FRAME-A
DO:
   ASSIGN RADIO-SET-1.
   IF RADIO-SET-1 EQ 1 THEN
      ASSIGN W_Status = {&BROWSE-NAME}:DESELECT-ROWS().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME SELECT-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-1 W-progra
ON VALUE-CHANGED OF SELECT-1 IN FRAME F-Main
DO:
  IF SELECT-1:SCREEN-VALUE EQ ? THEN
     RETURN NO-APPLY.
     
  ASSIGN W_Programa = INTEGER(ENTRY(2,SELECT-1:SCREEN-VALUE, "|")).
  FIND Programas WHERE Programas.Program EQ W_Programa NO-LOCK NO-ERROR.
  DISPLAY {&List-1} WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME SELECT-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL SELECT-2 W-progra
ON VALUE-CHANGED OF SELECT-2 IN FRAME F-Main
DO:
  ASSIGN Btn_Rul:SENSITIVE   = FALSE
         Btn_Salva:SENSITIVE = FALSE.

  IF SELECT-2:SCREEN-VALUE EQ ? THEN DO:
     RETURN NO-APPLY.
  END.

  ASSIGN W_NivelItem = INTEGER(ENTRY(3,SELECT-2:SCREEN-VALUE, "|"))
         W_Posicion  = SELECT-2:LOOKUP(SELECT-2:SCREEN-VALUE)
         Btn_Salva:SENSITIVE = TRUE.
         
  IF W_NivelItem EQ 1
  OR W_Posicion  EQ 1 THEN DO:
     RETURN NO-APPLY.
  END.

  ASSIGN Btn_Rul:SENSITIVE   = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Ejecutable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Ejecutable W-progra
ON VALUE-CHANGED OF W_Ejecutable IN FRAME F-Main
DO:
   IF W_PrimeraVez THEN DO:
      ASSIGN W_PrimeraVez = NO.
      RETURN.
   END.
   
   IF W_Ejecutable EQ 1 THEN DO:
      ASSIGN W_StrTodos:LIST-ITEMS = ""             
             W_StrEjecuta:LIST-ITEMS   = ""
             W_StrNoEjecuta:LIST-ITEMS = "".
      IF  SELECT-1:LIST-ITEMS NE ""
      AND SELECT-1:NUM-ITEMS GT 0 THEN
          ASSIGN W_Status = W_StrTodos:ADD-LAST(SELECT-1:LIST-ITEMS).
      SESSION:SET-WAIT-STATE("GENERAL").
      DO I = 1 TO SELECT-1:NUM-ITEMS:
         IF SELECT-1:LIST-ITEMS EQ "" THEN LEAVE.
         ASSIGN W_Item     = SELECT-1:ENTRY(i).
                W_Programa = INTEGER(ENTRY(2,W_Item, "|")).
         FIND Programas WHERE Programas.Program EQ W_Programa NO-LOCK NO-ERROR.
         IF Programas.Tipo THEN
            ASSIGN W_Status = W_StrEjecuta:ADD-LAST(W_Item).
         ELSE
            ASSIGN W_Status = W_StrNoEjecuta:ADD-LAST(W_Item).
      END.
      SESSION:SET-WAIT-STATE("").
   END.
   ELSE DO:
      IF W_Ejecutable EQ 2 THEN DO:
         ASSIGN W_StrEjecuta:LIST-ITEMS = "".
         IF  SELECT-1:LIST-ITEMS NE ""
         AND SELECT-1:NUM-ITEMS  GT 0 THEN
             ASSIGN W_Status = W_StrEjecuta:ADD-LAST(SELECT-1:LIST-ITEMS).
      END.
      ELSE
         IF W_Ejecutable EQ 3 THEN DO:
            ASSIGN W_StrNoEjecuta:LIST-ITEMS = "".
            IF  SELECT-1:LIST-ITEMS NE ""
            AND SELECT-1:NUM-ITEMS  GT 0  THEN
                ASSIGN W_Status = W_StrNoEjecuta:ADD-LAST(SELECT-1:LIST-ITEMS).
         END.

      IF W_StrEjecuta:NUM-ITEMS  EQ 0 
      OR W_StrEjecuta:LIST-ITEMS EQ "" THEN DO:
         ASSIGN W_StrTodos:LIST-ITEMS = "".
         IF  W_StrNoEjecuta:LIST-ITEMS NE ""
         AND W_StrNoEjecuta:NUM-ITEMS  GT 0 THEN
             ASSIGN W_Status = W_StrTodos:ADD-LAST(W_StrNoEjecuta:LIST-ITEMS).
      END.
      ELSE DO:
         IF W_StrNoEjecuta:NUM-ITEMS EQ 0
         OR W_StrNoEjecuta:LIST-ITEMS EQ "" THEN DO:
            ASSIGN W_StrTodos:LIST-ITEMS = "".
            IF  W_StrEjecuta:LIST-ITEMS NE ""
            AND W_StrEjecuta:NUM-ITEMS  GT 0 THEN
                ASSIGN W_Status = W_StrTodos:ADD-LAST(W_StrEjecuta:LIST-ITEMS).
         END.
         ELSE DO:
            ASSIGN W_StrTodos:LIST-ITEMS = "".
            IF  W_StrEjecuta:LIST-ITEMS NE ""
            AND W_StrEjecuta:NUM-ITEMS  GT 0 THEN
                ASSIGN W_Status = W_StrTodos:ADD-LAST(W_StrEjecuta:LIST-ITEMS).
            IF  W_StrNoEjecuta:NUM-ITEMS  GT 0
            AND W_StrNoEjecuta:LIST-ITEMS NE "" THEN 
                ASSIGN W_Status = W_StrTodos:ADD-LAST(W_StrNoEjecuta:LIST-ITEMS).
         END.
      END.
   END.

   ASSIGN W_Ejecutable.
   IF W_Ejecutable EQ 1 THEN DO:
      ASSIGN SELECT-1:LIST-ITEMS = "".
      IF  W_StrTodos:NUM-ITEMS GT 0 
      AND W_StrTodos:LIST-ITEMS NE ""THEN
          ASSIGN W_Status = SELECT-1:ADD-LAST(W_StrTodos:LIST-ITEMS).
   END.
   ELSE DO:
      IF W_Ejecutable EQ 2 THEN DO:
         ASSIGN SELECT-1:LIST-ITEMS = "".
         IF  W_StrEjecuta:NUM-ITEMS GT 0
         AND W_StrEjecuta:LIST-ITEMS NE "" THEN
             ASSIGN W_Status = SELECT-1:ADD-LAST(W_StrEjecuta:LIST-ITEMS).
      END.
      ELSE DO:
         ASSIGN SELECT-1:LIST-ITEMS = "".
         IF  W_StrNoEjecuta:NUM-ITEMS GT 0 
         AND W_StrNoEjecuta:LIST-ITEMS NE "" THEN DO:
             ASSIGN W_Status = SELECT-1:ADD-LAST(W_StrNoEjecuta:LIST-ITEMS).
         END.
      END.
   END.

   IF SELECT-1:NUM-ITEMS GT 0 THEN
      ASSIGN SELECT-1:SCREEN-VALUE = SELECT-1:ENTRY(1).
   APPLY "VALUE-CHANGED":U TO SELECT-1.
   APPLY "ENTRY" TO SELECT-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-progra 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-progra  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-progra  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Armar_Tmp_Excel W-progra 
PROCEDURE Armar_Tmp_Excel :
IF W_Consulta THEN
     FOR EACH Pro_Grupo WHERE Pro_Grupo.Grupo EQ W_Grupo
                          AND Pro_Grupo.Nivel EQ 1 NO-LOCK
                        BREAK BY Pro_Grupo.Grupo
                              BY Pro_Grupo.Orden:
         IF FIRST-OF(Pro_Grupo.Grupo) THEN DO:
            FIND FIRST Grupos WHERE Grupos.Grupo EQ Pro_Grupo.Grupo NO-LOCK NO-ERROR.
         END.
         ASSIGN W_Grupo = Pro_Grupo.Grupo.
         RUN Arma_Impresion_Excel(BUFFER Pro_Grupo).
         RUN Recursivo_Imp_Excel (Pro_Grupo.Nivel + 1, Pro_Grupo.Orden, Pro_Grupo.Padre, 
                                  Pro_Grupo.Orden).
     END.
  ELSE
     FOR EACH Pro_Grupo WHERE Pro_Grupo.Nivel EQ 1 NO-LOCK
                        BREAK BY Pro_Grupo.Grupo
                              BY Pro_Grupo.Orden:
         IF FIRST-OF(Pro_Grupo.Grupo) THEN DO:
            FIND FIRST Grupos WHERE Grupos.Grupo EQ Pro_Grupo.Grupo NO-LOCK NO-ERROR.
         END.
         ASSIGN W_Grupo = Pro_Grupo.Grupo.
   
         RUN Arma_Impresion_Excel(BUFFER Pro_Grupo).
         RUN Recursivo_Imp_Excel (Pro_Grupo.Nivel + 1, Pro_Grupo.Orden, Pro_Grupo.Padre, 
                            Pro_Grupo.Orden).
     END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Arma_Arbol W-progra 
PROCEDURE Arma_Arbol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/ 
  DEFINE PARAMETER BUFFER Arbol FOR Pro_Grupo.

  IF NOT Arbol.Raya THEN DO:
     FIND Programas WHERE Programas.Programa EQ Arbol.Programa NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Programas THEN DO:
        MESSAGE "Programa Eliminado" VIEW-AS ALERT-BOX.
        NEXT.
     END.
     ASSIGN W_Item = FILL(">  ", Arbol.Nivel - 1) + Programas.Opcion + FILL(" ", (70 - (3 * (Arbol.Nivel - 1)))) + "| " + STRING(Programas.Programa) + " | " + STRING(Arbol.Nivel).
  END.
  ELSE
     ASSIGN W_Item = FILL(">  ", Arbol.Nivel - 1) + "RAYA " + FILL(" ", (70 - (3 * (Arbol.Nivel - 1)))) + "| " + "" + " | " + STRING(Arbol.Nivel).
       
  ASSIGN W_Posicion = W_Posicion + 1
         W_Status   = SELECT-2:INSERT(W_Item, W_Posicion) IN FRAME {&FRAME-NAME}.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Arma_Impresion W-progra 
PROCEDURE Arma_Impresion :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE PARAMETER BUFFER Arbol FOR Pro_Grupo.

  IF NOT Arbol.Raya THEN DO:
     FIND Programas WHERE Programas.Programa EQ Arbol.Programa NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Programas THEN NEXT.
     ASSIGN W_Item = FILL(">  ", Arbol.Nivel - 1) + " (" + STRING(Programas.Programa) + ") " + Programas.Opcion.
  END.
  ELSE
     ASSIGN W_Item = FILL(">  ", Arbol.Nivel - 1) + "RAYA ".

  IF AVAILABLE Programas THEN
     ASSIGN SUBSTRING(W_Item, 50, 25) = TRIM(Programas.Ejecutable).
     
  DISPLAY W_Item WITH FRAME F_Imprime.
  DOWN 1 WITH FRAME F_Imprime.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Arma_Impresion_Excel W-progra 
PROCEDURE Arma_Impresion_Excel :
DEFINE PARAMETER BUFFER Arbol FOR Pro_Grupo.

  IF NOT Arbol.Raya THEN DO:
     FIND Programas WHERE Programas.Programa EQ Arbol.Programa NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Programas THEN NEXT.
     ASSIGN W_Item = FILL(">  ", Arbol.Nivel - 1) + " (" + STRING(Programas.Programa) + ") " + Programas.Opcion.
  END.
  ELSE
     ASSIGN W_Item = FILL(">  ", Arbol.Nivel - 1) + "RAYA ".

  CREATE Tmp_Ex.
  ASSIGN Tmp_Ex.Campo1 = W_Item
         Tmp_Ex.Campo2 = "".
  IF AVAILABLE Programas THEN
         Tmp_Ex.Campo2 = Programas.Ejecutable.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar W-progra 
PROCEDURE Asignar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE VARIABLE W_Origen_Rama LIKE Pro_Grupo.Origen_Rama.
   DEFINE VARIABLE W_NivelAnt    LIKE Pro_Grupo.Nivel.
   DEFINE VARIABLE W_Orden       LIKE Pro_Grupo.Orden  EXTENT 7 INITIAL 0.
   DEFINE VARIABLE W_Padre       LIKE Pro_Grupo.Padre  EXTENT 7 INITIAL 0.
   DEFINE VARIABLE W_Abuelo      LIKE Pro_Grupo.Abuelo EXTENT 7 INITIAL 0.

   FOR EACH Tmp:
       DELETE Tmp.
   END.
   DO I = 1 TO 7:
      ASSIGN W_Abuelo[i] = 0
             W_Padre[i]  = 0
             W_Orden[i]  = 0.
   END.

   ASSIGN W_NivelAnt = ?.
   DO I = 1 TO SELECT-2:NUM-ITEMS IN FRAME {&FRAME-NAME}:
      ASSIGN W_Item = SELECT-2:ENTRY(i)
             W_NivelItem = INTEGER(ENTRY(3,W_Item, "|"))
             W_NivelPadre = INTEGER(ENTRY(3,SELECT-2:ENTRY(I - 1), "|")).

      IF (    I EQ 1 
          AND W_NivelItem NE 1)
      OR ((W_NivelItem - W_NivelPadre) GT 1)
      OR (    I EQ 1
          AND LOOKUP("RAYA", W_Item, " ") GT 0) THEN DO:
          ASSIGN SELECT-2:SCREEN-VALUE = W_Item.
          RUN MostrarMensaje IN W_Manija (INPUT 381,OUTPUT W_Rpta).          
          RETURN ERROR.
      END.  

      IF  LOOKUP("RAYA", SELECT-2:ENTRY(I - 1), " ") GT 0 
      AND (W_NivelItem - W_NivelPadre) GE 1 THEN DO:
          ASSIGN SELECT-2:SCREEN-VALUE = W_Item.
          RUN MostrarMensaje IN W_Manija (INPUT 381,OUTPUT W_Rpta).          
          RETURN ERROR.
      END.

      IF I EQ 1 THEN DO:
         ASSIGN W_Programa = INTEGER(ENTRY(2,W_Item, "|")).
         FIND Programas WHERE Programas.Program EQ W_Programa NO-LOCK NO-ERROR.
         IF  Programa.Tipo THEN DO:
             ASSIGN SELECT-2:SCREEN-VALUE = W_Item.
             RUN MostrarMensaje IN W_Manija (INPUT 381,OUTPUT W_Rpta).          
             RETURN ERROR.
         END.
      END.

      IF  LOOKUP("RAYA", SELECT-2:ENTRY(I - 1), " ") LE 0 THEN DO:
          ASSIGN W_Programa = INTEGER(ENTRY(2,SELECT-2:ENTRY(I - 1), "|")). /* valida sobre el Padre */
          FIND Programas WHERE Programas.Program EQ W_Programa NO-LOCK NO-ERROR.
          IF  Programa.Tipo 
          AND (W_NivelItem - W_NivelPadre) GE 1 THEN DO:
              IF  Programas.Ejecutable NE "w-taquil.r"
              AND Programas.Ejecutable NE "w-sercli.r" THEN DO:
                  ASSIGN SELECT-2:SCREEN-VALUE = W_Item.
                  RUN MostrarMensaje IN W_Manija (INPUT 381,OUTPUT W_Rpta).          
                  RETURN ERROR.
              END.
          END.
      END.

      IF W_NivelItem EQ W_NivelAnt THEN
         ASSIGN W_Orden[W_NivelItem] = W_Orden[W_NivelItem] + 1.
      ELSE
         ASSIGN W_Orden[W_NivelItem] = IF W_NivelItem GT W_NivelAnt THEN 1 ELSE W_Orden[W_NivelItem] + 1.

      ASSIGN W_NivelAnt = W_NivelItem.

      IF W_NivelItem EQ 1 THEN
         ASSIGN W_Origen_Rama = W_Orden[W_NivelItem].

      IF W_NivelItem = 1 THEN
         ASSIGN W_Padre[W_NivelItem]  = 0
                W_Abuelo[W_NivelItem] = 0.
      ELSE
         ASSIGN W_Abuelo[W_NivelItem] = W_Padre[W_NivelItem - 1].

      ASSIGN W_Padre[W_NivelItem + 1] = W_Orden[W_NivelItem]. 

      CREATE Tmp.
      ASSIGN Nivel        = W_NivelItem
             Origen_Rama  = IF W_NivelItem EQ 1 THEN 0 ELSE W_Origen_Rama
             Abuelo       = W_Abuelo[W_NivelItem]
             Padre        = W_Padre[W_NivelItem]
             Orden        = W_Orden[W_NivelItem]
             Tmp.Programa = INTEGER(ENTRY(2,W_Item, "|")).
             Raya         = IF LOOKUP("RAYA", W_Item, " ") GT 0 THEN YES ELSE NO.
   END.
   FOR EACH Pro_Grupo WHERE Pro_Grupo.Grupo EQ W_Grupo EXCLUSIVE-LOCK:
       FIND Tmp WHERE Tmp.Nivel       EQ Pro_Grupo.Nivel
                  AND Tmp.Origen_Rama EQ Pro_Grupo.Origen_Rama
                  AND Tmp.Abuelo      EQ Pro_Grupo.Abuelo                  
                  AND Tmp.Padre       EQ Pro_Grupo.Padre
                  AND Tmp.Orden       EQ Pro_Grupo.Orden NO-ERROR.
       IF NOT AVAILABLE Tmp THEN
          DELETE Pro_Grupo.
       ELSE DO:
          IF Tmp.Raya NE Pro_Grupo.Raya THEN
             ASSIGN Pro_Grupo.Raya     = Tmp.Raya
                    Pro_Grupo.Programa = 0.
          IF Tmp.Programa NE Pro_Grupo.Programa THEN DO:
             ASSIGN Pro_Grupo.Programa = Tmp.Programa.
          END.
          DELETE Tmp.
       END.     
   END.
   FOR EACH Tmp:
       CREATE Pro_Grupo.
       ASSIGN Pro_Grupo.Grupo       = W_Grupo
              Pro_Grupo.Nivel       = Tmp.Nivel
              Pro_Grupo.Origen_Rama = Tmp.Origen_Rama
              Pro_Grupo.Abuelo      = Tmp.Abuelo
              Pro_Grupo.Padre       = Tmp.Padre
              Pro_Grupo.Orden       = Tmp.Orden
              Pro_Grupo.Raya        = Tmp.Raya
              Pro_Grupo.Programa    = Tmp.Programa.
       DELETE Tmp.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra_Item W-progra 
PROCEDURE Borra_Item :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT  PARAMETER P_Lista    AS WIDGET-HANDLE NO-UNDO.
  DEFINE INPUT  PARAMETER P_Valor    AS CHARACTER     NO-UNDO.

  DEFINE VARIABLE W_Posicion         AS INTEGER       NO-UNDO.
  DEFINE VARIABLE W_Valor            AS CHARACTER     NO-UNDO.
  DEFINE VARIABLE W_NumItems         AS INTEGER       NO-UNDO.

  ASSIGN W_Posicion = P_Lista:LOOKUP(P_Valor)
         W_Status   = P_Lista:DELETE(P_Valor)
         W_NumItems = P_Lista:NUM-ITEMS.

  IF W_NumItems GT 0 THEN DO:
     IF W_Posicion > W_NumItems THEN
        W_Posicion = W_Posicion - 1.

     ASSIGN W_Valor = P_Lista:ENTRY(W_Posicion)
            P_Lista:SCREEN-VALUE = W_Valor.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Borra_Plantilla W-progra 
PROCEDURE Borra_Plantilla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DO TRANSACTION:
      FOR EACH Pro_Grupo WHERE Pro_Grupo.Grupo EQ W_Grupo:
          DELETE Pro_Grupo.
      END.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Copia_Grupo W-progra 
PROCEDURE Copia_Grupo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE BUFFER Aux_Pro_Grupo FOR Pro_Grupo.
   
   FOR EACH Tmp_Pro_Grupo WHERE Tmp_Pro_Grupo.Grupo EQ W_GrupoAux:
       DELETE Tmp_Pro_Grupo.
   END.
   
   FOR EACH Aux_Pro_Grupo WHERE Aux_Pro_Grupo.Grupo EQ W_Grupo NO-LOCK:
       CREATE Tmp_Pro_Grupo.
       CREATE Temporal.
       BUFFER-COPY Aux_Pro_Grupo TO Temporal.
       ASSIGN Temporal.Grupo = W_GrupoAux.
       BUFFER-COPY Temporal TO Tmp_Pro_Grupo.
       DELETE Temporal.
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Deshabilita_Objetos W-progra 
PROCEDURE Deshabilita_Objetos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN Btn_Add:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
          Btn_del:SENSITIVE      = FALSE 
          Btn_Rul:SENSITIVE      = FALSE
          Btn_Up:SENSITIVE       = FALSE
          Btn_Dwn:SENSITIVE      = FALSE
          Btn_Fwd:SENSITIVE      = FALSE
          Btn_Bck:SENSITIVE      = FALSE
          Btn_Salva:SENSITIVE    = FALSE
          Btn_Borrar:SENSITIVE   = FALSE
          Btn_Cancela:SENSITIVE  = FALSE
          Btn_Copia:SENSITIVE    = FALSE
          Btn_Salir:SENSITIVE    = FALSE
          Btn_Consulta:SENSITIVE = FALSE
          Btn_Imprimir:SENSITIVE = FALSE
          Btn_Ayuda:SENSITIVE    = FALSE
          SELECT-1:SENSITIVE     = FALSE
          SELECT-2:SENSITIVE     = FALSE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-progra  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-progra)
  THEN DELETE WIDGET W-progra.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-progra  _DEFAULT-ENABLE
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
  DISPLAY W_Ejecutable SELECT-1 W_StrEjecuta W_StrNoEjecuta W_StrTodos SELECT-2 
          W_Grupo W_Nombre 
      WITH FRAME F-Main IN WINDOW W-progra.
  IF AVAILABLE Programas THEN 
    DISPLAY Programas.Proceso Programas.Tipo Programas.Descripcion 
          Programas.Id_Procesos Programas.Programa Programas.Nom_Fisico 
          Programas.Ejecutable 
      WITH FRAME F-Main IN WINDOW W-progra.
  ENABLE W_Ejecutable SELECT-1 W_StrEjecuta W_StrNoEjecuta W_StrTodos 
         Programas.Proceso Btn_Add Btn_del Btn_Rul Programas.Tipo 
         Programas.Descripcion Btn_Up Btn_Bck Btn_Dwn Btn_Fwd 
         Programas.Id_Procesos SELECT-2 BUTTON-44 Btn_Imprimir Btn_Consulta 
         Btn_Salva Btn_Borrar Btn_Cancela Btn_Copia Btn_Salir Btn_Ayuda W_Grupo 
         W_Nombre Programas.Programa Programas.Nom_Fisico Programas.Ejecutable 
         IMAGE-4 RECT-125 RECT-129 RECT-230 
      WITH FRAME F-Main IN WINDOW W-progra.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY RADIO-SET-1 
      WITH FRAME FRAME-A IN WINDOW W-progra.
  ENABLE BROWSE-1 Btn_Acepta RADIO-SET-1 Btn_CancelaCp Btn_SaleCp 
      WITH FRAME FRAME-A IN WINDOW W-progra.
  {&OPEN-BROWSERS-IN-QUERY-FRAME-A}
  VIEW W-progra.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Habilita_Objetos W-progra 
PROCEDURE Habilita_Objetos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN Btn_Add:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE
          Btn_del:SENSITIVE      = TRUE 
          Btn_Rul:SENSITIVE      = TRUE
          Btn_Up:SENSITIVE       = TRUE
          Btn_Dwn:SENSITIVE      = TRUE
          Btn_Fwd:SENSITIVE      = TRUE
          Btn_Bck:SENSITIVE      = TRUE
          Btn_Salva:SENSITIVE    = TRUE
          Btn_Borrar:SENSITIVE   = TRUE
          Btn_Cancela:SENSITIVE  = TRUE
          Btn_Copia:SENSITIVE    = TRUE
          Btn_Salir:SENSITIVE    = TRUE
          Btn_Consulta:SENSITIVE = TRUE
          Btn_Imprimir:SENSITIVE = TRUE
          Btn_Ayuda:SENSITIVE    = TRUE
          SELECT-1:SENSITIVE     = TRUE
          SELECT-2:SENSITIVE     = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-progra 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 E_NumFila = 1.
 E_NumColumn = 2.
 E_Fila      = "050" + "Nombre Opcion                                     "
             + "040" + "Programas Ejecutable                    ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).
 /* launch Excel so it is visible to the user */
 chExcelApp:Visible = TRUE.

 /* create a new Workbook */
 chWorkbook = chExcelApp:Workbooks:Add().

 /* get the active Worksheet */
 chWorkSheet = chExcelApp:Sheets:Item(1).

 RUN Armar_Tmp_Excel.
 FOR EACH Tmp_Ex:
    E_Fila2  = "".
    E_Fila2  = "050" + STRING(Campo1,"X(50)")
             + "040" + STRING(Campo2,"X(40)").
    {Incluido\imprimir_Excel.i}
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializa W-progra 
PROCEDURE Inicializa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  ASSIGN SELECT-1:LIST-ITEMS IN FRAME {&FRAME-NAME} = ""
         W_StrEjecuta:LIST-ITEMS   = ""
         W_StrNoEjecuta:LIST-ITEMS = ""
         W_StrTodos:LIST-ITEMS     = "".
  FOR EACH Programas WHERE Programas.Estado  EQ 1 
                       AND (   NOT Programas.Id_Procesos
                            OR Programas.Proceso NE 3) NO-LOCK BY Programas.Opcion:
      FIND FIRST Pro_Grupo WHERE Pro_Grupo.Grupo    EQ W_Grupo
                             AND Pro_Grupo.Programa EQ Programas.Programa 
                             AND Pro_Grupo.Grupo    NE 0 NO-ERROR.
      IF NOT AVAILABLE Pro_Grupo THEN DO:
         ASSIGN W_Item   = Programas.Opcion + FILL(" ", 70) + "| " + STRING(Programas.Programa)
                W_Status = W_StrTodos:ADD-LAST(W_Item).  

         IF Programas.Tipo THEN
            ASSIGN W_Status = W_StrEjecuta:ADD-LAST(W_Item).
         ELSE
            ASSIGN W_Status = W_StrNoEjecuta:ADD-LAST(W_Item).
      END.
  END.
  
  ASSIGN SELECT-1:LIST-ITEMS = W_StrTodos:LIST-ITEMS.
  APPLY "VALUE-CHANGED":U TO W_Ejecutable.

  ASSIGN Programas.Proceso:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE
         Programas.Tipo:SENSITIVE        = FALSE 
         Programas.Id_Procesos:SENSITIVE = FALSE.
  RUN Recupera_Grupo.
  
  IF SELECT-1:NUM-ITEMS GT 0 THEN
     ASSIGN SELECT-1:SCREEN-VALUE = SELECT-1:ENTRY(1).

  IF SELECT-2:NUM-ITEMS GT 0 THEN
     ASSIGN SELECT-2:SCREEN-VALUE = SELECT-2:ENTRY(1).

  APPLY "VALUE-CHANGED":U TO SELECT-1.
  APPLY "VALUE-CHANGED":U TO SELECT-2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-progra 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  /* Dispatch standard ADM method.                             */
  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN  W_StrEjecuta:HIDDEN IN FRAME {&FRAME-NAME} = TRUE
          W_StrNoEjecuta:HIDDEN = TRUE
          W_StrTodos:HIDDEN     = TRUE.

  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario
                  /*AND Usuarios.Agencia EQ W_Agencia*/
                  AND Usuarios.Estado  EQ 1 NO-LOCK.

  FIND Grupos WHERE Grupos.Grupo EQ Usuarios.Grupo NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Grupos THEN DO:
     FIND FIRST Grupos WHERE Grupos.Estado EQ 1 NO-LOCK NO-ERROR.
     IF NOT AVAILABLE Grupos THEN DO:
        MESSAGE "No existen Grupos Disponibles" VIEW-AS ALERT-BOX.
        APPLY "CHOOSE" TO Btn_Salir.
     END.
  END.
  
  ASSIGN W_Grupo:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Grupos.Grupo, "99")
         W_Grupo
         W_Nombre:SCREEN-VALUE = Grupos.Nombre
         W_PrimeraVez = TRUE.

  RUN Inicializa.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-progra 
PROCEDURE ProcesoImprimir :
W_Reporte    = "REPORTE   : OPCIONES DE PERFILES - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "        OPCIONES DEL MENU                          EJECUTABLE".

    FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
    IF AVAILABLE(Usuarios) THEN W_NomUsuEnc = Usuarios.Nombre.
    ELSE W_NomUsuEnc = "Usuario No Encontrado".
    FIND Estacion WHERE Estacion.Estacion EQ W_Estacion NO-LOCK NO-ERROR.
    IF AVAILABLE(Estacion) THEN W_NomEstEnc = Estacion.Descripcion.
    ELSE W_NomEstEnc = "Estacion No Encontrada".

    ASSIGN W_Linea     = FILL(W_Raya,130)
           W_Linea2    = FILL(W_Raya,130)
           W_Ubicacion = "UBICACION : " + TRIM(W_Nom_Agencia)
           W_IdReporta = "USUARIO   : " + W_Usuario + " - " + TRIM(W_NomUsuEnc)
           W_IdEstacion = "ESTACION  : " + STRING(INTEGER(W_Estacion)) + " - " + W_NomEstEnc
           W_PiePagina = TRIM(W_Nom_Agencia) + " / " + STRING(TODAY) + " / " + STRING(TIME,"hh:mm am").
 
  FORM HEADER
        W_Nom_Entidad AT 2
        "PAGINA:"     AT 90 PAGE-NUMBER FORMAT ">>>9"
        W_Ubicacion   AT 2
        W_IdEstacion  AT 2
        W_IdReporta   AT 2
        W_Reporte     AT 2
        W_Linea       AT 1 FORMAT "X(100)"
       "GRUPO: "      AT 2
        Pro_Grupo.Grupo AT 10
        Grupos.Nombre AT 15
        W_EncColumna  AT 2
        W_Linea2      AT 1 FORMAT "X(100)"
     WITH WIDTH 132 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.
          
  FORM HEADER 
        "________________________________________" AT 2
        W_PiePagina AT 2
/*       "FECHA:"            AT 2 
       TODAY               AT 10
       W_Nom_Agencia       AT 25 FORMAT "X(30)"
       "HORA:"             AT 65 STRING(TIME,"HH:MM AM")*/
     WITH FRAME F-Ftr PAGE-BOTTOM USE-TEXT STREAM-IO.

  IF W_Consulta THEN
     FOR EACH Pro_Grupo WHERE Pro_Grupo.Grupo EQ W_Grupo
                          AND Pro_Grupo.Nivel EQ 1 NO-LOCK
                        BREAK BY Pro_Grupo.Grupo
                              BY Pro_Grupo.Orden WITH FRAME F_Imprime:
         VIEW FRAME F-Encabezado.
         VIEW FRAME F-Ftr.
         IF FIRST-OF(Pro_Grupo.Grupo) THEN DO:
            FIND FIRST Grupos WHERE Grupos.Grupo EQ Pro_Grupo.Grupo NO-LOCK NO-ERROR.
            PAGE.
         END.
         ASSIGN W_Grupo = Pro_Grupo.Grupo.
   
         RUN Arma_Impresion(BUFFER Pro_Grupo).
         RUN Recursivo_Imp (Pro_Grupo.Nivel + 1, Pro_Grupo.Orden, Pro_Grupo.Padre, 
                            Pro_Grupo.Orden).
         DOWN 1 WITH FRAME F_Imprime.
     END.
  ELSE
     FOR EACH Pro_Grupo WHERE Pro_Grupo.Nivel EQ 1 NO-LOCK
                        BREAK BY Pro_Grupo.Grupo
                              BY Pro_Grupo.Orden WITH FRAME F_Imprime:
         VIEW FRAME F-Encabezado.
         VIEW FRAME F-ftr.

         IF FIRST-OF(Pro_Grupo.Grupo) THEN DO:
            FIND FIRST Grupos WHERE Grupos.Grupo EQ Pro_Grupo.Grupo NO-LOCK NO-ERROR.
            PAGE.
         END.
         ASSIGN W_Grupo = Pro_Grupo.Grupo.
   
         RUN Arma_Impresion(BUFFER Pro_Grupo).
         RUN Recursivo_Imp (Pro_Grupo.Nivel + 1, Pro_Grupo.Orden, Pro_Grupo.Padre, 
                            Pro_Grupo.Orden).
         DOWN 1 WITH FRAME F_Imprime.
     END.
  PAGE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recupera_Grupo W-progra 
PROCEDURE Recupera_Grupo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   ASSIGN SELECT-2:LIST-ITEMS IN FRAME {&FRAME-NAME} = ""
          W_Posicion = 0.

   FOR EACH Pro_Grupo WHERE Pro_Grupo.Grupo EQ W_Grupo
                        AND Pro_Grupo.Nivel EQ 1 NO-LOCK
                         BY Pro_Grupo.Orden:
        RUN Arma_Arbol(BUFFER Pro_Grupo).
        RUN Recursivo (Pro_Grupo.Nivel + 1, Pro_Grupo.Orden, 
                       Pro_Grupo.Padre, Pro_Grupo.Orden).
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recursivo W-progra 
PROCEDURE Recursivo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER P_Nivel       LIKE Pro_Grupo.Nivel.
  DEFINE INPUT PARAMETER P_Origen_Rama LIKE Pro_Grupo.Origen_Rama.  
  DEFINE INPUT PARAMETER P_Abuelo      LIKE Pro_Grupo.Abuelo.
  DEFINE INPUT PARAMETER P_Padre       LIKE Pro_Grupo.Padre.
  
  FOR EACH Tmp_Pro_Grupo WHERE Tmp_Pro_Grupo.Grupo       EQ W_Grupo
                           AND Tmp_Pro_Grupo.Nivel       EQ P_Nivel
                           AND Tmp_Pro_Grupo.Origen_Rama EQ P_Origen_Rama
                           AND Tmp_Pro_Grupo.Abuelo      EQ P_Abuelo                           
                           AND Tmp_Pro_Grupo.Padre       EQ P_Padre NO-LOCK
                            BY Tmp_Pro_Grupo.Orden:
      RUN Arma_Arbol(BUFFER Tmp_Pro_Grupo).
      RUN Recursivo (Tmp_Pro_Grupo.Nivel + 1, Tmp_Pro_Grupo.Origen_Rama,
                     Tmp_Pro_Grupo.Padre, Tmp_Pro_Grupo.Orden).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recursivo_Imp W-progra 
PROCEDURE Recursivo_Imp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER P_Nivel       LIKE Pro_Grupo.Nivel.
  DEFINE INPUT PARAMETER P_Origen_Rama LIKE Pro_Grupo.Origen_Rama.  
  DEFINE INPUT PARAMETER P_Abuelo      LIKE Pro_Grupo.Abuelo.
  DEFINE INPUT PARAMETER P_Padre       LIKE Pro_Grupo.Padre.
  
  FOR EACH Tmp_Pro_Grupo WHERE Tmp_Pro_Grupo.Grupo       EQ W_Grupo
                           AND Tmp_Pro_Grupo.Nivel       EQ P_Nivel
                           AND Tmp_Pro_Grupo.Origen_Rama EQ P_Origen_Rama
                           AND Tmp_Pro_Grupo.Abuelo      EQ P_Abuelo                           
                           AND Tmp_Pro_Grupo.Padre       EQ P_Padre NO-LOCK
                            BY Tmp_Pro_Grupo.Orden:
      RUN Arma_Impresion(BUFFER Tmp_Pro_Grupo).
      RUN Recursivo_Imp (Tmp_Pro_Grupo.Nivel + 1, Tmp_Pro_Grupo.Origen_Rama, 
                         Tmp_Pro_Grupo.Padre, Tmp_Pro_Grupo.Orden).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Recursivo_Imp_Excel W-progra 
PROCEDURE Recursivo_Imp_Excel :
DEFINE INPUT PARAMETER P_Nivel       LIKE Pro_Grupo.Nivel.
  DEFINE INPUT PARAMETER P_Origen_Rama LIKE Pro_Grupo.Origen_Rama.  
  DEFINE INPUT PARAMETER P_Abuelo      LIKE Pro_Grupo.Abuelo.
  DEFINE INPUT PARAMETER P_Padre       LIKE Pro_Grupo.Padre.
  
  FOR EACH Tmp_Pro_Grupo WHERE Tmp_Pro_Grupo.Grupo       EQ W_Grupo
                           AND Tmp_Pro_Grupo.Nivel       EQ P_Nivel
                           AND Tmp_Pro_Grupo.Origen_Rama EQ P_Origen_Rama
                           AND Tmp_Pro_Grupo.Abuelo      EQ P_Abuelo                           
                           AND Tmp_Pro_Grupo.Padre       EQ P_Padre NO-LOCK
                            BY Tmp_Pro_Grupo.Orden:
      RUN Arma_Impresion_Excel(BUFFER Tmp_Pro_Grupo).
      RUN Recursivo_Imp_Excel (Tmp_Pro_Grupo.Nivel + 1, Tmp_Pro_Grupo.Origen_Rama, 
                         Tmp_Pro_Grupo.Padre, Tmp_Pro_Grupo.Orden).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-progra  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.               */
  {src/adm/template/snd-head.i}

  /* For each requested table, put it's ROWID in the output list.      */
  {src/adm/template/snd-list.i "Grupos"}

  /* Deal with any unexpected table requests before closing.           */
  {src/adm/template/snd-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-progra 
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

