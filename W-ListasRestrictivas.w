&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

{Incluido/Variable.I "SHARED"}

DEFINE VARIABLE W_Ok AS LOGICAL.
DEFINE VARIABLE Puntero AS ROWID.
DEFINE VARIABLE P_Codigo LIKE ListaNegra.Codigo.
DEFINE VARIABLE P_Nit LIKE Clientes.Nit.
DEFINE VARIABLE p_Nombre LIKE Clientes.Nombre.
DEFINE VARIABLE P_Apellido1 AS CHARACTER FORMAT "X(25)".
DEFINE VARIABLE P_Apellido2 AS CHARACTER FORMAT "X(25)".

/* oakley */

  DEFINE TEMP-TABLE Tmp
    FIELD TNit LIKE Clientes.Nit
    FIELD TNom LIKE Clientes.Nombre
    FIELD TAp1 LIKE Clientes.Apellido1
    FIELD TAp2 LIKE Clientes.Apellido2.

   DEFINE TEMP-TABLE Tmplista
    FIELD Tobs LIKE ListaNegra.Observacion.

   DEFINE TEMP-TABLE tmpverifica LIKE listanegra
       INDEX observacion IS WORD-INDEX observacion .

    DEFINE VAR W_Nit  LIKE Clientes.Nit.
    DEFINE VAR W_Fec  LIKE Taquilla.Fec_Transaccion.
    
    DEFINE VAR i AS INTEGER FORMAT "99999999".
    DEFINE VAR j AS INTEGER FORMAT "99999999".
    DEFINE VAR k AS INTEGER FORMAT "99999999".

    DEFINE TEMP-TABLE vinculantes
          FIELD nro AS INTEGER
          FIELD texto AS CHAR
          INDEX idx1 IS WORD-INDEX texto .

DEFINE VAR agenciaIni AS INTEGER.
DEFINE VAR agenciaFin AS INTEGER.

/* Tabla temporal para recibir las lineas del XML */
DEFINE TEMP-TABLE listas
    FIELD nit AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Importar

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES ListaNegra

/* Definitions for FRAME F_Listas                                       */
&Scoped-define FIELDS-IN-QUERY-F_Listas ListaNegra.Nit ListaNegra.Nombre ~
ListaNegra.Apellido1 ListaNegra.Apellido2 ListaNegra.Fec_Actualizacion 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Listas ListaNegra.Nit ~
ListaNegra.Nombre ListaNegra.Apellido1 ListaNegra.Apellido2 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Listas ListaNegra
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Listas ListaNegra
&Scoped-define QUERY-STRING-F_Listas FOR EACH ListaNegra SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Listas OPEN QUERY F_Listas FOR EACH ListaNegra SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Listas ListaNegra
&Scoped-define FIRST-TABLE-IN-QUERY-F_Listas ListaNegra


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-285 BUTTON-129 Cmb_Limportar ~
Btn_Archivo Archivo BUTTON-127 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Limportar Archivo RegSubidos 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Archivo 
     IMAGE-UP FILE "imagenes/disque.jpg":U
     LABEL "Button 128" 
     SIZE 5 BY 1.35.

DEFINE BUTTON BUTTON-127 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 127" 
     SIZE 9 BY 1.88.

DEFINE BUTTON BUTTON-129 
     IMAGE-UP FILE "imagenes/proceso_g.bmp":U
     LABEL "Button 129" 
     SIZE 9 BY 1.88 TOOLTIP "Subir información del archivo".

DEFINE VARIABLE Cmb_Limportar AS CHARACTER FORMAT "X(256)":U INITIAL "00000 - Lista No Escogida" 
     LABEL "Lista a Importar" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - Lista No Escogida" 
     DROP-DOWN-LIST
     SIZE 43 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Archivo AS CHARACTER FORMAT "X(80)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RegSubidos AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Registros Subidos" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 60 BY 2.96.

DEFINE BUTTON BUTTON-130 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 130" 
     SIZE 10 BY 1.88.

DEFINE BUTTON BUTTON-131 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 131" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Cmb_IListas AS CHARACTER FORMAT "X(256)":U INITIAL "00000 - Todas las Listas" 
     LABEL "Lista a Imprimir" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - Todas las Listas" 
     DROP-DOWN-LIST
     SIZE 34 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-286
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51 BY 1.88.

DEFINE BUTTON Btn_Borrar 
     LABEL "Borrar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Deshacer 
     LABEL "&Deshacer" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_ingresar 
     LABEL "&Ingresar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Salvar 
     LABEL "&Salvar" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-132 
     LABEL "Identificación" 
     SIZE 14 BY 1.12 TOOLTIP "Proceso que Recorre la Taquilla en Busca de Operaciones Sospechosas".

DEFINE BUTTON BUTTON-158 
     LABEL "Verificación" 
     SIZE 14 BY 1.12
     FONT 2.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 12 BY 1.62.

DEFINE BUTTON BUTTON-4 
     LABEL "Importar" 
     SIZE 14 BY 1.12.

DEFINE BUTTON BUTTON-56 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 56" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE Cmb_Listas AS CHARACTER FORMAT "X(60)":U INITIAL "00000 - Lista No Asignada" 
     LABEL "Lista" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00000 - Lista No Asignada" 
     DROP-DOWN-LIST
     SIZE 59 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE IMAGE IMAGE-1
     FILENAME "imagenes/dog.bmp":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 82 BY 4.85.

DEFINE RECTANGLE RECT-270
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14 BY 10.23.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 14 BY 5.38.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY F_Listas FOR 
      ListaNegra SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Listas
     BUTTON-158 AT ROW 8.27 COL 97 WIDGET-ID 2
     Cmb_Listas AT ROW 1.54 COL 22.14 COLON-ALIGNED
     ListaNegra.Nit AT ROW 2.46 COL 22.14 COLON-ALIGNED
          LABEL "Identificación"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 
     ListaNegra.Nombre AT ROW 3.31 COL 22.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 59 BY .81
          BGCOLOR 15 
     ListaNegra.Apellido1 AT ROW 4.15 COL 22.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 59 BY .81
          BGCOLOR 15 
     ListaNegra.Apellido2 AT ROW 5 COL 22.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 59 BY .81
          BGCOLOR 15 
     BUTTON-1 AT ROW 1.54 COL 85.72
     BUTTON-2 AT ROW 3.15 COL 85.72
     BUTTON-3 AT ROW 4.77 COL 85.72
     BUTTON-4 AT ROW 6.92 COL 85.86
     ListaNegra.Fec_Actualizacion AT ROW 6.38 COL 62 COLON-ALIGNED
          LABEL "Fecha Actualización"
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Btn_Salvar AT ROW 10.81 COL 98
     Btn_Deshacer AT ROW 12.42 COL 98
     Btn_ingresar AT ROW 14.04 COL 98
     Btn_Borrar AT ROW 15.65 COL 98
     Btn_Cancelar AT ROW 17.27 COL 98
     Btn_Salir AT ROW 18.88 COL 98
     BUTTON-56 AT ROW 20.92 COL 102
     BUTTON-132 AT ROW 9.35 COL 97 WIDGET-ID 4
     IMAGE-1 AT ROW 3.69 COL 6
     RECT-1 AT ROW 1.27 COL 2
     RECT-270 AT ROW 10.54 COL 97
     RECT-3 AT ROW 1.27 COL 84.72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 21.12
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Imprimir
     BUTTON-130 AT ROW 1.19 COL 54.29
     Cmb_IListas AT ROW 2.27 COL 16 COLON-ALIGNED
     BUTTON-131 AT ROW 3.08 COL 54.29
     " Seleccione la lista que va a imprimir" VIEW-AS TEXT
          SIZE 32 BY .77 AT ROW 1.19 COL 4
          FGCOLOR 7 
     RECT-286 AT ROW 1.62 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 7.46
         SIZE 65 BY 5.12
         BGCOLOR 17 FONT 5
         TITLE "Imprimir Listas".

DEFINE FRAME F_Importar
     BUTTON-129 AT ROW 1.15 COL 62.72
     Cmb_Limportar AT ROW 1.27 COL 16 COLON-ALIGNED
     Btn_Archivo AT ROW 3.42 COL 3
     Archivo AT ROW 3.69 COL 7 COLON-ALIGNED NO-LABEL
     BUTTON-127 AT ROW 3.92 COL 63
     RegSubidos AT ROW 4.77 COL 48 COLON-ALIGNED
     "Escoja el Archivo a Importar" VIEW-AS TEXT
          SIZE 25 BY .62 AT ROW 2.62 COL 4
          FGCOLOR 7 
     RECT-285 AT ROW 2.88 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 12.85
         SIZE 75 BY 5.92
         BGCOLOR 17 FONT 5
         TITLE "Importar Lista desde Archivo PLano".


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
         TITLE              = "Mantenimiento de la Listas Vinculantes"
         HEIGHT             = 21.12
         WIDTH              = 113.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
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
/* REPARENT FRAME */
ASSIGN FRAME F_Importar:FRAME = FRAME F_Listas:HANDLE
       FRAME F_Imprimir:FRAME = FRAME F_Listas:HANDLE.

/* SETTINGS FOR FRAME F_Importar
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F_Importar:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RegSubidos IN FRAME F_Importar
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Imprimir
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Imprimir:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Listas
   Custom                                                               */
/* SETTINGS FOR FILL-IN ListaNegra.Fec_Actualizacion IN FRAME F_Listas
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN ListaNegra.Nit IN FRAME F_Listas
   EXP-LABEL                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Listas
/* Query rebuild information for FRAME F_Listas
     _TblList          = "bdCentral.ListaNegra"
     _Query            is OPENED
*/  /* FRAME F_Listas */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Mantenimiento de la Listas Vinculantes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Mantenimiento de la Listas Vinculantes */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Listas
&Scoped-define SELF-NAME ListaNegra.Apellido1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListaNegra.Apellido1 wWin
ON LEAVE OF ListaNegra.Apellido1 IN FRAME F_Listas /* Primer Apellido */
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ListaNegra.Apellido2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListaNegra.Apellido2 wWin
ON LEAVE OF ListaNegra.Apellido2 IN FRAME F_Listas /* Segundo Apellido */
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Importar
&Scoped-define SELF-NAME Btn_Archivo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Archivo wWin
ON CHOOSE OF Btn_Archivo IN FRAME F_Importar /* Button 128 */
DO:
  DEFINE VAR ProcName AS CHARACTER FORMAT "x(80)".
  SYSTEM-DIALOG GET-FILE procname
     TITLE      "Escoja el archivo que contiene la informacion ..."
     FILTERS    "Archivos delimitados por ; (*.csv)"   "*.csv"
     MUST-EXIST
     USE-FILENAME.
     
  IF Procname NE "" THEN Archivo:SCREEN-VALUE = ProcName.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Listas
&Scoped-define SELF-NAME Btn_Borrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Borrar wWin
ON CHOOSE OF Btn_Borrar IN FRAME F_Listas /* Borrar */
DO:
    DEFINE VAR W_SiErr AS LOGICAL.
    DEFINE VAR w_autorizo AS CHARACTER.

    IF AVAILABLE listaNegra THEN DO:
        RUN P-ValiDarTrans IN W_Manija (OUTPUT W_SiErr,OUTPUT W_Autorizo).

        IF W_SiErr = YES THEN DO:
            FIND FIRST usuarios WHERE usuarios.usuario = W_autorizo NO-LOCK NO-ERROR.
            IF AVAILABLE usuarios THEN DO:
                IF usuarios.prioridad < 6 THEN DO:
                    MESSAGE "El Usuario no tiene los privilegios para autorizar la Operación." SKIP
                            "No se permite realizar el retiro de la lista..."
                        VIEW-AS ALERT-BOX INFO BUTTONS OK.

                    RETURN NO-APPLY.
                END.
            END.
            ELSE DO:
                MESSAGE "Usuario inexistente. No se permite realizar el retiro de la lista..."
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.

                RETURN NO-APPLY.
            END.
        END.
        ELSE
            RETURN NO-APPLY.

        DELETE listaNegra.

        IF INTEGER(SUBSTRING(Cmb_Listas:SCREEN-VALUE,1,5)) = 4 THEN DO:
        CREATE hoja_vida.
            ASSIGN hoja_vida.tipo = 17
                   hoja_vida.codigo = 4
                   hoja_vida.nit = ListaNegra.Nit:SCREEN-VALUE
                   hoja_vida.usuario = w_usuario
                   hoja_vida.fec_grabacion = w_fecha
                   hoja_vida.observacion = "Se retira de Lista de Suspendidos"
                   hoja_vida.hora_grabacion = TIME.
        END.

        MESSAGE "El Asociado ha sido eliminado de la lista de Suspendidos."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        RUN Blanquear.
    END.
    ELSE DO:
        MESSAGE "No ha seleccionado ningún Asociado para ser" SKIP
                "borrado de la lista de Vetados"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Listas /* Cancelar */
DO:
  FIND ListaNegra WHERE ROWID(ListaNegra) EQ puntero NO-ERROR.
  IF AVAILABLE ListaNegra THEN RUN Mostrar_Sospechoso.
  ELSE DO: 
    RUN Blanquear.
    RUN Deshabilitar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Listas /* Deshacer */
DO:
  IF AVAILABLE ListaNegra THEN RUN Mostrar_Sospechoso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ingresar wWin
ON CHOOSE OF Btn_ingresar IN FRAME F_Listas /* Ingresar */
DO:
  Puntero = ROWID(ListaNegra).
  RUN Blanquear.
  RUN Habilitar.
  APPLY "entry" TO Cmb_Listas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Listas /* Salir */
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


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Listas /* Salvar */
DO:
    DO WITH FRAME F_Listas:
        IF SUBSTRING(Cmb_Listas:SCREEN-VALUE,1,5) EQ "00000" THEN DO:
            MESSAGE "Para Salvar el Registro debe elegirse" SKIP
                    "una lista con anticipación."
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO Cmb_Listas.
            RETURN NO-APPLY.
        END.

        IF ListaNegra.Nit:SCREEN-VALUE EQ "" OR ListaNegra.Apellido1:SCREEN-VALUE EQ "" THEN DO:
            MESSAGE "Es necesario entrar el nombre de la persona o empresa" SKIP
                    "o el apellido1 como minimo. Rectifique!"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "entry" TO ListaNegra.Nit.
            RETURN NO-APPLY.
        END.

        RUN Salvar_Registro.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Importar
&Scoped-define SELF-NAME BUTTON-127
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-127 wWin
ON CHOOSE OF BUTTON-127 IN FRAME F_Importar /* Button 127 */
DO:
  Archivo:SCREEN-VALUE = "".
  HIDE FRAME F_Importar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-129
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-129 wWin
ON CHOOSE OF BUTTON-129 IN FRAME F_Importar /* Button 129 */
DO:
    DEFINE VARIABLE xnom AS CHAR FORMAT "X50".
    DEFINE VARIABLE xnit LIKE clientes.nit.
    DEFINE VARIABLE xced LIKE clientes.nit.
    DEFINE VARIABLE xnom1 AS CHAR FORMAT "X50".
    DEFINE VARIABLE i AS integer.
    DEFINE VARIABLE j AS char.
    DEFINE VARIABLE xpos AS INTEGER.
    DEFINE VARIABLE cntrlval AS CHAR.
    DEFINE VAR registro AS CHARACTER.
    DEFINE VAR index1 AS INTEGER.
    DEFINE VAR index2 AS INTEGER.

    DO WITH FRAME F_Importar:
        ASSIGN Archivo.

        MESSAGE "Se cargarán los registros nuevos desde" SKIP
                Archivo:SCREEN-VALUE IN FRAME F_Importar SKIP
                "Desea continuar?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE vlpregunta AS LOGICAL.

        IF NOT vlpregunta THEN
            RETURN.

        RegSubidos = 0.

        IF INTEGER(SUBSTRING(Cmb_Limportar:SCREEN-VALUE,1,5)) EQ 00000 THEN DO:
            MESSAGE "Debe escogerse la lista a la cual" SKIP
                    "se subira información. Rectifique"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "Entry" TO Cmb_Limportar.
            RETURN NO-APPLY.
        END.

        IF Archivo:SCREEN-VALUE EQ "" THEN DO:
            MESSAGE "No se ha seleccionado ningun archivo" SKIP
                    "fuente de informacion. Rectifique"
                VIEW-AS ALERT-BOX ERROR.

            APPLY "choose" TO Btn_Archivo.
            RETURN NO-APPLY.
        END.

        FOR EACH listanegra WHERE ListaNegra.Codigo EQ INTEGER(SUBSTRING(Cmb_Limportar:SCREEN-VALUE,1,5)):
            DELETE listanegra.
        END.
        
        INPUT FROM VALUE(Archivo).
            IF INTEGER(SUBSTRING(Cmb_Limportar:SCREEN-VALUE,1,5)) = 1 THEN DO:
                REPEAT:
                    IMPORT UNFORMAT registro.
                    index1 = INDEX(registro,"<IDRegistrationNo>").

                    IF index1 > 0 THEN DO:
                        index2 = INDEX(registro,"</IDRegistrationNo>").
                        registro = SUBSTRING(registro,index1 + 18, index2 - (index1 + 18)).

                        CREATE listaNegra.
                        ListaNegra.Codigo = INTEGER(SUBSTRING(Cmb_Limportar:SCREEN-VALUE,1,5)).
                        listaNegra.estado = 1.
                        listaNegra.nit = registro.
                        ListaNegra.Usuario = W_Usuario.
                        ListaNegra.Fec_Actualizacion = TODAY.
                        ListaNegra.Id_HaVenido = NO.
                        ListaNegra.Fec_HaVenido = ?.

                        RegSubidos = RegSubidos + 1.
                    END.
                END.
            END.

            DISPLAY Regsubidos WITH FRAME F_Importar.
        INPUT CLOSE.

        IF regsubidos GT 0 THEN DO:
            FIND FIRST Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.

            CREATE LOGs.
            ASSIGN loGs.Agencia = W_agencia
                   Logs.Fecha = w_fecha
                   Logs.FechaL = W_fecha
                   Logs.HoraE = TIME
                   Logs.HoraL = TIME
                   Logs.IdRegistro = 3
                   Logs.Observacion = "Se actualiza " + SUBSTRING(Cmb_Limportar:SCREEN-VALUE,1,30) + "-Usuario- " + Usuarios.Nombre
                   Logs.Usuario = W_usuario.
        END.

        MESSAGE "Proceso de Carga finalizada"
            VIEW-AS ALERT-BOX INFO BUTTONS OK TITLE "Carga Terminada".
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Imprimir
&Scoped-define SELF-NAME BUTTON-130
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-130 wWin
ON CHOOSE OF BUTTON-130 IN FRAME F_Imprimir /* Button 130 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_PathSpl + "ListasNegras.LST".
  {incluido/Imprimir.I "Listado"} 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-131
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-131 wWin
ON CHOOSE OF BUTTON-131 IN FRAME F_Imprimir /* Button 131 */
DO:
  HIDE FRAME F_Imprimir.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Listas
&Scoped-define SELF-NAME BUTTON-132
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-132 wWin
ON CHOOSE OF BUTTON-132 IN FRAME F_Listas /* Identificación */
DO:
  VIEW FRAME F_Progreso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-158
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-158 wWin
ON CHOOSE OF BUTTON-158 IN FRAME F_Listas /* Verificación */
DO:
  DEFINE VARIABLE W_nit LIKE clientes.nit.
  DEFINE VAR vblelg AS LOGICAL.
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario  NO-ERROR.
  CREATE  LOGs.
  ASSIGN  loGs.Agencia     = W_agencia
          Logs.Fecha       = w_fecha
          Logs.FechaL      = W_fecha
          Logs.HoraE       = TIME
          Logs.HoraL       = TIME
          Logs.IdRegistro  = 3
          Logs.Observacion = "Se Revisa Base de datos contral lista clinton y lista Onu "  +  Usuarios.Nombre.
          Logs.Usuario     = W_usuario.
  DEFINE VARIABLE xnom LIKE clientes.nombre.  
  DEFINE VARIABLE leer AS CHAR.
  DEFINE VARIABLE i AS INTEGER.
  /*MESSAGE "Verifique que tiene actualizado el archivos: c:\info_juriscoop\sdn.csv." VIEW-AS ALERT-BOX.*/
  /*INPUT FROM sdn.csv.*/
/*  INPUT FROM C:\listas.csv.
  REPEAT:
      IMPORT UNFORMATTED leer.
      ASSIGN i = INDEX(leer,",").
      CREATE vinculantes.
      ASSIGN vinculantes.nro = integer(ENTRY(1,leer,","))
             vinculantes.texto = SUBSTRING(leer,i + 1). 

  END.

  INPUT CLOSE.*/
  SESSION:SET-WAIT("general").
/*   FOR EACH listanegra :                      */
/*       CREATE tmpverifica.                    */
/*       BUFFER-COPY listanegra TO tmpverifica. */
/*   END.                                       */


 /* OUTPUT TO c:\INFO_jURISCOOP\vinculantes.txt.*/
  FOR EACH clientes NO-LOCK:
      ASSIGN W_nit = clientes.nit.
      FIND first listanegra where  listanegra.nit =  W_nit AND
                                 ListaNegra.Estado         = 1 NO-ERROR.
       IF AVAILABLE(listanegra) THEN 
          ASSIGN ListaNegra.Fec_HaVenido = w_fecha
                 ListaNegra.id_havenido  = TRUE.
      ELSE ASSIGN ListaNegra.Fec_HaVenido = ?
                 ListaNegra.id_havenido  = FALSE.
  END.
  OUTPUT CLOSE.
 
   SESSION:SET-WAIT(" ").
   MESSAGE " fin - Revision"  VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Listas /* Button 2 */
DO:
ASSIGN FecIni:SCREEN-VALUE IN FRAME F_Imprimir = ""
       FecFin:SCREEN-VALUE IN FRAME F_Imprimir = "".
VIEW FRAME F_Imprimir. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON CHOOSE OF BUTTON-3 IN FRAME F_Listas /* Button 3 */
DO:
    ASSIGN WWin:SENSITIVE = FALSE.

    RUN C-ListasNegras.r(OUTPUT P_Codigo,
                         OUTPUT P_Nit,
                         OUTPUT P_Nombre,
                         OUTPUT P_Apellido1,
                         OUTPUT P_Apellido2).

    ASSIGN WWin:SENSITIVE = TRUE.
    WWin:MOVE-TO-TOP().

    FIND FIRST ListaNegra WHERE ListaNegra.Codigo EQ P_Codigo
                            AND ListaNegra.Nit EQ P_Nit NO-ERROR.
    IF AVAILABLE ListaNegra THEN
        RUN Mostrar_Sospechoso.
    ELSE
        MESSAGE "El Asociado que busca no se encuentra matriculado como Sancionado"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 wWin
ON CHOOSE OF BUTTON-4 IN FRAME F_Listas /* Importar */
DO:
    VIEW FRAME F_Importar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ListaNegra.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListaNegra.Nit wWin
ON LEAVE OF ListaNegra.Nit IN FRAME F_Listas /* Identificación */
DO:
    DO WITH FRAME F_Listas:
        FIND FIRST clientes WHERE clientes.nit = ListaNegra.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE clientes THEN DO:
            listaNegra.nombre:SCREEN-VALUE = clientes.nombre.
            listaNegra.apellido1:SCREEN-VALUE = clientes.apellido1.
            listaNegra.apellido2:SCREEN-VALUE = clientes.apellido2.
        END.

        IF AVAILABLE ListaNegra THEN DO:
            Puntero = ROWID(ListaNegra).

            FIND FIRST ListaNegra WHERE ListaNegra.Codigo EQ INTEGER(SUBSTRING(Cmb_Listas:SCREEN-VALUE,1,5))
                                    AND ListaNegra.Nit EQ ListaNegra.Nit:SCREEN-VALUE NO-ERROR.
            IF AVAILABLE ListaNegra THEN DO:
                MESSAGE "Este Número de Identificación ya se encuentra en esta lista" SKIP
                        "acontinuación se mostrará la información"
                    VIEW-AS ALERT-BOX.

                RUN Mostrar_Sospechoso.
            END.
            ELSE DO:
                FIND FIRST ListaNegra WHERE ROWID(ListaNegra) EQ Puntero NO-ERROR.

                FIND FIRST clientes WHERE clientes.nit = ListaNegra.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
                IF AVAILABLE clientes THEN DO:
                    listaNegra.nombre:SCREEN-VALUE = clientes.nombre.
                    listaNegra.apellido1:SCREEN-VALUE = clientes.apellido1.
                    listaNegra.apellido2:SCREEN-VALUE = clientes.apellido2.
                END.
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME ListaNegra.Nombre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ListaNegra.Nombre wWin
ON LEAVE OF ListaNegra.Nombre IN FRAME F_Listas /* Nombre */
DO:
  SELF:SCREEN-VALUE = CAPS(SELF:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Importar
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Blanquear wWin 
PROCEDURE Blanquear :
DO WITH FRAME F_Listas:
    ASSIGN Cmb_Listas:SCREEN-VALUE = Cmb_Listas:ENTRY(1)
           ListaNegra.Nit:SCREEN-VALUE = ""
           ListaNegra.Nombre:SCREEN-VALUE = ""
           ListaNegra.Apellido1:SCREEN-VALUE = ""
           ListaNegra.Apellido2:SCREEN-VALUE = ""
           ListaNegra.Fec_Actualizacion:SCREEN-VALUE = STRING(TODAY)
           ListaNegra.Observacion:SCREEN-VALUE = "".

    APPLY "entry" TO Cmb_Listas.
    RETURN NO-APPLY.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Deshabilitar wWin 
PROCEDURE Deshabilitar :
DO WITH FRAME F_Listas:
   DISABLE Cmb_Listas ListaNegra.Nit
           ListaNegra.Apellido1 ListaNegra.Apellido2 ListaNegra.Nombre.
END.
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

  {&OPEN-QUERY-F_Listas}
  GET FIRST F_Listas.
  DISPLAY Cmb_Listas 
      WITH FRAME F_Listas IN WINDOW wWin.
  IF AVAILABLE ListaNegra THEN 
    DISPLAY ListaNegra.Nit ListaNegra.Nombre ListaNegra.Apellido1 
          ListaNegra.Apellido2 ListaNegra.Fec_Actualizacion 
      WITH FRAME F_Listas IN WINDOW wWin.
  ENABLE BUTTON-158 Cmb_Listas ListaNegra.Nit ListaNegra.Nombre 
         ListaNegra.Apellido1 ListaNegra.Apellido2 BUTTON-1 BUTTON-2 BUTTON-3 
         BUTTON-4 Btn_Salvar Btn_Deshacer Btn_ingresar Btn_Borrar Btn_Cancelar 
         Btn_Salir BUTTON-56 BUTTON-132 IMAGE-1 RECT-1 RECT-270 RECT-3 
      WITH FRAME F_Listas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Listas}
  DISPLAY Cmb_IListas 
      WITH FRAME F_Imprimir IN WINDOW wWin.
  ENABLE RECT-286 BUTTON-130 Cmb_IListas BUTTON-131 
      WITH FRAME F_Imprimir IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Imprimir}
  DISPLAY Cmb_Limportar Archivo RegSubidos 
      WITH FRAME F_Importar IN WINDOW wWin.
  ENABLE RECT-285 BUTTON-129 Cmb_Limportar Btn_Archivo Archivo BUTTON-127 
      WITH FRAME F_Importar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Importar}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Habilitar wWin 
PROCEDURE Habilitar :
DO WITH FRAME F_Listas:
   ENABLE  Cmb_Listas ListaNegra.Nit
           ListaNegra.Apellido1 ListaNegra.Apellido2 ListaNegra.Nombre.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
    
E_NumFila = 1.
E_NumColumn = 7.

E_Fila = "005" + "Cod  " +
         "012" + "Nit         " +
         "025" + "Nombre                   " +
         "015" + "Apellido1      " +
         "015" + "Apellido2      " +
         "002" + "ID" +
         "004" + "Usu ".

RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

DEFINE VAR CIni LIKE Varios.Codigo.
DEFINE VAR CFin LIKE Varios.Codigo.
DEFINE VAR venido AS CHARACTER FORMAT "XX".

ASSIGN FRAME F_Imprimir
    Cmb_Ilistas.

IF INTEGER(SUBSTRING(Cmb_IListas,1,5)) EQ 0 THEN
    ASSIGN CIni = 0
           CFin = 99999.
ELSE
    ASSIGN CIni = INTEGER(SUBSTRING(Cmb_IListas,1,5))
           CFin = CIni.

FOR EACH ListaNegra WHERE ListaNegra.Codigo GE CIni
                      AND ListaNegra.Codigo LE CFin NO-LOCK:
    IF ListaNegra.Id_HaVenido THEN
        Venido = "SI".
    ELSE
        Venido = "NO".

    E_Fila2 = "".
    E_Fila2 = "005" + STRING(ListaNegra.Codigo,"99999") +
              "012" + STRING(ListaNegra.Nit,"X(12)") +
              "025" + STRING(ListaNegra.Nombre,"X(25)") +
              "015" + STRING(ListaNegra.Apellido1,"X(15)") +
              "015" + STRING(ListaNegra.Apellido2,"X(15)") +
              "002" + STRING(Venido,"X(2)") +
              "004" + STRING(ListaNegra.Usuario,"X(4)").

{Incluido\imprimir_Excel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Listas wWin 
PROCEDURE Informe_Listas :
{INCLUIDO\RepEncabezado.I}

W_Reporte = "REPORTE   : " + SUBSTRING(Cmb_Ilistas:SCREEN-VALUE IN FRAME F_Imprimir,9) + " - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
W_EncColumna = "AGE NIT        NOMBRE Y APELLIDOS                       COD - FEC-VCTO    SUSP-DESDE  SUSP-HASTA  ESTADO".

DEFINE VAR CIni LIKE Varios.Codigo.
DEFINE VAR CFin LIKE Varios.Codigo.
DEFINE VAR vAgencia AS INTEGER.

ASSIGN FRAME F_Imprimir
    Cmb_Ilistas.

agenciaIni = INTEGER(SUBSTRING(cmbAgencias:SCREEN-VALUE,1,2)).

IF agenciaIni > 0 THEN
    agenciaFin = agenciaIni.
ELSE
    agenciaFin = 999.

IF INTEGER(SUBSTRING(Cmb_IListas,1,5)) = 0 THEN
    ASSIGN CIni = 0
           CFin = 99999.
ELSE
    ASSIGN CIni = INTEGER(SUBSTRING(Cmb_IListas,1,5))
           CFin = CIni.

VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.

FOR EACH ListaNegra WHERE ListaNegra.Codigo GE CIni
                      AND ListaNegra.Codigo LE CFin
                      AND listaNegra.fec_actualizacion >= fecIni
                      AND listaNegra.fec_actualizacion <= fecFin NO-LOCK BY listaNegra.fec_actualizacion
                                                                         BY listaNegra.fec_exclusion:
    FIND FIRST clientes WHERE clientes.nit = listaNegra.nit NO-LOCK NO-ERROR.
    IF AVAILABLE clientes THEN
        vAgencia = clientes.agencia.
    ELSE
        vAgencia = 0.

    IF vAgencia >= agenciaIni AND vAgencia <= agenciaFin THEN DO:
        DISPLAY vAgencia                        AT 1 FORMAT "99"
                ListaNegra.Nit                  AT 5 FORMAT "X(10)"
                ListaNegra.Nombre + " " + ListaNegra.Apellido1 + " " + ListaNegra.Apellido2 AT 16 FORMAT "X(40)"
                listaNegra.observacion          AT 57 FORMAT "X(16)"
                listaNegra.fec_actualizacion    AT 75 FORMAT "99/99/9999"
                listaNegra.fec_exclusion        AT 87 FORMAT "99/99/9999"
                listaNegra.estado               AT 101
                
            WITH DOWN WIDTH 140 FRAME F-lista USE-TEXT STREAM-IO NO-LABELS NO-BOX.
    END.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FIND FIRST Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.

DO WITH FRAME F_Listas:
    FOR EACH Varios WHERE varios.Tipo EQ 17:
        ASSIGN W_Ok = Cmb_Listas:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion)
               W_Ok = Cmb_Limportar:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME F_Importar
               W_Ok = Cmb_IListas:ADD-LAST(STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion) IN FRAME F_Imprimir.
    END.

    FOR EACH agencias NO-LOCK BY agencias.agencia:
        W_Ok = cmbAgencias:ADD-LAST(STRING(agencias.agencia,"99") + " - " + agencias.nombre).
    END.
END.

RUN SUPER.

/*FIND FIRST ListaNegra NO-LOCK NO-ERROR.*/
/*IF AVAILABLE ListaNegra THEN DO:
    RUN Mostrar_Sospechoso.
END.
ELSE*/
    RUN Blanquear.

RUN Deshabilitar.

HIDE FRAME F_Importar FRAME F_Imprimir.

DO WITH FRAME F_Imprimir:
    ASSIGN FecIni:SCREEN-VALUE = STRING(TODAY)
           FecFin:SCREEN-VALUE = STRING(TODAY).
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Marcar_Dia wWin 
PROCEDURE Marcar_Dia :
FOR EACH Taquilla WHERE 
          Taquilla.Nit                   EQ W_Nit  AND
          Taquilla.Fec_Transaccion       EQ W_Fec  AND
          SUBSTRING(Taquilla.Cuenta,1,4) NE "1904" AND
          SUBSTRING(Taquilla.Cuenta,1,4) NE "2705" AND
          Taquilla.Id_NUD                EQ NO:
          j = j + 1.
          RUN Progreso.
          ASSIGN Taquilla.Id_NUD  = YES
                 Taquilla.Fec_GNU = TODAY
                 Taquilla.Usu_GNU = Taquilla.Usuario.
 END.
 FIND Taquilla WHERE ROWID(Taquilla) EQ Puntero.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Marcar_Mes wWin 
PROCEDURE Marcar_Mes :
FOR EACH Taquilla WHERE 
          Taquilla.Nit              EQ W_Nit        AND
    YEAR(Taquilla.Fec_Transaccion)  EQ YEAR(W_Fec)  AND
    MONTH(Taquilla.Fec_Transaccion) EQ MONTH(W_Fec) AND
    SUBSTRING(Taquilla.Cuenta,1,4)  NE "1904"       AND
    SUBSTRING(Taquilla.Cuenta,1,4)  NE "2705"       AND
          Taquilla.Id_NUM           EQ NO:
    j = j + 1.
    RUN Progreso.
    ASSIGN Taquilla.Id_NUM = YES
           Taquilla.Fec_GNUM = TODAY
           Taquilla.Usu_GNUM = Taquilla.Usuario.
 END.
 FIND Taquilla WHERE ROWID(Taquilla) EQ Puntero.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Marcar_Segmento wWin 
PROCEDURE Marcar_Segmento :
FOR EACH Taquilla WHERE 
          Taquilla.Nit              EQ W_Nit AND
    MONTH(Taquilla.Fec_Transaccion) EQ MONTH(W_Fec) AND
          Taquilla.Id_NUS           EQ NO:
    j = j + 1.
    RUN Progreso.
    ASSIGN Taquilla.Id_NUS = YES.
 END.
 FIND Taquilla WHERE ROWID(Taquilla) EQ Puntero.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Sospechoso wWin 
PROCEDURE Mostrar_Sospechoso :
DO WITH FRAME F_Listas:
    FIND FIRST Varios WHERE Varios.Tipo EQ 17
                        AND Varios.Codigo EQ ListaNegra.Codigo NO-LOCK NO-ERROR.
    IF AVAILABLE Varios THEN
        Cmb_Listas:SCREEN-VALUE = STRING(Varios.Codigo,"99999") + " - " + Varios.Descripcion.
    ELSE
        Cmb_Listas:SCREEN-VALUE = Cmb_Listas:ENTRY(1).

    ASSIGN ListaNegra.Nit:SCREEN-VALUE = ListaNegra.Nit
           ListaNegra.Nombre:SCREEN-VALUE = ListaNegra.Nombre
           ListaNegra.Apellido1:SCREEN-VALUE = ListaNegra.Apellido1
           ListaNegra.Apellido2:SCREEN-VALUE = ListaNegra.Apellido2
           ListaNegra.Fec_Actualizacion:SCREEN-VALUE = STRING(ListaNegra.Fec_Actualizacion)
           ListaNegra.Observacion:SCREEN-VALUE = ListaNegra.Observacion.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
ASSIGN FRAME F_Imprimir
    FecIni
    FecFin
    cmbAgencias
    cmb_IListas.

RUN Informe_listas.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Progreso wWin 
PROCEDURE Progreso :
DO WITH FRAME F_Progreso:
  IF j EQ 250 THEN DO:
        j = 0.
        k = k + 1.
        ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
               R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15.
               R9:BGCOLOR = 15.
        CASE k:
          WHEN 1 THEN
             R1:BGCOL = 18.
          WHEN 2 THEN
             R2:BGCOL = 18.
          WHEN 3 THEN
             R3:BGCOL = 18.
          WHEN 4 THEN
             R4:BGCOL = 18.
          WHEN 5 THEN
             R5:BGCOL = 18.
          WHEN 6 THEN
             R6:BGCOL = 18.
          WHEN 7 THEN
             R7:BGCOL = 18.
          WHEN 8 THEN
             R8:BGCOL = 18.
          WHEN 9 THEN
             R9:BGCOL = 18.
        END CASE.
        IF k = 9 THEN k = 0.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salvar_Registro wWin 
PROCEDURE Salvar_Registro :
DO WITH FRAME F_Listas:
    FIND FIRST listaNegra WHERE listaNegra.nit = listaNegra.nit:SCREEN-VALUE NO-ERROR.
    IF NOT AVAILABLE listaNegra THEN
        CREATE ListaNegra.

    ASSIGN ListaNegra.Codigo = INTEGER(SUBSTRING(Cmb_Listas:SCREEN-VALUE,1,5))
           ListaNegra.Nit = ListaNegra.Nit:SCREEN-VALUE
           ListaNegra.Nombre = ListaNegra.Nombre:SCREEN-VALUE
           ListaNegra.Estado = INTEGER(ListaNegra.Estado)
           ListaNegra.Apellido1 = ListaNegra.Apellido1:SCREEN-VALUE
           ListaNegra.Apellido2 = ListaNegra.Apellido2:SCREEN-VALUE
           ListaNegra.Fec_Actualizacion = DATE(ListaNegra.Fec_Actualizacion:SCREEN-VALUE)
           ListaNegra.Usuario = W_Usuario
           ListaNegra.observacion = listaNegra.observacion:SCREEN-VALUE.
        
    IF INTEGER(SUBSTRING(Cmb_Listas:SCREEN-VALUE,1,5)) = 4 THEN DO:
        FIND FIRST hoja_vida WHERE hoja_vida.nit = ListaNegra.Nit:SCREEN-VALUE
                               AND hoja_vida.tipo = 17
                               AND hoja_vida.codigo = 4
                               AND hoja_vida.usuario = w_usuario
                               AND hoja_vida.fec_grabacion = w_fecha NO-LOCK NO-ERROR.
        IF NOT AVAILABLE hoja_vida THEN DO:
            CREATE hoja_vida.

            ASSIGN hoja_vida.tipo = 17
                   hoja_vida.codigo = 4
                   hoja_vida.nit = ListaNegra.Nit:SCREEN-VALUE
                   hoja_vida.usuario = w_usuario
                   hoja_vida.fec_grabacion = w_fecha
                   hoja_vida.observacion = "Se registra en Lista de Suspendidos"
                   hoja_vida.hora_grabacion = TIME.
        END.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

