&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

{Incluido\variable.i "shared"}

DEFINE TEMP-TABLE tt
    FIELD referencia AS CHARACTER
    FIELD tipo_producto AS CHARACTER
    FIELD cod_producto AS CHARACTER
    FIELD num_producto AS CHARACTER
    FIELD nit AS CHARACTER
    FIELD valor AS DECIMAL.

DEFINE VAR vSec AS INTEGER.
DEFINE VAR fechaArchivoDate AS DATE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME Frm-Main
&Scoped-define BROWSE-NAME brwExportados

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Recaudos_IO

/* Definitions for BROWSE brwExportados                                 */
&Scoped-define FIELDS-IN-QUERY-brwExportados Recaudos_IO.fecha ~
STRING(hora,"HH:MM:SS") Recaudos_IO.tipo_convenio Recaudos_IO.nombreArchivo ~
Recaudos_IO.usuario 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwExportados 
&Scoped-define QUERY-STRING-brwExportados FOR EACH Recaudos_IO ~
      WHERE Recaudos_IO.tipo_io = "O" NO-LOCK ~
    BY Recaudos_IO.fecha DESCENDING ~
       BY Recaudos_IO.hora DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwExportados OPEN QUERY brwExportados FOR EACH Recaudos_IO ~
      WHERE Recaudos_IO.tipo_io = "O" NO-LOCK ~
    BY Recaudos_IO.fecha DESCENDING ~
       BY Recaudos_IO.hora DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwExportados Recaudos_IO
&Scoped-define FIRST-TABLE-IN-QUERY-brwExportados Recaudos_IO


/* Definitions for BROWSE brwImportados                                 */
&Scoped-define FIELDS-IN-QUERY-brwImportados Recaudos_IO.fecha ~
STRING(hora,"HH:MM:SS") Recaudos_IO.tipo_convenio Recaudos_IO.nombreArchivo ~
Recaudos_IO.usuario 
&Scoped-define ENABLED-FIELDS-IN-QUERY-brwImportados 
&Scoped-define QUERY-STRING-brwImportados FOR EACH Recaudos_IO ~
      WHERE Recaudos_IO.tipo_io = "I" NO-LOCK ~
    BY Recaudos_IO.fecha DESCENDING ~
       BY Recaudos_IO.hora DESCENDING INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-brwImportados OPEN QUERY brwImportados FOR EACH Recaudos_IO ~
      WHERE Recaudos_IO.tipo_io = "I" NO-LOCK ~
    BY Recaudos_IO.fecha DESCENDING ~
       BY Recaudos_IO.hora DESCENDING INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-brwImportados Recaudos_IO
&Scoped-define FIRST-TABLE-IN-QUERY-brwImportados Recaudos_IO


/* Definitions for FRAME Frm-Main                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frm-Main ~
    ~{&OPEN-QUERY-brwExportados}~
    ~{&OPEN-QUERY-brwImportados}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-345 RECT-346 RECT-347 RECT-348 ~
brwExportados btnExportarPSE btnExportarReferenciado brwImportados ~
btnAplicarPSE btnAplicarReferenciado Btn_Done 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON btnAplicarPSE 
     LABEL "PSE" 
     SIZE 14 BY 1.5.

DEFINE BUTTON btnAplicarReferenciado 
     LABEL "Referenciado" 
     SIZE 14 BY 1.5.

DEFINE BUTTON btnExportarPSE 
     LABEL "PSE" 
     SIZE 14 BY 1.5.

DEFINE BUTTON btnExportarReferenciado 
     LABEL "Referenciado" 
     SIZE 14 BY 1.5.

DEFINE BUTTON Btn_Done DEFAULT 
     IMAGE-UP FILE "imagenes/salir3.bmp":U
     LABEL "&Salir" 
     SIZE 14 BY 1.69 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE RECTANGLE RECT-345
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.43 BY 10.23
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE RECT-346
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 88.43 BY 10.23
     BGCOLOR 8 FGCOLOR 0 .

DEFINE RECTANGLE RECT-347
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 4.31.

DEFINE RECTANGLE RECT-348
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 4.31.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY brwExportados FOR 
      Recaudos_IO SCROLLING.

DEFINE QUERY brwImportados FOR 
      Recaudos_IO SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE brwExportados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwExportados C-Win _STRUCTURED
  QUERY brwExportados NO-LOCK DISPLAY
      Recaudos_IO.fecha COLUMN-LABEL "Fecha" FORMAT "99/99/9999":U
      STRING(hora,"HH:MM:SS") COLUMN-LABEL "hora" FORMAT "X(8)":U
      Recaudos_IO.tipo_convenio COLUMN-LABEL "Tipo Convenio" FORMAT "x(12)":U
      Recaudos_IO.nombreArchivo COLUMN-LABEL "Nombre de archivo" FORMAT "x(60)":U
            WIDTH 45
      Recaudos_IO.usuario COLUMN-LABEL "Usuario" FORMAT "x(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 9.35
         FONT 4
         TITLE "Listado de archivos exportados" FIT-LAST-COLUMN.

DEFINE BROWSE brwImportados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS brwImportados C-Win _STRUCTURED
  QUERY brwImportados NO-LOCK DISPLAY
      Recaudos_IO.fecha COLUMN-LABEL "Fecha" FORMAT "99/99/9999":U
      STRING(hora,"HH:MM:SS") COLUMN-LABEL "hora" FORMAT "X(8)":U
      Recaudos_IO.tipo_convenio COLUMN-LABEL "Tipo de Convenio" FORMAT "x(12)":U
      Recaudos_IO.nombreArchivo COLUMN-LABEL "Nombre de archivo" FORMAT "x(60)":U
            WIDTH 45
      Recaudos_IO.usuario COLUMN-LABEL "Usuario" FORMAT "x(25)":U
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 86 BY 9.35
         FONT 4
         TITLE "Listado de archivos importados" FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm-Main
     brwExportados AT ROW 2.15 COL 4 WIDGET-ID 200
     btnExportarPSE AT ROW 2.42 COL 93.14 WIDGET-ID 186
     btnExportarReferenciado AT ROW 4 COL 93.14 WIDGET-ID 194
     brwImportados AT ROW 13.27 COL 4 WIDGET-ID 300
     btnAplicarPSE AT ROW 13.54 COL 93.14 WIDGET-ID 188
     btnAplicarReferenciado AT ROW 15.15 COL 93.14 WIDGET-ID 204
     Btn_Done AT ROW 21.23 COL 93.14 WIDGET-ID 20
     " Aplicar" VIEW-AS TEXT
          SIZE 9.57 BY .81 AT ROW 12.38 COL 93 WIDGET-ID 202
          FGCOLOR 0 
     " Exportar" VIEW-AS TEXT
          SIZE 9.57 BY .81 AT ROW 1.19 COL 93 WIDGET-ID 198
          FGCOLOR 0 
     " Archivos exportados" VIEW-AS TEXT
          SIZE 20.86 BY .81 AT ROW 1.19 COL 3.14 WIDGET-ID 4
          FGCOLOR 0 
     " Archivos importados" VIEW-AS TEXT
          SIZE 20.86 BY .81 AT ROW 12.31 COL 3.14 WIDGET-ID 192
          FGCOLOR 0 
     RECT-345 AT ROW 1.54 COL 2.57 WIDGET-ID 2
     RECT-346 AT ROW 12.65 COL 2.57 WIDGET-ID 190
     RECT-347 AT ROW 1.54 COL 92 WIDGET-ID 196
     RECT-348 AT ROW 12.73 COL 92 WIDGET-ID 200
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 108 BY 22.23
         BGCOLOR 17  WIDGET-ID 100.


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
         TITLE              = "Administración Convenio de Recaudo"
         HEIGHT             = 22.23
         WIDTH              = 108
         MAX-HEIGHT         = 26.77
         MAX-WIDTH          = 194.86
         VIRTUAL-HEIGHT     = 26.77
         VIRTUAL-WIDTH      = 194.86
         RESIZE             = yes
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
/* SETTINGS FOR FRAME Frm-Main
   FRAME-NAME                                                           */
/* BROWSE-TAB brwExportados RECT-348 Frm-Main */
/* BROWSE-TAB brwImportados btnExportarReferenciado Frm-Main */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwExportados
/* Query rebuild information for BROWSE brwExportados
     _TblList          = "bdcentral.Recaudos_IO"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "bdcentral.Recaudos_IO.fecha|no,bdcentral.Recaudos_IO.hora|no"
     _Where[1]         = "Recaudos_IO.tipo_io = ""O"""
     _FldNameList[1]   > bdcentral.Recaudos_IO.fecha
"Recaudos_IO.fecha" "Fecha" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"STRING(hora,""HH:MM:SS"")" "hora" "X(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > bdcentral.Recaudos_IO.tipo_convenio
"Recaudos_IO.tipo_convenio" "Tipo Convenio" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > bdcentral.Recaudos_IO.nombreArchivo
"Recaudos_IO.nombreArchivo" "Nombre de archivo" "x(60)" "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > bdcentral.Recaudos_IO.usuario
"Recaudos_IO.usuario" "Usuario" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwExportados */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE brwImportados
/* Query rebuild information for BROWSE brwImportados
     _TblList          = "bdcentral.Recaudos_IO"
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _OrdList          = "bdcentral.Recaudos_IO.fecha|no,bdcentral.Recaudos_IO.hora|no"
     _Where[1]         = "Recaudos_IO.tipo_io = ""I"""
     _FldNameList[1]   > bdcentral.Recaudos_IO.fecha
"Recaudos_IO.fecha" "Fecha" ? "date" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[2]   > "_<CALC>"
"STRING(hora,""HH:MM:SS"")" "hora" "X(8)" ? ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[3]   > bdcentral.Recaudos_IO.tipo_convenio
"Recaudos_IO.tipo_convenio" "Tipo de Convenio" ? "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[4]   > bdcentral.Recaudos_IO.nombreArchivo
"Recaudos_IO.nombreArchivo" "Nombre de archivo" "x(60)" "character" ? ? ? ? ? ? no ? no no "45" yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _FldNameList[5]   > bdcentral.Recaudos_IO.usuario
"Recaudos_IO.usuario" "Usuario" "x(25)" "character" ? ? ? ? ? ? no ? no no ? yes no no "U" "" "" "" "" "" "" 0 no 0 no no
     _Query            is OPENED
*/  /* BROWSE brwImportados */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Administración Convenio de Recaudo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Administración Convenio de Recaudo */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAplicarPSE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAplicarPSE C-Win
ON CHOOSE OF btnAplicarPSE IN FRAME Frm-Main /* PSE */
DO:
    DEFINE VAR vcNomArchInput AS CHARACTER NO-UNDO.
    DEFINE VAR vlOkPressed AS LOGICAL.
    
    DEFINE VAR vTime AS INTEGER.
    DEFINE VAR cont AS INTEGER.
    DEFINE VAR registro AS CHARACTER.
    DEFINE VAR fechaArchivo AS CHARACTER.
    DEFINE VAR flagImportar AS LOGICAL.
    
    EMPTY TEMP-TABLE tt.

    /* Se selecciona el archivo a importar */
    SYSTEM-DIALOG GET-FILE vcNomArchInput
        TITLE "Selección Archivo de Recaudos..."
        FILTERS "Texto (*.txt)" "*.*"
        MUST-EXIST
        USE-FILENAME
        UPDATE vlOkPressed.

    IF vcNomArchInput <> "" THEN DO:
        MESSAGE "Se va a importar y aplicar los movimientos detallados en el archivo. Está seguro que desea" SKIP
                "realizar esta operación...?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Aplicar movimientos?" UPDATE flagImportar.

        IF flagImportar THEN DO:
            Aplicacion:
            DO TRANSACTION ON ERROR UNDO Aplicacion:
                INPUT FROM VALUE(vcNomArchInput).
                REPEAT:
                    cont = cont + 1.
                    registro = "".

                    IMPORT registro.

                    IF cont = 1 THEN DO:
                        FIND FIRST recaudos_IO WHERE recaudos_IO.tipo_io = "I"
                                                 AND recaudos_IO.tipo_convenio = "PSE"
                                                 AND recaudos_IO.encabezado = registro NO-LOCK NO-ERROR.
                        IF AVAILABLE recaudos_IO THEN DO:
                            MESSAGE "Este archivo ya fue importado anteriormente. No se" SKIP
                                    "permite la carga de un mismo archivo más de una ves."
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.

                            RETURN NO-APPLY.
                        END.
                        
                        fechaArchivo = SUBSTRING(registro,13,8).

                        FIND FIRST recaudos_IO WHERE recaudos_IO.tipo_io = "I"
                                                 AND recaudos_IO.tipo_convenio = "PSE"
                                                 AND recaudos_IO.fecha = DATE(INTEGER(SUBSTRING(fechaArchivo,5,2)),INTEGER(SUBSTRING(fechaArchivo,7,2)),INTEGER(SUBSTRING(fechaArchivo,1,4))) NO-LOCK NO-ERROR.
                        IF AVAILABLE recaudos_IO THEN DO:
                            MESSAGE "Ya se encuentra un archivo aplicado correspondiente a esta fecha." SKIP
                                    "Está seguro de realizar esta operación...?"
                                VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Aplicar movimientos?" UPDATE flagImportar.

                            IF flagImportar = NO THEN
                                RETURN NO-APPLY.
                        END.

                        CREATE recaudos_IO.
                        recaudos_IO.tipo_io = "I".
                        recaudos_io.tipo_convenio = "PSE".
                        recaudos_io.encabezado = registro.
                        recaudos_IO.fecha = DATE(INTEGER(SUBSTRING(fechaArchivo,5,2)),INTEGER(SUBSTRING(fechaArchivo,7,2)),INTEGER(SUBSTRING(fechaArchivo,1,4))).
                        recaudos_IO.nombreArchivo = vcNomArchInput.
                        recaudos_IO.hora = TIME.
                        recaudos_IO.usuario = w_usuario.
                        
                        fechaArchivoDate = recaudos_io.fecha.

                        RUN hallarDiaHabil (INPUT "PSE").
                    END.
                    ELSE DO:
                        IF SUBSTRING(registro,1,2) = "06" THEN DO:
                            CREATE tt.
                            tt.referencia = STRING(DECIMAL(SUBSTRING(registro,3,18))).
                            tt.nit = STRING(DECIMAL(SUBSTRING(registro,21,30))).
                            tt.valor = DECIMAL((SUBSTRING(registro,51,12))).
                        END.
                    END.
                END.
            

                FOR EACH tt NO-LOCK:
                    FIND FIRST comprobantes WHERE comprobantes.agencia = INTEGER(SUBSTRING(tt.referencia,1,1))
                                              AND comprobantes.comprobante = 15 NO-ERROR.
                    IF AVAILABLE comprobantes THEN DO:
                        comprobantes.secuencia = comprobantes.secuencia + 1.
                        vSec = comprobantes.secuencia.
                    END.

                    RELEASE comprobantes.

                    IF SUBSTRING(tt.referencia,7,1) = "1"  THEN
                        RUN aplicarAportes(INPUT "PSE").

                    IF SUBSTRING(tt.referencia,7,1) = "2"  THEN
                        RUN aplicarCreditos(INPUT "PSE").
                END.
            END.

            MESSAGE "El proceso fue realizado con éxito"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            OPEN QUERY brwImportados FOR EACH Recaudos_IO WHERE recaudos_IO.tipo_io = "I" NO-LOCK BY Recaudos_IO.fecha DESCENDING
                                                                                                  BY Recaudos_IO.hora DESCENDING INDEXED-REPOSITION.

        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnAplicarReferenciado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnAplicarReferenciado C-Win
ON CHOOSE OF btnAplicarReferenciado IN FRAME Frm-Main /* Referenciado */
DO:
    DEFINE VAR vcNomArchInput AS CHARACTER NO-UNDO.
    DEFINE VAR vlOkPressed AS LOGICAL.
    
    DEFINE VAR vTime AS INTEGER.
    DEFINE VAR cont AS INTEGER.
    DEFINE VAR registro AS CHARACTER.
    DEFINE VAR fechaArchivo AS CHARACTER.
    
    EMPTY TEMP-TABLE tt.

    /* Se selecciona el archivo a importar */
    SYSTEM-DIALOG GET-FILE vcNomArchInput
        TITLE "Selección Archivo de Recaudos..."
        FILTERS "Texto (*.txt)" "*.*"
        MUST-EXIST
        USE-FILENAME
        UPDATE vlOkPressed.

    IF vcNomArchInput <> "" THEN DO:
        MESSAGE "Se va a importar y aplicar los movimientos detallados en el archivo. Está seguro que desea" SKIP
                "realizar esta operación...?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Aplicar movimientos?" UPDATE flagImportar AS LOGICAL.

        IF flagImportar THEN DO:
            Aplicacion:
            DO TRANSACTION ON ERROR UNDO Aplicacion:
                INPUT FROM VALUE(vcNomArchInput).
                REPEAT:
                    cont = cont + 1.
                    registro = "".

                    IMPORT registro.

                    IF cont = 1 THEN DO:
                        fechaArchivo = SUBSTRING(registro,13,8).

                        FIND FIRST recaudos_IO WHERE recaudos_IO.tipo_io = "I"
                                                 AND recaudos_IO.tipo_convenio = "Referenciado"
                                                 AND recaudos_IO.fecha = DATE(INTEGER(SUBSTRING(fechaArchivo,5,2)),INTEGER(SUBSTRING(fechaArchivo,7,2)),INTEGER(SUBSTRING(fechaArchivo,1,4))) NO-LOCK NO-ERROR.
                        IF AVAILABLE recaudos_IO THEN DO:
                            MESSAGE "Ya se encuentra un archivo aplicado correspondiente a esta fecha." SKIP
                                    "No se permite su aplicación."
                                VIEW-AS ALERT-BOX INFO BUTTONS OK.

                            RETURN NO-APPLY.
                        END.
                        ELSE DO:
                            CREATE recaudos_IO.
                            recaudos_IO.tipo_io = "I".
                            recaudos_io.tipo_convenio = "Referenciado".
                            recaudos_IO.fecha = DATE(INTEGER(SUBSTRING(fechaArchivo,5,2)),INTEGER(SUBSTRING(fechaArchivo,7,2)),INTEGER(SUBSTRING(fechaArchivo,1,4))).
                            recaudos_IO.nombreArchivo = vcNomArchInput.
                            recaudos_IO.hora = TIME.
                            recaudos_IO.usuario = w_usuario.

                            fechaArchivoDate = recaudos_io.fecha.

                            RUN hallarDiaHabil (INPUT "Referenciado").
                        END.
                    END.
                    ELSE DO:
                        IF SUBSTRING(registro,1,2) = "06" THEN DO:
                            CREATE tt.
                            tt.nit = STRING(DECIMAL(SUBSTRING(registro,3,24))).
                            tt.referencia = STRING(DECIMAL(SUBSTRING(registro,27,24))).
                            tt.valor = DECIMAL((SUBSTRING(registro,51,12))).
                        END.
                    END.
                END.

                FOR EACH tt NO-LOCK:
                    FIND FIRST comprobantes WHERE comprobantes.agencia = INTEGER(SUBSTRING(tt.referencia,1,1))
                                              AND comprobantes.comprobante = 15 NO-ERROR.
                    IF AVAILABLE comprobantes THEN DO:
                        comprobantes.secuencia = comprobantes.secuencia + 1.
                        vSec = comprobantes.secuencia.
                    END.

                    RELEASE comprobantes.

                    FIND FIRST ahorros WHERE ahorros.nit = tt.nit
                                         AND ahorros.cod_ahorro = INTEGER(tt.referencia) NO-LOCK NO-ERROR.
                    IF AVAILABLE ahorros THEN
                        RUN aplicarAportes(INPUT "REF").
                    ELSE DO:
                        FIND FIRST creditos WHERE creditos.nit = tt.nit
                                              AND creditos.num_credito = INTEGER(tt.referencia) NO-LOCK NO-ERROR.
                        IF AVAILABLE creditos THEN
                            RUN aplicarCreditos(INPUT "REF").
                    END.
                END.
            END.

            MESSAGE "El proceso fue realizado con éxito"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

            OPEN QUERY brwImportados FOR EACH Recaudos_IO WHERE recaudos_IO.tipo_io = "I" NO-LOCK BY Recaudos_IO.fecha DESCENDING
                                                                                                  BY Recaudos_IO.hora DESCENDING INDEXED-REPOSITION.

        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExportarPSE
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExportarPSE C-Win
ON CHOOSE OF btnExportarPSE IN FRAME Frm-Main /* PSE */
DO:
    MESSAGE "Se va a generar el archivo con la información con destino" SKIP
            "a Davivienda (PSE). Está seguro que desea continuar...?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Exportar archivo?" UPDATE flagExportar AS LOGICAL.

    IF flagExportar THEN DO:
        RUN reporteSalidaPSE.r (INPUT w_usuario).

        MESSAGE "El reporte fue exportado con éxito"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        OPEN QUERY brwExportados FOR EACH Recaudos_IO WHERE recaudos_IO.tipo_io = "O" NO-LOCK BY Recaudos_IO.fecha DESCENDING
                                                                                           BY Recaudos_IO.hora DESCENDING INDEXED-REPOSITION.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btnExportarReferenciado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btnExportarReferenciado C-Win
ON CHOOSE OF btnExportarReferenciado IN FRAME Frm-Main /* Referenciado */
DO:
    MESSAGE "Se va a generar el archivo con la información con destino" SKIP
            "a Davivienda (Referenciado). Está seguro que desea continuar...?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Exportar archivo?" UPDATE flagExportar AS LOGICAL.

    IF flagExportar THEN DO:
        RUN reporteSalidaReferenciado.r (INPUT w_usuario).

        MESSAGE "El reporte fue exportado con éxito"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        OPEN QUERY brwExportados FOR EACH Recaudos_IO WHERE recaudos_IO.tipo_io = "O" NO-LOCK BY Recaudos_IO.fecha DESCENDING
                                                                                           BY Recaudos_IO.hora DESCENDING INDEXED-REPOSITION.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Done
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Done C-Win
ON CHOOSE OF Btn_Done IN FRAME Frm-Main /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME brwExportados
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
  
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aplicarAportes C-Win 
PROCEDURE aplicarAportes :
DEFINE INPUT PARAMETER pOrigen AS CHARACTER.

DEFINE VAR cuentaSyA AS CHARACTER.
DEFINE VAR valorAporte AS DECIMAL.
DEFINE VAR valorAhPermanente AS DECIMAL.
DEFINE VAR pResult AS LOGICAL.

valorAporte = ROUND(tt.valor * 0.2,0).
valorAhPermanente = tt.valor - valorAporte.

IF pOrigen = "PSE" THEN
    FIND FIRST ahorros WHERE ahorros.tip_ahorro = 4
                         AND ahorros.cod_ahorro = 2
                         AND ahorros.nit = tt.nit
                         AND SUBSTRING(ahorros.cue_ahorros,1,7) = STRING(INTEGER(SUBSTRING(tt.referencia,8)))
                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.
ELSE DO:
    IF pOrigen = "REF" THEN
        FIND FIRST ahorros WHERE ahorros.nit = tt.nit
                             AND ahorros.cod_ahorro = INTEGER(tt.referencia)
                             AND ahorros.estado = 1 NO-LOCK NO-ERROR.
END.

IF AVAILABLE ahorros THEN DO:
    FIND FIRST clientes WHERE clientes.nit = ahorros.nit NO-LOCK NO-ERROR.

    RUN ConsignacionAportes.r(INPUT ahorros.agencia,
                              INPUT ahorros.cod_ahorro,
                              INPUT ahorros.nit,
                              INPUT ahorros.nit,
                              INPUT ahorros.cue_ahorros,
                              INPUT valorAporte,
                              INPUT 0,
                              INPUT 15,
                              INPUT vSec,
                              INPUT "Pago por " + pOrigen,
                              INPUT clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2,
                              OUTPUT pResult).

    CREATE mov_contable.
    RUN movContable (INPUT pOrigen).
    ASSIGN Mov_Contable.Agencia = ahorros.agencia
           Mov_Contable.Destino = ahorros.agencia
           Mov_Contable.Cuenta = "11100509"
           Mov_Contable.Nit = ahorros.nit
           Mov_contable.db = valorAporte
           mov_contable.cr = 0.

    /* Para Sucursales y Agencias */
    IF ahorros.agencia <> 1 THEN DO:
        FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
        IF AVAILABLE pro_ahorros THEN DO:
            FIND FIRST cortoLargo WHERE cortoLargo.agencia = ahorros.agencia
                                    AND cortoLargo.clase = 1
                                    AND cortoLargo.cod_producto = pro_ahorros.cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE cortoLargo THEN
                cuentaSyA = cortoLargo.Cta_SYA.
        END.

        mov_contable.cuenta = cuentaSyA.
        mov_contable.nit = "001".

        FIND FIRST comprobante WHERE comprobante.comprobante = 15
                                 AND comprobantes.agencia = 1 NO-ERROR.
        IF AVAILABLE(comprobante) THEN DO:
            comprobantes.secuencia = comprobante.secuencia + 1.
            vSec = comprobante.secuencia.
        END.

        RELEASE comprobantes.

        CREATE mov_contable.
        RUN movContable (INPUT pOrigen).
        ASSIGN Mov_Contable.Agencia = 1
               Mov_Contable.Destino = 1
               Mov_Contable.Cuenta = cuentaSyA
               Mov_Contable.Nit = STRING(ahorros.agencia,"999")
               mov_contable.cr = valorAporte.

        CREATE mov_contable.
        RUN movContable (INPUT pOrigen).
        ASSIGN Mov_Contable.Agencia = 1
               Mov_Contable.Destino = 1
               Mov_Contable.Cuenta = "11100509"
               Mov_Contable.Nit = ahorros.nit
               Mov_contable.db = valorAporte.
    END.

    FIND FIRST ahorros WHERE ahorros.tip_ahorro = 2
                         AND ahorros.cod_ahorro = 3
                         AND ahorros.nit = tt.nit
                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        RUN ConsignacionAhorroAlaVista.r(INPUT ahorros.agencia,
                                         INPUT ahorros.cod_ahorro,
                                         INPUT ahorros.nit,
                                         INPUT ahorros.nit,
                                         INPUT ahorros.cue_ahorros,
                                         INPUT valorAhPermanente,
                                         INPUT 0,
                                         INPUT 15,
                                         INPUT vSec,
                                         INPUT "Pago por " + pOrigen,
                                         INPUT clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2,
                                         OUTPUT pResult).

        CREATE mov_contable.
        RUN movContable (INPUT pOrigen).
        ASSIGN Mov_Contable.Agencia = ahorros.agencia
               Mov_Contable.Destino = ahorros.agencia
               Mov_Contable.Cuenta = "11100509"
               Mov_Contable.Nit = ahorros.nit
               Mov_contable.db = valorAhPermanente
               mov_contable.cr = 0.

        /* Para Sucursales y Agencias */
        IF ahorros.agencia <> 1 THEN DO:
            FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE pro_ahorros THEN DO:
                FIND FIRST cortoLargo WHERE cortoLargo.agencia = ahorros.agencia
                                        AND cortoLargo.clase = 1
                                        AND cortoLargo.cod_producto = pro_ahorros.cod_ahorro NO-LOCK NO-ERROR.
                IF AVAILABLE cortoLargo THEN
                    cuentaSyA = cortoLargo.Cta_SYA.
            END.

            mov_contable.cuenta = cuentaSyA.
            mov_contable.nit = "001".

            FIND FIRST comprobante WHERE comprobante.comprobante = 15
                                     AND comprobantes.agencia = 1 NO-ERROR.
            IF AVAILABLE(comprobante) THEN DO:
                comprobantes.secuencia = comprobante.secuencia + 1.
                vSec = comprobante.secuencia.
            END.

            RELEASE comprobantes.

            CREATE mov_contable.
            RUN movContable (INPUT pOrigen).
            ASSIGN Mov_Contable.Agencia = 1
                   Mov_Contable.Destino = 1
                   Mov_Contable.Cuenta = cuentaSyA
                   Mov_Contable.Nit = STRING(ahorros.agencia,"999")
                   mov_contable.cr = valorAhPermanente.

            CREATE mov_contable.
            RUN movContable (INPUT pOrigen).
            ASSIGN Mov_Contable.Agencia = 1
                   Mov_Contable.Destino = 1
                   Mov_Contable.Cuenta = "11100509"
                   Mov_Contable.Nit = ahorros.nit
                   Mov_contable.db = valorAhPermanente.
        END.

    END.
    ELSE DO:
        FIND FIRST ahorros WHERE ahorros.tip_ahorro = 2
                             AND ahorros.cod_ahorro = 3
                             AND ahorros.nit = tt.nit
                             AND ahorros.estado = 2 NO-LOCK NO-ERROR.
        IF AVAILABLE ahorros THEN
            MESSAGE "Error en la aplicación del abono a Ahorro Permanente:" SKIP
                    "Identificación:" ahorros.nit SKIP
                    "Número de cuenta:" ahorros.cue_ahorros SKIP
                    "Valor a abonar:" STRING(valorAhPermanente,"$>>>,>>>,>>9.99") "(" + STRING(tt.valor,"$>>>,>>>,>>9.99") + ")" SKIP(2)
                    "Este valor será llevado a la cuenta de Aportes."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ELSE
            MESSAGE "No se encontró la cuenta de Ahorro Permanente solicitada." SKIP
                    "No se recibe la consignación." SKIP
                    "Cédula:" tt.nit SKIP
                    "Valor a abonar:" STRING(valorAhPermanente,"$>>>,>>>,>>9.99") "(" + STRING(tt.valor,"$>>>,>>>,>>9.99") + ")" SKIP(2)
                    "Este valor será llevado a la cuenta de Aportes."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.

        FIND FIRST ahorros WHERE ahorros.tip_ahorro = 4
                             AND ahorros.cod_ahorro = 2
                             AND ahorros.nit = tt.nit
                             AND SUBSTRING(ahorros.cue_ahorros,1,7) = STRING(INTEGER(SUBSTRING(tt.referencia,8)))
                             AND ahorros.estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            RUN ConsignacionAportes.r(INPUT ahorros.agencia,
                                      INPUT ahorros.cod_ahorro,
                                      INPUT ahorros.nit,
                                      INPUT ahorros.nit,
                                      INPUT ahorros.cue_ahorros,
                                      INPUT valorAhPermanente,
                                      INPUT 0,
                                      INPUT 15,
                                      INPUT vSec,
                                      INPUT "Pago por " + pOrigen,
                                      INPUT clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2,
                                      OUTPUT pResult).

            CREATE mov_contable.
            RUN movContable (INPUT pOrigen).
            ASSIGN Mov_Contable.Agencia = ahorros.agencia
                   Mov_Contable.Destino = ahorros.agencia
                   Mov_Contable.Cuenta = "11100509"
                   Mov_Contable.Nit = ahorros.nit
                   Mov_contable.db = valorAhPermanente
                   mov_contable.cr = 0.

            /* Para Sucursales y Agencias */
            IF ahorros.agencia <> 1 THEN DO:
                FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
                IF AVAILABLE pro_ahorros THEN DO:
                    FIND FIRST cortoLargo WHERE cortoLargo.agencia = ahorros.agencia
                                            AND cortoLargo.clase = 1
                                            AND cortoLargo.cod_producto = pro_ahorros.cod_ahorro NO-LOCK NO-ERROR.
                    IF AVAILABLE cortoLargo THEN
                        cuentaSyA = cortoLargo.Cta_SYA.
                END.

                mov_contable.cuenta = cuentaSyA.
                mov_contable.nit = "001".

                FIND FIRST comprobante WHERE comprobante.comprobante = 15
                                         AND comprobantes.agencia = 1 NO-ERROR.
                IF AVAILABLE(comprobante) THEN DO:
                    comprobantes.secuencia = comprobante.secuencia + 1.
                    vSec = comprobante.secuencia.
                END.

                RELEASE comprobantes.

                CREATE mov_contable.
                RUN movContable (INPUT pOrigen).
                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = cuentaSyA
                       Mov_Contable.Nit = STRING(ahorros.agencia,"999")
                       mov_contable.cr = valorAhPermanente.

                CREATE mov_contable.
                RUN movContable (INPUT pOrigen).
                ASSIGN Mov_Contable.Agencia = 1
                       Mov_Contable.Destino = 1
                       Mov_Contable.Cuenta = "11100509"
                       Mov_Contable.Nit = ahorros.nit
                       Mov_contable.db = valorAhPermanente.
            END.
        END.
    END.
END.
ELSE DO:
    FIND FIRST ahorros WHERE ahorros.tip_ahorro = 4
                         AND ahorros.cod_ahorro = 2
                         AND ahorros.nit = tt.nit
                         AND SUBSTRING(ahorros.cue_ahorros,1,7) = STRING(INTEGER(SUBSTRING(tt.referencia,8)))
                         AND ahorros.estado = 2 NO-LOCK NO-ERROR.
    IF AVAILABLE ahorros THEN
        MESSAGE "Error en la aplicación del abono a Aportes:" SKIP
                "Identificación:" ahorros.nit SKIP
                "Número de cuenta:" ahorros.cue_ahorros SKIP
                "Valor a abonar:" STRING(tt.valor,"$>>>,>>>,>>9.99") + ")" SKIP(2)
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ELSE
        MESSAGE "No se encontró la cuenta de Aportes solicitada." SKIP
                "No se recibe la consignación." SKIP
                "Cédula:" tt.nit SKIP
                "Número de cuenta:" STRING(INTEGER(SUBSTRING(tt.referencia,8))) SKIP
                "Valor a abonar:" STRING(tt.valor,"$>>>,>>>,>>9.99") + ")" SKIP(2)
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE aplicarCreditos C-Win 
PROCEDURE aplicarCreditos :
DEFINE INPUT PARAMETER pOrigen AS CHARACTER.

DEFINE VAR P_Poliza AS DECIMAL.
DEFINE VAR P_Honora AS DECIMAL.
DEFINE VAR P_Costas AS DECIMAL.
DEFINE VAR P_SeguroVida AS DECIMAL.
DEFINE VAR P_SeguroDeudor AS DECIMAL.
DEFINE VAR P_IMorDifC AS DECIMAL.
DEFINE VAR P_IMora AS DECIMAL.
DEFINE VAR P_IDifCob AS DECIMAL.
DEFINE VAR P_ICte AS DECIMAL.
DEFINE VAR P_IAntic AS DECIMAL.
DEFINE VAR P_Capit AS DECIMAL.
DEFINE VAR P_VlrNoDist AS DECIMAL.
DEFINE VAR P_IAC AS DECIMAL.
DEFINE VAR cuentaSyA AS CHARACTER.
DEFINE VAR pResult AS LOGICAL.
DEFINE VAR flagAhorro AS LOGICAL.

FIND FIRST clientes WHERE clientes.nit = tt.nit NO-LOCK NO-ERROR.

P_VlrNoDist = tt.valor.

IF pOrigen = "PSE" THEN
    FIND FIRST creditos WHERE creditos.nit = tt.nit
                          AND SUBSTRING(STRING(creditos.num_credito),1,7) = STRING(INTEGER(SUBSTRING(tt.referencia,8)))
                          AND creditos.estado = 2 NO-LOCK NO-ERROR.
ELSE DO:
    IF pOrigen = "REF" THEN DO:
        FIND FIRST creditos WHERE creditos.nit = tt.nit
                              AND creditos.num_credito = INTEGER(tt.referencia)
                              AND creditos.estado = 2 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE creditos THEN DO:
            FIND FIRST creditos WHERE creditos.nit = tt.nit
                                  AND creditos.cod_credito = INTEGER(tt.referencia)
                                  AND creditos.estado = 2 NO-LOCK NO-ERROR.
        END.
    END.
END.

IF AVAILABLE creditos THEN DO:
    RUN p-PagoCredito.r(INPUT YES,
                        INPUT Creditos.Cod_Credito,
                        INPUT Creditos.Nit,
                        INPUT Creditos.Num_Credito,
                        INPUT tt.valor,
                        INPUT 15,
                        INPUT vSec,
                        INPUT 0,
                        INPUT 1,
                        INPUT fechaArchivoDate,
                        INPUT FALSE,
                        OUTPUT P_Poliza,
                        OUTPUT P_Honora,
                        OUTPUT P_Costas,
                        OUTPUT P_SeguroVida,
                        OUTPUT P_SeguroDeudor,
                        OUTPUT P_IMorDifC,
                        OUTPUT P_IMora,
                        OUTPUT P_IDifCob,
                        OUTPUT P_ICte,
                        OUTPUT P_IAntic,
                        OUTPUT P_Capit,
                        OUTPUT P_VlrNoDist,
                        OUTPUT pResult).

    IF tt.valor - P_VlrNoDist > 0 THEN DO:
        CREATE mov_contable.
        RUN movContable (INPUT pOrigen).
        ASSIGN Mov_Contable.Agencia = creditos.agencia
               Mov_Contable.Destino = 1
               Mov_Contable.Cuenta = "11100509"
               Mov_Contable.Nit = tt.nit
               Mov_contable.db = tt.valor - P_VlrNoDist
               mov_contable.cr = 0.

        /* Para Sucursales y Agencias */
        IF creditos.agencia <> 1 THEN DO:
            FIND FIRST pro_creditos WHERE pro_creditos.cod_credito = creditos.cod_credito NO-LOCK NO-ERROR.
            IF AVAILABLE pro_creditos THEN DO:
                FIND FIRST cortoLargo WHERE cortoLargo.agencia = creditos.agencia
                                        AND cortoLargo.clase = 2
                                        AND cortoLargo.cod_producto = pro_creditos.cod_credito NO-LOCK NO-ERROR.
                IF AVAILABLE cortoLargo THEN
                    cuentaSyA = cortoLargo.Cta_SYA.
            END.

            mov_contable.cuenta = cuentaSyA.
            mov_contable.nit = "001".

            FIND FIRST comprobante WHERE comprobante.comprobante = 15
                                     AND comprobantes.agencia = 1 NO-ERROR.
            IF AVAILABLE(comprobante) THEN DO:
                comprobantes.secuencia = comprobante.secuencia + 1.
                vSec = comprobante.secuencia.
            END.

            RELEASE comprobantes.

            CREATE mov_contable.
            RUN movContable (INPUT pOrigen).
            ASSIGN Mov_Contable.Agencia = 1
                   Mov_Contable.Destino = 1
                   Mov_Contable.Cuenta = cuentaSyA
                   Mov_Contable.Nit = STRING(creditos.agencia,"999")
                   mov_contable.cr = tt.valor - P_VlrNoDist.

            CREATE mov_contable.
            RUN movContable (INPUT pOrigen).
            ASSIGN Mov_Contable.Agencia = 1
                   Mov_Contable.Destino = creditos.agencia
                   Mov_Contable.Cuenta = "11100509"
                   Mov_Contable.Nit = creditos.nit
                   Mov_contable.db = tt.valor - P_VlrNoDist.
        END.
    END.
END.
ELSE
    P_VlrNoDist = tt.valor.

IF P_VlrNoDist > 0 THEN DO:
    MESSAGE "Advertencia: Existe un sobrante en la aplicación del abono a créditos:" SKIP
            "Identificación:" tt.nit SKIP
            "Número de crédito:" INTEGER(SUBSTRING(tt.referencia,8)) SKIP
            "Valor a abonar:" STRING(tt.valor,"$>>>,>>>,>>9.99") SKIP(2)
            "Se llevarán" STRING(P_VlrNoDist,"$>>>,>>>,>>9.99") "al ahorro a la vista."
        VIEW-AS ALERT-BOX INFO BUTTONS OK.

    FIND FIRST ahorros WHERE ahorros.tip_ahorro = 1
                         AND ahorros.cod_ahorro = 4
                         AND ahorros.nit = tt.nit
                         AND ahorros.estado = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE ahorros THEN DO:
        RUN ConsignacionAhorroAlaVista.r(INPUT ahorros.agencia,
                                         INPUT ahorros.cod_ahorro,
                                         INPUT ahorros.nit,
                                         INPUT ahorros.nit,
                                         INPUT ahorros.cue_ahorros,
                                         INPUT P_VlrNoDist,
                                         INPUT 0,
                                         INPUT 15,
                                         INPUT vSec,
                                         INPUT "Pago por " + pOrigen,
                                         INPUT clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2,
                                         OUTPUT pResult).

        flagAhorro = TRUE.
    END.
    ELSE DO:
        MESSAGE "Advertencia: No fue posible llevar el sobrante al ahorro a la Vista." SKIP
                "Se llevará a Devoluciones."  SKIP(2)
                "Valor del sobrante:" STRING(P_VlrNoDist,"$>>>,>>>,>>9.99")
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

        FIND FIRST ahorros WHERE ahorros.tip_ahorro = 1
                             AND ahorros.cod_ahorro = 8
                             AND ahorros.nit = tt.nit
                             AND ahorros.estado = 1 NO-LOCK NO-ERROR.
        IF AVAILABLE ahorros THEN DO:
            RUN ConsignacionAhorroAlaVista.r(INPUT ahorros.agencia,
                                             INPUT ahorros.cod_ahorro,
                                             INPUT ahorros.nit,
                                             INPUT ahorros.nit,
                                             INPUT ahorros.cue_ahorros,
                                             INPUT P_VlrNoDist,
                                             INPUT 0,
                                             INPUT 15,
                                             INPUT vSec,
                                             INPUT "Pago por " + pOrigen,
                                             INPUT clientes.nombre + " " + clientes.apellido1 + " " + clientes.apellido2,
                                             OUTPUT pResult).

            flagAhorro = TRUE.
        END.
        ELSE
            MESSAGE "Advertencia: No fue posible llevar el sobrante a Devoluciones." SKIP
                    "El reintegro (" STRING(P_VlrNoDist,"$>>>,>>>,>>9") ") del sobrante deberá relizarse de forma manual."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.

    IF flagAhorro = TRUE THEN DO:
        CREATE mov_contable.
        RUN movContable (INPUT pOrigen).
        ASSIGN Mov_Contable.Agencia = ahorros.agencia
               Mov_Contable.Destino = 1
               Mov_Contable.Cuenta = "11100509"
               Mov_Contable.Nit = tt.nit
               Mov_contable.db = P_VlrNoDist
               mov_contable.cr = 0.

        /* Para Sucursales y Agencias */
        IF ahorros.agencia <> 1 THEN DO:
            FIND FIRST pro_ahorros WHERE pro_ahorros.cod_ahorro = ahorros.cod_ahorro NO-LOCK NO-ERROR.
            IF AVAILABLE pro_ahorros THEN DO:
                FIND FIRST cortoLargo WHERE cortoLargo.agencia = ahorros.agencia
                                        AND cortoLargo.clase = 1
                                        AND cortoLargo.cod_producto = pro_ahorros.cod_ahorro NO-LOCK NO-ERROR.
                IF AVAILABLE cortoLargo THEN
                    cuentaSyA = cortoLargo.Cta_SYA.
            END.

            mov_contable.cuenta = cuentaSyA.
            mov_contable.nit = "001".

            FIND FIRST comprobante WHERE comprobante.comprobante = 15
                                     AND comprobantes.agencia = 1 NO-ERROR.
            IF AVAILABLE(comprobante) THEN DO:
                comprobantes.secuencia = comprobante.secuencia + 1.
                vSec = comprobante.secuencia.
            END.

            RELEASE comprobantes.

            CREATE mov_contable.
            RUN movContable (INPUT pOrigen).
            ASSIGN Mov_Contable.Agencia = 1
                   Mov_Contable.Destino = 1
                   Mov_Contable.Cuenta = cuentaSyA
                   Mov_Contable.Nit = STRING(ahorros.agencia,"999")
                   mov_contable.cr = P_VlrNoDist.

            CREATE mov_contable.
            RUN movContable (INPUT pOrigen).
            ASSIGN Mov_Contable.Agencia = 1
                   Mov_Contable.Destino = ahorros.agencia
                   Mov_Contable.Cuenta = "11100509"
                   Mov_Contable.Nit = ahorros.nit
                   Mov_contable.db = P_VlrNoDist.
        END.

        P_VlrNoDist = 0.
    END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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
  ENABLE RECT-345 RECT-346 RECT-347 RECT-348 brwExportados btnExportarPSE 
         btnExportarReferenciado brwImportados btnAplicarPSE 
         btnAplicarReferenciado Btn_Done 
      WITH FRAME Frm-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm-Main}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hallarDiaHabil C-Win 
PROCEDURE hallarDiaHabil :
DEFINE INPUT PARAMETER pTipoConvenio AS CHARACTER.

DEFINE VAR vDiaHabil AS LOGICAL.
    
DO WHILE vDiaHabil = FALSE:
    FIND FIRST recaudos_IO WHERE recaudos_IO.tipo_io = 'O'
                             AND recaudos_IO.tipo_convenio = pTipoConvenio
                             AND recaudos_IO.fecha = fechaArchivoDate NO-LOCK NO-ERROR.
    IF NOT AVAILABLE recaudos_IO THEN
        fechaArchivoDate = fechaArchivoDate - 1.
    ELSE
        vDiaHabil = TRUE.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE movContable C-Win 
PROCEDURE movContable :
DEFINE INPUT PARAMETER pOrgien AS CHARACTER.

mov_contable.cen_costos = 999.
Mov_Contable.Comprobante = 15.
Mov_Contable.Num_Documento = vSec.
Mov_contable.Doc_referencia = tt.referencia.
Mov_Contable.Fec_Contable = w_fecha.
Mov_Contable.Fec_Grabacion = TODAY.
Mov_Contable.Comentario = "Pago x " + pOrgien.
Mov_Contable.Usuario = w_usuario.
Mov_Contable.Estacion = "005".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir C-Win 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
 {INCLUIDO\RepEncabezado.I}.  
 DEFI VAR w-nomtipo  AS CHARACTER FORMAT "X(40)".
 FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.
 W_Reporte    = "REPORTE   : Exportación de Datos - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "  Nro.Registros       Saldo Total  Saldo Disponible   Tipo de Exportación".
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.
    ASSIGN W-NomTipo = "Todos los Clientes".
 PUT "    ".
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

