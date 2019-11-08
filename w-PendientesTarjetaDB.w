&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */
  {Incluido/Variable.I "SHARED"}
  {Incluido/VARCON.I "SHARED"}
DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
DEFINE VAR W_sw          AS LOGICAL. 
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR i AS INTEGER.


DEFINE VAR Listado AS CHARACTER INITIAL "".
DEFINE VAR WListado AS CHARACTER INITIAL "".


DEFINE TEMP-TABLE pendientes
    FIELD Agencia           LIKE TarjetaDebito.Agencia   
    FIELD nit               LIKE tarjetaDebito.Nit
    FIELD Cue_ahorros       LIKE tarjetaDebito.Cue_ahorros
    FIELD TarjetaDB         LIKE TarjetaDebito.TarjetaDB
    FIELD usuario           LIKE TarjetaDebito.usuario 
    FIELD Num_documento     LIKE TarjetaDebito.Num_Documento
    FIELD hora              LIKE TarjetaDebito.hora 
    FIELD Comprobante       LIKE TarjetaDebito.Comprobante    
    FIELD Fec_Contable      LIKE TarjetaDebito.Fec_Contable   
    FIELD Monto             LIKE TarjetaDebito.Monto          
    FIELD TipoTransaccion   LIKE TarjetaDebito.TipoTransaccion
    FIELD Comentario        LIKE TarjetaDebito.Comentario
    FIELD Aplicado          LIKE TarjetaDebito.Aplicado
    FIELD RetCode           LIKE TarjetaDebito.RetCode
    FIELD horareal          AS CHARACTER FORMAT "X(8)"
    INDEX idxptes fec_contable hora.

  DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
  DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
  DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
  DEFINE VARIABLE WFec     AS DATE.   
  DEFINE VARIABLE FecIni   AS DATE.
  fecIni = TODAY - 30.
  DEFINE VARIABLE FecFin   AS DATE INITIAL TODAY.
  DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
  DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".
  DEFINE VARIABLE InsIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE InsFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE EIni     AS INTEGER FORMAT "999".
  DEFINE VARIABLE EFin     AS INTEGER FORMAT "999".
  DEFINE VARIABLE WUsuIni  AS INTEGER FORMAT "9999" INITIAL 0.
  DEFINE VARIABLE WUsuFin  AS INTEGER FORMAT "9999" INITIAL 999.
  DEFINE VARIABLE W_Age  LIKE Agencias.Agencia.
  DEFINE VARIABLE W_NitW LIKE Clientes.Nit.
  DEFINE VARIABLE hour AS INTEGER.
  DEFINE VARIABLE minute AS INTEGER.
  DEFINE VARIABLE sec AS INTEGER.
  DEFINE VARIABLE timeleft AS INTEGER.
  DEFINE VARIABLE whora    AS CHARACTER FORMAT "X(8)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define BROWSE-NAME BROWSE-14

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Pendientes

/* Definitions for BROWSE BROWSE-14                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-14 Pendientes.Agencia Pendientes.nit Pendientes.TarjetaDB Pendientes.Comprobante Pendientes.Num_Documento Pendientes.Fec_Contable Pendientes.Monto Pendientes.TipoTransaccion Pendientes.RetCode Pendientes.Usuario Pendientes.HoraReal Pendientes.Comentario   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-14   
&Scoped-define SELF-NAME BROWSE-14
&Scoped-define QUERY-STRING-BROWSE-14 FOR EACH Pendientes
&Scoped-define OPEN-QUERY-BROWSE-14 OPEN QUERY BROWSE-14 FOR EACH Pendientes.
&Scoped-define TABLES-IN-QUERY-BROWSE-14 Pendientes
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-14 Pendientes


/* Definitions for FRAME DEFAULT-FRAME                                  */
&Scoped-define OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME ~
    ~{&OPEN-QUERY-BROWSE-14}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-229 RECT-230 Btn_Imp BUTTON-144 ~
Rad_TipoTran Rad_Tipo Btn_Procesar BUTTON-186 Btn_TarjSol Btn_marcar ~
T-Concurrente BROWSE-14 
&Scoped-Define DISPLAYED-OBJECTS Rad_TipoTran Rad_Tipo T-Concurrente w_hora 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_marcar 
     LABEL "Marcar Todo" 
     SIZE 20.57 BY .81.

DEFINE BUTTON Btn_Procesar 
     LABEL "Enviar Pendientes" 
     SIZE 20.57 BY .81.

DEFINE BUTTON Btn_TarjSol 
     LABEL "Aplicar ~"Tarj.Solicit~"" 
     SIZE 20.57 BY .81.

DEFINE BUTTON BUTTON-144 
     LABEL "Filtro de Información" 
     SIZE 20.57 BY .85.

DEFINE BUTTON BUTTON-186  NO-CONVERT-3D-COLORS
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE w_hora AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 10 BY .65 TOOLTIP "Hora"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rad_Tipo AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Pendientes", 1,
"Procesados", 2,
"Todos", 3
     SIZE 15 BY 2.04 NO-UNDO.

DEFINE VARIABLE Rad_TipoTran AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Retiros", 1,
"Consignación", 2,
"Todo", 3
     SIZE 16 BY 2
     FGCOLOR 0  NO-UNDO.

DEFINE RECTANGLE RECT-229
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.57 BY 2.73.

DEFINE RECTANGLE RECT-230
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16.86 BY 2.73.

DEFINE VARIABLE T-Concurrente AS LOGICAL INITIAL no 
     LABEL "Aplicar Concurrencia" 
     VIEW-AS TOGGLE-BOX
     SIZE 23 BY .65 NO-UNDO.

DEFINE VARIABLE Wf1 AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29.86 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE wf2 AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29.86 BY .81
     FONT 4 NO-UNDO.

DEFINE VARIABLE wf3 AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29.86 BY .81
     FONT 4 NO-UNDO.

DEFINE BUTTON BtnFecFin 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 4 BY .54.

DEFINE BUTTON BtnFecIni 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     IMAGE-INSENSITIVE FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BtnSalFil 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 145" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(40)":U INITIAL "000 - Todas las Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 54 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Usuario AS CHARACTER FORMAT "X(50)":U INITIAL "000 - Todos" 
     LABEL "Usuarios" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0000 - Todos" 
     DROP-DOWN-LIST
     SIZE 39 BY .92
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_DiaFin AS INTEGER FORMAT "99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE VARIABLE W_DiaIni AS DECIMAL FORMAT "99":U INITIAL 1 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81 NO-UNDO.

DEFINE IMAGE Img_MesF
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE IMAGE Img_MesI
     FILENAME "adeicon/blank":U
     SIZE 11 BY .81.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 26 BY 1.88.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 27 BY 1.88.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-14 FOR 
      Pendientes SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-14 C-Win _FREEFORM
  QUERY BROWSE-14 DISPLAY
      Pendientes.Agencia            COLUMN-LABEL "Age" 
    Pendientes.nit                COLUMN-LABEL "Nit"
    Pendientes.TarjetaDB          COLUMN-LABEL "#Tarjeta" FORMAT "X(20)"
    Pendientes.Comprobante        COLUMN-LABEL "Com"
    Pendientes.Num_Documento      COLUMN-LABEL "#Doc"
    Pendientes.Fec_Contable       COLUMN-LABEL "Fecha"
    Pendientes.Monto              COLUMN-LABEL "Monto"
    Pendientes.TipoTransaccion    COLUMN-LABEL "TipoTra"
    Pendientes.RetCode            COLUMN-LABEL "CodRet"
    Pendientes.Usuario            COLUMN-LABEL "Usr"
    Pendientes.HoraReal           COLUMN-LABEL "Hora"
    Pendientes.Comentario         COLUMN-LABEL "Comentario"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 98 BY 13.04
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .62 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     Btn_Imp AT ROW 1.27 COL 88.72
     BUTTON-144 AT ROW 1.54 COL 36
     Rad_TipoTran AT ROW 2.04 COL 19 NO-LABEL
     Rad_Tipo AT ROW 2.08 COL 2 NO-LABEL
     Btn_Procesar AT ROW 2.5 COL 36
     BUTTON-186 AT ROW 2.88 COL 88.72
     Btn_TarjSol AT ROW 3.42 COL 36
     Btn_marcar AT ROW 4.35 COL 36
     T-Concurrente AT ROW 4.5 COL 2
     w_hora AT ROW 4.5 COL 23 COLON-ALIGNED NO-LABEL WIDGET-ID 4
     BROWSE-14 AT ROW 6.08 COL 1.29
     "4:Tarjeta Bloqueada  5:Fondos Insuficientes  9:Declinada" VIEW-AS TEXT
          SIZE 47 BY .77 AT ROW 19.08 COL 52.29
          BGCOLOR 18 FGCOLOR 15 FONT 4
     "Tipo Transac" VIEW-AS TEXT
          SIZE 12.14 BY .54 AT ROW 1.35 COL 20.86
          FGCOLOR 7 
     "Transacción" VIEW-AS TEXT
          SIZE 12.57 BY .62 AT ROW 1.27 COL 2.86
          FGCOLOR 7 
     "CodRet:  0:Exito  1:ErrorComunicac  2:Tarj.Inexistente  3:Cuenta Inexistent" VIEW-AS TEXT
          SIZE 51 BY .81 AT ROW 19.08 COL 1.29
          BGCOLOR 18 FGCOLOR 15 FONT 4
     "TipoTra:  0:Consulta 1:Ret.Efect 2:Ret.Cheque 3:Cons.Efec 4:Cons.Cheq" VIEW-AS TEXT
          SIZE 50 BY .81 AT ROW 5.35 COL 1.29
          BGCOLOR 18 FGCOLOR 15 FONT 4
     "5:Rev.Ret 6:Rev.Cons 7:Rev.Tras 8:TrasBco 9:AjusteDB 10:AjusteCR" VIEW-AS TEXT
          SIZE 48 BY .81 AT ROW 5.35 COL 51.29
          BGCOLOR 18 FGCOLOR 15 FONT 4
     RECT-229 AT ROW 1.54 COL 1.57
     RECT-230 AT ROW 1.54 COL 18.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 98.29 BY 18.88
         BGCOLOR 17 .

DEFINE FRAME frm_desFil
     Wf1 AT ROW 1.54 COL 1.14 NO-LABEL
     wf2 AT ROW 2.31 COL 1.14 NO-LABEL
     wf3 AT ROW 3.04 COL 1.14 NO-LABEL
     "Filtros Activos:" VIEW-AS TEXT
          SIZE 17 BY .54 AT ROW 1 COL 1
          BGCOLOR 7 FGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 57.14 ROW 1.31
         SIZE 31 BY 2.96
         BGCOLOR 7 FGCOLOR 15 .

DEFINE FRAME Frm_Filtro
     Cmb_Agencias AT ROW 1.27 COL 3 NO-LABEL
     W_DiaIni AT ROW 3.35 COL 2.43 COLON-ALIGNED NO-LABEL
     AnoIni AT ROW 3.35 COL 17.43 COLON-ALIGNED NO-LABEL
     BtnFecIni AT ROW 3.35 COL 25.43
     W_DiaFin AT ROW 3.35 COL 30.43 NO-LABEL
     AnoFin AT ROW 3.35 COL 43.43 COLON-ALIGNED NO-LABEL
     BtnFecFin AT ROW 3.38 COL 51.29
     Cmb_Usuario AT ROW 4.5 COL 15 COLON-ALIGNED
     BtnSalFil AT ROW 6.08 COL 47
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 2.58 COL 34.43
          BGCOLOR 17 FGCOLOR 7 
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 11 BY .69 AT ROW 2.54 COL 8.43
          BGCOLOR 17 FGCOLOR 7 
     Img_MesF AT ROW 3.35 COL 34.43
     Img_MesI AT ROW 3.35 COL 8.43
     RECT-282 AT ROW 2.42 COL 3.43
     RECT-283 AT ROW 2.42 COL 29.43
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 20.29 ROW 6.65
         SIZE 57 BY 8.08
         BGCOLOR 17 
         TITLE "Filtros".


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
         TITLE              = "Transacciones Pendientes para la Librería"
         HEIGHT             = 18.88
         WIDTH              = 98.29
         MAX-HEIGHT         = 21.27
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.27
         VIRTUAL-WIDTH      = 114.29
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
/* REPARENT FRAME */
ASSIGN FRAME frm_desFil:FRAME = FRAME DEFAULT-FRAME:HANDLE
       FRAME Frm_Filtro:FRAME = FRAME DEFAULT-FRAME:HANDLE.

/* SETTINGS FOR FRAME DEFAULT-FRAME
   FRAME-NAME                                                           */
/* BROWSE-TAB BROWSE-14 w_hora DEFAULT-FRAME */
/* SETTINGS FOR FILL-IN w_hora IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME frm_desFil
                                                                        */
/* SETTINGS FOR FILL-IN Wf1 IN FRAME frm_desFil
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN wf2 IN FRAME frm_desFil
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN wf3 IN FRAME frm_desFil
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FRAME Frm_Filtro
   NOT-VISIBLE UNDERLINE                                                */
/* SETTINGS FOR FILL-IN AnoFin IN FRAME Frm_Filtro
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME Frm_Filtro
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_Agencias IN FRAME Frm_Filtro
   ALIGN-L                                                              */
/* SETTINGS FOR FILL-IN W_DiaFin IN FRAME Frm_Filtro
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_DiaIni IN FRAME Frm_Filtro
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-14
/* Query rebuild information for BROWSE BROWSE-14
     _START_FREEFORM
OPEN QUERY BROWSE-14 FOR EACH Pendientes.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BROWSE-14 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME frm_desFil
/* Query rebuild information for FRAME frm_desFil
     _Query            is NOT OPENED
*/  /* FRAME frm_desFil */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frm_Filtro
/* Query rebuild information for FRAME Frm_Filtro
     _Query            is NOT OPENED
*/  /* FRAME Frm_Filtro */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME DEFAULT-FRAME:HANDLE
       ROW             = 1
       COLUMN          = 2
       HEIGHT          = 1.35
       WIDTH           = 4
       HIDDEN          = yes
       SENSITIVE       = yes.
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Transacciones Pendientes para la Librería */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Transacciones Pendientes para la Librería */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-14
&Scoped-define SELF-NAME BROWSE-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-14 C-Win
ON ROW-DISPLAY OF BROWSE-14 IN FRAME DEFAULT-FRAME
DO:
   IF NOT Pendientes.aplicado THEN
      ASSIGN Pendientes.Agencia:BGC IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.Comprobante:BGC IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.Num_Documento:BGC IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.Monto:BGC IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.usuario:BGC IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.Comentario:BGC IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.Nit:BGC   IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.Fec_contable:BGC IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.TarjetaDB:BGC IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.HoraReal:BGC IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.RetCode:BGC IN BROWSE {&BROWSE-NAME} = 4
             Pendientes.TipoTransaccion:BGC IN BROWSE {&BROWSE-NAME} = 4. /* color de letras :fGC */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm_Filtro
&Scoped-define SELF-NAME BtnFecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnFecFin C-Win
ON CHOOSE OF BtnFecFin IN FRAME Frm_Filtro /* Button 121 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         W_DiaFin:SCREEN-VALUE = STRING(DAY(WFec))
         AnoFin:SCREEN-VALUE = STRING(WAno)
         AnoFin = WAno
         MesFin = WMes
         FecFin = WFec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnFecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnFecIni C-Win
ON CHOOSE OF BtnFecIni IN FRAME Frm_Filtro /* Button 120 */
DO:
  RUN C-Fecha.r (OUTPUT WImagen, OUTPUT WAno, OUTPUT WMes, OUTPUT WFec).
  ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(WMes) + ".gif")
         AnoIni:SCREEN-VALUE = STRING(WAno)
         W_DiaIni:SCREEN-VALUE = STRING(DAY(WFec))
         AnoIni = WAno
         MesIni = WMes
         FecIni = WFec.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnSalFil
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnSalFil C-Win
ON CHOOSE OF BtnSalFil IN FRAME Frm_Filtro /* Button 145 */
DO:
  HIDE FRAME Frm_Filtro.
  RUN llenarTabla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp C-Win
ON CHOOSE OF Btn_Imp IN FRAME DEFAULT-FRAME /* Button 149 */
DO:
  RUN imprimir_Tdb.
  IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    ASSIGN WListado = Listado.
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado,INPUT-OUTPUT W_Dispositivo).
  OS-RENAME VALUE(WListado) VALUE(Listado).
  
    IF W_Dispositivo = "P" THEN  
      RUN Pantalla IN W_Manija (INPUT Listado).
    ELSE                                                  
      IF W_Dispositivo = "I" THEN
         RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  ?,INPUT  1,INPUT  1,
                                          INPUT  99999,OUTPUT W_sw).
    IF W_Dispositivo <> "A" THEN
        OS-DELETE VALUE(Listado).
    IF W_Dispositivo = "E" THEN
      RUN Imprimir_Excel.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_marcar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_marcar C-Win
ON CHOOSE OF Btn_marcar IN FRAME DEFAULT-FRAME /* Marcar Todo */
DO:
 FOR EACH Tarjetadebito  WHERE tarjetadebito.agencia          GE ageini  AND tarjetadebito.agencia          LE agefin  AND
                               tarjetadebito.fec_contable     GE fecini  AND tarjetadebito.fec_contable     LE fecfin  AND
                               integer(tarjetadebito.usuario) GE wusuini AND integer(tarjetadebito.usuario) LE wusufin AND
                               NOT TarjetaDebito.Aplicado AND tarjetadebito.retcode NE 0:
    ASSIGN tarjetadebito.aplicado = YES.
 END.
 RUN LlenarTabla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Procesar C-Win
ON CHOOSE OF Btn_Procesar IN FRAME DEFAULT-FRAME /* Enviar Pendientes */
DO:
 FOR EACH pendientes WHERE NOT Pendientes.Aplicado AND Pendientes.retcode NE 0 AND
                               Pendientes.agencia          GE ageini  AND Pendientes.agencia          LE agefin AND
                               Pendientes.fec_contable     GE fecini  AND Pendientes.fec_contable     LE fecfin AND
                               integer(Pendientes.usuario) GE wusuini AND integer(Pendientes.usuario) LE wusufin:
   IF  Rad_TipoTran:SCREEN-VALUE = '1' THEN
       IF Pendientes.tipoTransaccion GT 2 THEN NEXT.

   IF  Rad_TipoTran:SCREEN-VALUE = '2' THEN
       IF Pendientes.tipoTransaccion NE 3 AND Pendientes.tipoTransaccion NE 4 THEN NEXT.
   
   RUN grabar_TarDeb(Pendientes.monto, Pendientes.comentario, Pendientes.TipoTransaccion).
 END.
 RUN LlenarTabla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_TarjSol
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_TarjSol C-Win
ON CHOOSE OF Btn_TarjSol IN FRAME DEFAULT-FRAME /* Aplicar "Tarj.Solicit" */
DO:
 FOR EACH Tarjetadebito  WHERE tarjetadebito.agencia          GE ageini  AND tarjetadebito.agencia          LE agefin  AND
                               tarjetadebito.fec_contable     GE fecini  AND tarjetadebito.fec_contable     LE fecfin  AND
                               integer(tarjetadebito.usuario) GE wusuini AND integer(tarjetadebito.usuario) LE wusufin AND
                               tarjetadebito.tarjetadb = "TARJ.DB.SOLICIT." :
   ASSIGN tarjetadebito.aplicado = YES.
 END.
 RUN LlenarTabla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-144
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-144 C-Win
ON CHOOSE OF BUTTON-144 IN FRAME DEFAULT-FRAME /* Filtro de Información */
DO:
  VIEW FRAME Frm_Filtro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-186
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-186 C-Win
ON CHOOSE OF BUTTON-186 IN FRAME DEFAULT-FRAME /* Salir */
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


&Scoped-define FRAME-NAME Frm_Filtro
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias C-Win
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME Frm_Filtro
DO:
  ASSIGN FRAME Frm_Filtro Cmb_Agencias.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Usuario C-Win
ON VALUE-CHANGED OF Cmb_Usuario IN FRAME Frm_Filtro /* Usuarios */
DO:
  ASSIGN FRAME Frm_Filtro Cmb_Usuario.
  IF SUBSTRING(Cmb_Usuario,1,4) EQ "0000" THEN
     ASSIGN WUsuIni = 0 WUsuFin = 9999.
  ELSE
     ASSIGN WUsuIni = INTEGER(SUBSTRING(Cmb_Usuario,1,4)) WUsuFin = INTEGER(SUBSTRING(Cmb_Usuario,1,4)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME DEFAULT-FRAME
&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame C-Win OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
IF T-Concurrente:SCREEN-VALUE IN FRAME DEFAULT-FRAME EQ "YES" THEN DO:
  IF FRAME DEFAULT-FRAME:VISIBLE THEN DO:
     APPLY "choose" TO btn_procesar IN FRAME DEFAULT-frame.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rad_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rad_Tipo C-Win
ON VALUE-CHANGED OF Rad_Tipo IN FRAME DEFAULT-FRAME
DO:
  IF Rad_Tipo:SCREEN-VALUE = '1' THEN
     ASSIGN Btn_Procesar:VISIBLE = TRUE    Btn_TarjSol:VISIBLE = TRUE.
  ELSE
     ASSIGN Btn_Procesar:VISIBLE = FALSE   Btn_TarjSol:VISIBLE = FALSE.
  RUN llenarTabla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Rad_TipoTran
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rad_TipoTran C-Win
ON VALUE-CHANGED OF Rad_TipoTran IN FRAME DEFAULT-FRAME
DO:
  RUN llenarTabla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME T-Concurrente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL T-Concurrente C-Win
ON VALUE-CHANGED OF T-Concurrente IN FRAME DEFAULT-FRAME /* Aplicar Concurrencia */
DO:
  ASSIGN t-concurrente.
  IF SELF:SCREEN-VALUE = "YES" THEN DO:
     w_hora:SCREEN-VALUE = STRING(TIME,"HH:MM:SS").
     APPLY "choose" TO btn_procesar IN FRAME DEFAULT-frame. 
     btn_Procesar:SENSITIVE = NO.
  END.
  ELSE
     btn_Procesar:SENSITIVE = yes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it. 
                                                         */
ON CLOSE OF THIS-PROCEDURE 
  
   RUN disable_UI.
   ASSIGN Rad_Tipo:SCREEN-VALUE = '1'  Rad_TipoTran:SCREEN-VALUE = '3'.
   HIDE FRAME Frm_Filtro.
   wf1:SCREEN-VALUE IN FRAME Frm_DesFil = 'Agencia Ini: ' + TRIM(STRING(ageini))  + '       Agencia Fin:' + TRIM(STRING(agefin)).
   wf2:SCREEN-VALUE IN FRAME Frm_DesFil = 'Usuario Ini: ' + TRIM(STRING(wusuini)) + '      Usuario Fin:' + TRIM(STRING(wusufin)). 
   wf3:SCREEN-VALUE IN FRAME Frm_DesFil = 'Fecha   Ini: ' + TRIM(STRING(fecIni))  + ' Fecha Fin:' + TRIM(STRING(fecfin)). 
   RUN llenarTabla.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load C-Win  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "w-PendientesTarjetaDB.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "w-PendientesTarjetaDB.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crear_Registro C-Win 
PROCEDURE Crear_Registro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  CREATE pendientes.
  ASSIGN  pendientes.Agencia             =  TarjetaDebito.Agencia         
          pendientes.Comentario          =  TarjetaDebito.Comentario      
          pendientes.Comprobante         =  TarjetaDebito.Comprobante     
          pendientes.Fec_Contable        =  TarjetaDebito.Fec_Contable    
          pendientes.Hora                =  TarjetaDebito.Hora            
          pendientes.Monto               =  TarjetaDebito.Monto           
          pendientes.Nit                 =  TarjetaDebito.Nit  
          pendientes.cue_ahorros         =  TarjetaDebito.cue_ahorros
          pendientes.TarjetaDB           =  TarjetaDebito.TarjetaDB
          pendientes.Num_Documento       =  TarjetaDebito.Num_Documento   
          pendientes.TipoTransaccion     =  TarjetaDebito.TipoTransaccion 
          pendientes.Usuario             =  TarjetaDebito.Usuario
          pendientes.RetCode             =  TarjetaDebito.RetCode
          pendientes.Aplicado            =  TarjetaDebito.Aplicado.
  timeleft = pendientes.hora.                                                                     
  sec    = timeleft MOD 60.                                                             
  timeleft = (timeleft - sec) / 60.                                                     
  minute = timeleft MOD 60.                                                             
  hour   = (timeleft - minute) / 60.                                                    
  whora  = STRING(hour,"99") + ":" + STRING(minute,"99") + ":" + STRING(sec,"99").      
  ASSIGN pendientes.Horareal = whora.

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
  RUN control_load.
  DISPLAY Rad_TipoTran Rad_Tipo T-Concurrente w_hora 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE RECT-229 RECT-230 Btn_Imp BUTTON-144 Rad_TipoTran Rad_Tipo 
         Btn_Procesar BUTTON-186 Btn_TarjSol Btn_marcar T-Concurrente BROWSE-14 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  DISPLAY Wf1 wf2 wf3 
      WITH FRAME frm_desFil IN WINDOW C-Win.
  VIEW FRAME frm_desFil IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-frm_desFil}
  DISPLAY Cmb_Agencias W_DiaIni AnoIni W_DiaFin AnoFin Cmb_Usuario 
      WITH FRAME Frm_Filtro IN WINDOW C-Win.
  ENABLE Img_MesF Img_MesI RECT-282 RECT-283 Cmb_Agencias BtnFecIni BtnFecFin 
         Cmb_Usuario BtnSalFil 
      WITH FRAME Frm_Filtro IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm_Filtro}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TarDeb C-Win 
PROCEDURE Grabar_TarDeb :
/*------------------------------------------------------------------------------
  Purpose:   Graba movimientos de Tarjeta debito  
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER wvlrTrans  LIKE ahorros.sdo_disponible.
DEFINE INPUT PARAMETER wdesTrans  LIKE Mov_contable.comentario.
DEFINE INPUT PARAMETER wtipoTrans AS INTEGER.
DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wretcode      AS INTEGER INITIAL -1.
   wvlrmonTD = STRING(wvlrtrans,'9999999999999').
   FIND FIRST tarjetaDebito WHERE tarjetaDebito.nit EQ Pendientes.nit AND 
                                  TarjetaDebito.cue_ahorros EQ Pendientes.cue_ahorros AND
                                  TarjetaDebito.TarjetaDB   EQ Pendientes.TarjetaDB   AND
                                  NOT TarjetaDebito.Aplicado AND tarjetadebito.retcode NE 0 NO-ERROR.
   IF AVAILABLE(tarjetaDebito) THEN DO:
     RUN TranWebCaja(1,wtipotrans,TRIM(Tarjetadebito.nit), TRIM(TarjetaDebito.cue_ahorros), TarjetaDebito.TarjetaDB, wvlrmonTD, INPUT-OUTPUT wsecTD, INPUT-OUTPUT wsdoTD, INPUT-OUTPUT wdisTD, OUTPUT wretcode).
     IF wretcode = 0 THEN 
        ASSIGN tarjetadebito.Secuencia       = wsecTD 
               tarjetadebito.SdoTotal        = ROUND(decimal(wsdoTD) / 100,2)
               tarjetadebito.SdoDispon       = ROUND(decimal(wdisTD) / 100,2)
               tarjetadebito.RetCode         = wretcode
               tarjetadebito.Aplicado        = YES
               tarjetadebito.fec_replica     = TODAY
               tarjetadebito.hora_replica    = TIME.
     ELSE 
        ASSIGN tarjetadebito.RetCode         = wretcode.
   END.
   FIND CURRENT tarjetadebito NO-LOCK NO-ERROR.
   RELEASE tarjetadebito.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_TDB C-Win 
PROCEDURE Imprimir_TDB :
Listado = W_PathSpl + "\TarjDB_" + TRIM(W_Usuario) + ".txt".  
OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)" . /* INITIAL "AGE NIT            #Tarjeta   Comp  #Docum  Fecha  Monto  TipTra CodRet Usr Hora Comentario".*/
DEFINE VAR TotReg AS DECIMAL.
DEFINE VAR Motivo AS CHARACTER FORMAT "X(50)".
DEFINE VAR TotMot AS DECIMAL.
DEFINE BUFFER Relac-Cliente FOR Clientes.
DEFINE VAR wtipoP   AS CHARACTER FORMAT "X(10)".
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Libreria Tarjeta Debito: " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGE NIT            #Tarjeta      Comp  #Docum  Fecha         Monto  TipTra CodRet Usr Hora Comentario".
  VIEW FRAME F-Encabezado.
  ASSIGN i = 0 .
  FOR EACH pendientes  /*WHERE 
           Clientes.Agencia GE AgeIni AND
           Clientes.Agencia LE AgeFin */ NO-LOCK :  
      PUT     Pendientes.Agencia          FORMAT "z9"         " "
              Pendientes.nit              FORMAT "X(12)"      " "
              Pendientes.TarjetaDB        FORMAT "X(16)"      " "
              Pendientes.Comprobante      FORMAT "z9"         " "
              Pendientes.Num_Documento    FORMAT "zzzzzzzz9"  " "
              Pendientes.Fec_Contable     FORMAT "99/99/9999" " "
              Pendientes.Monto            FORMAT "zzz,zzz,zz9" " "
              Pendientes.TipoTransaccio   FORMAT "-zz9"        " "
              Pendientes.RetCode          FORMAT "-zz9"        "    "
              Pendientes.Usuario          FORMAT "X(3)"        " "
              Pendientes.Hora             FORMAT "zzzzz9"      " "
              Pendientes.Comentario       FORMAT "X(40)" SKIP(0).   
      i = i + 1.
  END.
DISPLAY SKIP "Total de Registros Reportados: " TotReg WITH FRAME FT WIDTH 132 NO-LABELS.
VIEW FRAME F-Ftr.
PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InitializeObject C-Win 
PROCEDURE InitializeObject :
DO WITH FRAME Frm_Filtro:
   FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
      W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
     /* IF Agencias.Agencia EQ W_Agencia THEN
         Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre. */
   END.
   FOR EACH usuarios WHERE usuarios.estado EQ 1 NO-LOCK:
       Cmb_Usuario:ADD-LAST(STRING(Usuarios.Usuario,"9999") + " - " + TRIM(Usuarios.Nombre)).
   END.
   ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
          W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
          AnoIni:SCREEN-VALUE = STRING(YEAR(TODAY))
          AnoFin:SCREEN-VALUE = STRING(YEAR(TODAY))
          MesIni = MONTH(TODAY)
          MesFin = MONTH(TODAY)
          FecIni = TODAY - (DAY(TODAY) + 1)
          FecFin = TODAY.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE LlenarTabla C-Win 
PROCEDURE LlenarTabla :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH Pendientes:
   DELETE pendientes.
END.
DEFINE VAR  wnroreg  AS INTEGER INITIAL 0.
FOR EACH Tarjetadebito  WHERE tarjetadebito.agencia          GE ageini  AND tarjetadebito.agencia          LE agefin AND
                              tarjetadebito.fec_contable     GE fecini  AND tarjetadebito.fec_contable     LE fecfin AND
                              integer(tarjetadebito.usuario) GE wusuini AND integer(tarjetadebito.usuario) LE wusufin
    NO-LOCK:
  CASE Rad_Tipo:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
      WHEN '1' THEN DO:    /* Pendientes */
         CASE Rad_TipoTran:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
             WHEN '1' THEN 
                 IF NOT tarjetaDebito.aplicado AND TarjetaDebito.retcode NE 0 AND  /* Retiro */
                    Tarjetadebito.TipoTransaccion > 0 AND Tarjetadebito.TipoTransaccion < 3 THEN DO:
                    RUN crear_registro.
                    ASSIGN wnroreg = wnroreg + 1.
                 END.
             WHEN '2' THEN 
                 IF NOT tarjetaDebito.aplicado AND TarjetaDebito.retcode NE 0 AND  /* Consignacion */
                    Tarjetadebito.TipoTransaccion > 2 AND Tarjetadebito.TipoTransaccion < 5 THEN DO:
                    RUN crear_registro.
                    ASSIGN wnroreg = wnroreg + 1.
                 END.
             
             WHEN '3' THEN 
               IF NOT tarjetaDebito.aplicado AND TarjetaDebito.retcode NE 0 THEN DO:
                 RUN crear_registro.
                 ASSIGN wnroreg = wnroreg + 1.
               END.
         END CASE.
      END.
      WHEN '2' THEN DO:  /* Procesados */
          CASE Rad_TipoTran:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
              WHEN '1' THEN 
                  IF tarjetaDebito.aplicado AND Tarjetadebito.secuencia NE " " AND 
                     Tarjetadebito.TipoTransaccion > 0 AND Tarjetadebito.TipoTransaccion < 3 THEN DO:
                     RUN crear_registro.
                     ASSIGN wnroreg = wnroreg + 1.
                  END.
              WHEN '2' THEN 
                  IF tarjetaDebito.aplicado AND Tarjetadebito.secuencia NE " " AND 
                     Tarjetadebito.TipoTransaccion > 2 AND Tarjetadebito.TipoTransaccion < 5 THEN DO:
                     RUN crear_registro.
                     ASSIGN wnroreg = wnroreg + 1.
                  END.
              WHEN '3' THEN 
                  IF tarjetaDebito.aplicado AND Tarjetadebito.secuencia NE " " THEN DO:
                     RUN crear_registro.
                     ASSIGN wnroreg = wnroreg + 1.
                  END.
          END CASE.
      END.

      WHEN '3' THEN DO:
        CASE Rad_TipoTran:SCREEN-VALUE IN FRAME {&FRAME-NAME}:
            WHEN "1" THEN
              IF Tarjetadebito.TipoTransaccion > 0 AND Tarjetadebito.TipoTransaccion < 3 THEN DO:
                RUN crear_registro.
                ASSIGN wnroreg = wnroreg + 1.
              END.
            WHEN "2" THEN
              IF Rad_TipoTran:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '2' AND 
                 Tarjetadebito.TipoTransaccion > 2 AND Tarjetadebito.TipoTransaccion < 5 THEN DO:
                 RUN crear_registro.
                 ASSIGN wnroreg = wnroreg + 1.
              END.
            WHEN "3" THEN DO:
                RUN crear_registro.
                ASSIGN wnroreg = wnroreg + 1.
            END.
        END CASE.
      END.
END CASE.
END.
wf1:SCREEN-VALUE IN FRAME Frm_DesFil = 'Agencia Ini: ' + TRIM(STRING(ageini))  + '       Agencia Fin:' + TRIM(STRING(agefin)).
wf2:SCREEN-VALUE IN FRAME Frm_DesFil = 'Usuario Ini: ' + TRIM(STRING(wusuini)) + '      Usuario Fin:' + TRIM(STRING(wusufin)). 
wf3:SCREEN-VALUE IN FRAME Frm_DesFil = 'Fecha   Ini: ' + TRIM(STRING(fecIni))  + ' Fecha Fin:' + TRIM(STRING(fecfin)). 
Listado = W_Pathspl + "\" + W_Usuario + "TarDeb.lst".
OPEN QUERY BROWSE-14 FOR EACH Pendientes.
APPLY "Entry" TO BUTTON-186 IN FRAME {&FRAME-NAME}.
IF Wnroreg = 0 THEN DO:
  ASSIGN Browse-14:HIDDEN IN FRAME {&FRAME-NAME} = TRUE 
         Btn_Procesar:VISIBLE = FALSE   Btn_TarjSol:VISIBLE = FALSE.
  APPLY "Entry" TO BUTTON-186 IN FRAME {&FRAME-NAME}.
  RETURN NO-APPLY.
END.
ELSE
    ASSIGN Browse-14:HIDDEN = FALSE .
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

