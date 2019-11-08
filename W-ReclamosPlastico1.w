&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
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

{Incluido\variable.i "shared"}
/* Local Variable Definitions ---                                       */
  DEFINE VAR P_Nit       LIKE Clientes.Nit        NO-UNDO.
  DEFINE VAR p_Nombre    LIKE Clientes.Nombre     NO-UNDO.
  DEFINE VAR P_Apellido  LIKE Clientes.Apellido1  NO-UNDO.
  DEFINE VAR P_AgeCli    LIKE Clientes.Agencia    NO-UNDO.
  DEFINE VAR w_ok        AS LOGICAL               NO-UNDO.
  DEFINE VAR w-tarjetaDB LIKE Ahorros.TarjetaDb   NO-UNDO.
  DEFINE VAR W-Seleccion AS INTEGER INITIAL 1     NO-UNDO.
  DEFINE VAR Choice      AS LOGICAL INITIAL FALSE NO-UNDO.
DEFINE TEMP-TABLE tmp-tarjetadb LIKE tarjetadebito.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb-TarjetaDb Cmb-MovTar Btn_Imprimir ~
Btn_Ingresar Btn_Salir RECT-338 Rec-Fondo RECT-340 RECT-341 RECT-342 
&Scoped-Define DISPLAYED-OBJECTS w-FecRec w-HoraRec w-Consecutivo w-cedula ~
W_NomTitular Cmb-TarjetaDb Cmb-MovTar w-monto Cmb-Tipo Cmb-Estado ~
w-Descripcion w-TipoCta w-Transaccion w-Num_Cuenta w-Lugar w-Monto2 ~
w-Fec_Transac w-Hora_Transac w-Comision w-NomUsuario w-NroTerminal w-Naud 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Imprimir Btn_Consulta Btn_Salvar Btn_Ingresar ~
Btn_Cancelar Btn_Modificar 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Salir2 DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 7 BY 1.5 TOOLTIP "Sale de la Consulta"
     BGCOLOR 8 .

DEFINE VARIABLE Cmb-Consulta AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 75 BY 3.12 NO-UNDO.

DEFINE BUTTON Btn-Visualizar 
     LABEL "Visualizar" 
     SIZE 8 BY .69 TOOLTIP "Visualizar por Pantalla / Impresora".

DEFINE BUTTON Btn_Borrar 
     LABEL "&Borrar" 
     SIZE 8 BY 1.65
     FONT 4.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 8 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_Ingresar 
     LABEL "&Ingresar" 
     SIZE 8 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Modificar 
     LABEL "&Modificar" 
     SIZE 8 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Salir DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "&Salir" 
     SIZE 8 BY 1.62 TOOLTIP "Sale de este proceso"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Salvar 
     LABEL "&Salvar" 
     SIZE 8 BY 1.62
     FONT 4.

DEFINE VARIABLE w-Descripcion AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL
     SIZE 71.86 BY 2.27 NO-UNDO.

DEFINE VARIABLE w-cedula LIKE Tarjetas.Nit
     VIEW-AS FILL-IN 
     SIZE 22 BY .73 NO-UNDO.

DEFINE VARIABLE w-Comision AS DECIMAL FORMAT "->>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE w-Consecutivo AS INTEGER FORMAT "99999999" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 1 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE w-FecRec AS DATE FORMAT "99/99/9999":U 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE w-Fec_Transac AS DATE FORMAT "99/99/9999" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81 NO-UNDO.

DEFINE VARIABLE w-HoraRec AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 8  NO-UNDO.

DEFINE VARIABLE w-Hora_Transac AS INTEGER FORMAT ">>>>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 9.29 BY .81 NO-UNDO.

DEFINE VARIABLE w-Lugar AS CHARACTER FORMAT "x(50)" 
     VIEW-AS FILL-IN 
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE w-monto AS DECIMAL FORMAT "->>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE w-Monto2 AS DECIMAL FORMAT "->>,>>>,>>9" INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE w-Naud AS CHARACTER FORMAT "x(4)" 
     VIEW-AS FILL-IN 
     SIZE 6.86 BY .81 NO-UNDO.

DEFINE VARIABLE w-NomUsuario AS CHARACTER FORMAT "X(65)":U 
     VIEW-AS FILL-IN 
     SIZE 60.57 BY .81 TOOLTIP "Usuario quién reporta el reclamo" NO-UNDO.

DEFINE VARIABLE w-NroTerminal AS CHARACTER FORMAT "x(8)" 
     VIEW-AS FILL-IN 
     SIZE 9 BY .81 NO-UNDO.

DEFINE VARIABLE w-Num_Cuenta AS CHARACTER FORMAT "x(20)" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE w-TipoCta AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 NO-UNDO.

DEFINE VARIABLE w-Transaccion AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY .81 TOOLTIP "Nombre del Titular de la Tarjeta Débito"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE Rec-Fondo
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 75 BY 5.12
     BGCOLOR 8 FGCOLOR 15 .

DEFINE RECTANGLE RECT-338
     EDGE-PIXELS 2 GRAPHIC-EDGE   GROUP-BOX  
     SIZE 75 BY 8.35
     FGCOLOR 8 .

DEFINE RECTANGLE RECT-340
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 74.29 BY 2.81.

DEFINE RECTANGLE RECT-341
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9.29 BY 3.62.

DEFINE RECTANGLE RECT-342
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 40 BY 1.88
     BGCOLOR 8 .

DEFINE VARIABLE Cmb-Estado AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "01 PENDIENTE","02 REVERSADO","03 NO EXITOSO" 
     SIZE 21 BY .81 NO-UNDO.

DEFINE VARIABLE Cmb-MovTar AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 72 BY 2.27 NO-UNDO.

DEFINE VARIABLE Cmb-TarjetaDb AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 22 BY 1.88 TOOLTIP "Tarjetas Débitos Existentes" NO-UNDO.

DEFINE VARIABLE Cmb-Tipo AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     LIST-ITEMS "01 DEBITO NO PAGO","02 DEBITO PARCIAL" 
     SIZE 25 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     w-FecRec AT ROW 2.08 COL 51.86 COLON-ALIGNED NO-LABEL WIDGET-ID 180
     w-HoraRec AT ROW 2.12 COL 71.72 COLON-ALIGNED NO-LABEL WIDGET-ID 182
     w-Consecutivo AT ROW 1.19 COL 83.86 RIGHT-ALIGNED NO-LABEL WIDGET-ID 148
     w-cedula AT ROW 2.08 COL 1 COLON-ALIGNED HELP
          "Nit del tercero" NO-LABEL WIDGET-ID 90
     W_NomTitular AT ROW 3.04 COL 23.14 NO-LABEL WIDGET-ID 14
     Cmb-TarjetaDb AT ROW 3.96 COL 23 NO-LABEL WIDGET-ID 36
     Cmb-MovTar AT ROW 7.04 COL 3 NO-LABEL WIDGET-ID 88
     w-monto AT ROW 10.12 COL 2 COLON-ALIGNED NO-LABEL WIDGET-ID 94
     Cmb-Tipo AT ROW 10.12 COL 23 NO-LABEL WIDGET-ID 96
     Cmb-Estado AT ROW 10.12 COL 53 NO-LABEL WIDGET-ID 98
     w-Descripcion AT ROW 12.27 COL 3.43 NO-LABEL WIDGET-ID 154
     w-TipoCta AT ROW 15.46 COL 13.14 COLON-ALIGNED NO-LABEL WIDGET-ID 122
     w-Transaccion AT ROW 15.5 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 134
     w-Num_Cuenta AT ROW 16.38 COL 13.14 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     w-Lugar AT ROW 16.42 COL 46 COLON-ALIGNED NO-LABEL WIDGET-ID 136
     w-Monto2 AT ROW 17.31 COL 13.14 COLON-ALIGNED NO-LABEL WIDGET-ID 112
     w-Fec_Transac AT ROW 17.38 COL 46 COLON-ALIGNED HELP
          "Ingrese la Fecha Contable en que se realizó la transacción" NO-LABEL WIDGET-ID 140
     w-Hora_Transac AT ROW 17.38 COL 58.72 COLON-ALIGNED NO-LABEL WIDGET-ID 142
     w-Comision AT ROW 18.27 COL 13.14 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     w-NomUsuario AT ROW 19.23 COL 13.43 COLON-ALIGNED NO-LABEL WIDGET-ID 150
     Btn-Visualizar AT ROW 18.35 COL 78 WIDGET-ID 8
     Btn_Imprimir AT ROW 3.92 COL 78.43 WIDGET-ID 166
     Btn_Consulta AT ROW 5.62 COL 78.57 WIDGET-ID 168
     Btn_Salvar AT ROW 7.92 COL 78.29 WIDGET-ID 18
     Btn_Ingresar AT ROW 9.65 COL 78.29 WIDGET-ID 160
     Btn_Cancelar AT ROW 13.04 COL 78.29 WIDGET-ID 16
     Btn_Salir AT ROW 16.54 COL 78.29 WIDGET-ID 20
     Btn_Borrar AT ROW 14.77 COL 78.29 WIDGET-ID 164
     w-NroTerminal AT ROW 18.31 COL 56 RIGHT-ALIGNED NO-LABEL WIDGET-ID 144
     w-Naud AT ROW 18.31 COL 57.43 NO-LABEL WIDGET-ID 146
     Btn_Modificar AT ROW 11.35 COL 78.29 WIDGET-ID 186
     "Lugar:" VIEW-AS TEXT
          SIZE 7 BY .69 AT ROW 16.38 COL 41 WIDGET-ID 128
          BGCOLOR 8 
     " Estado" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 9.5 COL 58.86 WIDGET-ID 104
     "Cuenta Nro:" VIEW-AS TEXT
          SIZE 11 BY .81 AT ROW 16.38 COL 3.29 WIDGET-ID 116
          BGCOLOR 8 
     "Fecha           Hora    TC    Cuenta               Monto TT #Aud  Lugar" VIEW-AS TEXT
          SIZE 72 BY .54 AT ROW 6.35 COL 3 WIDGET-ID 92
     " Tipo" VIEW-AS TEXT
          SIZE 5 BY .62 AT ROW 9.46 COL 31 WIDGET-ID 102
     " Monto" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 9.42 COL 7.29 WIDGET-ID 100
     "Nro.Tarjeta Débito" VIEW-AS TEXT
          SIZE 18 BY .81 AT ROW 3.92 COL 3 WIDGET-ID 28
     "Consecutivo:" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 1.19 COL 61.86 WIDGET-ID 174
          BGCOLOR 1 FGCOLOR 15 
     "Cédula" VIEW-AS TEXT
          SIZE 9 BY .81 AT ROW 1.27 COL 3 WIDGET-ID 22
     "Hora:" VIEW-AS TEXT
          SIZE 6 BY .54 AT ROW 2.23 COL 67.43 WIDGET-ID 178
          BGCOLOR 8 
     "Fecha:" VIEW-AS TEXT
          SIZE 7 BY .54 AT ROW 2.15 COL 46.86 WIDGET-ID 176
          BGCOLOR 8 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.14 BY 19.46
         FGCOLOR 0  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "Nombres Completos" VIEW-AS TEXT
          SIZE 19 BY .81 AT ROW 2.92 COL 3 WIDGET-ID 138
     " Comentarios" VIEW-AS TEXT
          SIZE 13.43 BY .62 AT ROW 11.5 COL 4.29 WIDGET-ID 158
          FGCOLOR 1 
     "Reporta:" VIEW-AS TEXT
          SIZE 8.72 BY .81 AT ROW 19.38 COL 5.57 WIDGET-ID 152
          BGCOLOR 8 
     "Transacción:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 15.5 COL 35 WIDGET-ID 126
          BGCOLOR 8 
     "Operación:" VIEW-AS TEXT
          SIZE 10.29 BY .54 AT ROW 18.38 COL 36.72 WIDGET-ID 132
          BGCOLOR 8 
     "Fecha y Hora:" VIEW-AS TEXT
          SIZE 13 BY .81 AT ROW 17.35 COL 34 WIDGET-ID 130
          BGCOLOR 8 
     "Comisión:" VIEW-AS TEXT
          SIZE 10 BY .81 AT ROW 18.27 COL 4.57 WIDGET-ID 120
          BGCOLOR 8 
     "Tipo Cuenta:" VIEW-AS TEXT
          SIZE 12 BY .81 AT ROW 15.5 COL 3 WIDGET-ID 124
          BGCOLOR 8 
     "Monto:" VIEW-AS TEXT
          SIZE 7.43 BY .81 AT ROW 17.35 COL 7.72 WIDGET-ID 118
          BGCOLOR 8 
     RECT-338 AT ROW 1 COL 1 WIDGET-ID 72
     Rec-Fondo AT ROW 15.19 COL 2 WIDGET-ID 106
     RECT-340 AT ROW 12 COL 2.29 WIDGET-ID 156
     RECT-341 AT ROW 3.85 COL 77.86 WIDGET-ID 170
     RECT-342 AT ROW 1.08 COL 45.86 WIDGET-ID 172
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 86.14 BY 19.46
         FGCOLOR 0  WIDGET-ID 100.

DEFINE FRAME F-Consulta
     Cmb-Consulta AT ROW 1.58 COL 2 NO-LABEL WIDGET-ID 22
     Btn_Salir2 AT ROW 2.62 COL 77.72 WIDGET-ID 20
     "Consecutivo Fecha           Hora    TC    Cuenta               Monto TT #Aud" VIEW-AS TEXT
          SIZE 74 BY .54 AT ROW 1 COL 2 WIDGET-ID 92
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.86 ROW 8.54
         SIZE 84.57 BY 4.58
         TITLE "Consulta Reclamaciones" WIDGET-ID 200.


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
         TITLE              = "Reclamos de Plástico - W-ReclamosPlastico.w"
         HEIGHT             = 19.46
         WIDTH              = 86
         MAX-HEIGHT         = 27.38
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 27.38
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Consulta:SENSITIVE        = FALSE.

/* SETTINGS FOR FRAME F-Main
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR BUTTON Btn-Visualizar IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       Btn-Visualizar:HIDDEN IN FRAME F-Main           = TRUE.

/* SETTINGS FOR BUTTON Btn_Borrar IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON Btn_Imprimir IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Ingresar IN FRAME F-Main
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Modificar IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F-Main
   NO-ENABLE 6                                                          */
/* SETTINGS FOR SELECTION-LIST Cmb-Estado IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR SELECTION-LIST Cmb-Tipo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-cedula IN FRAME F-Main
   NO-ENABLE LIKE = bdcentral.Tarjetas.Nit EXP-SIZE                     */
/* SETTINGS FOR FILL-IN w-Comision IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-Comision:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-Consecutivo IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       w-Consecutivo:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR EDITOR w-Descripcion IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-FecRec IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-FecRec:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-Fec_Transac IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-Fec_Transac:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-HoraRec IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-HoraRec:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-Hora_Transac IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-Hora_Transac:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-Lugar IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-Lugar:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-monto IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN w-Monto2 IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-Monto2:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-Naud IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       w-Naud:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-NomUsuario IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-NomUsuario:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-NroTerminal IN FRAME F-Main
   NO-ENABLE ALIGN-R                                                    */
ASSIGN 
       w-NroTerminal:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-Num_Cuenta IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-Num_Cuenta:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-TipoCta IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-TipoCta:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN w-Transaccion IN FRAME F-Main
   NO-ENABLE                                                            */
ASSIGN 
       w-Transaccion:READ-ONLY IN FRAME F-Main        = TRUE.

/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Consulta
/* Query rebuild information for FRAME F-Consulta
     _Query            is NOT OPENED
*/  /* FRAME F-Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "SHARE-LOCK KEEP-EMPTY"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Reclamos de Plástico - W-ReclamosPlastico.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Reclamos de Plástico - W-ReclamosPlastico.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn-Visualizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn-Visualizar C-Win
ON CHOOSE OF Btn-Visualizar IN FRAME F-Main /* Visualizar */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  listado = W_PathSpl + "L_Usuar.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Borrar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Borrar C-Win
ON CHOOSE OF Btn_Borrar IN FRAME F-Main /* Borrar */
DO:
    DEFINE VAR w-contAS AS INTEGER INITIAL 0.

    IF AVAILABLE(Reclamos_Tarj) THEN DO:
        MESSAGE "Esta seguro de Borrar el consecutivo " Reclamos_Tarj.Consecutivo " ?"
            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.

        IF choice THEN DO:
            FIND CURRENT Reclamos_Tarj NO-ERROR.
            DELETE Reclamos_Tarj.
            RELEASE Reclamos_Tarj.

            RUN inicializar_Variables.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME F-Main /* Cancelar */
DO:
  RUN Inicializar_Variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta C-Win
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Button 3 */
DO:
    ASSIGN FRAME F-main:SENSITIVE = FALSE.
    /*  HIDE   FRAME F-Main.  */
    ASSIGN FRAME F-Consulta:SENSITIVE = TRUE.

    VIEW FRAME F-Consulta.

    IF W-cedula NE "" THEN
        ENABLE Btn_modificar
               Btn_Cancelar
        WITH FRAME F-Main.
    ELSE
        DISABLE Btn_Imprimir
                Btn_Consulta
                Btn_Salir
                Btn_Salvar
                Btn_modificar
                Btn_Cancelar
            WITH FRAME F-Main.

    DO WITH FRAME F-Consulta:
        ASSIGN Cmb-Consulta:LIST-ITEMS = "".

        FOR EACH Reclamos_Tarj WHERE NO-LOCK BY Reclamos_Tarj.Fec_Transac DESC:
            Cmb-Consulta:ADD-LAST(STRING(Reclamos_Tarj.Consecutivo,"9999999999") + " " +
                                  STRING(Reclamos_Tarj.Fec_Transac,"99/99/9999") + " " + STRING(Reclamos_Tarj.Hora_Transac,"999999") + " " + 
                                  STRING(Reclamos_Tarj.TipoCuenta,"99")          + " " + STRING(Reclamos_Tarj.Num_cuenta,"X(13)") + " " +
                                  STRING(Reclamos_Tarj.Monto,"-99999999")        + " " + STRING(Reclamos_Tarj.TipoTransaccion,"99") + " " +
                                  STRING(Reclamos_Tarj.Naud,"X(4)")              + " " + Reclamos_Tarj.lugar ).
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir C-Win
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Button 2 */
/* Impresion de formatos TAC y CDAT*/
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
     DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
     listado = W_PathSpl + "L_Usuar.Lst".
     {Incluido\Imprimir.i "Listado" Tamano}
/*                                             */
/* DEFINE VAR Listado AS CHARACTER INITIAL "". */
/* Listado = W_Pathspl + "Lst_Reclamo.lst".    */
/* {incluido/imprimir.i "Listado"}.            */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar C-Win
ON CHOOSE OF Btn_Ingresar IN FRAME F-Main /* Ingresar */
DO:
    RUN Inicializar_Variables.
    
    ENABLE W-Descripcion
           w-monto
           Cmb-Tipo
           Cmb-Estado
           W-Cedula
           Cmb-TarjetaDb
        WITH FRAME F-Main.

    ASSIGN Cmb-TarjetaDb:LIST-ITEMS = ""
           W-Cedula = ""
           W-Consecutivo:SCREEN-VALUE = STRING(CURRENT-VALUE(Sec_ReclamoTdb) + 1)
           W-Consecutivo.

    FOR EACH tarjetas WHERE Tarjetas.estado EQ "01"
                        AND Tarjetas.Cue_Ahorros NE "" NO-LOCK BY tarjetas.tarjetaDb:
        W_Ok = Cmb-TarjetaDb:ADD-LAST(TRIM(STRING(Tarjetas.TarjetaDb))).
        ASSIGN W-Cedula = Tarjetas.Nit
               W-Cedula:SCREEN-VALUE = W-Cedula.
    END.

    DISABLE Btn_Consulta
            Btn_Ingresar
            Btn_Borrar
        WITH FRAME F-Main.

    ENABLE Btn_Salvar
           Btn_Cancelar
        WITH FRAME F-Main.

    ENABLE Cmb-Tipo
           Cmb-Estado
        WITH FRAME F-Main.

    Cmb-Tipo:SCREEN-VALUE = Cmb-Tipo:ENTRY(1).

    /*   RUN Mostrar_Registro.  */
    /*   APPLY 'entry' TO Cmb_TipoProductos. */
    /*   RETURN NO-APPLY.                    */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Modificar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Modificar C-Win
ON CHOOSE OF Btn_Modificar IN FRAME F-Main /* Modificar */
DO:
    DO WITH FRAME F-main:
        ASSIGN Cmb-tarjetaDb
               W-Cedula
               W-Monto
               Cmb-Tipo
               Cmb-Estado
               W-Descripcion
               W-TipoCta
               W-Num_Cuenta
               W-Monto2
               W-Comision
               W-NomUsuario
               W-Transaccion
               W-Lugar
               W-Fec_Transac
               W-Hora_Transac
               W-NroTerminal
               W-Naud.

        DISABLE Btn_Ingresar
                Btn_Modificar
                Btn_Consulta
            WITH FRAME F-Main.

        ENABLE Btn_Salvar
               Btn_Borrar
               Btn_CAncelar
            WITH FRAME F-Main.

        IF AVAILABLE(Reclamos_Tarj) THEN
            ENABLE W-Monto
                   Cmb-Tipo
                   Cmb-Estado
                   W-Descripcion
            WITH FRAME F-Main.
        
        W-Seleccion = 2.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir C-Win
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


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Btn_Salir2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir2 C-Win
ON CHOOSE OF Btn_Salir2 IN FRAME F-Consulta /* Salir */
DO:
   ASSIGN FRAME F-main:SENSITIVE     = TRUE.
   VIEW   FRAME F-Main.
   ASSIGN FRAME F-Consulta:SENSITIVE = FALSE.
   HIDE   FRAME F-Consulta.
   ENABLE Btn_Imprimir  Btn_Consulta  Btn_Salir  WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar C-Win
ON CHOOSE OF Btn_Salvar IN FRAME F-Main /* Salvar */
DO:
    DEFI VAR Mens1 AS CHARACTER FORMAT "X(80)" INITIAL "".
    DEFI VAR Mens2 AS CHARACTER FORMAT "X(80)" INITIAL "".
    DEFI VAR Mens3 AS CHARACTER FORMAT "X(80)" INITIAL "".
    DEFI VAR Mens4 AS CHARACTER FORMAT "X(80)" INITIAL "".

    DO WITH FRAME F-main:
        ASSIGN Cmb-tarjetaDb
               W-Cedula
               W-Monto
               Cmb-Tipo
               Cmb-Estado
               W-Descripcion
               W-TipoCta
               W-Num_Cuenta
               W-Monto2
               W-Comision
               W-NomUsuario
               W-Transaccion
               W-Lugar
               W-Fec_Transac
               W-Hora_Transac
               W-NroTerminal
               W-Naud.

        Grabando:
        DO TRANSACTION ON ERROR UNDO Grabando:
            IF W-Monto EQ 0 THEN Mens1 = "Debe Ingresar un Valor en el campo Monto".
            IF W-Cedula EQ "" THEN Mens2 = "Diligencie el campo cédula para el reclamo".
            IF W-Descripcion EQ "" THEN Mens3 = "Describa el Motivo del Reclamo".
            IF Cmb-TarjetaDb EQ "" THEN Mens4 = "Ingrese el número de la tarjeta Débito".
            
            IF Mens1 NE "" OR Mens2 NE "" OR Mens3 NE "" OR Mens4 NE "" THEN DO:
                MESSAGE "COMPLETAR LA INFORMACION QUE A CONTINUACION SE ENUNCIA "  SKIP(2)
                        Mens1 SKIP
                        Mens2 SKIP
                        Mens3 SKIP
                        Mens4
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
                RETURN.
            END.
            ELSE DO:
                IF W-Seleccion = 1 THEN DO:
                    FIND CURRENT Mov_Tarjetas NO-ERROR.
                    CREATE Reclamos_Tarj.
                    ASSIGN Reclamos_Tarj.Comision = Mov_tarjetas.Comision
                           Reclamos_Tarj.Consecutivo = NEXT-VALUE(Sec_ReclamoTdb)
                           Reclamos_Tarj.Descripcion = W-Descripcion
                           Reclamos_Tarj.Fec_Reclamo = W_Fecha
                           /*Reclamos_Tarj.Fec_Reverso       =  */
                           Reclamos_Tarj.Fec_Transac = Mov_Tarjetas.Fec_Transac
                           Reclamos_Tarj.Hora_Reclamo = STRING(TIME,"HH:MM:SS")
                           Reclamos_Tarj.Hora_Transac = Mov_tarjetas.Hora_Transac
                           Reclamos_Tarj.Lugar = Mov_Tarjetas.Lugar
                           Reclamos_Tarj.Monto = W-Monto
                           Reclamos_Tarj.MontoTransac = Mov_Tarjetas.Monto
                           Reclamos_Tarj.Naud = Mov_Tarjetas.Naud
                           Reclamos_Tarj.Nit = Mov_Tarjetas.Nit
                           Reclamos_Tarj.NroTerminal = Mov_Tarjetas.NroTerminal
                           Reclamos_Tarj.Num_cuenta = Mov_Tarjetas.Num_cuenta
                           /*Reclamos_Tarj.Reportado         =  */
                           /*Reclamos_Tarj.Reversado         =  */
                           Reclamos_Tarj.TarjetaDB = Mov_Tarjetas.TarjetaDb
                           Reclamos_Tarj.TipoCuenta = Mov_Tarjetas.TipoCuenta
                           Reclamos_Tarj.TipoRed = Mov_Tarjetas.TipoRed
                           Reclamos_Tarj.TipoTransaccion = Mov_Tarjetas.TipoTransaccion
                           Reclamos_Tarj.Usuario = W_Usuario.
                    APPLY "CHOOSE" TO btn-visualizar.
                    RELEASE Mov_Tarjetas.
                END.
                ELSE DO: /* Actualizar */
                    FIND CURRENT Reclamos_Tarj NO-ERROR.
                    ASSIGN Reclamos_Tarj.TipoRec = INTEGER(SUBSTRING(Cmb-Tipo:SCREEN-VALUE   IN FRAME F-MAIN,1,2))
                           Reclamos_tarj.EstadoRec = INTEGER(SUBSTRING(Cmb-Estado:SCREEN-VALUE IN FRAME F-MAIN,1,2))
                           Reclamos_Tarj.Descripcion = W-Descripcion
                           Reclamos_Tarj.Monto = W-Monto.
                    APPLY "CHOOSE" TO btn-visualizar.
                    RELEASE Reclamos_Tarj.
                END.
            END.

            ASSIGN W-Seleccion = 1.
        END.

        RUN inicializar_variables.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Cmb-Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-Consulta C-Win
ON VALUE-CHANGED OF Cmb-Consulta IN FRAME F-Consulta
DO:
    DEFI VAR zconsec LIKE Mov_tarjetas.Monto.

    ASSIGN Cmb-Consulta.

    ASSIGN zconsec = INTEGER(SUBSTRING(Cmb-Consulta,1,10))
           Cmb-TarjetaDb:LIST-ITEMS IN FRAME F-Main= "".

    FIND FIRST Reclamos_Tarj WHERE Reclamos_Tarj.Consecutivo = zconsec NO-LOCK.
    IF AVAILABLE(Reclamos_Tarj) THEN DO:
        CASE Reclamos_Tarj.TipoTransaccion:
            WHEN 0 THEN
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "Cobro Cuota/Plástico      "    W-Transaccion.
            WHEN 1 THEN
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "Consulta Cajero           "    W-Transaccion.
            WHEN 2 THEN
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "Retiro Cajero             "    W-Transaccion.
            WHEN 3 THEN
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "Pago en POS               "    W-Transaccion.
            WHEN 4 THEN
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "Declinada Cajero          "    W-Transaccion.
            WHEN 5 THEN
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "Reversa Credito           "    W-Transaccion.
            WHEN 6 THEN
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "Reversa Débito            "    W-Transaccion.
            WHEN 7 THEN
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "Transacc.Datáf Cooperativa"    W-Transaccion.
            WHEN 8 THEN
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "Consignaciones MegaExpress"    W-Transaccion.
            WHEN 9 THEN
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "Pago Servicios Públicos   "    W-Transaccion.
            OTHERWISE
                ASSIGN W-Transaccion:SCREEN-VALUE IN FRAME F-Main = "No Identificada           "    W-Transaccion.
        END CASE.

        ASSIGN w-monto:SCREEN-VALUE = STRING(Reclamos_Tarj.monto,"->>,>>>,>>9")         W-monto
               w-Num_Cuenta:SCREEN-VALUE = Reclamos_Tarj.Num_cuenta                     W-Num_Cuenta
               w-monto2:SCREEN-VALUE = string(Reclamos_Tarj.MontoTransac,"->>,>>>,>>9") W-monto2
               W-Comision:SCREEN-VALUE = STRING(Reclamos_Tarj.Comision,"->>,>>>,>>9")   W-Comision
               W-Lugar:SCREEN-VALUE = Reclamos_Tarj.Lugar                               W-Lugar
               W-Fec_Transac:SCREEN-VALUE = STRING(Reclamos_Tarj.Fec_Transac)           W-Fec_Transac
               W-Hora_Transac:SCREEN-VALUE = STRING(Reclamos_Tarj.Hora_Transac)         W-Hora_Transac
               W-NroTerminal:SCREEN-VALUE = Reclamos_Tarj.NroTerminal                   W-NroTerminal
               W-Naud:SCREEN-VALUE = Reclamos_Tarj.Naud                                 W-Naud
               W-Descripcion:SCREEN-VALUE = Reclamos_Tarj.Descripcion                   W-Descripcion
               W-Cedula:SCREEN-VALUE = Reclamos_Tarj.Nit                                W-Cedula
               W-Consecutivo:SCREEN-VALUE = STRING(Reclamos_Tarj.Consecutivo,"99999999")
               W-FecRec:SCREEN-VALUE = STRING(Reclamos_Tarj.Fec_Reclamo,"99/99/9999")
               W-HoraRec:SCREEN-VALUE = Reclamos_Tarj.Hora_Reclamo.

        Cmb-TarjetaDb:ADD-LAST(STRING(Reclamos_Tarj.TarjetaDb)) IN FRAME F-Main.
        IF Reclamos_Tarj.TipoRec EQ 1 THEN
            Cmb-Tipo:SCREEN-VALUE = "01 DEBITO NO PAGO".
        ELSE
            Cmb-Tipo:SCREEN-VALUE = "02 DEBITO PARCIAL".

        CASE Reclamos_Tarj.EstadoRec:
            WHEN 1 THEN Cmb-Estado:SCREEN-VALUE = "01 PENDIENTE".
            WHEN 2 THEN Cmb-Estado:SCREEN-VALUE = "02 REVERSADO".
            WHEN 3 THEN Cmb-Estado:SCREEN-VALUE = "03 NO EXITOSO".
        END CASE.

        ASSIGN Cmb-TarjetaDb.
        IF Reclamos_Tarj.TipoCuenta EQ 10 THEN
            w-TipoCta:SCREEN-VALUE = "Ahorros".
        ELSE
            W-TipoCta:SCREEN-VALUE = "Cupo Rotativo".

        FIND FIRST usuarios WHERE Usuarios.Usuario = Reclamos_Tarj.Usuario NO-LOCK NO-ERROR.
        IF AVAILABLE(usuarios) THEN
            ASSIGN W-NomUsuario:SCREEN-VALUE = Usuarios.Usuario + " - "  + TRIM(UsuarioS.Nombre).

        FIND FIRST Clientes WHERE Clientes.Nit = Reclamos_Tarj.Nit NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN
            ASSIGN W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2) W_NomTitular.

        DISABLE Cmb-TarjetaDb
                W-Cedula
                Btn_Ingresar
                Btn_Consulta
            WITH FRAME f-main.

        ENABLE Btn_Modificar
               Btn_Cancelar
            WITH FRAME f-main.
    END.
    ELSE
        MESSAGE "No se encontrá movimiento"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Cmb-MovTar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-MovTar C-Win
ON VALUE-CHANGED OF Cmb-MovTar IN FRAME F-Main
DO:
    DEFI VAR zfecT LIKE Mov_tarjetas.Fec_Transac.
    DEFI VAR ztipCta LIKE Mov_tarjetas.TipoCuenta.
    DEFI VAR znumCta LIKE Mov_tarjetas.Num_Cuenta.
    DEFI VAR ztipTra LIKE Mov_Tarjetas.TipoTransaccion.
    DEFI VAR znaud LIKE Mov_tarjetas.Naud.
    DEFI VAR zmonto LIKE Mov_tarjetas.Monto.

    ASSIGN Cmb-MovTar
           Cmb-TarjetaDb.

    ASSIGN zfecT = DATE(INTEGER(SUBSTRING(Cmb-MovTar,4,2)), INTEGER(SUBSTRING(Cmb-MovTar,1,2)), INTEGER(SUBSTRING(Cmb-MovTar,7,4)))
           ztipCta = INTEGER(SUBSTRING(Cmb-MovTar,19,2))
           znumCta = SUBSTRING(Cmb-MovTar,22,10)
           ztipTra = INTEGER(SUBSTRING(Cmb-MovTar,46,2))
           znaud = SUBSTRING(Cmb-MovTar,49,4)
           zmonto = DECIMAL(SUBSTRING(Cmb-MovTar,36,9)).

    FIND FIRST mov_tarjetas WHERE Mov_Tarjetas.TarjetaDB = Cmb-TarjetaDb
                              AND Mov_Tarjetas.Fec_transac = zfect
                              AND Mov_Tarjetas.TipoCuenta = ztipcta
                              AND Mov_Tarjetas.Num_Cuenta = znumcta
                              AND Mov_Tarjetas.TipoTransaccion = ztiptra
                              AND Mov_Tarjetas.Naud BEGINS znaud
                              AND Mov_Tarjetas.monto = zmonto NO-LOCK.
    IF AVAILABLE(Mov_Tarjeta) THEN DO:
        CASE Mov_Tarjetas.TipoTransaccion:
            WHEN 0 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Cobro Cuota/Plástico      "  W-Transaccion.
            WHEN 1 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Consulta Cajero           "  W-Transaccion.
            WHEN 2 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Retiro Cajero             "  W-Transaccion.
            WHEN 3 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Pago en POS               "  W-Transaccion.
            WHEN 4 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Declinada Cajero          "  W-Transaccion.
            WHEN 5 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Reversa Credito           "  W-Transaccion.
            WHEN 6 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Reversa Débito            "  W-Transaccion.
            WHEN 7 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Transacc.Datáf Cooperativa"  W-Transaccion.
            WHEN 8 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Consignaciones MegaExpress"  W-Transaccion.
            WHEN 9 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Pago Servicios Públicos   "  W-Transaccion.
            OTHERWISE   ASSIGN W-Transaccion:SCREEN-VALUE = "No Identificada           "  W-Transaccion.
        END CASE.

        ASSIGN w-monto:SCREEN-VALUE = STRING(Mov_tarjetas.monto,"->>,>>>,>>9")          w-monto
               w-Num_Cuenta:SCREEN-VALUE = Mov_Tarjetas.Num_cuenta                      w-Num_Cuenta
               w-monto2:SCREEN-VALUE = string(Mov_Tarjetas.Monto,"->>,>>>,>>9")         w-monto2
               W-Comision:SCREEN-VALUE = STRING(Mov_Tarjetas.Comision,"->>,>>>,>>9")    W-Comision
               W-Lugar:SCREEN-VALUE = Mov_tarjetas.Lugar                                W-Lugar
               W-Fec_Transac:SCREEN-VALUE = STRING(Mov_Tarjetas.Fec_Transac)            W-Fec_Transac
               W-Hora_Transac:SCREEN-VALUE = STRING(Mov_Tarjetas.Hora)                  W-Hora_Transac
               W-NroTerminal:SCREEN-VALUE = Mov_Tarjetas.NroTerminal                    W-NroTerminal
               W-Naud:SCREEN-VALUE = Mov_tarjetas.Naud                                  W-Naud.

        IF Mov_tarjeta.TipoCuenta EQ 10 THEN
            w-TipoCta:SCREEN-VALUE = "Ahorros".
        ELSE
            W-TipoCta:SCREEN-VALUE = "Cupo Rotativo".

        FIND FIRST usuarios WHERE Usuarios.Usuario = W_Usuario NO-LOCK NO-ERROR.
        IF AVAILABLE(usuarios) THEN
            ASSIGN W-NomUsuario:SCREEN-VALUE = Usuarios.Usuario + " - "  + TRIM(UsuarioS.Nombre).
    END.
    ELSE
        MESSAGE "No se encontró movimiento"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb-TarjetaDb
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb-TarjetaDb C-Win
ON VALUE-CHANGED OF Cmb-TarjetaDb IN FRAME F-Main
DO:
    ASSIGN Cmb-TarjetaDb.

    FIND FIRST Tarjetas WHERE tarjetas.TarjetaDb = Cmb-TarjetaDb NO-LOCK NO-ERROR.

    ASSIGN W-Cedula:SCREEN-VALUE = Tarjetas.nit  W-Cedula.

    FIND Clientes WHERE Clientes.Nit EQ W-Cedula:SCREEN-VALUE NO-LOCK NO-ERROR.
    FIND FIRST agencias WHERE agencias.agencia = Tarjetas.agencia NO-LOCK NO-ERROR.

    ASSIGN Cmb-MovTar:LIST-ITEMS = "".

    IF AVAILABLE(Tarjetas) THEN DO:
        ASSIGN W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).

        FOR EACH Mov_tarjetas WHERE Mov_Tarjetas.TarjetaDb = Tarjetas.TarjetaDb
                                AND Mov_Tarjetas.TipoTransaccion NE 0 NO-LOCK BY Mov_Tarjetas.Fec_Transac DESC:
            Cmb-MovTar:ADD-LAST(STRING(Mov_Tarjetas.Fec_Transac,"99/99/9999") + " " + STRING(Mov_Tarjetas.Hora_Transac,"999999") + " " +
                                STRING(Mov_tarjetas.TipoCuenta,"99") + " " + STRING(Num_cuenta,"X(13)") + " " +
                                STRING(Mov_Tarjetas.Monto,"-99999999") + " " + STRING(Mov_Tarjetas.TipoTransaccion,"99") + " " +
                                STRING(Mov_Tarjetas.Naud,"X(4)") + " " + Mov_tarjetas.lugar ).
        END.

        ASSIGN W-Monto:SCREEN-VALUE = ""
               W-Monto
               w-Num_Cuenta:SCREEN-VALUE = ""
               W-Num_Cuenta
               w-monto2:SCREEN-VALUE = ""
               W-Monto2
               W-Comision:SCREEN-VALUE = ""
               W-Comision
               W-Lugar:SCREEN-VALUE = ""
               W-Lugar
               W-Fec_Transac:SCREEN-VALUE = ""
               W-Fec_Transac
               W-NroTerminal:SCREEN-VALUE = ""
               W-NroTerminal
               W-Naud:SCREEN-VALUE = ""
               W-Naud
               W-Transaccion:SCREEN-VALUE = ""
               W-Transaccion
               W-TipoCta:SCREEN-VALUE = ""
               W-TipoCta
               W-Hora_Transac:SCREEN-VALUE = ""
               W-Hora_Transac
               W-Descripcion:SCREEN-VALUE = ""
               W-Descripcion
               W-HoraRec:SCREEN-VALUE = STRING(TIME,"HH:MM:SS")
               W-HoraRec
               W-FecRec:SCREEN-VALUE = STRING(W_Fecha)
               W-FecRec
               Cmb-Tipo:SCREEN-VALUE = "01 DEBITO NO PAGO"
               Cmb-Tipo
               Cmb-Estado:SCREEN-VALUE = "01 PENDIENTE"
               Cmb-Estado.
    END.
    ELSE
        MESSAGE "No encontró " Tarjetas.TarjetaDb
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w-cedula
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-cedula C-Win
ON LEAVE OF w-cedula IN FRAME F-Main
DO:
    DO WITH FRAME F-main:
        /* DISABLE btn_Salvar  WITH FRAME f-main.  */
        ASSIGN Cmb-MovTar:LIST-ITEMS = "".

        FIND Clientes WHERE Clientes.Nit EQ W-cedula:SCREEN-VALUE NO-LOCK NO-ERROR.
        IF AVAILABLE(Clientes) THEN
            W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
        ELSE DO:
            RUN C-Clientes.R(INPUT 2,
                             INPUT W_Agencia,
                             OUTPUT P_Nit,
                             OUTPUT P_Nombre,
                             OUTPUT P_Apellido,
                             OUTPUT P_AgeCli).

            ASSIGN W-cedula:SCREEN-VALUE = P_Nit
                   W_Nomtitular:SCREEN-VALUE = CAPS(TRIM(P_Nombre) + " " + P_Apellido).

            FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
        END.

        IF Clientes.Estado EQ 2 AND Clientes.Fec_Retiro NE ? THEN DO:
            MESSAGE "No se pueden crear cuentas de ahorro para clientes Retirados" SKIP
                VIEW-AS ALERT-BOX WARNING.
            APPLY "choose" TO Btn_Cancelar.
            RETURN NO-APPLY.
        END.

        IF Clientes.Tipo_Vinculo GT 2 THEN DO:
            MESSAGE W_NomTitular " No es un cliente de la Cooperativa" SKIP
                    "La persona o empresa debe estar matriculado como" SKIP
                    "Cliente o Asociado. Rectifique!"
                VIEW-AS ALERT-BOX.
            APPLY 'choose' TO btn_Cancelar.
            RETURN NO-APPLY.
        END.

        ASSIGN Cmb-TarjetaDb:LIST-ITEMS = "".

        FOR EACH tarjetas WHERE Tarjetas.estado EQ "01"
                            AND Tarjetas.Nit EQ Clientes.Nit
                            AND Tarjetas.Cue_Ahorros NE "" NO-LOCK BY Tarjetas.Nit:
            W_Ok = Cmb-TarjetaDb:ADD-LAST(TRIM(STRING(Tarjetas.TarjetaDb))).

            FIND FIRST agencias WHERE agencias.agencia = Tarjetas.agencia NO-LOCK NO-ERROR.
        END.
    END.
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
  RUN inicializar_variables.
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
  DISPLAY w-FecRec w-HoraRec w-Consecutivo w-cedula W_NomTitular Cmb-TarjetaDb 
          Cmb-MovTar w-monto Cmb-Tipo Cmb-Estado w-Descripcion w-TipoCta 
          w-Transaccion w-Num_Cuenta w-Lugar w-Monto2 w-Fec_Transac 
          w-Hora_Transac w-Comision w-NomUsuario w-NroTerminal w-Naud 
      WITH FRAME F-Main IN WINDOW C-Win.
  ENABLE Cmb-TarjetaDb Cmb-MovTar Btn_Imprimir Btn_Ingresar Btn_Salir RECT-338 
         Rec-Fondo RECT-340 RECT-341 RECT-342 
      WITH FRAME F-Main IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY Cmb-Consulta 
      WITH FRAME F-Consulta IN WINDOW C-Win.
  ENABLE Cmb-Consulta Btn_Salir2 
      WITH FRAME F-Consulta IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
  FRAME F-Consulta:SENSITIVE = NO.
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TarDebNew C-Win 
PROCEDURE Grabar_TarDebNew :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
  DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
  DEFINE VAR wretcode      AS INTEGER INITIAL -1.

  FOR EACH tmp-tarjetadb:
     CREATE tarjetadebito.
     BUFFER-COPY tmp-tarjetadb TO tarjetadebito.
     ASSIGN wvlrmonTD = STRING(tarjetadebito.Monto,'9999999999999').
     RUN TranWebCaja(1,tarjetadebito.TipoTransaccion,TRIM(tarjetadebito.Nit), TRIM(tarjetadebito.Cue_Ahorros), tarjetadebito.TarjetaDB, wvlrmonTD, INPUT-OUTPUT wsecTD, INPUT-OUTPUT wsdoTD, INPUT-OUTPUT wdisTD, OUTPUT wretcode).
     IF wretcode = 0 THEN 
        ASSIGN tarjetadebito.Secuencia       = wsecTD 
               tarjetadebito.SdoTotal        = ROUND(decimal(wsdoTD) / 100,2)
               tarjetadebito.SdoDispon       = ROUND(decimal(wdisTD) / 100,2)
               tarjetadebito.RetCode         = wretcode
               tarjetadebito.Aplicado        = YES.
     ELSE 
        ASSIGN tarjetadebito.RetCode         = wretcode.
     RELEASE tarjetadebito.
  END.
  /* Barrido de la tabla tarjetadb */
  FOR EACH tmp-tarjetadb:
      DELETE tmp-tarjetadb.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TmpTarDeb C-Win 
PROCEDURE Grabar_TmpTarDeb :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER wvlrTrans  LIKE ahorros.sdo_disponible.
DEFINE INPUT PARAMETER wdesTrans  LIKE Mov_contable.comentario.
DEFINE INPUT PARAMETER wtipoTrans AS INTEGER.
DEFINE VAR wvlrmonTD     AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsecTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wsdoTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wdisTD        AS CHARACTER INITIAL '0000000000000'.
DEFINE VAR wretcode      AS INTEGER INITIAL -1.

IF wtipotrans NE 0 THEN DO:
   FIND CURRENT ahorros  NO-ERROR.
   CREATE Tmp-tarjetadb.
   ASSIGN Tmp-tarjetadb.Agencia         =  w_agencia                                                                                                                 
          Tmp-tarjetadb.Usuario         =  w_usuario                                                                                                                 
          Tmp-tarjetadb.Comprobante     =  Comprobante.Comprobante                                                                                                   
          Tmp-tarjetadb.Num_Documento   =  Comprobantes.Secuencia                                                                                                    
          Tmp-tarjetadb.Fec_Contable    =  TODAY                                                                                                                     
          Tmp-tarjetadb.Hora            =  TIME                                                                                                                      
          Tmp-tarjetadb.Comentario      =  wdesTrans                                                                                                                 
          Tmp-tarjetadb.Aplicado        =  NO                                                                                                                        
          Tmp-tarjetadb.ManBiometrico   =  1                                                                                                                         
          Tmp-tarjetadb.TipoTransaccion =  wtipotrans                                                                                                                
          Tmp-tarjetadb.Nit             =  Ahorros.nit                                                                                                               
          Tmp-tarjetadb.Cue_Ahorros     =  Ahorros.cue_ahorros  /* SUBSTRING(Ahorros.TarjetaDB,10,7) */                                                              
          Tmp-tarjetadb.TarjetaDB       =  Ahorros.TarjetaDB                                                                                                         
          Tmp-tarjetadb.Monto           =  wvlrTrans                                                                                                                 
          Tmp-tarjetadb.Secuencia       =  "000000000000"                                                                                                            
          Tmp-tarjetadb.SdoTotal        =  0                                                                                                                         
          Tmp-tarjetadb.SdoDispon       =  0                                                                                                                         
          Tmp-tarjetadb.RetCode         = -1.
   RELEASE Tmp-Tarjetadb.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables C-Win 
PROCEDURE Inicializar_Variables :
HIDE FRAME F-Consulta.
VIEW FRAME F-Main.

FIND FIRST Cfg_Tarjeta NO-LOCK NO-ERROR.

IF w-seleccion EQ 1 OR W-Seleccion EQ 2 THEN DO:
    ASSIGN W-Cedula:SCREEN-VALUE IN FRAME f-main = ""
           W-cedula
           W-Consecutivo:SCREEN-VALUE = ""
           W-Consecutivo
           W-Monto:SCREEN-VALUE = ""
           W-Monto
           w_NomTitular:SCREEN-VALUE = ""
           W_NomTitular
           Cmb-MovTar:LIST-ITEMS = ""
           Cmb-MovTar
           Cmb-TarjetaDb:LIST-ITEMS = ""
           Cmb-TarjetaDb
           w-monto:SCREEN-VALUE = ""
           W-Monto
           w-Num_Cuenta:SCREEN-VALUE = ""
           W-Num_Cuenta
           w-monto2:SCREEN-VALUE = ""
           W-Monto2
           W-Comision:SCREEN-VALUE = ""
           W-Comision
           W-Lugar:SCREEN-VALUE = ""
           W-Lugar
           W-Fec_Transac:SCREEN-VALUE = ""
           W-Fec_Transac
           W-NroTerminal:SCREEN-VALUE = ""
           W-NroTerminal
           W-Naud:SCREEN-VALUE = ""
           W-Naud
           W-Cedula:SCREEN-VALUE = ""
           W-Cedula
           W-Transaccion:SCREEN-VALUE = ""
           W-Transaccion
           W-TipoCta:SCREEN-VALUE = ""
           W-TipoCta
           W-Hora_Transac:SCREEN-VALUE = ""
           W-Hora_Transac
           W-Descripcion:SCREEN-VALUE = ""
           W-Descripcion
           W-HoraRec:SCREEN-VALUE = STRING(TIME,"HH:MM:SS")
           W-HoraRec
           W-FecRec:SCREEN-VALUE = STRING(W_Fecha)
           W-FecRec.

    DISABLE Btn_Salvar
            Btn_Cancelar
            Btn_Borrar
            Btn_Modificar
            w-monto
            Cmb-Tipo
            Cmb-Estado
            W-Descripcion
            W-Cedula
        WITH FRAME F-Main.

    ENABLE Btn_Consulta
           Btn_Ingresar
        WITH FRAME F-Main.
END.
ELSE
    RUN Mostrar_Variables.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Variables C-Win 
PROCEDURE Mostrar_Variables :
/*------------------------------------------------------------------------------
  Autor: JOHN JAIRO MONCADA PUERTA    
  Cel  : 311 322 54 58 - Casa 253 49 71 
  Notes: diseñado bajo los parametros de Enpacto Administrativo      
------------------------------------------------------------------------------*/
IF AVAILABLE(Reclamos_Tarj) THEN DO:
  FIND FIRST Clientes WHERE Clientes.nit = Reclamos_Tarj.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN
      W_NomTitular:SCREEN-VALUE IN FRAME F-Main = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
     
  ASSIGN W-Cedula:SCREEN-VALUE           = Reclamos_Tarj.Nit                                W-Cedula
         W-Consecutivo:SCREEN-VALUE      = STRING(Reclamos_Tarj.Consecutivo)                W-Consecutivo
         Cmb-TarjetaDb:SCREEN-VALUE      = Reclamos_Tarj.TarjetaDB                          Cmb-TarjetaDb
         W-Monto:SCREEN-VALUE            = STRING(Reclamos_Tarj.Monto,"->>,>>>,>>9")        W-Monto
         W-Descripcion:SCREEN-VALUE      = Reclamos_Tarj.Descripcion                        W-Descripcion
         w-Num_Cuenta:SCREEN-VALUE       = Reclamos_Tarj.Num_Cuenta                         W-Num_Cuenta
         w-monto2:SCREEN-VALUE           = STRING(Reclamos_Tarj.MontoTransac,"->>,>>>,>>9") W-Monto2
         W-Comision:SCREEN-VALUE         = STRING(Reclamos_Tarj.Comision,"->>,>>>,>>9")     W-Comision
         W-Lugar:SCREEN-VALUE            = Reclamos_Tarj.Lugar                              W-Lugar
         W-Fec_Transac:SCREEN-VALUE      = STRING(Reclamos_Tarj.Fec_Transac)                W-Fec_Transac
         W-NroTerminal:SCREEN-VALUE      = Reclamos_Tarj.NroTerminal                        W-NroTerminal
         W-Naud:SCREEN-VALUE             = Reclamos_Tarj.Naud                               W-Naud                           
         W-Cedula:SCREEN-VALUE           = Reclamos_Tarj.Nit                                W-Cedula
         W-Hora_Transac:SCREEN-VALUE     = STRING(Reclamos_Tarj.Hora_Transac)               W-Hora_Transac
         W-TipoCta:SCREEN-VALUE          = ""  W-TipoCta.
  IF Reclamos_Tarj.TipoRec EQ 1 THEN
     Cmb-Tipo:SCREEN-VALUE    =  "01 DEBITO NO PAGO".
  ELSE
     Cmb-Tipo:SCREEN-VALUE    =  "02 DEBITO PARCIAL".
  CASE Reclamos_Tarj.EstadoRec:
      WHEN 1 THEN Cmb-Estado:SCREEN-VALUE = "01 PENDIENTE".
      WHEN 2 THEN Cmb-Estado:SCREEN-VALUE = "02 REVERSADO".
      WHEN 3 THEN Cmb-Estado:SCREEN-VALUE = "03 NO EXITOSO".
  END CASE.
  CASE Reclamos_Tarj.TipoTransaccion:
      WHEN 0 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Cobro Cuota/Plástico      "  W-Transaccion.
      WHEN 1 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Consulta Cajero           "  W-Transaccion.
      WHEN 2 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Retiro Cajero             "  W-Transaccion.
      WHEN 3 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Pago en POS               "  W-Transaccion.
      WHEN 4 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Declinada Cajero          "  W-Transaccion.
      WHEN 5 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Reversa Credito           "  W-Transaccion.
      WHEN 6 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Reversa Débito            "  W-Transaccion.
      WHEN 7 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Transacc.Datáf Cooperativa"  W-Transaccion.
      WHEN 8 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Consignaciones MegaExpress"  W-Transaccion.
      WHEN 9 THEN ASSIGN W-Transaccion:SCREEN-VALUE = "Pago Servicios Públicos   "  W-Transaccion.
      OTHERWISE   ASSIGN W-Transaccion:SCREEN-VALUE = "No Identificada           "  W-Transaccion.
  END CASE.
  IF Reclamos_Tarj.TipoCuenta EQ 10 THEN
    w-TipoCta:SCREEN-VALUE = "Ahorros".
  ELSE
    W-TipoCta:SCREEN-VALUE = "Cupo Rotativo".
  FIND FIRST usuarios WHERE Usuarios.Usuario = Reclamos_Tarj.Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE(usuarios) THEN
     ASSIGN W-NomUsuario:SCREEN-VALUE = Reclamos_Tarj.Usuario + " - "  + TRIM(Usuarios.Nombre).

END.
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
 DEFINE VAR w-mensrec    AS CHARACTER FORMAT "X(80)" INITIAL "" NO-UNDO.
 DEFINE VAR w-mensestado AS CHARACTER FORMAT "X(80)" INITIAL "" NO-UNDO.
 DEFINE VAR W-MTran      AS CHARACTER FORMAT "X(80)" INITIAL "" NO-UNDO.
 DEFINE VAR w-mred       AS CHARACTER FORMAT "X(80)" INITIAL "" NO-UNDO.
 DEFINE VAR w-nomus      AS CHARACTER FORMAT "X(80)" INITIAL "" NO-UNDO.
 DEFINE VAR w-nomt       AS CHARACTER FORMAT "X(80)" INITIAL "" NO-UNDO.
 FIND FIRST cfg_tarjetaDb NO-LOCK NO-ERROR.
 W_Reporte    = "REPORTE   : RECLAMOS :  - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 FIND CURRENT Reclamos_Tarj NO-LOCK NO-ERROR.
 IF AVAILABLE(Reclamos_Tarj) THEN DO:
     W_EncColumna = "Reclamo Nro:" + string(Reclamos_Tarj.Consecutivo,"99999999") + "    Tarjeta Débito Nro.: " + TRIM(Reclamos_Tarj.tarjetaDb) + "   Fecha Reclamo: " + STRING(Reclamos_Tarj.Fec_Reclamo,"99/99/9999").
     VIEW FRAME F-Encabezado.
     VIEW FRAME F-Ftr.
     FIND FIRST clientes WHERE clientes.nit = Reclamos_tarj.nit NO-LOCK NO-ERROR.
     FIND FIRST redes    WHERE Redes.Red    = Reclamos_tarj.TipoRed  NO-LOCK NO-ERROR.
     IF AVAILABLE redes THEN  w-mred = Redes.nombre.
     FIND FIRST usuarios WHERE Usuarios.Usuario = Reclamos_Tarj.Usuario NO-LOCK NO-ERROR.
     IF AVAILABLE(usuarios) THEN w-nomus = Usuarios.usuario + " - " + TRIM(Usuarios.Nombre).
     CASE Reclamos_Tarj.TipoTransaccion:
         WHEN 0 THEN ASSIGN W-MTran = "Cobro Cuota/Plástico      ".
         WHEN 1 THEN ASSIGN W-MTran = "Consulta Cajero           ".
         WHEN 2 THEN ASSIGN W-MTran = "Retiro Cajero             ".
         WHEN 3 THEN ASSIGN W-MTran = "Pago en POS               ".
         WHEN 4 THEN ASSIGN W-MTran = "Declinada Cajero          ".
         WHEN 5 THEN ASSIGN W-MTran = "Reversa Credito           ".
         WHEN 6 THEN ASSIGN W-MTran = "Reversa Débito            ".
         WHEN 7 THEN ASSIGN W-MTran = "Transacc.Datáf Cooperativa".
         WHEN 8 THEN ASSIGN W-MTran = "Consignaciones MegaExpress".
         WHEN 9 THEN ASSIGN W-MTran = "Pago Servicios Públicos   ".
         OTHERWISE   ASSIGN W-MTran = "No Identificada           ".
     END CASE.

     IF Reclamos_Tarj.TipoRec = 1 THEN
        w-mensrec = "DEBITO NO PAGO".
     ELSE
        w-mensrec = "DEBITO PARCIAL".
     CASE Reclamos_Tarj.Estado:
         WHEN 1 THEN w-mensestado = "PENDIENTE".
         WHEN 2 THEN w-mensestado = "REVERSADO".
         WHEN 3 THEN w-mensestado = "NO EXITOSO".
     END CASE.
     IF AVAILABLE(Clientes) THEN
        w-nomt = TRIM(Clientes.Nombre) + " " + TRIM(clientes.apellido1) + " " + TRIM(Clientes.apellido2). 
         DISPLAY "DETALLE DEL RECLAMO:" AT 1 FORMAT "X(50)"
                 "El Asociado: " + TRIM(w-nomt)  AT 1 FORMAT "X(80)"
                 "Identificado con " + Clientes.Tipo_Identificacion +  ": " + TRIM(Reclamos_Tarj.Nit) AT 1  FORMAT "X(80)"
                 "y con la Tarjeta Nro: " + Reclamos_Tarj.TarjetaDb   AT 1 FORMAT "X(80)"
                 "Estado del reclamo  : " + w-mensestado  AT 1  FORMAT "X(80)"
                 " "   AT 1 FORMAT "X(80)"
                 "Reclama la siguiente operación por concepto de: " + TRIM(w-mensrec)  AT 1 FORMAT "X(80)"
                 "Por valor de : $" + STRING(Reclamos_Tarj.Monto,"->>,>>>,>>9")      AT 1 FORMAT "X(80)"
                 "A continuación se detalla la operación en mención: "   AT 1 FORMAT "X(80)"
                 " " AT 1 FORMAT "x(80)"
                 WITH FRAME F-mov1 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
     
     DISPLAY "La transacción está identificada con la Operación única No.: " +  TRIM(Reclamos_Tarj.NroTerminal) + TRIM(Reclamos_Tarj.Naud) AT 1 FORMAT "X(80)"
             "que presenta los siguientes valores: "  AT 1 FORMAT "X(80)"
             "Monto        : $" + STRING(Reclamos_Tarj.MontoTrans,"->>,>>>,>>9")    AT 1 FORMAT "X(80)"
             "Comisión     : $" + STRING(Reclamos_Tarj.Comision,"->>,>>>,>>9")      AT 1 FORMAT "X(80)"
             "Nro. Cuenta  :  " + TRIM(Reclamos_Tarj.Num_Cuenta)     AT 1 FORMAT "X(80)"
             "Cod.Terminal :  " + TRIM(Reclamos_Tarj.NroTerminal)    AT 1 FORMAT "X(80)"
             "Nom.Terminal :  " + TRIM(Reclamos_Tarj.Lugar)          AT 1 FORMAT "X(80)"
             "Fecha Transac:  " + STRING(Reclamos_Tarj.Fec_Transac,"99/99/9999")  AT 1 FORMAT "X(80)"
             "Hora  Transac:  " + STRING(Reclamos_Tarj.Hora_Transac) AT 1 FORMAT "X(80)"
             "Operacion    :  " + TRIM(W-MTran) + " " + TRIM(W-mRed) AT 1 FORMAT "X(80)"
             "Consecutivo  :  " + STRING(Reclamos_tarj.Consecutivo,"99999999") AT 1 FORMAT "X(80)"
             " "                AT 1 FORMAT "X(2)"
             " "                AT 1 FORMAT "X(2)"
             "Reporta      :  " + w-nomus                            AT 1 FORMAT "X(80)"
             " "                AT 1 FORMAT "X(2)"
             "Comentario   :  " AT 1 FORMAT "X(80)"
             WITH FRAME F-mov2 USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO WIDTH 132 NO-LABELS.
      DISPLAY Reclamos_Tarj.descripcion VIEW-AS EDITOR SIZE 105 BY 3 AT 3
             WITH FRAME fmov3 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

      PUT " " SKIP(2).
      PAGE.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

