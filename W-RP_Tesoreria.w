&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
 {Incluido/Variable.I "SHARED"}
  {Incluido/VARCON.I "SHARED"}

  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR i AS INTEGER.
  DEFINE VAR j AS DECIMAL.
  DEFINE VAR k AS INTEGER.
  
  DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
  DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
  DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
  DEFINE VARIABLE WFec     AS DATE.   
  DEFINE VARIABLE FecIni   AS DATE.
  DEFINE VARIABLE FecFin   AS DATE.
  DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
  DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".
  DEFINE VARIABLE EstadoC  AS INTEGER FORMAT "9".
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  
  DEFINE TEMP-TABLE Tot
    FIELD Ord AS INTEGER FORMAT "9"
    FIELD Ope AS INTEGER FORMAT "9"
    FIELD Age LIKE Agencias.Agencia
    FIELD Val AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".

 /* DEFINE TEMP-TABLE Tmp  /*para guardar Disponible de Agencias*/
    FIELD TFec AS DATE
    FIELD TAge LIKE Agencias.Agencia
    FIELD TNAg LIKE Agencias.Nombre
    FIELD TVal AS CHARACTER FORMAT "X(15)" EXTENT 25.*/

  DEFINE TEMP-TABLE Tmp  /*para guardar Disponible de Agencias*/
    FIELD TFec AS DATE
    FIELD TAge LIKE Agencias.Agencia
    FIELD TNAg LIKE Agencias.Nombre
    FIELD TCod LIKE Cuentas.Cuenta
    FIELD TNCo LIKE Cuentas.Nombre
    FIELD TBan AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD TVal AS DECIMAL FORMAT "->>,>>>,>>>,>>9".

  DEFINE TEMP-TABLE Tmi
    FIELD iFec AS DATE
    FIELD iCod LIKE Cuentas.Cuenta
    FIELD iNCo LIKE Cuentas.Nombre
    FIELD iDis AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD iBan AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD iVar AS DECIMAL FORMAT "->>,>>>,>>>,>>9"
    FIELD iVal AS DECIMAL FORMAT "->>,>>>,>>>,>>9" EXTENT 11.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME FTes

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-153 BUTTON-155 BUTTON-121 BUTTON-120 ~
Btn_Ejecutar T3 T1 Rt1 Rt3 T2 T4 Rt2 Rt4 BUTTON-149 T5 T6 Tg_SdoCaja E1 ~
BtnDone BUTTON-158 Img_MesF Img_MesI RECT-282 RECT-283 RECT-284 RECT-285 ~
RECT-287 RECT-288 RECT-289 RECT-290 RECT-293 RECT-294 
&Scoped-Define DISPLAYED-OBJECTS W_DiaIni AnoIni W_DiaFin AnoFin T3 T1 Rt1 ~
Rt3 T2 T4 Rt2 Rt4 T5 T6 Tg_SdoCaja E1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 11 BY 1.65
     BGCOLOR 8 .

DEFINE BUTTON Btn_Ejecutar 
     LABEL "Ejecutar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 4 BY .54.

DEFINE BUTTON BUTTON-149 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 153" 
     SIZE 7 BY 1.65.

DEFINE BUTTON BUTTON-155 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 155" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-158 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 158" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE E1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 97 BY 16.15
     BGCOLOR 15 FONT 2 NO-UNDO.

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

DEFINE VARIABLE Rt1 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "+", 1,
"-", 2
     SIZE 9 BY .54
     FONT 1 NO-UNDO.

DEFINE VARIABLE Rt2 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "+", 1,
"-", 2
     SIZE 9 BY .54
     FONT 1 NO-UNDO.

DEFINE VARIABLE Rt3 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "+", 1,
"-", 2
     SIZE 7 BY .54
     FONT 1 NO-UNDO.

DEFINE VARIABLE Rt4 AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "+", 1,
"-", 2
     SIZE 7 BY .54
     FONT 1 NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26 BY 1.35.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27 BY 1.35.

DEFINE RECTANGLE RECT-284
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY 1.08.

DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 11 BY 1.08.

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY .92.

DEFINE RECTANGLE RECT-288
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 11 BY .92.

DEFINE RECTANGLE RECT-289
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9 BY 1.04.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 9 BY .92.

DEFINE RECTANGLE RECT-293
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 40 BY 1.08.

DEFINE RECTANGLE RECT-294
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 38 BY .92.

DEFINE VARIABLE T1 AS LOGICAL INITIAL no 
     LABEL "Informe de Disponible de Agencias" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .77 NO-UNDO.

DEFINE VARIABLE T2 AS LOGICAL INITIAL no 
     LABEL "Vencimientos de Inversiones" 
     VIEW-AS TOGGLE-BOX
     SIZE 33 BY .69 NO-UNDO.

DEFINE VARIABLE T3 AS LOGICAL INITIAL no 
     LABEL "Vencimiento de Títulos de Clientes" 
     VIEW-AS TOGGLE-BOX
     SIZE 36 BY .77 NO-UNDO.

DEFINE VARIABLE T4 AS LOGICAL INITIAL no 
     LABEL "Créditos Aprobados sin Desembolsar" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .62 NO-UNDO.

DEFINE VARIABLE T5 AS LOGICAL INITIAL no 
     LABEL "Créditos Desembolsados" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.43 BY .69 NO-UNDO.

DEFINE VARIABLE T6 AS LOGICAL INITIAL no 
     LABEL "Informe Disponible + entradas - salidas" 
     VIEW-AS TOGGLE-BOX
     SIZE 41 BY .77
     FGCOLOR 7 FONT 5 NO-UNDO.

DEFINE VARIABLE Tg_SdoCaja AS LOGICAL INITIAL no 
     LABEL "Saldos Caja X Agencia" 
     VIEW-AS TOGGLE-BOX
     SIZE 34.43 BY .62 NO-UNDO.

DEFINE BUTTON Btn_anterior 
     LABEL "Anterior" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_siguiente 
     LABEL "Siguiente" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-154 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 154" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(20)":U 
     LABEL "Buscar" 
     VIEW-AS FILL-IN 
     SIZE 33 BY .81
     BGCOLOR 15 FONT 0 NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "imagenes/clock05.ico":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R5
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R6
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R7
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R8
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .

DEFINE RECTANGLE R9
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 7 BY .54
     BGCOLOR 15 .


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME FTes
     BUTTON-153 AT ROW 1.27 COL 3
     BUTTON-155 AT ROW 1.27 COL 102
     BUTTON-121 AT ROW 1.77 COL 73
     W_DiaIni AT ROW 1.92 COL 24 COLON-ALIGNED NO-LABEL
     AnoIni AT ROW 1.92 COL 39 COLON-ALIGNED NO-LABEL
     BUTTON-120 AT ROW 1.92 COL 47
     W_DiaFin AT ROW 1.92 COL 52 NO-LABEL
     AnoFin AT ROW 1.92 COL 65 COLON-ALIGNED NO-LABEL
     Btn_Ejecutar AT ROW 3.15 COL 102
     T3 AT ROW 3.31 COL 52
     T1 AT ROW 3.35 COL 3
     Rt1 AT ROW 3.42 COL 41 NO-LABEL
     Rt3 AT ROW 3.42 COL 92 NO-LABEL
     T2 AT ROW 4.38 COL 3
     T4 AT ROW 4.38 COL 52
     Rt2 AT ROW 4.5 COL 41 NO-LABEL
     Rt4 AT ROW 4.5 COL 92 NO-LABEL
     BUTTON-149 AT ROW 4.77 COL 102
     T5 AT ROW 5.19 COL 52
     T6 AT ROW 5.58 COL 3
     Tg_SdoCaja AT ROW 5.96 COL 52
     E1 AT ROW 6.65 COL 3 NO-LABEL
     BtnDone AT ROW 20.12 COL 101
     BUTTON-158 AT ROW 22 COL 105
     Img_MesF AT ROW 1.92 COL 56
     Img_MesI AT ROW 1.92 COL 30
     RECT-282 AT ROW 1.54 COL 25
     RECT-283 AT ROW 1.54 COL 51
     RECT-284 AT ROW 3.15 COL 2
     RECT-285 AT ROW 3.15 COL 40
     RECT-287 AT ROW 4.23 COL 51
     RECT-288 AT ROW 4.23 COL 40
     RECT-289 AT ROW 3.15 COL 91
     RECT-290 AT ROW 4.23 COL 91
     RECT-293 AT ROW 3.15 COL 51
     RECT-294 AT ROW 4.23 COL 2
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 13 BY .69 AT ROW 1.27 COL 26
          BGCOLOR 17 FGCOLOR 7 
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.27 COL 52
          BGCOLOR 17 FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.43 BY 22.31
         BGCOLOR 17 FONT 8
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F_Progreso
     IMAGE-2 AT ROW 1.27 COL 4.57
     R1 AT ROW 6.92 COL 3.57
     R2 AT ROW 6.38 COL 3.57
     R3 AT ROW 5.85 COL 3.57
     R4 AT ROW 5.31 COL 3.57
     R5 AT ROW 4.77 COL 3.57
     R6 AT ROW 4.23 COL 3.57
     R7 AT ROW 3.69 COL 3.57
     R8 AT ROW 3.15 COL 3.57
     R9 AT ROW 2.62 COL 3.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 101 ROW 6.65
         SIZE 12 BY 7.54
         BGCOLOR 17 .

DEFINE FRAME F_Buscar
     Buscar AT ROW 1.27 COL 7 COLON-ALIGNED
     BUTTON-154 AT ROW 1.54 COL 45
     Btn_anterior AT ROW 2.35 COL 9
     Btn_siguiente AT ROW 2.35 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 45 ROW 6.65
         SIZE 53 BY 3.5
         BGCOLOR 17 FONT 4
         TITLE "Buscar".


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
         TITLE              = "SFG - Informes de Tesorería"
         HEIGHT             = 22.31
         WIDTH              = 113.43
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
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
ASSIGN FRAME F_Buscar:FRAME = FRAME FTes:HANDLE
       FRAME F_Progreso:FRAME = FRAME FTes:HANDLE.

/* SETTINGS FOR FRAME FTes
                                                                        */
/* SETTINGS FOR FILL-IN AnoFin IN FRAME FTes
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME FTes
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DiaFin IN FRAME FTes
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_DiaIni IN FRAME FTes
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Buscar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Buscar:HIDDEN           = TRUE
       FRAME F_Buscar:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Informes de Tesorería */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Informes de Tesorería */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME FTes /* Salir */
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


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME Btn_anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_anterior wWin
ON CHOOSE OF Btn_anterior IN FRAME F_Buscar /* Anterior */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,34) IN FRAME Ftes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FTes
&Scoped-define SELF-NAME Btn_Ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ejecutar wWin
ON CHOOSE OF Btn_Ejecutar IN FRAME FTes /* Ejecutar */
DO:
  ASSIGN FRAME FTes T1 T2 T3 T4 Rt1 Rt2 Rt3 Rt4 T5 T6.
  Listado = /*W_PathSpl +*/ "c:\info\TotAgencia.LST".
  OS-DELETE VALUE(Listado).
  FOR EACH Tmp: DELETE Tmp. END.
  FOR EACH Tot: DELETE Tot. END.
  /*OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.*/
  IF Tg_SdoCaja THEN
     RUN Rp_SdoCaja. /*<---Junio 17/05 GAER*/

  IF T1 THEN RUN Rp_Disponible.
  IF T2 THEN RUN Rp_VenInversiones.
  IF T3 THEN RUN Rp_Vencimientos.
  IF T4 THEN RUN Rp_Aprobados.
  IF T5 THEN RUN Rp_Desembolsados. 
  IF T6 THEN RUN Rp_Totales.
  W_ok = e1:READ-FILE(Listado) IN FRAME FTes.
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME Btn_siguiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_siguiente wWin
ON CHOOSE OF Btn_siguiente IN FRAME F_Buscar /* Siguiente */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,33) IN FRAME Ftes.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FTes
&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 wWin
ON CHOOSE OF BUTTON-120 IN FRAME FTes /* Button 120 */
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


&Scoped-define SELF-NAME BUTTON-121
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-121 wWin
ON CHOOSE OF BUTTON-121 IN FRAME FTes /* Button 121 */
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


&Scoped-define SELF-NAME BUTTON-149
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-149 wWin
ON CHOOSE OF BUTTON-149 IN FRAME FTes /* Button 149 */
DO:
  IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
    DEFINE VAR W_sw          AS LOGICAL. 
    APPLY "choose" TO Btn_Ejecutar.
    RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado,INPUT-OUTPUT W_Dispositivo).
    IF W_Dispositivo = "" THEN
      RETURN.
    IF W_Dispositivo = "P" THEN  
      RUN Pantalla IN W_Manija (INPUT Listado).
    ELSE                                                  
      IF W_Dispositivo = "I" THEN
          RUN adecomm/_osprint.r ( INPUT  ?, INPUT Listado,INPUT  ?,INPUT  1,INPUT  1,
                                          INPUT  99999,OUTPUT W_sw).
    IF W_Dispositivo = "E" THEN
      RUN Imprimir_Excel.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-153
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON CHOOSE OF BUTTON-153 IN FRAME FTes /* Button 153 */
DO:
  IF E1:SCREEN-VALUE EQ "" THEN
     MESSAGE "No se ha ejecutado ningún Informe" SKIP
             "Escoja el informe y presione clic" SKIP
             "en el boton ejecutar!." VIEW-AS ALERT-BOX INFORMATION.
  ELSE DO:
    Buscar:SCREEN-VALUE IN FRAME F_Buscar = E1:SELECTION-TEXT.
    ASSIGN FRAME F_Buscar Buscar.
    W_Ok = E1:SEARCH(Buscar,32).
    VIEW FRAME F_Buscar.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME BUTTON-154
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-154 wWin
ON CHOOSE OF BUTTON-154 IN FRAME F_Buscar /* Button 154 */
DO:
  HIDE FRAME F_Buscar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FTes
&Scoped-define SELF-NAME BUTTON-155
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-155 wWin
ON CHOOSE OF BUTTON-155 IN FRAME FTes /* Button 155 */
DO:
  RUN W-InfDia.R NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Tg_SdoCaja
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tg_SdoCaja wWin
ON VALUE-CHANGED OF Tg_SdoCaja IN FRAME FTes /* Saldos Caja X Agencia */
DO:
  ASSIGN Tg_SdoCaja.
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
  DISPLAY W_DiaIni AnoIni W_DiaFin AnoFin T3 T1 Rt1 Rt3 T2 T4 Rt2 Rt4 T5 T6 
          Tg_SdoCaja E1 
      WITH FRAME FTes IN WINDOW wWin.
  ENABLE BUTTON-153 BUTTON-155 BUTTON-121 BUTTON-120 Btn_Ejecutar T3 T1 Rt1 Rt3 
         T2 T4 Rt2 Rt4 BUTTON-149 T5 T6 Tg_SdoCaja E1 BtnDone BUTTON-158 
         Img_MesF Img_MesI RECT-282 RECT-283 RECT-284 RECT-285 RECT-287 
         RECT-288 RECT-289 RECT-290 RECT-293 RECT-294 
      WITH FRAME FTes IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FTes}
  DISPLAY Buscar 
      WITH FRAME F_Buscar IN WINDOW wWin.
  ENABLE Buscar BUTTON-154 Btn_anterior Btn_siguiente 
      WITH FRAME F_Buscar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Buscar}
  ENABLE IMAGE-2 R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HallarSdo wWin 
PROCEDURE HallarSdo :
DEFINE INPUT  PARAMETER Smes AS INTEGER.
    DEFINE OUTPUT PARAMETER Sini LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE OUTPUT PARAMETER SFin LIKE Sal_Cuenta.Sal_Inicial.
    DEFINE VAR i AS INTEGER.
    SFin = Sal_Cuenta.Sal_Inicial.
    DO i = 1 TO Smes BY 1:
        IF Cuentas.Naturaleza EQ "DB" THEN
           ASSIGN SFin  = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i]
                  SIni  = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i].
        ELSE
           ASSIGN SFin  = SFin - Sal_Cuenta.DB[i] + Sal_Cuenta.Cr[i]
                  SIni  = SFin + Sal_Cuenta.DB[i] - Sal_Cuenta.Cr[i].
    END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  DO WITH FRAME FTes:
    ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(MONTH(W_Fecha)) + ".gif")
           W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(W_Fecha)) + ".gif")
           AnoIni:SCREEN-VALUE = STRING(YEAR(W_Fecha))
           AnoFin:SCREEN-VALUE = STRING(YEAR(W_Fecha))
           MesIni = MONTH(W_Fecha)
           MesFin = MONTH(W_Fecha)
           FecIni = W_Fecha - DAY(W_Fecha) + 1
           FecFin = W_Fecha
           W_DiaFin:SCREEN-VALUE = STRING(DAY(W_Fecha)).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Progreso wWin 
PROCEDURE Progreso :
DO WITH FRAME F_Progreso:
  IF j EQ 50 THEN DO:
        j = 0.
        k = k + 1.
        ASSIGN R1:BGCOLOR IN FRAME F_Progreso = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rp_Aprobados wWin 
PROCEDURE Rp_Aprobados :
DEFINE VAR GPValor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR GPNumero AS DECIMAL FORMAT ">,>>9".
DEFINE VAR GRValor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR GRNumero AS DECIMAL FORMAT ">,>>9".
DEFINE VAR TotalAgencia AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TGPValor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TGPNumero AS DECIMAL FORMAT ">,>>9".
DEFINE VAR TGRValor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TGRNumero AS DECIMAL FORMAT ">,>>9".
DEFINE VAR NomAge AS CHARACTER FORMAT "X(40)".

Listado = "c:\info\TotAgencia.LST".
J = 0.
k = 0.

{Incluido\RepEncabezado.i}

W_Reporte = "REPORTE   : CREDITOS APROBADOS NO DESEMBOLSADOS" + " - ENTRE: " + STRING(FecIni) + " Y " + STRING(FecFin) + " Hora: " + STRING(TIME,"hh:mm am").
W_EncColumna = "AGENCIA                CREDITOS GARANTIA PERSONAL                       CREDITOS GARANTIA REAL                  TOTAL".

OUTPUT TO VALUE(Listado) PAGED PAGE-SIZE 65 APPEND.
VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.

FOR EACH Creditos WHERE Creditos.Estado = 1
                    AND Creditos.Fec_Desembolso = ? NO-LOCK BREAK BY Creditos.Agencia:
    j = j + 1.

    /* oakley */

      RUN Progreso.
      FIND FIRST Garantia WHERE 
                 Garantias.Tip_Credito   EQ Creditos.Tip_Credito   AND
                 Garantias.Cod_Credito   EQ Creditos.Cod_Credito   AND
                 Garantias.Num_Solicitud EQ Creditos.Num_Solicitud AND
                 Garantias.Estado        EQ 1 NO-LOCK NO-ERROR.
      IF AVAILABLE Garantias THEN
         ASSIGN GRValor   = GRValor  + Creditos.Monto
                GRNumero  = GRNumero + 1
                TGRValor  = TGRValor + Creditos.Monto
                TGRNumero = TGRNumero + 1.
      ELSE
         ASSIGN GPValor   = GPValor  + Creditos.Monto
                GPNumero  = GPNumero + 1
                TGPValor  = TGPValor + Creditos.Monto
                TGPNumero = TGPNumero + 1.
      IF LAST-OF(Creditos.Agencia) THEN DO:
        FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
        NomAge = "Agencia no Existe".
        IF AVAILABLE Agencias THEN
          NomAge = STRING(Agencias.Nombre,"X(25)") + "... $".
          TotalAgencia = GRValor + GPValor.
          DISPLAY NomAge       AT 1  FORMAT "X(30)"
                  GPValor      AT 35 
                  GPNumero     AT 53
                  GRValor      AT 71
                  GRNumero     AT 89
                  TotalAgencia AT 107
          WITH FRAME FCD2 WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
          ASSIGN TGPValor = TGPValor + GPValor
                 TGRValor = TGRValor + GRValor
                 TGPNumero = TGPNumero + GPNumero
                 TGRNumero = TGRNumero + GRNumero.
          CREATE Tot.
          ASSIGN Tot.Ord = 4
                 Tot.Ope = Rt4
                 Tot.Age = Creditos.Agencia
                 Tot.Val = TotalAgencia.
          ASSIGN GPValor = 0 GRValor = 0 GPNumero = 0 GRNumero = 0 TotalAgencia = 0.
      END.
  END.
  TotalAgencia = TGPValor + TGRValor.
  DISPLAY SKIP(1)
    "TOTAL" AT 1
    TGPValor      AT 35
    TGPNumero     AT 53
    TGRValor      AT 71
    TGRNumero     AT 89
    TotalAgencia AT 107
  WITH FRAME FTotales2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rp_Desembolsados wWin 
PROCEDURE Rp_Desembolsados :
DEFINE VAR Valor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR Numero AS DECIMAL FORMAT ">,>>9".
DEFINE VAR Promedio AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TValor AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TNumero AS DECIMAL FORMAT ">,>>9".
DEFINE VAR TPromedio AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR NomAge AS CHARACTER FORMAT "X(40)".

Listado = "c:\info\TotAgencia.LST".
J = 0.
k = 0.

{Incluido\RepEncabezado.i}

W_Reporte = "REPORTE   : CREDITOS DESEMBOLSADOS" + " - ENTRE: " + STRING(FecIni) + " Y " + STRING(FecFin) + " Hora: " + STRING(TIME,"hh:mm am").
W_EncColumna = "AGENCIA                                      VALOR CREDITOS       NUMERO               TOTAL".

OUTPUT TO VALUE(Listado) PAGED PAGE-SIZE 65 APPEND.
VIEW FRAME F-Encabezado.
VIEW FRAME F-Ftr.

FOR EACH Creditos WHERE Creditos.Fec_Desembolso >= FecIni
                    AND Creditos.Fec_Desembolso <= FecFin NO-LOCK BREAK BY Creditos.Agencia:
    /* oakley */

      j = j + 1.
      RUN Progreso.
      ASSIGN Valor   = Valor  + Monto
             Numero  = Numero + 1
             TValor  = TValor + Monto
             TNumero = TNumero + 1.
      IF LAST-OF(Creditos.Agencia) THEN DO:
        FIND Agencias WHERE Agencias.Agencia EQ Creditos.Agencia NO-LOCK NO-ERROR.
        NomAge = "Agencia no Existe".
        IF AVAILABLE Agencias THEN
          NomAge = STRING(Agencias.Nombre,"X(35)") + "... $".
          Promedio = Valor / Numero.
          DISPLAY NomAge     AT 1 
                  Valor      AT 45
                  Numero     AT 65
                  Promedio   AT 80
          WITH FRAME FCD1 WIDTH 300 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
          ASSIGN Valor = 0 Numero = 0 Promedio = 0.
      END.
  END.
  TPromedio = TValor / TNumero.
  DISPLAY SKIP(1)
          "TOTAL GENERAL DE LA ENTIDAD" AT 1
          TValor    AT 45
          TNumero   AT 65
          TPromedio AT 80
  WITH FRAME FTotales WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rp_Disponible wWin 
PROCEDURE Rp_Disponible :
DEFINE VAR TotAge AS DECIMAL FORMAT "->>,>>>,>>>,>>9" EXTENT 10.
DEFINE VAR TotDis AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR TotBan AS DECIMAL FORMAT "->>,>>>,>>>,>>9".

DEFINE VAR Encabezado AS CHARACTER FORMAT "X(300)".
DEFINE VAR UltFec     AS DATE.
DEFINE VAR Valor AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
VIEW FRAME F_Progreso.
I = 3.
FOR EACH tmi: DELETE tmi. END. FOR EACH tmp: DELETE tmp. END.
FOR EACH Cuentas 
   FIELDS(Cuentas.Cuenta Cuentas.Nombre Cuentas.Cod_FlujoEfec Cuentas.Car_Efectivo Cuentas.Estado) 
   WHERE Cuentas.Cod_FlujoEfec EQ "D" AND
         Cuentas.Car_Efectivo  NE 1   AND  /*1.ninguno/2.caja/3.bancos*/
         Cuentas.Id_Total      EQ YES AND
         Cuentas.Estado        EQ 1 NO-LOCK BY Cuentas.Cuenta:
         Encabezado = Encabezado + " " + STRING(Cuentas.Nombre,"X(15)") + " ".
   FOR EACH TOTAL_Agencia WHERE
            TOTAL_Agencia.Tipo_Producto EQ 3              AND
            TOTAL_Agencia.Codigo        EQ Cuentas.Cuenta AND
            TOTAL_Agencia.Fecha         GE FecIni         AND
            TOTAL_Agencia.Fecha         LE FecFin NO-LOCK
            BREAK BY TOTAL_Agencia.Fecha BY TOTAL_Agencia.Agencia BY TOTAL_Agencia.Codigo:
       j = j + 1.
       RUN Progreso.
       FIND Tmp WHERE Tmp.TFec EQ TOTAL_Agencia.Fecha   AND
                      Tmp.TAge EQ TOTAL_Agencia.Agencia AND
                      Tmp.TCod EQ TOTAL_Agencia.Codigo NO-ERROR.
       IF NOT AVAILABLE Tmp THEN DO:
          FIND Agencias WHERE Agencias.Agencia EQ TOTAL_Agencia.Agencia NO-LOCK NO-ERROR.
          CREATE Tmp.
          ASSIGN Tmp.TFec = TOTAL_Agencia.Fecha
                 Tmp.TAge = TOTAL_Agencia.Agencia
                 Tmp.TNAg = Agencias.Nombre WHEN AVAILABLE Agencias
                 Tmp.TCod = Cuentas.Cuenta
                 Tmp.TNco = Cuentas.Nombre.
          ASSIGN Tmp.TVal = Tmp.TVal + TOTAL_Agencia.Sdo_Dia.
          IF Cuentas.Car_Efectivo EQ 3 THEN
             Tmp.TBan = Tmp.TBan + TOTAL_Agencia.Sdo_Dia.
       END.
   END.
END.

FOR EACH Tmp BREAK BY Tmp.TFec:
    j = j + 1.
    RUN Progreso.
    FIND Tmi WHERE Tmi.iFec EQ Tmp.TFec AND
                   Tmi.iCod EQ Tmp.TCod NO-ERROR.
    IF NOT AVAILABLE Tmi THEN DO:
       CREATE Tmi.
       ASSIGN Tmi.iFec = Tmp.TFec
              Tmi.iNco = Tmp.TNco
              Tmi.iCod = Tmp.TCod.
    END.
    ASSIGN Tmi.iDis = Tmi.IDis + Tmp.TVal
           Tmi.iVal[Tmp.TAge] = Tmi.iVal[Tmp.TAge] + Tmp.TVal
           Tmi.iBan = Tmi.iBan + Tmp.TBan.
END.



Listado = /*W_PathSpl +*/ "c:\info\TotAgencia.LST".
OS-DELETE VALUE(Listado).
ASSIGN J = 0 k = 0.
OUTPUT TO value(Listado) PAGED PAGE-SIZE 65 APPEND .
  DISPLAY "COOPERATIVA FINANCIERA BELEN" AT 1 SKIP(1)
          "INFORME DEL DISPONIBLE"       AT 1 SKIP(1)
          "Entre: " FecIni " y " FecFin
          WITH FRAME Ftit WIDTH 132 USE-TEXT NO-BOX.
  FOR EACH Tmi NO-LOCK
      BREAK BY Tmi.iFec BY Tmi.iCod:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Tmi.iFec) THEN DO:
          ASSIGN TotBan = 0 TotDis = 0.
          DO I = 1 TO 10 BY 1: TotAge[i] = 0. END.
          DISPLAY 
           "Fecha : " Tmi.iFec SKIP 
           "CUENTA         NOMBRE CUENTA                  DISPONIBLE          BANCOS           BELEN          CENTRO        SABANETA          CALDAS        ENVIGADO          ITAGUI        FLORESTA        RIONEGRO       ALTAVISTA  ADMINISTRATIVA"
           WITH FRAME TFec WIDTH 300 NO-LABELS.
      END.
      DISPLAY Tmi.iCod Tmi.iNco FORMAT "X(25)" Tmi.iDis Tmi.iBan Tmi.iVal
          WITH FRAME Fmov WIDTH 300 NO-BOX USE-TEXT STREAM-IO NO-LABELS.
      ASSIGN TotDis = TotDis + Tmi.iDis
             TotBan = TotBan + Tmi.iBan.
      DO I = 1 TO 10 BY 1: TotAge[i] = TotAge[i] + Tmi.iVal[i]. END.
      IF LAST-OF(Tmi.iFec) THEN DO:
         DISPLAY SKIP(1) "Totales de la fecha                     " TotDis TotBan TotAge
         WITH FRAME Ftot WIDTH 300 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
      END.
  END.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rp_SdoCaja wWin 
PROCEDURE Rp_SdoCaja :
/*------------------------------------------------------------------------------
  Purpose:     Informe Sdos-Caja x Agencia Vs. Vr.Máximo configurado.
  Notes:       Junio 17/05 GAER.
------------------------------------------------------------------------------*/
DEFINE VAR TotAge AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR SIni   AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFINE VAR SFin   AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
DEFI   VAR Cont   AS INTEG   FORMAT "99".

DEFINE VAR UltFec     AS DATE.
DEFINE VAR Valor AS DECIMAL FORMAT "->>,>>>,>>>,>>9".

FOR EACH tmp: DELETE tmp. END.

/*Listado = /*W_PathSpl +*/ "c:\info\TotAgCaja.LST".*/
ASSIGN J = 0 k = 0.
{Incluido\RepEncabezado.i}

  W_Reporte   = "REPORTE   : INFORME SALDOS DE CAJA"
              + " - FECHA: " + STRING(W_Fecha) + " Hora: " + STRING(TIME,"hh:mm am").
  W_EncColumna = "".

OUTPUT TO VALUE(Listado) PAGED PAGE-SIZE 65 APPEND.
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.

FIND FIRST Cuentas WHERE Cuentas.Cod_FlujoEfec EQ "D"
                   AND   Cuentas.Car_Efectivo  EQ  2
                   AND   Cuentas.Estado        EQ  1 NO-LOCK NO-ERROR.

ASSIGN W_EncColumna = W_EncColumna + " " + STRING(Cuentas.Nombre,"X(35)") 
       I          = 3.

FOR EACH Agencias WHERE Agencias.Estado NE 3 NO-LOCK:
    TotAge = 0.

    FIND FIRST Sal_Cuenta WHERE Sal_Cuenta.Agencia EQ Agencias.Agencia
                            AND Sal_Cuenta.Cuenta  EQ Cuentas.Cuenta
                            AND Sal_Cuenta.Ano     EQ YEAR(W_Fecha) NO-LOCK NO-ERROR.
    IF AVAIL(Sal_Cuenta) THEN DO:
       RUN HallarSdo (INPUT MONTH(W_Fecha), OUTPUT SIni, OUTPUT SFin).
       TotAge = SFin.

/*       DO Cont = 1 TO MONTH(W_Fecha) BY 1:
          ASSIGN TotAge = TotAge + Sal_Cuenta.Db[Cont] - Sal_Cuenta.Cr[Cont].
       END.*/
    END.

    j = j + 1.
    RUN Progreso.

    CREATE Tmp.
    ASSIGN Tmp.TAge = Agencias.Agencia                        
           Tmp.TNAg = Agencias.Nombre    
           Tmp.TCod = Cuentas.Cuenta                               
           Tmp.TNco = Cuentas.Nombre.                              
    ASSIGN Tmp.TVal = SFin
           Tmp.TBan = Agencias.ValMax_Caja.
END.

FOR EACH Tmp WHERE Tmp.TVal GT 0 BY Tmp.TAge:
    DISPLAY Tmp.TAge   LABEL "Ag."
            Tmp.TNAg   LABEL "Nombre de la Agencia"
            Tmp.TVal   LABEL "Valores en Cajas"
            Tmp.TBan   LABEL "Vr.Máximo Permitido"
            (Tmp.TVal - Tmp.TBan) LABEL "Diferencia" FORM "->>>>,>>>,>>>,>>9"
        WITH DOWN WIDTH 150 FRAME FTOTAGE_1 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rp_Totales wWin 
PROCEDURE Rp_Totales :
DEFINE VAR TotDisp AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
DEFINE VAR Linea   AS CHARACTER FORMAT "X(132)".
DEFINE VAR Signo   AS CHARACTER FORMAT "X".
DEFINE VAR ValImp  AS CHARACTER FORMAT "X(16)".
DEFINE VAR Enc     AS CHARACTER FORMAT "X(132)".

Listado = /*W_PathSpl +*/ "c:\info\TotAgencia.LST".
/*OS-DELETE VALUE(Listado).*/
ASSIGN J = 0 k = 0.
{Incluido\RepEncabezado.i}

  W_Reporte   = "REPORTE   : TOTAL DISPONIBLE POR AGENCIA"
              + " - ENTRE: " + STRING(FecIni) + " Y " + STRING(FecFin) + " Hora: " + STRING(TIME,"hh:mm am").
OUTPUT TO VALUE(Listado) PAGED PAGE-SIZE 65 APPEND.
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
FOR EACH Tot BREAK BY Tot.Age BY Tot.Ord:
   IF FIRST-OF(Tot.Age) THEN DO:
      FIND Agencias WHERE Agencias.Agencia EQ Tot.Age NO-LOCK NO-ERROR.
      IF AVAILABLE Agencias THEN
         Linea = STRING(Agencias.Agencia,"999") + " " + STRING(Agencias.Nombre,"X(20)") + "  : ".
   END.
   CASE Tot.Ord:
    WHEN 1 THEN Enc = Enc + "   DISPONIBLE   ".
    WHEN 2 THEN Enc = Enc + "    " + "VENCIM.INVERSION".
    WHEN 3 THEN Enc = Enc + "    " + "VENCI.DE.TITULOS".
    WHEN 4 THEN Enc = Enc + "    " + "CREDIT.APROBADOS".
   END CASE.
   IF Tot.Ope EQ 1 THEN 
      ASSIGN TotDisp = TotDisp + Tot.Val
             Signo   = "+".
   ELSE 
      ASSIGN TotDisp = TotDisp - Tot.Val
             Signo   = "-".
/*   ValImp = RIGHT-TRIM(STRING(Tot.Val,"->>>,>>>,>>>,>>9")).*/
   Linea = Linea + "  " +  Signo + " " + RIGHT-TRIM(STRING(Tot.Val,"->>,>>>,>>>,>>9")).
   IF LAST-OF(Tot.Age) THEN DO:
      Linea = Linea + "    =  " + RIGHT-TRIM(STRING(TotDisp,"->>>,>>>,>>>,>>9")).
      DISPLAY SKIP(1)
          Enc   AT 33 FORMAT "X(90)" SKIP(1)
          Linea AT 1 WITH FRAME FLin WIDTH 132 NO-LABELS USE-TEXT NO-BOX.
      ASSIGN TotDisp = 0 Enc = "".
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rp_Vencimientos wWin 
PROCEDURE Rp_Vencimientos :
DEFINE VAR Valor    AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR Numero   AS DECIMAL FORMAT ">,>>9".
DEFINE VAR Promedio AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TValor    AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TNumero   AS DECIMAL FORMAT ">,>>9".
DEFINE VAR TPromedio AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR NomAge    AS CHARACTER FORMAT "X(40)".

DEFINE VAR ValFecha AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR NumFecha AS DECIMAL FORMAT ">,>>9".


Listado = /*W_PathSpl +*/ "c:\info\TotAgencia.LST".
/*OS-DELETE VALUE(Listado).*/
ASSIGN J = 0 k = 0.
{Incluido\RepEncabezado.i}

  W_Reporte   = "REPORTE   : VENCIMIENTO DE TITULOS"
              + " - ENTRE: " + STRING(FecIni) + " Y " + STRING(FecFin) + " Hora: " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGENICA                    FECHA                 VALOR            NUMERO".
OUTPUT TO VALUE(Listado) PAGED PAGE-SIZE 65 APPEND.
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH Ahorros WHERE
           Ahorros.Tip_Ahorro      GE 2      AND
           Ahorros.Tip_Ahorro      LE 3      AND
           Ahorros.Estado          EQ 1      AND
           Ahorros.Fec_Vencimiento NE ?      AND
           Ahorros.Fec_Vencimiento GE FecIni AND
           Ahorros.Fec_Vencimiento LE FecFin NO-LOCK
           BREAK BY Ahorros.Fec_Vencimiento BY Ahorros.Agencia:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Ahorros.Fec_Vencimiento) THEN DO:
         DISPLAY SKIP(1) 
                "FECHA DE VENCIMIENTO: " AT 1
                 Ahorros.Fec_Vencimiento AT 26
         WITH FRAME FFecVen WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
      END.
      ASSIGN Valor   = Valor  + Ahorros.Sdo_Disponible
             Numero  = Numero + 1
             TValor  = TValor + Ahorros.Sdo_Disponible
             TNumero = TNumero + 1.
      IF LAST-OF(Ahorros.Agencia) THEN DO:
        FIND Agencias WHERE Agencias.Agencia EQ Ahorros.Agencia NO-LOCK NO-ERROR.
        NomAge = "Agencia no Existe".
        IF AVAILABLE Agencias THEN
          NomAge = STRING(Agencias.Nombre,"X(35)") + "... $".
          DISPLAY NomAge     AT 1 
                  Valor      AT 45
                  Numero     AT 65
          WITH FRAME FCD1 WIDTH 300 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
          ASSIGN ValFecha = ValFecha + Valor
                 NumFecha = NumFecha + Numero.
          FIND Tot WHERE Tot.Age = Ahorros.Agencia AND
                         Tot.Ord = 3 NO-ERROR.
          IF NOT AVAILABLE Tot THEN DO:
             CREATE Tot.
             ASSIGN Tot.Ord = 3
                    Tot.Ope = Rt3
                    Tot.Age = Ahorros.Agencia.
          END.
          Tot.Val = Tot.Val + Valor.
          ASSIGN Valor = 0 Numero = 0 Promedio = 0.
      END.
      IF LAST-OF(Ahorros.Fec_Vencimiento) THEN DO:
         DISPLAY SKIP(1) 
                "TOTAL DE LA FECHA   : " AT 1
                 ValFecha                AT 45
                 NumFecha                AT 65
         WITH FRAME FTotFec WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
         ASSIGN ValFecha = 0 NumFecha = 0.
      END.
  END.
  DISPLAY SKIP(1)
          "TOTAL GENERAL DE LA ENTIDAD" AT 1
          TValor    AT 45
          TNumero   AT 65
  WITH FRAME FTotales WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Rp_VenInversiones wWin 
PROCEDURE Rp_VenInversiones :
DEFINE VAR Valor    AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR Numero   AS DECIMAL FORMAT ">,>>9".
DEFINE VAR Promedio AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TValor    AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TNumero   AS DECIMAL FORMAT ">,>>9".
DEFINE VAR TPromedio AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR NomAge    AS CHARACTER FORMAT "X(40)".

DEFINE VAR ValFecha AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR NumFecha AS DECIMAL FORMAT ">,>>9".


Listado = /*W_PathSpl +*/ "c:\info\TotAgencia.LST".
/*OS-DELETE VALUE(Listado).*/
ASSIGN J = 0 k = 0.
{Incluido\RepEncabezado.i}

  W_Reporte   = "REPORTE   : VENCIMIENTO DE INVERSIONES"
              + " - ENTRE: " + STRING(FecIni) + " Y " + STRING(FecFin) + " Hora: " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGENICA                    FECHA                 VALOR            NUMERO".
OUTPUT TO VALUE(Listado) PAGED PAGE-SIZE 65 APPEND.
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH Inversion_Sdo WHERE
           Inversion_Sdo.Fec_Vcto NE ?      AND
           Inversion_Sdo.Fec_Vcto GE FecIni AND
           Inversion_Sdo.Fec_Vcto LE FecFin NO-LOCK
           BREAK BY Inversion_Sdo.Fec_Vcto BY Inversion_Sdo.Agencia:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Inversion_Sdo.Fec_Vcto) THEN DO:
         DISPLAY SKIP(1) 
                "FECHA DE VENCIMIENTO: " AT 1
                 Inversion_Sdo.Fec_Vcto AT 26
         WITH FRAME FFecVen WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
      END.
      ASSIGN Valor   = Valor  + Inversion_Sdo.Sdo_Actual
             Numero  = Numero + 1
             TValor  = TValor + Inversion_Sdo.Sdo_Actual
             TNumero = TNumero + 1.
      IF LAST-OF(Inversion_Sdo.Agencia) THEN DO:
        FIND Agencias WHERE Agencias.Agencia EQ Inversion_Sdo.Agencia NO-LOCK NO-ERROR.
        NomAge = "Agencia no Existe".
        IF AVAILABLE Agencias THEN
          NomAge = STRING(Agencias.Nombre,"X(35)") + "... $".
          DISPLAY NomAge     AT 1 
                  Valor      AT 45
                  Numero     AT 65
          WITH FRAME FCD1 WIDTH 300 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
          ASSIGN ValFecha = ValFecha + Valor
                 NumFecha = NumFecha + Numero.
          FIND Tot WHERE Tot.Age = Ahorros.Agencia AND
                         Tot.Ord = 2 NO-ERROR.
          IF NOT AVAILABLE Tot THEN DO:
             CREATE Tot.
             ASSIGN Tot.Ord = 2
                    Tot.Ope = Rt2
                    Tot.Age = Inversion_Sdos.Agencia.
          END.
          Tot.Val = Tot.Val + Valor.
          ASSIGN Valor = 0 Numero = 0 Promedio = 0.
      END.
      IF LAST-OF(Inversion_Sdo.Fec_Vcto) THEN DO:
         DISPLAY SKIP(1) 
                "TOTAL DE LA FECHA   : " AT 1
                 ValFecha                AT 45
                 NumFecha                AT 65
         WITH FRAME FTotFec WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
         ASSIGN ValFecha = 0 NumFecha = 0.
      END.
  END.
  DISPLAY SKIP(1)
          "TOTAL GENERAL DE LA ENTIDAD" AT 1
          TValor    AT 45
          TNumero   AT 65
  WITH FRAME FTotales WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

