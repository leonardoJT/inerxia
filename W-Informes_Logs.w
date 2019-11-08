&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
  {Incluido/Variable.I "SHARED"}
  {Incluido/VARCON.I "SHARED"}

  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR i AS INTEGER.
  
  DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
  DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
  DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
  DEFINE VARIABLE WFec     AS DATE.   
  DEFINE VARIABLE FecIni   AS DATE.
  DEFINE VARIABLE FecFin   AS DATE.
  DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
  DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".
  DEFINE VARIABLE AgeIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE AgeFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE UsuIni   LIKE Usuarios.Usuario INITIAL "".
  DEFINE VARIABLE UsuFin   LIKE Usuarios.Usuario INITIAL "9999".
  DEFINE VARIABLE EstIni   LIKE Estaciones.Estacion INITIAL "".
  DEFINE VARIABLE EstFin   LIKE Estaciones.Estacion INITIAL "9999999999".
  
  DEFINE VARIABLE EstadoC  AS INTEGER FORMAT "9".

  DEFINE VAR Listado AS CHARACTER INITIAL "".

 DEFINE VAR j AS DECIMAL.
 DEFINE VAR k AS INTEGER.

DEFINE VAR W_Nin AS CHARACTER FORMAT "X(61)".
DEFINE VAR W_Vig AS INTEGER FORMAT "9999".
DEFINE VAR W_Nus AS CHARACTER FORMAT "X(20)".

DEFINE VAR W_Compromiso AS CHARACTER FORMAT "X(2)".
DEFINE VAR W_Interes AS DECIMAL FORMAT "->>>,>>>,>>9".
DEFINE VAR TotReg    AS DECIMAL.
DEFINE VAR TotSdo    AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR TotRegAge AS DECIMAL.
DEFINE VAR TotSdoAge AS DECIMAL FORMAT ">,>>>,>>>,>>9".
DEFINE VAR W_TipPdt AS CHARACTER FORMAT "X(12)".
DEFINE VAR W_Vigen  AS INTEGER FORMAT "9999".
DEFINE VAR W_ForPag AS CHARACTER FORMAT "X(10)".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Basicos

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-144 BUTTON-143 BUTTON-153 BUTTON-149 ~
E1 w-BUTTON-148 BUTTON-150 BUTTON-152 
&Scoped-Define DISPLAYED-OBJECTS E1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-144 
     LABEL "Cambiar los Rangos de Filtro de Información" 
     SIZE 40 BY 1.12.

DEFINE BUTTON BUTTON-149 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-150 
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-152 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 152" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 153" 
     SIZE 7 BY 1.65.

DEFINE BUTTON w-BUTTON-148 
     LABEL "Ejecutar" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE E1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 96 BY 17.5
     BGCOLOR 15 FONT 2 NO-UNDO.

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

DEFINE BUTTON BUTTON-120 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-121 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 4 BY .54.

DEFINE BUTTON BUTTON-145 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 145" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 53 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Estaciones AS CHARACTER FORMAT "X(256)":U INITIAL "0000000000 - Todas las Estaciones" 
     LABEL "Estaciones" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0000000000 - Todas las Estaciones" 
     DROP-DOWN-LIST
     SIZE 41 BY 1
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE Cmb_Usuarios AS CHARACTER FORMAT "X(256)":U INITIAL "0000 - Todos los Usuarios" 
     LABEL "Usuarios Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0000 - Todos los Usuarios" 
     DROP-DOWN-LIST
     SIZE 41 BY 1
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

DEFINE VARIABLE RTipo AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Log del Sistema", 0,
"Correspondencia Interna", 1
     SIZE 50 BY .81 NO-UNDO.

DEFINE VARIABLE R_Estado AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activos", 1,
"Inactivos", 2
     SIZE 29 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-282
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 26 BY 1.88.

DEFINE RECTANGLE RECT-283
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 27 BY 1.88.

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

DEFINE FRAME F_Basicos
     BUTTON-144 AT ROW 1.27 COL 3
     BUTTON-143 AT ROW 1.54 COL 102
     BUTTON-153 AT ROW 2.88 COL 3
     BUTTON-149 AT ROW 3.15 COL 102
     E1 AT ROW 4.5 COL 3 NO-LABEL
     w-BUTTON-148 AT ROW 4.77 COL 102
     BUTTON-150 AT ROW 19.04 COL 102
     BUTTON-152 AT ROW 20.92 COL 105
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 21.38
         BGCOLOR 17 FONT 5.

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
         SIZE 12 BY 6.73
         BGCOLOR 17 .

DEFINE FRAME F_Filtros
     RTipo AT ROW 1.54 COL 12 NO-LABEL
     Cmb_Agencias AT ROW 2.62 COL 10 COLON-ALIGNED
     BUTTON-121 AT ROW 4.46 COL 60
     W_DiaIni AT ROW 4.62 COL 11 COLON-ALIGNED NO-LABEL
     AnoIni AT ROW 4.62 COL 26 COLON-ALIGNED NO-LABEL
     BUTTON-120 AT ROW 4.62 COL 34
     W_DiaFin AT ROW 4.62 COL 39 NO-LABEL
     AnoFin AT ROW 4.62 COL 52 COLON-ALIGNED NO-LABEL
     R_Estado AT ROW 5.85 COL 22 NO-LABEL
     Cmb_Usuarios AT ROW 6.92 COL 21 COLON-ALIGNED
     Cmb_Estaciones AT ROW 8 COL 21 COLON-ALIGNED
     BUTTON-145 AT ROW 9.35 COL 54
     Img_MesF AT ROW 4.62 COL 43
     Img_MesI AT ROW 4.62 COL 17
     RECT-282 AT ROW 3.69 COL 12
     RECT-283 AT ROW 3.69 COL 38
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 11 BY .69 AT ROW 3.81 COL 17
          BGCOLOR 17 FGCOLOR 7 
     "Fecha de Final" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 3.85 COL 43
          BGCOLOR 17 FGCOLOR 7 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23 ROW 1.27
         SIZE 69 BY 11.58
         BGCOLOR 17 FONT 5
         TITLE "Filtros de Información".

DEFINE FRAME F_Buscar
     Buscar AT ROW 1.27 COL 7 COLON-ALIGNED
     BUTTON-154 AT ROW 1.54 COL 45
     Btn_anterior AT ROW 2.35 COL 9
     Btn_siguiente AT ROW 2.35 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14 ROW 3.69
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
         TITLE              = "SFG - Modulo de Informes Básicos"
         HEIGHT             = 21.38
         WIDTH              = 113.57
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
ASSIGN FRAME F_Buscar:FRAME = FRAME F_Basicos:HANDLE
       FRAME F_Filtros:FRAME = FRAME F_Basicos:HANDLE
       FRAME F_Progreso:FRAME = FRAME F_Basicos:HANDLE.

/* SETTINGS FOR FRAME F_Basicos
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Filtros:MOVE-AFTER-TAB-ITEM (BUTTON-144:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Filtros:MOVE-BEFORE-TAB-ITEM (BUTTON-143:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Buscar:MOVE-AFTER-TAB-ITEM (BUTTON-149:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Buscar:MOVE-BEFORE-TAB-ITEM (E1:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-AFTER-TAB-ITEM (w-BUTTON-148:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-BEFORE-TAB-ITEM (BUTTON-150:HANDLE IN FRAME F_Basicos)
/* END-ASSIGN-TABS */.

/* SETTINGS FOR FRAME F_Buscar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Buscar:HIDDEN           = TRUE
       FRAME F_Buscar:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Filtros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Filtros:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN AnoFin IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN AnoIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DiaFin IN FRAME F_Filtros
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_DiaIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
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
ON END-ERROR OF wWin /* SFG - Modulo de Informes Básicos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Modulo de Informes Básicos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME Btn_anterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_anterior wWin
ON CHOOSE OF Btn_anterior IN FRAME F_Buscar /* Anterior */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,34) IN FRAME F_Basicos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_siguiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_siguiente wWin
ON CHOOSE OF Btn_siguiente IN FRAME F_Buscar /* Siguiente */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,33) IN FRAME F_Basicos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-120
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-120 wWin
ON CHOOSE OF BUTTON-120 IN FRAME F_Filtros /* Button 120 */
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
ON CHOOSE OF BUTTON-121 IN FRAME F_Filtros /* Button 121 */
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


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME BUTTON-143
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-143 wWin
ON CHOOSE OF BUTTON-143 IN FRAME F_Basicos /* Button 143 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-144
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-144 wWin
ON CHOOSE OF BUTTON-144 IN FRAME F_Basicos /* Cambiar los Rangos de Filtro de Información */
DO:
  VIEW FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-145
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-145 wWin
ON CHOOSE OF BUTTON-145 IN FRAME F_Filtros /* Button 145 */
DO:
  HIDE FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME BUTTON-149
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-149 wWin
ON CHOOSE OF BUTTON-149 IN FRAME F_Basicos /* Button 149 */
DO:
  IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
    DEFINE VAR W_sw          AS LOGICAL. 
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


&Scoped-define SELF-NAME BUTTON-150
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-150 wWin
ON CHOOSE OF BUTTON-150 IN FRAME F_Basicos /* Salir */
DO:
  OS-DELETE VALUE(Listado).
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


&Scoped-define SELF-NAME BUTTON-153
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON CHOOSE OF BUTTON-153 IN FRAME F_Basicos /* Button 153 */
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


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME F_Filtros /* Agencias */
DO:
  ASSIGN FRAME F_Filtros Cmb_Agencias R_Estado.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).
  Cmb_Usuarios:LIST-ITEMS = "".
  FOR EACH Usuarios WHERE Usuarios.Agencia GE AgeIni AND Usuarios.Agencia LE AgeFin AND
                          Usuarios.Estado EQ R_Estado NO-LOCK:
     W_Ok = Cmb_Usuarios:ADD-LAST(STRING(Usuarios.Usuario,"X(4)") + " - " + Usuarios.Nombre).
  END.
  ASSIGN W_Ok = Cmb_Usuarios:ADD-FIRST("0000 - Todos los Usuarios de la Agencia")
         Cmb_Usuarios:SCREEN-VALUE = Cmb_Usuarios:ENTRY(2)
         Cmb_Estaciones:LIST-ITEMS = "".
  FOR EACH Estaciones WHERE Estaciones.Agencia GE AgeIni AND Estaciones.Agencia LE AgeFin NO-LOCK:
    W_Ok = Cmb_Estaciones:ADD-LAST(STRING(Estaciones.Estacion,"X(10)") + " - " + Estaciones.Descripcion).
  END.
  ASSIGN W_Ok = Cmb_Estaciones:ADD-FIRST("0000000000 - Todas las Estaciones de la Agencia") 
         Cmb_Estaciones:SCREEN-VALUE = Cmb_Estaciones:ENTRY(2).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME w-BUTTON-148
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-BUTTON-148 wWin
ON CHOOSE OF w-BUTTON-148 IN FRAME F_Basicos /* Ejecutar */
DO:
  ASSIGN FRAME F_Filtros RTipo Cmb_Usuarios 
               Cmb_Estaciones AnoIni AnoFin Cmb_Agencias W_DiaIni W_DiaFin.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = AgeIni.
  
  IF SUBSTRING(Cmb_Usuarios,1,4) EQ "0000" THEN
     ASSIGN UsuIni = "" UsuFin = "9999".
  ELSE 
     ASSIGN UsuIni = SUBSTRING(Cmb_Usuarios,1,4) UsuFin = UsuIni.

  IF SUBSTRING(Cmb_Estaciones,1,10) EQ "0000000000" THEN ASSIGN EstIni = "" EstFin = "9999999999".
  ELSE ASSIGN EstIni = SUBSTRING(Cmb_Estaciones,1,10) EstFin = EstIni.
  
  VIEW FRAME F_Progreso.
  RUN InfoLog.
  W_ok = e1:READ-FILE(Listado) IN FRAME F_Basicos.
  HIDE FRAME F_Progreso.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.
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
  DISPLAY E1 
      WITH FRAME F_Basicos IN WINDOW wWin.
  ENABLE BUTTON-144 BUTTON-143 BUTTON-153 BUTTON-149 E1 w-BUTTON-148 BUTTON-150 
         BUTTON-152 
      WITH FRAME F_Basicos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Basicos}
  DISPLAY RTipo Cmb_Agencias W_DiaIni AnoIni W_DiaFin AnoFin R_Estado 
          Cmb_Usuarios Cmb_Estaciones 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE RTipo Cmb_Agencias BUTTON-121 BUTTON-120 R_Estado Cmb_Usuarios 
         Cmb_Estaciones BUTTON-145 Img_MesF Img_MesI RECT-282 RECT-283 
      WITH FRAME F_Filtros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Filtros}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel wWin 
PROCEDURE Imprimir_Excel :
MESSAGE "Opción deshabilitada" VIEW-AS ALERT-BOX INFORMATION.
/*CASE Cmb_Tipo:
  WHEN "Clientes" THEN RUN ImpEx_Clientes.
  WHEN "Ahorros"  THEN RUN ImpEx_Ahorros.
END CASE.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE InfoLog wWin 
PROCEDURE InfoLog :
Listado = /*W_PathSpl +*/ "c:\info\Clientes.LST".  
OS-DELETE VALUE(Listado).
DEFINE VAR TotReg AS DECIMAL.
DEFINE VAR NomAge AS CHARACTER FORMAT "X(40)".
DEFINE VAR NomUsu AS CHARACTER FORMAT "X(100)".
DEFINE VAR NomEst AS CHARACTER FORMAT "X(40)".
DEFINE VAR Obs AS CHARACTER FORMAT "X(100)".
DEFINE VAR TotMot AS DECIMAL.

OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : LOGS " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "ADV: Advertencia, NOR: Normal, IN: Ingreso".

  VIEW FRAME F-Encabezado.
  ASSIGN j = 0 k = 0.
  FOR EACH Logs WHERE 
           Logs.Agencia     GE AgeIni AND
           Logs.Agencia     LE AgeFin AND
           Logs.Usuario     GE UsuIni AND
           Logs.Usuario     LE UsuFin AND
           Logs.Estacion    GE EstIni AND
           Logs.Estacion    LE EstFin AND
           Logs.Fecha       GE FecIni AND
           Logs.Fecha       LE FecFin AND
           Logs.IdRegistro  EQ RTipo
           NO-LOCK BREAK BY Logs.Agencia BY Logs.Usuario BY Logs.Estacion BY Logs.Fecha BY Logs.HoraE:  
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Logs.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Logs.Agencia NO-LOCK NO-ERROR.
         NomAge = "Agencia no encontrada".
         IF AVAILABLE Agencias THEN NomAge = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
         DISPLAY NomAge AT 1 WITH FRAME FAge WIDTH 132 NO-LABELS USE-TEXT NO-BOX.         
      END.
      IF FIRST-OF(Logs.Usuario) THEN DO:
         FIND Usuarios WHERE Usuarios.Usuario EQ Logs.Usuario NO-LOCK NO-ERROR.
         NomUsu = "".
         IF AVAILABLE Usuarios THEN NomUsu = STRING(Usuarios.Usuario,"X(4)") + " - " + Usuarios.Nombre.
         DISPLAY SKIP(1) 
             NomUsu AT 1 FORMAT "X(35)" 
             WITH FRAME FUsu WIDTH 132 NO-LABELS NO-BOX USE-TEXT.
      END.
      IF FIRST-OF(Logs.Estacion) THEN DO:
         ASSIGN NomEst = " / Estacion : " + Logs.Estacion.
         DISPLAY SKIP(1) 
             NomEst AT 3 FORMAT "X(25)"
             WITH FRAME FEst WIDTH 132 NO-LABELS NO-BOX USE-TEXT.
      END.
      IF FIRST-OF(Logs.Fecha) THEN DO:
         DISPLAY SKIP(1) 
         Logs.Fecha AT 4 WITH FRAME FFec WIDTH 132 NO-LABELS NO-BOX USE-TEXT.
      END.
      IF Logs.IdRegistro EQ 0 THEN DO:
        Obs = Logs.Observacion.
        DISPLAY STRING(Logs.HoraE,"hh:mm am") AT 5
                Obs              AT 15 FORMAT "X(100)"
        WITH FRAME Fdetalle WIDTH 132 NO-LABELS STREAM-IO NO-BOX USE-TEXT.
      END.
      ELSE DO:
        IF FechaL EQ ? THEN 
           Obs = "Mensaje aún no leido por el usuario".
        ELSE
           Obs = "Mensaje leido el: " + STRING(Logs.FechaL) + " a las " + STRING(Logs.HoraL,"hh:mm am").
        DISPLAY STRING(Logs.HoraE,"hh:mm am") AT 5
                Obs                           AT 15
                Logs.Observacion              AT 15
        WITH FRAME Fdetalle2 WIDTH 132 NO-LABELS STREAM-IO NO-BOX USE-TEXT.
      END.
      TotReg = TotReg + 1.
  END.
  VIEW FRAME F-Ftr.
  PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DO WITH FRAME F_Filtros:
     FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
        W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
        IF Agencias.Agencia EQ W_Agencia THEN
           Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
     END.
     FOR EACH Usuarios WHERE Usuarios.Agencia EQ W_Agencia AND Usuarios.Estado EQ 1 NO-LOCK:
       W_Ok = Cmb_Usuarios:ADD-LAST(STRING(Usuarios.Usuario,"X(4)") + " - " + Usuarios.Nombre).
     END.
     FOR EACH Estaciones WHERE Estaciones.Agencia EQ W_Agencia NO-LOCK:
       W_Ok = Cmb_Estaciones:ADD-LAST(STRING(Estaciones.Estacion,"X(10)") + " - " + Estaciones.Descripcion).
     END.
  END.
  W_DiaFin = DAY(TODAY).
  RUN SUPER.
  DO WITH FRAME F_Filtros:
    ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month1.gif")
           W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
           AnoIni:SCREEN-VALUE = "1960"
           AnoFin:SCREEN-VALUE = STRING(YEAR(TODAY))
           MesIni = 01
           MesFin = MONTH(TODAY)
           AgeIni = W_Agencia
           AgeFin = W_Agencia
           FecIni = DATE("01/01/1960")
           FecFin = TODAY.
   END.

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

