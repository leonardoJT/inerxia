&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
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

DEFINE VAR AgeIni LIKE Agencias.Agencia.
DEFINE VAR AgeFin LIKE Agencias.Agencia.
DEFINE VAR UsuIni LIKE Usuarios.Usuario.
DEFINE VAR UsuFin LIKE Usuarios.Usuario.
DEFINE VAR EstIni LIKE Estaciones.Estacion.
DEFINE VAR EstFin LIKE Estaciones.Estacion.
DEFINE VAR FecIni AS DATE.
DEFINE VAR FecFin AS DATE.

  {Incluido/Variable.I "SHARED"}
  {Incluido/VARCON.I "SHARED"}

  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR i AS INTEGER.
  
  DEFINE VARIABLE WImagen  AS CHARACTER FORMAT "X(40)". 
  DEFINE VARIABLE WAno     AS INTEGER FORMAT "9999".    
  DEFINE VARIABLE WMes     AS INTEGER FORMAT "99".      
  DEFINE VARIABLE WFec     AS DATE.   

  DEFINE VARIABLE MesIni   AS INTEGER FORMAT "99".
  DEFINE VARIABLE MesFin   AS INTEGER FORMAT "99".
  
  
  
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

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Basicos

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_Usuarios BUTTON-143 BUTTON-144 ~
Cmb_Agencias BUTTON-149 Cmb_Estaciones w-BUTTON-148 BUTTON-153 E1 ~
BUTTON-150 BUTTON-152 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Usuarios Cmb_Agencias Cmb_Estaciones ~
E1 

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
     LABEL "Filtros de Información" 
     SIZE 24 BY 1.12.

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

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todas las Agencias" 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 57 BY 1
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE Cmb_Estaciones AS CHARACTER FORMAT "X(256)":U INITIAL "0000000000 - Todas las Estaciones" 
     LABEL "Estacion" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0000000000 - Todas las Estaciones" 
     DROP-DOWN-LIST
     SIZE 57 BY 1
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE Cmb_Usuarios AS CHARACTER FORMAT "X(256)":U INITIAL "0000 - Todos los Usuarios" 
     LABEL "Usuario" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "0000 - Todos los Usuarios" 
     DROP-DOWN-LIST
     SIZE 57 BY 1
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE E1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 96 BY 14
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

DEFINE BUTTON Btn_FecFin 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 121" 
     SIZE 3 BY .54.

DEFINE BUTTON Btn_FecIni 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "Button 120" 
     SIZE 3 BY .54.

DEFINE BUTTON BUTTON-171 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 171" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CueFin AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Final" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE CueIni AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta Inicial" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NitFin AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit Final" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NitIni AS CHARACTER FORMAT "X(14)":U 
     LABEL "Nit Inicial" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCueFin AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomCueIni AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomNitFin AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomNitIni AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 37 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ValFin AS DECIMAL FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor Final" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ValIni AS DECIMAL FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor Inicial" 
     VIEW-AS FILL-IN 
     SIZE 15 BY .81
     BGCOLOR 15  NO-UNDO.

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
     Cmb_Usuarios AT ROW 1.54 COL 13 COLON-ALIGNED
     BUTTON-143 AT ROW 1.54 COL 102
     BUTTON-144 AT ROW 1.81 COL 74
     Cmb_Agencias AT ROW 3.15 COL 13 COLON-ALIGNED
     BUTTON-149 AT ROW 3.15 COL 102
     Cmb_Estaciones AT ROW 4.5 COL 13 COLON-ALIGNED
     w-BUTTON-148 AT ROW 4.77 COL 102
     BUTTON-153 AT ROW 6.12 COL 3
     E1 AT ROW 8 COL 3 NO-LABEL
     BUTTON-150 AT ROW 19.04 COL 102
     BUTTON-152 AT ROW 20.92 COL 105
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.29 BY 21.81
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Buscar
     Buscar AT ROW 1.27 COL 7 COLON-ALIGNED
     BUTTON-154 AT ROW 1.54 COL 45
     Btn_anterior AT ROW 2.35 COL 9
     Btn_siguiente AT ROW 2.35 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 40 ROW 6.92
         SIZE 53 BY 3.5
         BGCOLOR 17 FONT 4
         TITLE "Buscar".

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
     Btn_FecFin AT ROW 2.04 COL 59
     W_DiaIni AT ROW 2.19 COL 10 COLON-ALIGNED NO-LABEL
     AnoIni AT ROW 2.19 COL 25 COLON-ALIGNED NO-LABEL
     Btn_FecIni AT ROW 2.19 COL 33
     W_DiaFin AT ROW 2.19 COL 38 NO-LABEL
     AnoFin AT ROW 2.19 COL 51 COLON-ALIGNED NO-LABEL
     CueIni AT ROW 3.69 COL 14 COLON-ALIGNED
     NomCueIni AT ROW 3.69 COL 29 COLON-ALIGNED NO-LABEL
     CueFin AT ROW 4.77 COL 14 COLON-ALIGNED
     NomCueFin AT ROW 4.77 COL 29 COLON-ALIGNED NO-LABEL
     NitIni AT ROW 6.38 COL 14 COLON-ALIGNED
     NomNitIni AT ROW 6.38 COL 29 COLON-ALIGNED NO-LABEL
     NitFin AT ROW 7.46 COL 14 COLON-ALIGNED
     NomNitFin AT ROW 7.46 COL 29 COLON-ALIGNED NO-LABEL
     ValIni AT ROW 8.81 COL 14 COLON-ALIGNED
     BUTTON-171 AT ROW 8.81 COL 57
     ValFin AT ROW 9.88 COL 14 COLON-ALIGNED
     "Fecha de Corte" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.42 COL 42
          BGCOLOR 17 FGCOLOR 7 
     "Fecha Inicial" VIEW-AS TEXT
          SIZE 11 BY .69 AT ROW 1.38 COL 16
          BGCOLOR 17 FGCOLOR 7 
     Img_MesF AT ROW 2.19 COL 42
     Img_MesI AT ROW 2.19 COL 16
     RECT-282 AT ROW 1.27 COL 11
     RECT-283 AT ROW 1.27 COL 37
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 14 ROW 6.38
         SIZE 70 BY 11.31
         BGCOLOR 17 FONT 5
         TITLE "Filtros".


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
         TITLE              = "SFG - Seguimiento de Transacciones a los Usuarios"
         HEIGHT             = 21.81
         WIDTH              = 114.29
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
ASSIGN FRAME F_Buscar:FRAME = FRAME F_Basicos:HANDLE
       FRAME F_Filtros:FRAME = FRAME F_Basicos:HANDLE
       FRAME F_Progreso:FRAME = FRAME F_Basicos:HANDLE.

/* SETTINGS FOR FRAME F_Basicos
   NOT-VISIBLE FRAME-NAME                                               */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Filtros:MOVE-AFTER-TAB-ITEM (BUTTON-153:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Buscar:MOVE-BEFORE-TAB-ITEM (E1:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-BEFORE-TAB-ITEM (FRAME F_Buscar:HANDLE)
       XXTABVALXX = FRAME F_Filtros:MOVE-BEFORE-TAB-ITEM (FRAME F_Progreso:HANDLE)
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
/* SETTINGS FOR FILL-IN NomCueFin IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomCueIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomNitFin IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomNitIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_DiaFin IN FRAME F_Filtros
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN W_DiaIni IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Progreso
                                                                        */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Seguimiento de Transacciones a los Usuarios */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Seguimiento de Transacciones a los Usuarios */
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


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Btn_FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FecFin wWin
ON CHOOSE OF Btn_FecFin IN FRAME F_Filtros /* Button 121 */
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


&Scoped-define SELF-NAME Btn_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_FecIni wWin
ON CHOOSE OF Btn_FecIni IN FRAME F_Filtros /* Button 120 */
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


&Scoped-define FRAME-NAME F_Buscar
&Scoped-define SELF-NAME Btn_siguiente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_siguiente wWin
ON CHOOSE OF Btn_siguiente IN FRAME F_Buscar /* Siguiente */
DO:
  ASSIGN FRAME F_Buscar Buscar.
  W_Ok = E1:SEARCH(Buscar,33) IN FRAME F_Basicos.
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
ON CHOOSE OF BUTTON-144 IN FRAME F_Basicos /* Filtros de Información */
DO:
  VIEW FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


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
&Scoped-define SELF-NAME BUTTON-171
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-171 wWin
ON CHOOSE OF BUTTON-171 IN FRAME F_Filtros /* Button 171 */
DO:
  HIDE FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON VALUE-CHANGED OF Cmb_Agencias IN FRAME F_Basicos /* Agencia */
DO:
  ASSIGN FRAME F_Basicos Cmb_Agencias.
  IF SUBSTRING(Cmb_Agencias,1,3) NE "000" THEN DO:
    Cmb_Estaciones:LIST-ITEMS = "".
    W_Ok = Cmb_Estaciones:ADD-LAST("0000000000 - Todas las Estaciones").
    FOR EACH Estaciones WHERE Estaciones.Agencia EQ INTEGER(SUBSTRING(Cmb_Agencias,1,3)) NO-LOCK:
        W_Ok = Cmb_Estaciones:ADD-LAST(STRING(Estaciones.Estacion,"9999999999") + " - " + Estaciones.Descripcion).
    END.
    Cmb_Estaciones:SCREEN-VALUE = Cmb_Estaciones:ENTRY(1).
  END.
  ELSE DO:
    Cmb_Estaciones:LIST-ITEMS = "".
    W_Ok = Cmb_Estaciones:ADD-LAST("0000000000 - Todas las Estaciones").
    FOR EACH Estaciones NO-LOCK:
       W_Ok = Cmb_Estaciones:ADD-LAST(STRING(Estaciones.Estacion,"9999999999") + " - " + Estaciones.Descripcion).
    END.
    Cmb_Estaciones:SCREEN-VALUE = Cmb_Estaciones:ENTRY(1).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME w-BUTTON-148
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-BUTTON-148 wWin
ON CHOOSE OF w-BUTTON-148 IN FRAME F_Basicos /* Ejecutar */
DO:
  ASSIGN FRAME F_Basicos Cmb_Usuarios Cmb_Agencias Cmb_Estaciones.
  ASSIGN FRAME F_Filtros AnoIni AnoFin 
               W_DiaIni W_DiaFin CueIni CueFin NitIni NitFin ValIni ValFin.
  
  IF SUBSTRING(Cmb_Usuarios,1,4) EQ "0000" THEN ASSIGN UsuIni = "0000" UsuFin = "9999".
  ELSE ASSIGN UsuIni = SUBSTRING(Cmb_Usuarios,1,4) 
              UsuFin = UsuIni.
  
  IF INTEGER(SUBSTRING(Cmb_Agencias,1,3)) EQ 0 THEN ASSIGN AgeIni = 000 AgeFin = 999.
  ELSE
     ASSIGN AgeIni =  INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = AgeIni.
  
  EstIni = TRIM(SUBSTRING(Cmb_Estaciones,1,10)).
  
  VIEW FRAME F_Progreso.
  RUN Armar_Informe.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Armar_Informe wWin 
PROCEDURE Armar_Informe :
Listado = /*W_PathSpl +*/ "c:\info_utrahuilca\Clientes.LST".  
DEFINE VAR W_ValInf LIKE Ahorros.Sdo_Disponible.
DEFINE VAR W_Natur LIKE Cuentas.Naturaleza.
DEFINE VAR W_Nombre AS CHARACTER FORMAT "X(25)".

OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1 AS CHARACTER FORMAT "X(75)".
DEFINE VAR WEnc2 AS CHARACTER FORMAT "X(2)".
DEFINE VAR TotReg AS DECIMAL.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Transacciones de Usuarios: " 
              + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
               /*          1         2         3         4         5         6         7         8         9         0         1         2*/  
               /* 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/ 
  W_EncColumna = "ESTACION   COMENTARIO                       NIT              NOMBRE                             VALOR        NAT  FECHA".
  VIEW FRAME F-Encabezado.
  
  ASSIGN j = 0 k = 0.
  
  FOR EACH Mov_Contable WHERE 
           Mov_Contable.Agencia      GE AgeIni AND
           Mov_Contable.Agencia      LE AgeFin AND
           Mov_Contable.Cuenta       GE CueIni AND
           Mov_Contable.Cuenta       LE CueFin AND
           Mov_Contable.Nit          GE NitIni AND
           Mov_Contable.Nit          LE NitFin AND
           Mov_Contable.Fec_Contable GE FecIni AND
           Mov_Contable.Fec_Contable LE FecFin AND
         ((Mov_Contable.DB           GE ValIni AND Mov_Contable.DB LE ValFin) OR
          (Mov_Contable.CR           GE ValIni AND Mov_Contable.CR LE ValFin)) AND
           Mov_Contable.Usuario      GE TRIM(UsuIni) AND
           Mov_Contable.Usuario      LE TRIM(UsuFin) NO-LOCK
           BREAK BY Mov_Contable.Usuario 
                 BY Mov_Contable.Agencia
                 BY Mov_Contable.Fec_Contable:
                 
      IF SUBSTRING(Cmb_Estaciones,1,10) NE "0000000000" AND 
         Mov_Contable.Estacion NE EstIni THEN DO:
         NEXT.
      END.
      j = j + 1.
      RUN Progreso.
      IF FIRST-OF(Mov_Contable.Usuario) THEN DO:
         FIND Usuarios WHERE Usuario.Usuario EQ Mov_Contable.Usuario NO-LOCK NO-ERROR.
         IF NOT AVAILABLE Usuarios THEN DO:
            DISPLAY SKIP(1) "Usuario      : Inconsistente" AT 1
                     Mov_Contable.Usuario AT 35
            WITH WIDTH 132 FRAME F_UsuInco USE-TEXT NO-BOX STREAM-IO NO-LABELS.
         END.
         ELSE DO:
            DISPLAY SKIP(1) "Usuario      : " AT 1
                     Usuarios.Usuario AT 20
                     Usuarios.Nombre  AT 30
            WITH WIDTH 132 FRAME F_Usuario USE-TEXT NO-BOX STREAM-IO NO-LABELS.
         END.
         
      END.
      IF FIRST-OF(Mov_Contable.Agencia) THEN DO:
         FIND Agencias WHERE Agencias.Agencia EQ Mov_Contable.Agencia NO-LOCK NO-ERROR.
         IF AVAILABLE Agencias THEN DO:
            DISPLAY SKIP(1) "Agencia      : " AT 1
                     Agencias.Agencia AT 20
                     Agencias.Nombre  AT 30
                     "-----------------------------------------------------------------------------------------------------------------------------" AT 1
            WITH WIDTH 132 FRAME F_Agencia USE-TEXT NO-BOX STREAM-IO NO-LABELS.
         END.
      END.
      W_ValInf = Mov_Contable.CR + Mov_Contable.DB.
      IF Mov_Contable.CR GT 0 THEN W_Natur = "CR". ELSE W_Natur = "DB".
      W_Nombre = "Cedula en blanco y  Nombre en Blanco".
      IF Mov_Contable.Nit NE "" THEN DO:
         FIND Clientes WHERE Clientes.Nit EQ Mov_Contable.Nit NO-LOCK NO-ERROR.
         W_Nombre = "No en clientes".
         IF AVAILABLE Clientes THEN
            W_Nombre = Mov_Contable.Nit + " - " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
      END.
      DISPLAY 
        Mov_Contable.Estacion       AT 1   FORMAT "X(10)"
        Mov_Contable.Comentario     AT 12  FORMAT "X(30)"
        W_Nombre                    AT 45  FORMAT "X(35)"
        W_ValInf                    AT 82  FORMAT ">>,>>>,>>>,>>9"
        W_Natur                     AT 97
        Mov_Contable.Fec_Contable   AT 100
        Mov_Contable.Cuenta         AT 113
      WITH FRAME F_Movimiento WIDTH 132 NO-LABEL NO-BOX USE-TEXT STREAM-IO NO-UNDERLINE.
      ASSIGN TotRegAge = TotRegAge + 1
             TotSdoAge = TotSdoAge + W_ValInf.
      IF LAST-OF(Mov_Contable.Usuario) THEN DO:
         DISPLAY SKIP(1) 
                 "Total Usuario: " AT 1
                 TotRegAge         AT 50
                 TotSdoAge         AT 90
                 "-----------------------------------------------------------------------------------------------------------------------------" AT 1
         WITH FRAME F_Tot WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
         ASSIGN TotRegAge = 0 TotSdoAge = 0.
      END.
  END.
  DISPLAY SKIP "Total de Registros Reportados: " TotReg WITH FRAME FT WIDTH 132 NO-LABELS.
  VIEW FRAME F-Ftr.
  PAGE.

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
  DISPLAY Cmb_Usuarios Cmb_Agencias Cmb_Estaciones E1 
      WITH FRAME F_Basicos IN WINDOW wWin.
  ENABLE Cmb_Usuarios BUTTON-143 BUTTON-144 Cmb_Agencias BUTTON-149 
         Cmb_Estaciones w-BUTTON-148 BUTTON-153 E1 BUTTON-150 BUTTON-152 
      WITH FRAME F_Basicos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Basicos}
  DISPLAY W_DiaIni AnoIni W_DiaFin AnoFin CueIni NomCueIni CueFin NomCueFin 
          NitIni NomNitIni NitFin NomNitFin ValIni ValFin 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE Img_MesF Img_MesI RECT-282 RECT-283 Btn_FecFin Btn_FecIni CueIni 
         CueFin NitIni NitFin ValIni BUTTON-171 ValFin 
      WITH FRAME F_Filtros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Filtros}
  ENABLE IMAGE-2 R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW wWin.
  VIEW FRAME F_Progreso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
  DISPLAY Buscar 
      WITH FRAME F_Buscar IN WINDOW wWin.
  ENABLE Buscar BUTTON-154 Btn_anterior Btn_siguiente 
      WITH FRAME F_Buscar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Buscar}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*
        FOR EACH Usuarios NO-LOCK BREAK BY INTEGER(Usuarios.Usuario):
     W_Ok = Cmb_Usuarios:ADD-LAST(STRING(INTEGER(Usuarios.Usuario),">>>9") + " - " + Usuarios.Nombre) IN FRAME F_Basicos.
  END.
 */
  Cmb_Usuarios:SCREEN-VALUE = Cmb_Usuarios:ENTRY(1).
  FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK:
     W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
  END.
  Cmb_Agencias:SCREEN-VALUE = Cmb_Agencias:ENTRY(1).
  FOR EACH Estaciones NO-LOCK:
     W_Ok = Cmb_Estaciones:ADD-LAST(STRING(Estaciones.Estacion,"9999999999") + " - " + Estaciones.Descripcion).
  END.
  Cmb_Estaciones:SCREEN-VALUE = Cmb_Estaciones:ENTRY(1).
  RUN SUPER.
  DO WITH FRAME F_Filtros:
    ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(MONTH(W_Fecha)) + ".gif")
           W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(W_Fecha)) + ".gif")
           AnoIni:SCREEN-VALUE = STRING(YEAR(W_Fecha))
           AnoFin:SCREEN-VALUE = STRING(YEAR(W_Fecha))
           MesIni = MONTH(W_Fecha)
           MesFin = MONTH(W_Fecha)
           AgeIni = W_Agencia
           AgeFin = W_Agencia
           FecIni = W_Fecha - DAY(W_Fecha) + 1
           FecFin = W_Fecha
           W_DiaIni:SCREEN-VALUE = "01"
           W_DiaFin:SCREEN-VALUE  = STRING(DAY(W_Fecha))
           CueIni:SCREEN-VALUE    = "1"
           NomCueIni:SCREEN-VALUE = "Todas las Cuentas"
           CueFin:SCREEN-VALUE    = "99999999999999"
           NomCueFin:SCREEN-VALUE = "Todas las Cuentas"
           NitIni:SCREEN-VALUE    = "              "
           NomNitIni:SCREEN-VALUE = "Todos los Nits"
           NitFin:SCREEN-VALUE    = "99999999999999"
           NomNitFin:SCREEN-VALUE = "Todos los Nits"
           ValIni:SCREEN-VALUE    = "0"
           ValFin:SCREEN-VALUE    = "999999999".
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

