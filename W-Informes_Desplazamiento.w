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
    DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
    DEFINE VAR W_sw          AS LOGICAL. 
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
  DEFINE VARIABLE ProIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE ProFin   AS INTEGER FORMAT "999" INITIAL 999.
  DEFINE VARIABLE TpdIni   AS INTEGER FORMAT "999" INITIAL 0.
  DEFINE VARIABLE TpdFin   AS INTEGER FORMAT "999" INITIAL 9.
  DEFINE VARIABLE EstadoC  AS INTEGER FORMAT "9".
  DEFINE VARIABLE Listado  AS CHARACTER INITIAL "".
  DEFINE VARIABLE WListado AS CHARACTER INITIAL "".
  DEFINE VARIABLE j        AS DECIMAL.
  DEFINE VARIABLE k        AS INTEGER.

  DEFINE TEMP-TABLE Corte
      FIELD Agencia       LIKE Agencias.Agencia
      FIELD Cod_Califica  LIKE Creditos.Cod_Califica
      FIELD Numero        AS INTEGER FORMAT ">>,>>9"
      FIELD Valor         AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".

  DEFINE TEMP-TABLE Comparacion
      FIELD Agencia       LIKE Agencias.Agencia
      FIELD Cod_Califica  LIKE Creditos.Cod_Califica
      FIELD Numero        AS INTEGER FORMAT ">>,>>9"
      FIELD Valor         AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".

  DEFINE TEMP-TABLE Diferencia
      FIELD Agencia       LIKE Agencias.Agencia
      FIELD Cod_Califica  LIKE Creditos.Cod_Califica
      FIELD Numero        AS INTEGER FORMAT "->>,>>9"
      FIELD Valor         AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".

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
&Scoped-Define ENABLED-OBJECTS Cmb_Tipo BUTTON-143 BUTTON-153 BUTTON-144 ~
Btn_Imp E1 w-BUTTON-148 BUTTON-150 BUTTON-152 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Tipo E1 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 149" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-144 
     LABEL "Cambiar los Rangos de Filtro de Información" 
     SIZE 40 BY 1.12.

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

DEFINE VARIABLE Cmb_Tipo AS CHARACTER FORMAT "X(30)":U INITIAL "Clientes" 
     LABEL "Tipo de Informe" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "General","Por Usuario","Por Linea de Crédito","Por Tipo de Crédito" 
     DROP-DOWN-LIST
     SIZE 21.72 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE E1 AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 96 BY 16.42
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

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(40)":U 
     LABEL "Agencias" 
     VIEW-AS COMBO-BOX INNER-LINES 15
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todos" 
     LABEL "Productos Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todos" 
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_TProducto AS CHARACTER FORMAT "X(256)":U 
     LABEL "Tipos de Producto" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 37 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoFin AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE AnoIni AS INTEGER FORMAT "9999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE UsuFin AS CHARACTER FORMAT "X(4)":U 
     LABEL "Usuario Final" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE UsuIni AS CHARACTER FORMAT "X(4)":U 
     LABEL "Usuario Inicial" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
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
     Cmb_Tipo AT ROW 1.54 COL 15 COLON-ALIGNED
     BUTTON-143 AT ROW 1.54 COL 102
     BUTTON-153 AT ROW 2.88 COL 3
     BUTTON-144 AT ROW 2.88 COL 56
     Btn_Imp AT ROW 3.15 COL 102
     E1 AT ROW 4.77 COL 3 NO-LABEL
     w-BUTTON-148 AT ROW 4.77 COL 102
     BUTTON-150 AT ROW 19.04 COL 102
     BUTTON-152 AT ROW 20.92 COL 105
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.29 BY 21.04
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Buscar
     Buscar AT ROW 1.27 COL 7 COLON-ALIGNED
     BUTTON-154 AT ROW 1.54 COL 45
     Btn_anterior AT ROW 2.35 COL 9
     Btn_siguiente AT ROW 2.35 COL 27
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 41 ROW 4.5
         SIZE 53 BY 3.5
         BGCOLOR 17 FONT 4
         TITLE "Buscar".

DEFINE FRAME F_Filtros
     Cmb_Agencias AT ROW 1.27 COL 9 COLON-ALIGNED
     W_DiaIni AT ROW 3.27 COL 11 COLON-ALIGNED NO-LABEL
     AnoIni AT ROW 3.27 COL 26 COLON-ALIGNED NO-LABEL
     BUTTON-120 AT ROW 3.27 COL 34
     W_DiaFin AT ROW 3.27 COL 39 NO-LABEL
     AnoFin AT ROW 3.27 COL 52 COLON-ALIGNED NO-LABEL
     BUTTON-121 AT ROW 3.31 COL 59.86
     Cmb_TProducto AT ROW 5.04 COL 25 COLON-ALIGNED
     Cmb_Productos AT ROW 6.12 COL 25 COLON-ALIGNED
     UsuIni AT ROW 7.19 COL 37 COLON-ALIGNED
     UsuFin AT ROW 7.19 COL 56 COLON-ALIGNED
     BUTTON-145 AT ROW 8.54 COL 55
     Img_MesF AT ROW 3.27 COL 43
     Img_MesI AT ROW 3.27 COL 17
     RECT-282 AT ROW 2.35 COL 12
     RECT-283 AT ROW 2.35 COL 38
     "Fecha Corte" VIEW-AS TEXT
          SIZE 11 BY .69 AT ROW 2.54 COL 13
          BGCOLOR 17 FGCOLOR 7 
     "Fecha de Comparación" VIEW-AS TEXT
          SIZE 21 BY .62 AT ROW 2.54 COL 39
          BGCOLOR 17 FGCOLOR 7 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 30 ROW 4.5
         SIZE 69 BY 10.77
         BGCOLOR 17 FONT 5
         TITLE "Filtros de Información".

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
         TITLE              = "SFG - Modulo de Informes de Desplazamiento"
         HEIGHT             = 21.04
         WIDTH              = 114.29
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

ASSIGN XXTABVALXX = FRAME F_Filtros:MOVE-AFTER-TAB-ITEM (Btn_Imp:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Buscar:MOVE-BEFORE-TAB-ITEM (E1:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-AFTER-TAB-ITEM (w-BUTTON-148:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Progreso:MOVE-BEFORE-TAB-ITEM (BUTTON-150:HANDLE IN FRAME F_Basicos)
       XXTABVALXX = FRAME F_Filtros:MOVE-BEFORE-TAB-ITEM (FRAME F_Buscar:HANDLE)
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
/* SETTINGS FOR COMBO-BOX Cmb_Productos IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX Cmb_TProducto IN FRAME F_Filtros
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN UsuFin IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       UsuFin:HIDDEN IN FRAME F_Filtros           = TRUE.

/* SETTINGS FOR FILL-IN UsuIni IN FRAME F_Filtros
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       UsuIni:HIDDEN IN FRAME F_Filtros           = TRUE.

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


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Basicos
/* Query rebuild information for FRAME F_Basicos
     _Query            is NOT OPENED
*/  /* FRAME F_Basicos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Filtros
/* Query rebuild information for FRAME F_Filtros
     _Query            is NOT OPENED
*/  /* FRAME F_Filtros */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Modulo de Informes de Desplazamiento */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Modulo de Informes de Desplazamiento */
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


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp wWin
ON CHOOSE OF Btn_Imp IN FRAME F_Basicos /* Button 149 */
DO:
  IF Listado EQ "" THEN 
     MESSAGE "No se ha ejecutado ningún informe" VIEW-AS ALERT-BOX.
  ELSE
  DO:
    ASSIGN WListado = Listado.
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Listado,INPUT-OUTPUT W_Dispositivo).
  OS-RENAME VALUE(WListado) VALUE(Listado).
  
  /*MESSAGE listado VIEW-AS ALERT-BOX.*/
/*    IF W_Dispositivo = "" THEN
      RETURN.*/
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
  ASSIGN FRAME F_Filtros Cmb_Agencias.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = INTEGER(SUBSTRING(Cmb_Agencias,1,3)).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON VALUE-CHANGED OF Cmb_Productos IN FRAME F_Filtros /* Productos Disponibles */
DO:
  ASSIGN FRAME F_Filtros Cmb_Productos.
  IF Cmb_Productos EQ "000 - Todos" THEN ASSIGN ProIni = 0 ProFin = 999.
  ELSE ASSIGN ProIni = INTEGER(SUBSTRING(Cmb_Productos,1,3)) ProFin = ProIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME Cmb_Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Tipo wWin
ON VALUE-CHANGED OF Cmb_Tipo IN FRAME F_Basicos /* Tipo de Informe */
DO:
  ASSIGN FRAME F_Basicos Cmb_Tipo.
  ENABLE BUTTON-144 WITH FRAME F_basicos.
  ASSIGN Cmb_TProducto:LIST-ITEMS IN FRAME F_Filtros = ""
         Cmb_Productos:LIST-ITEMS IN FRAME F_Filtros = ""
         W_Ok = Cmb_Productos:ADD-LAST("000 - Todos")
         Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME Cmb_TProducto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TProducto wWin
ON VALUE-CHANGED OF Cmb_TProducto IN FRAME F_Filtros /* Tipos de Producto */
DO:
  ASSIGN FRAME F_Basicos Cmb_Tipo.
  ASSIGN FRAME F_Filtros Cmb_TProducto
         Cmb_Productos:LIST-ITEMS = ""
         W_Ok = Cmb_Productos:ADD-LAST("000 - Todos")
         Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
  IF Cmb_Tipo EQ "Ahorros" THEN DO:
     FOR EACH Pro_Ahorros WHERE 
         Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TProducto,1,1)) AND
         Pro_Ahorros.Estado       EQ 1 NO-LOCK:
         W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).      
     END.
  END.
  IF Cmb_Tipo EQ "Créditos" THEN DO:
     FOR EACH Pro_Creditos WHERE 
         Pro_Creditos.Tip_Credito EQ INTEGER(SUBSTRING(Cmb_TProducto,1,1)) AND
         Pro_Creditos.Estado       EQ 1 NO-LOCK:
         W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Credito.Cod_Credito,"999") + " - " + Pro_Credito.Nom_Producto).      
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Basicos
&Scoped-define SELF-NAME w-BUTTON-148
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL w-BUTTON-148 wWin
ON CHOOSE OF w-BUTTON-148 IN FRAME F_Basicos /* Ejecutar */
DO:
  FOR EACH Corte: DELETE Corte. END.
  FOR EACH Comparacion: DELETE Comparacion. END.
  FOR EACH Diferencia: DELETE Diferencia. END.
  ASSIGN FRAME F_Basicos Cmb_Tipo.
  ASSIGN FRAME F_Filtros AnoIni AnoFin Cmb_Agencias W_DiaIni W_DiaFin UsuIni UsuFin
               Cmb_TProducto Cmb_Productos.

  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3)) AgeFin = AgeIni.
  
  IF SUBSTRING(Cmb_TProducto,1,1) EQ "0" THEN ASSIGN TpdIni = 0 TpdFin = 9.
  ELSE ASSIGN TpdIni = INTEGER(SUBSTRING(Cmb_TProducto,1,1)) TpdFin = TpdIni.
  
  IF SUBSTRING(Cmb_Productos,1,3) EQ "000" THEN ASSIGN ProIni = 0 ProFin = 999.
  ELSE ASSIGN ProIni = INTEGER(SUBSTRING(Cmb_Productos,1,3)) ProFin = ProIni.
  
  IF W_DiaIni NE DAY(FecIni) OR MesIni NE MONTH(FecIni) THEN DO:
     FecIni = DATE(STRING(W_DiaIni) + "/" + STRING(MesIni) + "/" + STRING(AnoIni)).
  END.
  IF W_DiaFin NE DAY(FecFin) OR MesFin NE MONTH(FecFin) THEN DO:
     FecFin = DATE(STRING(W_DiaFin) + "/" + STRING(MesFin) + "/" + STRING(AnoFin)).
  END.
  VIEW FRAME F_Progreso.

  CASE Cmb_Tipo:
    WHEN "General"              THEN RUN Informe_General.
    WHEN "Por Usuario"          THEN RUN Informe_Usuario.
    WHEN "Por Linea de Cédito"  THEN RUN Informe_Lineas.
    WHEN "Por Tipo de Créditos" THEN RUN Informe_Tipos.
  END.

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
  DISPLAY Cmb_Tipo E1 
      WITH FRAME F_Basicos IN WINDOW wWin.
  ENABLE Cmb_Tipo BUTTON-143 BUTTON-153 BUTTON-144 Btn_Imp E1 w-BUTTON-148 
         BUTTON-150 BUTTON-152 
      WITH FRAME F_Basicos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Basicos}
  DISPLAY Cmb_Agencias W_DiaIni AnoIni W_DiaFin AnoFin Cmb_TProducto 
          Cmb_Productos 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE Cmb_Agencias BUTTON-120 BUTTON-121 BUTTON-145 Img_MesF Img_MesI 
         RECT-282 RECT-283 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_General wWin 
PROCEDURE Informe_General :
Listado = /*W_PathSpl +*/ "c:\info\Empresas.LST".  
OS-DELETE VALUE(Listado).
DEFINE VAR WEnc1      AS CHARACTER FORMAT "X(75)".
DEFINE VAR WEnc2      AS CHARACTER FORMAT "X(2)".
DEFINE VAR NomEmp     AS CHARACTER FORMAT "X(50)".
DEFINE VAR TotReg     AS DECIMAL.
DEFINE VAR MorCorte   AS DECIMAL FORMAT "->>9.999" EXTENT 9.
DEFINE VAR MorCompa AS DECIMAL FORMAT "->>9.999" EXTENT 9.
OUTPUT TO value(Listado) NO-ECHO PAGED PAGE-SIZE 65.
{Incluido\RepEncabezado.i}
  W_Reporte   = "REPORTE   : Empresas: " + Cmb_Tipo
              + " - Fec.Inicial: " + STRING(FecIni) + " - Fec.Final: " + STRING(FecFin) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "AGE    CARTERA AL DIA             CARTERA TIPO A         CARTERA TIPO B           CARTERA TIPO C          CARTERA TIPO D           CARTERA TIPO E           TOTAL MORA       MORO   TOTCRE   CART. TOTAL".
  VIEW FRAME F-Encabezado.
  ASSIGN j = 0. 

FOR EACH Desplazamiento  WHERE
         Desplazamiento.Fecha EQ FecIni NO-LOCK
         BREAK BY Desplazamiento.Agencia
               BY Desplazamiento.Cod_Califica:
    j = j + 1.
    RUN Progreso.
    FIND Corte WHERE
         Corte.Agencia      EQ Desplazamiento.Agencia AND
         Corte.Cod_Califica EQ Desplazamiento.Cod_Califica NO-ERROR.
    IF NOT AVAILABLE Corte THEN DO:
       CREATE Corte.
       ASSIGN Corte.Agencia      = Desplazamiento.Agencia
              Corte.Cod_Califica = Desplazamiento.Cod_Califica.
    END.
    ASSIGN Corte.Numero = Corte.Numero + 1
           Corte.Valor  = Corte.Valor  + Desplazamiento.Sdo_Capital.
END.

FOR EACH Desplazamiento WHERE
         Desplazamiento.Fecha EQ FecFin NO-LOCK
         BREAK BY Desplazamiento.Agencia
               BY Desplazamiento.Cod_Califica:
    j = j + 1.
    RUN Progreso.
    FIND Comparacion WHERE
         Comparacion.Agencia      EQ Desplazamiento.Agencia AND
         Comparacion.Cod_Califica EQ Desplazamiento.Cod_Califica NO-ERROR.
    IF NOT AVAILABLE Comparacion THEN DO:
       CREATE Comparacion.
       ASSIGN Comparacion.Agencia      = Desplazamiento.Agencia
              Comparacion.Cod_Califica = Desplazamiento.Cod_Califica.
    END.
    ASSIGN Comparacion.Numero = Comparacion.Numero + 1
           Comparacion.Valor  = Comparacion.Valor  + Desplazamiento.Sdo_Capital.
END.

FOR EACH Corte
    BREAK BY Corte.Agencia
          BY Corte.Cod_Califica:
    j = j + 1.
    RUN Progreso.
    FIND Comparacion WHERE 
         Comparacion.Agencia      EQ Corte.Agencia AND
         Comparacion.Cod_Califica EQ Corte.Cod_Califica NO-ERROR.
    IF AVAILABLE Comparacion THEN DO:
       CREATE Diferencia.
       ASSIGN Diferencia.Agencia      = Comparacion.Agencia
              Diferencia.Cod_Califica = Comparacion.Cod_Califica
              Diferencia.Numero       = Corte.Numero - Comparacion.Numero
              Diferencia.Valor        = Corte.Valor - Comparacion.Valor.
    END.
END.

DEFINE VAR W_LineaT       AS CHARACTER FORMAT "X(250)".
DEFINE VAR W_Columna      AS CHARACTER FORMAT "X(200)" INITIAL "Consolidado    ".
DEFINE VAR TE_Numero      AS DECIMAL FORMAT ">>>,>>9".
DEFINE VAR TE_Valor       AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TL_NumCartera  AS DECIMAL FORMAT ">>>,>>9".
DEFINE VAR TL_ValCartera  AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TL_ValCarteraA AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TL_NumCarteraA AS DECIMAL FORMAT ">>,>>9".
DEFINE VAR TL_ValCarteraM AS DECIMAL FORMAT ">>>,>>>,>>>,>>9".
DEFINE VAR TL_NumCarteraM AS DECIMAL FORMAT ">>>,>>9".
DEFINE VAR TL_Morosidad   AS DECIMAL FORMAT ">9.999".

DEFINE VAR TC_Numero      AS DECIMAL FORMAT ">>>,>>9" EXTENT 8.
DEFINE VAR TC_Valor       AS DECIMAL FORMAT ">>>,>>>,>>>,>>9" EXTENT 8.
DEFINE VAR TC_PorMor      AS DECIMAL FORMAT ">9.999".


FOR EACH Corte BREAK BY Corte.Agencia BY Corte.Cod_Califica:
   IF FIRST-OF(Corte.Agencia) THEN DO:
      FIND Agencias WHERE Agencias.Agencia EQ Corte.Agencia NO-LOCK NO-ERROR.
      IF AVAILABLE Agencias THEN W_Linea = STRING(Agencias.Nombre,"X(15)").
      IF Corte.Cod_Califica LE 5 THEN
         ASSIGN W_LineaT = W_LineaT + STRING(Corte.Agencia,"999")
                         + " " + STRING(Corte.Numero,">>,>>9")
                         + " " + STRING(Corte.Valor,">>>,>>>,>>>,>>9") + " ".
      ELSE
         ASSIGN TE_Valor  = TE_Valor + Corte.Valor
                TE_Numero = TE_Numero + Corte.Numero.
   END.
   ELSE DO:
       IF Corte.Cod_Califica LE 5 THEN
          ASSIGN W_LineaT = W_LineaT 
                          + " " + STRING(Corte.Numero,">>,>>9")
                          + " " + STRING(Corte.Valor,">>>,>>>,>>>,>>9") + " ".
       ELSE
          ASSIGN TE_Valor  = TE_Valor + Corte.Valor
                 TE_Numero = TE_Numero + Corte.Numero.
   END.
   ASSIGN TL_ValCartera = TL_ValCartera + Corte.Valor
          TL_NumCartera = TL_NumCartera + Corte.Numero.

   /*totales por columna*/
   IF Corte.Cod_Califica LE 5 THEN
      ASSIGN TC_Numero[Corte.Cod_Califica] = TC_Numero[Corte.Cod_Califica] + Corte.Numero
             TC_Valor[Corte.Cod_Califica]  = TC_Valor[Corte.Cod_Califica]  + Corte.Valor.
   ELSE
      IF Corte.Cod_Califica GT 5 THEN
       ASSIGN TC_Numero[6] = TC_Numero[6] + Corte.Numero
              TC_Valor[6]  = TC_Valor[6]  + Corte.Valor.
   
   /*MESSAGE Corte.Cod_Califica Corte.Agencia Corte.Numero Corte.Valor VIEW-AS ALERT-BOX.       */
   IF Corte.Cod_Califica LE 2 THEN 
      ASSIGN TL_ValCarteraA = TL_ValCarteraA + Corte.Valor
             TL_NumCarteraA = TL_NumCarteraA + Corte.Numero.
   ELSE 
      ASSIGN TL_ValCarteraM = TL_ValCarteraM + Corte.Valor
             TL_NumCarteraM = TL_NumCarteraM + Corte.Numero.
   IF LAST-OF(Corte.Agencia) THEN DO:
       TL_Morosidad = ((TL_ValCartera - TL_ValCarteraA) / TL_ValCartera) * 100.
       MorCorte[Corte.Agencia] = TL_Morosidad.
       W_LineaT = W_LineaT + " " + STRING(TE_Numero,">>>,>>9")
                           + " " + STRING(TE_Valor,">>>,>>>,>>>,>>9")
                           + " " + STRING(TL_NumCarteraM,">>>,>>9")
                           + " " + STRING(TL_ValCarteraM,">>>,>>>,>>>,>>9")
                           + " " + STRING(TL_Morosidad,">9.999") 
                           + " " + STRING(TL_NumCartera,">>>,>>9")
                           + " " + STRING(TL_ValCartera,">>>,>>>,>>>,>>9").
       ASSIGN TC_Numero[7] = TC_Numero[7] + TL_NumCarteraM
              TC_Valor[7]  = TC_Valor[7]  + TL_ValCarteraM
              TC_PorMor    = TC_PorMor    + TL_Morosidad.
       ASSIGN TC_Numero[8] = TC_Numero[8] + TL_NumCartera
              TC_Valor[8]  = TC_Valor[8]  + TL_ValCartera.
       DISPLAY W_LineaT WITH FRAME FCorte WIDTH 270 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
       ASSIGN W_LineaT = "" TE_Numero = 0 TE_Valor = 0 TL_NumCarteraM = 0 TL_ValCarteraM = 0
              TL_Morosidad = 0 TL_NumCartera = 0 TL_ValCartera = 0 TL_ValCarteraA = 0.
   END.
END.
DISPLAY TC_Numero[1] AT 4 FORMAT ">>>,>>9"
        TC_Valor[1]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[2]      FORMAT ">>>,>>9"         
        TC_Valor[2]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[3]      FORMAT ">>>,>>9"         
        TC_Valor[3]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[4]      FORMAT ">>>,>>9"         
        TC_Valor[4]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[5]      FORMAT ">>>>,>>9"         
        TC_Valor[5]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[6]      FORMAT ">>>,>>9"         
        TC_Valor[6]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[7]      FORMAT ">>>,>>9"         
        TC_Valor[7]       FORMAT ">>>,>>>,>>>,>>9"
        TC_PorMor
        TC_Numero[8] AT 180 FORMAT ">>>,>>9"         
        TC_Valor[8]       FORMAT ">>>,>>>,>>>,>>9"
        WITH FRAME TotCor WIDTH 270 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.

/*comparacion*/
ASSIGN W_LineaT = "" TE_Valor = 0 TE_Numero = 0
       TL_ValCartera = 0 TL_NumCartera = 0
       TC_Numero     = 0 TC_Valor = 0
       TL_ValCarteraA = 0 TL_NumCarteraA = 0
       TL_ValCarteraM = 0 TL_NumCarteraM = 0
       TL_Morosidad = 0 TC_PorMor = 0.

DISPLAY SKIP(1) "Fecha de Comparacion : " FecFin  SKIP(1) WITH FRAME FEspacio NO-LABELS.
FOR EACH Comparacion BREAK BY Comparacion.Agencia BY Comparacion.Cod_Califica:
   IF FIRST-OF(Comparacion.Agencia) THEN DO:
      FIND Agencias WHERE Agencias.Agencia EQ Comparacion.Agencia NO-LOCK NO-ERROR.
      IF AVAILABLE Agencias THEN W_Linea = STRING(Agencias.Nombre,"X(15)").
      IF Comparacion.Cod_Califica LE 5 THEN
         ASSIGN W_LineaT = W_LineaT + STRING(Comparacion.Agencia,"999")
                         + " " + STRING(Comparacion.Numero,">>,>>9")
                         + " " + STRING(Comparacion.Valor,">>>,>>>,>>>,>>9") + " ".
      ELSE
         ASSIGN TE_Valor  = TE_Valor + Comparacion.Valor
                TE_Numero = TE_Numero + Comparacion.Numero.
   END.
   ELSE DO:
       IF Comparacion.Cod_Califica LE 5 THEN
          ASSIGN W_LineaT = W_LineaT 
                          + " " + STRING(Comparacion.Numero,">>,>>9")
                          + " " + STRING(Comparacion.Valor,">>>,>>>,>>>,>>9") + " ".
       ELSE
          ASSIGN TE_Valor  = TE_Valor + Comparacion.Valor
                 TE_Numero = TE_Numero + Comparacion.Numero.
   END.
   ASSIGN TL_ValCartera = TL_ValCartera + Comparacion.Valor
          TL_NumCartera = TL_NumCartera + Comparacion.Numero.

   /*totales por columna*/
   IF Comparacion.Cod_Califica LE 5 THEN
      ASSIGN TC_Numero[Comparacion.Cod_Califica] = TC_Numero[Comparacion.Cod_Califica] + Comparacion.Numero
             TC_Valor[Comparacion.Cod_Califica]  = TC_Valor[Comparacion.Cod_Califica]  + Comparacion.Valor.
   ELSE
      IF Comparacion.Cod_Califica GT 5 THEN
       ASSIGN TC_Numero[6] = TC_Numero[6] + Comparacion.Numero
              TC_Valor[6]  = TC_Valor[6]  + Comparacion.Valor.
   
   /*MESSAGE Corte.Cod_Califica Corte.Agencia Corte.Numero Corte.Valor VIEW-AS ALERT-BOX.       */
   IF Comparacion.Cod_Califica LE 2 THEN 
      ASSIGN TL_ValCarteraA = TL_ValCarteraA + Comparacion.Valor
             TL_NumCarteraA = TL_NumCarteraA + Comparacion.Numero.
   ELSE 
      ASSIGN TL_ValCarteraM = TL_ValCarteraM + Comparacion.Valor
             TL_NumCarteraM = TL_NumCarteraM + Comparacion.Numero.
   IF LAST-OF(Comparacion.Agencia) THEN DO:
       TL_Morosidad = ((TL_ValCartera - TL_ValCarteraA) / TL_ValCartera) * 100.
       MorCompa[Comparacion.Agencia] = TL_Morosidad.
       W_LineaT = W_LineaT + " " + STRING(TE_Numero,">>>,>>9")
                           + " " + STRING(TE_Valor,">>>,>>>,>>>,>>9")
                           + " " + STRING(TL_NumCarteraM,">>>,>>9")
                           + " " + STRING(TL_ValCarteraM,">>>,>>>,>>>,>>9")
                           + " " + STRING(TL_Morosidad,">9.999") 
                           + " " + STRING(TL_NumCartera,">>>,>>9")
                           + " " + STRING(TL_ValCartera,">>>,>>>,>>>,>>9").
       ASSIGN TC_Numero[7] = TC_Numero[7] + TL_NumCarteraM
              TC_Valor[7]  = TC_Valor[7]  + TL_ValCarteraM
              TC_PorMor    = TC_PorMor    + TL_Morosidad.
       ASSIGN TC_Numero[8] = TC_Numero[8] + TL_NumCartera
              TC_Valor[8]  = TC_Valor[8]  + TL_ValCartera.
       DISPLAY W_LineaT WITH FRAME FComparacion WIDTH 270 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
       ASSIGN W_LineaT = "" TE_Numero = 0 TE_Valor = 0 TL_NumCarteraM = 0 TL_ValCarteraM = 0
              TL_Morosidad = 0 TL_NumCartera = 0 TL_ValCartera = 0 TL_ValCarteraA = 0.
   END.
END.
DISPLAY TC_Numero[1] AT 4 FORMAT ">>>,>>9"
        TC_Valor[1]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[2]      FORMAT ">>>,>>9"         
        TC_Valor[2]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[3]      FORMAT ">>>,>>9"         
        TC_Valor[3]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[4]      FORMAT ">>>,>>9"         
        TC_Valor[4]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[5]      FORMAT ">>>>,>>9"         
        TC_Valor[5]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[6]      FORMAT ">>>,>>9"         
        TC_Valor[6]       FORMAT ">>>,>>>,>>>,>>9"
        TC_Numero[7]      FORMAT ">>>,>>9"         
        TC_Valor[7]       FORMAT ">>>,>>>,>>>,>>9"
        TC_PorMor         FORMAT ">>9.99999"
        TC_Numero[8] AT 180 FORMAT ">>>,>>9"         
        TC_Valor[8]       FORMAT ">>>,>>>,>>>,>>9"
        WITH FRAME TotCom WIDTH 270 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.

/*diferencia*/
ASSIGN W_LineaT = "" TE_Valor = 0 TE_Numero = 0
       TL_ValCartera = 0 TL_NumCartera = 0
       TC_Numero     = 0 TC_Valor = 0
       TL_ValCarteraA = 0 TL_NumCarteraA = 0
       TL_ValCarteraM = 0 TL_NumCarteraM = 0
       TL_Morosidad = 0 TC_PorMor = 0.

DISPLAY SKIP(1) "Diferencia" SKIP(1)
    "AGE         CARTERA AL DIA             CARTERA TIPO A              CARTERA TIPO B             CARTERA TIPO C             CARTERA TIPO D             CARTERA TIPO E            TOTAL MORA          MORO     TOTCRE     CART. TOTAL" 
     WITH WIDTH 270 FRAME FDiferenciaE NO-LABELS.
FOR EACH Diferencia BREAK BY Diferencia.Agencia BY Diferencia.Cod_Califica:
   IF FIRST-OF(Diferencia.Agencia) THEN DO:
      FIND Agencias WHERE Agencias.Agencia EQ Diferencia.Agencia NO-LOCK NO-ERROR.
      IF AVAILABLE Agencias THEN W_Linea = STRING(Agencias.Nombre,"X(15)").
      IF Diferencia.Cod_Califica LE 5 THEN
         ASSIGN W_LineaT = W_LineaT + STRING(Diferencia.Agencia,"999")
                         + " " + STRING(Diferencia.Numero,"->>>,>>9")
                         + " " + STRING(Diferencia.Valor,"->>>,>>>,>>>,>>9") + " ".
      ELSE
         ASSIGN TE_Valor  = TE_Valor + Diferencia.Valor
                TE_Numero = TE_Numero + Diferencia.Numero.
   END.
   ELSE DO:
       IF Diferencia.Cod_Califica LE 5 THEN
          ASSIGN W_LineaT = W_LineaT 
                          + " " + STRING(Diferencia.Numero,"->>>,>>9")
                          + " " + STRING(Diferencia.Valor,"->>>,>>>,>>>,>>9") + " ".
       ELSE
          ASSIGN TE_Valor  = TE_Valor + Diferencia.Valor
                 TE_Numero = TE_Numero + Diferencia.Numero.
   END.
   ASSIGN TL_ValCartera = TL_ValCartera + Diferencia.Valor
          TL_NumCartera = TL_NumCartera + Diferencia.Numero.

   /*totales por columna*/
   IF Diferencia.Cod_Califica LE 5 THEN
      ASSIGN TC_Numero[Diferencia.Cod_Califica] = TC_Numero[Diferencia.Cod_Califica] + Diferencia.Numero
             TC_Valor[Diferencia.Cod_Califica]  = TC_Valor[Diferencia.Cod_Califica]  + Diferencia.Valor.
   ELSE
      IF Diferencia.Cod_Califica GT 5 THEN
       ASSIGN TC_Numero[6] = TC_Numero[6] + Diferencia.Numero
              TC_Valor[6]  = TC_Valor[6]  + Diferencia.Valor.
   
   /*MESSAGE Corte.Cod_Califica Corte.Agencia Corte.Numero Corte.Valor VIEW-AS ALERT-BOX.       */
   IF Diferencia.Cod_Califica LE 2 THEN 
      ASSIGN TL_ValCarteraA = TL_ValCarteraA + Diferencia.Valor
             TL_NumCarteraA = TL_NumCarteraA + Diferencia.Numero.
   ELSE 
      ASSIGN TL_ValCarteraM = TL_ValCarteraM + Diferencia.Valor
             TL_NumCarteraM = TL_NumCarteraM + Diferencia.Numero.
   IF LAST-OF(Diferencia.Agencia) THEN DO:
       /*MESSAGE MorCorte[Diferencia.Agencia] MorCompa[Diferencia.Agencia] VIEW-AS ALERT-BOX.*/
       TL_Morosidad = MorCorte[Diferencia.Agencia] - MorCompa[Diferencia.Agencia].
       W_LineaT = W_LineaT + " " + STRING(TE_Numero,"->>>,>>9")
                           + " " + STRING(TE_Valor,"->>>,>>>,>>>,>>9")
                           + " " + STRING(TL_NumCarteraM,"->>>,>>9")
                           + " " + STRING(TL_ValCarteraM,"->>>,>>>,>>>,>>9")
                           + " " + STRING(TL_Morosidad,"->>9.999") 
                           + " " + STRING(TL_NumCartera,"->>>,>>9")
                           + " " + STRING(TL_ValCartera,"->>>,>>>,>>>,>>9").
       ASSIGN TC_Numero[7] = TC_Numero[7] + TL_NumCarteraM
              TC_Valor[7]  = TC_Valor[7]  + TL_ValCarteraM
              TC_PorMor    = TC_PorMor    + TL_Morosidad.
       ASSIGN TC_Numero[8] = TC_Numero[8] + TL_NumCartera
              TC_Valor[8]  = TC_Valor[8]  + TL_ValCartera.
       DISPLAY W_LineaT WITH FRAME Fdiferencia2 WIDTH 270 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
       ASSIGN W_LineaT = "" TE_Numero = 0 TE_Valor = 0 TL_NumCarteraM = 0 TL_ValCarteraM = 0
              TL_Morosidad = 0 TL_NumCartera = 0 TL_ValCartera = 0 TL_ValCarteraA = 0.
   END.
END.

DISPLAY TC_Numero[1] AT 5 FORMAT "->>>,>>9"
        TC_Valor[1]       FORMAT "->>>,>>>,>>>,>>9"
        TC_Numero[2]      FORMAT "->>>>,>>9"         
        TC_Valor[2]       FORMAT "->>>,>>>,>>>,>>9"
        TC_Numero[3]      FORMAT "->>>>,>>9"         
        TC_Valor[3]       FORMAT "->>>,>>>,>>>,>>9"
        TC_Numero[4]      FORMAT "->>>>,>>9"         
        TC_Valor[4]       FORMAT "->>>,>>>,>>>,>>9"
        TC_Numero[5]      FORMAT "->>>>,>>9"         
        TC_Valor[5]       FORMAT "->>>,>>>,>>>,>>9"
        TC_Numero[6]      FORMAT "->>>>,>>9"         
        TC_Valor[6]       FORMAT "->>>,>>>,>>>,>>9"
        TC_Numero[7]      FORMAT "->>>,>>9"         
        TC_Valor[7]       FORMAT "->>>,>>>,>>>,>>9"
        TC_PorMor         FORMAT "->>9.999"
        TC_Numero[8]      FORMAT "->>>,>>9"         
        TC_Valor[8]       FORMAT "->>>,>>>,>>>,>>9"
        WITH FRAME TotDif WIDTH 270 NO-LABELS USE-TEXT NO-BOX NO-UNDERLINE STREAM-IO.


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
  END.
  W_DiaFin = DAY(TODAY).
  RUN SUPER.
  DO WITH FRAME F_Filtros:
    ASSIGN W_OK = Img_MesI:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
           W_OK = Img_MesF:LOAD-IMAGE("imagenes\month" + STRING(MONTH(TODAY)) + ".gif")
           AnoIni:SCREEN-VALUE = STRING(YEAR(TODAY))
           AnoFin:SCREEN-VALUE = STRING(YEAR(TODAY))
           MesIni = MONTH(TODAY)
           MesFin = MONTH(TODAY)
           AgeIni = W_Agencia
           AgeFin = W_Agencia
           FecIni = TODAY
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
          WHEN 1 THEN R1:BGCOL = 18.
          WHEN 2 THEN R2:BGCOL = 18.
          WHEN 3 THEN R3:BGCOL = 18.
          WHEN 4 THEN R4:BGCOL = 18.
          WHEN 5 THEN R5:BGCOL = 18.
          WHEN 6 THEN R6:BGCOL = 18.
          WHEN 7 THEN R7:BGCOL = 18.
          WHEN 8 THEN R8:BGCOL = 18.
          WHEN 9 THEN R9:BGCOL = 18.
        END CASE.
        IF k = 9 THEN k = 0.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

