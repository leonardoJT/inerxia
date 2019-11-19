&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
{adecomm/appserv.i}
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}
{incluido\VARIABLE.i "SHARED"}

DEFINE VAR Hora1 AS INTEGER NO-UNDO.
DEFINE VAR Hora2 AS INTEGER NO-UNDO.
ASSIGN Hora1 = TIME.
DEFINE VARIABLE i AS INTEGER NO-UNDO. /*Agencias*/

DEFINE TEMP-TABLE TTCtasInc NO-UNDO
    FIELD Agencia   AS INTEGER
    FIELD Nit       AS CHARACTER
    FIELD Cuenta    AS CHARACTER
    FIELD Fecha     AS DATE
    FIELD Documento AS CHARACTER
    FIELD DB        AS DECIMAL
    FIELD CR        AS DECIMAL
    FIELD Usuario   AS CHARACTER
    FIELD Estado    AS CHARACTER.

DEFINE VARIABLE viAno AS INTEGER NO-UNDO.
DEFINE VARIABLE viMes AS INTEGER NO-UNDO.
DEFINE VARIABLE viAgeIni AS INTEGER NO-UNDO.
DEFINE VARIABLE viAgeFin AS INTEGER NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS TG-1 BTN-Info BT-Imprimir BTN-Cancelar ~
BTN-Salir BTN-Help TG-2 TG-3 TG-4 TG-6 TG-7 RECT-1 RECT-274 RECT-321 ~
RECT-322 RECT-323 RECT-324 
&Scoped-Define DISPLAYED-OBJECTS RS-3 F-CTA-1 RS-1 TG-1 CB-Tipo TG-2 TG-3 ~
TG-4 TG-6 TG-7 F-ANO 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 TG-1 TG-2 TG-3 TG-4 TG-6 TG-7 
&Scoped-define List-2 F-CTA-1 RS-1 
&Scoped-define List-3 F-CTA-1 RS-1 
&Scoped-define List-4 RS-3 F-ANO 
&Scoped-define List-6 BTN-Info Btn-Consulta BTN-Cancelar BTN-Help 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BT-Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 7 BY 1.65 TOOLTIP "Listar - Procesar".

DEFINE BUTTON BTN-Cancelar 
     IMAGE-UP FILE "imagenes/borrar.bmp":U
     LABEL "&Cancelar" 
     SIZE 7 BY 1.65 TOOLTIP "Cancelar"
     FONT 4.

DEFINE BUTTON Btn-Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Consultar" 
     SIZE 7 BY 1.65 TOOLTIP "Buscar".

DEFINE BUTTON BTN-Help 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Ayuda" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BTN-Info 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Info" 
     SIZE 7 BY 1.65 TOOLTIP "Información".

DEFINE BUTTON BTN-Salir DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 7 BY 1.65 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON BTN-Titulo 
     LABEL "" 
     SIZE 53.72 BY 1.12.

DEFINE VARIABLE CB-Tipo AS INTEGER FORMAT ">>9":U INITIAL 2 
     LABEL "Tipo Cuenta" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEM-PAIRS "1- Mayor",1,
                     "2- Movimiento",2,
                     "3- Todas",3
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-ANO AS INTEGER FORMAT ">>>>":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-CTA-1 AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cuenta" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RS-1 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Cuentas que empiezan por", 1,
"Cuenta específica", 2
     SIZE 20.72 BY 1.62 NO-UNDO.

DEFINE VARIABLE RS-3 AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Saldos", 1,
"Movimiento detallado", 2
     SIZE 25.72 BY 1.08 NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 86.86 BY 14.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 9 BY 11.58.

DEFINE RECTANGLE RECT-321
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 4.31.

DEFINE RECTANGLE RECT-322
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 4.31.

DEFINE RECTANGLE RECT-323
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 5.69.

DEFINE RECTANGLE RECT-324
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 46 BY 5.69.

DEFINE VARIABLE TG-1 AS LOGICAL INITIAL no 
     LABEL "Todo" 
     VIEW-AS TOGGLE-BOX
     SIZE 7 BY .81 NO-UNDO.

DEFINE VARIABLE TG-2 AS LOGICAL INITIAL no 
     LABEL "Cuentas Sin Homologar" 
     VIEW-AS TOGGLE-BOX
     SIZE 19 BY .81 NO-UNDO.

DEFINE VARIABLE TG-3 AS LOGICAL INITIAL no 
     LABEL "Cuentas Homologas Inexistentes" 
     VIEW-AS TOGGLE-BOX
     SIZE 25 BY .81 NO-UNDO.

DEFINE VARIABLE TG-4 AS LOGICAL INITIAL no 
     LABEL "Verificación Naturaleza Cuentas PUC" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE TG-6 AS LOGICAL INITIAL no 
     LABEL "Cuentas de Mayor con movimiento" 
     VIEW-AS TOGGLE-BOX
     SIZE 28 BY .81 NO-UNDO.

DEFINE VARIABLE TG-7 AS LOGICAL INITIAL no 
     LABEL "Movimiento cuentas empiezan por..." 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .81 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RS-3 AT ROW 11.77 COL 42.29 NO-LABEL WIDGET-ID 174
     F-CTA-1 AT ROW 9.77 COL 70.72 COLON-ALIGNED WIDGET-ID 152
     RS-1 AT ROW 9.35 COL 42 NO-LABEL WIDGET-ID 164
     TG-1 AT ROW 3.96 COL 6 WIDGET-ID 130
     CB-Tipo AT ROW 3.96 COL 49 COLON-ALIGNED WIDGET-ID 116
     BTN-Titulo AT ROW 1.27 COL 21 WIDGET-ID 68
     BTN-Info AT ROW 1.81 COL 91 WIDGET-ID 58
     Btn-Consulta AT ROW 3.96 COL 91 WIDGET-ID 54
     BT-Imprimir AT ROW 6.38 COL 91 WIDGET-ID 56
     BTN-Cancelar AT ROW 8.81 COL 91 WIDGET-ID 52
     BTN-Salir AT ROW 11.23 COL 91 WIDGET-ID 50
     BTN-Help AT ROW 14.46 COL 92 WIDGET-ID 60
     TG-2 AT ROW 5.31 COL 6 WIDGET-ID 132
     TG-3 AT ROW 6.65 COL 6 WIDGET-ID 134
     TG-4 AT ROW 9.08 COL 5 WIDGET-ID 136
     TG-6 AT ROW 12.23 COL 5 WIDGET-ID 144
     TG-7 AT ROW 10.69 COL 5 WIDGET-ID 172
     F-ANO AT ROW 11.77 COL 77 COLON-ALIGNED WIDGET-ID 178
     "Reportes PUC" VIEW-AS TEXT
          SIZE 13.43 BY .81 AT ROW 1.42 COL 41 WIDGET-ID 74
          BGCOLOR 11 FONT 1
     "Reportes Homologación cuentas" VIEW-AS TEXT
          SIZE 30 BY .81 AT ROW 3.12 COL 7 WIDGET-ID 110
          FONT 1
     "Filtros" VIEW-AS TEXT
          SIZE 7 BY .81 AT ROW 3.12 COL 59.57 WIDGET-ID 112
          FONT 1
     "Reportes PUC" VIEW-AS TEXT
          SIZE 13.43 BY .81 AT ROW 7.85 COL 14.86 WIDGET-ID 122
          FONT 1
     "Filtros" VIEW-AS TEXT
          SIZE 7 BY .81 AT ROW 7.88 COL 59 WIDGET-ID 150
          FONT 1
     RECT-1 AT ROW 2.62 COL 1.14 WIDGET-ID 2
     RECT-274 AT ROW 1.54 COL 90 WIDGET-ID 66
     RECT-321 AT ROW 3.42 COL 40.43 WIDGET-ID 106
     RECT-322 AT ROW 3.42 COL 3 WIDGET-ID 108
     RECT-323 AT ROW 8.23 COL 3 WIDGET-ID 128
     RECT-324 AT ROW 8.23 COL 40.43 WIDGET-ID 148
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS THREE-D 
         AT COL 1 ROW 1
         SIZE 98 BY 15.73
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Reporte Homologación Cuentas - wRP-Homol_ctas"
         HEIGHT             = 15.81
         WIDTH              = 97.86
         MAX-HEIGHT         = 38.23
         MAX-WIDTH          = 182.86
         VIRTUAL-HEIGHT     = 38.23
         VIRTUAL-WIDTH      = 182.86
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
/* SETTINGS FOR FRAME fMain
   FRAME-NAME UNDERLINE Custom                                          */
/* SETTINGS FOR BUTTON BTN-Cancelar IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON Btn-Consulta IN FRAME fMain
   NO-ENABLE 6                                                          */
/* SETTINGS FOR BUTTON BTN-Help IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON BTN-Info IN FRAME fMain
   6                                                                    */
/* SETTINGS FOR BUTTON BTN-Titulo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX CB-Tipo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-ANO IN FRAME fMain
   NO-ENABLE 4                                                          */
/* SETTINGS FOR FILL-IN F-CTA-1 IN FRAME fMain
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR RADIO-SET RS-1 IN FRAME fMain
   NO-ENABLE 2 3                                                        */
/* SETTINGS FOR RADIO-SET RS-3 IN FRAME fMain
   NO-ENABLE 4                                                          */
/* SETTINGS FOR TOGGLE-BOX TG-1 IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX TG-2 IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX TG-3 IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX TG-4 IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX TG-6 IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR TOGGLE-BOX TG-7 IN FRAME fMain
   1                                                                    */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Reporte Homologación Cuentas - wRP-Homol_ctas */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Reporte Homologación Cuentas - wRP-Homol_ctas */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BT-Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BT-Imprimir wWin
ON CHOOSE OF BT-Imprimir IN FRAME fMain /* Imprimir */
DO:
    DEFINE VARIABLE vcCodProd AS CHARACTER   NO-UNDO. /* Codigo del producto*/
    DEFINE VARIABLE vcXprograma AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vc_Titulo AS CHARACTER FORMAT "X(60)" NO-UNDO.
    DEFINE VARIABLE vc_Opc    AS CHARACTER FORMAT "X"     NO-UNDO.
    DEFINE VARIABLE vc_cta    LIKE cuentas.cuenta NO-UNDO.

    ASSIGN CB-Tipo.
    CASE CB-Tipo:
      WHEN 1 THEN DO:
        vcCodProd = "1".
      END.
      WHEN 2 THEN DO: 
        vcCodProd = "2".
      END.
      OTHERWISE DO:
        vcCodProd = "3".
      END.
    END CASE.

    IF TG-1 = TRUE THEN DO:
      ASSIGN vcXprograma = 'pHomolCtaTodo.p'. /*Envia parametro de tipo de cuenta*/
      RUN Conecta_multi.p.
      RUN wimprime.w (vcXprograma, "Homologación Cuentas",
                      vcCodProd, "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      ?, ?,
                      ?, ?,
                      ?, ?,
                      ?, ?,
                      ?, ?,  /* 4 Campos Lógicos*/
                      ?, ?).
      RUN Desconecta_multi.p.
    END.
    
    IF TG-2 = TRUE THEN DO:
      ASSIGN vcXprograma = 'pHomolCtaSin.p'.
      RUN Conecta_multi.p.
      RUN wimprime.w (vcXprograma, "Sin Cuenta Homologa",
                      vcCodProd, "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      ?, ?,
                      ?, ?,
                      ?, ?,
                      ?, ?,
                      ?, ?,  /* 4 Campos Lógicos*/
                      ?, ?).
      RUN Desconecta_multi.p.
    END.

    IF TG-3 = TRUE THEN DO:
      ASSIGN vcXprograma = 'pHomolCtaInexiste.p'.
      RUN Conecta_multi.p.
      RUN wimprime.w (vcXprograma, "Cuenta Homologa NO Existe",
                      vcCodProd, "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      ?, ?,
                      ?, ?,
                      ?, ?,
                      ?, ?,
                      ?, ?,  /* 4 Campos Lógicos*/
                      ?, ?).
      RUN Desconecta_multi.p.
    END.
    
        
    IF TG-4 = TRUE THEN DO:
      ASSIGN vcXprograma = 'pNaturErrPUC.p'.
      RUN wimprime.w (vcXprograma, "Cuenta Naturaleza Errada",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      ?, ?,
                      ?, ?,
                      ?, ?,
                      ?, ?,
                      ?, ?,  /* 4 Campos Lógicos*/
                      ?, ?).
    END.

    IF TG-6 = TRUE THEN DO:
      ASSIGN vcXprograma = 'pCtasMayorEnSaldos.p'
             vc_titulo   = IF RS-3 = 1 THEN " Saldos cuentas Mayor con movimiento " + F-ANO:SCREEN-VALUE IN FRAME fmain
                           ELSE " Detalle cuentas Mayor con movimiento " + F-ANO:SCREEN-VALUE IN FRAME fmain
             vc_Opc      = STRING(RS-3). /* 1 = Saldos, 2 = Movimiento */  
      RUN wimprime.w (vcXprograma, vc_titulo,
                      vc_opc, F-ANO:SCREEN-VALUE IN FRAME fmain,
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      "", "",
                      ?, ?,
                      ?, ?,
                      ?, ?,
                      ?, ?,
                      ?, ?,  /* 4 Campos Lógicos*/
                      ?, ?).
    END.
        
    IF TG-7 = TRUE THEN DO:
      ASSIGN vcXprograma = 'pMvtoCtasMayor.p'
             vc_titulo   = IF RS-1 = 1 THEN "Movimiento Cuentas que empiezan por " + F-CTA-1:SCREEN-VALUE IN FRAME fmain
                           ELSE "Movimiento Cuenta " + F-CTA-1:SCREEN-VALUE IN FRAME fmain
             vc_cta      = F-CTA-1:SCREEN-VALUE IN FRAME fmain
             vc_Opc      = STRING(RS-3). /* 1 = por begins, 2 = Igual */
      IF F-CTA-1:SCREEN-VALUE IN FRAME fmain  <> '' THEN DO:
        RUN wimprime.w (vcXprograma, vc_Titulo,
                        vc_Opc, vc_cta,
                        "", "",
                        "", "",
                        "", "",
                        "", "",
                        "", "",
                        "", "",
                        "", "",
                        "", "",
                        "", "",
                        ?, ?,
                        ?, ?,
                        ?, ?,
                        ?, ?,
                        ?, ?,  /* 4 Campos Lógicos*/
                        ?, ?).
        
      END.
      ELSE DO:
        MESSAGE "Por favor digite cuenta"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "ENTRY" TO F-CTA-1 IN FRAME fmain.
        RETURN NO-APPLY.
      END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Info
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Info wWin
ON CHOOSE OF BTN-Info IN FRAME fMain /* Info */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Salir wWin
ON CHOOSE OF BTN-Salir IN FRAME fMain
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


&Scoped-define SELF-NAME CB-Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CB-Tipo wWin
ON VALUE-CHANGED OF CB-Tipo IN FRAME fMain /* Tipo Cuenta */
DO:
  ASSIGN  CB-Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-ANO
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-ANO wWin
ON LEAVE OF F-ANO IN FRAME fMain /* Año */
DO:
  ASSIGN F-ANO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-CTA-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-CTA-1 wWin
ON LEAVE OF F-CTA-1 IN FRAME fMain /* Cuenta */
DO:
  ASSIGN F-CTA-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-1 wWin
ON VALUE-CHANGED OF RS-1 IN FRAME fMain
DO:
  ASSIGN RS-1.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-3 wWin
ON VALUE-CHANGED OF RS-3 IN FRAME fMain
DO:
  ASSIGN RS-3.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-1 wWin
ON VALUE-CHANGED OF TG-1 IN FRAME fMain /* Todo */
DO:
  ASSIGN TG-1 = FALSE
         TG-2 = FALSE
         TG-3 = FALSE
         TG-4 = FALSE
         TG-6 = FALSE
         TG-7 = FALSE.
  ASSIGN TG-1.
  DISPLAY {&List-1} WITH FRAME fmain.
  DISABLE {&List-2} WITH FRAME fmain.
  DISABLE {&List-4} WITH FRAME fmain.
  ENABLE CB-Tipo WITH FRAME fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-2 wWin
ON VALUE-CHANGED OF TG-2 IN FRAME fMain /* Cuentas Sin Homologar */
DO:
  ASSIGN TG-1 = FALSE
         TG-2 = FALSE
         TG-3 = FALSE
         TG-4 = FALSE
         TG-6 = FALSE
         TG-7 = FALSE.
  ASSIGN TG-2.
  DISPLAY {&List-1} WITH FRAME fmain.
  DISABLE {&List-2} WITH FRAME fmain.
  DISABLE {&List-4} WITH FRAME fmain.
  ENABLE CB-Tipo WITH FRAME fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-3 wWin
ON VALUE-CHANGED OF TG-3 IN FRAME fMain /* Cuentas Homologas Inexistentes */
DO:
  ASSIGN TG-1 = FALSE
         TG-2 = FALSE
         TG-3 = FALSE
         TG-4 = FALSE
         TG-6 = FALSE
         TG-7 = FALSE.
  ASSIGN TG-3.
  DISPLAY {&List-1} WITH FRAME fmain.
  DISABLE {&List-2} WITH FRAME fmain.
  DISABLE {&List-4} WITH FRAME fmain.
  ENABLE CB-Tipo WITH FRAME fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-4 wWin
ON VALUE-CHANGED OF TG-4 IN FRAME fMain /* Verificación Naturaleza Cuentas PUC */
DO:
  ASSIGN TG-1 = FALSE
         TG-2 = FALSE
         TG-3 = FALSE
         TG-4 = FALSE
         TG-6 = FALSE
         TG-7 = FALSE.
  ASSIGN TG-4.
  DISPLAY {&List-1} WITH FRAME fmain.
  DISABLE CB-Tipo WITH FRAME fmain.
  DISABLE {&List-2} WITH FRAME fmain.
  DISABLE {&List-4} WITH FRAME fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-6
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-6 wWin
ON VALUE-CHANGED OF TG-6 IN FRAME fMain /* Cuentas de Mayor con movimiento */
DO:
  ASSIGN TG-1 = FALSE
         TG-2 = FALSE
         TG-3 = FALSE
         TG-4 = FALSE
         TG-6 = FALSE
         TG-7 = FALSE.
  ASSIGN TG-6.
  DISPLAY {&List-1} WITH FRAME fmain.
  DISABLE CB-Tipo WITH FRAME fmain.
  DISABLE {&List-2} WITH FRAME fmain.
  IF TG-6 = TRUE THEN
    ENABLE {&List-4} WITH FRAME fmain.
  ELSE
    DISABLE {&List-4} WITH FRAME fmain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TG-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TG-7 wWin
ON VALUE-CHANGED OF TG-7 IN FRAME fMain /* Movimiento cuentas empiezan por... */
DO:
  ASSIGN TG-1 = FALSE
         TG-2 = FALSE
         TG-3 = FALSE
         TG-4 = FALSE
         TG-6 = FALSE
         TG-7 = FALSE.
  ASSIGN TG-7.
  DISPLAY {&List-1} WITH FRAME fmain.
  DISABLE CB-Tipo WITH FRAME fmain.
  DISABLE {&List-2} WITH FRAME fmain.
  ENABLE  {&List-3} WITH FRAME fmain.
  DISABLE {&List-4} WITH FRAME fmain.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cargaProductos wWin 
PROCEDURE cargaProductos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

    /*CB-Tipo:DELETE(1) IN FRAME fMain.
    FOR EACH pro_ahorros NO-LOCK:
        CB-Tipo:ADD-LAST(pro_ahorros.Nom_Producto,INTEGER(pro_ahorros.cod_ahorro)).
    END.*/
/*     CB-Tipo:ADD-FIRST("Todos",0). */

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
  DISPLAY RS-3 F-CTA-1 RS-1 TG-1 CB-Tipo TG-2 TG-3 TG-4 TG-6 TG-7 F-ANO 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE TG-1 BTN-Info BT-Imprimir BTN-Cancelar BTN-Salir BTN-Help TG-2 TG-3 
         TG-4 TG-6 TG-7 RECT-1 RECT-274 RECT-321 RECT-322 RECT-323 RECT-324 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
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
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.
  ASSIGN F-ANO = YEAR(TODAY).
  DISPLAY F-ANO WITH FRAME fmain.
    
    
  /* Code placed here will execute AFTER standard behavior.    */

  /*     RUN cargaProductos. */
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

