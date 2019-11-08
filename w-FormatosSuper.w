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
{incluido/Variable.i "SHARED"}.

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
&Scoped-Define ENABLED-OBJECTS RECT-290 SDiario SSmnal BUTTON-166 SSmnal15 ~
SSmnal31 Btn_Imp SBsmnal STrimesMnsual Btn_Ejecutar SMnsual STrimestral ~
BUTTON-14 SMnsualSmstral SSmstral BUTTON-11 SAnual SEsprdca 
&Scoped-Define DISPLAYED-OBJECTS SDiario SSmnal SSmnal15 SSmnal31 SBsmnal ~
STrimesMnsual SMnsual STrimestral SMnsualSmstral SSmstral SAnual SEsprdca 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ejecutar 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Ejecutar" 
     SIZE 10 BY 1.62 TOOLTIP "Envía Repositorio Directamente A Excel".

DEFINE BUTTON Btn_Imp 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Imprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Opciones De Impresión".

DEFINE BUTTON BUTTON-11 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 11" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-14 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 14" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-166 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 166" 
     SIZE 10 BY 1.62.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE VARIABLE SAnual AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Base Gravable del Impuesto de Industria y Comercio","321" 
     SIZE 49 BY 1.35 NO-UNDO.

DEFINE VARIABLE SBsmnal AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Encaje bisemanal en moneda legal - Exigibilidades","226",
                     "Encaje bisemanal en moneda legal - Disponible","227" 
     SIZE 49 BY 1.62 NO-UNDO.

DEFINE VARIABLE SDiario AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Flujo de Caja Acido","213",
                     "Inversiones Obligatorias en TRD - Ley 546 de 1999","255",
                     "Composición del portafolio de inversiones","351" 
     SIZE 49 BY 2.69 NO-UNDO.

DEFINE VARIABLE SEsprdca AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Liquidación de Créditos Comerciales y de Consumo","326" 
     SIZE 49 BY 1.35 NO-UNDO.

DEFINE VARIABLE SMnsual AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Patrimonios Autonomos Establecimientos de Crédito","243",
                     "Liquidación Inversiones Obligatorias en TRD - Ley 546 de 1999","257",
                     "Infomación para la Excención del Gravamen de Movimientos Financieros","264",
                     "Evaluación del Riesgo de Tasa de Interés - Moneda Legal/Extranjera","269",
                     "Evaluación del Riesgo de Tasa de Interés - Unidades de Valor Real","270",
                     "Evaluación del Riesgo de Precio","271",
                     "Valor en Riesgo por Factores","280",
                     "Informe de clientes exonerados del registro de transacciones en efectivo","289",
                     "Cuentas de ahorro programado y AFC","337",
                     "Desagregado de Sectorización de Principales Operaciones","338",
                     "Tarifas de servicios financieros - establecimientos de crédito","365",
                     "CORRESPONSALES Y BANCA DE LAS OPORTUNIDADES","398" 
     SIZE 49 BY 6.19 NO-UNDO.

DEFINE VARIABLE SMnsualSmstral AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Formato de Cuentas No PUC para el Cálculo del Patrimonio Adecuado","110",
                     "Declaración del Control de Ley Margen de Solvencia","301" 
     SIZE 49 BY 1.35 NO-UNDO.

DEFINE VARIABLE SSmnal AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Informe Semanal - Tasas de Interés Activas y Pasivas.","88",
                     "Detalle de Activos y Pasivos en Moneda Legal Indexados a Moneda Extranjera","170",
                     "Control Diario de Posición Propia y Posición Cambiaria Global","230",
                     "Informe Semanal Principales Cuentas Activas y Pasivas-Saldos al Cierre","281",
                     "Reporte Semanal de Compra y Venta de Divisas","311" 
     SIZE 49 BY 3.23 NO-UNDO.

DEFINE VARIABLE SSmnal15 AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Saldos diarios de las operaciones del mercado monetario","442" 
     SIZE 49 BY 1.62 NO-UNDO.

DEFINE VARIABLE SSmnal31 AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Informe diario de tasas de interés de captación y operaciones del mercado monetario","441" 
     SIZE 49 BY 1.35 NO-UNDO.

DEFINE VARIABLE SSmstral AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Tasas de Interés Saldos Cartera de Créditos Personas Naturales y Jurídicas","317",
                     "Informe Estadístico por Tipo de Queja","340" 
     SIZE 49 BY 1.35 NO-UNDO.

DEFINE VARIABLE STrimesMnsual AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Medición del Riesgo de Liquidez","164" 
     SIZE 49 BY 1.62 NO-UNDO.

DEFINE VARIABLE STrimestral AS CHARACTER 
     VIEW-AS SELECTION-LIST MULTIPLE SORT 
     SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL 
     LIST-ITEM-PAIRS "Captaciones por Rango","202",
                     "Accionistas de Primer Nivel - Accionistas Principales","209",
                     "Accionistas de Segundo y Tercer Niel - Accionistas Indirectos","210",
                     "Número de Acciones y Número de Accinistas - Composición Accionaria","211",
                     "Exigibilidades Finagro","246",
                     "Disponibilidades Finagro","247",
                     "Operaciones Reciprocas Consolidadas Intergrupo","261",
                     "Informe de Reestructuración de Operaciones Activas de Crédito","279",
                     "Estadísticas de Servicios Especiales por Municipio","297",
                     "Servicios Especiales por Zonas de Municipio","298",
                     "Defraudaciones por Zonas de Municipio","299",
                     "Defraudaciones por Municipio","300",
                     "Captaciones y Colocaciones por Municipio","322",
                     "Captaciones y Colocaciones por Zonas de Municipio","323",
                     "Informe Individual por Deudor - Operaciones Activas de Crédito","341",
                     "Reporte Individual - Venta y/o Compra de Operaciones Activas de Crédito y/o Cartera Castigada","343",
                     "Informe Consolidado - Venta y/o Compra de Operaciones Activas de Crédito y/o Cartera Castigada","344",
                     "Novedades de Creditos CxCobrar y Bienes Dados Leasing que se encuentren en incumplimiento","347" 
     SIZE 49 BY 6.19 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     SDiario AT ROW 1.81 COL 2 NO-LABEL WIDGET-ID 2
     SSmnal AT ROW 1.81 COL 52 NO-LABEL WIDGET-ID 28
     BUTTON-166 AT ROW 4.77 COL 104 WIDGET-ID 62
     SSmnal15 AT ROW 5.58 COL 2 NO-LABEL WIDGET-ID 32
     SSmnal31 AT ROW 5.85 COL 52 NO-LABEL WIDGET-ID 34
     Btn_Imp AT ROW 6.38 COL 104 WIDGET-ID 54
     SBsmnal AT ROW 8 COL 2 NO-LABEL WIDGET-ID 12
     STrimesMnsual AT ROW 8 COL 52 NO-LABEL WIDGET-ID 42
     Btn_Ejecutar AT ROW 8 COL 104 WIDGET-ID 56
     SMnsual AT ROW 10.15 COL 2 NO-LABEL WIDGET-ID 20
     STrimestral AT ROW 10.15 COL 52 NO-LABEL WIDGET-ID 46
     BUTTON-14 AT ROW 15.27 COL 105 WIDGET-ID 60
     SMnsualSmstral AT ROW 17.15 COL 2 NO-LABEL WIDGET-ID 24
     SSmstral AT ROW 17.15 COL 52 NO-LABEL WIDGET-ID 40
     BUTTON-11 AT ROW 17.15 COL 107 WIDGET-ID 58
     SAnual AT ROW 19.31 COL 2 NO-LABEL WIDGET-ID 8
     SEsprdca AT ROW 19.31 COL 52 NO-LABEL WIDGET-ID 16
     "Diario:" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 1 COL 2 WIDGET-ID 4
          FGCOLOR 18 
     "Trimestral:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 9.62 COL 58 WIDGET-ID 48
          FGCOLOR 18 
     "Trimes-Mensual:" VIEW-AS TEXT
          SIZE 16 BY .62 AT ROW 7.19 COL 58 WIDGET-ID 44
          FGCOLOR 18 
     "Semestral:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 16.35 COL 58 WIDGET-ID 38
          FGCOLOR 18 
     "Semanal Con Corte Al 31:" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 5.04 COL 58 WIDGET-ID 36
          FGCOLOR 18 
     "Semanal Con Corte Al 15:" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 4.77 COL 2 WIDGET-ID 30
          FGCOLOR 18 
     "Semanal:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 1 COL 58 WIDGET-ID 26
          FGCOLOR 18 
     "Mensual/Semestral:" VIEW-AS TEXT
          SIZE 19 BY .62 AT ROW 16.35 COL 2 WIDGET-ID 22
          FGCOLOR 18 
     "Mensual:" VIEW-AS TEXT
          SIZE 9 BY .62 AT ROW 9.62 COL 2 WIDGET-ID 18
          FGCOLOR 18 
     "Esporádica:" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 18.5 COL 58 WIDGET-ID 14
          FGCOLOR 18 
     "Bisemanal:" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 7.19 COL 2 WIDGET-ID 10
          FGCOLOR 18 
     "Anual:" VIEW-AS TEXT
          SIZE 7 BY .62 AT ROW 18.5 COL 2 WIDGET-ID 6
          FGCOLOR 18 
     RECT-290 AT ROW 4.5 COL 103 WIDGET-ID 64
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 146 BY 26.38
         BGCOLOR 17  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE APPSERVER
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Formatos Financieros"
         HEIGHT             = 19.77
         WIDTH              = 113.14
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
         CONTROL-BOX        = no
         MIN-BUTTON         = no
         MAX-BUTTON         = no
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
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Formatos Financieros */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Formatos Financieros */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ejecutar
&Scoped-define SELF-NAME Btn_Imp
&Scoped-define SELF-NAME BUTTON-14
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-14 wWin
ON CHOOSE OF BUTTON-14 IN FRAME fMain /* Button 14 */
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


&Scoped-define SELF-NAME BUTTON-166
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-166 wWin
ON CHOOSE OF BUTTON-166 IN FRAME fMain /* Button 166 */
DO:
    RUN W-InfDia.w NO-ERROR.
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
  DISPLAY SDiario SSmnal SSmnal15 SSmnal31 SBsmnal STrimesMnsual SMnsual 
          STrimestral SMnsualSmstral SSmstral SAnual SEsprdca 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-290 SDiario SSmnal BUTTON-166 SSmnal15 SSmnal31 Btn_Imp SBsmnal 
         STrimesMnsual Btn_Ejecutar SMnsual STrimestral BUTTON-14 
         SMnsualSmstral SSmstral BUTTON-11 SAnual SEsprdca 
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

