&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
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

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Act_Fijo

/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain Act_Fijo.Agencia Act_Fijo.Estado ~
Act_Fijo.Codigo Act_Fijo.Nombre Act_Fijo.Grupo Act_Fijo.Descripcion ~
Act_Fijo.Cen_Costos Act_Fijo.Nit_Proveedor Act_Fijo.Nit_Seguro ~
Act_Fijo.Nit_Responsable Act_Fijo.Fec_Compra Act_Fijo.Nro_Seguro ~
Act_Fijo.Fec_Asignacion Act_Fijo.Vto_Seguro Act_Fijo.Ord_Compra ~
Act_Fijo.Mejoras Act_Fijo.Val_Garantia Act_Fijo.Nro_Factura ~
Act_Fijo.Val_Valorizacion Act_Fijo.Fec_Garantia Act_Fijo.Val_Compra ~
Act_Fijo.Val_Comercial Act_Fijo.Fec_IniDepre Act_Fijo.Fec_Retiro ~
Act_Fijo.Val_Provision Act_Fijo.Anos_Adepreciar Act_Fijo.Fec_debaja ~
Act_Fijo.Sdo_Provision Act_Fijo.Per_Depreciado Act_Fijo.Fec_Venta ~
Act_Fijo.Cos_Historico Act_Fijo.ValDepMes Act_Fijo.Id_Prestamo ~
Act_Fijo.Neto Act_Fijo.ValDepAcum Act_Fijo.Sdo_Depre 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain Act_Fijo.Agencia ~
Act_Fijo.Codigo Act_Fijo.Nombre Act_Fijo.Grupo Act_Fijo.Descripcion ~
Act_Fijo.Cen_Costos Act_Fijo.Nit_Proveedor Act_Fijo.Nit_Seguro ~
Act_Fijo.Nit_Responsable Act_Fijo.Fec_Compra Act_Fijo.Nro_Seguro ~
Act_Fijo.Fec_Asignacion Act_Fijo.Vto_Seguro Act_Fijo.Ord_Compra ~
Act_Fijo.Mejoras Act_Fijo.Val_Garantia Act_Fijo.Nro_Factura ~
Act_Fijo.Val_Valorizacion Act_Fijo.Fec_Garantia Act_Fijo.Val_Compra ~
Act_Fijo.Val_Comercial Act_Fijo.Fec_IniDepre Act_Fijo.Fec_Retiro ~
Act_Fijo.Val_Provision Act_Fijo.Anos_Adepreciar Act_Fijo.Fec_debaja ~
Act_Fijo.Sdo_Provision Act_Fijo.Per_Depreciado Act_Fijo.Fec_Venta ~
Act_Fijo.Cos_Historico Act_Fijo.ValDepMes Act_Fijo.Neto Act_Fijo.ValDepAcum ~
Act_Fijo.Sdo_Depre 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain Act_Fijo
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain Act_Fijo
&Scoped-define QUERY-STRING-fMain FOR EACH Act_Fijo SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH Act_Fijo SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain Act_Fijo
&Scoped-define FIRST-TABLE-IN-QUERY-fMain Act_Fijo


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Act_Fijo.Agencia Act_Fijo.Codigo ~
Act_Fijo.Nombre Act_Fijo.Grupo Act_Fijo.Descripcion Act_Fijo.Cen_Costos ~
Act_Fijo.Nit_Proveedor Act_Fijo.Nit_Seguro Act_Fijo.Nit_Responsable ~
Act_Fijo.Fec_Compra Act_Fijo.Nro_Seguro Act_Fijo.Fec_Asignacion ~
Act_Fijo.Vto_Seguro Act_Fijo.Ord_Compra Act_Fijo.Mejoras ~
Act_Fijo.Val_Garantia Act_Fijo.Nro_Factura Act_Fijo.Val_Valorizacion ~
Act_Fijo.Fec_Garantia Act_Fijo.Val_Compra Act_Fijo.Val_Comercial ~
Act_Fijo.Fec_IniDepre Act_Fijo.Fec_Retiro Act_Fijo.Val_Provision ~
Act_Fijo.Anos_Adepreciar Act_Fijo.Fec_debaja Act_Fijo.Sdo_Provision ~
Act_Fijo.Per_Depreciado Act_Fijo.Fec_Venta Act_Fijo.Cos_Historico ~
Act_Fijo.ValDepMes Act_Fijo.Neto Act_Fijo.ValDepAcum Act_Fijo.Sdo_Depre 
&Scoped-define ENABLED-TABLES Act_Fijo
&Scoped-define FIRST-ENABLED-TABLE Act_Fijo
&Scoped-Define ENABLED-OBJECTS RECT-4 RECT-5 RECT-6 RECT-7 RECT-8 RECT-285 
&Scoped-Define DISPLAYED-FIELDS Act_Fijo.Agencia Act_Fijo.Estado ~
Act_Fijo.Codigo Act_Fijo.Nombre Act_Fijo.Grupo Act_Fijo.Descripcion ~
Act_Fijo.Cen_Costos Act_Fijo.Nit_Proveedor Act_Fijo.Nit_Seguro ~
Act_Fijo.Nit_Responsable Act_Fijo.Fec_Compra Act_Fijo.Nro_Seguro ~
Act_Fijo.Fec_Asignacion Act_Fijo.Vto_Seguro Act_Fijo.Ord_Compra ~
Act_Fijo.Mejoras Act_Fijo.Val_Garantia Act_Fijo.Nro_Factura ~
Act_Fijo.Val_Valorizacion Act_Fijo.Fec_Garantia Act_Fijo.Val_Compra ~
Act_Fijo.Val_Comercial Act_Fijo.Fec_IniDepre Act_Fijo.Fec_Retiro ~
Act_Fijo.Val_Provision Act_Fijo.Anos_Adepreciar Act_Fijo.Fec_debaja ~
Act_Fijo.Sdo_Provision Act_Fijo.Per_Depreciado Act_Fijo.Fec_Venta ~
Act_Fijo.Cos_Historico Act_Fijo.ValDepMes Act_Fijo.Id_Prestamo ~
Act_Fijo.Neto Act_Fijo.ValDepAcum Act_Fijo.Sdo_Depre 
&Scoped-define DISPLAYED-TABLES Act_Fijo
&Scoped-define FIRST-DISPLAYED-TABLE Act_Fijo


/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-285
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 36 BY 2.69.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 8.88.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 5.12.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 107 BY 5.92.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 37 BY 8.88.

DEFINE RECTANGLE RECT-8
     EDGE-PIXELS 6 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 3.73.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY fMain FOR 
      Act_Fijo SCROLLING.
&ANALYZE-RESUME

/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     Act_Fijo.Agencia AT ROW 2.62 COL 18 COLON-ALIGNED WIDGET-ID 2
          VIEW-AS FILL-IN 
          SIZE 5 BY .81
     Act_Fijo.Estado AT ROW 3.42 COL 105 RIGHT-ALIGNED NO-LABEL WIDGET-ID 14
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activo", 1,
"Retirado", 2,
"Depreciado", 3
          SIZE 13 BY 1.62
     Act_Fijo.Codigo AT ROW 3.69 COL 18 COLON-ALIGNED WIDGET-ID 8
          VIEW-AS FILL-IN 
          SIZE 15 BY .81
     Act_Fijo.Nombre AT ROW 3.69 COL 35 COLON-ALIGNED NO-LABEL WIDGET-ID 46
          VIEW-AS FILL-IN 
          SIZE 53 BY .81
     Act_Fijo.Grupo AT ROW 4.77 COL 18 COLON-ALIGNED WIDGET-ID 32
          VIEW-AS FILL-IN 
          SIZE 8 BY .81
     Act_Fijo.Descripcion AT ROW 4.77 COL 37 NO-LABEL WIDGET-ID 12
          VIEW-AS FILL-IN 
          SIZE 53 BY .81
     Act_Fijo.Cen_Costos AT ROW 5.85 COL 18 COLON-ALIGNED WIDGET-ID 6
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
     Act_Fijo.Nit_Proveedor AT ROW 7.46 COL 18 COLON-ALIGNED WIDGET-ID 40
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
     Act_Fijo.Nit_Seguro AT ROW 7.46 COL 51.86 COLON-ALIGNED WIDGET-ID 44
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
     Act_Fijo.Nit_Responsable AT ROW 7.46 COL 89 COLON-ALIGNED WIDGET-ID 42
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
     Act_Fijo.Fec_Compra AT ROW 8.35 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 20
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
     Act_Fijo.Nro_Seguro AT ROW 8.35 COL 51.86 COLON-ALIGNED WIDGET-ID 50
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
     Act_Fijo.Fec_Asignacion AT ROW 8.62 COL 89 COLON-ALIGNED NO-LABEL WIDGET-ID 18
          VIEW-AS FILL-IN 
          SIZE 16 BY .73
     Act_Fijo.Vto_Seguro AT ROW 9.23 COL 52.14 COLON-ALIGNED NO-LABEL WIDGET-ID 74
          VIEW-AS FILL-IN 
          SIZE 16 BY .73
     Act_Fijo.Ord_Compra AT ROW 9.27 COL 18 COLON-ALIGNED WIDGET-ID 52
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
     Act_Fijo.Mejoras AT ROW 9.62 COL 89 COLON-ALIGNED NO-LABEL WIDGET-ID 36
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
     Act_Fijo.Val_Garantia AT ROW 10.04 COL 52.14 COLON-ALIGNED WIDGET-ID 68
          VIEW-AS FILL-IN 
          SIZE 18.57 BY .81
     Act_Fijo.Nro_Factura AT ROW 10.19 COL 18 COLON-ALIGNED WIDGET-ID 48
          VIEW-AS FILL-IN 
          SIZE 16 BY .81
     Act_Fijo.Val_Valorizacion AT ROW 10.69 COL 89 COLON-ALIGNED WIDGET-ID 72
          VIEW-AS FILL-IN 
          SIZE 18.57 BY .81
     Act_Fijo.Fec_Garantia AT ROW 10.96 COL 55.43 COLON-ALIGNED WIDGET-ID 24
          VIEW-AS FILL-IN 
          SIZE 12.86 BY .81
     Act_Fijo.Val_Compra AT ROW 11.15 COL 18 COLON-ALIGNED WIDGET-ID 66
          VIEW-AS FILL-IN 
          SIZE 18.57 BY .81
     Act_Fijo.Val_Comercial AT ROW 11.81 COL 89 COLON-ALIGNED WIDGET-ID 64
          VIEW-AS FILL-IN 
          SIZE 18.57 BY .77
     Act_Fijo.Fec_IniDepre AT ROW 12.04 COL 18 COLON-ALIGNED NO-LABEL WIDGET-ID 26
          VIEW-AS FILL-IN 
          SIZE 15 BY .73
     Act_Fijo.Fec_Retiro AT ROW 12.58 COL 56 COLON-ALIGNED WIDGET-ID 28
          VIEW-AS FILL-IN 
          SIZE 16 BY 1.08
     Act_Fijo.Val_Provision AT ROW 12.92 COL 89 COLON-ALIGNED WIDGET-ID 70
          VIEW-AS FILL-IN 
          SIZE 18.57 BY .73
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.57 BY 21.77 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     Act_Fijo.Anos_Adepreciar AT ROW 13.38 COL 18 COLON-ALIGNED WIDGET-ID 102
          VIEW-AS FILL-IN 
          SIZE 7.57 BY 1
     Act_Fijo.Fec_debaja AT ROW 13.73 COL 56 COLON-ALIGNED WIDGET-ID 22
          VIEW-AS FILL-IN 
          SIZE 16 BY 1.08
     Act_Fijo.Sdo_Provision AT ROW 14 COL 89 COLON-ALIGNED WIDGET-ID 58
          VIEW-AS FILL-IN 
          SIZE 18.57 BY .73
     Act_Fijo.Per_Depreciado AT ROW 14.46 COL 18.29 COLON-ALIGNED NO-LABEL WIDGET-ID 54
          VIEW-AS FILL-IN 
          SIZE 7 BY .81
     Act_Fijo.Fec_Venta AT ROW 14.96 COL 56 COLON-ALIGNED WIDGET-ID 30
          VIEW-AS FILL-IN 
          SIZE 16 BY 1.08
     Act_Fijo.Cos_Historico AT ROW 17.15 COL 24 COLON-ALIGNED WIDGET-ID 10
          VIEW-AS FILL-IN 
          SIZE 20 BY 1.08
     Act_Fijo.ValDepMes AT ROW 17.15 COL 68.57 COLON-ALIGNED NO-LABEL WIDGET-ID 62
          VIEW-AS FILL-IN 
          SIZE 20.43 BY 1.08
     Act_Fijo.Id_Prestamo AT ROW 17.15 COL 94 COLON-ALIGNED NO-LABEL WIDGET-ID 34
          VIEW-AS FILL-IN 
          SIZE 4 BY 1
     Act_Fijo.Neto AT ROW 18.23 COL 24 COLON-ALIGNED WIDGET-ID 38
          VIEW-AS FILL-IN 
          SIZE 19.72 BY 1
     Act_Fijo.ValDepAcum AT ROW 18.35 COL 68.57 COLON-ALIGNED NO-LABEL WIDGET-ID 60
          VIEW-AS FILL-IN 
          SIZE 20.43 BY 1
     Act_Fijo.Sdo_Depre AT ROW 19.58 COL 68.57 COLON-ALIGNED WIDGET-ID 56
          VIEW-AS FILL-IN 
          SIZE 20.43 BY 1
     "Venci/to:" VIEW-AS TEXT
          SIZE 8 BY 1.08 AT ROW 9.08 COL 45 WIDGET-ID 78
     "Meses" VIEW-AS TEXT
          SIZE 7 BY 1.62 AT ROW 13.65 COL 30 WIDGET-ID 104
     "Periodos Depre.:" VIEW-AS TEXT
          SIZE 16 BY 1.08 AT ROW 14.19 COL 4 WIDGET-ID 106
     "Fecha Compra:" VIEW-AS TEXT
          SIZE 14 BY 1.08 AT ROW 8.23 COL 6 WIDGET-ID 80
     "Fecha de Asigna:" VIEW-AS TEXT
          SIZE 16.14 BY 1.08 AT ROW 8.46 COL 74.86 WIDGET-ID 86
     "Mejoras:" VIEW-AS TEXT
          SIZE 9 BY 1.08 AT ROW 9.62 COL 81 WIDGET-ID 82
     "Vr. Deprec.Mensual:" VIEW-AS TEXT
          SIZE 18.43 BY 1.08 AT ROW 17.15 COL 52 WIDGET-ID 92
     "Fecha Ini.Depre:" VIEW-AS TEXT
          SIZE 15.43 BY 1.08 AT ROW 11.85 COL 4 WIDGET-ID 88
     "Vr. Deprec.Acumulada:" VIEW-AS TEXT
          SIZE 21.43 BY 1.08 AT ROW 18.23 COL 49 WIDGET-ID 94
     RECT-4 AT ROW 7.19 COL 3 WIDGET-ID 76
     RECT-5 AT ROW 7.19 COL 41 WIDGET-ID 84
     RECT-6 AT ROW 16.35 COL 3 WIDGET-ID 96
     RECT-7 AT ROW 7.19 COL 74 WIDGET-ID 98
     RECT-8 AT ROW 12.35 COL 41 WIDGET-ID 100
     RECT-285 AT ROW 13.12 COL 4 WIDGET-ID 108
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.57 BY 21.77 WIDGET-ID 100.


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
         TITLE              = "<insert SmartWindow title>"
         HEIGHT             = 21.77
         WIDTH              = 110.57
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
/* SETTINGS FOR FRAME fMain
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN Act_Fijo.Descripcion IN FRAME fMain
   ALIGN-L                                                              */
/* SETTINGS FOR RADIO-SET Act_Fijo.Estado IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN Act_Fijo.Id_Prestamo IN FRAME fMain
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "bdcentral.Act_Fijo"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* <insert SmartWindow title> */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* <insert SmartWindow title> */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
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

  {&OPEN-QUERY-fMain}
  GET FIRST fMain.
  IF AVAILABLE Act_Fijo THEN 
    DISPLAY Act_Fijo.Agencia Act_Fijo.Estado Act_Fijo.Codigo Act_Fijo.Nombre 
          Act_Fijo.Grupo Act_Fijo.Descripcion Act_Fijo.Cen_Costos 
          Act_Fijo.Nit_Proveedor Act_Fijo.Nit_Seguro Act_Fijo.Nit_Responsable 
          Act_Fijo.Fec_Compra Act_Fijo.Nro_Seguro Act_Fijo.Fec_Asignacion 
          Act_Fijo.Vto_Seguro Act_Fijo.Ord_Compra Act_Fijo.Mejoras 
          Act_Fijo.Val_Garantia Act_Fijo.Nro_Factura Act_Fijo.Val_Valorizacion 
          Act_Fijo.Fec_Garantia Act_Fijo.Val_Compra Act_Fijo.Val_Comercial 
          Act_Fijo.Fec_IniDepre Act_Fijo.Fec_Retiro Act_Fijo.Val_Provision 
          Act_Fijo.Anos_Adepreciar Act_Fijo.Fec_debaja Act_Fijo.Sdo_Provision 
          Act_Fijo.Per_Depreciado Act_Fijo.Fec_Venta Act_Fijo.Cos_Historico 
          Act_Fijo.ValDepMes Act_Fijo.Id_Prestamo Act_Fijo.Neto 
          Act_Fijo.ValDepAcum Act_Fijo.Sdo_Depre 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-4 RECT-5 RECT-6 RECT-7 RECT-8 RECT-285 Act_Fijo.Agencia 
         Act_Fijo.Codigo Act_Fijo.Nombre Act_Fijo.Grupo Act_Fijo.Descripcion 
         Act_Fijo.Cen_Costos Act_Fijo.Nit_Proveedor Act_Fijo.Nit_Seguro 
         Act_Fijo.Nit_Responsable Act_Fijo.Fec_Compra Act_Fijo.Nro_Seguro 
         Act_Fijo.Fec_Asignacion Act_Fijo.Vto_Seguro Act_Fijo.Ord_Compra 
         Act_Fijo.Mejoras Act_Fijo.Val_Garantia Act_Fijo.Nro_Factura 
         Act_Fijo.Val_Valorizacion Act_Fijo.Fec_Garantia Act_Fijo.Val_Compra 
         Act_Fijo.Val_Comercial Act_Fijo.Fec_IniDepre Act_Fijo.Fec_Retiro 
         Act_Fijo.Val_Provision Act_Fijo.Anos_Adepreciar Act_Fijo.Fec_debaja 
         Act_Fijo.Sdo_Provision Act_Fijo.Per_Depreciado Act_Fijo.Fec_Venta 
         Act_Fijo.Cos_Historico Act_Fijo.ValDepMes Act_Fijo.Neto 
         Act_Fijo.ValDepAcum Act_Fijo.Sdo_Depre 
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

