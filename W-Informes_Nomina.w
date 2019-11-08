&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
*/
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

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

{src/adm2/widgetprto.i}

    {Incluido/Variable.I "SHARED"}

  DEFINE VAR WBr AS INTEGER FORMAT 9.
  DEFINE VAR WHistorico AS LOGICAL INITIAL NO.
  DEFINE VAR i AS INTEGER FORMAT "99".
  DEFINE VAR WVAnt AS DECIMAL FORMAT ">>>,>>>,>>9.99".
  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR WTipo AS INTEGER FORMAT "9".
  DEFINE VAR Basico LIKE Empleados.Salario_Mensual.
  DEFINE VAR Linea AS INTEGER FORMAT "99".
  DEFINE VAR WDiferencia AS DECIMAL FORMAT "->>>,>>>,>>9.99".
  DEFINE VAR Choice AS LOGICAL.
  DEFINE VAR W_DocContab LIKE Comprobantes.Secuencia.
  DEFINE VAR W_Error       AS LOGICAL.
  DEFINE VARIABLE Cbte        LIKE Operacion.Comprobante.
  DEFINE VAR WDes AS CHARACTER FORMAT "X(20)".
  DEFINE VAR WNom AS CHARACTER FORMAT "X(25)".

  DEFINE VAR AgeIni LIKE Agencias.Agencia.
  DEFINE VAR AgeFin LIKE Agencias.Agencia.

  DEFINE VAR WSMLV LIKE Indicadores.Valor.
  DEFINE TEMP-TABLE TEmpleados
      FIELD Age    LIKE Agencias.Agencia
      FIELD Nit    LIKE Clientes.Nit
      FIELD Nom    AS CHARACTER FORMAT "X(45)"
      FIELD Car    AS CHARACTER FORMAT "X(40)"
      FIELD Est    LIKE Empleados.Estado
      FIELD TDv    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
      FIELD TDd    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
      FIELD Fec    AS DATE FORMAT "99/99/9999"
      FIELD Dia    AS INTEGER FORMAT "999"
      FIELD Bas    LIKE Empleados.Salario_Mensual
      FIELD Lin    AS INTEGER FORMAT "99".

  
  DEFINE TEMP-TABLE TDedDev
      FIELD Age    LIKE Agencias.Agencia
      FIELD Tip    LIKE Cfg_Novedades.Tipo
      FIELD Cod    LIKE Cfg_Novedades.Codigo
      FIELD Nom    AS CHARACTER FORMAT "X(40)"
      FIELD Nit    LIKE Clientes.Nit
      FIELD Fec    AS DATE FORMAT "99/99/9999"
      FIELD Vdv    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
      FIELD Vdd    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
      FIELD Dia    AS INTEGER FORMAT "99"
      FIELD IMo    LIKE Cfg_Novedades.Id_Valor
      FIELD Nat    LIKE Cfg_Novedades.Naturaleza
      FIELD Cta    LIKE Cfg_Novedades.Cuenta
      FIELD Per    LIKE Empleados.PerNom_Actual
      FIELD Est    LIKE Novedades_Nomina.Estado_Liquidacion
      FIELD Usu    LIKE Usuarios.Usuario
      FIELD NmE    AS INTEGER FORMAT "99".

  DEFINE TEMP-TABLE TTDedDev
      FIELD Age    LIKE Agencias.Agencia
      FIELD Tip    LIKE Cfg_Novedades.Tipo
      FIELD Cod    LIKE Cfg_Novedades.Codigo
      FIELD Nom    AS CHARACTER FORMAT "X(40)"
      FIELD Nit    LIKE Clientes.Nit
      FIELD Vdv    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
      FIELD Vdd    AS DECIMAL FORMAT ">>>,>>>,>>9.99"
      FIELD Dia    AS INTEGER FORMAT "999"
      FIELD NmE    AS INTEGER FORMAT "99".
      
      
      
      
      
        
  DEFINE TEMP-TABLE TImpresion
      FIELD Num AS INTEGER FORMAT "999"
      FIELD Nit LIKE Clientes.Nit
      FIELD Vec AS CHARACTER FORMAT "X(115)".

  DEFINE TEMP-TABLE TConceptos
      FIELD Tip    LIKE Cfg_Novedades.Tipo
      FIELD Cod    LIKE Cfg_Novedades.Codigo
      FIELD Nom    LIKE Cfg_Novedades.Nombre
      FIELD Idv    LIKE Cfg_Novedades.Id_Valor
      FIELD Cta    LIKE Cfg_Novedades.Cuenta
      FIELD Val    LIKE Cfg_Novedades.Valor.

  DEFINE TEMP-TABLE Det_Novedades
      FIELD FecNov AS DATE
      FIELD ValNov LIKE Ahorros.Sdo_Disponible.

  DEFINE TEMP-TABLE TConta LIKE Mov_Contable.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME fMain
&Scoped-define BROWSE-NAME BConceptos

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TTDedDev TDedDev TEmpleados

/* Definitions for BROWSE BConceptos                                    */
&Scoped-define FIELDS-IN-QUERY-BConceptos TTDedDev.Age TTDedDev.Nit WNom TTDedDev.Dia TTDedDev.Vdv TTDedDev.Vdd   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BConceptos   
&Scoped-define SELF-NAME BConceptos
&Scoped-define QUERY-STRING-BConceptos FOR EACH TTDedDev
&Scoped-define OPEN-QUERY-BConceptos OPEN QUERY {&SELF-NAME} FOR EACH TTDedDev.
&Scoped-define TABLES-IN-QUERY-BConceptos TTDedDev
&Scoped-define FIRST-TABLE-IN-QUERY-BConceptos TTDedDev


/* Definitions for BROWSE BDeducciones                                  */
&Scoped-define FIELDS-IN-QUERY-BDeducciones TTDedDev.Cod TTDedDev.Nom TTDedDev.Vdd   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BDeducciones   
&Scoped-define SELF-NAME BDeducciones
&Scoped-define QUERY-STRING-BDeducciones FOR EACH TTDeddev WHERE TTDeddev.Tip EQ 2
&Scoped-define OPEN-QUERY-BDeducciones OPEN QUERY {&SELF-NAME} FOR EACH TTDeddev WHERE TTDeddev.Tip EQ 2.
&Scoped-define TABLES-IN-QUERY-BDeducciones TTDeddev
&Scoped-define FIRST-TABLE-IN-QUERY-BDeducciones TTDeddev


/* Definitions for BROWSE BDetalle                                      */
&Scoped-define FIELDS-IN-QUERY-BDetalle TDedDev.Per TDedDev.Dia TDedDev.Cod TDedDev.Nme TDedDev.Fec TDedDev.VDv TDedDev.VDd   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BDetalle   
&Scoped-define SELF-NAME BDetalle
&Scoped-define QUERY-STRING-BDetalle FOR EACH TDedDev
&Scoped-define OPEN-QUERY-BDetalle OPEN QUERY {&SELF-NAME} FOR EACH TDedDev.
&Scoped-define TABLES-IN-QUERY-BDetalle TDedDev
&Scoped-define FIRST-TABLE-IN-QUERY-BDetalle TDedDev


/* Definitions for BROWSE BDevengados                                   */
&Scoped-define FIELDS-IN-QUERY-BDevengados TTDedDev.Cod TTDedDev.Nom TTDedDev.Vdv   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BDevengados   
&Scoped-define SELF-NAME BDevengados
&Scoped-define QUERY-STRING-BDevengados FOR EACH TTDedDev WHERE TTDeddev.Tip EQ 1
&Scoped-define OPEN-QUERY-BDevengados OPEN QUERY {&SELF-NAME} FOR EACH TTDedDev WHERE TTDeddev.Tip EQ 1.
&Scoped-define TABLES-IN-QUERY-BDevengados TTDedDev
&Scoped-define FIRST-TABLE-IN-QUERY-BDevengados TTDedDev


/* Definitions for BROWSE BEmpleados                                    */
&Scoped-define FIELDS-IN-QUERY-BEmpleados TEmpleados.Age TEmpleados.Nit TEmpleados.Nom TEmpleados.Car TEmpleados.Est TEmpleados.Dia TEmpleados.TDv TEmpleados.TDd WDiferencia   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BEmpleados   
&Scoped-define SELF-NAME BEmpleados
&Scoped-define QUERY-STRING-BEmpleados FOR EACH TEmpleados
&Scoped-define OPEN-QUERY-BEmpleados OPEN QUERY {&SELF-NAME} FOR EACH TEmpleados.
&Scoped-define TABLES-IN-QUERY-BEmpleados TEmpleados
&Scoped-define FIRST-TABLE-IN-QUERY-BEmpleados TEmpleados


/* Definitions for FRAME FDetNomina                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FDetNomina ~
    ~{&OPEN-QUERY-BDetalle}

/* Definitions for FRAME fMain                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-fMain ~
    ~{&OPEN-QUERY-BDeducciones}~
    ~{&OPEN-QUERY-BDevengados}~
    ~{&OPEN-QUERY-BEmpleados}

/* Definitions for FRAME FNovedades                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FNovedades ~
    ~{&OPEN-QUERY-BConceptos}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RTipo Btn_Generar Btn_Informacion Btn_Salir ~
CAgencia FecIni FecFin BEmpleados BDevengados BDeducciones 
&Scoped-Define DISPLAYED-OBJECTS RTipo CAgencia FecIni FecFin TE_Devengados ~
TE_Deducciones TE_Diferencia 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Btn_Informacion Btn_Salir CAgencia BEmpleados ~
BDevengados BDeducciones 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-BConceptos 
       MENU-ITEM m_Ver_detalle_de_este_emplead LABEL "Ver detalle de este empleado".

DEFINE MENU POPUP-MENU-BDeducciones 
       MENU-ITEM m_Reporte_Detallado_de_esta_d LABEL "Reporte Detallado de esta deducción".

DEFINE MENU POPUP-MENU-BDevengados 
       MENU-ITEM m_Reporte_Detallado_de_Esta_N LABEL "Reporte Detallado de Esta Novedad".

DEFINE MENU POPUP-MENU-BEmpleados 
       MENU-ITEM m_Informacion_Empleado LABEL "Informacion Empleado"
       MENU-ITEM m_Reporte_Detallado_de_todas_ LABEL "Reporte Detallado de todas las novedades".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-197 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE Cmb_org AS CHARACTER FORMAT "X(256)":U 
     LABEL "Organizar por" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Periodo","Fecha","Concepto","Tipo de Concepto" 
     DROP-DOWN-LIST
     SIZE 23 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TDeducciones AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 12 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TDevengados AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-186 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE SIE AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 67 BY 11.04
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_Generar 
     LABEL "Generar" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_Informacion 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 2" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE CAgencia AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todas las Agencias" 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TE_Deducciones AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TE_Devengados AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TE_Diferencia AS DECIMAL FORMAT "->>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RTipo AS INTEGER INITIAL 2 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Desde Conceptos", 2,
"Desde Empleados", 1
     SIZE 38 BY .81 NO-UNDO.

DEFINE BUTTON BUTTON-198 
     LABEL "Imprimir Informe" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE Cmb_Conceptos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Conceptos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WDeducciones AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 5 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WDevengados AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 7 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE ROpcion AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Devengados", 1,
"Deducciones", 2
     SIZE 26 BY 1.08 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BConceptos FOR 
      TTDedDev SCROLLING.

DEFINE QUERY BDeducciones FOR 
      TTDeddev SCROLLING.

DEFINE QUERY BDetalle FOR 
      TDedDev SCROLLING.

DEFINE QUERY BDevengados FOR 
      TTDedDev SCROLLING.

DEFINE QUERY BEmpleados FOR 
      TEmpleados SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BConceptos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BConceptos wWin _FREEFORM
  QUERY BConceptos DISPLAY
      TTDedDev.Age
 TTDedDev.Nit
 WNom COLUMN-LABEL "Nombre Empleado" WIDTH 48
 TTDedDev.Dia
 TTDedDev.Vdv COLUMN-LABEL "Devengados"
 TTDedDev.Vdd COLUMN-LABEL "Deducciones"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 93 BY 11.04
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE BDeducciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BDeducciones wWin _FREEFORM
  QUERY BDeducciones DISPLAY
      TTDedDev.Cod
TTDedDev.Nom WIDTH 25
TTDedDev.Vdd COLUMN-LABEL "Valor"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 6.73
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE BDetalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BDetalle wWin _FREEFORM
  QUERY BDetalle DISPLAY
      TDedDev.Per COLUMN-LABEL "Per"
TDedDev.Dia
TDedDev.Cod
TDedDev.Nme COLUMN-LABEL "H.Ext"
TDedDev.Fec COLUMN-LABEL "Fecha"
TDedDev.VDv COLUMN-LABEL "Devengados"
TDedDev.VDd COLUMN-LABEL "Deducciones"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 50 BY 9.15
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE BDevengados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BDevengados wWin _FREEFORM
  QUERY BDevengados DISPLAY
      TTDedDev.Cod
TTDedDev.Nom WIDTH 25
TTDedDev.Vdv COLUMN-LABEL "Valor"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 54 BY 6.73
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.

DEFINE BROWSE BEmpleados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BEmpleados wWin _FREEFORM
  QUERY BEmpleados DISPLAY
      TEmpleados.Age COLUMN-LABEL "Age"
      TEmpleados.Nit
      TEmpleados.Nom COLUMN-LABEL "Nombre del Empleado"
      TEmpleados.Car COLUMN-LABEL "Cargo Desempeñado" WIDTH 15
      TEmpleados.Est COLUMN-LABEL "EST"
      TEmpleados.Dia COLUMN-LABEL "Dias"
      TEmpleados.TDv WIDTH 12 COLUMN-LABEL "Devengados"
      TEmpleados.TDd WIDTH 12 COLUMN-LABEL "Deducciones"
      WDiferencia WIDTH 12 COLUMN-LABEL "Neto Pagar"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108 BY 6.19
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .5 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     RTipo AT ROW 1.27 COL 14 NO-LABEL
     Btn_Generar AT ROW 1.54 COL 71
     Btn_Informacion AT ROW 1.54 COL 88
     Btn_Salir AT ROW 1.54 COL 100
     CAgencia AT ROW 2.46 COL 11 COLON-ALIGNED
     FecIni AT ROW 3.54 COL 11 COLON-ALIGNED
     FecFin AT ROW 3.54 COL 37 COLON-ALIGNED
     BEmpleados AT ROW 6.15 COL 3
     TE_Devengados AT ROW 12.42 COL 82.72 RIGHT-ALIGNED NO-LABEL
     TE_Deducciones AT ROW 12.42 COL 95.14 RIGHT-ALIGNED NO-LABEL
     TE_Diferencia AT ROW 12.42 COL 94 COLON-ALIGNED NO-LABEL
     BDevengados AT ROW 14.23 COL 3
     BDeducciones AT ROW 14.23 COL 57.57
     "  Devengados entre las fechas" VIEW-AS TEXT
          SIZE 54 BY .81 AT ROW 13.42 COL 3
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "  Deducciones entre las fechas" VIEW-AS TEXT
          SIZE 54 BY .81 AT ROW 13.42 COL 57.57
          BGCOLOR 18 FGCOLOR 15 FONT 1
     "  Empleados de la agencia" VIEW-AS TEXT
          SIZE 108 BY .81 AT ROW 5.35 COL 3
          BGCOLOR 18 FGCOLOR 15 FONT 1
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 112.29 BY 20.62
         BGCOLOR 17 FONT 4.

DEFINE FRAME FNovedades
     ROpcion AT ROW 1.27 COL 11 NO-LABEL
     Cmb_Conceptos AT ROW 2.35 COL 9 COLON-ALIGNED
     BUTTON-198 AT ROW 2.62 COL 88
     BConceptos AT ROW 3.96 COL 10
     WDevengados AT ROW 15 COL 75 COLON-ALIGNED NO-LABEL
     WDeducciones AT ROW 15 COL 87 COLON-ALIGNED NO-LABEL
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 5.31
         SIZE 109 BY 15.62
         BGCOLOR 17 FGCOLOR 0 FONT 4.

DEFINE FRAME FDetNomina
     Cmb_org AT ROW 1.54 COL 26 COLON-ALIGNED
     BDetalle AT ROW 2.62 COL 4
     TDevengados AT ROW 11.77 COL 26 COLON-ALIGNED NO-LABEL
     TDeducciones AT ROW 11.77 COL 38 COLON-ALIGNED NO-LABEL
     BUTTON-197 AT ROW 12.85 COL 36
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 26 ROW 5.04
         SIZE 55 BY 13.96
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Detalle de Conceptos de Nomina entre fechas".

DEFINE FRAME FIE
     SIE AT ROW 1.54 COL 3 NO-LABEL
     BUTTON-186 AT ROW 12.85 COL 55
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 26 ROW 5.04
         SIZE 71 BY 14
         BGCOLOR 17 FONT 4
         TITLE "Información Empleados".


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
         TITLE              = "Informes de Nómina"
         HEIGHT             = 20.62
         WIDTH              = 112.29
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
ASSIGN FRAME FDetNomina:FRAME = FRAME fMain:HANDLE
       FRAME FIE:FRAME = FRAME fMain:HANDLE
       FRAME FNovedades:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME FDetNomina
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BDetalle Cmb_org FDetNomina */
ASSIGN 
       FRAME FDetNomina:HIDDEN           = TRUE
       FRAME FDetNomina:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN TDeducciones IN FRAME FDetNomina
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TDevengados IN FRAME FDetNomina
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FIE
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME FIE:HIDDEN           = TRUE
       FRAME FIE:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME fMain
                                                                        */
/* BROWSE-TAB BEmpleados FNovedades fMain */
/* BROWSE-TAB BDevengados TE_Diferencia fMain */
/* BROWSE-TAB BDeducciones BDevengados fMain */
/* SETTINGS FOR BROWSE BDeducciones IN FRAME fMain
   1                                                                    */
ASSIGN 
       BDeducciones:POPUP-MENU IN FRAME fMain             = MENU POPUP-MENU-BDeducciones:HANDLE.

/* SETTINGS FOR BROWSE BDevengados IN FRAME fMain
   1                                                                    */
ASSIGN 
       BDevengados:POPUP-MENU IN FRAME fMain             = MENU POPUP-MENU-BDevengados:HANDLE.

/* SETTINGS FOR BROWSE BEmpleados IN FRAME fMain
   1                                                                    */
ASSIGN 
       BEmpleados:POPUP-MENU IN FRAME fMain             = MENU POPUP-MENU-BEmpleados:HANDLE.

/* SETTINGS FOR BUTTON Btn_Informacion IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Salir IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR COMBO-BOX CAgencia IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR FILL-IN TE_Deducciones IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN TE_Devengados IN FRAME fMain
   NO-ENABLE ALIGN-R                                                    */
/* SETTINGS FOR FILL-IN TE_Diferencia IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FNovedades
                                                                        */
/* BROWSE-TAB BConceptos BUTTON-198 FNovedades */
ASSIGN 
       BConceptos:POPUP-MENU IN FRAME FNovedades             = MENU POPUP-MENU-BConceptos:HANDLE.

/* SETTINGS FOR FILL-IN WDeducciones IN FRAME FNovedades
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WDevengados IN FRAME FNovedades
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BConceptos
/* Query rebuild information for BROWSE BConceptos
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TTDedDev.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BConceptos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BDeducciones
/* Query rebuild information for BROWSE BDeducciones
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TTDeddev WHERE TTDeddev.Tip EQ 2.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BDeducciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BDetalle
/* Query rebuild information for BROWSE BDetalle
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TDedDev.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BDetalle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BDevengados
/* Query rebuild information for BROWSE BDevengados
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TTDedDev WHERE TTDeddev.Tip EQ 1.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BDevengados */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BEmpleados
/* Query rebuild information for BROWSE BEmpleados
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TEmpleados.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BEmpleados */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Informes de Nómina */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Informes de Nómina */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BConceptos
&Scoped-define FRAME-NAME FNovedades
&Scoped-define SELF-NAME BConceptos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BConceptos wWin
ON ROW-DISPLAY OF BConceptos IN FRAME FNovedades
DO:

  FIND TEmpleados WHERE TEmpleados.Nit EQ TTDedDev.Nit NO-ERROR.
  IF AVAILABLE TEmpleados THEN
     WNom = TEmpleados.Nom.
  IF TTDedDev.Tip EQ 1 THEN WDevengados = WDevengados + TTDedDev.Vdv.
  IF TTDedDev.Tip EQ 2 THEN WDeducciones = WDeducciones + TTDedDev.Vdd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BEmpleados
&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BEmpleados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BEmpleados wWin
ON MOUSE-SELECT-CLICK OF BEmpleados IN FRAME fMain
DO:
  OPEN QUERY BDevengados FOR EACH TTDedDev WHERE TTDedDev.Nit EQ TEmpleados.Nit AND TTDeddev.Tip EQ 1.
  OPEN QUERY BDeducciones FOR EACH TTDedDev WHERE TTDedDev.Nit EQ TEmpleados.Nit AND TTDeddev.Tip EQ 2.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BEmpleados wWin
ON ROW-DISPLAY OF BEmpleados IN FRAME fMain
DO:
  WDiferencia = TEmpleados.TDv - TEmpleados.TDd.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Generar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Generar wWin
ON CHOOSE OF Btn_Generar IN FRAME fMain /* Generar */
DO:
  FOR EACH TEmpleados: DELETE TEmpleados. END.
  FOR EACH TTDedDev: DELETE TTDedDev. END.
  FOR EACH TDedDev:  DELETE TDedDev.  END.
  ASSIGN FRAME Fmain CAgencia FecIni FecFin.
  ASSIGN TE_Devengados = 0 TE_Deducciones = 0 TE_Diferencia = 0.
  IF INTEGER(SUBSTRING(CAgencia,1,3)) EQ 0 THEN
     ASSIGN AgeIni = 0 AgeFin = 999.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(CAgencia,1,3)) AgeFin = AgeIni.
  RUN Cargar.
  OPEN QUERY BEmpleados FOR EACH TEmpleados.
  OPEN QUERY BDevengados FOR EACH TTDedDev WHERE TTDedDev.Nit EQ TEmpleados.Nit AND TTDeddev.Tip EQ 1.
  OPEN QUERY BDeducciones FOR EACH TTDedDev WHERE TTDedDev.Nit EQ TEmpleados.Nit AND TTDeddev.Tip EQ 2.
  APPLY "value-changed" TO Cmb_Conceptos IN FRAME FNovedades.
  VIEW FRAME FNovedades.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME fMain /* Salir */
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


&Scoped-define FRAME-NAME FIE
&Scoped-define SELF-NAME BUTTON-186
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-186 wWin
ON CHOOSE OF BUTTON-186 IN FRAME FIE /* Ocultar */
DO:
  HIDE FRAME FIE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetNomina
&Scoped-define SELF-NAME BUTTON-197
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-197 wWin
ON CHOOSE OF BUTTON-197 IN FRAME FDetNomina /* Ocultar */
DO:
  HIDE FRAME FDetNomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FNovedades
&Scoped-define SELF-NAME BUTTON-198
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-198 wWin
ON CHOOSE OF BUTTON-198 IN FRAME FNovedades /* Imprimir Informe */
DO:
DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Empresas.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME CAgencia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CAgencia wWin
ON VALUE-CHANGED OF CAgencia IN FRAME fMain /* Agencia */
DO:
  APPLY "choose" TO Btn_Generar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FNovedades
&Scoped-define SELF-NAME Cmb_Conceptos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Conceptos wWin
ON VALUE-CHANGED OF Cmb_Conceptos IN FRAME FNovedades /* Conceptos */
DO:
  ASSIGN FRAME FNovedades Cmb_Conceptos ROpcion.
  ASSIGN WDevengados = 0 WDeducciones = 0.
  WBr = 3.
  OPEN QUERY Bconceptos FOR EACH TTDedDev WHERE TTDedDev.Cod EQ INTEGER(SUBSTRING(Cmb_Conceptos,1,5)).
  DISPLAY WDevengados Wdeducciones WITH FRAME FNovedades.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FDetNomina
&Scoped-define SELF-NAME Cmb_org
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_org wWin
ON VALUE-CHANGED OF Cmb_org IN FRAME FDetNomina /* Organizar por */
DO:
  ASSIGN FRAME FDetNomina Cmb_Org.
  IF WBr EQ 1 THEN /*Browser general*/
  DO:
      IF Cmb_Org EQ "Periodo" THEN
         OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit BY TDedDev.Per.
      IF Cmb_Org EQ "Fecha" THEN
         OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit BY TDedDev.Fec.
      IF Cmb_Org EQ "Concepto" THEN
         OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit BY TDedDev.Cod.
      IF Cmb_Org EQ "Fecha" THEN
         OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit BY TDedDev.Tip.
  END.
  IF WBr EQ 2 THEN /*devengados y dedeucciones*/
  DO:
      IF Cmb_Org EQ "Periodo" THEN
          OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDedDev.Cod EQ TTDedDev.Cod BY TDedDev.Per.
      IF Cmb_Org EQ "Fecha" THEN
         OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDedDev.Cod EQ TTDedDev.Cod BY TDedDev.Fec.
      IF Cmb_Org EQ "Concepto" THEN
         OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDedDev.Cod EQ TTDedDev.Cod BY TDedDev.Cod.
      IF Cmb_Org EQ "Fecha" THEN
         OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND TDedDev.Cod EQ TTDedDev.Cod BY TDedDev.Tip.
  END.
  IF WBr EQ 3 THEN /*devengados y dedeucciones*/
  DO:
      IF Cmb_Org EQ "Periodo" THEN
          OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TTDedDev.Nit AND TDedDev.Cod EQ INTEGER(SUBSTRING(Cmb_Conceptos,1,5)) BY TDedDev.Per.
      IF Cmb_Org EQ "Fecha" THEN
         OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TTDedDev.Nit AND TDedDev.Cod EQ INTEGER(SUBSTRING(Cmb_Conceptos,1,5)) BY TDedDev.Fec.
      IF Cmb_Org EQ "Concepto" THEN
         OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TTDedDev.Nit AND TDedDev.Cod EQ INTEGER(SUBSTRING(Cmb_Conceptos,1,5)) BY TDedDev.Cod.
      IF Cmb_Org EQ "Fecha" THEN
         OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TTDedDev.Nit AND TDedDev.Cod EQ INTEGER(SUBSTRING(Cmb_Conceptos,1,5)) BY TDedDev.Tip.
  END.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Informacion_Empleado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Informacion_Empleado wWin
ON CHOOSE OF MENU-ITEM m_Informacion_Empleado /* Informacion Empleado */
DO:
  FIND Empleados WHERE Empleados.Nit EQ TEmpleados.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Empleados THEN DO:
     ASSIGN FRAME FIE:TITLE = TEmpleados.Nom.
     W_Ok = SIE:ADD-LAST("Cargo desempeñado         :   " + Empleados.Cargo). 
     CASE Empleados.Estado:
         WHEN 1 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Activo - Normal"). 
         WHEN 2 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Retirado"). 
         WHEN 3 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Lic.Maternindad"). 
         WHEN 4 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Lic.Remunerada"). 
         WHEN 5 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Lic.No.Remunerada"). 
         WHEN 6 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Vacaciones"). 
         WHEN 7 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Comision"). 
         WHEN 8 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Calamidad Domestica"). 
         WHEN 9 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Incapacidad"). 
         WHEN 10 THEN W_Ok = SIE:ADD-LAST("Estado actual             :   " + "Suspendido"). 
     END CASE.
     W_Ok = SIE:ADD-LAST("Fecha de Ingreso          :   " + STRING(Empleados.Fec_Ingreso)). 
     CASE Empleados.Forma_Pago:
         WHEN 1 THEN W_Ok = SIE:ADD-LAST("Forma de Pago             :   " + "Efectivo"). 
         WHEN 2 THEN W_Ok = SIE:ADD-LAST("Forma de Pago             :   " + "Cheque"). 
         WHEN 3 THEN W_Ok = SIE:ADD-LAST("Forma de Pago             :   " + "Banca Electronica"). 
     END CASE.
     FIND Varios WHERE Varios.Tipo EQ 32 AND Varios.Codigo EQ Empleados.Fon_Cesantias NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN W_Ok = SIE:ADD-LAST("Fondo de Cesantias        :   " + Varios.Descripcion).
     ELSE W_Ok = SIE:ADD-LAST("Fondo de Cesantias        :   " + "No Matriculado"). 
     FIND Varios WHERE Varios.Tipo EQ 31 AND Varios.Codigo EQ Empleados.Fon_Pensiones NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN W_Ok = SIE:ADD-LAST("Fondo de Pensiones        :   " + Varios.Descripcion).
     ELSE W_Ok = SIE:ADD-LAST("Fondo de Pensiones        :   " + "No Matriculado"). 
     FIND Varios WHERE Varios.Tipo EQ 33 AND Varios.Codigo EQ Empleados.EPS NO-LOCK NO-ERROR.
     IF AVAILABLE Varios THEN W_Ok = SIE:ADD-LAST("EPS                       :   " + Varios.Descripcion).
     ELSE W_Ok = SIE:ADD-LAST("EPS                       :   " + "No Matriculada"). 
     SIE:ADD-LAST("Salario Basico Mensual    :   " + STRING(Empleados.Salario_Mensual)). 
     CASE Empleados.Periodo_Pago:
         WHEN 1 THEN SIE:ADD-LAST("Periodo de Pago           :   " + "Semanal"). 
         WHEN 2 THEN SIE:ADD-LAST("Periodo de Pago           :   " + "Quincenal"). 
         WHEN 3 THEN SIE:ADD-LAST("Periodo de Pago           :   " + "Mensual"). 
     END CASE.
     IF Empleados.Prestamo_Especial GT 0 THEN DO:
        SIE:ADD-LAST("Prestamo Especial         :   " + STRING(Empleados.Prestamo_Especial)). 
        SIE:ADD-LAST("Plazo Prestamo            :   " + STRING(Empleados.Plazo_Prestamo)). 
        SIE:ADD-LAST("Fec.Inicio Prestamo       :   " + STRING(Empleados.Fec_InicioPrestamo)). 
     END.
  END.
  VIEW FRAME FIE.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Reporte_Detallado_de_esta_d
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Reporte_Detallado_de_esta_d wWin
ON CHOOSE OF MENU-ITEM m_Reporte_Detallado_de_esta_d /* Reporte Detallado de esta deducción */
DO:
 WBr = 3.
 ASSIGN TDeducciones = 0
        TDevengados  = 0.
 FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND
                       TDedDev.Cod EQ TTDedDev.Cod:

    ASSIGN TDevengados  = TDevengados + TDedDev.VDv
           TDeducciones = TDeducciones + TDedDev.VDd.
 END.
  OPEN QUERY BDetalle 
       FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND
                              TDedDev.Cod EQ TTDedDev.Cod BY TDedDev.Per.
  DISPLAY TDeducciones TDevengados WITH FRAME FDetNomina.
  VIEW FRAME FDetNomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Reporte_Detallado_de_Esta_N
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Reporte_Detallado_de_Esta_N wWin
ON CHOOSE OF MENU-ITEM m_Reporte_Detallado_de_Esta_N /* Reporte Detallado de Esta Novedad */
DO:
 WBr = 2.
 ASSIGN TDeducciones = 0
        TDevengados  = 0.
 FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND
                       TDedDev.Cod EQ TTDedDev.Cod:

    ASSIGN TDevengados  = TDevengados + TDedDev.VDv
           TDeducciones = TDeducciones + TDedDev.VDd.
 END.
 OPEN QUERY BDetalle 
      FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit AND
                              TDedDev.Cod EQ TTDedDev.Cod BY TDedDev.Per.
 DISPLAY TDeducciones TDevengados WITH FRAME FDetNomina.
 VIEW FRAME FDetNomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Reporte_Detallado_de_todas_
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Reporte_Detallado_de_todas_ wWin
ON CHOOSE OF MENU-ITEM m_Reporte_Detallado_de_todas_ /* Reporte Detallado de todas las novedades */
DO:
 WBr = 1.
 ASSIGN TDeducciones = 0
        TDevengados  = 0.
 FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit:
    ASSIGN TDevengados  = TDevengados + TDedDev.VDv
           TDeducciones = TDeducciones + TDedDev.VDd.
  END.
  OPEN QUERY BDetalle FOR EACH TDedDev WHERE TDedDev.Nit EQ TEmpleados.Nit BY TDedDev.Per.
  DISPLAY TDeducciones TDevengados WITH FRAME FDetNomina.
  VIEW FRAME FDetNomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Ver_detalle_de_este_emplead
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Ver_detalle_de_este_emplead wWin
ON CHOOSE OF MENU-ITEM m_Ver_detalle_de_este_emplead /* Ver detalle de este empleado */
DO:
 WBr = 3.
 ASSIGN TDeducciones = 0
        TDevengados  = 0.
 FOR EACH TDedDev WHERE TDedDev.Nit EQ TTDedDev.Nit AND
                       TDedDev.Cod EQ TTDedDev.Cod:

    ASSIGN TDevengados  = TDevengados + TDedDev.VDv
           TDeducciones = TDeducciones + TDedDev.VDd.
 END.
 OPEN QUERY BDetalle 
      FOR EACH TDedDev WHERE TDedDev.Nit EQ TTDedDev.Nit AND
                              TDedDev.Cod EQ TTDedDev.Cod BY TDedDev.Per.
 DISPLAY TDeducciones TDevengados WITH FRAME FDetNomina.
 VIEW FRAME FDetNomina.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FNovedades
&Scoped-define SELF-NAME ROpcion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL ROpcion wWin
ON VALUE-CHANGED OF ROpcion IN FRAME FNovedades
DO:
  Cmb_Conceptos:LIST-ITEMS = "".
  ASSIGN FRAME FNovedades ROpcion.
  FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Tipo LT 3 AND Cfg_Novedades.Estado EQ 1 AND
           Cfg_Novedades.Tipo EQ ROpcion NO-LOCK:
      W_Ok = Cmb_Conceptos:ADD-LAST(STRING(Cfg_Novedades.Codigo,"99999") + " - " + Cfg_Novedades.Nombre) IN FRAME FNovedades.
  END.
  Cmb_Conceptos:SCREEN-VALUE = Cmb_Conceptos:ENTRY(1).
  APPLY "value-changed" TO Cmb_Conceptos IN FRAME FNovedades.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME RTipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RTipo wWin
ON VALUE-CHANGED OF RTipo IN FRAME fMain
DO:
  ASSIGN FRAME Fmain RTipo.
  IF RTipo EQ 1 THEN DO:
     w_ok = BROWSE BEmpleados:SELECT-ROW(1).
     HIDE FRAME FNovedades.
  END.
  ELSE VIEW FRAME FNovedades.
  APPLY "value-changed" TO Cmb_Conceptos IN FRAME FNovedades.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BConceptos
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cargar wWin 
PROCEDURE Cargar :
ASSIGN FRAME FMain CAgencia FecIni FecFin.
DEFINE VAR Trabajados AS INTEGER FORMAT "999".
DEFINE VAR TTDv AS DECIMAL FORMAT ">>>,>>>,>>9.99".
DEFINE VAR TTDd AS DECIMAL FORMAT ">>>,>>>,>>9.99".
Linea = 0.


FOR EACH Novedades_Nomina WHERE
         Novedades_Nomina.Agencia            GE AgeIni AND
         Novedades_Nomina.Agencia            LE AgeFin AND
         Novedades_Nomina.Estado_Liquidacion EQ 2 AND
         Novedades_Nomina.Fecha              GE FecIni AND
         Novedades_Nomina.Fecha              LE FecFin NO-LOCK
         BREAK BY Novedades_Nomina.Nit BY Novedades_Nomina.PerNomina:
    IF FIRST-OF(Novedades_Nomina.PerNomina) THEN Trabajados = Trabajados + Novedades_Nomina.Dias_Trabajados.
    
    FIND Cfg_Novedades WHERE Cfg_Novedades.Codigo EQ Novedades_Nomina.Codigo NO-LOCK NO-ERROR.
    FIND TTDedDev WHERE 
         TTDedDev.Nit EQ Novedades_Nomina.Nit AND
         TTDedDev.Cod EQ Novedades_Nomina.Codigo NO-ERROR.
    IF NOT AVAILABLE TTDedDev THEN DO:
        CREATE TTdedDev.
        ASSIGN TTDedDev.Age = Novedades_Nomina.Agencia
               TTDedDev.Tip = Novedades_Nomina.Tipo
               TTDedDev.Cod = Novedades_Nomina.Codigo
               TTDedDev.Nom = Cfg_Novedades.Nombre
               TTDedDev.Nit = Novedades_Nomina.Nit.
    END.
    TTDedDev.Dia = TTDedDev.Dia + Novedades_Nomina.Dias_Trabajados.
    IF Cfg_Novedades.Tipo EQ 1 THEN
       TTDedDev.Vdv = TTDedDev.Vdv + Novedades_Nomina.Valor.
    IF Cfg_Novedades.Tipo EQ 2 THEN
       TTDedDev.Vdd = TTDedDev.Vdd + Novedades_Nomina.Valor.
    
    CREATE TDedDev.
    ASSIGN TDedDev.Age = Novedades_Nomina.Agencia
           TDedDev.Tip = Novedades_Nomina.Tipo
           TDedDev.Cod = Novedades_Nomina.Codigo
           TDedDev.Nom = Cfg_Novedades.Nombre
           TDedDev.Nit = Novedades_Nomina.Nit
           TDedDev.Fec = Novedades_Nomina.Fecha
           TDedDev.IMo = Cfg_Novedades.Id_Valor
           TDedDev.Nat = Novedades_Nomina.Naturaleza
           TDedDev.Cta = Novedades_Nomina.Cuenta
           TDedDev.Dia = Novedades_Nomina.Dias_Trabajados
           TDedDev.Est = Novedades_Nomina.Estado_Liquidacion
           TDedDev.Per = Novedades_Nomina.PerNomina
           TDedDev.Usu = Novedades_Nomina.Usuario
           TDedDev.Nme = Novedades_Nomina.Horas_Extras.
    IF Novedades_Nomina.Tipo EQ 1 THEN
       ASSIGN TTDv = TTDv + Novedades_Nomina.Valor
              TDedDev.VDv = Novedades_Nomina.Valor.
    ELSE
       ASSIGN TTDd = TTDd + Novedades_Nomina.Valor
              TDedDev.VDd = Novedades_Nomina.Valor.
    IF LAST-OF(Novedades_Nomina.Nit) THEN DO:
       FIND Clientes WHERE Clientes.Nit EQ Novedades_Nomina.Nit NO-LOCK NO-ERROR.
       IF AVAILABLE Clientes THEN DO:
          FIND Empleados WHERE Empleados.Agencia EQ Novedades_Nomina.Agencia AND
               Empleados.Nit EQ Clientes.Nit NO-LOCK NO-ERROR.
          IF AVAILABLE Empleados THEN DO:
             CREATE TEmpleados.
             ASSIGN TEmpleados.Age  = Novedades_Nomina.Agencia
                    TEmpleados.Nit  = Clientes.Nit
                    TEmpleados.Nom  = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                    TEmpleados.Car  = Empleados.Cargo
                    TEmpleados.Est  = Empleados.Estado
                    TEmpleados.TDv  = TTDv
                    TEmpleados.TDd  = TTDd
                    TEmpleados.Fec  = Novedades_Nomina.Fecha
                    TEmpleados.Dia  = TEmpleados.Dia + Trabajados
                    TEmpleados.Bas  = Empleados.Salario_Mensual
                    TEmpleados.Lin  = Linea + 1
                    Linea           = Linea + 1.
             ASSIGN TE_Devengados  = TE_Devengados + TTDv
                    TE_Deducciones = TE_Deducciones + TTDd
                    TTDv = 0 TTDd = 0.
          END.
       END.
       Trabajados = 0.
    END.
END. 
TE_Diferencia = TE_Devengados - TE_Deducciones.
DISPLAY TE_Devengados TE_Deducciones TE_Diferencia WITH FRAME FMain.

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
  DISPLAY RTipo CAgencia FecIni FecFin TE_Devengados TE_Deducciones 
          TE_Diferencia 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RTipo Btn_Generar Btn_Informacion Btn_Salir CAgencia FecIni FecFin 
         BEmpleados BDevengados BDeducciones 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY Cmb_org TDevengados TDeducciones 
      WITH FRAME FDetNomina IN WINDOW wWin.
  ENABLE Cmb_org BDetalle BUTTON-197 
      WITH FRAME FDetNomina IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FDetNomina}
  DISPLAY SIE 
      WITH FRAME FIE IN WINDOW wWin.
  ENABLE SIE BUTTON-186 
      WITH FRAME FIE IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FIE}
  DISPLAY ROpcion Cmb_Conceptos WDevengados WDeducciones 
      WITH FRAME FNovedades IN WINDOW wWin.
  ENABLE ROpcion Cmb_Conceptos BUTTON-198 BConceptos 
      WITH FRAME FNovedades IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FNovedades}
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
RUN SUPER.
  ASSIGN FecIni = DATE(STRING(W_Fecha - DAY(W_Fecha))).
         FecFin = DATE(STRING(W_Fecha)).
  DISPLAY FecFin FecIni WITH FRAME FMain.
  FOR EACH Agencias WHERE Agencias.Estado EQ 1 NO-LOCK BREAK BY Agencias.Agencia:
      W_Ok = CAgencia:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME FMain.
      IF FIRST-OF(Agencias.Agencia) THEN
         CAgencia:SCREEN-VALUE = CAgencia:ENTRY(1).
  END.

  FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Tipo LT 3 AND Cfg_Novedades.Estado EQ 1 NO-LOCK:
      CREATE TConceptos.
      ASSIGN TConceptos.Tip = Cfg_Novedades.Tipo
             TConceptos.Cod = Cfg_Novedades.Codigo
             TConceptos.Nom = Cfg_Novedades.Nombre
             TConceptos.Idv = Cfg_Novedades.Id_Valor
             TConceptos.Val = Cfg_Novedades.Valor
             TConceptos.Cta = Cfg_Novedades.Cuenta.
      IF Cfg_Novedades.Tipo EQ 1 THEN
         W_Ok = Cmb_Conceptos:ADD-LAST(STRING(Cfg_Novedades.Codigo,"99999") + " - " + Cfg_Novedades.Nombre) IN FRAME FNovedades.
  END.
  Cmb_Conceptos:SCREEN-VALUE = Cmb_Conceptos:ENTRY(1).
      FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  IF AVAILABLE Entidad THEN DO:
     IF Entidad.Ind_SMLV EQ ? OR Entidad.Ind_SMLV EQ 0 THEN DO:
        MESSAGE "No se ha entrado el indicador del SMLV en la Configuracion" SKIP
                "del registro de la organizacion. para hacer el calculo" SKIP
                "de algunas novedades de nomina, este valor debe estar" SKIP
                "configurado en la informacion de la entidad" SKIP(1)
                "Configure este indicador y vuelva a intentar hacer una prenomina"
                VIEW-AS ALERT-BOX ERROR.
        APPLY "choose" TO Btn_Salir IN FRAME FMain.
     END.
     ELSE DO:
        FIND Indicadores WHERE Indicadores.Indicador EQ Entidad.Ind_SMLV NO-LOCK NO-ERROR.
        IF NOT AVAILABLE Indicadores THEN DO:
            MESSAGE "No se ha entrado el indicador del SMLV en la" SKIP 
                    "configuracion de indicadores." SKIP(1)
                    "Configure este indicador y vuelva a intentar hacer una prenomina"
                    VIEW-AS ALERT-BOX ERROR.
            APPLY "choose" TO Btn_Salir IN FRAME FMain.
        END.
        ELSE WSMLV = Indicadores.Valor.
     END.
  END.
  VIEW FRAME FNovedades.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
DEFINE VAR W_NomEmp AS CHARACTER FORMAT "X(40)".
DEFINE VAR WEmpIni LIKE Clientes.Nit.
DEFINE VAR WEmpFin LIKE Clientes.Nit.
DEFINE VAR WConIni AS INTEGER FORMAT "99999".
DEFINE VAR WConFin AS INTEGER FORMAT "99999".
DEFINE VAR WLinea1 AS CHARACTER FORMAT "X(120)".
DEFINE VAR WLinea2 AS CHARACTER FORMAT "X(120)".
DEFINE VAR WLinea3 AS CHARACTER FORMAT "X(120)".
DEFINE VAR WLinea4 AS CHARACTER FORMAT "X(120)".

{INCLUIDO\RepEncabezado.I}    

  W_Reporte    = "REPORTE   : HOJA DE VIDA DE LOS EMPLEADOS - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "Age Cedula       Nombre                                  Basico Mes    FecIngreso".
  
  DEFINE VAR NomEmp AS CHARACTER FORMAT "X(35)".
  DEFINE VAR iTotDev AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99".  
  DEFINE VAR iTotDed AS DECIMAL FORMAT ">>>,>>>,>>>,>>9.99".
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.

  FOR EACH TTDedDev WHERE TTDedDev.Cod EQ INTEGER(SUBSTRING(Cmb_Conceptos,1,5)) BY TTDedDev.Nit:
     NomEmp = "Empleado no esta en clientes".
     FIND Clientes WHERE Clientes.Nit EQ TTDedDev.Nit NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN NomEmp = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     DISPLAY TTDedDev.Age
             TTDedDev.Nit
             NomEmp
             TTDedDev.Dia
             TTDedDev.Vdv AT 65
             TTDedDev.Vdd AT 85
     WITH FRAME F-empleados WIDTH 200 NO-LABELS STREAM-IO USE-TEXT NO-BOX.
     ASSIGN iTotDev = itotdev + TTDedDev.Vdv
            itotded = itotded + TTDedDev.Vdd.
  END.  
  DISPLAY SKIP(1) "Totales : " AT 1
           itotdev     AT 65
           itotded     AT 85
  WITH FRAME ft WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

