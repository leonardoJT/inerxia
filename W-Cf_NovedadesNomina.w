&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
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

{Incluido\variable.i "shared"}
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_IdPagoExiste AS LOGICAL.
DEFINE VAR W_IdBaseExiste AS LOGICAL.
   
DEFINE VAR W_Nvo AS LOGICAL INITIAL NO.
DEFINE VAR P_Nit      LIKE Clientes.Nit.
DEFINE VAR p_Nombre   LIKE Clientes.Nombre.
DEFINE VAR P_Apellido LIKE Clientes.Apellido1.
DEFINE VAR P_AgeCli   LIKE Clientes.Agencia.

DEFINE VAR P_CodInd  LIKE Indicadores.Indicador.
DEFINE VAR P_NomInd  LIKE Indicadores.Nombre.
DEFINE VAR P_ValTas  LIKE Indicadores.Tasa.
DEFINE VAR P_Valor   LIKE Indicadores.Valor.

DEFINE VAR W_Pcodigo    LIKE Varios.Codigo.
DEFINE VAR W_Pcuenta    LIKE Cuenta.Cuenta.
DEFINE VAR W_Pnombre    LIKE Cuenta.Nombre.
DEFINE VAR W_Pnomcta    LIKE Cuenta.Nombre.
DEFINE VAR W_Naturaleza LIKE Cuenta.Naturaleza.
DEFINE VAR W_ctrnat     LIKE Cuentas.Ctr_Naturaleza.
DEFINE VAR W_HayError   AS   LOGICAL.
DEFINE VAR W_ctasal     LIKE Cuentas.Cuenta.
DEFINE VAR W_posicion   AS INTEGER INITIAL 0.
DEFINE VAR W_nroreg     AS INTEGER INITIAL 0.

DEFINE TEMP-TABLE Cfg_NovedadesT LIKE Cfg_Novedades.

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
&Scoped-define BROWSE-NAME BNovedades

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cfg_Novedades

/* Definitions for BROWSE BNovedades                                    */
&Scoped-define FIELDS-IN-QUERY-BNovedades Cfg_Novedades.Codigo Cfg_Novedades.Nombre Cfg_Novedades.Cuenta   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BNovedades   
&Scoped-define SELF-NAME BNovedades
&Scoped-define QUERY-STRING-BNovedades FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Estado EQ REstado
&Scoped-define OPEN-QUERY-BNovedades OPEN QUERY {&SELF-NAME} FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Estado EQ REstado.
&Scoped-define TABLES-IN-QUERY-BNovedades Cfg_Novedades
&Scoped-define FIRST-TABLE-IN-QUERY-BNovedades Cfg_Novedades


/* Definitions for FRAME fMain                                          */
&Scoped-define FIELDS-IN-QUERY-fMain Cfg_Novedades.Tipo ~
Cfg_Novedades.Naturaleza Cfg_Novedades.Cuenta Cfg_Novedades.Codigo ~
Cfg_Novedades.Nombre Cfg_Novedades.Cta_Contrapartida Cfg_Novedades.Id_Base ~
Cfg_Novedades.Id_Descripcion Cfg_Novedades.Nit_Contable ~
Cfg_Novedades.Id_Pago Cfg_Novedades.Id_Extras Cfg_Novedades.Id_Salario ~
Cfg_Novedades.indicador Cfg_Novedades.Estado Cfg_Novedades.Programa ~
Cfg_Novedades.Id_Valor Cfg_Novedades.Clase Cfg_Novedades.Valor ~
Cfg_Novedades.Id_SMLV Cfg_Novedades.Num_SMLV Cfg_Novedades.PunAd_Calculo ~
Cfg_Novedades.Porc_Parafiscal 
&Scoped-define ENABLED-FIELDS-IN-QUERY-fMain Cfg_Novedades.Tipo ~
Cfg_Novedades.Naturaleza Cfg_Novedades.Cuenta Cfg_Novedades.Nombre ~
Cfg_Novedades.Cta_Contrapartida Cfg_Novedades.Id_Base ~
Cfg_Novedades.Id_Descripcion Cfg_Novedades.Nit_Contable ~
Cfg_Novedades.Id_Pago Cfg_Novedades.Id_Extras Cfg_Novedades.Id_Salario ~
Cfg_Novedades.indicador Cfg_Novedades.Estado Cfg_Novedades.Programa ~
Cfg_Novedades.Id_Valor Cfg_Novedades.Clase Cfg_Novedades.Valor ~
Cfg_Novedades.Id_SMLV Cfg_Novedades.Porc_Parafiscal 
&Scoped-define ENABLED-TABLES-IN-QUERY-fMain Cfg_Novedades
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-fMain Cfg_Novedades
&Scoped-define QUERY-STRING-fMain FOR EACH Cfg_Novedades SHARE-LOCK
&Scoped-define OPEN-QUERY-fMain OPEN QUERY fMain FOR EACH Cfg_Novedades SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-fMain Cfg_Novedades
&Scoped-define FIRST-TABLE-IN-QUERY-fMain Cfg_Novedades


/* Definitions for FRAME FNovedades                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-FNovedades ~
    ~{&OPEN-QUERY-BNovedades}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Cfg_Novedades.Tipo Cfg_Novedades.Naturaleza ~
Cfg_Novedades.Cuenta Cfg_Novedades.Nombre Cfg_Novedades.Cta_Contrapartida ~
Cfg_Novedades.Id_Base Cfg_Novedades.Id_Descripcion ~
Cfg_Novedades.Nit_Contable Cfg_Novedades.Id_Pago Cfg_Novedades.Id_Extras ~
Cfg_Novedades.Id_Salario Cfg_Novedades.indicador Cfg_Novedades.Estado ~
Cfg_Novedades.Programa Cfg_Novedades.Id_Valor Cfg_Novedades.Clase ~
Cfg_Novedades.Valor Cfg_Novedades.Id_SMLV Cfg_Novedades.Porc_Parafiscal 
&Scoped-define ENABLED-TABLES Cfg_Novedades
&Scoped-define FIRST-ENABLED-TABLE Cfg_Novedades
&Scoped-Define ENABLED-OBJECTS RECT-15 RECT-16 RECT-17 RECT-18 RECT-19 ~
RECT-20 RECT-21 RECT-22 RECT-23 BUTTON-6 BUTTON-7 BCon Btn_Salvar ~
Btn_Deshacer Btn_Ingresar Btn_Cancelar BtnDone 
&Scoped-Define DISPLAYED-FIELDS Cfg_Novedades.Tipo Cfg_Novedades.Naturaleza ~
Cfg_Novedades.Cuenta Cfg_Novedades.Codigo Cfg_Novedades.Nombre ~
Cfg_Novedades.Cta_Contrapartida Cfg_Novedades.Id_Base ~
Cfg_Novedades.Id_Descripcion Cfg_Novedades.Nit_Contable ~
Cfg_Novedades.Id_Pago Cfg_Novedades.Id_Extras Cfg_Novedades.Id_Salario ~
Cfg_Novedades.indicador Cfg_Novedades.Estado Cfg_Novedades.Programa ~
Cfg_Novedades.Id_Valor Cfg_Novedades.Clase Cfg_Novedades.Valor ~
Cfg_Novedades.Id_SMLV Cfg_Novedades.Num_SMLV Cfg_Novedades.PunAd_Calculo ~
Cfg_Novedades.Porc_Parafiscal 
&Scoped-define DISPLAYED-TABLES Cfg_Novedades
&Scoped-define FIRST-DISPLAYED-TABLE Cfg_Novedades
&Scoped-Define DISPLAYED-OBJECTS WNomCta NomCtaContra NomNit WNomIndicador 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 BUTTON-6 BUTTON-7 BCon Btn_Salvar Btn_Deshacer ~
Btn_Ingresar Btn_Cancelar BtnDone 
&Scoped-define List-2 Cfg_Novedades.Naturaleza Cfg_Novedades.Cuenta ~
Cfg_Novedades.Cta_Contrapartida Cfg_Novedades.Id_Base ~
Cfg_Novedades.Id_Descripcion Cfg_Novedades.Nit_Contable ~
Cfg_Novedades.Id_Pago Cfg_Novedades.Id_Extras Cfg_Novedades.Id_Salario ~
Cfg_Novedades.indicador Cfg_Novedades.Programa Cfg_Novedades.Id_Valor ~
Cfg_Novedades.Clase Cfg_Novedades.Valor Cfg_Novedades.Id_SMLV ~
Cfg_Novedades.Num_SMLV Cfg_Novedades.PunAd_Calculo ~
Cfg_Novedades.Porc_Parafiscal 
&Scoped-define List-3 Cfg_Novedades.Cta_Contrapartida ~
Cfg_Novedades.Nit_Contable 
&Scoped-define List-6 Cfg_Novedades.Id_Valor Cfg_Novedades.Valor ~
Cfg_Novedades.Id_SMLV Cfg_Novedades.Num_SMLV 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BCon 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 8" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 11 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Cancelar 
     LABEL "Cancelar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Deshacer 
     LABEL "Deshacer" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Ingresar 
     LABEL "Ingresar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-6 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 6" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-7 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 7" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE NomCtaContra AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomNit AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomCta AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 27 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WNomIndicador AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 31 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-15
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 46 BY 1.35.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 1.62.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 6.

DEFINE RECTANGLE RECT-18
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 5.92.

DEFINE RECTANGLE RECT-19
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 8.62.

DEFINE RECTANGLE RECT-20
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 5.38.

DEFINE RECTANGLE RECT-21
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 6.19.

DEFINE RECTANGLE RECT-22
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 83 BY 3.19.

DEFINE RECTANGLE RECT-23
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 41 BY 1.62.

DEFINE BUTTON BUTTON-25 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE RCon AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Todas", 1,
"Devengados", 2,
"Deducciones", 3,
"Informativo", 4,
"Provisiones", 5
     SIZE 46 BY 4.58 NO-UNDO.

DEFINE VARIABLE REstado AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Activas", 1,
"Inactivas", 2
     SIZE 22 BY .81 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BNovedades FOR 
      Cfg_Novedades SCROLLING.

DEFINE QUERY fMain FOR 
      Cfg_Novedades SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BNovedades
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BNovedades wWin _FREEFORM
  QUERY BNovedades DISPLAY
      Cfg_Novedades.Codigo
 Cfg_Novedades.Nombre
          Cfg_Novedades.Cuenta
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 46 BY 8.88
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .65 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     BUTTON-6 AT ROW 1.54 COL 99
     Cfg_Novedades.Tipo AT ROW 2.88 COL 5.43 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Devengado", 1,
"Deduccion", 2,
"Informativa", 3,
"Provisiones", 4
          SIZE 43.57 BY .92
     Cfg_Novedades.Naturaleza AT ROW 3.15 COL 53 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Débito", "DB":U,
"Crédito", "CR":U
          SIZE 25 BY .54
     BUTTON-7 AT ROW 3.15 COL 99
     Cfg_Novedades.Cuenta AT ROW 3.81 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     WNomCta AT ROW 3.81 COL 62 COLON-ALIGNED NO-LABEL
     Cfg_Novedades.Codigo AT ROW 4.5 COL 9 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 6 BY .81
          BGCOLOR 15 
     Cfg_Novedades.Nombre AT ROW 4.5 COL 15 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 32 BY .81
          BGCOLOR 15 
     BCon AT ROW 4.77 COL 99
     Cfg_Novedades.Cta_Contrapartida AT ROW 4.88 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     NomCtaContra AT ROW 4.88 COL 62 COLON-ALIGNED NO-LABEL
     Cfg_Novedades.Id_Base AT ROW 5.58 COL 11.14
          VIEW-AS TOGGLE-BOX
          SIZE 20.86 BY .65
     Cfg_Novedades.Id_Descripcion AT ROW 6.58 COL 11.14
          VIEW-AS TOGGLE-BOX
          SIZE 29.29 BY .65
     Cfg_Novedades.Nit_Contable AT ROW 7.08 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 15 
     NomNit AT ROW 7.08 COL 62.72 COLON-ALIGNED NO-LABEL
     Btn_Salvar AT ROW 7.19 COL 99
     Cfg_Novedades.Id_Pago AT ROW 7.58 COL 11.14
          VIEW-AS TOGGLE-BOX
          SIZE 21 BY .65
     Cfg_Novedades.Id_Extras AT ROW 8.58 COL 11
          LABEL "Es concepto de Horas Extras?"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .65
     Btn_Deshacer AT ROW 8.81 COL 99
     Cfg_Novedades.Id_Salario AT ROW 9.58 COL 11.14
          LABEL "Hace parte del Salario?"
          VIEW-AS TOGGLE-BOX
          SIZE 24 BY .65
     Cfg_Novedades.indicador AT ROW 9.62 COL 51 COLON-ALIGNED NO-LABEL
          VIEW-AS FILL-IN 
          SIZE 7 BY .81
          BGCOLOR 15 
     WNomIndicador AT ROW 9.62 COL 58 COLON-ALIGNED NO-LABEL
     Cfg_Novedades.Estado AT ROW 10.42 COL 11 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 18 BY .81
     Btn_Ingresar AT ROW 10.42 COL 99
     Cfg_Novedades.Programa AT ROW 10.96 COL 58 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 32 BY .81
          BGCOLOR 15 
     Btn_Cancelar AT ROW 12.08 COL 99
     Cfg_Novedades.Id_Valor AT ROW 12.85 COL 52 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Porcentaje", 1,
"Valor", 2,
"Manual", 3
          SIZE 27 BY .81
     Cfg_Novedades.Clase AT ROW 13.12 COL 10 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Liquidación Nómina Normal", 1,
"Novedad Exporadica en Liquidación de Nómina", 2,
"Cambio en Información del Maestro de Empleados", 3,
"Deducción para Productos", 4,
"Deducción de Opción Variable (Seg.Soc)", 5,
"Proceso Especial", 6
          SIZE 37.29 BY 4.58
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.43 BY 20.73
         BGCOLOR 17 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME fMain
     BtnDone AT ROW 13.69 COL 99
     Cfg_Novedades.Valor AT ROW 13.92 COL 56 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Cfg_Novedades.Id_SMLV AT ROW 13.92 COL 69
          VIEW-AS TOGGLE-BOX
          SIZE 18.57 BY .81
     Cfg_Novedades.Num_SMLV AT ROW 14.73 COL 83 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.14 BY .81
          BGCOLOR 15 
     Cfg_Novedades.PunAd_Calculo AT ROW 15.62 COL 89.14 RIGHT-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 5.14 BY .81
          BGCOLOR 15 
     Cfg_Novedades.Porc_Parafiscal AT ROW 17 COL 69 COLON-ALIGNED
          LABEL "Porcentaje Empleador"
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     "  Recuerde" VIEW-AS TEXT
          SIZE 83 BY .81 AT ROW 18.38 COL 9
          BGCOLOR 18 FGCOLOR 15 
     "  Indicador para prestamo rotativo" VIEW-AS TEXT
          SIZE 41 BY .81 AT ROW 8.54 COL 51
          BGCOLOR 18 FGCOLOR 15 
     "-  Usar los códigos de 800 al 999 para las provisiones" VIEW-AS TEXT
          SIZE 38 BY .5 AT ROW 19.46 COL 53
     "  Información para el asiento contable" VIEW-AS TEXT
          SIZE 41 BY .81 AT ROW 1.81 COL 51
          BGCOLOR 18 FGCOLOR 15 
     " Identifique el comportamiento del concepto frente al salario" VIEW-AS TEXT
          SIZE 41 BY .81 AT ROW 12.04 COL 51
          BGCOLOR 18 FGCOLOR 15 
     "  Clase de novedad" VIEW-AS TEXT
          SIZE 41 BY .81 AT ROW 12.04 COL 9
          BGCOLOR 18 FGCOLOR 15 
     "-  Usar los códigos del 300 al 499 para las deducciones" VIEW-AS TEXT
          SIZE 38 BY .54 AT ROW 20.08 COL 11
     "-  Usar los códigos del 500 al 799 novedades informativas" VIEW-AS TEXT
          SIZE 41 BY .54 AT ROW 20.69 COL 11
     "  Tipo" VIEW-AS TEXT
          SIZE 46 BY .81 AT ROW 1.81 COL 4
          BGCOLOR 18 FGCOLOR 15 
     "-  Usar los códigos de 100 al 299 para los devengados" VIEW-AS TEXT
          SIZE 38 BY .54 AT ROW 19.46 COL 11
     "  Nit donde quedará registrada la partida contable" VIEW-AS TEXT
          SIZE 41 BY .81 AT ROW 6 COL 51
          BGCOLOR 18 FGCOLOR 15 
     RECT-15 AT ROW 2.62 COL 4
     RECT-16 AT ROW 9.23 COL 51
     RECT-17 AT ROW 2.27 COL 51
     RECT-18 AT ROW 12.31 COL 9
     RECT-19 AT ROW 6.92 COL 98
     RECT-20 AT ROW 1.27 COL 98
     RECT-21 AT ROW 12.04 COL 51
     RECT-22 AT ROW 18.27 COL 9
     RECT-23 AT ROW 16.62 COL 51
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 111.43 BY 20.73
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME FNovedades
     RCon AT ROW 1 COL 3 NO-LABEL
     BNovedades AT ROW 5.85 COL 3
     BUTTON-25 AT ROW 15 COL 34
     REstado AT ROW 15.27 COL 3 NO-LABEL
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 29 ROW 3.15
         SIZE 50 BY 16.42
         BGCOLOR 17 FONT 4
         TITLE "Consulta de Novedades".


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
         TITLE              = "Cfg de Novedades de Nomina y Recursos Humanos"
         HEIGHT             = 20.73
         WIDTH              = 111.43
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
ASSIGN FRAME FNovedades:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
                                                                        */
/* SETTINGS FOR BUTTON BCon IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR BUTTON BtnDone IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Deshacer IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Ingresar IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-6 IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR BUTTON BUTTON-7 IN FRAME fMain
   1                                                                    */
/* SETTINGS FOR RADIO-SET Cfg_Novedades.Clase IN FRAME fMain
   2                                                                    */
/* SETTINGS FOR FILL-IN Cfg_Novedades.Codigo IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_Novedades.Cta_Contrapartida IN FRAME fMain
   2 3 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN Cfg_Novedades.Cuenta IN FRAME fMain
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cfg_Novedades.Id_Base IN FRAME fMain
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cfg_Novedades.Id_Descripcion IN FRAME fMain
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cfg_Novedades.Id_Extras IN FRAME fMain
   2 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Cfg_Novedades.Id_Pago IN FRAME fMain
   2                                                                    */
/* SETTINGS FOR TOGGLE-BOX Cfg_Novedades.Id_Salario IN FRAME fMain
   2 EXP-LABEL                                                          */
/* SETTINGS FOR TOGGLE-BOX Cfg_Novedades.Id_SMLV IN FRAME fMain
   2 6                                                                  */
/* SETTINGS FOR RADIO-SET Cfg_Novedades.Id_Valor IN FRAME fMain
   2 6                                                                  */
/* SETTINGS FOR FILL-IN Cfg_Novedades.indicador IN FRAME fMain
   2                                                                    */
/* SETTINGS FOR RADIO-SET Cfg_Novedades.Naturaleza IN FRAME fMain
   2                                                                    */
/* SETTINGS FOR FILL-IN Cfg_Novedades.Nit_Contable IN FRAME fMain
   2 3 EXP-LABEL                                                        */
/* SETTINGS FOR FILL-IN NomCtaContra IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomNit IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_Novedades.Num_SMLV IN FRAME fMain
   NO-ENABLE 2 6                                                        */
/* SETTINGS FOR FILL-IN Cfg_Novedades.Porc_Parafiscal IN FRAME fMain
   2 EXP-LABEL                                                          */
/* SETTINGS FOR FILL-IN Cfg_Novedades.Programa IN FRAME fMain
   2                                                                    */
/* SETTINGS FOR FILL-IN Cfg_Novedades.PunAd_Calculo IN FRAME fMain
   NO-ENABLE ALIGN-R 2                                                  */
/* SETTINGS FOR FILL-IN Cfg_Novedades.Valor IN FRAME fMain
   2 6                                                                  */
/* SETTINGS FOR FILL-IN WNomCta IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WNomIndicador IN FRAME fMain
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME FNovedades
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BNovedades RCon FNovedades */
ASSIGN 
       FRAME FNovedades:HIDDEN           = TRUE
       FRAME FNovedades:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BNovedades
/* Query rebuild information for BROWSE BNovedades
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Estado EQ REstado.
     _END_FREEFORM
     _Query            is OPENED
*/  /* BROWSE BNovedades */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME fMain
/* Query rebuild information for FRAME fMain
     _TblList          = "bdcentral.Cfg_Novedades"
     _Query            is OPENED
*/  /* FRAME fMain */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Cfg de Novedades de Nomina y Recursos Humanos */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Cfg de Novedades de Nomina y Recursos Humanos */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCon wWin
ON CHOOSE OF BCon IN FRAME fMain /* Button 8 */
DO:
  ASSIGN FRAME FNovedades REstado RCon.
  ENABLE ALL WITH FRAME FNovedades.
  CASE RCon:
      WHEN 1 THEN
        OPEN QUERY BNovedades FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Estado EQ REstado BY Cfg_Novedades.Tipo  BY Cfg_Novedades.Codigo INDEXED-REPOSITION.
      WHEN 2 THEN
        OPEN QUERY BNovedades FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Tipo EQ 1 AND Cfg_Novedades.Estado EQ REstado BY Cfg_Novedades.Tipo  BY Cfg_Novedades.Codigo INDEXED-REPOSITION.
      WHEN 3 THEN
        OPEN QUERY BNovedades FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Tipo EQ 2 AND Cfg_Novedades.Estado EQ REstado BY Cfg_Novedades.Tipo  BY Cfg_Novedades.Codigo INDEXED-REPOSITION.
      WHEN 4 THEN
        OPEN QUERY BNovedades FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Tipo EQ 3 AND Cfg_Novedades.Estado EQ REstado BY Cfg_Novedades.Tipo  BY Cfg_Novedades.Codigo INDEXED-REPOSITION.
      WHEN 5 THEN
        OPEN QUERY BNovedades FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Tipo EQ 4 AND Cfg_Novedades.Estado EQ REstado BY Cfg_Novedades.Tipo  BY Cfg_Novedades.Codigo INDEXED-REPOSITION.
  END CASE.
  VIEW FRAME FNovedades.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BNovedades
&Scoped-define FRAME-NAME FNovedades
&Scoped-define SELF-NAME BNovedades
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BNovedades wWin
ON MOUSE-SELECT-CLICK OF BNovedades IN FRAME FNovedades
DO:
  RUN Mostrar_Registro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain /* Salir */
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


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME fMain /* Cancelar */
DO:
  ENABLE Btn_Ingresar WITH FRAME Fmain.
  IF W_Nvo THEN DO:
     FIND CURRENT Cfg_Novedades NO-LOCK NO-ERROR.
     IF AVAILABLE Cfg_Novedades THEN RUN Mostrar_Registro.
     ELSE DO: 
         DISABLE ALL WITH FRAME FMain.
         ENABLE {&List-1} WITH FRAME FMain.
         RUN Inicializar_Pantalla.
     END.
     W_Nvo = NO.
  END.
  ELSE APPLY "choose" TO Btn_Deshacer.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME fMain /* Deshacer */
DO:
DO WITH FRAME FMain:
  FIND CURRENT CFG_Novedades.
  IF NOT W_Nvo THEN DO:
    FIND FIRST Cfg_NovedadesT.
    ASSIGN Cfg_Novedades.Clase:SCREEN-VALUE          = STRING(Cfg_NovedadesT.Clase)
           Cfg_Novedades.Codigo:SCREEN-VALUE         = STRING(Cfg_NovedadesT.Codigo)
           Cfg_Novedades.Cuenta:SCREEN-VALUE         = STRING(Cfg_NovedadesT.Cuenta)
           Cfg_Novedades.Id_Descripcion:SCREEN-VALUE = STRING(Cfg_NovedadesT.Id_Descripcion)
           Cfg_Novedades.indicador:SCREEN-VALUE      = STRING(Cfg_NovedadesT.Indicador)
           Cfg_Novedades.Naturaleza:SCREEN-VALUE     = STRING(Cfg_NovedadesT.Naturaleza)
           Cfg_Novedades.Nombre:SCREEN-VALUE         = STRING(Cfg_NovedadesT.Nombre)
           Cfg_Novedades.Programa:SCREEN-VALUE       = STRING(Cfg_NovedadesT.Programa)
           Cfg_Novedades.Tipo:SCREEN-VALUE           = STRING(Cfg_NovedadesT.Tipo)
           /*Cfg_Novedades.Id_Ingreso:SCREEN-VALUE     = STRING(Cfg_NovedadesT.Id_Ingreso)*/
           Cfg_Novedades.Id_Base:SCREEN-VALUE        = STRING(Cfg_NovedadesT.Id_Base)
           Cfg_Novedades.Id_Valor:SCREEN-VALUE       = STRING(Cfg_NovedadesT.Id_Valor)
           Cfg_Novedades.Valor:SCREEN-VALUE          = STRING(Cfg_NovedadesT.Valor)
           Cfg_Novedades.Id_SMLV:SCREEN-VALUE        = STRING(Cfg_NovedadesT.Id_SMLV)
           Cfg_Novedades.Num_SMLV:SCREEN-VALUE       = STRING(Cfg_NovedadesT.Num_SMLV)
           Cfg_Novedades.PunAd_Calculo:SCREEN-VALUE  = STRING(Cfg_NovedadesT.PunAd_Calculo).
    IF Cfg_Novedades.Id_SMLV THEN
       ENABLE Cfg_Novedades.Num_SMLV Cfg_Novedades.PunAd_Calculo WITH FRAME FMain.
    ELSE
       DISABLE Cfg_Novedades.Num_SMLV Cfg_Novedades.PunAd_Calculo WITH FRAME FMain.
    FIND Cuentas WHERE Cuentas.Cuenta EQ Cfg_NovedadesT.Cuenta NO-LOCK NO-ERROR.
    IF AVAILABLE Cuentas THEN WNomCta:SCREEN-VALUE = Cuentas.nombre.
    ELSE WNomCta:SCREEN-VALUE = "".
    FIND Indicadores WHERE Indicadores.Indicador EQ Cfg_NovedadesT.Indicador NO-LOCK NO-ERROR.
    IF AVAILABLE Indicadores THEN ASSIGN WNomIndicador:SCREEN-VALUE = Indicadores.Nombre.
    ELSE WNomIndicador:SCREEN-VALUE = "".
  END.
  ELSE DO:
    FIND LAST Cfg_Novedades NO-LOCK NO-ERROR.
    IF AVAILABLE Cfg_Novedades THEN RUN Mostrar_Registro.
    ELSE RUN Inicializar_Pantalla.
    W_Nvo = NO.
  END.
END.
DISABLE Cfg_Novedades.Codigo WITH FRAME FMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME fMain /* Ingresar */
DO:
  W_Nvo = YES.
  RUN Inicializar_Pantalla.
  RUN Buscar_Consecutivo.
  ENABLE ALL WITH FRAME FMain.
  DISABLE WNomCta NomCtaContra NomNit WNomIndicador WITH FRAME FMain.
  DISABLE Btn_Ingresar WITH FRAME FMain.
  APPLY "value-changed" TO Cfg_Novedades.Tipo.
  APPLY "Entry" TO Cfg_Novedades.Tipo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME fMain /* Salvar */
DO:

  ENABLE Btn_Ingresar.
  IF W_Nvo THEN DO:
      CREATE Cfg_Novedades.
      ASSIGN FRAME FMain Cfg_Novedades.Codigo.
  END.
  ELSE FIND Cfg_Novedades WHERE Cfg_Novedades.Codigo EQ INTEGER(CfG_Novedades.Codigo:SCREEN-VALUE IN FRAME fmain).
  
  ASSIGN FRAME FMain
    Cfg_Novedades.Porc_Parafiscal
    Cfg_Novedades.Nit_Contable
    Cfg_Novedades.Cta_Contrapartida
    Cfg_Novedades.Clase
    Cfg_Novedades.Id_Salario
    Cfg_Novedades.Cuenta
    Cfg_Novedades.Estado
    Cfg_Novedades.Id_Descripcion
    Cfg_Novedades.indicador
    Cfg_Novedades.Naturaleza
    Cfg_Novedades.Nombre
    Cfg_Novedades.Programa
    Cfg_Novedades.Tipo
    /*Cfg_Novedades.Id_Ingreso*/
    Cfg_Novedades.Id_Base
    Cfg_Novedades.Id_Valor
    Cfg_Novedades.Valor
    Cfg_Novedades.Id_SMLV
    Cfg_Novedades.Num_SMLV
    Cfg_Novedades.PunAd_Calculo
    Cfg_Novedades.Id_Pago
    Cfg_Novedades.Id_Extras.
  W_Nvo = NO.
  FOR EACH Cfg_NovedadesT: DELETE Cfg_NovedadesT. END.
  RUN Mostrar_Registro.
  DISABLE Cfg_Novedades.Codigo WITH FRAME Fmain.
  ENABLE Btn_Ingresar WITH FRAME FMain.
  /*APPLY "choose" TO BCon.*/
  HIDE FRAME FNovedades.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FNovedades
&Scoped-define SELF-NAME BUTTON-25
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-25 wWin
ON CHOOSE OF BUTTON-25 IN FRAME FNovedades /* Ocultar */
DO:
  HIDE FRAME FNovedades.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON CHOOSE OF BUTTON-7 IN FRAME fMain /* Button 7 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Empresas.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_Novedades.Clase
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_Novedades.Clase wWin
ON VALUE-CHANGED OF Cfg_Novedades.Clase IN FRAME fMain /* Clase */
DO:
  ENABLE {&List-6} WITH FRAME FMain.
  DISABLE Cfg_Novedades.Porc_Parafiscal.
  IF SELF:SCREEN-VALUE EQ "6" THEN DO:
     DISABLE {&List-6} WITH FRAME FMain.
     APPLY "entry" TO Cfg_Novedades.Programa.
     RETURN NO-APPLY.
  END.
  IF SELF:SCREEN-VALUE EQ "5" THEN 
     ENABLE Cfg_Novedades.Porc_Parafiscal.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_Novedades.Codigo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_Novedades.Codigo wWin
ON LEAVE OF Cfg_Novedades.Codigo IN FRAME fMain /* Codigo */
DO:
  FIND FIRST Cfg_Novedades WHERE Cfg_Novedades.Codigo EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  IF AVAILABLE Cfg_Novedades THEN DO:
     MESSAGE "Ya existe un codigo de novedad con este número" SKIP
             "intente entrando otro número diferente no existente" VIEW-AS ALERT-BOX.
     SELF:SCREEN-VALUE = "00000".
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_Novedades.Cta_Contrapartida
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_Novedades.Cta_Contrapartida wWin
ON LEAVE OF Cfg_Novedades.Cta_Contrapartida IN FRAME fMain /* Cta_Contrapartida */
DO:
  DO WITH FRAME {&FRAME-NAME}:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN DO:
      NomCtaContra:SCREEN-VALUE = Cuentas.nombre.
      IF Cuentas.Tipo EQ 1 THEN DO:
         MESSAGE "La cuenta ha escoger no debe ser una cuenta mayor." SKIP
                 "digite de nuevo la cuenta." VIEW-AS ALERT-BOX.
         RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                         OUTPUT W_CtrNat, INPUT "M").
         ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
                NomCtacontra:SCREEN-VALUE = W_Pnombre.
      END.
  END.
     
  ELSE DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
            NomCtacontra:SCREEN-VALUE = W_Pnombre.
  END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_Novedades.Cuenta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_Novedades.Cuenta wWin
ON LEAVE OF Cfg_Novedades.Cuenta IN FRAME fMain /* Cuenta */
DO:
DO WITH FRAME {&FRAME-NAME}:
  FIND Cuentas WHERE Cuentas.Cuenta EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE Cuentas THEN DO:
      WNomCta:SCREEN-VALUE = Cuentas.nombre.
      IF Cuentas.Tipo EQ 1 THEN DO:
         MESSAGE "La cuenta ha escoger no debe ser una cuenta mayor." SKIP
                 "digite de nuevo la cuenta." VIEW-AS ALERT-BOX.
         RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                         OUTPUT W_CtrNat, INPUT "M").
         ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
                WNomCta:SCREEN-VALUE = W_Pnombre.
      END.
  END.
     
  ELSE DO:
     RUN C-Cuentas.r (OUTPUT W_Pcuenta, OUTPUT W_Pnombre, OUTPUT W_Naturaleza, 
                     OUTPUT W_CtrNat, INPUT "M").
     ASSIGN SELF:SCREEN-VALUE IN FRAME {&FRAME-NAME}    = W_Pcuenta
            WNomCta:SCREEN-VALUE = W_Pnombre.
  END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_Novedades.Id_SMLV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_Novedades.Id_SMLV wWin
ON VALUE-CHANGED OF Cfg_Novedades.Id_SMLV IN FRAME fMain /* Calculo segun SMLV? */
DO:
  IF Cfg_Novedades.Id_SMLV:SCREEN-VALUE IN FRAME FMain EQ "yes" THEN
     ENABLE Cfg_Novedades.Num_SMLV Cfg_Novedades.PunAd_Calculo WITH FRAME FMain.
  ELSE
     DISABLE Cfg_Novedades.Num_SMLV Cfg_Novedades.PunAd_Calculo WITH FRAME FMain.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_Novedades.Id_Valor
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_Novedades.Id_Valor wWin
ON VALUE-CHANGED OF Cfg_Novedades.Id_Valor IN FRAME fMain /* Id_Valor */
DO:
  /*IF SELF:SCREEN-VALUE NE "3" THEN
     ENABLE Cfg_Novedades.Valor WITH FRAME FMain.
  ELSE DO:
     DISABLE Cfg_Novedades.Valor WITH FRAME FMain.
     Cfg_Novedades.Valor:SCREEN-VALUE = "0.00".
  END.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_Novedades.indicador
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_Novedades.indicador wWin
ON LEAVE OF Cfg_Novedades.indicador IN FRAME fMain /* Código de Indicador */
DO:
DO WITH FRAME FMain:
   FIND Indicadores WHERE Indicadores.Indicador EQ INTEGER(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
   IF AVAILABLE Indicadores THEN
      ASSIGN WNomIndicador:SCREEN-VALUE = Indicadores.Nombre.
   ELSE DO:
      RUN C-Indicadores.r (OUTPUT P_CodInd, OUTPUT P_NomInd, OUTPUT P_ValTas, OUTPUT P_Valor).
      ASSIGN Cfg_Novedades.Indicador:SCREEN-VALUE   = STRING(P_CodInd).
             WNomIndicador:SCREEN-VALUE             = P_NomInd.
   END.
END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_Novedades.Nit_Contable
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_Novedades.Nit_Contable wWin
ON LEAVE OF Cfg_Novedades.Nit_Contable IN FRAME fMain /* Nit_Contable */
DO:
  FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
  IF AVAILABLE (Clientes) THEN
     ASSIGN NomNit:SCREEN-VALUE IN FRAME FMain = Clientes.Nombre.
  ELSE
  DO:
    RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
    ASSIGN NomNit:SCREEN-VALUE IN FRAME {&FRAME-NAME} = P_Nombre
           Cfg_Novedades.Nit_Contable:SCREEN-VALUE    IN FRAME {&FRAME-NAME} = P_Nit.
    FIND Clientes WHERE Clientes.Agencia EQ P_AgeCli AND Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
  END.
  IF AVAILABLE(Clientes) THEN DO:
   IF Clientes.Tipo_Identificacion NE "NIT" THEN DO:
       MESSAGE "Debe ser escogida una Persona Jurídica" VIEW-AS ALERT-BOX.
       RETURN NO-APPLY.
   END.
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FNovedades
&Scoped-define SELF-NAME RCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RCon wWin
ON VALUE-CHANGED OF RCon IN FRAME FNovedades
DO:
  APPLY "choose" TO BCon IN FRAME FMain.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME REstado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL REstado wWin
ON VALUE-CHANGED OF REstado IN FRAME FNovedades
DO:
  ASSIGN FRAME FNovedades REstado.
  OPEN QUERY BNovedades FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Estado EQ REstado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME Cfg_Novedades.Tipo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_Novedades.Tipo wWin
ON VALUE-CHANGED OF Cfg_Novedades.Tipo IN FRAME fMain /* Tipo */
DO:
  RUN Buscar_Consecutivo.
  RUN Campos_ActivosInactivos(INPUT INTEGER(Cfg_Novedades.Tipo:SCREEN-VALUE),
                              INTEGER(Cfg_Novedades.Clase:SCREEN-VALUE)).
IF W_IdPagoExiste THEN DISABLE Cfg_Novedades.Id_Pago WITH FRAME FMain.
IF W_IdBaseExiste THEN DISABLE Cfg_Novedades.Id_Base WITH FRAME FMain.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Consecutivo wWin 
PROCEDURE Buscar_Consecutivo :
FIND LAST Cfg_Novedades WHERE 
          Cfg_Novedades.Tipo EQ INTEGER(Cfg_Novedades.Tipo:SCREEN-VALUE IN FRAME Fmain)
          NO-LOCK NO-ERROR.
IF AVAILABLE Cfg_Novedades THEN DO:
   Cfg_Novedades.Codigo:SCREEN-VALUE = STRING(Cfg_Novedades.Codigo + 1,"99999").
END.
ELSE DO:
  IF Cfg_Novedades.Tipo:SCREEN-VALUE IN FRAME Fmain EQ "3" THEN
     Cfg_Novedades.Codigo:SCREEN-VALUE = "00500".
  IF Cfg_Novedades.Tipo:SCREEN-VALUE IN FRAME Fmain EQ "2" THEN
     Cfg_Novedades.Codigo:SCREEN-VALUE = "00300".
  IF Cfg_Novedades.Tipo:SCREEN-VALUE IN FRAME Fmain EQ "1" THEN
     Cfg_Novedades.Codigo:SCREEN-VALUE = "00100".
  IF Cfg_Novedades.Tipo:SCREEN-VALUE IN FRAME Fmain EQ "4" THEN
     Cfg_Novedades.Codigo:SCREEN-VALUE = "00800".

END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Campos_ActivosInactivos wWin 
PROCEDURE Campos_ActivosInactivos :
DEFINE INPUT PARAMETER Tip LIKE Cfg_Novedades.Tipo.
DEFINE INPUT PARAMETER Cla LIKE Cfg_Novedades.Clase.

  IF Tip EQ 3 THEN DO:
    Cfg_Novedades.Clase:SCREEN-VALUE IN FRAME Fmain = "3".
    DISABLE {&List-2} WITH FRAME FMain.
  END.
  ELSE 
    ENABLE {&List-2} WITH FRAME FMain.
  IF Tip NE 2 AND Tip NE 4 THEN
     DISABLE Cfg_Novedades.Nit_Contable Cfg_Novedades.Cta_Contrapartida Cfg_Novedades.Porc_Parafiscal
         WITH FRAME Fmain.
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
  DISPLAY WNomCta NomCtaContra NomNit WNomIndicador 
      WITH FRAME fMain IN WINDOW wWin.
  IF AVAILABLE Cfg_Novedades THEN 
    DISPLAY Cfg_Novedades.Tipo Cfg_Novedades.Naturaleza Cfg_Novedades.Cuenta 
          Cfg_Novedades.Codigo Cfg_Novedades.Nombre 
          Cfg_Novedades.Cta_Contrapartida Cfg_Novedades.Id_Base 
          Cfg_Novedades.Id_Descripcion Cfg_Novedades.Nit_Contable 
          Cfg_Novedades.Id_Pago Cfg_Novedades.Id_Extras Cfg_Novedades.Id_Salario 
          Cfg_Novedades.indicador Cfg_Novedades.Estado Cfg_Novedades.Programa 
          Cfg_Novedades.Id_Valor Cfg_Novedades.Clase Cfg_Novedades.Valor 
          Cfg_Novedades.Id_SMLV Cfg_Novedades.Num_SMLV 
          Cfg_Novedades.PunAd_Calculo Cfg_Novedades.Porc_Parafiscal 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE RECT-15 RECT-16 RECT-17 RECT-18 RECT-19 RECT-20 RECT-21 RECT-22 
         RECT-23 BUTTON-6 Cfg_Novedades.Tipo Cfg_Novedades.Naturaleza BUTTON-7 
         Cfg_Novedades.Cuenta Cfg_Novedades.Nombre BCon 
         Cfg_Novedades.Cta_Contrapartida Cfg_Novedades.Id_Base 
         Cfg_Novedades.Id_Descripcion Cfg_Novedades.Nit_Contable Btn_Salvar 
         Cfg_Novedades.Id_Pago Cfg_Novedades.Id_Extras Btn_Deshacer 
         Cfg_Novedades.Id_Salario Cfg_Novedades.indicador Cfg_Novedades.Estado 
         Btn_Ingresar Cfg_Novedades.Programa Btn_Cancelar 
         Cfg_Novedades.Id_Valor Cfg_Novedades.Clase BtnDone Cfg_Novedades.Valor 
         Cfg_Novedades.Id_SMLV Cfg_Novedades.Porc_Parafiscal 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY RCon REstado 
      WITH FRAME FNovedades IN WINDOW wWin.
  ENABLE RCon BNovedades BUTTON-25 REstado 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Pantalla wWin 
PROCEDURE Inicializar_Pantalla :
DO WITH FRAME FMain:
    ASSIGN Cfg_Novedades.Clase:SCREEN-VALUE          = "1"
           Cfg_Novedades.Codigo:SCREEN-VALUE         = "000"
           Cfg_Novedades.Cuenta:SCREEN-VALUE         = ""
           Cfg_Novedades.Id_Descripcion:SCREEN-VALUE = "No"
           Cfg_Novedades.indicador:SCREEN-VALUE      = "0"
           Cfg_Novedades.Naturaleza:SCREEN-VALUE     = "DB"
           Cfg_Novedades.Nombre:SCREEN-VALUE         = ""
           Cfg_Novedades.Programa:SCREEN-VALUE       = ""
           Cfg_Novedades.Tipo:SCREEN-VALUE           = "1"
           WNomIndicador:SCREEN-VALUE                = ""
           WNomCta:SCREEN-VALUE                      = ""
           /*Cfg_Novedades.Id_Ingreso:SCREEN-VALUE     = "NO"*/
           Cfg_Novedades.Id_Base:SCREEN-VALUE        = "NO"
           Cfg_Novedades.Id_Valor:SCREEN-VALUE       = "1"
           Cfg_Novedades.Valor:SCREEN-VALUE          = "0.00"
           Cfg_Novedades.Id_SMLV:SCREEN-VALUE        = "no"
           Cfg_Novedades.Num_SMLV:SCREEN-VALUE       = "0"
           Cfg_Novedades.PunAd_Calculo:SCREEN-VALUE  = "0.00"
           Cfg_Novedades.Porc_Parafiscal:SCREEN-VALUE = "0.00"
           Cfg_Novedades.Nit_Contable:SCREEN-VALUE   = ""
           Cfg_Novedades.Cta_Contrapartida:SCREEN-VALUE  = ""
           Cfg_Novedades.Id_Extras:SCREEN-VALUE      = "no"
           Cfg_Novedades.Id_Salario:SCREEN-VALUE     = "no"
           Cfg_Novedades.Id_Pago:SCREEN-VALUE        = "no"
           NomNit:SCREEN-VALUE = ""
           NomCtaContra:SCREEN-VALUE = "".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
  RUN Inicializar_Pantalla.
  FIND Cfg_Novedades WHERE Cfg_Novedades.Id_Pago NO-LOCK NO-ERROR.
  IF AVAILABLE Cfg_Novedades THEN
     W_IdPagoExiste = YES.
  FIND Cfg_Novedades WHERE Cfg_Novedades.Id_Base NO-LOCK NO-ERROR.
  IF AVAILABLE Cfg_Novedades THEN
     W_IdBaseExiste = YES.
  FIND LAST Cfg_Novedades NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Cfg_Novedades THEN DO:
     DISABLE ALL WITH FRAME FMain.
     ENABLE {&List-1} WITH FRAME FMain.
  END.
  ELSE RUN Mostrar_Registro.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Registro wWin 
PROCEDURE Mostrar_Registro :
FOR EACH Cfg_NovedadesT: DELETE Cfg_NovedadesT. END.
ASSIGN WNomCta:SCREEN-VALUE IN FRAME FMain = ""
       WNomIndicador:SCREEN-VALUE = ""
       NomNit:SCREEN-VALUE = ""
       NomCtaContra:SCREEN-VALUE = "".
RUN Registro_BackUp.
DISPLAY Cfg_Novedades.Clase
        Cfg_Novedades.Porc_Parafiscal
        Cfg_Novedades.Nit_Contable
        Cfg_Novedades.Cta_Contrapartida
        Cfg_Novedades.Codigo
        Cfg_Novedades.Cuenta
        Cfg_Novedades.Id_Descripcion
        Cfg_Novedades.indicador
        Cfg_Novedades.Naturaleza
        Cfg_Novedades.Nombre
        Cfg_Novedades.Programa
        Cfg_Novedades.Id_Salario
        /*Cfg_Novedades.Id_Ingreso*/
        Cfg_Novedades.Id_Base
        Cfg_Novedades.Tipo
        Cfg_Novedades.Id_Valor
        Cfg_Novedades.Valor
        Cfg_Novedades.Id_SMLV
        Cfg_Novedades.Num_SMLV
        Cfg_Novedades.Id_Pago
        Cfg_Novedades.Id_Extras
        Cfg_Novedades.Estado
        Cfg_Novedades.PunAd_Calculo WITH FRAME FMain.
FIND Cuentas WHERE Cuentas.Cuenta EQ Cfg_Novedades.Cuenta NO-LOCK NO-ERROR.
IF AVAILABLE Cuentas THEN WNomCta:SCREEN-VALUE = Cuentas.nombre.

FIND Cuentas WHERE Cuentas.Cuenta EQ Cfg_Novedades.Cta_Contrapartida NO-LOCK NO-ERROR.
IF AVAILABLE Cuentas THEN NomCtaContra:SCREEN-VALUE = Cuentas.nombre.

FIND Clientes WHERE Clientes.Nit EQ Cfg_Novedades.Nit_Contable NO-LOCK NO-ERROR.
IF AVAILABLE Clientes THEN NomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.


FIND Indicadores WHERE Indicadores.Indicador EQ Cfg_Novedades.Indicador NO-LOCK NO-ERROR.
IF AVAILABLE Indicadores THEN ASSIGN WNomIndicador:SCREEN-VALUE = Indicadores.Nombre.
IF Cfg_Novedades.Id_SMLV THEN
   ENABLE Cfg_Novedades.Num_SMLV Cfg_Novedades.PunAd_Calculo WITH FRAME FMAin.
ELSE
   DISABLE Cfg_Novedades.Num_SMLV Cfg_Novedades.PunAd_Calculo WITH FRAME FMAin.

/*IF Cfg_Novedades.Id_Valor:SCREEN-VALUE NE "3" THEN
   ENABLE Cfg_Novedades.Valor WITH FRAME FMain.
ELSE DO:
   DISABLE Cfg_Novedades.Valor WITH FRAME FMain.
   Cfg_Novedades.Valor:SCREEN-VALUE = "0".
END.*/
ENABLE {&List-6} WITH FRAME FMain.
IF Cfg_Novedades.Clase EQ 6 THEN DISABLE {&List-6} WITH FRAME FMain.

  IF Cfg_Novedades.Tipo EQ 3 THEN DO:
    Cfg_Novedades.Clase:SCREEN-VALUE = "3".
    DISABLE {&List-2} WITH FRAME FMain.
  END.
  ELSE 
    ENABLE {&List-2} WITH FRAME FMain.

  RUN Campos_ActivosInactivos(INPUT Cfg_Novedades.Tipo,
                              Cfg_Novedades.Clase).

IF W_IdPagoExiste THEN DISABLE Cfg_Novedades.Id_Pago WITH FRAME FMain.
IF W_IdBaseExiste THEN DISABLE Cfg_Novedades.Id_Base WITH FRAME FMain.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
DEFINE VAR W_NomEmp AS CHARACTER FORMAT "X(40)".
{INCLUIDO\RepEncabezado.I}    
  W_Reporte    = "REPORTE   : CONFIGURACION DE NOVEDADES - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "T  COD  NOMBRE                                   NAT  CUENTA       INDIC CLASE".

  DEFINE VAR Des AS CHARACTER FORMAT "X(10)".
    
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH Cfg_Novedades WHERE Cfg_Novedades.Estado EQ 1 BREAK BY Cfg_Novedades.Tipo BY Cfg_Novedades.Codigo:
     DISPLAY Cfg_Novedades.Tipo 
             Cfg_Novedades.Codigo
             Cfg_Novedades.Nombre
             Cfg_Novedades.Naturaleza
             Cfg_Novedades.Cuenta
             Cfg_Novedades.indicador
             Cfg_Novedades.Clase
     WITH FRAME F-Detalle WIDTH 200 NO-LABELS STREAM-IO USE-TEXT NO-BOX.
  END.  
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Registro_Backup wWin 
PROCEDURE Registro_Backup :
CREATE Cfg_NovedadesT.
ASSIGN Cfg_NovedadesT.Clase            = Cfg_Novedades.Clase
        Cfg_NovedadesT.Codigo          = Cfg_Novedades.Codigo
        Cfg_NovedadesT.Cuenta          = Cfg_Novedades.Cuenta
        Cfg_NovedadesT.Id_Descripcion  = Cfg_Novedades.Id_Descripcion
        Cfg_NovedadesT.indicador       = Cfg_Novedades.Indicador
        Cfg_NovedadesT.Naturaleza      = Cfg_Novedades.Naturaleza
        Cfg_NovedadesT.Nombre          = Cfg_Novedades.Nombre
        Cfg_NovedadesT.Programa        = Cfg_Novedades.Programa
        Cfg_NovedadesT.Tipo            = Cfg_Novedades.Tipo
        /*Cfg_NovedadesT.Id_Ingreso     = Cfg_Novedades.Id_Ingreso*/
        Cfg_NovedadesT.Id_Base         = Cfg_Novedades.Id_Base
        Cfg_NovedadesT.Id_Valor        = Cfg_Novedades.Id_Valor
        Cfg_NovedadesT.Valor           = Cfg_Novedades.Valor
        Cfg_NovedadesT.Id_SMLV         = Cfg_Novedades.Id_SMLV
        Cfg_NovedadesT.Num_SMLV        = Cfg_Novedades.Num_SMLV
        Cfg_NovedadesT.PunAd_Calculo   = Cfg_Novedades.PunAd_Calculo
        Cfg_NovedadesT.Porc_Parafiscal = Cfg_Novedades.Porc_Parafiscal
        Cfg_NovedadesT.Nit_Contable    = Cfg_Novedades.Nit_Contable
        Cfg_NovedadesT.Cta_Contrapartida = Cfg_Novedades.Cta_Contrapartida
        Cfg_NovedadesT.Id_Extras       = Cfg_Novedades.Id_Extras
        Cfg_NovedadesT.Id_Salario      = Cfg_Novedades.Id_Salario.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

