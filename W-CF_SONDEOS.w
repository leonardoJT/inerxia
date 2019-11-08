&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
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

DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR W_NomPdt AS CHARACTER FORMAT "X(20)".
DEFINE VAR W_Nit    AS CHARACTER FORMAT "X(14)".

 {Incluido/Variable.I "SHARED"}


DEFINE TEMP-TABLE TSim
  FIELD W_CodEnt LIKE Bancos.Cod_Compensa
  FIELD W_NomEnt AS CHARACTER FORMAT "X(30)"
  FIELD W_TasEnt LIKE Tasas_Mercado.Tasa
  FIELD W_Valor  AS DECIMAL   FORMAT "->>,>>>,>>>,>>9".

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_DatSim
&Scoped-define BROWSE-NAME BROWSE-11

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Indicadores TSim Tasas_Mercado

/* Definitions for BROWSE BROWSE-11                                     */
&Scoped-define FIELDS-IN-QUERY-BROWSE-11 Indicadores.indicador Indicadores.Nombre Indicadores.Tasa Indicadores.Valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-11 Indicadores.Tasa ~
Indicadores.Valor   
&Scoped-define ENABLED-TABLES-IN-QUERY-BROWSE-11 Indicadores
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-BROWSE-11 Indicadores
&Scoped-define SELF-NAME BROWSE-11
&Scoped-define QUERY-STRING-BROWSE-11 FOR EACH Indicadores WHERE Indicadores.Id_Economico EQ YES NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-11 OPEN QUERY {&SELF-NAME} FOR EACH Indicadores WHERE Indicadores.Id_Economico EQ YES NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-11 Indicadores
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-11 Indicadores


/* Definitions for BROWSE Brw_Simulacion                                */
&Scoped-define FIELDS-IN-QUERY-Brw_Simulacion TSim.W_CodEnt TSim.W_NomEnt TSim.W_TasEnt TSim.W_Valor   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw_Simulacion   
&Scoped-define SELF-NAME Brw_Simulacion
&Scoped-define QUERY-STRING-Brw_Simulacion FOR EACH TSim BY TSim.W_Valor INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Brw_Simulacion OPEN QUERY {&SELF-NAME}   FOR EACH TSim BY TSim.W_Valor INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Brw_Simulacion TSim
&Scoped-define FIRST-TABLE-IN-QUERY-Brw_Simulacion TSim


/* Definitions for BROWSE B_Valora                                      */
&Scoped-define FIELDS-IN-QUERY-B_Valora Tasas_Mercado.Cod_Producto W_NomPdt Tasas_Mercado.Nom_TasaUnid Tasas_Mercado.Fec_Crea Tasas_Mercado.Monto_Final Tasas_Mercado.Pla_Final Tasas_Mercado.Tasa Tasas_Mercado.Valor_Unidad   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Valora Tasas_Mercado.Tasa Tasas_Mercado.Valor_Unidad   
&Scoped-define ENABLED-TABLES-IN-QUERY-B_Valora Tasas_Mercado
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-B_Valora Tasas_Mercado
&Scoped-define SELF-NAME B_Valora
&Scoped-define OPEN-QUERY-B_Valora FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE, ~
      1, ~
      2)) NO-LOCK NO-ERROR. OPEN QUERY {&SELF-NAME}   FOR EACH Tasas_Mercado WHERE            Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND            Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val) AND            Tasas_Mercado.Estado EQ 1   INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Valora Tasas_Mercado
&Scoped-define FIRST-TABLE-IN-QUERY-B_Valora Tasas_Mercado


/* Definitions for FRAME F_DatSim                                       */
&Scoped-define QUERY-STRING-F_DatSim FOR EACH Tasas_Mercado SHARE-LOCK
&Scoped-define OPEN-QUERY-F_DatSim OPEN QUERY F_DatSim FOR EACH Tasas_Mercado SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_DatSim Tasas_Mercado
&Scoped-define FIRST-TABLE-IN-QUERY-F_DatSim Tasas_Mercado


/* Definitions for FRAME F_Economicos                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Economicos ~
    ~{&OPEN-QUERY-BROWSE-11}

/* Definitions for FRAME F_Nuevo                                        */
&Scoped-define FIELDS-IN-QUERY-F_Nuevo Tasas_Mercado.Nom_TasaUnid ~
Tasas_Mercado.Monto_Final Tasas_Mercado.Pla_Final 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Nuevo Tasas_Mercado.Nom_TasaUnid ~
Tasas_Mercado.Monto_Final Tasas_Mercado.Pla_Final 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Nuevo Tasas_Mercado
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Nuevo Tasas_Mercado
&Scoped-define QUERY-STRING-F_Nuevo FOR EACH Tasas_Mercado SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Nuevo OPEN QUERY F_Nuevo FOR EACH Tasas_Mercado SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Nuevo Tasas_Mercado
&Scoped-define FIRST-TABLE-IN-QUERY-F_Nuevo Tasas_Mercado


/* Definitions for FRAME F_Simulacion                                   */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Simulacion ~
    ~{&OPEN-QUERY-Brw_Simulacion}

/* Definitions for FRAME F_Val                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Val ~
    ~{&OPEN-QUERY-B_Valora}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Cmb_TInversiones_S Cmb_Productos_S Monto_S ~
Btn_Sim Plazo_S Btn_OutDatSim 
&Scoped-Define DISPLAYED-OBJECTS Cmb_TInversiones_S Cmb_Productos_S Monto_S ~
Plazo_S 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-B_Valora 
       MENU-ITEM m_ActivaInactivar LABEL "Activa/Inactivar".


/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_OutDatSim 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 155" 
     SIZE 12 BY 1.62.

DEFINE BUTTON Btn_Sim 
     LABEL "Simular" 
     SIZE 12 BY 1.65.

DEFINE VARIABLE Cmb_Productos_S AS CHARACTER FORMAT "X(256)":U 
     LABEL "Productos Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_TInversiones_S AS CHARACTER FORMAT "X(256)":U INITIAL "1 - Negociables" 
     LABEL "Tipos de Inversiones" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 - Negociables","2 - Permanentes","3 - Disponibles" 
     DROP-DOWN-LIST
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Monto_S AS DECIMAL FORMAT "->>>,>>>,>>>,>>9" INITIAL 0 
     LABEL "Monto Final" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 15 .

DEFINE VARIABLE Plazo_S AS DECIMAL FORMAT ">>,>>9" INITIAL 0 
     LABEL "Plazo Final" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 15 .

DEFINE BUTTON BUTTON-65 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 65" 
     SIZE 9 BY 1.88.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 12 BY 1.65.

DEFINE BUTTON BUTTON-62 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 62" 
     SIZE 12 BY 1.62.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(256)":U 
     LABEL "Productos Disponibles" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_TInversiones AS CHARACTER FORMAT "X(256)":U INITIAL "1 - Negociables" 
     LABEL "Tipos de Inversiones" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1 - Negociables","2 - Permanentes","3 - Disponibles" 
     DROP-DOWN-LIST
     SIZE 42 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE BUTTON BUTTON-156 
     LABEL "Cambiar Valores Simulación" 
     SIZE 28 BY 1.12.

DEFINE BUTTON BUTTON-87 
     IMAGE-UP FILE "Imagenes/volver.bmp":U
     LABEL "Btn_OutSimulacion" 
     SIZE 9 BY 1.88.

DEFINE BUTTON BtnDone-2 DEFAULT 
     LABEL "&Salir" 
     SIZE 13 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_A 
     LABEL "Ver Inactivas" 
     SIZE 15 BY 1.65.

DEFINE BUTTON Btn_Simulacion 
     LABEL "Simulación" 
     SIZE 15 BY 1.92.

DEFINE BUTTON BUTTON-49 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 49" 
     SIZE 15 BY 1.54.

DEFINE BUTTON BUTTON-50 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 50" 
     SIZE 15 BY 1.62.

DEFINE BUTTON BUTTON-51 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 51" 
     SIZE 15 BY 1.85.

DEFINE BUTTON BUTTON-60 
     LABEL "Nuevo" 
     SIZE 15 BY 1.62.

DEFINE BUTTON BUTTON-64 
     LABEL "Indicadores Grales" 
     SIZE 15 BY 1.88 TOOLTIP "Actualizar Indicadores Economicos"
     FONT 4.

DEFINE VARIABLE Cmb_Entidades AS CHARACTER FORMAT "X(256)":U 
     LABEL "Entidad" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 35 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Contacto AS CHARACTER FORMAT "X(100)":U 
     LABEL "Contacto" 
     VIEW-AS FILL-IN 
     SIZE 83 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Pcto AS INTEGER INITIAL 3 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Captaciones", 1,
"Colocaciones", 2,
"Inversiones", 3
     SIZE 46 BY 1.08 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-11 FOR 
      Indicadores SCROLLING.

DEFINE QUERY Brw_Simulacion FOR 
      TSim SCROLLING.

DEFINE QUERY B_Valora FOR 
      Tasas_Mercado SCROLLING.

DEFINE QUERY F_DatSim FOR 
      Tasas_Mercado SCROLLING.

DEFINE QUERY F_Nuevo FOR 
      Tasas_Mercado SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-11
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-11 wWin _FREEFORM
  QUERY BROWSE-11 NO-LOCK DISPLAY
      Indicadores.indicador FORMAT "99999":U COLUMN-LABEL "Indic"
      Indicadores.Nombre FORMAT "X(30)":U
      Indicadores.Tasa FORMAT ">>9.9999999":U
      Indicadores.Valor FORMAT ">>>,>>>,>>>,>>9":U
  ENABLE
      Indicadores.Tasa
      Indicadores.Valor
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 66 BY 7
         BGCOLOR 15 FONT 5 EXPANDABLE.

DEFINE BROWSE Brw_Simulacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw_Simulacion wWin _FREEFORM
  QUERY Brw_Simulacion NO-LOCK DISPLAY
      TSim.W_CodEnt 
   TSim.W_NomEnt COLUMN-LABEL "Nombre Entidad"
   TSim.W_TasEnt 
   TSim.W_Valor COLUMN-LABEL "Valor"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 90 BY 13.19
         BGCOLOR 15 FONT 5 EXPANDABLE.

DEFINE BROWSE B_Valora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Valora wWin _FREEFORM
  QUERY B_Valora NO-LOCK DISPLAY
      Tasas_Mercado.Cod_Producto FORMAT "999":U    LABEL "CodPdt"
      W_NomPdt FORMAT "X(30)" LABEL "Nombre Pdt"
      Tasas_Mercado.Nom_TasaUnid FORMAT "X(30)":U  LABEL "Descripción"
      Tasas_Mercado.Fec_Crea FORMAT "99/99/9999":U LABEL "Fecha"
      Tasas_Mercado.Monto_Final FORMAT "->>>,>>>,>>>,>>9":U LABEL "Monto"
      Tasas_Mercado.Pla_Final FORMAT ">>,>>9":U    LABEL "Plazo"
      Tasas_Mercado.Tasa FORMAT ">>9.99":U    LABEL "Tasa"
      Tasas_Mercado.Valor_Unidad FORMAT "->>>,>>>,>>9.9999":U  LABEL "Valor"
      ENABLE Tasas_Mercado.Tasa Tasas_Mercado.Valor_Unidad
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 16.96
         BGCOLOR 15 FONT 4 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Val
     Cmb_Entidades AT ROW 1.27 COL 10 COLON-ALIGNED
     R_Pcto AT ROW 1.27 COL 49 NO-LABEL
     BUTTON-49 AT ROW 1.54 COL 98
     Contacto AT ROW 2.62 COL 10 COLON-ALIGNED
     BUTTON-50 AT ROW 3.04 COL 98
     B_Valora AT ROW 4.5 COL 3
     BUTTON-51 AT ROW 4.65 COL 98
     BUTTON-64 AT ROW 9.88 COL 98
     Btn_Simulacion AT ROW 11.73 COL 98
     Btn_A AT ROW 13.65 COL 98
     BUTTON-60 AT ROW 15.31 COL 98
     BtnDone-2 AT ROW 19.58 COL 100
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 21.19
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_DatSim
     Cmb_TInversiones_S AT ROW 1.27 COL 26 COLON-ALIGNED
     Cmb_Productos_S AT ROW 2.35 COL 26 COLON-ALIGNED
     Monto_S AT ROW 3.69 COL 26 COLON-ALIGNED
     Btn_Sim AT ROW 3.69 COL 58
     Plazo_S AT ROW 4.77 COL 26 COLON-ALIGNED
     Btn_OutDatSim AT ROW 5.58 COL 58
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 20 ROW 5.31
         SIZE 74 BY 7.27
         BGCOLOR 17 FONT 5
         TITLE "Datos para la Simulación".

DEFINE FRAME F_Economicos
     BROWSE-11 AT ROW 1.81 COL 4
     BUTTON-65 AT ROW 9.08 COL 60
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 17 ROW 6.38
         SIZE 71 BY 11.31
         BGCOLOR 17 FONT 5
         TITLE "Actualización de Indicadores Económicos".

DEFINE FRAME F_Simulacion
     Brw_Simulacion AT ROW 1.27 COL 3
     BUTTON-156 AT ROW 15 COL 3
     BUTTON-87 AT ROW 15 COL 84
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 4.5
         SIZE 94 BY 16.96
         BGCOLOR 17 FONT 5
         TITLE "Simulación de las Inversiones en Todas las Entidades".

DEFINE FRAME F_Nuevo
     Cmb_TInversiones AT ROW 1.27 COL 26 COLON-ALIGNED
     Cmb_Productos AT ROW 2.35 COL 26 COLON-ALIGNED
     Tasas_Mercado.Nom_TasaUnid AT ROW 3.42 COL 26 COLON-ALIGNED
          LABEL "Nombre de la Tasa o Unidad"
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 15 
     Tasas_Mercado.Monto_Final AT ROW 4.5 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     Btn_Salvar AT ROW 4.77 COL 58
     Tasas_Mercado.Pla_Final AT ROW 5.58 COL 26 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 19 BY .81
          BGCOLOR 15 
     BUTTON-62 AT ROW 6.65 COL 58
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 8.81
         SIZE 71 BY 8.35
         BGCOLOR 17 FONT 5
         TITLE "Nuevo Item de Valoración".


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
         TITLE              = "SFG - Configuración de las Valoraciones"
         HEIGHT             = 21.19
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
ASSIGN FRAME F_DatSim:FRAME = FRAME F_Val:HANDLE
       FRAME F_Economicos:FRAME = FRAME F_Val:HANDLE
       FRAME F_Nuevo:FRAME = FRAME F_Val:HANDLE
       FRAME F_Simulacion:FRAME = FRAME F_Val:HANDLE.

/* SETTINGS FOR FRAME F_DatSim
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_DatSim:HIDDEN           = TRUE
       FRAME F_DatSim:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Economicos
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-11 1 F_Economicos */
ASSIGN 
       FRAME F_Economicos:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Nuevo
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Nuevo:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Tasas_Mercado.Nom_TasaUnid IN FRAME F_Nuevo
   EXP-LABEL                                                            */
/* SETTINGS FOR FRAME F_Simulacion
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Brw_Simulacion 1 F_Simulacion */
ASSIGN 
       FRAME F_Simulacion:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Val
                                                                        */

DEFINE VARIABLE XXTABVALXX AS LOGICAL NO-UNDO.

ASSIGN XXTABVALXX = FRAME F_Simulacion:MOVE-AFTER-TAB-ITEM (BUTTON-50:HANDLE IN FRAME F_Val)
       XXTABVALXX = FRAME F_Simulacion:MOVE-BEFORE-TAB-ITEM (B_Valora:HANDLE IN FRAME F_Val)
       XXTABVALXX = FRAME F_DatSim:MOVE-AFTER-TAB-ITEM (BUTTON-51:HANDLE IN FRAME F_Val)
       XXTABVALXX = FRAME F_Nuevo:MOVE-BEFORE-TAB-ITEM (BUTTON-64:HANDLE IN FRAME F_Val)
       XXTABVALXX = FRAME F_Economicos:MOVE-BEFORE-TAB-ITEM (FRAME F_Nuevo:HANDLE)
       XXTABVALXX = FRAME F_DatSim:MOVE-BEFORE-TAB-ITEM (FRAME F_Economicos:HANDLE)
/* END-ASSIGN-TABS */.

/* BROWSE-TAB B_Valora F_Simulacion F_Val */
/* SETTINGS FOR BUTTON BUTTON-51 IN FRAME F_Val
   NO-ENABLE                                                            */
ASSIGN 
       B_Valora:POPUP-MENU IN FRAME F_Val             = MENU POPUP-MENU-B_Valora:HANDLE.

/* SETTINGS FOR FILL-IN Contacto IN FRAME F_Val
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-11
/* Query rebuild information for BROWSE BROWSE-11
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Indicadores WHERE Indicadores.Id_Economico EQ YES NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-11 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw_Simulacion
/* Query rebuild information for BROWSE Brw_Simulacion
     _START_FREEFORM

OPEN QUERY {&SELF-NAME}
  FOR EACH TSim BY TSim.W_Valor INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Brw_Simulacion */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Valora
/* Query rebuild information for BROWSE B_Valora
     _START_FREEFORM
FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
OPEN QUERY {&SELF-NAME}
  FOR EACH Tasas_Mercado WHERE
           Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
           Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val) AND
           Tasas_Mercado.Estado EQ 1
  INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Valora */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_DatSim
/* Query rebuild information for FRAME F_DatSim
     _TblList          = "bdCentral.Tasas_Mercado"
     _Query            is OPENED
*/  /* FRAME F_DatSim */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Nuevo
/* Query rebuild information for FRAME F_Nuevo
     _TblList          = "bdCentral.Tasas_Mercado"
     _Query            is OPENED
*/  /* FRAME F_Nuevo */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Configuración de las Valoraciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Configuración de las Valoraciones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Val
&Scoped-define SELF-NAME BtnDone-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone-2 wWin
ON CHOOSE OF BtnDone-2 IN FRAME F_Val /* Salir */
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


&Scoped-define SELF-NAME Btn_A
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_A wWin
ON CHOOSE OF Btn_A IN FRAME F_Val /* Ver Inactivas */
DO:

IF SELF:LABEL EQ "Ver Inactivas" THEN DO:
  FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
  OPEN QUERY B_Valora
    FOR EACH Tasas_Mercado WHERE
             Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
             Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val) AND
             Tasas_Mercado.Estado EQ 2
    NO-LOCK INDEXED-REPOSITION.  
  SELF:LABEL = "Ver Activas".
END.
ELSE DO:
  FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
  OPEN QUERY B_Valora
    FOR EACH Tasas_Mercado WHERE
             Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
             Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val) AND
             Tasas_Mercado.Estado EQ 1
    NO-LOCK INDEXED-REPOSITION.  
  SELF:LABEL = "Ver Inactivas".
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_DatSim
&Scoped-define SELF-NAME Btn_OutDatSim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutDatSim wWin
ON CHOOSE OF Btn_OutDatSim IN FRAME F_DatSim /* Button 155 */
DO:
  HIDE FRAME F_DatSim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Nuevo
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Nuevo /* Salvar */
DO:
DO WITH FRAME F_Nuevo:
   IF Cmb_Productos:SCREEN-VALUE EQ "" OR Tasas_Mercado.Nom_TasaUnid:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "No se puede grabar la información" SKIP
              "debe entrarse el Producto y el nombre de la valoración"
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO Tasas_Mercado.Nom_TasaUnid.
      RETURN NO-APPLY.
   END.
   IF Tasas_Mercado.Pla_Final:SCREEN-VALUE EQ "" OR 
      Tasas_Mercado.Monto_Final:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "No se puede grabar la información" SKIP
              "debe digitarse el Monto y el Plazo"
              VIEW-AS ALERT-BOX ERROR.
      APPLY "entry" TO Tasas_Mercado.Monto_Final.
      RETURN NO-APPLY.
   END.
   CREATE Tasas_Mercado.
   ASSIGN Tasas_Mercado.Cod_Producto  = INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3))
          Tasas_Mercado.Fec_Crea      = TODAY
          Tasas_Mercado.Nit_Entidad   = W_Nit
          Tasas_Mercado.Nom_TasaUnid  = Tasas_Mercado.Nom_TasaUnid:SCREEN-VALUE
          Tasas_Mercado.Pdcto         = INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val)
          Tasas_Mercado.Monto_Final   = DECIMAL(Tasas_Mercado.Monto_Final:SCREEN-VALUE)
          Tasas_Mercado.Pla_Final     = DECIMAL(Tasas_Mercado.Pla_Final:SCREEN-VALUE)
          Tasas_Mercado.Tipo          = INTEGER(SUBSTRING(Cmb_TInversiones:SCREEN-VALUE,1,1)).
FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
OPEN QUERY B_Valora
  FOR EACH Tasas_Mercado WHERE
           Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
           Tasas_Mercado.Pdcto       EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val)
  NO-LOCK INDEXED-REPOSITION.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_DatSim
&Scoped-define SELF-NAME Btn_Sim
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Sim wWin
ON CHOOSE OF Btn_Sim IN FRAME F_DatSim /* Simular */
DO:
FOR EACH TSim: DELETE TSim. END.
DO WITH FRAME F_DatSim:
   ASSIGN Plazo_S Monto_S.
   FOR EACH Bancos NO-LOCK:
       FIND FIRST Tasas_Mercado WHERE 
            Tasas_Mercado.Nit     EQ Bancos.Nit AND
            Tasas_Mercado.Pdcto   EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val) AND
            Tasas_Mercado.Tipo    EQ INTEGER(SUBSTRING(Cmb_TInversiones_S:SCREEN-VALUE,1,1)) AND
            Tasas_Mercado.Cod_Producto EQ INTEGER(SUBSTRING(Cmb_Productos_S:SCREEN-VALUE,1,3)) AND
            Tasas_Mercado.Pla_Final    GE PLazo_S AND 
            Tasas_Mercado.Estado       EQ 1 NO-LOCK NO-ERROR.
       IF AVAILABLE Tasas_Mercado THEN DO:
          CREATE TSim.
          ASSIGN TSim.W_CodEnt = Bancos.Cod_Compensa
                 TSim.W_NomEnt = Bancos.Nombre
                 TSim.W_TasEnt = Tasas_Mercado.Tasa
                 TSim.W_Valor  = ((Tasas_Mercado.Tasa / 360) * Plazo_S) * Monto_S.
       END.
   END.
   OPEN QUERY Brw_Simulacion FOR EACH TSim BY TSim.W_Valor DESCENDING INDEXED-REPOSITION.
   HIDE FRAME F_DatSim.
   VIEW FRAME F_Simulacion.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Val
&Scoped-define SELF-NAME Btn_Simulacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Simulacion wWin
ON CHOOSE OF Btn_Simulacion IN FRAME F_Val /* Simulación */
DO:
  VIEW FRAME F_DatSim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Simulacion
&Scoped-define SELF-NAME BUTTON-156
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-156 wWin
ON CHOOSE OF BUTTON-156 IN FRAME F_Simulacion /* Cambiar Valores Simulación */
DO:
  VIEW FRAME F_DatSim.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Val
&Scoped-define SELF-NAME BUTTON-49
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-49 wWin
ON CHOOSE OF BUTTON-49 IN FRAME F_Val /* Button 49 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-50
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-50 wWin
ON CHOOSE OF BUTTON-50 IN FRAME F_Val /* Button 50 */
DO:
  DEFINE VAR Listado AS CHAR INITIAL "L_ProCre.LST".
  
  Listado = W_PathSpl + Listado.
  {INCLUIDO/Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-51
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-51 wWin
ON CHOOSE OF BUTTON-51 IN FRAME F_Val /* Button 51 */
DO:
/*  OPEN QUERY Br_Asignacion FOR EACH CFG_Instancias 
     SHARE-LOCK BY Cfg_Instancias.Tipo_Instancia BY CFG_Instancias.Orden INDEXED-REPOSITION.
  VIEW FRAME F_ConsAsignaciones.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-60
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-60 wWin
ON CHOOSE OF BUTTON-60 IN FRAME F_Val /* Nuevo */
DO:
  VIEW FRAME F_Nuevo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Nuevo
&Scoped-define SELF-NAME BUTTON-62
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-62 wWin
ON CHOOSE OF BUTTON-62 IN FRAME F_Nuevo /* Button 62 */
DO:
  HIDE FRAME F_Nuevo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Val
&Scoped-define SELF-NAME BUTTON-64
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-64 wWin
ON CHOOSE OF BUTTON-64 IN FRAME F_Val /* Indicadores Grales */
DO:
  VIEW FRAME F_Economicos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Economicos
&Scoped-define SELF-NAME BUTTON-65
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-65 wWin
ON CHOOSE OF BUTTON-65 IN FRAME F_Economicos /* Button 65 */
DO:
  HIDE FRAME F_Economicos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Simulacion
&Scoped-define SELF-NAME BUTTON-87
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-87 wWin
ON CHOOSE OF BUTTON-87 IN FRAME F_Simulacion /* Btn_OutSimulacion */
DO:
  HIDE FRAME F_Simulacion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Valora
&Scoped-define FRAME-NAME F_Val
&Scoped-define SELF-NAME B_Valora
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_Valora wWin
ON ROW-DISPLAY OF B_Valora IN FRAME F_Val
DO:
  CASE R_Pcto:SCREEN-VALUE IN FRAME F_Val:
    WHEN "3" THEN DO:
      FIND Pro_Inversiones WHERE Pro_Inversiones.Categoria EQ Tasas_Mercado.Tipo AND
           Pro_Inversiones.Cod_Producto EQ Tasas_Mercado.Cod_Producto NO-ERROR.
      IF AVAILABLE Pro_Inversiones THEN DO:
         W_NomPdt = Pro_Inversiones.Nom_Producto.
         IF Pro_Inversiones.Id_TasaUnid EQ 0 THEN Tasas_Mercado.Valor_Unidad:SCREEN-VALUE IN BROWSE B_Valora = "0".
         IF Pro_Inversiones.Id_TasaUnid EQ 1 THEN Tasas_Mercado.Tasa:SCREEN-VALUE IN BROWSE B_Valora = "0".
      END.
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Entidades
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Entidades wWin
ON VALUE-CHANGED OF Cmb_Entidades IN FRAME F_Val /* Entidad */
DO:
  FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,2)) NO-LOCK.
  IF AVAILABLE Bancos THEN DO:
     Contacto:SCREEN-VALUE = Bancos.Nom_Contacto + " - " + Bancos.Tel_Contacto.
     FIND Clientes WHERE Clientes.Nit EQ Bancos.Nit NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN W_Nit = Clientes.Nit.
  END.
  APPLY "Value-Changed" TO R_Pcto IN FRAME F_Val.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Nuevo
&Scoped-define SELF-NAME Cmb_TInversiones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TInversiones wWin
ON VALUE-CHANGED OF Cmb_TInversiones IN FRAME F_Nuevo /* Tipos de Inversiones */
DO:
  RUN Combo_Inversiones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_DatSim
&Scoped-define SELF-NAME Cmb_TInversiones_S
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TInversiones_S wWin
ON VALUE-CHANGED OF Cmb_TInversiones_S IN FRAME F_DatSim /* Tipos de Inversiones */
DO:
  RUN Combo_Inversiones_S.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_ActivaInactivar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_ActivaInactivar wWin
ON CHOOSE OF MENU-ITEM m_ActivaInactivar /* Activa/Inactivar */
DO:
FIND CURRENT Tasas_Mercado EXCLUSIVE-LOCK.
  IF Btn_A:LABEL IN FRAME F_Val EQ "Ver Inactivas" THEN DO:
     MESSAGE "Desea Inactivar la Configuración de" SKIP
             "Valorización" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice AS LOGICAL.
     IF choice THEN DO:
        ASSIGN Tasas_Mercado.Estado = 2
               Tasas_Mercado.Fec_Inactivo = TODAY.
        OPEN QUERY B_Valora
         FOR EACH Tasas_Mercado WHERE
                  Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
                  Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val) AND
                  Tasas_Mercado.Estado EQ 1
        NO-LOCK INDEXED-REPOSITION.  
     END.
  END.
  ELSE DO:
     MESSAGE "Desea Activar la Configuración de" SKIP
             "Valorización" VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE choice2 AS LOGICAL.
     IF choice2 THEN DO:
        ASSIGN Tasas_Mercado.Estado = 1
               Tasas_Mercado.Fec_Inactivo = ?.
        OPEN QUERY B_Valora
         FOR EACH Tasas_Mercado WHERE
                  Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
                  Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val) AND
                  Tasas_Mercado.Estado EQ 2
         NO-LOCK INDEXED-REPOSITION.  
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Val
&Scoped-define SELF-NAME R_Pcto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Pcto wWin
ON VALUE-CHANGED OF R_Pcto IN FRAME F_Val
DO:
DO WITH FRAME F_Val:
  CASE SELF:SCREEN-VALUE:
    WHEN "3" THEN DO: 
      RUN Combo_Inversiones.
      ENABLE Btn_Simulacion WITH FRAME F_Val.
    END.
    OTHERWISE DISABLE Btn_Simulacion WITH FRAME F_Val.
  END CASE.
  FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
  OPEN QUERY B_Valora
  FOR EACH Tasas_Mercado WHERE
           Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
           Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val) AND
           Tasas_Mercado.Estado EQ 1
  INDEXED-REPOSITION.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_DatSim
&Scoped-define BROWSE-NAME BROWSE-11
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Combo_Inversiones wWin 
PROCEDURE Combo_Inversiones :
DO WITH FRAME F_Nuevo:
   Cmb_Productos:LIST-ITEMS = "".
   FOR EACH Pro_Inversiones WHERE 
            Pro_Inversiones.Categoria EQ INTEGER(SUBSTRING(Cmb_TInversiones:SCREEN-VALUE,1,1))
            NO-LOCK:
            W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Inversiones.Cod_Producto,"999") + " - " + Pro_Inversiones.Nom_Producto).
   END.
   IF Cmb_Productos:NUM-ITEMS NE 0 THEN
      Cmb_Productos:SCREEN-VALUE = Cmb_Productos:ENTRY(1).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Combo_Inversiones_S wWin 
PROCEDURE Combo_Inversiones_S :
DO WITH FRAME F_DatSim:
   Cmb_Productos_S:LIST-ITEMS = "".
   FOR EACH Pro_Inversiones WHERE 
            Pro_Inversiones.Categoria EQ INTEGER(SUBSTRING(Cmb_TInversiones_S:SCREEN-VALUE,1,1))
            NO-LOCK:
            W_Ok = Cmb_Productos_S:ADD-LAST(STRING(Pro_Inversiones.Cod_Producto,"999") + " - " + Pro_Inversiones.Nom_Producto).
   END.
   IF Cmb_Productos_S:NUM-ITEMS NE 0 THEN
      Cmb_Productos_S:SCREEN-VALUE = Cmb_Productos_S:ENTRY(1).
END.
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
  DISPLAY Cmb_Entidades R_Pcto Contacto 
      WITH FRAME F_Val IN WINDOW wWin.
  ENABLE Cmb_Entidades R_Pcto BUTTON-49 BUTTON-50 B_Valora BUTTON-64 
         Btn_Simulacion Btn_A BUTTON-60 BtnDone-2 
      WITH FRAME F_Val IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Val}
  ENABLE Brw_Simulacion BUTTON-156 BUTTON-87 
      WITH FRAME F_Simulacion IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Simulacion}

  {&OPEN-QUERY-F_DatSim}
  GET FIRST F_DatSim.
  DISPLAY Cmb_TInversiones_S Cmb_Productos_S Monto_S Plazo_S 
      WITH FRAME F_DatSim IN WINDOW wWin.
  ENABLE Cmb_TInversiones_S Cmb_Productos_S Monto_S Btn_Sim Plazo_S 
         Btn_OutDatSim 
      WITH FRAME F_DatSim IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_DatSim}
  ENABLE BROWSE-11 BUTTON-65 
      WITH FRAME F_Economicos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Economicos}

  {&OPEN-QUERY-F_Nuevo}
  GET FIRST F_Nuevo.
  DISPLAY Cmb_TInversiones Cmb_Productos 
      WITH FRAME F_Nuevo IN WINDOW wWin.
  IF AVAILABLE Tasas_Mercado THEN 
    DISPLAY Tasas_Mercado.Nom_TasaUnid Tasas_Mercado.Monto_Final 
          Tasas_Mercado.Pla_Final 
      WITH FRAME F_Nuevo IN WINDOW wWin.
  ENABLE Cmb_TInversiones Cmb_Productos Tasas_Mercado.Nom_TasaUnid 
         Tasas_Mercado.Monto_Final Btn_Salvar Tasas_Mercado.Pla_Final BUTTON-62 
      WITH FRAME F_Nuevo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Nuevo}
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
DO WITH FRAME F_Val:
  FOR EACH Bancos NO-LOCK BREAK BY Bancos.Cod_Compensa:
    FIND Clientes WHERE Clientes.Nit EQ Bancos.Nit NO-LOCK NO-ERROR.
    IF FIRST-OF(Bancos.Cod_Compensa) THEN W_Nit = Clientes.Nit.
    IF AVAILABLE Clientes THEN
       W_Ok = Cmb_Entidades:ADD-LAST(STRING(Bancos.Cod_Compensa,"99") + " - " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
    ELSE
       W_Ok = Cmb_Entidades:ADD-LAST(STRING(Bancos.Cod_Compensa,"99") + " - " + "No esta en Clientes").       
  END.
  IF Cmb_Entidades:NUM-ITEMS NE 0 THEN
     Cmb_Entidades:SCREEN-VALUE = Cmb_Entidades:ENTRY(1).
END.
FIND Bancos WHERE Bancos.Cod_Compensa EQ INTEGER(SUBSTRING(Cmb_Entidades:SCREEN-VALUE,1,2)) NO-LOCK NO-ERROR.
IF AVAILABLE Bancos THEN Contacto:SCREEN-VALUE = Bancos.Nom_Contacto + " - " + Bancos.Tel_Contacto.
APPLY "value-changed" TO R_Pcto IN FRAME F_Val.
RUN Combo_Inversiones.
HIDE FRAME F_Nuevo.
HIDE FRAME F_Economicos.
HIDE FRAME F_Simulacion.
HIDE FRAME F_DatSim.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}


  W_Reporte   = "REPORTE   : VALORACION DE INVERCIONES (CFG-INDICADORES) - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
                       /*  1         2         3         4         5         6         7         8         9         0         1         2         3*/
                /*1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
  W_EncColumna = "PDT NOMBRE PRODUCTO            TASA O UNIDAD                      FEC.VIGENCIA MONTOFINAL     PLA.FINAL TASA      VALOR".

  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.

IF Btn_A:LABEL IN FRAME F_Val EQ "Ver Inactivas" THEN DO:
  FOR EACH Tasas_Mercado WHERE
           Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
           Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val) AND
           Tasas_Mercado.Estado EQ 1 NO-LOCK:
      CASE R_Pcto:SCREEN-VALUE IN FRAME F_Val:
        WHEN "3" THEN DO:
         FIND Pro_Inversiones WHERE Pro_Inversiones.Categoria EQ Tasas_Mercado.Tipo AND
              Pro_Inversiones.Cod_Producto EQ Tasas_Mercado.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE Pro_Inversiones THEN
            W_NomPdt = Pro_Inversiones.Nom_Producto.
        END.
      END CASE.
      DISPLAY 
       Tasas_Mercado.Cod_Producto  AT 1  FORMAT "999"
       W_NomPdt                    AT 5  FORMAT "X(25)"
       Tasas_Mercado.Nom_TasaUnid  AT 32 FORMAT "X(25)"
       Tasas_Mercado.Fec_Crea      AT 67 FORMAT "99/99/9999"
       Tasas_Mercado.Monto_Final   AT 80 FORMAT "->>>,>>>,>>9"
       Tasas_Mercado.Pla_Final     AT 95 FORMAT ">>,>>9"
       Tasas_Mercado.Tasa          AT 105 FORMAT ">>9.99"
       Tasas_Mercado.Valor_Unidad  AT 113 FORMAT "->>>,>>>,>>9.99"
      WITH FRAME F_Activas WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.
  END.
END.
ELSE DO:
  FOR EACH Tasas_Mercado WHERE
           Tasas_Mercado.Nit_Entidad EQ Bancos.Nit AND
           Tasas_Mercado.Pdcto EQ INTEGER(R_Pcto:SCREEN-VALUE IN FRAME F_Val) AND
           Tasas_Mercado.Estado EQ 2 NO-LOCK:
      CASE R_Pcto:SCREEN-VALUE IN FRAME F_Val:
        WHEN "3" THEN DO:
         FIND Pro_Inversiones WHERE Pro_Inversiones.Categoria EQ Tasas_Mercado.Tipo AND
              Pro_Inversiones.Cod_Producto EQ Tasas_Mercado.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE Pro_Inversiones THEN
            W_NomPdt = Pro_Inversiones.Nom_Producto.
        END.
      END CASE.
      DISPLAY 
       Tasas_Mercado.Cod_Producto  AT 1  FORMAT "999"
       W_NomPdt                    AT 5  FORMAT "X(25)"
       Tasas_Mercado.Nom_TasaUnid  AT 32 FORMAT "X(25)"
       Tasas_Mercado.Fec_Crea      AT 67 FORMAT "99/99/9999"
       Tasas_Mercado.Monto_Final   AT 80 FORMAT "->>>,>>>,>>9"
       Tasas_Mercado.Pla_Final     AT 95 FORMAT ">>,>>9"
       Tasas_Mercado.Tasa          AT 105 FORMAT ">>9.99"
       Tasas_Mercado.Valor_Unidad  AT 113 FORMAT "->>>,>>>,>>9.99"
      WITH FRAME F_Inactivas WIDTH 132 NO-BOX USE-TEXT STREAM-IO NO-LABELS.
  END.
END.

PAGE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

