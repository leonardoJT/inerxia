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
{Incluido\VARIABLE.I "SHARED"}
/* Local Variable Definitions ---                                       */
DEFINE VAR i AS DECIMAL.
DEFINE VAR W_Ok AS LOGICAL.

DEFINE VAR TotTra AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotCon AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".
DEFINE VAR TotRet AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9".

DEFINE TEMP-TABLE TTaquilla LIKE Taquilla.

DEFINE TEMP-TABLE Tot
  FIELD TCodigo LIKE Clientes.Nit
  FIELD TNombre AS CHARACTER FORMAT "X(50)"
  FIELD TPorcen AS DECIMAL FORMAT ">>>.99"
  FIELD TNroTra AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TValCon AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9"
  FIELD TValRet AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9".
       
DEFINE TEMP-TABLE Tra
  FIELD TCodTra LIKE Operacion.Cod_Operacion
  FIELD TNomTra LIKE Operacion.Nom_Operacion
  FIELD TNroTra AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TValCon AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9"
  FIELD TValRet AS DECIMAL FORMAT ">>,>>>,>>>,>>>,>>9".
  
DEFINE TEMP-TABLE Det
  FIELD TAgeDoc LIKE Taquilla.Agencia
  FIELD TAgeOri LIKE Taquilla.Age_Destino
  FIELD TNitDoc LIKE Taquilla.Nit
  FIELD TFecDoc LIKE Taquilla.Fec_Transaccion
  FIELD TNumDoc LIKE Taquilla.Num_documento
  FIELD TNumTra LIKE Taquilla.Nro_Transaccion
  FIELD TCodCom LIKE Taquilla.Cod_Compensa
  FIELD TValEfe LIKE Taquilla.Val_Efectivo
  FIELD TValChe LIKE Taquilla.Val_Cheque
  FIELD THorDoc LIKE Taquilla.Hora_Transaccion
  FIELD TCodUsu LIKE Taquilla.Usuario
  FIELD TEstUsu LIKE Taquilla.Estacion
  FIELD TId_NUD LIKE Taquilla.Id_NUD
  FIELD TId_NUM LIKE Taquilla.Id_NUM
  FIELD TId_Sos LIKE Taquilla.Id_Sospechosa
  FIELD TFC_Sos LIKE Taquilla.Fec_Sospechosa
  FIELD TUs_Sos LIKE Taquilla.Usu_Sospechosa
  FIELD TId_Exo LIKE Taquilla.Id_Exonerada
  FIELD TFC_Exo LIKE Taquilla.Fec_Exonera
  FIELD TUs_Exo LIKE Taquilla.Usu_Exonera
  FIELD TId_Rep LIKE Taquilla.Id_RepFiscal
  FIELD TFC_Rep LIKE Taquilla.Fec_Reportada
  FIELD TUs_Rep LIKE Taquilla.Usu_Reportada.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define BROWSE-NAME Brw_Consolidado

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Tot Det Tra Taquilla

/* Definitions for BROWSE Brw_Consolidado                               */
&Scoped-define FIELDS-IN-QUERY-Brw_Consolidado Tot.TCodigo Tot.TNombre Tot.TPorcen Tot.TNroTra Tot.TValCon Tot.TValRet   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw_Consolidado   
&Scoped-define SELF-NAME Brw_Consolidado
&Scoped-define QUERY-STRING-Brw_Consolidado FOR EACH Tot NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Brw_Consolidado OPEN QUERY {&SELF-NAME} FOR EACH Tot NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Brw_Consolidado Tot
&Scoped-define FIRST-TABLE-IN-QUERY-Brw_Consolidado Tot


/* Definitions for BROWSE Brw_Detalle                                   */
&Scoped-define FIELDS-IN-QUERY-Brw_Detalle Det.TAgeDoc Det.TAgeOri Det.TNitDoc Det.TFecDoc Det.TNumDoc Det.TNumTra Det.TCodCom Det.TValEfe Det.TValChe STRING(Det.THorDoc,"HH:MM:SS") /* Det.THorDoc / 3600 */ Det.TCodUsu Det.TEstUsu Det.TId_NUD Det.TId_NUM   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw_Detalle   
&Scoped-define SELF-NAME Brw_Detalle
&Scoped-define QUERY-STRING-Brw_Detalle FOR EACH Det NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Brw_Detalle OPEN QUERY {&SELF-NAME} FOR EACH Det NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Brw_Detalle Det
&Scoped-define FIRST-TABLE-IN-QUERY-Brw_Detalle Det


/* Definitions for BROWSE Brw_Transacciones                             */
&Scoped-define FIELDS-IN-QUERY-Brw_Transacciones Tra.TCodTra Tra.TNomTra Tra.TNroTra Tra.TValCon Tra.TValRet   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw_Transacciones   
&Scoped-define SELF-NAME Brw_Transacciones
&Scoped-define QUERY-STRING-Brw_Transacciones FOR EACH Tra NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Brw_Transacciones OPEN QUERY {&SELF-NAME} FOR EACH Tra NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Brw_Transacciones Tra
&Scoped-define FIRST-TABLE-IN-QUERY-Brw_Transacciones Tra


/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-Brw_Consolidado}~
    ~{&OPEN-QUERY-Brw_Detalle}~
    ~{&OPEN-QUERY-Brw_Transacciones}

/* Definitions for FRAME F_Sipla                                        */
&Scoped-define QUERY-STRING-F_Sipla FOR EACH Taquilla SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Sipla OPEN QUERY F_Sipla FOR EACH Taquilla SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Sipla Taquilla
&Scoped-define FIRST-TABLE-IN-QUERY-F_Sipla Taquilla


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-10 RECT-11 RECT-13 BUTTON-2 ~
BUTTON-1 Btn_Ejecutar FIni FFin BUTTON-15 Btn_MaxCon T_ImpCon ~
Brw_Consolidado T_ImpTra Btn_MaxTra Brw_Transacciones Btn_MaxDet T_ImpDet ~
Brw_Detalle BtnDone BUTTON-17 
&Scoped-Define DISPLAYED-OBJECTS FIni FFin T_Transacciones T_Consignaciones ~
T_Retiros F_Eleccion T_ImpCon F_Transacciones T_ImpTra F_Detalle T_ImpDet 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 R4 R1 R2 R3 Id_Procesando 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Menu Definitions                                                     */
DEFINE MENU POPUP-MENU-Brw_Consolidado 
       MENU-ITEM m_Sipla        LABEL "Gestión SIPLA" .

DEFINE MENU POPUP-MENU-Brw_Detalle 
       MENU-ITEM m_Cliente      LABEL "Ver Información del Cliente".


/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 11 BY 1.62
     BGCOLOR 8 .

DEFINE BUTTON Btn_Ejecutar 
     LABEL "Ejecutar" 
     SIZE 12 BY 1.08.

DEFINE BUTTON Btn_MaxCon 
     LABEL "Maximizar" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_MaxDet 
     LABEL "Maximizar" 
     SIZE 15 BY .81.

DEFINE BUTTON Btn_MaxTra 
     LABEL "Maximizar" 
     SIZE 15 BY .81.

DEFINE BUTTON BUTTON-1 
     LABEL "Cambiar Valores Predeterminados" 
     SIZE 25 BY 1.08.

DEFINE BUTTON BUTTON-15 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 15" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-17 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 17" 
     SIZE 4 BY 1.12.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 2" 
     SIZE 11 BY 1.62.

DEFINE VARIABLE FFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE FIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE F_Detalle AS CHARACTER FORMAT "X(60)":U INITIAL "Detalle" 
     VIEW-AS FILL-IN 
     SIZE 51.43 BY .81
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE F_Eleccion AS CHARACTER FORMAT "X(50)":U INITIAL "Valores Consolidados por Agencia" 
     VIEW-AS FILL-IN 
     SIZE 49 BY .81
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE F_Transacciones AS CHARACTER FORMAT "X(60)":U INITIAL "Transacciones" 
     VIEW-AS FILL-IN 
     SIZE 52 BY .81
     FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE T_Consignaciones AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Total Consignaciones" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE T_Retiros AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Total de Retiros" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE T_Transacciones AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Nro.Transacciones" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 85 BY 1.88.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.35.

DEFINE RECTANGLE RECT-11
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 33.86 BY 1.35.

DEFINE RECTANGLE RECT-13
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.14 BY 1.35.

DEFINE VARIABLE T_ImpCon AS LOGICAL INITIAL no 
     LABEL "Imprimir" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE T_ImpDet AS LOGICAL INITIAL no 
     LABEL "Imprimir" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE T_ImpTra AS LOGICAL INITIAL no 
     LABEL "Imprimir" 
     VIEW-AS TOGGLE-BOX
     SIZE 11.57 BY .77 NO-UNDO.

DEFINE VARIABLE Id_Procesando AS CHARACTER FORMAT "X(256)":U INITIAL "Procesando..." 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     FGCOLOR 7 FONT 4 NO-UNDO.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .54
     BGCOLOR 17 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .54
     BGCOLOR 17 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .54
     BGCOLOR 17 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 3 BY .54
     BGCOLOR 17 .

DEFINE BUTTON BUTTON-7 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 7" 
     SIZE 9 BY 1.62.

DEFINE BUTTON BUTTON-8 
     LABEL "Asumir Rangos de Entidad" 
     SIZE 29 BY 1.12.

DEFINE VARIABLE Cmb_Filtro AS CHARACTER FORMAT "X(256)":U INITIAL "01 - Todas las Transacciones" 
     LABEL "Filtrar" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "01 - Todas las Transacciones","02 - Transacciones No Usuales","03 - Transacciones Sospechosas","04 - Transacciones Reportadas","05 - Transacciones Exoneradas" 
     DROP-DOWN-LIST
     SIZE 50.72 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE V_FinDia AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor Final" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE V_IniDia AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Valor Inicial" 
     VIEW-AS FILL-IN 
     SIZE 17.57 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE R_Eleccion AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Agencias", 1,
"Segmentos", 2,
"Clientes", 3,
"Usuarios", 4
     SIZE 60 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-14
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 1.88.

DEFINE RECTANGLE RECT-16
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 63 BY 2.69.

DEFINE BUTTON BUTTON-16 
     LABEL "Salir" 
     SIZE 15 BY .85.

DEFINE BUTTON BUTTON-18 
     LABEL "Ver Mas Información" 
     SIZE 18 BY .81.

DEFINE VARIABLE S_Informativo AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 79 BY 3.77
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_Gestion 
     LABEL "Gestionar" 
     SIZE 10 BY 1.88.

DEFINE BUTTON Btn_informa 
     LABEL "Ver Mas Información del Cliente" 
     SIZE 25 BY .81
     FONT 4.

DEFINE BUTTON BUTTON-19 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 19" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE ESipla AS CHARACTER 
     VIEW-AS EDITOR SCROLLBAR-VERTICAL
     SIZE 85 BY 4.04
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Fec_Exonerado LIKE Taquilla.Fec_Exonerado
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fec_Reportada LIKE Taquilla.Fec_Reportada
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Fec_Sospechosa LIKE Taquilla.Fec_Sospechosa
     VIEW-AS FILL-IN 
     SIZE 11.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomUsu_Exonerado AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomUsu_Reportada AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomUsu_Sospechosa AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Usu_Exonerado LIKE Taquilla.Usu_Exonerado
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Usu_Reportada LIKE Taquilla.Usu_Reportada
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Usu_Sospechosa LIKE Taquilla.Usu_Sospechosa
     VIEW-AS FILL-IN 
     SIZE 5.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ClienteSipla AS CHARACTER FORMAT "X(80)":U 
     VIEW-AS FILL-IN 
     SIZE 69 BY .81
     FGCOLOR 14  NO-UNDO.

DEFINE RECTANGLE RECT-209
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 1.35.

DEFINE RECTANGLE RECT-210
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 1.35.

DEFINE RECTANGLE RECT-211
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 96 BY 1.35.

DEFINE VARIABLE Id_Exonerada LIKE Taquilla.Id_Exonerada
     LABEL "Exonera estas transacciones de toda Sospecha?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .77
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Id_RepFiscal LIKE Taquilla.Id_RepFiscal
     LABEL "Reporta estas transacciones a un ente de Control?" 
     VIEW-AS TOGGLE-BOX
     SIZE 46 BY .77
     FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Id_Sospechosa LIKE Taquilla.Id_Sospechosa
     LABEL "Marcar Transacciones como Sospechosas?" 
     VIEW-AS TOGGLE-BOX
     SIZE 45 BY .77
     FGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw_Consolidado FOR 
      Tot SCROLLING.

DEFINE QUERY Brw_Detalle FOR 
      Det SCROLLING.

DEFINE QUERY Brw_Transacciones FOR 
      Tra SCROLLING.

DEFINE QUERY F_Sipla FOR 
      Taquilla SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw_Consolidado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw_Consolidado wWin _FREEFORM
  QUERY Brw_Consolidado NO-LOCK DISPLAY
      Tot.TCodigo FORMAT "X(14)" LABEL "Codigo/Nit"
      Tot.TNombre WIDTH 29 LABEL "Nombre"
      Tot.TPorcen WIDTH 6  LABEL "Porc"
      Tot.TNroTra WIDTH 10 LABEL "Nro.Transacc."
      Tot.TValCon WIDTH 20 LABEL "Total Consignaciones"
      Tot.TValRet WIDTH 20 LABEL "Total Retiros"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 4.04
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Brw_Detalle
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw_Detalle wWin _FREEFORM
  QUERY Brw_Detalle NO-LOCK DISPLAY
      Det.TAgeDoc LABEL "Age"
    Det.TAgeOri LABEL "Orig"
    Det.TNitDoc WIDTH 11
    Det.TFecDoc LABEL "FechaTra" FORMAT "99/99/99"
    Det.TNumDoc LABEL "Num.Docto"
    Det.TNumTra LABEL "Num.Trans"
    Det.TCodCom LABEL "Banco"
    Det.TValEfe WIDTH 12 FORMAT ">>>,>>>,>>9"
    Det.TValChe WIDTH 12 FORMAT ">>>,>>>,>>9"
          STRING(Det.THorDoc,"HH:MM:SS")
/*     Det.THorDoc / 3600 LABEL "Hora" FORM "99.99" */
    Det.TCodUsu LABEL "Usu"
    Det.TEstUsu COLUMN-LABEL "Estac."
    Det.TId_NUD LABEL "NUD"
    Det.TId_NUM LABEL "NUM"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 5.92
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Brw_Transacciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw_Transacciones wWin _FREEFORM
  QUERY Brw_Transacciones NO-LOCK DISPLAY
      Tra.TCodTra FORMAT "999999999" LABEL "Codigo"  WIDTH 10.3  
    Tra.TNomTra WIDTH 35 LABEL "Nombre Transaccion"                            
    Tra.TNroTra WIDTH 10 LABEL "Nro.Transacc."       
    Tra.TValCon WIDTH 20 LABEL "Total Consignaciones"
    Tra.TValRet WIDTH 20 LABEL "Total Retiros"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 99 BY 4
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Consulta
     BUTTON-2 AT ROW 1.27 COL 102.29
     BUTTON-1 AT ROW 1.81 COL 48
     Btn_Ejecutar AT ROW 1.81 COL 74
     FIni AT ROW 1.92 COL 10.57 COLON-ALIGNED
     FFin AT ROW 1.92 COL 32.43 COLON-ALIGNED
     BUTTON-15 AT ROW 2.88 COL 102.29
     T_Transacciones AT ROW 4 COL 14.86 COLON-ALIGNED
     T_Consignaciones AT ROW 4 COL 48.72 COLON-ALIGNED
     T_Retiros AT ROW 4 COL 79 COLON-ALIGNED
     F_Eleccion AT ROW 5.12 COL 3 NO-LABEL
     Btn_MaxCon AT ROW 5.12 COL 86
     T_ImpCon AT ROW 5.15 COL 73
     Brw_Consolidado AT ROW 6.12 COL 2
     F_Transacciones AT ROW 10.15 COL 1 COLON-ALIGNED NO-LABEL
     T_ImpTra AT ROW 10.23 COL 73
     Btn_MaxTra AT ROW 10.23 COL 86
     Brw_Transacciones AT ROW 11.08 COL 2
     F_Detalle AT ROW 15.19 COL 1.29 COLON-ALIGNED NO-LABEL
     Btn_MaxDet AT ROW 15.19 COL 86
     T_ImpDet AT ROW 15.27 COL 73
     Brw_Detalle AT ROW 16.08 COL 2
     BtnDone AT ROW 17.69 COL 102
     BUTTON-17 AT ROW 19.58 COL 106
     "Totales de la Entidad" VIEW-AS TEXT
          SIZE 15.43 BY .54 AT ROW 3.35 COL 3.57
          FGCOLOR 7 FONT 4
     RECT-1 AT ROW 1.27 COL 2
     RECT-10 AT ROW 3.65 COL 2
     RECT-11 AT ROW 3.65 COL 34.86
     RECT-13 AT ROW 3.65 COL 68.57
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.57 BY 21.38
         BGCOLOR 17 FONT 4
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F_Informativo
     S_Informativo AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-16 AT ROW 5.12 COL 65.43
     BUTTON-18 AT ROW 5.15 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 20 ROW 16.08
         SIZE 81 BY 5.92
         BGCOLOR 17 FONT 4
         TITLE "Informativo".

DEFINE FRAME F_Filtros
     R_Eleccion AT ROW 2 COL 3 NO-LABEL
     Cmb_Filtro AT ROW 3 COL 7.29 COLON-ALIGNED
     V_IniDia AT ROW 5.31 COL 12.43 COLON-ALIGNED
     V_FinDia AT ROW 5.31 COL 42 COLON-ALIGNED
     BUTTON-7 AT ROW 6.92 COL 56
     BUTTON-8 AT ROW 7.19 COL 2
     "Rango de Valores para Transacciones por Día" VIEW-AS TEXT
          SIZE 42 BY .62 AT ROW 4.5 COL 3.43
          FGCOLOR 7 
     "Listar las Operaciones de" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 1.27 COL 3
          FGCOLOR 7 
     RECT-14 AT ROW 4.77 COL 2
     RECT-16 AT ROW 1.54 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 19 ROW 2.88
         SIZE 67 BY 8.62
         BGCOLOR 17 FONT 5
         TITLE "Valores Predeterminados".

DEFINE FRAME F_Conteo
     Id_Procesando AT ROW 1.27 COL 1 COLON-ALIGNED NO-LABEL
     R4 AT ROW 2.08 COL 12
     R1 AT ROW 2.08 COL 3
     R2 AT ROW 2.08 COL 6
     R3 AT ROW 2.08 COL 9
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 87 ROW 1.25
         SIZE 15 BY 1.88
         BGCOLOR 17 .

DEFINE FRAME F_Sipla
     W_ClienteSipla AT ROW 1.12 COL 1 COLON-ALIGNED NO-LABEL
     Btn_informa AT ROW 1.15 COL 73
     Id_Sospechosa AT ROW 3 COL 2.86 HELP
          ""
          LABEL "Marcar Transacciones como Sospechosas?"
          FGCOLOR 15 
     Fec_Sospechosa AT ROW 3 COL 48 COLON-ALIGNED HELP
          "" NO-LABEL
          BGCOLOR 18 FGCOLOR 15 
     Usu_Sospechosa AT ROW 3 COL 60 COLON-ALIGNED HELP
          "" NO-LABEL
          BGCOLOR 18 FGCOLOR 15 
     NomUsu_Sospechosa AT ROW 3 COL 66 COLON-ALIGNED NO-LABEL
     Id_Exonerada AT ROW 4.35 COL 2.72 HELP
          ""
          LABEL "Exonera estas transacciones de toda Sospecha?"
          FGCOLOR 15 
     Fec_Exonerado AT ROW 4.35 COL 48 COLON-ALIGNED HELP
          "" NO-LABEL
          BGCOLOR 18 FGCOLOR 15 
     Usu_Exonerado AT ROW 4.35 COL 60 COLON-ALIGNED HELP
          "" NO-LABEL
          BGCOLOR 18 FGCOLOR 15 
     NomUsu_Exonerado AT ROW 4.35 COL 66 COLON-ALIGNED NO-LABEL
     Id_RepFiscal AT ROW 5.69 COL 2.72 HELP
          ""
          LABEL "Reporta estas transacciones a un ente de Control?"
          FGCOLOR 15 
     Fec_Reportada AT ROW 5.69 COL 48 COLON-ALIGNED HELP
          "" NO-LABEL
          BGCOLOR 18 FGCOLOR 15 
     Usu_Reportada AT ROW 5.69 COL 60 COLON-ALIGNED HELP
          "" NO-LABEL
          BGCOLOR 18 FGCOLOR 15 
     NomUsu_Reportada AT ROW 5.69 COL 66 COLON-ALIGNED NO-LABEL
     ESipla AT ROW 7.73 COL 2 NO-LABEL
     Btn_Gestion AT ROW 7.73 COL 88
     BUTTON-19 AT ROW 9.88 COL 88
     "Usu" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 2.04 COL 62.57
          FGCOLOR 15 
     "Fecha" VIEW-AS TEXT
          SIZE 6 BY .62 AT ROW 2.04 COL 52
          FGCOLOR 15 
     "Descripción de la Gestión" VIEW-AS TEXT
          SIZE 23 BY .62 AT ROW 6.92 COL 2
          FGCOLOR 15 
     "Nombre del Usuario" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 2.04 COL 68.14
          FGCOLOR 15 
     RECT-209 AT ROW 2.73 COL 2
     RECT-210 AT ROW 4.08 COL 2
     RECT-211 AT ROW 5.42 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2 ROW 10.15
         SIZE 99 BY 11.85
         BGCOLOR 18 FGCOLOR 15 FONT 5
         TITLE "Gestión del SIPLA".


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
         TITLE              = "SFG - Consulta General de Transacciones"
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
ASSIGN FRAME F_Conteo:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Filtros:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Informativo:FRAME = FRAME F_Consulta:HANDLE
       FRAME F_Sipla:FRAME = FRAME F_Consulta:HANDLE.

/* SETTINGS FOR FRAME F_Consulta
   FRAME-NAME                                                           */
/* BROWSE-TAB Brw_Consolidado T_ImpCon F_Consulta */
/* BROWSE-TAB Brw_Transacciones Btn_MaxTra F_Consulta */
/* BROWSE-TAB Brw_Detalle T_ImpDet F_Consulta */
ASSIGN 
       Brw_Consolidado:POPUP-MENU IN FRAME F_Consulta             = MENU POPUP-MENU-Brw_Consolidado:HANDLE.

ASSIGN 
       Brw_Detalle:POPUP-MENU IN FRAME F_Consulta             = MENU POPUP-MENU-Brw_Detalle:HANDLE.

/* SETTINGS FOR FILL-IN F_Detalle IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F_Eleccion IN FRAME F_Consulta
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN F_Transacciones IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN T_Consignaciones IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN T_Retiros IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN T_Transacciones IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Conteo
                                                                        */
/* SETTINGS FOR FILL-IN Id_Procesando IN FRAME F_Conteo
   NO-DISPLAY NO-ENABLE 1                                               */
/* SETTINGS FOR RECTANGLE R1 IN FRAME F_Conteo
   NO-ENABLE 1                                                          */
ASSIGN 
       R1:HIDDEN IN FRAME F_Conteo           = TRUE.

/* SETTINGS FOR RECTANGLE R2 IN FRAME F_Conteo
   NO-ENABLE 1                                                          */
ASSIGN 
       R2:HIDDEN IN FRAME F_Conteo           = TRUE.

/* SETTINGS FOR RECTANGLE R3 IN FRAME F_Conteo
   NO-ENABLE 1                                                          */
ASSIGN 
       R3:HIDDEN IN FRAME F_Conteo           = TRUE.

/* SETTINGS FOR RECTANGLE R4 IN FRAME F_Conteo
   NO-ENABLE 1                                                          */
ASSIGN 
       R4:HIDDEN IN FRAME F_Conteo           = TRUE.

/* SETTINGS FOR FRAME F_Filtros
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Filtros:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Informativo
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Informativo:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Sipla
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Sipla:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_Gestion IN FRAME F_Sipla
   NO-ENABLE                                                            */
/* SETTINGS FOR EDITOR ESipla IN FRAME F_Sipla
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Fec_Exonerado IN FRAME F_Sipla
   NO-ENABLE LIKE = bdCentral.Taquilla.                                 */
/* SETTINGS FOR FILL-IN Fec_Reportada IN FRAME F_Sipla
   NO-ENABLE LIKE = bdCentral.Taquilla.                                 */
/* SETTINGS FOR FILL-IN Fec_Sospechosa IN FRAME F_Sipla
   NO-ENABLE LIKE = bdCentral.Taquilla. EXP-SIZE                        */
/* SETTINGS FOR TOGGLE-BOX Id_Exonerada IN FRAME F_Sipla
   LIKE = bdCentral.Taquilla. EXP-LABEL                                 */
/* SETTINGS FOR TOGGLE-BOX Id_RepFiscal IN FRAME F_Sipla
   LIKE = bdCentral.Taquilla. EXP-LABEL EXP-SIZE                        */
/* SETTINGS FOR TOGGLE-BOX Id_Sospechosa IN FRAME F_Sipla
   LIKE = bdCentral.Taquilla. EXP-LABEL                                 */
/* SETTINGS FOR FILL-IN NomUsu_Exonerado IN FRAME F_Sipla
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomUsu_Reportada IN FRAME F_Sipla
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN NomUsu_Sospechosa IN FRAME F_Sipla
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Usu_Exonerado IN FRAME F_Sipla
   NO-ENABLE LIKE = bdCentral.Taquilla.                                 */
/* SETTINGS FOR FILL-IN Usu_Reportada IN FRAME F_Sipla
   NO-ENABLE LIKE = bdCentral.Taquilla.                                 */
/* SETTINGS FOR FILL-IN Usu_Sospechosa IN FRAME F_Sipla
   NO-ENABLE LIKE = bdCentral.Taquilla.                                 */
/* SETTINGS FOR FILL-IN W_ClienteSipla IN FRAME F_Sipla
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw_Consolidado
/* Query rebuild information for BROWSE Brw_Consolidado
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Tot NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Brw_Consolidado */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw_Detalle
/* Query rebuild information for BROWSE Brw_Detalle
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Det NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Brw_Detalle */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw_Transacciones
/* Query rebuild information for BROWSE Brw_Transacciones
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Tra NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Brw_Transacciones */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Sipla
/* Query rebuild information for FRAME F_Sipla
     _TblList          = "bdCentral.Taquilla"
     _Query            is OPENED
*/  /* FRAME F_Sipla */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Consulta General de Transacciones */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Consulta General de Transacciones */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Brw_Consolidado
&Scoped-define SELF-NAME Brw_Consolidado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw_Consolidado wWin
ON MOUSE-SELECT-DBLCLICK OF Brw_Consolidado IN FRAME F_Consulta
DO:
  FOR EACH Tra: DELETE Tra. END.
  RUN Transacciones.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Brw_Transacciones
&Scoped-define SELF-NAME Brw_Transacciones
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw_Transacciones wWin
ON MOUSE-SELECT-DBLCLICK OF Brw_Transacciones IN FRAME F_Consulta
DO:
  FOR EACH Det: DELETE Det. END.
  RUN Detalle.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F_Consulta /* Salir */
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


&Scoped-define SELF-NAME Btn_Ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ejecutar wWin
ON CHOOSE OF Btn_Ejecutar IN FRAME F_Consulta /* Ejecutar */
DO:
  RUN Inicializar_Consulta.
  ASSIGN FRAME F_Filtros R_Eleccion V_Inidia V_FinDia Cmb_Filtro.
  ASSIGN FRAME F_Consulta FIni FFin
         TotTra = 0 TotCon = 0 TotRet = 0.
  FOR EACH TTaquilla: DELETE TTaquilla. END.
  RUN Temporal_Taquilla.
  RUN Acumulados.
/*  CASE R_Eleccion:
       WHEN 1 THEN RUN Acumulados_Agencia.
       WHEN 2 THEN RUN Acumulados_Segmento.
       WHEN 3 THEN RUN Acumulados_Clientes.
       WHEN 4 THEN RUN Acumulados_Usuario.
  END CASE.*/
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Sipla
&Scoped-define SELF-NAME Btn_Gestion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Gestion wWin
ON CHOOSE OF Btn_Gestion IN FRAME F_Sipla /* Gestionar */
DO:
  ASSIGN FRAME F_Sipla Id_RepFiscal Id_Exonerada Id_Sospechosa
    Fec_Exonerado Usu_Exonerado Fec_Sospechosa Usu_Sospechosa Fec_Reportada Usu_Reportada.
  IF NOT Id_RepFiscal AND NOT Id_Exonerada AND NOT Id_Sospechosa THEN DO:
     MESSAGE "Paga relizar la gestion debe haber Decidido" SKIP
             "ante la información. Rectifique!" VIEW-AS ALERT-BOX.
     APPLY "entry" TO Btn_Informa.
     RETURN NO-APPLY.
  END.
  IF ESipla:SCREEN-VALUE EQ "" THEN DO:
     MESSAGE "No se puede gestionar si no se entra una descripcion" SKIP
             "de la gestion realizada. Rectifique" VIEW-AS ALERT-BOX.
     APPLY "entry" TO ESipla.
     RETURN NO-APPLY.
  END.
  
  CREATE Hoja_Vida.
  ASSIGN Hoja_Vida.Codigo         = 6
         Hoja_Vida.DoctoRefer     = Cfg_Instancias.Instancia
         Hoja_Vida.Fec_Grabacion  = TODAY
         Hoja_Vida.Hora_Grabacion = TIME
         Hoja_Vida.Nit            = Tot.TCodigo
         Hoja_Vida.Tipo           = 9
         Hoja_Vida.Usuario        = W_Usuario.
         
  IF Id_RepFiscal AND Id_RepFiscal:SENSITIVE EQ YES THEN
     Hoja_Vida.Observacion    = "Transacciones Reportadas: " + ESipla:SCREEN-VALUE.
     
  IF Id_Sospechosa AND Id_Sospechosa:SENSITIVE EQ YES THEN
     Hoja_Vida.Observacion    = "Transacciones Marcadas como Sospechosas: " + ESipla:SCREEN-VALUE.

  IF Id_Exonerada AND Id_Exonerada:SENSITIVE EQ YES THEN
     Hoja_Vida.Observacion    = "Transacciones Exoneradas: " + ESipla:SCREEN-VALUE.
  
  FIND Clientes WHERE Clientes.Nit EQ Tot.TCodigo NO-ERROR.
  IF AVAILABLE Clientes THEN Clientes.Con_Sospechosas = Clientes.Con_Sospechosas + 1.
  FOR EACH Det WHERE Det.TNitDoc = Tot.TCodigo:
      FIND FIRST Taquilla WHERE 
           Taquilla.Agencia EQ Det.TAgeDoc AND 
           Taquilla.Nit EQ Det.TNitDoc AND 
           Taquilla.Nro_Transaccion EQ Det.TNumTra NO-ERROR.
      IF AVAILABLE Taquilla THEN DO:
         ASSIGN Taquilla.Id_RepFiscal   = Id_RepFiscal
                Taquilla.Fec_Reportada  = Fec_Reportada
                Taquilla.Usu_Reportada  = W_Usuario
                Taquilla.Id_Sospechosa  = Id_Sospechosa
                Taquilla.Fec_Sospechosa = W_Fecha
                Taquilla.Usu_Sospechosa = Usu_Sospechosa
                Taquilla.Id_Exonerada   = Id_Exonerada
                Taquilla.Fec_Exonerado  = Fec_Exonerado
                Taquilla.Usu_Exonerado  = Usu_Exonerado.
          FIND FIRST TTaquilla WHERE 
               TTaquilla.Agencia EQ Det.TAgeDoc AND 
               TTaquilla.Nit EQ Det.TNitDoc AND 
               TTaquilla.Nro_Transaccion EQ Det.TNumTra NO-ERROR.
          IF AVAILABLE TTaquilla THEN DELETE TTaquilla.
      END.
      ELSE
        MESSAGE "No disponible TRANS: " det.Tnumtra " Nit: " det.tnitdoc.
  END.
  APPLY "choose" TO Btn_Ejecutar IN FRAME F_Consulta.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_informa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_informa wWin
ON CHOOSE OF Btn_informa IN FRAME F_Sipla /* Ver Mas Información del Cliente */
DO:
  RUN W-ConsultaGeneral.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_MaxCon
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_MaxCon wWin
ON CHOOSE OF Btn_MaxCon IN FRAME F_Consulta /* Maximizar */
DO:
  IF SELF:LABEL EQ "Maximizar" THEN 
     ASSIGN Brw_Consolidado:HEIGHT   = 16
            Btn_MaxCon:LABEL         = "Minimizar"
            F_Transacciones:HIDDEN   = YES
            Brw_Transacciones:HIDDEN = YES
            Btn_MaxTra:HIDDEN        = YES
            F_Detalle:HIDDEN         = YES
            Brw_Detalle:HIDDEN       = YES
            Btn_MaxDet:HIDDEN        = YES
            T_ImpDet:HIDDEN          = YES
            T_ImpTra:HIDDEN          = YES.
  ELSE
     ASSIGN Brw_Consolidado:HEIGHT   = 4.04
            Btn_MaxCon:LABEL         = "Maximizar"  
            F_Transacciones:HIDDEN   = NO
            Brw_Transacciones:HIDDEN = NO
            Btn_MaxTra:HIDDEN        = NO
            F_Detalle:HIDDEN         = NO
            Brw_Detalle:HIDDEN       = NO
            Btn_MaxDet:HIDDEN        = NO
            T_ImpDet:HIDDEN          = NO
            T_ImpTra:HIDDEN          = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_MaxDet
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_MaxDet wWin
ON CHOOSE OF Btn_MaxDet IN FRAME F_Consulta /* Maximizar */
DO:
  IF SELF:LABEL EQ "Maximizar" THEN 
     ASSIGN Brw_Detalle:ROW          = 6
            Brw_Detalle:HEIGHT       = 16
            T_ImpDet:ROW             = 5.15
            Btn_MaxDet:ROW           = 5.12
            F_Detalle:ROW            = 5.12
            Btn_MaxDet:LABEL         = "Minimizar".
  ELSE
     ASSIGN Brw_Detalle:HEIGHT       = 5.92
            T_ImpDet:ROW             = 15.27
            Brw_Detalle:ROW          = 16.08
            Btn_MaxDet:ROW           = 15.19
            F_Detalle:ROW            = 15.19
            Btn_MaxDet:LABEL         = "Maximizar".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_MaxTra
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_MaxTra wWin
ON CHOOSE OF Btn_MaxTra IN FRAME F_Consulta /* Maximizar */
DO:
  IF SELF:LABEL EQ "Maximizar" THEN 
     ASSIGN Brw_Transacciones:ROW    = 6
            Brw_Transacciones:HEIGHT = 16
            T_ImpTra:ROW             = 5.12
            Btn_MaxTra:ROW           = 5.12
            F_Transacciones:ROW      = 5.12
            Btn_MaxTra:LABEL         = "Minimizar"
            F_Detalle:HIDDEN         = YES
            Brw_Detalle:HIDDEN       = YES
            Btn_MaxDet:HIDDEN        = YES
            T_ImpDet:HIDDEN          = YES.
  ELSE
     ASSIGN Brw_Transacciones:HEIGHT = 4
            Brw_Transacciones:ROW    = 11.08
            T_ImpTra:ROW             = 10.27
            Btn_MaxTra:ROW           = 10.23
            F_Transacciones:ROW      = 10.15
            Btn_MaxTra:LABEL         = "Maximizar"
            F_Detalle:HIDDEN         = NO
            Brw_Detalle:HIDDEN       = NO
            Btn_MaxDet:HIDDEN        = NO
            T_ImpDet:HIDDEN          = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Consulta /* Cambiar Valores Predeterminados */
DO:
  VIEW FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-15
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-15 wWin
ON CHOOSE OF BUTTON-15 IN FRAME F_Consulta /* Button 15 */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_Pathspl + "L-varios.lst".
  {Incluido\IMPRIMIR.i "Listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Informativo
&Scoped-define SELF-NAME BUTTON-16
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-16 wWin
ON CHOOSE OF BUTTON-16 IN FRAME F_Informativo /* Salir */
DO:
  HIDE FRAME F_Informativo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-18
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-18 wWin
ON CHOOSE OF BUTTON-18 IN FRAME F_Informativo /* Ver Mas Información */
DO:
  RUN W-ConsultaGeneral.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Sipla
&Scoped-define SELF-NAME BUTTON-19
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-19 wWin
ON CHOOSE OF BUTTON-19 IN FRAME F_Sipla /* Button 19 */
DO:
  HIDE FRAME F_Sipla.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Filtros
&Scoped-define SELF-NAME BUTTON-7
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-7 wWin
ON CHOOSE OF BUTTON-7 IN FRAME F_Filtros /* Button 7 */
DO:
  HIDE FRAME F_Filtros.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-8
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-8 wWin
ON CHOOSE OF BUTTON-8 IN FRAME F_Filtros /* Asumir Rangos de Entidad */
DO:
  FIND Entidad WHERE Entidad.Entidad EQ W_Entidad NO-LOCK NO-ERROR.
  IF AVAILABLE Entidad THEN DO:
     ASSIGN V_IniDia:SCREEN-VALUE IN FRAME F_Filtros = STRING(MaxOp_Efectivo_Dia).
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Sipla
&Scoped-define SELF-NAME Id_Exonerada
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_Exonerada wWin
ON VALUE-CHANGED OF Id_Exonerada IN FRAME F_Sipla /* Exonera estas transacciones de toda Sospecha? */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
    FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
    ASSIGN Fec_Exonerado:SCREEN-VALUE = STRING(TODAY)
           Usu_Exonerado:SCREEN-VALUE = W_Usuario.
           NomUsu_Exonerado:SCREEN-VALUE = Usuarios.Nombre.
    ENABLE ESipla Btn_Gestion WITH FRAME F_Sipla.
    APPLY "entry" TO ESipla.
    RETURN NO-APPLY.
  END.
  ELSE
      ASSIGN Fec_Exonerado:SCREEN-VALUE = ""
           Usu_Exonerado:SCREEN-VALUE = ""
           NomUsu_Exonerado:SCREEN-VALUE = ""
           ESipla:SCREEN-VALUE = ""
           ESipla:SENSITIVE = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_RepFiscal
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_RepFiscal wWin
ON VALUE-CHANGED OF Id_RepFiscal IN FRAME F_Sipla /* Reporta estas transacciones a un ente de Control? */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
    FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
    ASSIGN Fec_Reportada:SCREEN-VALUE = STRING(TODAY)
           Usu_Reportada:SCREEN-VALUE = W_Usuario.
           NomUsu_Reportada:SCREEN-VALUE = Usuarios.Nombre.
    ENABLE ESipla  Btn_Gestion WITH FRAME F_Sipla.
    APPLY "entry" TO ESipla.
    RETURN NO-APPLY.
  END.
  ELSE
    ASSIGN Fec_Reportada:SCREEN-VALUE = ""
           Usu_Reportada:SCREEN-VALUE = ""
           NomUsu_Reportada:SCREEN-VALUE = ""
           ESipla:SCREEN-VALUE = ""
           ESipla:SENSITIVE = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Id_Sospechosa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Id_Sospechosa wWin
ON VALUE-CHANGED OF Id_Sospechosa IN FRAME F_Sipla /* Marcar Transacciones como Sospechosas? */
DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN DO:
    FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
    ASSIGN Fec_Sospechosa:SCREEN-VALUE = STRING(TODAY)
           Usu_Sospechosa:SCREEN-VALUE = W_Usuario.
           NomUsu_Sospechosa:SCREEN-VALUE = Usuarios.Nombre.
    ENABLE ESipla  Btn_Gestion WITH FRAME F_Sipla.
    APPLY "entry" TO ESipla.
    RETURN NO-APPLY.
  END.
  ELSE
    ASSIGN Fec_Sospechosa:SCREEN-VALUE = ""
           Usu_Sospechosa:SCREEN-VALUE = ""
           NomUsu_Sospechosa:SCREEN-VALUE = ""
           ESipla:SCREEN-VALUE = ""
           ESipla:SENSITIVE = NO.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Cliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Cliente wWin
ON CHOOSE OF MENU-ITEM m_Cliente /* Ver Información del Cliente */
DO:
DEFINE VAR WComodin AS CHARACTER FORMAT "X(15)".
DO WITH FRAME F_Informativo:
  DO i = 1 TO 20:
     W_Ok = S_Informativo:DELETE(1).
  END.
  FIND Clientes WHERE Clientes.Nit EQ Det.TNitDoc NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN DO:
     W_Ok = S_Informativo:ADD-LAST("Agencia Origen : " + STRING(Clientes.Agencia,"999") + 
                                "         - Nombre      : " + Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
     CASE Clientes.Tipo_Vinculo:
       WHEN 1 THEN WComodin = "Asociado".
       WHEN 2 THEN WComodin = "Cliente NA".
       WHEN 3 THEN WComodin = "Tercero".
       WHEN 4 THEN WComodin = "Proveedor".
     END CASE.
     W_Ok = S_Informativo:ADD-LAST("Fecha Ingreso  : " + STRING(Clientes.Fec_Ingreso) +
                                   "    - Tipo Vinculo: " + WComodin).
     W_Ok = S_Informativo:ADD-LAST("Tel.Residencia : " + STRING(Clientes.Tel_Residencia,"x(10)") + 
                                      "  - Dir.Residenc: " + Clientes.DIR_Residencia).
     W_Ok = S_Informativo:ADD-LAST("Tel.Comercial  : " + STRING(Clientes.Tel_Comercial,"x(10)") + 
                                      "  - Dir.Comercia: " + Clientes.DIR_Comercial).
  END.
  VIEW FRAME F_Informativo.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME m_Sipla
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL m_Sipla wWin
ON CHOOSE OF MENU-ITEM m_Sipla /* Gestión SIPLA */
DO:
  DEFINE VAR W_Mensaje AS CHARACTER FORMAT "X(50)".
  ASSIGN FRAME F_Filtros R_Eleccion Cmb_Filtro.
  IF R_Eleccion NE 3 THEN 
     MESSAGE "La gestion de SIPLA solo puede ser procesada" SKIP
             "Cuando el filtro de la información esta en" SKIP
             "la opción Clientes. RECTIFIQUE!" VIEW-AS ALERT-BOX WARNING.
  ELSE DO:
     IF SUBSTRING(Cmb_Filtro,1,2) EQ "01" OR 
        SUBSTRING(Cmb_Filtro,1,2) EQ "04" OR 
        SUBSTRING(Cmb_Filtro,1,2) EQ "05" THEN
        MESSAGE "Debe escogerse una opción diferente" SKIP
                "en la pantalla de Filtros." SKIP 
                "La opcion escogida no permite la Gestion SIPLA" VIEW-AS ALERT-BOX.
     ELSE RUN Validar_Instancias (OUTPUT W_Ok, OUTPUT W_Mensaje).
     IF W_Ok THEN DO:
        ASSIGN ESipla:SCREEN-VALUE IN FRAME F_Sipla = "".
        W_ClienteSipla:SCREEN-VALUE IN FRAME F_Sipla = STRING(Tot.TCodigo) + " - " + Tot.TNombre.
        VIEW FRAME F_Sipla.
     END.
     ELSE IF W_Mensaje NE "" THEN MESSAGE W_Mensaje VIEW-AS ALERT-BOX TITLE "Permiso de Gestion SIPLA".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define BROWSE-NAME Brw_Consolidado
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Acumulados wWin 
PROCEDURE Acumulados :
DEFINE VAR WNom AS CHARACTER FORMAT "X(15)".
 DEFINE VAR WCod AS INTEGER FORMAT "99999".
CASE R_Eleccion:
    WHEN 1 THEN DO:
        FOR EACH TTaquilla BREAK BY TTaquilla.Agencia:
            RUN Acumular_Linea.
            IF LAST-OF(TTaquilla.Agencia) THEN DO:
               FIND Agencias WHERE Agencias.Agencia EQ TTaquilla.Agencia NO-LOCK NO-ERROR.
               CREATE Tot.
               ASSIGN Tot.TCodigo      = TRIM(STRING(Agencias.Agencia,"999"))
                      Tot.TNombre      = Agencias.Nombre
                      Tot.TNroTra      = TotTra
                      Tot.TValCon      = TotCon
                      Tot.TValRet      = TotRet
                      T_Transacciones  = T_Transacciones + TotTra
                      T_Consignaciones = T_Consignaciones + TotCon
                      T_Retiros        = T_Retiros + TotRet
                      TotTra = 0 TotCon = 0 TotRet = 0.
            END.
        END.
    END.
    WHEN 2 THEN DO:
         FOR EACH TTaquilla BREAK BY TTaquilla.Cod_Segmento:
             RUN Acumular_Linea.
             IF LAST-OF(TTaquilla.Cod_Segmento) THEN DO:
                FIND Varios WHERE Varios.Tipo EQ 6 AND Varios.Codigo EQ TTaquilla.Cod_Segmento NO-LOCK NO-ERROR.
                IF AVAILABLE Varios THEN ASSIGN WNom = Varios.Descripcion WCod = Varios.Codigo.
                ELSE ASSIGN WNom = "Sin Segmento" WCod = 0.
                CREATE Tot.
                ASSIGN Tot.TCodigo      = STRING(WCod,"99999")
                       Tot.TNombre      = WNom
                       Tot.TNroTra      = TotTra
                       Tot.TValCon      = TotCon
                       Tot.TValRet      = TotRet
                       T_Transacciones  = T_Transacciones + TotTra
                       T_Consignaciones = T_Consignaciones + TotCon
                       T_Retiros        = T_Retiros + TotRet
                       TotTra = 0 TotCon = 0 TotRet = 0.
             END.
         END.
    END.
    WHEN 3 THEN DO:
        FOR EACH TTaquilla BREAK BY TTaquilla.Nit:
            RUN Acumular_Linea.
            IF LAST-OF(TTaquilla.Nit) THEN DO:
               FIND Clientes WHERE Clientes.Nit EQ TTaquilla.Nit NO-LOCK NO-ERROR.
               IF AVAILABLE Clientes THEN DO:
                 CREATE Tot.
                 ASSIGN Tot.TCodigo      = TRIM(STRING(Clientes.Nit,"X(14)"))
                        Tot.TNombre      = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2
                        Tot.TNroTra      = TotTra
                        Tot.TValCon      = TotCon
                        Tot.TValRet      = TotRet
                        T_Transacciones  = T_Transacciones + TotTra
                        T_Consignaciones = T_Consignaciones + TotCon
                        T_Retiros        = T_Retiros + TotRet
                        TotTra = 0 TotCon = 0 TotRet = 0.
               END.
            END.
        END.
    END.
    WHEN 4 THEN DO:
      FOR EACH TTaquilla BREAK BY TTaquilla.Usuario:
         RUN Acumular_Linea.
          IF LAST-OF(TTaquilla.Usuario) THEN DO:
             FIND Usuarios WHERE Usuarios.Usuario EQ TTaquilla.Usuario NO-LOCK NO-ERROR.
             CREATE Tot.
             ASSIGN Tot.TCodigo      = TRIM(STRING(Usuarios.Usuario,"X(4)"))
                    Tot.TNombre      = Usuarios.Nombre
                    Tot.TNroTra      = TotTra
                    Tot.TValCon      = TotCon
                    Tot.TValRet      = TotRet
                    T_Transacciones  = T_Transacciones + TotTra
                    T_Consignaciones = T_Consignaciones + TotCon
                    T_Retiros        = T_Retiros + TotRet
                    TotTra = 0 TotCon = 0 TotRet = 0.
          END.
      END.
    END.
  END CASE.
  FOR EACH Tot:
      Tot.TPorcen = (Tot.TNroTra * 100) / T_Transacciones.
  END.
 DISPLAY T_Transacciones T_Consignaciones T_Retiros WITH FRAME F_Consulta.
 OPEN QUERY Brw_Consolidado FOR EACH Tot NO-LOCK INDEXED-REPOSITION.
 RUN Transacciones.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Acumular_Linea wWin 
PROCEDURE Acumular_Linea :
TotTra = TotTra + 1.
   IF SUBSTRING(STRING(TTaquilla.Cod_Operacion,"999999999"),5,2) EQ "01" THEN
      TotCon = TotCon + TTaquilla.Val_Efectivo + TTaquilla.Val_Cheque.
   ELSE
      TotRet = TotRet + TTaquilla.Val_Efectivo + TTaquilla.Val_Cheque.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crear_Detalle wWin 
PROCEDURE Crear_Detalle :
CREATE Det.
     ASSIGN TAgeDoc = TTaquilla.Agencia
            TAgeOri = TTaquilla.Age_Fuente
            TNitDoc = TTaquilla.Nit             
            TFecDoc = TTaquilla.Fec_Transaccion 
            TNumDoc = TTaquilla.Num_documento   
            TNumTra = TTaquilla.Nro_Transaccion 
            TCodCom = TTaquilla.Cod_Compensa    
            TValEfe = TTaquilla.Val_Efectivo    
            TValChe = TTaquilla.Val_Cheque      
            THorDoc = TTaquilla.Hora_Transaccion
            TCodUsu = TTaquilla.Usuario         
            TEstUsu = TTaquilla.Estacion
            TId_NUD = TTaquilla.Id_NUD
            TId_NuM = TTaquilla.Id_NUM
            TId_Sos = TTaquilla.Id_Sospechosa
            TId_Rep = TTaquilla.Id_RepFiscal
            TId_Exo = TTaquilla.Id_Exonera
            TFc_Sos = TTaquilla.Fec_Sospechosa
            TFc_Exo = TTaquilla.Fec_Exonera
            TFc_Rep = TTaquilla.Fec_Reporta
            TUs_Sos = TTaquilla.Usu_Sospechosa
            TUs_Rep = TTaquilla.Usu_Reportada
            TUs_Exo = TTaquilla.Usu_Exonera.       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Crear_Transaccion wWin 
PROCEDURE Crear_Transaccion :
CREATE Tra.
   ASSIGN Tra.TCodTra = Operacion.Cod_Operacion
          Tra.TNomTra = Operacion.Nom_Operacion
          Tra.TNroTra = TotTra
          Tra.TValcon = TotCon
          Tra.TValRet = TotRet.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Detalle wWin 
PROCEDURE Detalle :
DO WITH FRAME F_Consulta:
 F_Detalle:SCREEN-VALUE = "Detalle de la Transaccion:  " + CAPS(Tra.TNomTra).
 CASE R_Eleccion:
    WHEN 1 THEN DO:
      FOR EACH TTaquilla WHERE TTaquilla.Agencia EQ INTEGER(Tot.TCodigo) AND 
          TTaquilla.Cod_Operacion EQ Tra.TCodTra BREAK BY TTaquilla.Agencia BY TTaquilla.Fec_Transaccion:
          RUN Crear_Detalle.
      END.
    END.
    WHEN 2 THEN DO:
      FOR EACH TTaquilla WHERE TTaquilla.Cod_Segmento EQ INTEGER(Tot.TCodigo) AND 
          TTaquilla.Cod_Operacion EQ Tra.TCodTra BREAK BY TTaquilla.Agencia BY TTaquilla.Fec_Transaccion:
          RUN Crear_Detalle.
      END.
    END.
    WHEN 3 THEN DO:
      FOR EACH TTaquilla WHERE TTaquilla.Nit EQ Tot.TCodigo AND 
          TTaquilla.Cod_Operacion EQ Tra.TCodTra BREAK BY TTaquilla.Agencia BY TTaquilla.Fec_Transaccion:
          RUN Crear_Detalle.
      END.
    END.
    WHEN 4 THEN DO:
      FOR EACH TTaquilla WHERE TTaquilla.Usuario EQ Tot.TCodigo AND 
          TTaquilla.Cod_Operacion EQ Tra.TCodTra BREAK BY TTaquilla.Agencia BY TTaquilla.Fec_Transaccion:
          RUN Crear_Detalle.
      END.
    END.
 END CASE.
 OPEN QUERY Brw_Detalle FOR EACH Det NO-LOCK INDEXED-REPOSITION.
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
  DISPLAY FIni FFin T_Transacciones T_Consignaciones T_Retiros F_Eleccion 
          T_ImpCon F_Transacciones T_ImpTra F_Detalle T_ImpDet 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE RECT-1 RECT-10 RECT-11 RECT-13 BUTTON-2 BUTTON-1 Btn_Ejecutar FIni 
         FFin BUTTON-15 Btn_MaxCon T_ImpCon Brw_Consolidado T_ImpTra Btn_MaxTra 
         Brw_Transacciones Btn_MaxDet T_ImpDet Brw_Detalle BtnDone BUTTON-17 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  VIEW FRAME F_Conteo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Conteo}
  DISPLAY R_Eleccion Cmb_Filtro V_IniDia V_FinDia 
      WITH FRAME F_Filtros IN WINDOW wWin.
  ENABLE RECT-14 RECT-16 R_Eleccion Cmb_Filtro V_IniDia V_FinDia BUTTON-7 
         BUTTON-8 
      WITH FRAME F_Filtros IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Filtros}

  {&OPEN-QUERY-F_Sipla}
  GET FIRST F_Sipla.
  DISPLAY W_ClienteSipla Id_Sospechosa Fec_Sospechosa Usu_Sospechosa 
          NomUsu_Sospechosa Id_Exonerada Fec_Exonerado Usu_Exonerado 
          NomUsu_Exonerado Id_RepFiscal Fec_Reportada Usu_Reportada 
          NomUsu_Reportada ESipla 
      WITH FRAME F_Sipla IN WINDOW wWin.
  ENABLE RECT-209 RECT-210 RECT-211 Btn_informa Id_Sospechosa Id_Exonerada 
         Id_RepFiscal BUTTON-19 
      WITH FRAME F_Sipla IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Sipla}
  DISPLAY S_Informativo 
      WITH FRAME F_Informativo IN WINDOW wWin.
  ENABLE S_Informativo BUTTON-16 BUTTON-18 
      WITH FRAME F_Informativo IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Informativo}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_TTaquilla wWin 
PROCEDURE Grabar_TTaquilla :
IF SUBSTRING(Taquilla.Cuenta,1,4) EQ "1904" OR SUBSTRING(Taquilla.Cuenta,1,4) EQ "2705" THEN
   RETURN.
CREATE TTaquilla.
   BUFFER-COPY Taquilla TO TTaquilla NO-ERROR.
   IF ERROR-STATUS:ERROR THEN MESSAGE "Error al Leer la Taquilla".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Consulta wWin 
PROCEDURE Inicializar_Consulta :
FOR EACH Tot: DELETE Tot. END.
 FOR EACH Tra: DELETE Tra. END.
 FOR EACH Det: DELETE Det. END.
 ASSIGN T_Transacciones = 0 T_Consignaciones = 0 T_Retiros = 0 TotTra = 0 TotCon = 0 TotRet = 0.
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
  ASSIGN FIni = TODAY
         FFin = TODAY.
  DO WITH FRAME F_Filtros:
     ASSIGN V_FinDia = 999999999999.
  END.
  RUN SUPER.
  APPLY "entry" TO FIni IN FRAME F_Consulta.
  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}
 ASSIGN FRAME F_Consulta T_ImpCon T_ImpTra T_ImpDet F_Transacciones F_Detalle
              T_Transacciones T_Consignaciones T_Retiros.
 W_Reporte   = "REPORTE   : CONSULTA DE TRANSACCIONES - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
 W_EncColumna = "Nro.Total de Transacciones: " + STRING(T_Transacciones,">>,>>>,>>>,>>9") + " - " +
                "Total de Consignaciones: " + STRING(T_Consignaciones,">>>,>>>,>>>,>>9") + " - " + 
                "Total de Retiros: " + STRING(T_Retiros,">>>,>>>,>>>,>>9").
 VIEW FRAME F-Encabezado.
 VIEW FRAME f-ftr.
 IF T_ImpCon THEN DO:
    DISPLAY F_Eleccion:SCREEN-VALUE FORMAT "X(50)" WITH FRAME FTTotal NO-BOX USE-TEXT NO-LABELS.
    DISPLAY "Codigo         Nombre                         Porcentaje  Nro.Transacciones  Total Consignaciones     Total Retiros" AT 1 
    WITH FRAME FEncTot WIDTH 132 NO-BOX USE-TEXT.
    FOR EACH Tot BREAK BY INTEGER(Tot.TCodigo):
        DISPLAY 
        Tot.TCodigo AT 1 FORMAT "X(14)"
        Tot.TNombre AT 16 FORMAT "X(30)"             
        Tot.TPorcen AT 50
        Tot.TNroTra AT 62 FORMAT ">>,>>>,>>>,>>9"       
        Tot.TValCon AT 81 FORMAT ">>,>>>,>>>,>>>,>>9"    
        Tot.TValRet AT 106 FORMAT ">>,>>>,>>>,>>>,>>9"
        WITH DOWN FRAME F_Totales WIDTH 132 NO-BOX USE-TEXT NO-LABELS.    
    END.
 END.
 IF T_ImpTra THEN DO:
    DISPLAY SKIP(1) F_Transacciones:SCREEN-VALUE FORMAT "X(50)" WITH FRAME FTTra NO-BOX USE-TEXT NO-LABELS.
    DISPLAY "Codigo         Nombre                                     Nro.Transacciones  Total Consignaciones     Total Retiros" AT 1 
    WITH FRAME FEncTra WIDTH 132 NO-BOX USE-TEXT.
    FOR EACH Tra BREAK BY INTEGER(Tra.TCodTra):
        DISPLAY 
         Tra.TCodTra AT 1 FORMAT "9999999999"            
         Tra.TNomTra AT 16 FORMAT "X(35)"           
         Tra.TNroTra AT 62 FORMAT ">>,>>>,>>>,>>9"      
         Tra.TValCon AT 81 FORMAT ">>,>>>,>>>,>>>,>>9"  
         Tra.TValRet AT 106 FORMAT ">>,>>>,>>>,>>>,>>9"
        WITH DOWN FRAME F_Transa WIDTH 132 NO-BOX USE-TEXT NO-LABELS.    
    END.
 END.
 IF T_ImpDet THEN DO:
    DISPLAY SKIP(1) F_Detalle:SCREEN-VALUE FORMAT "X(50)" WITH FRAME FTDet NO-BOX USE-TEXT NO-LABELS.
                     /* 1         2         3         4         5         6         7         8         9         1         2
             12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
    DISPLAY "Age Des Nit            Fecha      Num.Doc       Num.Tra    Com  Valor Efectivo    Valor Cheque    Hora      Usu     Estacion" AT 1 
    WITH FRAME FEncDet WIDTH 132 NO-BOX USE-TEXT.
    FOR EACH Det BREAK BY INTEGER(Det.TAgeDoc):
        DISPLAY 
          Det.TAgeDoc AT 1   FORMAT "999" 
          Det.TAgeOri AT 5   FORMAT "999" 
          Det.TNitDoc AT 9   FORMAT "X(14)"
          Det.TFecDoc AT 24  
          Det.TNumDoc AT 36  FORMAT "X(10)"
          Det.TNumTra AT 49  FORMAT "99999999"
          Det.TCodCom AT 60  FORMAT "99"
          Det.TValEfe AT 65  FORMAT ">>,>>>,>>>,>>9"      
          Det.TValChe AT 81  FORMAT ">>,>>>,>>>,>>9"      
          Det.THorDoc AT 99 FORMAT "999999"   
          Det.TCodUsu AT 112 FORMAT "X(4)"
          Det.TEstUsu AT 120 FORMAT "X(10)"
        WITH DOWN FRAME F_Detal2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS NO-UNDERLINE.    
    END.
 END.
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Temporal_Taquilla wWin 
PROCEDURE Temporal_Taquilla :
CASE INTEGER(SUBSTRING(Cmb_Filtro,1,2)):
  WHEN 1 THEN DO:
    FOR EACH Taquilla WHERE Taquilla.Fec_Transaccion GE Fini AND Taquilla.Fec_Transaccion  LE FFin AND
                           (Taquilla.Val_Efectivo + Taquilla.Val_Cheque) GE V_IniDia AND 
                           (Taquilla.Val_Efectivo + Taquilla.Val_Cheque) LE V_FinDia NO-LOCK:
                           RUN Grabar_TTaquilla. END.
  END.
  WHEN 2 THEN DO:
    FOR EACH Taquilla WHERE Taquilla.Fec_Transaccion GE Fini AND Taquilla.Fec_Transaccion  LE FFin AND
                           (Taquilla.Val_Efectivo + Taquilla.Val_Cheque) GE V_IniDia AND 
                           (Taquilla.Val_Efectivo + Taquilla.Val_Cheque) LE V_FinDia AND
                           (Taquilla.Id_NUD EQ TRUE OR Taquilla.Id_NUM EQ TRUE) AND
                            Taquilla.Id_Sospechosa EQ NO AND Taquilla.Id_RepFiscal EQ NO AND
                            Taquilla.Id_Exonerada EQ NO
                           NO-LOCK:
                           RUN Grabar_TTaquilla. END.
  END.
  WHEN 3 THEN DO:
    FOR EACH Taquilla WHERE Taquilla.Fec_Transaccion GE Fini AND Taquilla.Fec_Transaccion  LE FFin AND
                           (Taquilla.Val_Efectivo + Taquilla.Val_Cheque) GE V_IniDia AND 
                           (Taquilla.Val_Efectivo + Taquilla.Val_Cheque) LE V_FinDia AND
                           (Taquilla.Id_NUD EQ TRUE OR Taquilla.Id_NUM EQ TRUE) AND
                            Taquilla.Id_Sospechosa EQ YES AND Taquilla.Id_RepFiscal EQ NO AND
                            Taquilla.Id_Exonerada EQ NO
                           NO-LOCK:
                           RUN Grabar_TTaquilla. END.
  END.
  WHEN 4 THEN DO:
    FOR EACH Taquilla WHERE Taquilla.Fec_Transaccion GE Fini AND Taquilla.Fec_Transaccion  LE FFin AND
                           (Taquilla.Val_Efectivo + Taquilla.Val_Cheque) GE V_IniDia AND 
                           (Taquilla.Val_Efectivo + Taquilla.Val_Cheque) LE V_FinDia AND
                           (Taquilla.Id_NUD EQ TRUE OR Taquilla.Id_NUM EQ TRUE) AND
                            Taquilla.Id_Sospechosa EQ YES AND Taquilla.Id_RepFiscal EQ YES AND
                            Taquilla.Id_Exonerada EQ NO
                           NO-LOCK:
                           RUN Grabar_TTaquilla. END.
  END.
  WHEN 5 THEN DO:
    FOR EACH Taquilla WHERE Taquilla.Fec_Transaccion GE Fini AND Taquilla.Fec_Transaccion  LE FFin AND
                           (Taquilla.Val_Efectivo + Taquilla.Val_Cheque) GE V_IniDia AND 
                           (Taquilla.Val_Efectivo + Taquilla.Val_Cheque) LE V_FinDia AND
                           (Taquilla.Id_NUD EQ TRUE OR Taquilla.Id_NUM EQ TRUE) AND
                            Taquilla.Id_Sospechosa EQ NO AND Taquilla.Id_RepFiscal EQ NO AND
                            Taquilla.Id_Exonerada EQ YES
                           NO-LOCK:
                           RUN Grabar_TTaquilla. END.
  END.
END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Transacciones wWin 
PROCEDURE Transacciones :
DO WITH FRAME F_Consulta:
ASSIGN TotTra = 0 TotRet = 0 TotCon = 0.
IF Brw_Consolidado:NUM-ENTRIES GT 0 THEN DO:
  CASE R_Eleccion:
    WHEN 1 THEN DO:
       F_Transacciones:SCREEN-VALUE = "Transacciones de la " + Tot.TNombre.
       FOR EACH TTaquilla WHERE TTaquilla.Agencia EQ INTEGER(Tot.TCodigo) BREAK BY TTaquilla.Cod_Operacion:
          IF FIRST-OF(TTaquilla.Cod_Operacion) THEN ASSIGN TotTra = 0 TotCon = 0 TotRet = 0.
          TotTra = TotTra + 1.
          IF SUBSTRING(STRING(TTaquilla.Cod_Operacion,"999999999"),5,2) EQ "01" THEN
             TotCon = TotCon + TTaquilla.Val_Efectivo + TTaquilla.Val_Cheque.
          ELSE
             TotRet = TotRet + TTaquilla.Val_Efectivo + TTaquilla.Val_Cheque.
          IF LAST-OF(TTaquilla.Cod_Operacion) THEN DO:
             FIND Operacion WHERE Operacion.Cod_Operacion EQ TTaquilla.Cod_Operacion NO-LOCK NO-ERROR.
             IF AVAILABLE Operacion THEN RUN Crear_Transaccion.
          END.
       END.
    END.
    WHEN 2 THEN DO:
       F_Transacciones:SCREEN-VALUE = "Transacciones del Segmento " + Tot.TNombre.
       FOR EACH TTaquilla WHERE TTaquilla.Cod_Segmento EQ INTEGER(Tot.TCodigo) BREAK BY TTaquilla.Cod_Operacion:
          IF FIRST-OF(TTaquilla.Cod_Operacion) THEN ASSIGN TotTra = 0 TotCon = 0 TotRet = 0.
          TotTra = TotTra + 1.
          IF SUBSTRING(STRING(TTaquilla.Cod_Operacion,"999999999"),5,2) EQ "01" THEN
             TotCon = TotCon + TTaquilla.Val_Efectivo + TTaquilla.Val_Cheque.
          ELSE
             TotRet = TotRet + TTaquilla.Val_Efectivo + TTaquilla.Val_Cheque.
          IF LAST-OF(TTaquilla.Cod_Operacion) THEN DO:
             FIND Operacion WHERE Operacion.Cod_Operacion EQ TTaquilla.Cod_Operacion NO-LOCK NO-ERROR.
             IF AVAILABLE Operacion THEN RUN Crear_Transaccion.
          END.
       END.
    END.
    WHEN 3 THEN DO:
       F_Transacciones:SCREEN-VALUE = "Transacciones del Cliente " + Tot.TNombre.
       FOR EACH TTaquilla WHERE TTaquilla.Nit EQ Tot.TCodigo BREAK BY TTaquilla.Cod_Operacion:
          IF FIRST-OF(TTaquilla.Cod_Operacion) THEN ASSIGN TotTra = 0 TotCon = 0 TotRet = 0.
          TotTra = TotTra + 1.
          IF SUBSTRING(STRING(TTaquilla.Cod_Operacion,"999999999"),5,2) EQ "01" THEN
             TotCon = TotCon + TTaquilla.Val_Efectivo + TTaquilla.Val_Cheque.
          ELSE
             TotRet = TotRet + TTaquilla.Val_Efectivo + TTaquilla.Val_Cheque.
          IF LAST-OF(TTaquilla.Cod_Operacion) THEN DO:
             FIND Operacion WHERE Operacion.Cod_Operacion EQ TTaquilla.Cod_Operacion NO-LOCK NO-ERROR.
             IF AVAILABLE Operacion THEN RUN Crear_Transaccion.
          END.
       END.
    END.
    WHEN 4 THEN DO:
       F_Transacciones:SCREEN-VALUE = "Transacciones del Usuario " + Tot.TNombre.
       FOR EACH TTaquilla WHERE TTaquilla.Usuario EQ Tot.TCodigo BREAK BY TTaquilla.Cod_Operacion:
          IF FIRST-OF(TTaquilla.Cod_Operacion) THEN ASSIGN TotTra = 0 TotCon = 0 TotRet = 0.
          TotTra = TotTra + 1.
          IF SUBSTRING(STRING(TTaquilla.Cod_Operacion,"999999999"),5,2) EQ "01" THEN
             TotCon = TotCon + TTaquilla.Val_Efectivo + TTaquilla.Val_Cheque.
          ELSE
             TotRet = TotRet + TTaquilla.Val_Efectivo + TTaquilla.Val_Cheque.
          IF LAST-OF(TTaquilla.Cod_Operacion) THEN DO:
             FIND Operacion WHERE Operacion.Cod_Operacion EQ TTaquilla.Cod_Operacion NO-LOCK NO-ERROR.
             IF AVAILABLE Operacion THEN RUN Crear_Transaccion.
          END.
       END.
    END.
  END CASE.
  OPEN QUERY Brw_Transacciones FOR EACH Tra NO-LOCK INDEXED-REPOSITION.
  FOR EACH Det: DELETE Det. END.
  RUN Detalle.
  OPEN QUERY Brw_Detalle FOR EACH Det NO-LOCK INDEXED-REPOSITION.
END.
ELSE DO:
  FOR EACH Tra: DELETE Tra. END.
  OPEN QUERY Brw_Transacciones FOR EACH Tra NO-LOCK INDEXED-REPOSITION.
  FOR EACH Det: DELETE Det. END.
  OPEN QUERY Brw_Detalle FOR EACH Det NO-LOCK INDEXED-REPOSITION.
END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Validar_Instancias wWin 
PROCEDURE Validar_Instancias :
DEFINE OUTPUT PARAMETER Wok AS LOGICAL.
 DEFINE OUTPUT PARAMETER WMn AS CHARACTER FORMAT "X(50)".
 DO WITH FRAME F_Sipla:
   ASSIGN Id_Sospechosa:SCREEN-VALUE = "NO"
          Fec_Sospechosa:SCREEN-VALUE = ""
          Usu_Sospechosa:SCREEN-VALUE = ""
          NomUsu_Sospechosa:SCREEN-VALUE = ""
          NomUsu_Sospechosa:SCREEN-VALUE = ""
          Id_Exonerada:SCREEN-VALUE = "NO"
          Fec_Exonerado:SCREEN-VALUE = ""
          Usu_Exonerado:SCREEN-VALUE = ""
          NomUsu_Exonerado:SCREEN-VALUE = ""
          Id_RepFiscal:SCREEN-VALUE = "NO"
          Fec_Reportada:SCREEN-VALUE = ""
          NomUsu_Reportada:SCREEN-VALUE = ""
          id_Exonerada:FGCOL = 15
          id_RepFiscal:FGCOL = 15
          id_Sospechosa:FGCOL = 15.
 END.
 CASE SUBSTRING(Cmb_Filtro,1,2):
   WHEN "02" THEN DO: 
      DISABLE Id_RepFiscal WITH FRAME F_Sipla.
      ENABLE Id_Exonerada Id_Sospechosa WITH FRAME F_Sipla.
      ASSIGN Id_Sospechosa:FGCOL = 14
             Id_Exonerada:FGCOL = 14.
      FIND FIRST Cfg_Instancias WHERE Cfg_Instancias.Tipo_Instancia = 6 AND 
           Cfg_Instancias.Instancia EQ 860 AND
           Cfg_Instancias.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
      IF AVAILABLE Cfg_Instancias THEN ASSIGN WOk = YES.
      ELSE ASSIGN WOk = NO WMn = "No puede Gestionar Operaciones No Usuales".
   END.
   WHEN "03" THEN DO:
      ENABLE Id_RepFiscal WITH FRAME F_Sipla.
      DISABLE Id_Exonerada Id_Sospechosa WITH FRAME F_Sipla.
      ASSIGN Id_Exonerada:SCREEN-VALUE = STRING(Det.TId_Exo)
             Id_Sospechosa:SCREEN-VALUE = STRING(Det.TId_Sos)
             Usu_Sospechosa:SCREEN-VALUE = Det.TUs_Sos
             Usu_Exonerado:SCREEN-VALUE = Det.TUs_Exo
             Fec_Sospechosa:SCREEN-VALUE = STRING(Det.TFc_Sos)
             Fec_Exonerado:SCREEN-VALUE = STRING(Det.TFc_Exo)
             Id_RepFiscal:FGCOL = 14.
      IF Det.TId_Exo THEN DO:
         FIND Usuarios WHERE Usuarios.Usuario EQ Det.TUs_Exo NO-LOCK NO-ERROR.
         IF AVAILABLE Usuarios THEN
            NomUsu_Exonerado:SCREEN-VALUE = Usuarios.Nombre.
      END.
      IF Det.TId_Sos THEN DO:
         FIND Usuarios WHERE Usuarios.Usuario EQ Det.TUs_Sos NO-LOCK NO-ERROR.
         IF AVAILABLE Usuarios THEN
            NomUsu_Sospechosa:SCREEN-VALUE = Usuarios.Nombre.
      END.
      FIND FIRST Cfg_Instancias WHERE Cfg_Instancias.Tipo_Instancia = 6 AND 
           Cfg_Instancias.Instancia EQ 870 AND
           Cfg_Instancias.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
      IF AVAILABLE Cfg_Instancias THEN ASSIGN WOk = YES.
      ELSE ASSIGN WOk = NO WMn = "Sin permiso - Operaciones Sospechosas".
   END.
   OTHERWISE
      ASSIGN WOk = NO WMn = "Ya gestionadas".
 END CASE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

