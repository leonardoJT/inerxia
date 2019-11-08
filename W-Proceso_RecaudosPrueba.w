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
{INCLUIDO\VARIABLE.I "SHARED"}
  DEFINE VAR W_Re AS INTEGER.
  DEFINE VARIABLE W_Error       AS LOGICAL.
  DEFINE VARIABLE W_Autorizo  LIKE Usuarios.Usuario.
  DEFINE VARIABLE choice AS LOGICAL.
  DEFINE VARIABLE W_Grupo LIKE Grupos.Grupo.
  DEFINE VAR W_NvaAdm AS LOGICAL.
  DEFINE VAR W_NvaHV AS LOGICAL.
  DEFINE VAR W_NvoCD AS LOGICAL.
  DEFINE VAR W_Pdt AS CHARACTER FORMAT "X(80)".
  DEFINE VAR Puntero AS ROWID.
  DEFINE VAR PuntGar AS ROWID.
  DEFINE VAR Longitud AS DECIMAL.
  DEFINE VAR W_Ultima  LIKE Instancias.Instancia.
  DEFINE VAR W_Primera LIKE Instancias.Instancia.
  DEFINE VAR W_SucAgen LIKE Usuarios.Id_OpeOfi.
  DEFINE VAR Id_Agregar AS CHARACTER FORMAT "X(2)".
  DEFINE VAR W_Tippdt LIKE Creditos.Tip_Credito.
  DEFINE VAR W_TipoInforme AS CHARACTER FORMAT "X(10)".
  DEFINE VAR T_Operacion LIKE Operacion.Cod_Operacion.
  DEFINE VAR T_Deducible LIKE Operacion.Cod_Deducible.
  DEFINE VAR W_ProFor    LIKE Formatos.Nom_Proceso.
  DEFINE VAR W_CodChe    LIKE Cuentas.Cod_Formato.
  DEFINE VAR W_VigIns    LIKE Instancias.TMI.
  DEFINE VAR W_NumCbt    LIKE Comprobantes.Secuencia.
   

DEFINE VAR W_Age LIKE Creditos.Agencia.
DEFINE VAR W_Pro LIKE Creditos.Cod_Credito.
DEFINE VAR W_NitW LIKE Creditos.Nit.
DEFINE VAR W_Cue LIKE Creditos.Num_Credito.   
   
  DEFINE VAR W_CtaCorCre LIKE Cuentas.Cuenta.
  DEFINE VAR W_CtaCorAho LIKE Cuentas.Cuenta.
  DEFINE VAR W_CtaBanco  LIKE Cuentas.Cuenta.
  DEFINE VAR W_Des       AS LOGICAL.
  DEFINE VAR W_MontoCre LIKE Creditos.Sdo_Capital.
  DEFINE VAR W_MontoDeb LIKE Creditos.Sdo_Capital.
  DEFINE VAR W_Cbte     LIKE Comprobantes.Comprobante.
 
/*para buscar un cliente*/
  DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
  DEFINE VARIABLE W_Nuevo    AS LOGICAL.
  
  DEFINE VAR W_Contenido AS CHARACTER FORMAT "X(400)".
  
/*para buscar una cuenta de ahorro*/
  DEFINE VARIABLE A_Nit LIKE Ahorros.Nit.
  DEFINE VARIABLE A_Age LIKE Ahorros.Agencia.
  DEFINE VARIABLE A_Pro LIKE Ahorros.Cod_Ahorro.
  DEFINE VARIABLE A_NitW LIKE Ahorros.Nit.
  DEFINE VARIABLE A_Cue LIKE Ahorros.Cue_Ahorros.
  
  DEFINE VARIABLE i AS INTEGER.
  DEFINE VARIABLE W_Ok AS LOGICAL.
  DEFINE VARIABLE W_TipoProducto LIKE Pro_Creditos.Tip_Credito.
  DEFINE VARIABLE Dias AS DECIMAL.
      
  DEFINE TEMP-TABLE Consulta
      FIELD Num_Credito   LIKE Creditos.Num_Credito
      FIELD Num_Solicitud LIKE Creditos.Num_Solicitud
      FIELD pagare        LIKE Creditos.pagare
      FIELD Cuota         LIKE Creditos.Cuota 
      FIELD Nit           LIKE Creditos.Nit
      FIELD PagaX         AS CHARACTER FORMAT "X"
      FIELD PdoP          AS CHARACTER FORMAT "X"
      FIELD AgeCredito    LIKE Agencias.Agencia
      FIELD CodProducto   LIKE Creditos.Cod_Credito
      FIELD TipProducto   LIKE Creditos.Tip_Credito
      FIELD NomProducto   AS CHARACTER FORMAT "X(40)"
      FIELD Fec_Ingreso   LIKE Mov_Instancias.Fec_Ingreso
      FIELD Hor_Ingreso   AS CHARACTER FORMAT "X(15)"
      FIELD Sdo_Capital   LIKE Creditos.Sdo_Capital
      FIELD INT_Mora      LIKE Creditos.Sdo_Capital
      FIELD INT_Corrientes    LIKE Creditos.Sdo_Capital
      FIELD INT_Anticipados   LIKE Creditos.Sdo_Capital
      FIELD Costas            LIKE Creditos.Costas
      FIELD Honorarios        LIKE Creditos.Honorarios
      FIELD Polizas           LIKE Creditos.Polizas.
      

   DEFINE VAR W_Consulta   AS   LOGICAL.
   DEFINE VAR W_TotCuoEsp  LIKE Pro_Especiales.Ran_FinCuota.
   DEFINE VAR W_NomTer     LIKE  Terceros.Nombre.
   DEFINE VAR W_Tercero    LIKE  Tercero.Nit.
   DEFINE VAR W_NvaAse     AS LOGICAL INITIAL FALSE.
   DEFINE VAR W_Rpta1      AS LOGICAL INITIAL FALSE.
   DEFINE VAR W_NomLin     AS    CHARACTER FORMAT "X(15)".
   DEFINE VAR W_P          AS    INTEGER.
   DEFINE VAR W_ConDed     AS    INTEGER INITIAL 1.
   DEFINE VAR W_Ind        AS    INTEGER INITIAL 0.  
   DEFINE VAR W_WidSel     AS    WIDGET-HANDLE.
   DEFINE VAR W_Cerrado    AS    INTEGER INITIAL 0.
   DEFINE VAR W_VlrAux     AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_PerDed     AS    INTEGER INITIAL 1.
   DEFINE VAR W_Codfor     LIKE  Formatos.Cod_Formato INITIAL 0.
   DEFINE VAR W_Interplazo AS    DECIMAL FORMAT "->>>>>>>>>>>9" INITIAL 0.   
   DEFINE VAR W_Destino    LIKE  Solicitud.Destino.
   DEFINE VAR W_TasEfe     AS    DECIMAL FORMAT ">>9.999999".
   DEFINE VAR W_TasaCont   AS    DECIMAL FORMAT ">>9.999999".
   DEFINE VAR W_Liquidar   AS    LOGICAL INITIAL FALSE.
   DEFINE VAR W_Existe     AS    LOGICAL INITIAL FALSE.
   DEFINE VAR W_NroPer     AS    INTEGER INITIAL 1.
   DEFINE VAR W_MonMul     AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_Razon      AS    DECIMAL FORMAT "->>>>>>>>>>>9.999" INITIAL 0.
   DEFINE VAR P_Band       AS    LOGICAL INITIAL FALSE.
   DEFINE VAR W_TasNom     AS    DECIMAL FORMAT "->>>>>>>>>>>9.999" INITIAL 0.
   DEFINE VAR W_ErrIndica  AS    LOGICAL.
   DEFINE VAR W_TotPorDed  AS    DECIMAL FORMAT ">>9.9999" INITIAL 0.
   DEFINE VAR Suma         AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_Suma       AS    DECIMAL FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEFINE VAR W_PlaPer     AS    INTEGER INITIAL 0.   
   DEFINE VAR W_PlazoDias  AS    INTEGER INITIAL 0.   
   DEFINE VAR W_DesAse     LIKE  Varios.Codigo.
   DEFINE VAR W_MonMin     LIKE  Pro_Creditos.Val_Montominimo.
   DEFINE VAR W_MonMax     LIKE  Pro_Creditos.Val_Montomaximo.
   DEFINE VAR W_PlaMin     LIKE  Pro_Creditos.Pla_Minimo.
   DEFINE VAR W_PlaMax     LIKE  Pro_Creditos.Pla_Maximo.    
   DEFINE VAR W_Sistema    LIKE  Solicitud.Sistema INITIAL 0.      
   DEFINE VAR W_Gracia     LIKE  Solicitud.Per_Gracia.
   DEFINE VAR W_TipInt     LIKE  Solicitud.For_Interes INITIAL 1.
   DEFINE VAR W_Incremento LIKE  Solicitud.Incremento.
   DEFINE VAR W_ClaTas     LIKE  Pro_Creditos.Tip-Tasa.
   DEFINE VAR W_PNegocia   LIKE  Ran_Interes.Pun_Negociables.
   DEFINE VAR W_TasDif     LIKE  Pro_Creditos.Id_TasDiferencial.
   DEFINE VAR W_TotExtras  LIKE  Pro_Creditos.Val_Montomaximo.
   DEFINE VAR W_DiaPer     AS    INTEGER   FORMAT "9999".
   DEFINE VAR W_PerLiqui   AS    INTEGER   FORMAT "99".
   DEFINE VAR W_Per        AS    DECIMAL   FORMAT "999.9999999".
   DEFINE VAR W_LinAho     LIKE Solicitud.Lin_Ahorro.
   DEFINE VAR W_CueAho     LIKE Solicitud.Cue_desembolso.
   DEFINE VAR W_CreMixto   LIKE Solicitud.Monto.

   DEFINE TEMP-TABLE Castigados
       FIELD Nit LIKE Clientes.Nit
       FIELD Num_credito LIKE Creditos.Num_Credito.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Creditos
&Scoped-define BROWSE-NAME Br_Castigados

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Castigados Consulta

/* Definitions for BROWSE Br_Castigados                                 */
&Scoped-define FIELDS-IN-QUERY-Br_Castigados Castigados.Nit Castigados.Num_Credito   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Castigados   
&Scoped-define SELF-NAME Br_Castigados
&Scoped-define QUERY-STRING-Br_Castigados FOR EACH Castigados
&Scoped-define OPEN-QUERY-Br_Castigados OPEN QUERY {&SELF-NAME} FOR EACH Castigados.
&Scoped-define TABLES-IN-QUERY-Br_Castigados Castigados
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Castigados Castigados


/* Definitions for BROWSE Br_Consulta                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Consulta Consulta.AgeCredito Consulta.Num_Credito Consulta.Pagare Consulta.NomProducto Consulta.Fec_Ingreso Consulta.PagaX Consulta.PdoP Consulta.Cuota Consulta.Sdo_Capital Consulta.INT_Mora Consulta.INT_Corrientes Consulta.Costas Consulta.Honorarios Consulta.Polizas   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Consulta   
&Scoped-define SELF-NAME Br_Consulta
&Scoped-define QUERY-STRING-Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Credito INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Consulta OPEN QUERY {&SELF-NAME} FOR EACH Consulta NO-LOCK BY Consulta.Num_Credito INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Consulta Consulta
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Consulta Consulta


/* Definitions for FRAME F_castigados                                   */

/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-Br_Consulta}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 W_NitCliente BUTTON-173 BUTTON-1 ~
Btn_Imprimir BUTTON-2 Cmb_Instancias Btn_Opera BUTTON-4 
&Scoped-Define DISPLAYED-OBJECTS W_NitCliente W_NomCliente Cmb_Instancias 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE VARIABLE WCompromiso AS CHARACTER FORMAT "X(256)":U 
     LABEL "Compromiso" 
     VIEW-AS FILL-IN 
     SIZE 84 BY 1
     BGCOLOR 14  NO-UNDO.

DEFINE BUTTON BUTTON-219 
     LABEL "Ocultar" 
     SIZE 15 BY 1.12.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 8" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Opera 
     LABEL "Realizar Operación" 
     SIZE 20 BY 1.08
     FONT 4.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-173 
     LABEL "Consulta Clientes" 
     SIZE 16 BY .81 TOOLTIP "Click para Consulta de Clientes"
     FONT 5.

DEFINE BUTTON BUTTON-2 
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 4" 
     SIZE 5 BY 1.12.

DEFINE VARIABLE Cmb_Instancias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Operaciones" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 66 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NitCliente AS CHARACTER FORMAT "X(14)":U 
     LABEL "Cliente" 
     VIEW-AS FILL-IN 
     SIZE 18 BY .81 TOOLTIP "Con Enter trae consulta de Clientes, Con Doble-Click de Créditos"
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomCliente AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 52 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 6.46.

DEFINE BUTTON BUTTON-108 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 108" 
     SIZE 7 BY 1.35.

DEFINE BUTTON BUTTON-156 
     LABEL "Ver Información Detallada" 
     SIZE 22 BY 1.12.

DEFINE VARIABLE S_InfoCliente AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 23 BY 2.42
     BGCOLOR 15 FONT 2 NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Castigados FOR 
      Castigados SCROLLING.

DEFINE QUERY Br_Consulta FOR 
      Consulta SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Castigados
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Castigados wWin _FREEFORM
  QUERY Br_Castigados DISPLAY
      Castigados.Nit Castigados.Num_Credito LABEL "Num.Credito"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 36 BY 2.69
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Consulta wWin _FREEFORM
  QUERY Br_Consulta NO-LOCK DISPLAY
      Consulta.AgeCredito COLUMN-LABEL "Age"
    Consulta.Num_Credito FORMAT "999999999":U COLUMN-LABEL "Num.Credito"
    Consulta.Pagare      FORMAT "X(14)" COLUMN-LABEL "Pagaré"
    Consulta.NomProducto FORMAT "X(22)"
    Consulta.Fec_Ingreso FORMAT "99/99/99":U COLUMN-LABEL "Ult.Pago" WIDTH 10
    Consulta.PagaX       COLUMN-LABEL "PX"  
    Consulta.PdoP        COLUMN-LABEL "PP"
    Consulta.Cuota       COLUMN-LABEL "Cuota" WIDTH 12
    Consulta.Sdo_Capital COLUMN-LABEL "Sdo.Capital" WIDTH 12
    Consulta.INT_Mora   COLUMN-LABEL "Int.x Mora"   WIDTH 12
    Consulta.INT_Corrientes COLUMN-LABEL "I.Corrientes" WIDTH 12
    Consulta.Costas COLUMN-LABEL "Costas" WIDTH 11
    Consulta.Honorarios COLUMN-LABEL "Honorarios" WIDTH 11
    Consulta.Polizas COLUMN-LABEL "Polizas" WIDTH 11
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96 BY 4.04
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .46 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Creditos
     W_NitCliente AT ROW 1.27 COL 9 COLON-ALIGNED
     W_NomCliente AT ROW 1.27 COL 28 COLON-ALIGNED NO-LABEL
     BUTTON-173 AT ROW 1.27 COL 85
     BUTTON-1 AT ROW 1.54 COL 103
     Btn_Imprimir AT ROW 3.15 COL 103
     BUTTON-2 AT ROW 5.31 COL 103
     Cmb_Instancias AT ROW 8 COL 13 COLON-ALIGNED
     Btn_Opera AT ROW 8 COL 81
     BUTTON-4 AT ROW 8 COL 106
     RECT-2 AT ROW 1.27 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 21.15
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Consulta
     Br_Consulta AT ROW 1.27 COL 2
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 2.35
         SIZE 98 BY 5.38
         BGCOLOR 17 FONT 5
         TITLE "Creditos Vigentes".

DEFINE FRAME F_Acuerdos
     WCompromiso AT ROW 1.27 COL 17 COLON-ALIGNED
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 9.35
         SIZE 109 BY 2.42
         BGCOLOR 17 FONT 5
         TITLE "Acuerdo de Pago".

DEFINE FRAME F_InfoCliente
     S_InfoCliente AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-108 AT ROW 3.69 COL 26
     BUTTON-156 AT ROW 3.96 COL 3
    WITH 1 DOWN KEEP-TAB-ORDER 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 67 ROW 2.35
         SIZE 34 BY 5.12
         BGCOLOR 17 FONT 5
         TITLE "Información del Cliente".

DEFINE FRAME F_castigados
     Br_Castigados AT ROW 1.27 COL 2
     BUTTON-219 AT ROW 4.23 COL 23
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 23 ROW 3.42
         SIZE 39 BY 5.65
         BGCOLOR 17 FONT 4
         TITLE "Creditos Castigados".


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
         TITLE              = "Proceso de Cobros de Cartera, Prog.W-Proceso_Recaudos.W"
         HEIGHT             = 21.15
         WIDTH              = 113.72
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
ASSIGN FRAME F_Acuerdos:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_castigados:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_Consulta:FRAME = FRAME F_Creditos:HANDLE
       FRAME F_InfoCliente:FRAME = FRAME F_Creditos:HANDLE.

/* SETTINGS FOR FRAME F_Acuerdos
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Acuerdos:HIDDEN           = TRUE
       FRAME F_Acuerdos:MOVABLE          = TRUE.

/* SETTINGS FOR FILL-IN WCompromiso IN FRAME F_Acuerdos
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_castigados
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Castigados 1 F_castigados */
ASSIGN 
       FRAME F_castigados:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Consulta
                                                                        */
/* BROWSE-TAB Br_Consulta 1 F_Consulta */
/* SETTINGS FOR FRAME F_Creditos
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN W_NomCliente IN FRAME F_Creditos
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_InfoCliente
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_InfoCliente:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Castigados
/* Query rebuild information for BROWSE Br_Castigados
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Castigados.
     _END_FREEFORM
     _Query            is NOT OPENED
*/  /* BROWSE Br_Castigados */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Consulta
/* Query rebuild information for BROWSE Br_Consulta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Consulta NO-LOCK BY Consulta.Num_Credito INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Acuerdos
/* Query rebuild information for FRAME F_Acuerdos
     _Query            is NOT OPENED
*/  /* FRAME F_Acuerdos */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_castigados
/* Query rebuild information for FRAME F_castigados
     _Query            is NOT OPENED
*/  /* FRAME F_castigados */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Consulta
/* Query rebuild information for FRAME F_Consulta
     _Query            is NOT OPENED
*/  /* FRAME F_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Creditos
/* Query rebuild information for FRAME F_Creditos
     _Query            is NOT OPENED
*/  /* FRAME F_Creditos */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Proceso de Cobros de Cartera, Prog.W-Proceso_Recaudos.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Proceso de Cobros de Cartera, Prog.W-Proceso_Recaudos.W */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Consulta
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Br_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Consulta wWin
ON ROW-DISPLAY OF Br_Consulta IN FRAME F_Consulta
DO:
  FIND Pro_Creditos WHERE 
       Pro_Creditos.Tip_Credito EQ Consulta.TipProducto and
       Pro_Creditos.Cod_Credito EQ Consulta.CodProducto NO-LOCK NO-ERROR.
  IF AVAILABLE Pro_Creditos THEN
     Consulta.NomProducto = Pro_Creditos.Nom_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Consulta wWin
ON VALUE-CHANGED OF Br_Consulta IN FRAME F_Consulta
DO:
    FIND LAST Cobros WHERE 
              Cobros.Num_Credito EQ Consulta.Num_Credito AND
              Cobros.Nit         EQ Consulta.Nit AND 
              Cobros.Estado      EQ 1 AND
              YEAR(Cobros.Fec_Compromiso) EQ YEAR(W_Fecha) NO-LOCK NO-ERROR.
    IF AVAILABLE Cobros THEN DO:
       WCompromiso = "Por valor de: " + STRING(Cobros.Val_Compromiso) + " para el credito numero: "
              + STRING(Cobros.Num_Credito) + " a cumplir: " + STRING(Cobros.Fec_Compromiso).
       FRAME F_Acuerdos:VISIBLE = TRUE.
       DISPLAY WCompromiso WITH FRAME F_Acuerdos.
    END.
    ELSE FRAME F_Acuerdos:VISIBLE = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir wWin
ON CHOOSE OF Btn_Imprimir IN FRAME F_Creditos /* Button 8 */
DO:
  DEFINE VAR Listado     AS CHARACTER INITIAL "".
  Listado = W_PathSpl + "Proyeccion.LST".
  {INCLUIDO\Imprimir.I "Listado"}  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Opera
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Opera wWin
ON CHOOSE OF Btn_Opera IN FRAME F_Creditos /* Realizar Operación */
DO:
DO WITH FRAME F_Cre:
  IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE,1,5)) LE 0
  OR NOT AVAIL(Consulta) 
  OR W_NitW LE " " THEN 
     RETURN.
  
  FIND Instancias WHERE Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE,1,5)) NO-LOCK NO-ERROR.
  IF AVAILABLE Instancias THEN DO:
    IF Instancias.Programa NE 0 THEN DO:
       FIND Programas WHERE Programas.Programa EQ Instancias.Programa NO-LOCK NO-ERROR.
       IF AVAILABLE Programas THEN DO:
          /*DISABLE ALL WITH FRAME F_Creditos.*/
          ASSIGN WWin:SENSITIVE = NO.
          MESSAGE 
              Cmb_Instancias:SCREEN-VALUE SKIP
              W_NitCliente:SCREEN-VALUE SKIP
              Consulta.CodProducto SKIP
              Consulta.TipProducto SKIP                       
              Consulta.Num_Credito SKIP                       
              STRING(Instancias.Cod_Operacion,"999999999") SKIP
              1
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          RUN VALUE(Programas.Ejecutable)
              (INPUT Cmb_Instancias:SCREEN-VALUE,
               INPUT W_NitCliente:SCREEN-VALUE,
               INPUT Consulta.CodProducto,
               INPUT Consulta.TipProducto,
               INPUT Consulta.Num_Credito,
               INPUT STRING(Instancias.Cod_Operacion,"999999999"),
               INPUT 1).
           /*ENABLE ALL EXCEPT W_NomCliente WITH FRAME F_Creditos.*/
          ASSIGN WWin:SENSITIVE    = YES.
                 
          WWin:MOVE-TO-TOP().
           
          RUN Creditos_X_Cliente.
          OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Credito INDEXED-REPOSITION.
          
       END.
    END.
    ELSE DO:
      MESSAGE "La instancia no tiene ningún programa asociado" SKIP
              "comunique esta inconsistencia al administrador" VIEW-AS ALERT-BOX.
    END.
  END.
  ELSE MESSAGE "No existe la instancia" VIEW-AS ALERT-BOX.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Creditos /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfoCliente
&Scoped-define SELF-NAME BUTTON-108
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-108 wWin
ON CHOOSE OF BUTTON-108 IN FRAME F_InfoCliente /* Button 108 */
DO:
  ENABLE ALL WITH FRAME F_Creditos.
  HIDE FRAME F_InfoCliente.
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-156
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-156 wWin
ON CHOOSE OF BUTTON-156 IN FRAME F_InfoCliente /* Ver Información Detallada */
DO:
  RUN W-ConsultaGeneral.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME BUTTON-173
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-173 wWin
ON CHOOSE OF BUTTON-173 IN FRAME F_Creditos /* Consulta Clientes */
DO:
  /*RUN p-Buscanit.r (OUTPUT W_NitW).
  IF W_NitW NE " " THEN W_NitCliente:SCREEN-VALUE = W_nitW.*/

    DEFI VAR W_NomCte AS CHAR FORM "X(35)".  
    DEFI VAR W_Age    LIKE Agencias.Agencia.

    ASSIGN WWin:SENSITIVE = FALSE.                                                           

    RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                      OUTPUT W_NitCliente, OUTPUT W_NomCte, OUTPUT W_NomCte, OUTPUT W_Age).       

    ASSIGN W_NitCliente:SCREEN-VALUE = W_NitCliente
           WWin:SENSITIVE             = TRUE.

    WWin:MOVE-TO-TOP().

    APPLY "leave" TO W_NitCliente IN FRAME F_Creditos.    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Creditos /* Salir */
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


&Scoped-define FRAME-NAME F_castigados
&Scoped-define SELF-NAME BUTTON-219
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-219 wWin
ON CHOOSE OF BUTTON-219 IN FRAME F_castigados /* Ocultar */
DO:
  HIDE FRAME F_Castigados.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Creditos
&Scoped-define SELF-NAME Cmb_Instancias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Instancias wWin
ON VALUE-CHANGED OF Cmb_Instancias IN FRAME F_Creditos /* Operaciones */
DO:
  FIND Instancias WHERE Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos,1,3)) NO-LOCK NO-ERROR.
  VIEW FRAME F_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_NitCliente
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCliente wWin
ON LEAVE OF W_NitCliente IN FRAME F_Creditos /* Cliente */
DO:
  ASSIGN W_NitCliente.
  
  FIND Clientes WHERE Clientes.Nit EQ W_NitCliente NO-LOCK NO-ERROR.
  
  IF W_NitCliente EQ "" OR NOT AVAILABLE (Clientes) THEN DO:
     ASSIGN WWin:SENSITIVE = FALSE.                                                           

     RUN C-Clientes.R (INPUT  1,W_Agencia,                                                           
                      OUTPUT W_NitCliente, OUTPUT W_NomCliente, OUTPUT W_NomCliente, OUTPUT W_Age).       

     ASSIGN W_NitCliente:SCREEN-VALUE = W_NitCliente
            WWin:SENSITIVE             = TRUE.

     WWin:MOVE-TO-TOP().     
  END.
  
  FIND Clientes WHERE Clientes.Nit EQ W_NitCliente NO-LOCK NO-ERROR.                                     
  IF AVAILABLE Clientes THEN DO: 
     ASSIGN W_NitW = W_NitCliente                                                                        
            W_NomCliente:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     RUN Creditos_X_Cliente.                                                                                  
     OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Credito INDEXED-REPOSITION.             
  END. 
  ELSE DO:
      MESSAGE "Falta El Cliente."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN NO-APPLY.
  END.
  
  FRAME F_Acuerdos:VISIBLE = FALSE.
  FIND FIRST Consulta NO-ERROR.
  IF AVAILABLE Consulta THEN DO:
      FIND LAST Cobros WHERE 
                Cobros.Num_Credito    EQ Consulta.Num_Credito AND
                Cobros.Nit            EQ W_NitW AND
                Cobros.Estado         EQ 1 AND
                Cobros.Fec_Compromiso GE W_Fecha - 90 NO-LOCK NO-ERROR.
      IF AVAILABLE Cobros THEN DO:
         WCompromiso = "Por valor de: " + STRING(Cobros.Val_Compromiso) + " para el credito numero: "
                + STRING(Cobros.Num_Credito) + " a cumplir: " + STRING(Cobros.Fec_Compromiso).
         FRAME F_Acuerdos:VISIBLE = TRUE.
         DISPLAY WCompromiso WITH FRAME F_Acuerdos.
      END.
      ELSE FRAME F_Acuerdos:VISIBLE = FALSE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_NitCliente wWin
ON MOUSE-SELECT-DBLCLICK OF W_NitCliente IN FRAME F_Creditos /* Cliente */
DO:
    ASSIGN WWin:SENSITIVE = FALSE.
   
    RUN C-Creditos.R (INPUT "", OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_NitW, OUTPUT W_Cue).
    
    ASSIGN WWin:SENSITIVE = TRUE.
    
    SELF:SCREEN-VALUE = W_NitW.
    APPLY "Leave" TO SELF.

    WWin:MOVE-TO-TOP().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Castigados
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Creditos_X_Cliente wWin 
PROCEDURE Creditos_X_Cliente :
FOR EACH Consulta: DELETE Consulta. END.
FOR EACH Castigados: DELETE Castigados. END.
 FOR EACH Creditos WHERE Creditos.Nit EQ Clientes.Nit NO-LOCK:
     IF Creditos.Estado EQ 2 THEN DO:
         CREATE Consulta.
         ASSIGN Consulta.Num_Credito     = Creditos.Num_Credito
                Consulta.Num_Solicitud   = Creditos.Num_Solicitud
                Consulta.Nit             = Creditos.Nit
                Consulta.AgeCredito      = Creditos.Agencia
                Consulta.PagaX           = "C"
                Consulta.PdoP            = "M"
                Consulta.Fec_Ingreso     = Creditos.Fec_UltPago
                Consulta.CodProducto     = Creditos.Cod_Credito
                Consulta.TipProducto     = Creditos.Tip_Credito
                Consulta.Sdo_Capital     = Creditos.Sdo_Capital
                Consulta.INT_Mora        = Creditos.INT_MorCobrar + Creditos.INT_MoraDifCob
                Consulta.INT_Corrientes  = Creditos.INT_Corrientes + Creditos.INT_DifCob - Creditos.INT_Anticipado
                Consulta.Costas          = Creditos.Costas
                Consulta.Honorarios      = Creditos.Honorarios
                Consulta.Polizas         = Creditos.Polizas
                Consulta.pagare          = Creditos.pagare
                Consulta.Cuota           = Creditos.Cuota.

          IF Creditos.Per_pago EQ 1 THEN       
             Consulta.PdoP = "S".              
          ELSE IF Creditos.Per_pago EQ 2 THEN  
             Consulta.PdoP = "D".              
          ELSE IF Creditos.Per_pago EQ 3 THEN  
             Consulta.PdoP = "Q".              

          IF Creditos.For_Pago EQ 2 THEN       
             Consulta.PagaX = "N".             
          ELSE IF Creditos.For_pago EQ 3 THEN  
             Consulta.PagaX = "D".             
     END.
     IF Creditos.Estado EQ 5 THEN DO:
        CREATE Castigados.
        ASSIGN Castigados.Nit = Clientes.Nit
               Castigados.Num_Credito = Creditos.Num_Credito.
     END.
 END.
 FIND FIRST Castigados NO-LOCK NO-ERROR.
 IF AVAILABLE Castigados THEN DO: 
   MESSAGE "El cliente tiene credito castigados" SKIP
           "desea ver el registro de esos creditos?" VIEW-AS ALERT-BOX UPDATE pga AS LOGICAL.
   IF pga THEN DO:
       OPEN QUERY Br_Castigados FOR EACH Castigados.
       VIEW FRAME F_Castigados.
   END.
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
  DISPLAY W_NitCliente W_NomCliente Cmb_Instancias 
      WITH FRAME F_Creditos IN WINDOW wWin.
  ENABLE RECT-2 W_NitCliente BUTTON-173 BUTTON-1 Btn_Imprimir BUTTON-2 
         Cmb_Instancias Btn_Opera BUTTON-4 
      WITH FRAME F_Creditos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Creditos}
  ENABLE Br_Consulta 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  DISPLAY S_InfoCliente 
      WITH FRAME F_InfoCliente IN WINDOW wWin.
  ENABLE S_InfoCliente BUTTON-108 BUTTON-156 
      WITH FRAME F_InfoCliente IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_InfoCliente}
  ENABLE Br_Castigados BUTTON-219 
      WITH FRAME F_castigados IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_castigados}
  DISPLAY WCompromiso 
      WITH FRAME F_Acuerdos IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Acuerdos}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe wWin 
PROCEDURE Informe :
{Incluido\RepEncabezado.i}
/*    DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
    DEFINE VAR T_Plazo   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR T_Dedu    AS CHARACTER FORMAT "X(30)".
    
    ASSIGN W_Cliente = Creditos.Nit:SCREEN-VALUE IN FRAME F_Solicitud + " - " + NomNit:SCREEN-VALUE IN FRAME F_Solicitud.
 
    W_Reporte   = "REPORTE   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "Cliente Solicitante         :   " + W_Cliente.
    
    IF W_Re EQ 1 AND FRAME F_Repro:HIDDEN EQ NO THEN
       W_Reporte   = "REESTRUCTURACION   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    IF W_Re EQ 2 AND FRAME F_Repro:HIDDEN EQ NO THEN
       W_Reporte   = "PRORROGA   : CREDITO - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.

 DO WITH FRAME F_Solicitud:
   T_Plazo = Creditos.Plazo:SCREEN-VALUE  + " (" + SUBSTRING(Cmb_PerPago:SCREEN-VALUE,5,15) + ")".
   DISPLAY 
  /* 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
     "=============================================DATOS GENERALES DE LA SOLICITUD==============================================" AT 1
     "Agencia de Radicación       : " AT 1
     Cmb_Agencias:SCREEN-VALUE        AT 33 FORMAT "X(30)"
     "Número del Crédito          : " AT 65
     STRING(Creditos.Num_Credito:SCREEN-VALUE) AT 98
     "Número de Solicitud         : " AT 1
     STRING(Creditos.Num_Solicitud:SCREEN-VALUE) AT 33
     "Fecha de Aprobación         : " AT 65
     Creditos.Fec_Aprobacion:SCREEN-VALUE  AT 98  FORMAT "X(10)"
     "Producto de Crédito         : " AT 1
     Nom_Producto:SCREEN-VALUE        AT 33 FORMAT "X(30)"
     "Tipo de Producto            : " AT 65
     TRIM(W_Tipo_Credito:SCREEN-VALUE) AT 98 FORMAT "X(30)"
     "Instancia Actual            : " AT 1
     Cmb_Instancias:SCREEN-VALUE IN FRAME F_Creditos AT 33  FORMAT "X(30)"
     "Usuario Actualmente Procesa : " AT 65
     NomUsuario:SCREEN-VALUE IN FRAME F_Creditos AT 98  FORMAT "X(30)" 
     "Forma de Pago de la Cuota   : " AT 1
     W_ForPago:SCREEN-VALUE           AT 33 FORMAT "X(30)"
     "=============================================DETALLE DE VALORES DEL CREDITO==============================================" AT 1
     "Monto a Prestar             : " AT 1
     Creditos.Monto:SCREEN-VALUE     AT 33  FORMAT "X(30)"
     "Tasa Efectiva Anual         : " AT 65
     Creditos.Tasa:SCREEN-VALUE      AT 98  FORMAT "X(30)" SKIP(1)
     WITH FRAME F_Sol WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
   IF W_Re EQ 1 OR W_Re EQ 2 THEN
     DISPLAY "CAMBIOS POR REESTRUCTURACION O PRORROGA" AT 1
     "Sdo Capital Anterior        : " AT 1
     Creditos.Sdo_Capital:SCREEN-VALUE IN FRAME F_Solicitud AT 33  FORMAT "X(30)"
     "Saldo Capital Reestructurado: " AT 65
     W_NvoSdoCap:SCREEN-VALUE IN FRAME F_Repro AT 98
     "Plazo Anterior              : " AT 1
     T_Plazo                          AT 33  FORMAT "X(30)"
     "Nuevo Plazo                 : " AT 65
     W_NvoPlazo:SCREEN-VALUE IN FRAME F_Repro AT 98
     "Cuota Anterior              : " AT 1
     Creditos.Cuota:SCREEN-VALUE IN FRAME F_Solicitud AT 33  FORMAT "X(30)"
     "Nuevo Cuota                 : " AT 65
     W_NvaCuota:SCREEN-VALUE IN FRAME F_Repro AT 98
   WITH FRAME F_Sol2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
   
   IF W_Re EQ 2 THEN
   DISPLAY 
     "=============================================DETALLE DEL ATRASO==============================================" AT 1
     "Valor del Atraso Actual     : " AT 1
     Creditos.Val_Atraso:SCREEN-VALUE      AT 33  FORMAT "X(30)"
     "Dias Atrasados              : " AT 65
     Creditos.Dias_Atraso:SCREEN-VALUE       AT 98  FORMAT "X(30)"
     "Cuotas Atrasadas            : " AT 1
     Creditos.Cuo_Atraso:SCREEN-VALUE      AT 33  FORMAT "X(30)"
   WITH FRAME F_Sol3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
 END.
  
 PAGE.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FRAME F_Acuerdos:VISIBLE = FALSE.

  FOR EACH Instancias WHERE Instancias.Tipo_Instancia EQ 4 AND
                          Instancias.Estado         EQ 1 AND
                          Instancias.Tipo_Producto  EQ 2 NO-LOCK BREAK BY Instancias.Orden:
     IF Instancias.Ultima THEN W_Ultima = Instancias.Instancia.
     IF Instancias.Primera THEN W_Primera = Instancias.Instancia.
     FIND FIRST Cfg_Instancias WHERE
          /* Cfg_Instancias.Agencia EQ W_Agencia AND*/
           Cfg_Instancias.Tipo_Instancia EQ 4  AND
           Cfg_Instancias.Instancia EQ Instancias.Instancia AND
           Cfg_Instancias.Usuario EQ W_Usuario AND
           Cfg_Instancias.Estado  EQ 1 NO-LOCK NO-ERROR.
     IF AVAILABLE Cfg_Instancias THEN DO:
        W_Ok = Cmb_Instancias:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) IN FRAME F_Creditos.
     END.
  END.
  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  ASSIGN W_SucAgen  = Usuarios.Id_OpeOfi
         W_Grupo    = Usuarios.Grupo.
  
                             
  
  RUN SUPER.
  
  Cmb_Instancias:SCREEN-VALUE = Cmb_Instancias:ENTRY(1).
  WWin:TITLE = "Proceso de Recaudos de Crédito - Agencia Actual: " + STRING(W_Agencia).
  
  FRAME F_Acuerdos:VISIBLE = FALSE.
  
  APPLY "ENTRY" TO W_NitCliente.
  
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_InfoCliente wWin 
PROCEDURE Llenar_InfoCliente :
DEFINE VARIABLE gtexto AS CHARACTER FORMAT "x(60)".
   DEFINE VARIABLE TTOTAL  AS DECIMAL FORMAT ">>,>>>,>>>,>>9".
   DEFINE VARIABLE TDISPO  AS DECIMAL FORMAT "->>,>>>,>>>,>>9".
   DO i = 1 TO 500:
      W_Ok = S_InfoCliente:DELETE(1) IN FRAME F_InfoCliente.
   END.
   IF AVAILABLE Clientes THEN DO:
      TTotal = Clientes.Ing_Otros + Clientes.Ing_Financieros + Clientes.Ing_Honorarios + Clientes.Salario.
      IF TTotal GT 0 THEN DO:
         gTexto = "              - INGRESOS -".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Salario GT 0 THEN DO:
         gTexto = "Salario                : " + STRING(Clientes.Salario,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Ing_arriendos GT 0 THEN DO:
         gtexto = "Ingresos Financieros   : " + STRING(Clientes.Ing_Financieros,">>,>>>,>>>,>>9").
         RUN Sinfo (INPUT gtexto).
      END.
      IF Clientes.Ing_Honorarios GT 0 THEN DO:
         gtexto = "Ingresos por Honorarios: " + STRING(Clientes.Ing_Honorarios,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Ing_Otros GT 0 THEN DO:
         gtexto = "Otros Ingresos         : " + STRING(Clientes.Ing_Otros,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF TTotal GT 0 THEN DO:
         gtexto = "Total Ingresos---------: " + STRING(TTotal,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      TDispo = TDispo + TTotal.
      TTotal = 0.
      
      TTotal = Clientes.Gto_Obligacion + Clientes.Gto_Familiar + Clientes.Gto_Arriendo.
      IF TTotal GT 0 THEN DO:
         gTexto = "              - GASTOS -".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Gto_Arriendo GT 0 THEN DO:
         gtexto = "Gastos Arriendo        : " + STRING(Clientes.Gto_Arriendo,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Gto_Familiar GT 0 THEN DO:
         gtexto = "Gastos Familiares      : " + STRING(Clientes.Gto_Familiar,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Gto_Obligacion GT 0 THEN DO:
         gtexto = "Obligaciones           : " + STRING(Clientes.Gto_Obligacion,">>,>>>,>>>,>>9").
         RUN Sinfo (INPUT gtexto).
      END.

      IF TTotal GT 0 THEN DO:
         gtexto = "Total Egresos----------: " + STRING(TTotal,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      TDispo = TDispo - TTotal.
      IF TDispo GT 0 THEN DO:
         gTexto = "Disponible (Ing - Egre): " + STRING(TDispo,"->>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      TTotal = 0.
      TDispo = 0.
      TTotal = Clientes.Act_Vehiculo + Clientes.Act_Inversion + Clientes.Act_Casa.
      IF TTotal GT 0 THEN DO:
         gTexto = "              - ACTIVOS -".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Act_Casa GT 0 THEN DO:
         gTexto = "Valor en Propiedades   : " + STRING(Clientes.Act_casa,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Act_Inversion GT 0 THEN DO:
         gtexto = "Valor Inversiones      : " + STRING(Clientes.Act_inversion,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Act_Vehiculo GT 0 THEN DO:
         gtexto = "Valor Vehiculo         : " + STRING(Clientes.Act_Vehiculo,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      IF TTotal GT 0 THEN DO:
         gtexto = "".
         RUN SInfo (INPUT gtexto).
         gtexto = "Total Egresos----------: " + STRING(TTotal,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
         gtexto = "".
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Sdo_Obligaciones GT 0 THEN DO:
         gTexto = "              - PASIVOS -".
         RUN SInfo (INPUT gtexto).
         gTexto = "Saldo Obligaciones     : " + STRING(Clientes.Sdo_Obligaciones,">>,>>>,>>>,>>9").
         RUN SInfo (INPUT gtexto).
      END.
      gTexto = "----------------------------------------------------------".
      RUN SInfo (INPUT gtexto).
      CASE Clientes.Tipo_Vinculo:
        WHEN 1 THEN DO:
          gTexto = "Tipo de Vinculo        : Asociado".
          RUN SInfo (INPUT gtexto).
        END.
        WHEN 2 THEN DO:
          gTexto = "Tipo de Vinculo        : Cliente No Asociado".
          RUN SInfo (INPUT gtexto).
        END.
        WHEN 3 THEN DO:
          gTexto = "Tipo de Vinculo        : Tercero".
          RUN SInfo (INPUT gtexto).
        END.
        WHEN 3 THEN DO:
          gTexto = "Tipo de Vinculo        : Proveedor".
          RUN SInfo (INPUT gtexto).
        END.
      END CASE.
      
      IF Clientes.Cod_Profesion NE 0 THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 1 AND Varios.Codigo EQ Clientes.Cod_Profesion NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN DO:
           gTexto = "Profesión              : " + STRING(Varios.Descripcion,"X(20)").
           RUN SInfo (INPUT gtexto).
         END.
      END.
      gTexto = "Telefono Residencia    : " + STRING(Clientes.Tel_Residencia,"X(14)").
      RUN SInfo (INPUT gtexto).
      gTexto = "Telefono Comercial     : " + STRING(Clientes.Tel_Comercial,"X(14)").
      RUN SInfo (INPUT gtexto).
      gTexto = "Telefono Celular       : " + STRING(Clientes.Celular,"X(14)").
      RUN SInfo (INPUT gtexto).
      gTexto = "Dirección Residencia   : " + STRING(Clientes.Dir_Residencia,"X(40)").
      RUN SInfo (INPUT gtexto).
      gTexto = "Dirección Comercial    : " + STRING(Clientes.Dir_Comercial,"X(40)").
      RUN SInfo (INPUT gtexto).
      gTexto = "e-mail                 : " + STRING(Clientes.Email,"X(40)").
      RUN SInfo (INPUT gtexto).
      
      gTexto = "".
      RUN SInfo (INPUT gtexto).
      IF Clientes.Cod_Empresa NE 0 THEN DO:
         gTexto = "Código Empresa         : " + STRING(Clientes.Cod_Empresa,"9999").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Fec_IngEmpresa NE ? THEN DO:
         gTexto = "Fec.Ingreso Empresa    : " + STRING(Clientes.Fec_IngEmpresa,"99/99/9999").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Cod_Cargo NE 0 THEN DO:
         FIND Varios WHERE Varios.Tipo EQ 2 AND Varios.Codigo EQ Clientes.Cod_Cargo NO-LOCK NO-ERROR.
         IF AVAILABLE Varios THEN DO:
           gTexto = "Cargo que Desempeña    : " + STRING(Varios.Descripcion,"X(40)").
           RUN SInfo (INPUT gtexto).
         END.
      END.
      CASE Clientes.Tip_Contrato:
        WHEN 1 THEN DO:
           gTexto = "Tipo de Contrato       : Termino Indefinido".
           RUN SInfo (INPUT gtexto).
        END.
        WHEN 2 THEN DO:
           gTexto = "Tipo de Contrato       : Termino Fijo".
           RUN SInfo (INPUT gtexto).
        END.
        WHEN 3 THEN DO:
           gTexto = "Tipo de Contrato       : Labor Contratada".
           RUN SInfo (INPUT gtexto).
        END.
        WHEN 4 THEN DO:
           gTexto = "Tipo de Contrato       : Prestación de Servicios".
           RUN SInfo (INPUT gtexto).
        END.
      END CASE.
      
      gTexto = "".
      RUN SInfo (INPUT gtexto).
      IF Clientes.Per_Acargo NE 0 THEN DO:
         gTexto = "Personas a Cargo       : " + STRING(Clientes.Per_Acargo,"99").
         RUN SInfo (INPUT gtexto).
      END.
      IF Clientes.Num_Hijos NE 0 THEN DO:
         gTexto = "Número de Hijos        : " + STRING(Clientes.Num_Hijos,"99").
         RUN SInfo (INPUT gtexto).
      END.
      gTexto = "Estado Civil           : " + STRING(Clientes.Est_Civil,"X(15)").
      RUN SInfo (INPUT gtexto).
      
   /*Clientes.Calificacion
   Clientes.Cod_Empresa
   Clientes.Cod_Cargo
   Clientes.Cod_Segmento
   Clientes.Cod_Zona
   Clientes.Con_Sospechosas
   Clientes.Est_Civil
   Clientes.Edad
   Clientes.Estrato
   Clientes.Fec_Asociacion
   Clientes.Fec_IngEmpresa
   Clientes.Per_Acargo
   
   Clientes.Tipo_Vivienda
   Clientes.Tip_Contrato*/
   END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
IF W_TipoInforme EQ "Proyeccion" THEN 
   RUN Proyeccion.
ELSE DO:
   RUN Informe.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SInfo wWin 
PROCEDURE SInfo :
DEFINE INPUT PARAMETER texto AS CHARACTER FORMAT "X(60)".
W_Ok = S_InfoCliente:ADD-LAST(texto) IN FRAME F_InfoCliente.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

