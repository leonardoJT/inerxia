&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{Incluido\variable.i "shared"}


/* DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion.                     */
/* DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario INITIAL "193".          */
/* DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia INITIAL 1.              */
/* DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.  */
/* DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl.                         */
/* DEFINE {1} VAR W_Manija        AS HANDLE.                                  */
/* DEFINE {1} VAR W_ManFin        AS HANDLE.                                  */
/* DEFINE {1} VAR W_Nom_Entidad   AS CHARACTER FORMAT "X(60)".                */
/* DEFINE {1} VAR W_Nom_Agencia   AS CHARACTER FORMAT "X(60)".                */



   DEFINE VAR W_PerLiq  LIKE Ahorros.Per_Liquidacion.
   DEFINE VARIABLE W_Ok AS LOGICAL.
   DEFINE VARIABLE TPdt LIKE Pro_Ahorros.Tip_Ahorro.
   DEFINE VARIABLE NuevaCta AS LOGICAL INITIAL NO.
   DEFINE VARIABLE Puntero AS ROWID.
   DEFINE VAR W_Inf        AS CHARACTER FORMAT "X(10)".
   DEFI   VAR WInt         LIKE Pro_Creditos.Val_Montomaximo.
   DEFI   VAR Dias         AS INTEG FORMAT "99999".
   DEFI   VAR NomPer       AS CHAR FORMAT "X(15)".
   DEFI   VAR W_SiNva      AS LOG INIT FALSE.
   DEFI   VAR W_Manual     AS LOG INIT FALSE.
   DEFI   VAR W_DiasLiq    AS DEC FORM "9999".
   DEFINE VARIABLE Tas_Nominal LIKE Solicitud.Tasa.
   DEFINE VAR p_linea  LIKE solicitud.cod_credito.
   
   DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
   DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
   DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
   DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
   DEFI VAR W_Plazo  AS INTEG FORM "999999" INIT 0.
   DEFI VAR W_UsuImp LIKE Usuarios.Usuario.
   
  /*para buscar cuentas destino y cuentas debito automatico*/
  DEFINE VAR W_Age  LIKE Ahorros.Agencia.
  DEFINE VAR W_Pro  LIKE Ahorros.Cod_Ahorro.
  DEFINE VAR W_Nit  LIKE Ahorros.Nit.
  DEFINE VAR W_Cue  LIKE Ahorros.Cue_Ahorros.

   DEFINE VARIABLE X AS INTEGER.
   DEFINE VAR W_DigChe           AS INTEGER FORMAT ">>9".
   DEFINE VAR W_CueAho         LIKE Ahorros.Cue_Ahorros.
   DEFINE VAR W_tasa LIKE Indicadores.Tasa.
   DEFINE VAR W_Puntos LIKE Ran_Intereses.Puntos.

   /*variables para la proyeccion*/
   DEFINE VAR WK_Fecha AS DATE. 
   DEFINE VAR W_porce          LIKE Base_Ret.Porcentaje.
   DEFINE VAR W_DiaPer           AS INTEGER FORMAT "9999".
   DEFINE VAR W_DiaCred          AS INTEGER FORMAT "9999".
   DEFINE VAR W_CodBase        LIKE Liqui_Int.Cod_base. 
   DEFINE VAR W_BasRet         LIKE Pro_Creditos.Val_Montomaximo.
    
   /*vlbles plant*/
    DEFINE VAR W_Per              AS DECIMAL FORMAT "999.9999999".
    DEFINE VAR W_Pla              AS INTEGER FORMAT "99999".  
    DEFI   VAR W_Primero          AS LOG INIT FALSE.

    DEFINE VAR W_TasPer           AS DECIMAL FORMAT ">>9.99".
    DEFINE VAR W_TasaWork         AS DECIMAL FORMAT ">>9.99".  
    DEFINE VAR W_PerLiqui         AS INTEGER FORMAT "99".
    DEFINE VAR W_Rpta             AS LOGICAL.
    DEFINE VAR W-Procedure        AS CHARACTER.
    DEFINE VAR P_TipoAsociado     AS LOGICAL.
    DEFINE VAR W_NomCliWork       AS CHARACTER FORMAT "X(40)".
    DEFINE VAR W_Fec              AS DATE FORMAT "99/99/9999".  
    DEFINE VAR W_Mes              AS INTEGER.
    DEFINE VAR I                  AS INTEGER.  
    DEFINE VAR W_TipExtra         AS CHARACTER FORMAT "X".

    DEFINE VAR W_PunNegociables LIKE Ran_Intereses.Pun_Negociables.
    DEFINE VAR W_Nit1           LIKE Ahorros.Nit.
    DEFINE VAR W_TipProdWork    LIKE Pro_Ahorros.Tip_Ahorro.
    DEFINE VAR W_CodProdWork    LIKE Pro_Ahorros.Cod_Ahorro.
    DEFINE VAR W_NomProdWork    LIKE Pro_Ahorros.Nom_Producto.
    
    
    DEFINE VAR W_Valor          LIKE Ahorros.Monto_Apertura.
    DEFINE VAR W_Cap            LIKE Ahorros.Sdo_Disponible INITIAL 0.
    DEFINE VAR W_Int            LIKE Ahorros.Sdo_Disponible INITIAL 0.
    DEFINE VAR W_IntTot         LIKE Ahorros.Sdo_Disponible INITIAL 0.
    DEFINE VAR W_Tot            LIKE Ahorros.Sdo_Disponible INITIAL 0.
    DEFINE VAR W_TotGen         LIKE Ahorros.Sdo_Disponible INITIAL 0.
    DEFINE VAR W_IntGen         LIKE Ahorros.Sdo_Disponible INITIAL 0.
    DEFINE VAR W_IntRet         LIKE Ahorros.Sdo_Disponible INITIAL 0.
    DEFINE VAR W_Ret            LIKE Ahorros.Sdo_Disponible INITIAL 0.
    
    DEFINE TEMP-TABLE TmpAho
           FIELD T_Fecha   LIKE Ahorros.Fec_Apertura
           FIELD T_Valor   LIKE Ahorros.Cuota
           FIELD T_Capital LIKE Ahorros.Monto_Apertura
           FIELD T_IntGra    AS DECIMAL FORMAT ">>>,>>>,>>9"
           FIELD T_Interes   AS DECIMAL FORMAT ">>>,>>>,>>9"
           FIELD T_Reten     AS DECIMAL FORMAT ">,>>>,>>9"
           FIELD T_IntTot    AS DECIMAL FORMAT ">>>,>>>,>>9".
           
    /*Prescoring 
    Giocam Nov 16/07
    */

    DEFINE VARIABLE viSMLV AS INTEGER     NO-UNDO.
    DEFINE VARIABLE viCodTasa LIKE Pro_Creditos.Cod_Tasa NO-UNDO.
    DEFINE VARIABLE vcCiudad AS INTEGER EXTENT 13  INITIAL  [4, 5, 6, 7, 8, 9, 10, 24, /* Distrito Capital */
                                                            20, 21, /* Medellin */
                                                            13, /* Cali */
                                                            3, /* B/quilla */
                                                            11] /* B/Manga */
                                                                NO-UNDO.
    DEFINE VARIABLE vcTpParamCiu AS CHARACTER   NO-UNDO. /* Asignar al cliente TTParamCiudad.Tipo de acuerdo a la ciudad y personas a Cargo*/
    DEFINE VARIABLE vdePorcenCiu AS DECIMAL     NO-UNDO. /* Asigna el porcentaje correspondiente al parametro de la Ciudad*/
    DEFINE VARIABLE vlOk AS LOGICAL INITIAL FALSE NO-UNDO.
    DEFINE VARIABLE viCnt    AS INTEGER     NO-UNDO.
    DEFINE VARIABLE vcNit LIKE clientes.nit NO-UNDO.

    DEFINE TEMP-TABLE TTParamCiudad
        FIELD   tipo        AS  CHARACTER   COLUMN-LABEL "Tipo" 
        FIELD   ingIni      AS INTEGER      COLUMN-LABEL "Ingre. Ini"   FORMAT ">,>>>,>>>,>>9"
        FIELD   ingFin      AS INTEGER      COLUMN-LABEL "Ingre. Fin"   FORMAT ">,>>>,>>>,>>9"
        FIELD   porcentaje  AS DECIMAL      COLUMN-LABEL "Porcent."     FORMAT ">9.99%".


    DEFINE TEMP-TABLE TTOpcCred
        FIELD   TPlazo      AS  INTEGER
        FIELD   TMOpcA      AS  INTEGER
        FIELD   TMOpcB      AS  INTEGER.

    DEFINE TEMP-TABLE TTGranCiudad
        FIELD ciudad    AS CHARACTER
        FIELD nombre    AS CHARACTER.

    DEFINE NEW SHARED TEMP-TABLE TTAsesoria    
        FIELD   nit         LIKE    clientes.nit    FORMAT "X(14)"
        FIELD   cliente     AS CHARACTER            FORMAT "X(40)"
        FIELD   agencia     LIKE    clientes.Agencia
        FIELD   monto       AS INTEGER
        FIELD   plazo       AS INTEGER
        FIELD   numPers     AS  INTEGER
        FIELD   tasa        AS DECIMAL
        FIELD   porGtoSost  AS DECIMAL       
        FIELD   ingBruto    AS INTEGER
        FIELD   ingOtros    AS INTEGER
        FIELD   ingHonor    AS INTEGER
        FIELD   ingTotal    AS INTEGER
        FIELD   egrNomina   AS INTEGER
        FIELD   cap50       AS INTEGER
        FIELD   capSMLV     AS INTEGER
        FIELD   gtosSost    AS INTEGER
        FIELD   gtosVivi    AS INTEGER
        FIELD   gtosCIFIN   AS INTEGER
        FIELD   ingNeto50   AS INTEGER
        FIELD   egreMes50   AS INTEGER
        FIELD   ctaMax50     AS INTEGER 
        FIELD   CtaValSol50  AS INTEGER 
        FIELD   IngNetoSMLV  AS INTEGER 
        FIELD   egreMesSMLV  AS INTEGER 
        FIELD   ctaMaxSMLV   AS INTEGER 
        
        FIELD   plazo1      AS INTEGER
        FIELD   Monto1A      AS INTEGER
        FIELD   Monto1B      AS INTEGER
        FIELD   plazo2      AS INTEGER
        FIELD   Monto2A      AS INTEGER
        FIELD   Monto2B      AS INTEGER
        FIELD   plazo3      AS INTEGER
        FIELD   Monto3A      AS INTEGER
        FIELD   Monto3B      AS INTEGER
        FIELD   plazo4      AS INTEGER
        FIELD   Monto4A      AS INTEGER
        FIELD   Monto4B      AS INTEGER
        FIELD   plazo5      AS INTEGER
        FIELD   Monto5A      AS INTEGER
        FIELD   Monto5B      AS INTEGER.

/*end vbles plant*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Asesoria
&Scoped-define BROWSE-NAME B_Proyeccion

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TmpAho

/* Definitions for BROWSE B_Proyeccion                                  */
&Scoped-define FIELDS-IN-QUERY-B_Proyeccion T_Fecha T_Valor T_Capital T_Reten T_IntGra T_Interes T_IntTot   
&Scoped-define ENABLED-FIELDS-IN-QUERY-B_Proyeccion   
&Scoped-define SELF-NAME B_Proyeccion
&Scoped-define QUERY-STRING-B_Proyeccion FOR EACH TmpAho NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-B_Proyeccion OPEN QUERY {&SELF-NAME} FOR EACH TmpAho NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-B_Proyeccion TmpAho
&Scoped-define FIRST-TABLE-IN-QUERY-B_Proyeccion TmpAho


/* Definitions for FRAME F_Asesoria                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Asesoria ~
    ~{&OPEN-QUERY-B_Proyeccion}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Asesoria.Val_IngresosMes ~
Asesoria.Val_EgresosMes Asesoria.Num_Asesoria Asesoria.Fec_Asesoria ~
Asesoria.Nit Asesoria.Monto Asesoria.Cuota Asesoria.Plazo ~
Asesoria.For_Liquidacion Asesoria.Tasa Asesoria.Id_Resultado ~
Asesoria.Clase_Producto Asesoria.Fec_Apertura 
&Scoped-define ENABLED-TABLES Asesoria
&Scoped-define FIRST-ENABLED-TABLE Asesoria
&Scoped-Define ENABLED-OBJECTS BUTTON-1 BUTTON-2 BUTTON-3 Rs_PerPago ~
Cmb_PerLiquidacion Btn_Ingresar B_Proyeccion Btn_Cancelar BtnDone BUTTON-95 ~
W_TManual RECT-274 RECT-275 RECT-276 RECT-277 RECT-281 RECT-284 RECT-300 ~
RECT-301 RECT-316 
&Scoped-Define DISPLAYED-FIELDS Asesoria.Val_IngresosMes ~
Asesoria.Val_EgresosMes Asesoria.Num_Asesoria Asesoria.Fec_Asesoria ~
Asesoria.Usuario Asesoria.Nit Asesoria.Monto Asesoria.Cuota Asesoria.Plazo ~
Asesoria.For_Liquidacion Asesoria.Tasa Asesoria.Id_Resultado ~
Asesoria.Clase_Producto Asesoria.Fec_Apertura 
&Scoped-define DISPLAYED-TABLES Asesoria
&Scoped-define FIRST-DISPLAYED-TABLE Asesoria
&Scoped-Define DISPLAYED-OBJECTS W_Telef W_PorEnd W_NomAsesor ~
Cmb_TipoProductos Cmb_Productos W_NomTitular Rs_PerPago Tasa_Nominal ~
Cmb_PerLiquidacion Interes_PorPagar AS_Tasa W_SdoApor W_ReqPtmo W_FaltApor ~
W_TManual W_PromedDD 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-1 Cmb_TipoProductos Cmb_Productos Interes_PorPagar ~
Btn_Salvar AS_Tasa W_TManual 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.62
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Borrar 
     LABEL "&Borrar" 
     SIZE 10 BY 1.65
     FONT 4.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.65
     FONT 4.

DEFINE BUTTON Btn_Deshacer 
     LABEL "&Deshacer" 
     SIZE 10 BY 1.65
     FONT 4.

DEFINE BUTTON Btn_Ingresar 
     LABEL "&Ingresar" 
     SIZE 10 BY 1.62
     FONT 4.

DEFINE BUTTON Btn_Salvar 
     LABEL "&Salvar" 
     SIZE 10 BY 1.62
     FONT 4.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 2" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-3 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE Cmb_PerLiquidacion AS CHARACTER FORMAT "X(256)":U INITIAL "01 - Diaria" 
     LABEL "Liquidación" 
     VIEW-AS COMBO-BOX INNER-LINES 6
     LIST-ITEMS "01 - Diaria","02 - Mensual","03 - Trimestral","04 - Semestral","05 - Anual","06 - Al Vencimiento" 
     DROP-DOWN-LIST
     SIZE 16.43 BY 1
     BGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 63 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE Cmb_TipoProductos AS CHARACTER FORMAT "X(256)":U INITIAL "A la Vista" 
     LABEL "Productos" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "2 - Contractual","3 - A Termino" 
     DROP-DOWN-LIST
     SIZE 17 BY 1
     BGCOLOR 15 FONT 1 NO-UNDO.

DEFINE VARIABLE AS_Tasa AS DECIMAL FORMAT ">9.9999999":U INITIAL 0 
     LABEL "Tasa" 
     VIEW-AS FILL-IN 
     SIZE 16.43 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Interes_PorPagar AS DECIMAL FORMAT ">>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tasa_Nominal AS DECIMAL FORMAT "->9.9999999":U INITIAL 0 
     LABEL "Tasa Nominal Anual" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FaltApor AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Faltante" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomAsesor AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 38.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 37.72 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PorEnd AS DECIMAL FORMAT "->>9.99":U INITIAL 0 
     LABEL "% Endeudamiento" 
     VIEW-AS FILL-IN 
     SIZE 10.72 BY .85
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PromedDD AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .77 TOOLTIP "Promedio Ult.Trimestre Ahorros a la vista"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_ReqPtmo AS DECIMAL FORMAT "->>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Para este Crèdito" 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_SdoApor AS DECIMAL FORMAT "->>>>,>>>,>>9.99":U INITIAL 0 
     LABEL "Saldo Actual" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .77
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Telef AS CHARACTER FORMAT "X(12)":U 
     LABEL "Telèfono" 
     VIEW-AS FILL-IN 
     SIZE 13.86 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TManual AS DECIMAL FORMAT ">9.9999999":U INITIAL 0 
     LABEL "Efectiva Anual" 
     VIEW-AS FILL-IN 
     SIZE 16.43 BY .85
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_PerPago AS INTEGER INITIAL 4 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Sem.", 1,
"Dec.", 2,
"Qnal", 3,
"Mensual", 4
     SIZE 32.14 BY .69 NO-UNDO.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE RECTANGLE RECT-275
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 95 BY 1.88.

DEFINE RECTANGLE RECT-276
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.43 BY 4.12.

DEFINE RECTANGLE RECT-277
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 30.57 BY 4.12.

DEFINE RECTANGLE RECT-281
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 25.86 BY 1.23.

DEFINE RECTANGLE RECT-284
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 94.72 BY 1.35.

DEFINE RECTANGLE RECT-300
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 51.72 BY 3.04.

DEFINE RECTANGLE RECT-301
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12.29 BY 10.62.

DEFINE RECTANGLE RECT-316
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 32.57 BY .81.

DEFINE BUTTON Btn_Salir 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_Salir" 
     SIZE 9.57 BY 1.85.

DEFINE BUTTON BUTTON-167 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 167" 
     SIZE 10 BY 1.85.

DEFINE VARIABLE Cmb_Usuario AS CHARACTER FORMAT "X(40)":U 
     LABEL "Usuarios" 
     VIEW-AS COMBO-BOX INNER-LINES 20
     DROP-DOWN-LIST
     SIZE 35.86 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Hasta" 
     VIEW-AS FILL-IN 
     SIZE 13.14 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Desde" 
     VIEW-AS FILL-IN 
     SIZE 13.29 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Rs_Inf AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Inf.Por Usuario", 1,
"Asesoria", 2,
"PreScoring", 3
     SIZE 18 BY 2.54 NO-UNDO.

DEFINE RECTANGLE RECT-315
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 20.29 BY 3.23.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY B_Proyeccion FOR 
      TmpAho SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE B_Proyeccion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS B_Proyeccion wWin _FREEFORM
  QUERY B_Proyeccion NO-LOCK DISPLAY
      T_Fecha   LABEL "Fecha"
  T_Valor   LABEL "Cuota"
  T_Capital LABEL "Sdo-Capital"
  T_Reten   LABEL "RetFte/Capital"
  T_IntGra  LABEL "Int-PdoGracia"
  T_Interes LABEL "Interès"  
  T_IntTot  LABEL "Int.Acumulado"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 96 BY 6.31
         BGCOLOR 15 FONT 5 ROW-HEIGHT-CHARS .54 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Impres
     Cmb_Usuario AT ROW 2.08 COL 11 COLON-ALIGNED
     W_FecIni AT ROW 3.46 COL 11 COLON-ALIGNED
     W_FecFin AT ROW 3.42 COL 33.57 COLON-ALIGNED
     Rs_Inf AT ROW 5.85 COL 18.43 NO-LABEL
     BUTTON-167 AT ROW 9.08 COL 4.72
     Btn_Salir AT ROW 9.08 COL 40.57
     "Desea Imprimir ?" VIEW-AS TEXT
          SIZE 15.86 BY .62 AT ROW 4.77 COL 19
          BGCOLOR 18 
     RECT-315 AT ROW 5.42 COL 17
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 36.14 ROW 10.04
         SIZE 52 BY 11.19
         BGCOLOR 17 
         TITLE "Frame de Impresión".

DEFINE FRAME F_Asesoria
     W_Telef AT ROW 5.12 COL 81.29 COLON-ALIGNED
     Asesoria.Val_IngresosMes AT ROW 7.12 COL 46.43 COLON-ALIGNED
          LABEL "Ingresos Mes" FORMAT ">>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.57 BY .81
          BGCOLOR 15 
     Asesoria.Val_EgresosMes AT ROW 8.08 COL 46.43 COLON-ALIGNED
          LABEL "Egresos Mes" FORMAT ">>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 14.57 BY .85
          BGCOLOR 15 
     W_PorEnd AT ROW 9.19 COL 50.29 COLON-ALIGNED
     Asesoria.Num_Asesoria AT ROW 1.54 COL 12 COLON-ALIGNED
          LABEL "Asesoria" FORMAT "99999999"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Asesoria.Fec_Asesoria AT ROW 1.54 COL 31 COLON-ALIGNED
          LABEL "Fecha" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 11.57 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Asesoria.Usuario AT ROW 1.54 COL 51 COLON-ALIGNED
          LABEL "Asesor" FORMAT "X(4)"
          VIEW-AS FILL-IN 
          SIZE 5.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     W_NomAsesor AT ROW 1.54 COL 56.57 COLON-ALIGNED NO-LABEL
     BUTTON-1 AT ROW 1.58 COL 101.43
     BUTTON-2 AT ROW 3.19 COL 101.43
     BUTTON-3 AT ROW 4.81 COL 101.43
     Cmb_TipoProductos AT ROW 3.73 COL 14.57 COLON-ALIGNED
     Cmb_Productos AT ROW 3.73 COL 32.57 COLON-ALIGNED NO-LABEL
     Asesoria.Nit AT ROW 5 COL 14.72 COLON-ALIGNED
          LABEL "Id. Asesorado" FORMAT "X(12)"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 15 
     W_NomTitular AT ROW 5.04 COL 33 COLON-ALIGNED NO-LABEL
     Asesoria.Monto AT ROW 5.92 COL 14.72 COLON-ALIGNED
          LABEL "Monto" FORMAT ">>>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 15 
     Asesoria.Cuota AT ROW 6.81 COL 14.72 COLON-ALIGNED
          LABEL "Cuota" FORMAT ">>>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 15 
     Rs_PerPago AT ROW 7.85 COL 1.43 NO-LABEL
     Asesoria.Plazo AT ROW 8.81 COL 15 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 16.43 BY .81
          BGCOLOR 15 
     Asesoria.For_Liquidacion AT ROW 12.85 COL 21.29 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Anticipado", 1,
"Vencido", 2
          SIZE 23.86 BY .73
     Tasa_Nominal AT ROW 6.69 COL 83.72 COLON-ALIGNED
     Asesoria.Tasa AT ROW 7.62 COL 83.72 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Cmb_PerLiquidacion AT ROW 11.38 COL 14.72 COLON-ALIGNED
     Interes_PorPagar AT ROW 8.54 COL 83.72 COLON-ALIGNED NO-LABEL
     Btn_Salvar AT ROW 9.65 COL 101.43
     AS_Tasa AT ROW 9.73 COL 14.72 COLON-ALIGNED
     Btn_Ingresar AT ROW 11.27 COL 101.43
     B_Proyeccion AT ROW 13.96 COL 2
     Btn_Cancelar AT ROW 12.88 COL 101.43
     Btn_Deshacer AT ROW 14.5 COL 101.43
     Btn_Borrar AT ROW 16.12 COL 101.43
     BtnDone AT ROW 17.73 COL 101.43
     Asesoria.Id_Resultado AT ROW 21.5 COL 90.72 COLON-ALIGNED
          VIEW-AS COMBO-BOX 
          LIST-ITEMS "5","4","3","2","1","0" 
          DROP-DOWN-LIST
          SIZE 5.14 BY 1
          BGCOLOR 15 
     BUTTON-95 AT ROW 20.54 COL 106
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 22.04
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON BtnDone.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Asesoria
     Asesoria.Clase_Producto AT ROW 2.96 COL 43.57 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ahorro", 1,
"Crédito", 2,
"PreScoring", 3
          SIZE 31.43 BY .58
     Asesoria.Fec_Apertura AT ROW 9.46 COL 83.57 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 15 
     W_SdoApor AT ROW 11.15 COL 81.86 COLON-ALIGNED
     W_ReqPtmo AT ROW 12.04 COL 81.72 COLON-ALIGNED
     W_FaltApor AT ROW 12.88 COL 81.72 COLON-ALIGNED
     W_TManual AT ROW 10.5 COL 14.72 COLON-ALIGNED
     W_PromedDD AT ROW 12.19 COL 48 COLON-ALIGNED NO-LABEL
     "Información General" VIEW-AS TEXT
          SIZE 18 BY .62 AT ROW 6.19 COL 65.86
          FGCOLOR 7 
     "Información Econòmica" VIEW-AS TEXT
          SIZE 20.29 BY .62 AT ROW 6.19 COL 35.72
          FGCOLOR 7 
     "Prom.Ult-Trim.(A la Vista)" VIEW-AS TEXT
          SIZE 21.29 BY .65 AT ROW 11.54 COL 46.72
          FGCOLOR 7 
     "Informaciòn de Aportes" VIEW-AS TEXT
          SIZE 20.43 BY .85 AT ROW 10.5 COL 63.29
          FGCOLOR 7 
     "Escoja la Clase de Producto de la Asesoria" VIEW-AS TEXT
          SIZE 37.72 BY .62 AT ROW 2.88 COL 5.43
          FGCOLOR 7 
     "Proyecciones" VIEW-AS TEXT
          SIZE 13 BY .62 AT ROW 13.38 COL 2
          FGCOLOR 7 
     " Forma de Liquidación" VIEW-AS TEXT
          SIZE 20 BY .54 AT ROW 12.31 COL 21.29
          FGCOLOR 7 
     RECT-274 AT ROW 1.31 COL 100.43
     RECT-275 AT ROW 3 COL 3.43
     RECT-276 AT ROW 6.42 COL 65.29
     RECT-277 AT ROW 6.42 COL 34.57
     RECT-281 AT ROW 12.54 COL 20
     RECT-284 AT ROW 1.27 COL 3.72
     RECT-300 AT ROW 10.92 COL 46
     RECT-301 AT ROW 9.23 COL 100.14
     RECT-316 AT ROW 2.81 COL 43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 22.04
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON BtnDone.


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
         TITLE              = "Asesoria de Ahorro / Crèdito, Prog.W-Asesoria.W"
         HEIGHT             = 21.12
         WIDTH              = 112.72
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
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
/* SETTINGS FOR FRAME F_Asesoria
   NOT-VISIBLE FRAME-NAME Custom                                        */
/* BROWSE-TAB B_Proyeccion Btn_Ingresar F_Asesoria */
/* SETTINGS FOR FILL-IN AS_Tasa IN FRAME F_Asesoria
   NO-ENABLE 1                                                          */
/* SETTINGS FOR BUTTON Btn_Borrar IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Deshacer IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F_Asesoria
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX Cmb_Productos IN FRAME F_Asesoria
   NO-ENABLE 1                                                          */
/* SETTINGS FOR COMBO-BOX Cmb_TipoProductos IN FRAME F_Asesoria
   NO-ENABLE 1                                                          */
/* SETTINGS FOR FILL-IN Asesoria.Cuota IN FRAME F_Asesoria
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Asesoria.Fec_Asesoria IN FRAME F_Asesoria
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Interes_PorPagar IN FRAME F_Asesoria
   NO-ENABLE 1                                                          */
ASSIGN 
       Interes_PorPagar:HIDDEN IN FRAME F_Asesoria           = TRUE.

/* SETTINGS FOR FILL-IN Asesoria.Monto IN FRAME F_Asesoria
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Asesoria.Nit IN FRAME F_Asesoria
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Asesoria.Num_Asesoria IN FRAME F_Asesoria
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Tasa_Nominal IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Asesoria.Usuario IN FRAME F_Asesoria
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Asesoria.Val_EgresosMes IN FRAME F_Asesoria
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Asesoria.Val_IngresosMes IN FRAME F_Asesoria
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN W_FaltApor IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomAsesor IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_PorEnd IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_PromedDD IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_ReqPtmo IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_SdoApor IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Telef IN FRAME F_Asesoria
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TManual IN FRAME F_Asesoria
   1                                                                    */
/* SETTINGS FOR FRAME F_Impres
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Impres:HIDDEN           = TRUE
       FRAME F_Impres:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE B_Proyeccion
/* Query rebuild information for BROWSE B_Proyeccion
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TmpAho NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE B_Proyeccion */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Asesoria
/* Query rebuild information for FRAME F_Asesoria
     _Query            is NOT OPENED
*/  /* FRAME F_Asesoria */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Impres
/* Query rebuild information for FRAME F_Impres
     _Query            is NOT OPENED
*/  /* FRAME F_Impres */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Asesoria de Ahorro / Crèdito, Prog.W-Asesoria.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Asesoria de Ahorro / Crèdito, Prog.W-Asesoria.W */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F_Asesoria /* Salir */
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
ON CHOOSE OF Btn_Cancelar IN FRAME F_Asesoria /* Cancelar */
DO:
  RUN Inicializar_Variables.
  
  CLOSE QUERY B_Proyeccion.
  FOR EACH TmpAho: DELETE TmpAho. END.
  
  DISABLE {&List-1} WITH FRAME F_Asesoria.
  
  ASSIGN Button-3:SENSITIVE = TRUE
         AS_Tasa:SENSITIVE  = FALSE
         Btn_Ingresar:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Asesoria /* Deshacer */
DO:
  ASSIGN Button-3:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_Asesoria /* Ingresar */
DO:
 DO WITH FRAME F_Asesoria:
    RELEASE Asesoria.       
    
    ASSIGN Button-3:SENSITIVE     = FALSE
           Btn_Ingresar:SENSITIVE = FALSE
           W_SdoApor:SCREEN-VALUE    = "0"
           W_ReqPtmo:SCREEN-VALUE    = "0"
           W_FaltApor:SCREEN-VALUE   = "0"
           W_PromedDD:SCREEN-VALUE   = "0"
           W_SdoApor  = 0
           W_ReqPtmo  = 0
           W_FaltApor = 0
           W_PromedDD = 0
           W_SiNva    = TRUE.
    
    RUN Inicializar_Variables.
    
    CLOSE QUERY B_Proyeccion.
    FOR EACH TmpAho: DELETE TmpAho. END.
    
    ENABLE {&List-1} WITH FRAME F_Asesoria.
    FIND LAST Asesoria WHERE Asesoria.Num_Asesoria GT 0 NO-LOCK NO-ERROR.
    IF AVAILABLE Asesoria THEN
       ASSIGN Asesoria.Num_Asesoria:SCREEN-VALUE IN FRAME F_Asesoria = STRING(Asesoria.Num_Asesoria + 1).
    ELSE
      ASSIGN Asesoria.Num_Asesoria:SCREEN-VALUE = "1".
  
   
   ASSIGN Asesoria.Fec_Asesoria:SCREEN-VALUE IN FRAME F_Asesoria = STRING(TODAY)
          Asesoria.Usuario:SCREEN-VALUE IN FRAME F_Asesoria     = STRING(W_Usuario).
          
   FIND Usuarios WHERE Usuarios.Agencia EQ W_Agencia AND
                      Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
   IF AVAILABLE Usuarios THEN 
      W_NomAsesor:SCREEN-VALUE IN FRAME F_Asesoria = Usuarios.Nombre.
      
   APPLY 'value-changed' TO Asesoria.Clase_Producto.
   APPLY 'value-changed' TO Cmb_Productos.
   APPLY 'entry' TO Cmb_TipoProductos.
   
   AS_Tasa:SENSITIVE = FALSE.
      
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Impres
&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON CHOOSE OF Btn_Salir IN FRAME F_Impres /* Btn_Salir */
DO: 
  ASSIGN FRAME F_Asesoria:SENSITIVE = TRUE.
         FRAME F_Impres:VISIBLE     = FALSE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir wWin
ON MOUSE-SELECT-CLICK OF Btn_Salir IN FRAME F_Impres /* Btn_Salir */
DO:
  APPLY "CHOOSE" TO SELF.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asesoria
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Asesoria /* Salvar */
DO:
   
    grbndo:
    REPEAT TRANSACTION ON ERROR UNDO grbndo, LEAVE grbndo:
        IF NOT can-find(FIRST clientes WHERE clientes.nit = Asesoria.Nit:SCREEN-VALUE)
        THEN DO:
            MESSAGE "No se permite crear una asesoria a una persona o empresa" SKIP
                    "No matriculadas en la Base de datos. Rectifique!" VIEW-AS ALERT-BOX ERROR.
            APPLY "ENTRY" TO Asesoria.Nit.
            RETURN NO-APPLY.
        END.
        /*   IF (Asesoria.Nit:SCREEN-VALUE IN FRAME F_asesoria LE "0" OR W_NomTitular:SCREEN-VALUE IN FRAME F_asesoria = "") THEN DO: */
        /*      MESSAGE "No se permite crear una asesoria a una persona o empresa" SKIP                                               */
        /*                 "No matriculadas en la Base de datos. Rectifique!" VIEW-AS ALERT-BOX ERROR.                                */
        /*      APPLY "ENTRY" TO Asesoria.Nit.                                                                                        */
        /*      RETURN.                                                                                                               */
        /*   END.                                                                                                                     */
        IF Asesoria.Nit:SCREEN-VALUE LE " " OR (DECIMAL(Asesoria.Monto:SCREEN-VALUE) LE 0 AND DECIMAL(Asesoria.Plazo:SCREEN-VALUE) LE 0) 
        THEN DO:
            APPLY "ENTRY" TO Asesoria.Nit.             
            MESSAGE "Nit y Monto/Plazo son obligatorios...Revise por favor" VIEW-AS ALERT-BOX.
            RETURN NO-APPLY.
        END. 
        IF W_SiNva 
        THEN DO:
            Asesoria.Num_Asesoria:SCREEN-VALUE = "1".
            FIND LAST Asesoria WHERE Asesoria.Num_Asesoria GT 0 NO-LOCK NO-ERROR.
            IF AVAILABLE Asesoria 
            THEN ASSIGN Asesoria.Num_Asesoria:SCREEN-VALUE = STRING(Asesoria.Num_Asesoria + 1).
            CREATE Asesoria.
            ASSIGN  Asesoria.Num_Asesoria    = DECIMAL(Asesoria.Num_Asesoria:SCREEN-VALUE)
                    Asesoria.Agencia         = W_Agencia
                    Asesoria.Cod_Producto    = INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3))
                    Asesoria.Nit             = Asesoria.Nit:SCREEN-VALUE
                    Asesoria.Fec_Asesoria    = W_Fecha
                    W_SiNva                  = FALSE.
        END.
        ELSE FIND CURRENT Asesoria NO-ERROR.
        ASSIGN  Asesoria.Agencia         = W_Agencia
                Asesoria.Clase_Producto  = INTEGER(Asesoria.Clase_Producto:SCREEN-VALUE)
                Asesoria.Cod_Producto    = INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3))
                Asesoria.Cuota           = DECIMAL(Asesoria.Cuota:SCREEN-VALUE)
                Asesoria.Fec_Asesoria    = W_Fecha
                Asesoria.For_Liquidacion = INTEGER(Asesoria.FOR_Liquidacion:SCREEN-VALUE)
                Asesoria.Fec_Apertura    = DATE(Asesoria.Fec_Apertura:SCREEN-VALUE)
                Asesoria.Id_Resultado    = INTEGER(Asesoria.Id_Resultado:SCREEN-VALUE)
                Asesoria.Monto           = DECIMAL(Asesoria.Monto:SCREEN-VALUE)
                Asesoria.Per_Liquidacion = INTEGER(SUBSTRING(Cmb_PerLiquidacion:SCREEN-VALUE,1,2))
                Asesoria.Plazo           = INTEGER(Asesoria.Plazo:SCREEN-VALUE)
                Asesoria.Tasa            = DECIMAL(Asesoria.Tasa:SCREEN-VALUE)
                Asesoria.Usuario         = W_Usuario
                Asesoria.Val_IngresosMes
                Asesoria.Val_EgresosMes.      
        FIND FIRST clientes EXCLUSIVE-LOCK
            WHERE 
                clientes.nit EQ Asesoria.Nit:SCREEN-VALUE IN FRAME F_asesoria NO-WAIT NO-ERROR.
        DO WHILE LOCKED clientes:
            MESSAGE "ERROR: Actualizando Información Del Cliente." SKIP "Registro En Uso Por Otro Usuario." SKIP "Intente Más Tarde"
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
            FIND FIRST clientes EXCLUSIVE-LOCK
                WHERE 
                    clientes.nit EQ Asesoria.Nit:SCREEN-VALUE IN FRAME F_asesoria NO-WAIT NO-ERROR.
        END.
        DO WITH FRAME FRAME-PreScoring:
       /*ASSIGN  Clientes.Salario            = decimal(FIngBruto:SCREEN-VALUE)
                Clientes.Ing_Honorarios     = decimal(FIngHonorarios:SCREEN-VALUE)
                Clientes.Ing_Otros          = decimal(FIngOtros:SCREEN-VALUE)
                Clientes.Gto_Familiar       = decimal(FEgreSost:SCREEN-VALUE)
                Clientes.Gto_Arriendo       = decimal(FEgreVivienda:SCREEN-VALUE)
                Clientes.Gto_obligacion     = decimal(FEgreNomina:SCREEN-VALUE)
                Clientes.GtoFinanc_Indir    = decimal(FEgreCIFIN:SCREEN-VALUE).*/
        END.
        FIND CURRENT clientes NO-LOCK.
        FIND CURRENT Asesoria NO-LOCK NO-ERROR.
        DISABLE {&List-1} WITH FRAME F_Asesoria.
        ASSIGN  Btn_Ingresar:SENSITIVE  = TRUE
                Button-3:SENSITIVE      = TRUE
                AS_Tasa:SENSITIVE       = FALSE.
        ENABLE Btn_Ingresar.  
        IF INTEGER(Asesoria.Clase_Producto:SCREEN-VALUE) EQ 2 
        THEN APPLY "LEAVE" TO Asesoria.Val_EgresosM.
        LEAVE.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Asesoria /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Impres
&Scoped-define SELF-NAME BUTTON-167
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-167 wWin
ON CHOOSE OF BUTTON-167 IN FRAME F_Impres /* Button 167 */
DO:
  ASSIGN Rs_Inf
         W_UsuImp = SUBSTRING(Cmb_Usuario:SCREEN-VALUE,1,4)
         W_FecIni
         W_FecFin.

  IF Rs_Inf EQ 2 THEN DO:
     MESSAGE "Desea Imprimir la Proyección del Producto" SKIP
          "dentro del Informe de la Asesoria?" VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO 
          UPDATE W_Ok.

      W_Inf = "Asesoria".     
      DEFINE VAR Listado AS CHARACTER INITIAL "".
      Listado = W_Pathspl + "\" + W_Usuario + "-LstAsesor.lst".
      {incluido/imprimir.i "Listado"}.
  END.
  IF Rs_Inf EQ 3 THEN DO:
     /* APPLY "CHOOSE":U TO BUTTON-Calcular IN FRAME FRAME-PreScoring.
      RUN creaTTAsesoria.
      RUN wimprime.w ("prAsesoriaPreScoring.p", "Asesoria PreScoring",
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
                     ?, ?,
                     ?, ?).*/
  END.                     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asesoria
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Asesoria /* Button 2 */
DO:
  ASSIGN FRAME F_Asesoria:SENSITIVE = FALSE.
         FRAME F_Impres:VISIBLE     = TRUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-3 wWin
ON CHOOSE OF BUTTON-3 IN FRAME F_Asesoria /* Button 3 */
DO:
  DEFINE VARIABLE P_PunteroA AS ROWID.
  ASSIGN WWin:SENSITIVE = FALSE.
  /*WWin:MOVE-TO-BOTTOM().*/
  
  RUN C-Asesoria.r (INPUT W_Agencia, OUTPUT P_PunteroA).
  
  ASSIGN WWin:SENSITIVE = TRUE.
  WWin:MOVE-TO-TOP().
  
  FIND Asesoria WHERE ROWID(Asesoria) EQ P_PunteroA NO-LOCK NO-ERROR.
  IF AVAILABLE Asesoria THEN DO:
     RUN Mostrar_Asesoria.
     IF INTEGER(Asesoria.Clase_Producto:SCREEN-VALUE) EQ 2 THEN
        APPLY "LEAVE" TO Asesoria.Val_EgresosM.
     IF INTEGER(Asesoria.Clase_Producto:SCREEN-VALUE) EQ 3 THEN DO:
       /* APPLY "CHOOSE":U TO BUTTON-Calcular IN FRAME FRAME-PreScoring.*/
     END.
  END.
  ELSE DO:
     MESSAGE "No ha sido escogida ninguna Asesoria" VIEW-AS ALERT-BOX.
     APPLY 'choose' TO Btn_Cancelar.
  END.
  ASSIGN Btn_Ingresar:SENSITIVE = TRUE
         Btn_Salvar:SENSITIVE   = TRUE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Asesoria.Clase_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Asesoria.Clase_Producto wWin
ON MOUSE-SELECT-CLICK OF Asesoria.Clase_Producto IN FRAME F_Asesoria /* Clase_Producto */
DO:
  APPLY "Value-Changed" TO SELF.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Asesoria.Clase_Producto wWin
ON VALUE-CHANGED OF Asesoria.Clase_Producto IN FRAME F_Asesoria /* Clase_Producto */
DO:
    CASE SELF:SCREEN-VALUE:
        WHEN "3" THEN DO:
            /*ASSIGN FRAME FRAME-PreScoring:HIDDEN           = FALSE.                        
            APPLY "ENTRY":U TO RS-FrmPago.
            APPLY "VALUE-CHANGED":U TO RS-FrmPago.*/
        END.
        OTHERWISE 
           .
    END CASE.


  IF Asesoria.Clase_Producto:SCREEN-VALUE EQ "1" THEN DO:
     ASSIGN Cmb_TipoProductos:LIST-ITEMS = ""
            W_Ok = Cmb_TipoProductos:ADD-LAST("2 - Contractual")
            W_Ok = Cmb_TipoProductos:ADD-LAST("3 - A Termino")
            Cmb_TipoProductos:SCREEN-VALUE = Cmb_TipoProductos:ENTRY(1)
            Asesoria.Val_EgresosMes:SENSITIVE     = FALSE 
            Asesoria.Val_IngresosMes:SENSITIVE    = FALSE
            Asesoria.Plazo:LABEL = "Plazo en Dìas"
            Rs_PerPago:SENSITIVE = FALSE.
     RUN Llenar_CmbProductos.
  END.
  ELSE DO:
      ASSIGN Cmb_TipoProductos:LIST-ITEMS = ""
             W_Ok = Cmb_TipoProductos:ADD-LAST("1 - Consumo")
             W_Ok = Cmb_TipoProductos:ADD-LAST("2 - Comercial")
             W_Ok = Cmb_TipoProductos:ADD-LAST("3 - Hipotecario")
             W_Ok = Cmb_TipoProductos:ADD-LAST("4 - MicroCredito")
             Cmb_TipoProductos:SCREEN-VALUE = Cmb_TipoProductos:ENTRY(1)
             Asesoria.Val_EgresosMes:SENSITIVE     = TRUE 
             Asesoria.Val_IngresosMes:SENSITIVE    = TRUE
             Asesoria.Monto:SENSITIVE              = TRUE
             Asesoria.Cuota:SENSITIVE              = FALSE
             Asesoria.Plazo:SENSITIVE              = TRUE
             Cmb_Perliquidacion:SENSITIVE          = FALSE
             Asesoria.Plazo:LABEL                  = "Plazo en Meses"
             Rs_PerPago:SENSITIVE                  = TRUE
             Rs_PerPago:SCREEN-VALUE               = "4"
             Rs_PerPago.

      RUN Llenar_CmbProductos.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_PerLiquidacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_PerLiquidacion wWin
ON VALUE-CHANGED OF Cmb_PerLiquidacion IN FRAME F_Asesoria /* Liquidación */
DO:
  APPLY "Leave" TO Asesoria.Plazo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON ENTRY OF Cmb_Productos IN FRAME F_Asesoria
DO:
    /*ASSIGN FTasa:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING(0.0)
        FTEfectiva:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING(0.0).*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON VALUE-CHANGED OF Cmb_Productos IN FRAME F_Asesoria
DO:
  DEFI VAR W_NitAnt LIKE Asesoria.Nit.
  DEFI VAR W_AseAnt LIKE Asesoria.Num_Asesoria.

 DO WITH FRAME F_Asesoria:
    ASSIGN W_NitAnt = Asesoria.Nit:SCREEN-VALUE
           W_AseAnt = INTEG(Asesoria.Num_Asesoria:SCREEN-VALUE).

    RUN Inicializar_Variables.

    ASSIGN Asesoria.Nit:SCREEN-VALUE          = W_NitAnt
           Asesoria.Num_Asesoria:SCREEN-VALUE = STRING(W_AseAnt).

  IF Asesoria.Clase_Producto:SCREEN-VALUE EQ "1" THEN DO:
     FIND Pro_Ahorros WHERE 
          Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) AND
          Pro_Ahorros.Cod_Ahorro EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3))
          NO-LOCK NO-ERROR.
     IF AVAILABLE(Pro_Ahorros) THEN DO:
        IF Pro_Ahorros.Id_ForLiquidacion EQ 1 THEN DO: 
           DISABLE Asesoria.FOR_Liquidacion.
        END.
        ELSE ENABLE Asesoria.FOR_Liquidacion.
        
        IF Pro_Ahorros.Tip_Ahorro EQ 2 THEN DO:
           DISABLE Cmb_PerLiquidacion.
           Cmb_PerLiquidacion:SCREEN-VALUE = Cmb_PerLiquidacion:ENTRY(INTEGER(Pro_Ahorros.Per_Liquidacion)).
        END.
        ELSE DO:
           ENABLE Cmb_PerLiquidacion.
           Cmb_PerLiquidacion:SCREEN-VALUE = Cmb_PerLiquidacion:ENTRY(INTEGER(Pro_Ahorros.Per_Liquidacion)).
        END.
        
        IF SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "2" THEN DO:
           DISABLE Asesoria.Monto.
           ENABLE Asesoria.Cuota.
        END.
        ELSE DO:
           ENABLE Asesoria.Monto.
           DISABLE Asesoria.Cuota.
        END.
        
        IF Pro_Ahorros.Id_Tasa EQ 1 THEN
           DISABLE AS_Tasa.
        ELSE
           ENABLE AS_Tasa.
           
        APPLY "ENTRY" TO Asesoria.Nit.
     END.
  END.
  IF Asesoria.Clase_Producto:SCREEN-VALUE EQ "2" THEN DO:
     FIND Pro_Creditos WHERE 
          Pro_Creditos.Tip_Credito EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) AND
          Pro_Creditos.Cod_Credito EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3))
          NO-LOCK NO-ERROR.
     IF AVAILABLE(Pro_Creditos) THEN
        APPLY "ENTRY" TO Asesoria.Nit.
       /* RUN Habilitar_Campos_Credito.*/
  END.
 END.

/*  IF Asesoria.Clase_Producto:SCREEN-VALUE EQ "3" THEN DO:                                                                            */
/*      FIND FIRST pro_creditos WHERE Pro_Creditos.Cod_Credito EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) NO-LOCK NO-ERROR. */
/*      IF AVAILABLE pro_creditos THEN DO:                                                                                             */
/*         FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Creditos.Cod_Tasa                                                 */
/*                                       AND Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.                                              */
/*         IF AVAILABLE indicadores THEN                                                                                               */
/*             ASSIGN viCodTasa = indicadores.tasa                                                                                     */
/*                 FTasa:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING(indicadores.tasa).                                            */
/*         ELSE DO:                                                                                                                    */
/*             MESSAGE "Tasa para producto no encontrado" SKIP                                                                         */
/*                 "No se puede continuar"                                                                                             */
/*                 VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                  */
/*             RETURN.                                                                                                                 */
/*         END.                                                                                                                        */
/*      END.                                                                                                                           */
/*      IF NOT AVAILABLE(pro_creditos) THEN DO:                                                                                        */
/*          MESSAGE "No se encuentra Producto de Credito"                                                                              */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                     */
/*          RETURN.                                                                                                                    */
/*      END.                                                                                                                           */
/*  END.                                                                                                                               */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_TipoProductos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_TipoProductos wWin
ON VALUE-CHANGED OF Cmb_TipoProductos IN FRAME F_Asesoria /* Productos */
DO:
  DO WITH FRAME F_Asesoria:
     RUN Llenar_CmbProductos.
     Tpdt = INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)).
  END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Impres
&Scoped-define SELF-NAME Cmb_Usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Usuario wWin
ON VALUE-CHANGED OF Cmb_Usuario IN FRAME F_Impres /* Usuarios */
DO:
  ASSIGN W_UsuImp = SUBSTRING(Cmb_Usuario:SCREEN-VALUE,1,4).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asesoria
&Scoped-define SELF-NAME Asesoria.Cuota
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Asesoria.Cuota wWin
ON LEAVE OF Asesoria.Cuota IN FRAME F_Asesoria /* Cuota */
DO:
  IF Asesoria.Nit:SCREEN-VALUE LE " " THEN DO:  
     APPLY "ENTRY" TO Asesoria.Nit.             
     RETURN NO-APPLY.                           
  END.                                          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Asesoria.Monto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Asesoria.Monto wWin
ON LEAVE OF Asesoria.Monto IN FRAME F_Asesoria /* Monto */
DO:
  IF Asesoria.Nit:SCREEN-VALUE LE " " THEN DO:
     APPLY "ENTRY" TO Asesoria.Nit.
     RETURN NO-APPLY.
  END.
  
  APPLY "ENTRY" TO Asesoria.Plazo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Asesoria.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Asesoria.Nit wWin
ON LEAVE OF Asesoria.Nit IN FRAME F_Asesoria /* Id. Asesorado */
DO:
  DO WITH FRAME F_Asesoria:
      IF (Asesoria.Clase_Producto:SCREEN-VALUE EQ "2") THEN 
          ASSIGN Asesoria.Monto:SENSITIVE = TRUE.

     IF (Asesoria.Clase_Producto:SCREEN-VALUE EQ "1" AND NOT AVAIL Pro_Ahorros)
     OR (Asesoria.Clase_Producto:SCREEN-VALUE EQ "2" AND NOT AVAIL Pro_Creditos) THEN DO:
         APPLY "Entry" TO Cmb_Productos.
         MESSAGE "seleccione El pdcto.de Ahorro/Crédito a Asesorar..."
             VIEW-AS ALERT-BOX INFO BUTTONS OK.
         RETURN.
     END.
  
     FIND Clientes WHERE Clientes.Nit EQ Asesoria.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE(Clientes) THEN
        ASSIGN W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2)
               W_Telef:SCREEN-VALUE = Clientes.Tel_Comercial.
     ELSE DO:
        WWin:SENSITIVE = FALSE.    
            
        RUN C-Clientes.R(INPUT  1,    INPUT W_Agencia, 
                         OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
            
        WWin:SENSITIVE = TRUE.     
        FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
        ASSIGN Asesoria.Nit:SCREEN-VALUE = P_Nit
               W_Nomtitular:SCREEN-VALUE = CAPS(TRIM(P_Nombre) + " " + P_Apellido)
               W_Telef:SCREEN-VALUE = Clientes.Tel_Comercial.
               
        
        APPLY "ENTRY" TO SELF.    
     END.
     
     /*IF (Asesoria.Nit:SCREEN-VALUE LE "0" OR W_NomTitular:SCREEN-VALUE = "") 
     AND W_Primero THEN DO:
        MESSAGE "No se permite crear una asesoria a una persona o empresa" SKIP
                "No matriculadas en la Base de datos. Rectifique!" VIEW-AS ALERT-BOX ERROR.
        APPLY "ENTRY" TO SELF.
        RETURN.
     END.*/

     
     FIND FIRST Creditos WHERE Creditos.Nit         EQ Asesoria.Nit:SCREEN-VALUE
                           AND Creditos.Sdo_Capital GT 0
                           AND Creditos.Fec_Pago    LT W_Fecha NO-LOCK NO-ERROR.
     IF AVAIL(Creditos) THEN
        MESSAGE "El solicitante tiene Crèditos en Mora en la Cooperativa..."
            VIEW-AS ALERT-BOX TITLE "Alerta por Morosidad".

     IF  AVAIL(Clientes) AND (W_Fecha - Clientes.Fec_Nacimiento) / 364 GT 69
     AND (W_Fecha - Clientes.Fec_Asociac) / 364 LT 3 THEN
         MESSAGE "El solicitante no califica como Sujeto de Crèdito..."
            VIEW-AS ALERT-BOX TITLE "Alerta por Edad".
     
     W_Primero = TRUE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Asesoria.Plazo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Asesoria.Plazo wWin
ON LEAVE OF Asesoria.Plazo IN FRAME F_Asesoria /* Plazo del Crédito */
DO:
 DO WITH FRAME F_Asesoria:
    IF Asesoria.Nit:SCREEN-VALUE LE " " THEN DO:  
       APPLY "ENTRY" TO Asesoria.Nit.             
       RETURN NO-APPLY.                           
    END.  
    
    IF Asesoria.Plazo:SCREEN-VALUE LE "0" THEN
       RETURN NO-APPLY.                               
 
    IF Asesoria.Clase_Producto:SCREEN-VALUE EQ "1" THEN DO:
       FIND Pro_Ahorros WHERE 
          Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) AND
          Pro_Ahorros.Cod_Ahorro EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) NO-LOCK NO-ERROR.
    
       RUN AS_Ahorros.
       ASSIGN Asesoria.Val_EgresosMes:SCREEN-VALUE  = "0"
              Asesoria.Val_IngresosMes:SCREEN-VALUE = "0"
              Asesoria.Val_EgresosMes:SENSITIVE     = FALSE
              Asesoria.Val_IngresosMes:SENSITIVE    = FALSE.
              
    END.
    ELSE DO:
       FIND Pro_Creditos WHERE 
          Pro_Creditos.Tip_Credito EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) AND
          Pro_Creditos.Cod_Credito EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) NO-LOCK NO-ERROR.

       APPLY "leave" TO Asesoria.Val_EgresosM.
       APPLY "Mouse-Select-Click" TO Rs_PerPago.

       RUN AS_Creditos. 

       RUN MostrarAportes.
       
       ASSIGN Asesoria.Val_EgresosMes:SENSITIVE     = TRUE                                  
              Asesoria.Val_IngresosMes:SENSITIVE    = TRUE. 
              
       APPLY "Leave" TO Asesoria.Val_EgresosM.       
              
       APPLY "ENTRY" TO Asesoria.Val_Ingres.
       /*RETURN NO-APPLY.*/
    END.

    ASSIGN W_TManual:SCREEN-VALUE = AS_Tasa:SCREEN-VALUE
           W_TManual.
 END.  
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Impres
&Scoped-define SELF-NAME Rs_Inf
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Inf wWin
ON MOUSE-SELECT-CLICK OF Rs_Inf IN FRAME F_Impres
DO:
  ASSIGN Rs_Inf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_Inf wWin
ON VALUE-CHANGED OF Rs_Inf IN FRAME F_Impres
DO:
  ASSIGN Rs_Inf.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asesoria
&Scoped-define SELF-NAME Rs_PerPago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Rs_PerPago wWin
ON MOUSE-SELECT-CLICK OF Rs_PerPago IN FRAME F_Asesoria
DO:
  ASSIGN Rs_PerPago
         Asesoria.Plazo:LABEL = "Plazo en Meses".

  IF Rs_PerPago EQ 1 THEN
     Asesoria.Plazo:LABEL = "Plazo Semanal".
  ELSE IF Rs_PerPago EQ 2 THEN
     Asesoria.Plazo:LABEL = "Plazo Decadal".
  ELSE IF Rs_PerPago EQ 3 THEN
     Asesoria.Plazo:LABEL = "Plazo Quincenal".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Asesoria.Val_EgresosMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Asesoria.Val_EgresosMes wWin
ON LEAVE OF Asesoria.Val_EgresosMes IN FRAME F_Asesoria /* Egresos Mes */
DO:
  IF Asesoria.Nit:SCREEN-VALUE LE " " THEN DO:  
     APPLY "ENTRY" TO Asesoria.Nit.             
     RETURN NO-APPLY.                           
  END.  
  
  ASSIGN W_PorEnd = ((DEC(Asesoria.Val_EgresosM:SCREEN-VALUE) + DEC(Asesoria.Cuota:SCREEN-VALUE)) / DEC(Asesoria.Val_IngresosM:SCREEN-VALUE)) * 100
         W_PorEnd:SCREEN-VALUE = STRING(W_PorEnd).

  IF W_PorEnd GT 50 THEN
     MESSAGE "El nivel de Endeudamiento Supera el 50%..." VIEW-AS ALERT-BOX
         TITLE "      Alerta de Viabilidad".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Asesoria.Val_IngresosMes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Asesoria.Val_IngresosMes wWin
ON LEAVE OF Asesoria.Val_IngresosMes IN FRAME F_Asesoria /* Ingresos Mes */
DO:
  IF Asesoria.Nit:SCREEN-VALUE LE " " THEN DO:  
     APPLY "ENTRY" TO Asesoria.Nit.             
     RETURN NO-APPLY.                           
  END.  
  
  IF Asesoria.Cuota:SCREEN-VALUE LE " " THEN DO:  
     APPLY "ENTRY" TO Asesoria.Plazo.             
     RETURN NO-APPLY.                           
  END.  

  APPLY "ENTRY" TO Asesoria.Val_Egreso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Impres
&Scoped-define SELF-NAME W_FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecFin wWin
ON LEAVE OF W_FecFin IN FRAME F_Impres /* Hasta */
DO:
  ASSIGN W_FecFin.

  IF W_FecFin LT W_FecIni THEN
     ASSIGN W_FecFin = W_FecIni
            W_FecFin:SCREEN-VALUE = STRING(W_FecIni).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecIni wWin
ON LEAVE OF W_FecIni IN FRAME F_Impres /* Desde */
DO:
  ASSIGN W_FecIni.
  APPLY "ENTRY" TO W_FecFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Asesoria
&Scoped-define SELF-NAME W_TManual
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_TManual wWin
ON LEAVE OF W_TManual IN FRAME F_Asesoria /* Efectiva Anual */
DO:
    ASSIGN W_TManual
           W_Manual  = TRUE.

    APPLY "LEAVE" TO Asesoria.Plazo.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME B_Proyeccion
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna_Linea wWin 
PROCEDURE Asigna_Linea :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN P_linea = Pro_creditos.cod_credito.
CASE P_linea:
     WHEN 705 THEN ASSIGN P_linea = 5.
     WHEN 710 THEN ASSIGN P_linea = 10.
     WHEN 711 THEN ASSIGN P_linea = 11.
     WHEN 715 THEN ASSIGN P_linea = 15.
     WHEN 720 THEN ASSIGN P_linea = 20.
     WHEN 722 THEN ASSIGN P_linea = 22.
     WHEN 725 THEN ASSIGN P_linea = 25.
     WHEN 730 THEN ASSIGN P_linea = 30.
     WHEN 733 THEN ASSIGN P_linea = 33.
     WHEN 735 THEN ASSIGN P_linea = 35.
     WHEN 740 THEN ASSIGN P_linea = 40.
     WHEN 745 THEN ASSIGN P_linea = 45.
     WHEN 748 THEN ASSIGN P_linea = 48.
     WHEN 749 THEN ASSIGN P_linea = 49.
     WHEN 750 THEN ASSIGN P_linea = 50.
     WHEN 751 THEN ASSIGN P_linea = 51.
     WHEN 752 THEN ASSIGN P_linea = 52.
     WHEN 753 THEN ASSIGN P_linea = 53.
     WHEN 770 THEN ASSIGN P_linea = 70.
     WHEN 775 THEN ASSIGN P_linea = 75.
     WHEN 780 THEN ASSIGN P_linea = 80.
     WHEN 785 THEN ASSIGN P_linea = 85.
     WHEN 790 THEN ASSIGN P_linea = 90.
     WHEN 795 THEN ASSIGN P_linea = 95.
     WHEN 821 THEN ASSIGN P_linea = 521.
     WHEN 822 THEN ASSIGN P_linea = 522.
     WHEN 823 THEN ASSIGN P_linea = 523.
     WHEN 824 THEN ASSIGN P_linea = 524.
     WHEN 825 THEN ASSIGN P_linea = 525.
     WHEN 826 THEN ASSIGN P_linea = 526.
     WHEN 827 THEN ASSIGN P_linea = 527.
     WHEN 829 THEN ASSIGN P_linea = 529.
     WHEN 830 THEN ASSIGN P_linea = 530.
     WHEN 831 THEN ASSIGN P_linea = 531.
     WHEN 832 THEN ASSIGN P_linea = 532.
     WHEN 833 THEN ASSIGN P_linea = 533.
     WHEN 834 THEN ASSIGN P_linea = 534.
     WHEN 835 THEN ASSIGN P_linea = 535.
     WHEN 836 THEN ASSIGN P_linea = 536.
     WHEN 837 THEN ASSIGN P_linea = 537.
     WHEN 838 THEN ASSIGN P_linea = 538.
     WHEN 839 THEN ASSIGN P_linea = 539.
     WHEN 840 THEN ASSIGN P_linea = 540.
     WHEN 841 THEN ASSIGN P_linea = 541.
     WHEN 842 THEN ASSIGN P_linea = 542.
     WHEN 856 THEN ASSIGN P_linea = 556.
     WHEN 857 THEN ASSIGN P_linea = 557.
     WHEN 858 THEN ASSIGN P_linea = 558.
     WHEN 859 THEN ASSIGN P_linea = 559.
     WHEN 860 THEN ASSIGN P_linea = 560.
     WHEN 861 THEN ASSIGN P_linea = 561.
     WHEN 870 THEN ASSIGN P_linea = 570.
     WHEN 871 THEN ASSIGN P_linea = 571.
     WHEN 872 THEN ASSIGN P_linea = 572.
     WHEN 873 THEN ASSIGN P_linea = 573.
     WHEN 874 THEN ASSIGN P_linea = 574.
     WHEN 875 THEN ASSIGN P_linea = 575.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AS_Ahorros wWin 
PROCEDURE AS_Ahorros :
DO WITH FRAME F_Asesoria:
  IF Pro_Ahorros.Id_Tasa EQ 1 THEN DO:
     IF (DECIMAL(Asesoria.Monto:SCREEN-VALUE) EQ 0 OR DECIMAL(Asesoria.Plazo:SCREEN-VALUE) EQ 0) AND
        SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1) NE "2" THEN DO:
        MESSAGE "El Monto y el Plazo deben ser mayores a cero. Rectifique!" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
     END.
     
     IF NOT W_Manual THEN DO:
        RUN Hallar_Tasa (INPUT Pro_Ahorros.Indicador, INPUT DECIMAL(Asesoria.Monto:SCREEN-VALUE),
                         INPUT DECIMAL(Asesoria.Plazo:SCREEN-VALUE), OUTPUT W_Tasa, OUTPUT W_Puntos).

        AS_Tasa:SCREEN-VALUE = STRING(W_Tasa).
     END.
     ELSE DO:
        ASSIGN W_Manual             = FALSE
               AS_Tasa:SCREEN-VALUE = STRING(W_TManual).

        RUN Convertir_Tasa_Periodica (INPUT DECIMAL(Asesoria.Monto:SCREEN-VALUE), INPUT W_TManual).
     END.

     RUN Proyeccion_Ahorros.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE AS_Creditos wWin 
PROCEDURE AS_Creditos :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DO WITH FRAME F_Asesoria. END.
  
  IF (DECIMAL(Asesoria.Monto:SCREEN-VALUE) LE 0 OR DECIMAL(Asesoria.Plazo:SCREEN-VALUE) LE 0) AND
        SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1) EQ "2" THEN DO:
        MESSAGE "El Monto y el Plazo deben ser mayores a cero. Rectifique!" VIEW-AS ALERT-BOX ERROR.
        RETURN NO-APPLY.
  END.
  
  IF AVAILABLE Pro_Creditos THEN DO:
     ASSIGN Dias      = INTEG(Asesoria.Plazo:SCREEN-VALUE) * 30
            NomPer    = " Meses"
            W_DiaCred = 30.
     
     CASE Rs_PerPago:
       WHEN 1 THEN ASSIGN Dias      = INTEG(Asesoria.Plazo:SCREEN-VALUE) * 7
                          W_DiaCred = 7
                          NomPer    = " Semanas".               
       WHEN 2 THEN ASSIGN Dias      = INTEG(Asesoria.Plazo:SCREEN-VALUE) * 10                 
                          W_DiaCred = 10
                          NomPer    = " Decadas".               
       WHEN 3 THEN ASSIGN Dias      = INTEG(Asesoria.Plazo:SCREEN-VALUE) * 15                 
                          W_DiaCred = 15
                          NomPer    = " Quincenas".                                   
     END CASE.

     IF NOT W_Manual THEN
        RUN Buscar_Indicadores.
     ELSE DO:
         ASSIGN W_Manual             = FALSE
                 AS_Tasa:SCREEN-VALUE = STRING(W_TManual).

         RUN Hallar_TasaNominal.
     
         Asesoria.Tasa:SCREEN-VALUE = STRING(Tas_Nominal / 12).
     END.
  END.
  ELSE DO:
     MESSAGE "Debe seleccionar un pdcto de Crèditos." VIEW-AS ALERT-BOX.
     RETURN.
  END.
  
  RUN Liquidar. 
  
  RUN Proyecc_Credito.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Base_Retencion wWin 
PROCEDURE Buscar_Base_Retencion :
ASSIGN W_Porce   = 0
           W_BasRet  = 0
           W_CodBase = "".
    
    FIND Liqui_Int WHERE Liqui_Int.Cod_Producto   EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Asesoria,1,3))
                     AND Liqui_Int.Clase_Producto EQ 1
                   NO-LOCK NO-ERROR.
    IF AVAILABLE Liqui_Int THEN
       ASSIGN W_BasRet  = Liqui_Int.Base 
              W_CodBase = Liqui_Int.Cod_base.
              
    FIND Base_Ret WHERE Base_Ret.Cod_base EQ W_CodBase NO-LOCK NO-ERROR.
    IF AVAILABLE(Base_Ret) AND AVAILABLE Liqui_Int THEN
       W_Porce = Base_Ret.Porcentaje.
 

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Indicadores wWin 
PROCEDURE Buscar_Indicadores :
/*------------------------------------------------------------------------------
  Purpose:     Indicadores para pdctos de crèdito.  
------------------------------------------------------------------------------*/
  DO WITH FRAME F_Asesoria.
     IF Pro_Creditos.Id_Tasa EQ 1 THEN DO:
        FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Creditos.Cod_Tasa
                                 AND Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Indicadores) THEN DO:
           MESSAGE "No exite un indicadores para la linea" SKIP
                   "de producto de crédito. Consulte con el Administrador" SKIP
                   "del sistema acerca de esta inconsistencia" VIEW-AS ALERT-BOX ERROR TITLE "Configuración del Indicador".
           APPLY "ENTRY" TO Asesoria.Monto.
           RETURN NO-APPLY.
        END.
        
        IF NOT Indicadores.Rangos THEN DO:
           IF Indicadores.Tasa EQ 0 THEN DO:
              MESSAGE "El indicador tiene tasa en 0" SKIP
                      "no se permite crear la asesorìa con esta tasa" SKIP
                       VIEW-AS ALERT-BOX ERROR.
              RETURN.
           END. /*IF Indicadores.Tasa EQ 0*/
           ASSIGN AS_Tasa:SCREEN-VALUE = STRING(Indicadores.Tasa).
        END. /*IF NOT Indicadores.Rangos*/
        ELSE 
            RUN Hallar_RangosInd.
     END. /*IF Pro_Creditos.Id_Tasa EQ 1 */
     ELSE DO:
        IF AS_Tasa:SCREEN-VALUE LE "0" THEN DO:
           MESSAGE "EL producto de Crèdito permite que el asesor" SKIP
                   "entre la tasa para la asesorìa." VIEW-AS ALERT-BOX INFORMATION.
           ASSIGN AS_Tasa:SENSITIVE = YES.
           APPLY "Entry" TO AS_Tasa.
           RETURN NO-APPLY.
        END. /*IF AS_Tasa:SCREEN-VALUE LE "0" */
     END. /*ELSE DO:*/
     
     RUN Hallar_TasaNominal.
     
     Asesoria.Tasa:SCREEN-VALUE = STRING(Tas_Nominal / 12).
     IF Rs_PerPago EQ 1 THEN
        Asesoria.Tasa:SCREEN-VALUE = STRING(Tas_Nominal / 52).
     ELSE IF Rs_PerPago EQ 2 THEN
        Asesoria.Tasa:SCREEN-VALUE = STRING(Tas_Nominal / 36).
     IF Rs_PerPago EQ 3 THEN
        Asesoria.Tasa:SCREEN-VALUE = STRING(Tas_Nominal / 24).

  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcEgrePS wWin 
PROCEDURE calcEgrePS :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
/*
CASE RS-FrmPago:SCREEN-VALUE IN FRAME FRAME-PreScoring :
    WHEN "1" THEN DO:
        ASSIGN 
            FEgreMes50:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING(INTEGER(FEgreNomina:SCREEN-VALUE) + 
                                                                         INTEGER(FEgreCap50:SCREEN-VALUE))
            FEgreMesSMLV:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING(INTEGER(FEgreNomina:SCREEN-VALUE) + 
                                                                         INTEGER(FEgreCapSMLV:SCREEN-VALUE)).


        ASSIGN FEgreCap50:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING((INTEGER(FIngBruto:SCREEN-VALUE)  / 2) - 
                                                                         INTEGER(FEgreNomina:SCREEN-VALUE)).
               FEgreCapSMLV:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING(INTEGER(FIngBruto:SCREEN-VALUE) - 
                                                                            INTEGER(FEgreNomina:SCREEN-VALUE) -
                                                                            viSMLV).
        ASSIGN  FctaMax50:SCREEN-VALUE IN FRAME FRAME-PreScoring = FEgreCap50:SCREEN-VALUE
                FctaMaxSMLV:SCREEN-VALUE IN FRAME FRAME-PreScoring = FEgreCapSMLV:SCREEN-VALUE.

    END.
    WHEN "2" THEN DO:

        ASSIGN FIngMes50:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING(INTEGER(Asesoria.Val_ingresosMes:SCREEN-VALUE IN FRAME FRAME-PreScoring) - 
                                                                          (INTEGER(FEgreNomina:SCREEN-VALUE)+
                                                                          INTEGER(FEgreSost:SCREEN-VALUE) + 
                                                                          INTEGER(FEgreVivienda:SCREEN-VALUE) + 
                                                                          INTEGER(FEgreCIFIN:SCREEN-VALUE)
                                                                          ))
            FEgreMes50:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING((INTEGER(FEgreNomina:SCREEN-VALUE)+
                                                                       INTEGER(FEgreSost:SCREEN-VALUE) + 
                                                                       INTEGER(FEgreVivienda:SCREEN-VALUE) + 
                                                                       INTEGER(FEgreCIFIN:SCREEN-VALUE)
                                                                     ))
            FctaMax50:SCREEN-VALUE IN FRAME FRAME-PreScoring = FIngMes50:SCREEN-VALUE.

    END.
END CASE.
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calcIngPSNom wWin 
PROCEDURE calcIngPSNom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

 /*   ASSIGN Val_ingresosMes:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING(INTEGER(FIngBruto:SCREEN-VALUE) + 
                                                                           INTEGER(FIngOtros:SCREEN-VALUE) + 
                                                                           INTEGER(FIngHonorarios:SCREEN-VALUE))
           FIngMes50:SCREEN-VALUE = STRING(INTEGER(Val_IngresosMes:SCREEN-VALUE IN FRAME FRAME-PreScoring) - 
                                           INTEGER(FEgreMes50:SCREEN-VALUE))
           FIngMesSMLV:SCREEN-VALUE = STRING(INTEGER(Val_IngresosMes:SCREEN-VALUE IN FRAME FRAME-PreScoring) - 
                                             INTEGER(FEgreMesSMLV:SCREEN-VALUE)).

   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Colocar_Dia wWin 
PROCEDURE Colocar_Dia :
DEFINE INPUT-OUTPUT PARAMETER W_PFecAux AS DATE.
 DEFINE VAR                    W_FecAux  AS DATE.
 
   W_FecAux = W_PFecAux.
   IF W_Mes EQ 2 THEN DO:  /*necesario, puesto que si la apertura fue realizada un 30 o 31 */
      IF YEAR(W_Fec) / 4 EQ 0 THEN DO:
         IF W_DiaPer GT 28 THEN
            W_FecAux = DATE(MONTH(W_PfecAux), 29, YEAR(W_Pfecaux)).
         ELSE
            W_FecAux = DATE(MONTH(W_PfecAux), W_DiaPer, YEAR(W_Pfecaux)).
      END.
      ELSE DO:
         IF W_DiaPer GT 28 THEN
            W_FecAux = DATE(MONTH(W_PfecAux), 28, YEAR(W_Pfecaux)).
         ELSE
            W_FecAux = DATE(MONTH(W_PfecAux), W_DiaPer, YEAR(W_Pfecaux)).
      END.
      W_FecAux = DATE(W_Mes, DAY(W_FecAux), YEAR(W_Pfecaux)).
   END.
   IF W_Mes EQ 13 THEN 
      W_Mes = 1.
   IF W_Mes EQ 4 OR W_Mes EQ 6  OR W_Mes EQ 9 OR W_Mes EQ 11 THEN DO:
      IF W_DiaPer GT 30 THEN 
         W_FecAux = DATE(W_Mes, 30, YEAR(W_Pfecaux)).
      ELSE
         W_FecAux = DATE(W_Mes, W_DiaPer, YEAR(W_Pfecaux)).
   END.
   ELSE
      IF W_Mes NE 2 THEN 
         W_FecAux = DATE(W_Mes, W_DiaPer, YEAR(W_Pfecaux)).
   W_PFecAux = W_FecAux.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contractual wWin 
PROCEDURE Contractual :
/*------------------------------------------------------------------------------
  Objetivo:     Liquida los intereses de los productos de ahorro contractuales  
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
    
    ASSIGN TmpAho.T_Capital       = W_Cap + TmpAho.T_Valor
                 W_Cap            = TmpAho.T_Capital
                 TmpAho.T_Interes = W_Cap * (DECIMAL(Asesoria.Tasa:SCREEN-VALUE) / 12) / 100 
                 WInt             = WInt + TmpAho.T_Interes
                 TmpAho.T_IntTot  = WInt 
                 W_Cap            = TmpAho.T_Capital + TmpAho.T_Interes.
                 
    IF W_porce GT 0 AND W_BasRet GT 0 AND W_BasRet LT TmpAho.T_Interes THEN 
       TmpAho.T_Reten = (TmpAho.T_Interes - W_BasRet) * (W_porce / 100).
    ELSE
       TmpAho.T_Reten = 0.                 
                 
END.
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Convertir_Tasa_Periodica wWin 
PROCEDURE Convertir_Tasa_Periodica :
DEFINE INPUT PARAMETER W_Monto LIKE Ahorros.Monto.
DEFINE INPUT PARAMETER W_Tasa  LIKE Indicadores.Tasa.
CASE INTEGER(SUBSTRING(Cmb_Perliquidacion:SCREEN-VALUE IN FRAME F_Asesoria,1,2)):
     WHEN 1 THEN
         ASSIGN W_Per      = 365
                W_Perliqui = 0
                W_DiaPer   = 1.
     WHEN 2 THEN
         ASSIGN W_Per      = 12
                W_Perliqui = 4
                W_DiaPer   = 30.
     WHEN 3 THEN
         ASSIGN W_Per      = 4
                W_Perliqui = 6
                W_DiaPer   = 90.
     WHEN 4 THEN
         ASSIGN W_Per      = 2
                W_Perliqui = 8
                W_DiaPer   = 180.
     WHEN 5 THEN
         ASSIGN W_Per      = 1
                W_Perliqui = 9
                W_DiaPer   = 360.
     WHEN 6 THEN DO:
         IF INTEGER(Asesoria.Plazo:SCREEN-VALUE) GT 360 THEN
            RUN MostrarMensaje IN W_Manija (INPUT 325,OUTPUT W_Rpta).
         ELSE
            ASSIGN W_Per      = (360 / INTEGER(Asesoria.Plazo:SCREEN-VALUE))
                   W_Perliqui = 10
                   W_DiaPer   = INTEGER(Asesoria.Plazo:SCREEN-VALUE).
     END.
  END CASE.
  
  IF INTEGER(Asesoria.For_Liquidacion:SCREEN-VALUE IN FRAME F_Asesoria) EQ 2 THEN
     RUN EFNV IN W_ManFin (INPUT W_Tasa / 100, INPUT W_Per, OUTPUT W_TasPer).
  ELSE
     IF INTEGER(Asesoria.For_Liquidacion:SCREEN-VALUE) EQ 1 THEN
        RUN EFNA IN W_ManFin (INPUT W_Tasa / 100, INPUT W_Per, OUTPUT W_TasPer).

  ASSIGN Asesoria.Tasa:SCREEN-VALUE   = STRING((W_TasPer * 100))
         Tasa_Nominal:SCREEN-VALUE     = STRING((W_TasPer * W_Per) * 100).
        /* Interes_PorPagar:SCREEN-VALUE = STRING(W_Tasa * W_Monto / 100).*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ConvPlazo wWin 
PROCEDURE ConvPlazo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME F_Asesoria: END.
   W_Plazo = 1.
   IF INTEG (SUBSTRING(Cmb_PerLiquidacion:SCREEN-VALUE,1,2)) EQ 1 THEN
      W_Plazo = INTEGER(Asesoria.Plazo:SCREEN-VALUE).
   IF INTEG (SUBSTRING(Cmb_PerLiquidacion:SCREEN-VALUE,1,2)) EQ 2 THEN
      W_Plazo = INTEGER(Asesoria.Plazo:SCREEN-VALUE) / 30.
   ELSE IF INTEG (SUBSTRING(Cmb_PerLiquidacion:SCREEN-VALUE,1,2)) EQ 3 THEN
      W_Plazo = INTEGER(Asesoria.Plazo:SCREEN-VALUE) / 90.
   ELSE IF INTEG (SUBSTRING(Cmb_PerLiquidacion:SCREEN-VALUE,1,2)) EQ 4 THEN
      W_Plazo = INTEGER(Asesoria.Plazo:SCREEN-VALUE) / 180.
   ELSE IF INTEG (SUBSTRING(Cmb_PerLiquidacion:SCREEN-VALUE,1,2)) EQ 5 THEN
      W_Plazo = INTEGER(Asesoria.Plazo:SCREEN-VALUE) / 360.
   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTTAsesoria wWin 
PROCEDURE creaTTAsesoria :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

      /*FIND Clientes WHERE Clientes.Nit EQ Asesoria.Nit:SCREEN-VALUE IN FRAME FRAME-PreScoring NO-LOCK NO-ERROR.
      EMPTY TEMP-TABLE TTAsesoria.
      CREATE TTAsesoria.
      UPDATE
          TTAsesoria.nit        = Asesoria.nit:SCREEN-VALUE IN FRAME FRAME-PreScoring
          cliente               = W_NomTitular:SCREEN-VALUE IN FRAME FRAME-PreScoring
          TTAsesoria.agencia    = Clientes.Agencia
          TTAsesoria.monto      = INTEGER(Asesoria.Monto:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          TTAsesoria.plazo      = INTEGER(FPlazo:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          numPers               = INTEGER(FPersCargo:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          TTAsesoria.tasa       = DECIMAL(FTasa:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          porGtoSost            = vdePorcenCiu
          ingBruto              = INTEGER(FIngBruto:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          ingOtros              = INTEGER(FIngOtros:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          ingHonor              = INTEGER(FIngHonorarios:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          ingTotal              = INTEGER(Asesoria.Val_IngresosMes:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          egrNomina             = INTEGER(FEgreNomina:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          cap50                 = INTEGER(FEgreCap50:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          capSMLV               = INTEGER(FEgreCapSMLV:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          gtosSost              = INTEGER(FEgreSost:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          gtosVivi              = INTEGER(FEgreVivienda:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          gtosCIFIN             = INTEGER(FEgreCIFIN:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          ingNeto50             = INTEGER(FIngMes50:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          egreMes50             = INTEGER(FEgreMes50:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          ctaMax50              = INTEGER(FctaMax50:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          CtaValSol50           = INTEGER(FCtaValSol50:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          IngNetoSMLV           = INTEGER(FIngMesSMLV:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          egreMesSMLV           = INTEGER(FEgreMesSMLV:SCREEN-VALUE IN FRAME FRAME-PreScoring)
          ctaMaxSMLV            = INTEGER(FctaMaxSMLV:SCREEN-VALUE IN FRAME FRAME-PreScoring).

      ASSIGN viCnt = 0.
      FOR EACH TTOpcCred NO-LOCK:
          ASSIGN viCnt = viCnt + 1.
          CASE viCnt:
              WHEN 1 THEN
                  UPDATE
                          TTAsesoria.plazo1    = TTOpcCred.TPlazo 
                          TTAsesoria.Monto1A   = TTOpcCred.TMOpcA 
                          TTAsesoria.Monto1B   = TTOpcCred.TMOpcB.
              WHEN 2 THEN
                  UPDATE
                          TTAsesoria.plazo2    = TTOpcCred.TPlazo 
                          TTAsesoria.Monto2A   = TTOpcCred.TMOpcA 
                          TTAsesoria.Monto2B   = TTOpcCred.TMOpcB.
              WHEN 3 THEN
                  UPDATE
                          TTAsesoria.plazo3    = TTOpcCred.TPlazo 
                          TTAsesoria.Monto3A   = TTOpcCred.TMOpcA 
                          TTAsesoria.Monto3B   = TTOpcCred.TMOpcB.
              WHEN 4 THEN
                  UPDATE
                          TTAsesoria.plazo4    = TTOpcCred.TPlazo 
                          TTAsesoria.Monto4A   = TTOpcCred.TMOpcA 
                          TTAsesoria.Monto4B   = TTOpcCred.TMOpcB.
              WHEN 5 THEN
                  UPDATE
                          TTAsesoria.plazo5    = TTOpcCred.TPlazo 
                          TTAsesoria.Monto5A   = TTOpcCred.TMOpcA 
                          TTAsesoria.Monto5B   = TTOpcCred.TMOpcB.
          END CASE.
      END.
        */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE creaTTParamCiudad wWin 
PROCEDURE creaTTParamCiudad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


/* CON Personas a Cargo Ciudades Grandes*/
    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "pcCapi"
        ingIni     = viSMLV
        ingFin     = viSMLV * 3
        porcentaje = 55.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "pcCapi"
        ingIni     = viSMLV * 3
        ingFin     = viSMLV * 10
        porcentaje = 50.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "pcCapi"
        ingIni     = viSMLV * 10
        ingFin     = viSMLV * 20
        porcentaje = 45.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "pcCapi"
        ingIni     = viSMLV * 20
        ingFin     = viSMLV * 300
        porcentaje = 40.

/* CON Personas a Cargo Resto de Pais*/

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "pcResp"
        ingIni     = viSMLV
        ingFin     = viSMLV * 3
        porcentaje = 50.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "pcResp"
        ingIni     = viSMLV * 3
        ingFin     = viSMLV * 10
        porcentaje = 45.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "pcResp"
        ingIni     = viSMLV * 10
        ingFin     = viSMLV * 20
        porcentaje = 40.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "pcResp"
        ingIni     = viSMLV * 20
        ingFin     = viSMLV * 300
        porcentaje = 35.

/* SIN Personas a Cargo Ciudades Grandes*/
    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "spCapi"
        ingIni     = viSMLV
        ingFin     = viSMLV * 3
        porcentaje = 50.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "spCapi"
        ingIni     = viSMLV * 3
        ingFin     = viSMLV * 10
        porcentaje = 45.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "spCapi"
        ingIni     = viSMLV * 10
        ingFin     = viSMLV * 20
        porcentaje = 40.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "spCapi"
        ingIni     = viSMLV * 20
        ingFin     = viSMLV * 300
        porcentaje = 35.

/* SIN Personas a Cargo Resto del Pais*/
    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "spResp"
        ingIni     = viSMLV
        ingFin     = viSMLV * 3
        porcentaje = 45.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "spResp"
        ingIni     = viSMLV * 3
        ingFin     = viSMLV * 10
        porcentaje = 40.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "spResp"
        ingIni     = viSMLV * 10
        ingFin     = viSMLV * 20
        porcentaje = 35.

    CREATE TTParamCiudad.
    UPDATE 
        tipo       = "spResp"
        ingIni     = viSMLV * 20
        ingFin     = viSMLV * 300
        porcentaje = 30.

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
  DISPLAY W_Telef W_PorEnd W_NomAsesor Cmb_TipoProductos Cmb_Productos 
          W_NomTitular Rs_PerPago Tasa_Nominal Cmb_PerLiquidacion 
          Interes_PorPagar AS_Tasa W_SdoApor W_ReqPtmo W_FaltApor W_TManual 
          W_PromedDD 
      WITH FRAME F_Asesoria IN WINDOW wWin.
  IF AVAILABLE Asesoria THEN 
    DISPLAY Asesoria.Val_IngresosMes Asesoria.Val_EgresosMes Asesoria.Num_Asesoria 
          Asesoria.Fec_Asesoria Asesoria.Usuario Asesoria.Nit Asesoria.Monto 
          Asesoria.Cuota Asesoria.Plazo Asesoria.For_Liquidacion Asesoria.Tasa 
          Asesoria.Id_Resultado Asesoria.Clase_Producto Asesoria.Fec_Apertura 
      WITH FRAME F_Asesoria IN WINDOW wWin.
  ENABLE Asesoria.Val_IngresosMes Asesoria.Val_EgresosMes Asesoria.Num_Asesoria 
         Asesoria.Fec_Asesoria BUTTON-1 BUTTON-2 BUTTON-3 Asesoria.Nit 
         Asesoria.Monto Asesoria.Cuota Rs_PerPago Asesoria.Plazo 
         Asesoria.For_Liquidacion Asesoria.Tasa Cmb_PerLiquidacion Btn_Ingresar 
         B_Proyeccion Btn_Cancelar BtnDone Asesoria.Id_Resultado BUTTON-95 
         Asesoria.Clase_Producto Asesoria.Fec_Apertura W_TManual RECT-274 
         RECT-275 RECT-276 RECT-277 RECT-281 RECT-284 RECT-300 RECT-301 
         RECT-316 
      WITH FRAME F_Asesoria IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Asesoria}
  DISPLAY Cmb_Usuario W_FecIni W_FecFin Rs_Inf 
      WITH FRAME F_Impres IN WINDOW wWin.
  ENABLE Cmb_Usuario W_FecIni W_FecFin Rs_Inf BUTTON-167 Btn_Salir RECT-315 
      WITH FRAME F_Impres IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Impres}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE FecOk1 wWin 
PROCEDURE FecOk1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFI VAR W_Ano    AS INTEG FORM "9999".
   DEFI VAR W_DiaIni AS INTEG FORM "99". 
   
   ASSIGN W_DiaIni = DAY(DATE(Asesoria.Fec_Apertura:SCREEN-VALUE IN FRAME F_Asesoria))
          W_Ano    = YEAR(W_Fec).
   
   IF W_Mes GT 12 THEN
      ASSIGN W_Mes = W_Mes - 12
             W_Ano = YEAR(W_Fec) + 1.
             
   IF W_DiaIni GT 30 THEN
      W_DiaIni = 30.

   IF W_DiaIni GT 28 AND W_Mes EQ 2 THEN
      W_DiaIni = 28.
      
   ASSIGN W_Fec = DATE (W_Mes,W_DiaIni,W_Ano).   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getIndicador wWin 
PROCEDURE getIndicador :
/*------------------------------------------------------------------------------
  Purpose:     Indicadores para pdctos de crèdito.  
------------------------------------------------------------------------------*/
/*   DEFINE VARIABLE wtotaportes AS DECIMAL INITIAL 0 .
    DEFINE VARIABLE Periodo     AS INTEGER INITIAL 12 FORMAT "999".
    DEFINE VARIABLE viFidel AS INTEGER     NO-UNDO.


    ASSIGN viFidel = INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Asesoria,1,3)).
    IF viFidel EQ 874 THEN 
        ASSIGN viFidel = 574.
    
    FIND Pro_Creditos WHERE 
         Pro_Creditos.Tip_Credito EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME F_Asesoria,1,1)) AND
         Pro_Creditos.Cod_Credito EQ INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Asesoria,1,3))
         NO-LOCK NO-ERROR.
    IF Pro_Creditos.Id_Tasa EQ 1 THEN DO:
        FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Creditos.Cod_Tasa AND
                                     Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Indicadores) THEN DO:
            MESSAGE "No exite un indicadores para la linea" SKIP
                "de producto de crédito. Consulte con el Administrador" SKIP
                "del sistema acerca de esta inconsistencia" 
                VIEW-AS ALERT-BOX ERROR TITLE "Configuración del Indicador".
            /*APPLY "ENTRY" TO Asesoria.Monto  IN FRAME FRAME-PreScoring.*/
            RETURN NO-APPLY.
        END.
    
        IF NOT Indicadores.Rangos THEN DO:
            IF Indicadores.Tasa EQ 0 THEN DO:
                MESSAGE "El indicador tiene tasa en 0" SKIP
                "no se permite crear la asesorìa con esta tasa" SKIP
                    VIEW-AS ALERT-BOX ERROR.
                RETURN.
            END. /*IF Indicadores.Tasa EQ 0*/
            ASSIGN FTEfectiva:SCREEN-VALUE = STRING(Indicadores.Tasa).
        END. /*IF NOT Indicadores.Rangos*/
        ELSE DO:
/*             RUN Hallar_RangosInd. */
           IF INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Asesoria,1,3)) NE 574 THEN DO: 
          IF viFidel NE 574 THEN DO:
            FIND FIRST Ran_Intereses WHERE 
                   Ran_Intereses.Indicador EQ Indicador.Indicador AND
                   DECIMAL(Asesoria.Monto:SCREEN-VALUE IN FRAME FRAME-PreScoring)  GE Ran_Intereses.Val_Inicial AND
                   DECIMAL(Asesoria.Monto:SCREEN-VALUE IN FRAME FRAME-PreScoring)  LE Ran_Intereses.Val_Final AND
                   (Dias GE Ran_Intereses.Pla_Inicial  AND Dias LE Ran_Intereses.Pla_Final) AND
                   Ran_Interes.Estado EQ 1 NO-LOCK NO-ERROR.
          END.
          ELSE DO: /* POR APORTES*/
              FOR EACH ahorros WHERE Ahorros.nit = Asesoria.Nit:SCREEN-VALUE  IN FRAME FRAME-PreScoring AND 
                         Ahorros.Tip_ahorro = 4 AND
                         Ahorros.estado = 1 NO-LOCK :
                    wtotaportes = wtotaportes + ahorros.sdo_disponible.
              END.
              IF wtotaportes LE 0 THEN DO:
                 MESSAGE "Esta persona no tiene Aportes,  por tanto no puede tomar esta linea"
                 VIEW-AS ALERT-BOX INFO BUTTONS OK.
                 
                 RETURN NO-APPLY.
              END.
              FIND FIRST Ran_Intereses WHERE Ran_Intereses.Indicador EQ Indicador.Indicador 
                                         AND ( wtotaportes GE Ran_Intereses.Val_Inicial
                                         AND wtotaportes LE Ran_Intereses.Val_Final)
                                         AND Ran_Interes.Estado                     EQ 1
                                          NO-LOCK NO-ERROR.
          END. /* FIN BUSQUDA APORTES */

            IF AVAILABLE(Ran_Intereses) THEN
               FTEfectiva:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING(Indicadores.Tasa + Ran_Intereses.Puntos_Asoc).
            ELSE DO:
                MESSAGE "El Indicador no tiene creado un rango que contemple" SKIP
                        "los valores de Plazo y/o Monto digitados por el usuario" SKIP(1)
                        "Consulte con el Administrador" VIEW-AS ALERT-BOX INFO  TITLE "Configuración del Indicador".    
                ASSIGN FTEfectiva:SCREEN-VALUE IN FRAME FRAME-PreScoring = STRING(Indicadores.Tasa).
            END.
        END.
    END. /*IF Pro_Creditos.Id_Tasa EQ 1 */
    ELSE DO:
        IF FTEfectiva:SCREEN-VALUE LE "0" THEN DO:
            MESSAGE "EL producto de Crèdito permite que el asesor" SKIP
                "entre la tasa para la asesorìa." VIEW-AS ALERT-BOX INFORMATION.
            ASSIGN FTasa:SENSITIVE = YES.
            APPLY "Entry" TO FTasa.
            RETURN NO-APPLY.
        END. /*IF AS_Tasa:SCREEN-VALUE LE "0" */
    END. /*ELSE DO:*/
    
    /* Hallamos tasa Nominal */

    RUN EFNV IN W_ManFin  (INPUT (DECIMAL(FTEfectiva:SCREEN-VALUE IN FRAME FRAME-PreScoring) / 100), 
                           Periodo, 
                           OUTPUT Tas_Nominal).
    ASSIGN Tas_Nominal = ((Tas_Nominal * Periodo) * 100).
     FTasa:SCREEN-VALUE = STRING(Tas_Nominal / 12). 
    ASSIGN FTasa:SCREEN-VALUE = STRING(Tas_Nominal).
  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE getPorcenCiudad wWin 
PROCEDURE getPorcenCiudad :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*  DEFINE INPUT  PARAMETER ipTingr AS INTEGER     NO-UNDO.
    DEFINE OUTPUT PARAMETER opPorcent AS DECIMAL   NO-UNDO.

    ASSIGN 
        vcTpParamCiu = ""
        vlOk = FALSE.
    FIND Clientes WHERE Clientes.Nit EQ Asesoria.Nit:SCREEN-VALUE IN FRAME FRAME-PreScoring NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN DO:
        DO viCnt = 1 TO 13:
            IF vcCiudad[viCnt] EQ Clientes.Agencia THEN
                ASSIGN vlOk = TRUE.
        END.
    END.
    IF vlOk THEN DO: /* Cliente en agencia de Ciudad Grande */
        IF INTEGER(FPersCargo:SCREEN-VALUE IN FRAME FRAME-PreScoring) > 0 THEN 
            ASSIGN vcTpParamCiu = "pcCapi".
        ELSE
            ASSIGN vcTpParamCiu = "spCapi".
    END.
    ELSE DO: /* Es cliente en oficina de Ciudad Resto Pais*/
        IF INTEGER(FPersCargo:SCREEN-VALUE IN FRAME FRAME-PreScoring) > 0 THEN 
            ASSIGN vcTpParamCiu = "pcResp".
        ELSE
            ASSIGN vcTpParamCiu = "spResp".
    END.

    FIND FIRST TTParamCiudad WHERE TTParamCiudad.tipo EQ vcTpParamCiu AND 
        TTParamCiudad.ingIni < ipTingr AND ipTingr <= TTParamCiudad.ingFin
        NO-LOCK NO-ERROR.
    IF AVAILABLE(TTParamCiudad) THEN DO:
        ASSIGN opPorcent = TTParamCiudad.porcentaje.
    END.
    ELSE DO:
        MESSAGE "Parámetro Cliente en Ciudad no encontrado" SKIP
            "en procedimiento Parametro Ciudad"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
    END.
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_RangosInd wWin 
PROCEDURE Hallar_RangosInd :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME F_Asesoria: END.
  
  FIND FIRST Ran_Intereses WHERE 
            /* Ran_Intereses.Agencia                  EQ W_Agencia
         AND*/ Ran_Intereses.Indicador                EQ Indicador.Indicador 
         AND (DECIMAL(Asesoria.Monto:SCREEN-VALUE)  GE Ran_Intereses.Val_Inicial
         AND  DECIMAL(Asesoria.Monto:SCREEN-VALUE)  LE Ran_Intereses.Val_Final)
         AND (Dias                                   GE Ran_Intereses.Pla_Inicial  
         AND  Dias                                   LE Ran_Intereses.Pla_Final) 
         AND Ran_Interes.Estado                     EQ 1 NO-LOCK NO-ERROR.
  IF AVAILABLE(Ran_Intereses) THEN DO:
     AS_Tasa:SCREEN-VALUE = STRING(Indicadores.Tasa + Ran_Intereses.Puntos_Asoc).
  END.
  ELSE DO: 
    MESSAGE "El Indicador no tiene creado un rango que contemple" SKIP
           "los valores de Plazo y/o Monto digitados por el usuario" SKIP(1)
           "Consulte con el Administrador" VIEW-AS ALERT-BOX ERROR  TITLE "Configuración del Indicador".    
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa wWin 
PROCEDURE Hallar_Tasa :
DEFINE INPUT PARAMETER  W_Indicador LIKE Indicadores.Indicador.
  DEFINE INPUT PARAMETER  W_Monto     LIKE Ahorros.Monto.
  DEFINE INPUT PARAMETER  W_Plazo     LIKE Ahorros.Plazo.
  DEFINE OUTPUT PARAMETER W_TasaW     LIKE Indicadores.Tasa.
  DEFINE OUTPUT PARAMETER W_PuntosW   LIKE Ran_Intereses.Puntos.
  FIND Indicadores WHERE
       Indicadores.Indicador EQ W_Indicador AND
       Indicadores.FecVcto   GT TODAY       AND
       Indicadores.Estado    EQ 1           NO-LOCK NO-ERROR.
  IF AVAILABLE Indicadores THEN DO:
     IF NOT Indicadores.Rangos THEN DO:
        W_TasaW = Indicadores.Tasa.
        RUN Convertir_Tasa_Periodica (INPUT W_Monto, INPUT W_TasaW).
     END.
     ELSE DO:
        FIND FIRST Ran_Intereses 
             WHERE /*Ran_Intereses.Agencia    EQ W_Agencia                  AND*/
                   Ran_Intereses.Indicador  EQ W_Indicador                AND
                   W_Monto                  GE Ran_Intereses.Val_Inicial  AND
                   W_Monto                  LE Ran_Intereses.Val_Final    AND
                   W_plazo                  GE Ran_Intereses.Pla_Inicial  AND
                   W_Plazo                  LE Ran_Intereses.Pla_Final    AND 
                   Ran_Interes.Estado       EQ 1 NO-LOCK NO-ERROR.
        IF AVAILABLE Ran_Intereses THEN DO:
           ASSIGN W_TasaW   = Indicadores.Tasa + Ran_Intereses.Puntos_Asoc.
           
           /*ASSIGN W_Puntos = Ran_Intereses.Pun_Negociables.  */
           RUN Convertir_Tasa_Periodica (INPUT W_Monto, INPUT W_TasaW).
        END.
        ELSE DO:
           RUN MostrarMensaje IN W_Manija (INPUT 257, OUTPUT W_Rpta).
           ASSIGN W_TasaW   = 0
                  W_PuntosW = 0.
        END.
/**/
     END.
  END.
  ELSE DO:
     MESSAGE "No se ha encontrado un Indicador Valido para" SKIP
             "el producto: " Cmb_Productos:SCREEN-VALUE IN FRAME F_Asesoria SKIP
             "Rectifique o cree el indicador requerido" VIEW-AS ALERT-BOX ERROR.
     APPLY 'choose' TO Btn_Cancelar IN FRAME F_Asesoria.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_TasaNominal wWin 
PROCEDURE Hallar_TasaNominal :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE Periodo     AS INTEGER FORMAT "999".

Periodo = 12.

DO WITH FRAME F_Asesoria:
   /*CASE INTEGER(SUBSTRING(Cmb_PerPago:SCREEN-VALUE,1,1)):
     WHEN 1 THEN Periodo = 52.
     WHEN 2 THEN Periodo = 36.
     WHEN 3 THEN Periodo = 24.
     WHEN 4 THEN Periodo = 12.
   END CASE.  */
   
   /*IF Solicitud.FOR_Interes:SCREEN-VALUE EQ "1" THEN*/
   RUN EFNV IN W_ManFin  (INPUT (DECIMAL(AS_Tasa:SCREEN-VALUE) / 100), Periodo, OUTPUT Tas_Nominal).
   /*ELSE
     RUN EFNA IN W_ManFin (INPUT (DECIMAL(AS_Tasa:SCREEN-VALUE) / 100), Periodo, OUTPUT Tas_Nominal).*/
      
   ASSIGN Tas_Nominal                = ((Tas_Nominal * Periodo) * 100)
          Tasa_Nominal:SCREEN-VALUE   = STRING(Tasa_Nominal).
 END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Hallar_Tasa_Periodo wWin 
PROCEDURE Hallar_Tasa_Periodo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VAR Tas_Periodo LIKE Solicitud.Tasa.
DO WITH FRAME F_Asesoria:
  RUN HallarTasPer IN W_ManFin 
     (INPUT 4,
      INPUT DECIMAL(AS_Tasa:SCREEN-VALUE),1,
      OUTPUT Tas_Periodo).
  ASSIGN Tas_Periodo                = (Tas_Periodo * 100)
         Asesoria.Tasa:SCREEN-VALUE = STRING(Tas_Periodo).
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HCMONTO wWin 
PROCEDURE HCMONTO :
/***************** HALLAR CUOTA DADA TASA, MONTO, PLAZO ***********************
   CREADA: GioCam   Nov. 16 / 2007
   DESCRIPCION : Devuelve Cuota - preScoring.
   FORMULA     :                            ((1 + (TASA/12))^Plazo) -1
                        CUOTA = MONTO / ---------------------------------
                                        (1 + (TASA/12))^PLAZO) * (TASA/12)
  *************************************************************************/
      
      DEFINE INPUT  PARAMETER P_Monto     AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
      DEFINE INPUT  PARAMETER P_Tasa      AS DECIMAL FORMAT ">>9.9999999".
      DEFINE INPUT  PARAMETER P_Plazo     AS INTEGER.                  
      DEFINE OUTPUT PARAMETER P_Resultado AS DECIMAL FORMAT "->>,>>>,>>>,>>9" INITIAL 0.

      ASSIGN P_Tasa = P_Tasa / 100.

    ASSIGN P_Resultado = P_Monto /  (
                                    (EXP((1 + (P_Tasa / 12)), P_Plazo) - 1) /
                                    (EXP((1 + (P_Tasa / 12)), P_Plazo) * (P_Tasa / 12))
                                    ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE HMontSug wWin 
PROCEDURE HMontSug :
/***************** HALLAR CUOTA DADA TASA, MONTO, PLAZO ***********************
   CREADA: GioCam   Nov. 16 / 2007
   DESCRIPCION : Devuelve Cuota - preScoring.
   FORMULA     :                            ((1 + (TASA/12))^Plazo) -1
                        CUOTA = MONTO / ---------------------------------
                                        (1 + (TASA/12))^PLAZO) * (TASA/12)
  *************************************************************************/
      
      DEFINE INPUT  PARAMETER P_CtaMax    AS DECIMAL FORMAT "->>>,>>>,>>>,>>9". /*Cuota Maxima*/
      DEFINE INPUT  PARAMETER P_Tasa      AS DECIMAL FORMAT ">>9.9999999".
      DEFINE INPUT  PARAMETER P_Plazo     AS INTEGER.                  
      DEFINE OUTPUT PARAMETER P_Resultado AS DECIMAL FORMAT "->>,>>>,>>>,>>9" INITIAL 0.


      ASSIGN P_Tasa = P_Tasa / 100.
      ASSIGN P_Resultado = (
                            (EXP((1 + (P_Tasa / 12)), P_Plazo) - 1) /
                            ((EXP((1 + (P_Tasa / 12)), P_Plazo) * (P_Tasa / 12))) * P_CtaMax
                           ).


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Usuario wWin 
PROCEDURE Informe_Usuario :
/*------------------------------------------------------------------------------
  Purpose:   Informe Asosorias por usuario.   
  Notes:     Mayo 23/05 GAER.  
------------------------------------------------------------------------------*/
  DEFI VAR W_Tot LIKE Asesoria.Monto INIT 0.
  DEFI VAR W_Nro AS INTEG FORM "9999999" INIT 0.

  {Incluido\RepEncabezado.i}

  W_Reporte  = "Informe de Asesorías por Usuario : " + Cmb_Usuario:SCREEN-VALUE IN FRAME F_Impres +
               " Entre Fechas : " + STRING(W_FecIni,"99/99/9999") + " Y " + STRING(W_FecFin,"99/99/9999").

  VIEW FRAME F-Encabezado.

  FOR EACH Asesoria WHERE Asesoria.Fec_Asesoria GE W_FecIni
                      AND Asesoria.Fec_Asesoria LE W_FecFin
                      AND Asesoria.Usuario      EQ W_UsuImp NO-LOCK
                          BREAK BY Asesoria.Agencia BY Asesoria.Fec_Asesoria:
      FIND Clientes WHERE Clientes.Nit EQ Asesoria.Nit NO-LOCK NO-ERROR.
      ASSIGN W_Tot = W_Tot + Asesoria.Monto
             W_Nro = W_Nro + 1.

      DISPLAY Asesoria.Agencia        LABEL "Ag."
              Asesoria.Fec_Asesoria   LABEL "Fec-Asesoría"
              Asesoria.Nit            LABEL "Ced./Nit" 
              trim(Clientes.Nombre) + " " + trim(Clientes.Apellido1) + " " + trim(Clientes.Apellido2) 
                                      LABEL "Nombre del Cliente" FORMAT "X(35)"
              Clientes.Tel_Comercial  LABEL "Teléfono"
              Asesoria.Clase_Producto LABEL "A/C" 
              Asesoria.Cod_Producto   LABEL "Pdcto"
              Asesoria.Monto          LABEL "Monto-Asesorado"
              Asesoria.Plazo          LABEL "Plazo"
              Asesoria.Cuota          LABEL "Valor-Cuota" SKIP(0)
          WITH DOWN WIDTH 200 FRAME FDet NO-BOX NO-LABELS USE-TEXT STREAM-IO.
              
  END.

  DISPLAY "                          --------------------------------" SKIP
          "Número y valor Asesorado :"
          W_Nro NO-LABEL
          " "
          W_Tot NO-LABEL.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables wWin 
PROCEDURE Inicializar_Variables :
DO WITH FRAME F_Asesoria:
   ASSIGN Asesoria.Num_Asesoria:SCREEN-VALUE IN FRAME F_Asesoria = ""
          Asesoria.Fec_Asesoria:SCREEN-VALUE IN FRAME F_Asesoria = ""
          Asesoria.Usuario:SCREEN-VALUE IN FRAME F_Asesoria      = ""
          W_NomAsesor:SCREEN-VALUE      IN FRAME F_Asesoria      = ""
          Asesoria.Nit:SCREEN-VALUE     IN FRAME F_Asesoria      = ""
          W_NomTitular:SCREEN-VALUE     IN FRAME F_Asesoria      = ""
          Asesoria.Monto:SCREEN-VALUE   IN FRAME F_Asesoria      = ""
          Asesoria.Cuota:SCREEN-VALUE   IN FRAME F_Asesoria      = ""
          Asesoria.Plazo:SCREEN-VALUE   IN FRAME F_Asesoria      = ""
          Asesoria.Tasa:SCREEN-VALUE    IN FRAME F_Asesoria      = ""
          Asesoria.FOR_Liquidacion:SCREEN-VALUE IN FRAME F_Asesoria = "2"
          AS_Tasa:SCREEN-VALUE          IN FRAME F_Asesoria      = ""
          Tasa_Nominal:SCREEN-VALUE     IN FRAME F_Asesoria      = ""
          Interes_PorPagar:SCREEN-VALUE IN FRAME F_Asesoria      = ""
          Asesoria.Fec_Apertura:SCREEN-VALUE IN FRAME F_Asesoria = STRING(W_Fecha)
          Asesoria.Val_EgresosMes:SCREEN-VALUE IN FRAME F_Asesoria  = "0"   
          Asesoria.Val_IngresosMes:SCREEN-VALUE IN FRAME F_Asesoria = "0"   
          Asesoria.Val_EgresosMes:SENSITIVE     IN FRAME F_Asesoria = FALSE 
          Asesoria.Val_IngresosMes:SENSITIVE    IN FRAME F_Asesoria = FALSE.
          
   FOR EACH TmpAho: DELETE TmpAho. END.
END.

DO WITH FRAME F_Impres: END.

ASSIGN W_FecIni:SCREEN-VALUE = STRING(W_Fecha)
       W_FecIni
       W_FecFin:SCREEN-VALUE = STRING(W_Fecha)  .
       W_FecFin.                                

FOR EACH Usuarios NO-LOCK BY Usuarios.Usuario:
    Cmb_Usuario:ADD-LAST(STRING(Usuarios.Usuario,"X(4)") + " - " + STRING(Usuarios.Nombre,"X(35)")).
END.

/*Inicio variable PreScoring*/
/*ASSIGN  Asesoria.Nit:SCREEN-VALUE IN FRAME Frame-PreScoring = ""
        W_NomTitular:SCREEN-VALUE IN FRAME Frame-PreScoring = "".

FIND FIRST indicadores WHERE Indicadores.indicador = 21 NO-LOCK NO-ERROR.
ASSIGN viSMLV = Indicadores.Valor.

  */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
RUN SUPER.
  ASSIGN Cmb_TipoProductos:LIST-ITEMS IN FRAME F_Asesoria = ""
         W_Ok = Cmb_TipoProductos:ADD-LAST("2 - Contractual")
         W_Ok = Cmb_TipoProductos:ADD-LAST("3 - A Termino")
         Cmb_TipoProductos:SCREEN-VALUE = Cmb_TipoProductos:ENTRY(1).
         
  Asesoria.FOR_Liquidacion:SCREEN-VALUE IN FRAME F_Asesoria = "2".
  APPLY 'value-changed' TO Asesoria.Clase_Producto IN FRAME F_Asesoria.
  
  RUN Inicializar_Variables.
  DISABLE {&List-1} WITH FRAME F_Asesoria.

  APPLY "Value-Changed" TO Asesoria.Clase_Producto.
  
  APPLY "CHOOSE" TO Btn_Ingresar.

  APPLY "ENTRY" TO Asesoria.Nit.

  /* PreScoring */
  
 /* ASSIGN FPersCargo:HIDDEN IN FRAME frame-PreScoring = TRUE.  
  RUN creaTTParamCiudad. /* rutina para cargar parametros porcentajes para calculo prescoring por Caja */
   
  RETURN NO-APPLY.
   */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidar wWin 
PROCEDURE Liquidar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE PlazoW LIKE Solicitud.Plazo.
  DEFINE VARIABLE TotPtW LIKE Solicitud.Monto.
  DEFINE VARIABLE CuotaW LIKE Solicitud.Cuota.
  DEFINE VARIABLE TasaW  LIKE Solicitud.Tasa.
  DEFINE VARIABLE TinteW LIKE Solicitud.Monto.
  
  DEFINE VAR W_Resultado AS INTEGER INITIAL 0.
  DEFINE VAR Wimp_Coop   AS INTEGER INITIAL 0.
     
  DO WITH FRAME F_Asesoria:
      ASSIGN CuotaW   = 0
             TotPtW   = DECIMAL(Asesoria.Monto:SCREEN-VALUE) 
             TasaW    = DECIMAL(AS_Tasa:SCREEN-VALUE)
             PlazoW   = DECIMAL(Asesoria.Plazo:SCREEN-VALUE).
             
      IF Pro_Creditos.Id_PerGracia AND Pro_Creditos.Dia_Gracia GT 0 THEN
         ASSIGN /*PlazoW = PlazoW - 1*/
                TotPtW = TotPtW + (((TotPtW * Tas_Nominal / 360) * Pro_Creditos.Dia_Gracia) / 100).
             
      RUN Calculo_Cuota.R (INPUT-OUTPUT TotPtw, INPUT-OUTPUT PlazoW, INPUT-OUTPUT CuotaW,
                           INPUT-OUTPUT TInteW, INPUT-OUTPUT TasaW, INPUT 0,
                           INPUT 0,INPUT Rs_PerPago,INPUT 3,
                           INPUT 1,INPUT 1). 
      IF CuotaW LE 0 THEN DO:
         MESSAGE "El Valor de la cuota debe ser mayor que cero. Rectifique!" VIEW-AS ALERT-BOX ERROR.         
         RETURN.
      END.
      
      ASSIGN Asesoria.Cuota:SCREEN-VALUE = STRING(CuotaW).
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_CmbProductos wWin 
PROCEDURE Llenar_CmbProductos :
DO WITH FRAME F_Asesoria:
   IF Asesoria.Clase_Producto:SCREEN-VALUE EQ "1" THEN DO: /*ahorros*/
       Cmb_Productos:LIST-ITEMS = "".
       FOR EACH Pro_Ahorros WHERE 
                Pro_Ahorros.Tip_Ahorro EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) AND
                Pro_Ahorros.Estado       EQ 1
                NO-LOCK BREAK BY Pro_Ahorros.Tip_Ahorro:
         W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).
         IF FIRST-OF(Pro_Ahorros.Tip_Ahorro) THEN
            Cmb_Productos:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
       END.
   END.
   ELSE DO:
       Cmb_Productos:LIST-ITEMS = "".
       FOR EACH Pro_Creditos WHERE 
                Pro_Creditos.Tip_Credito EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)) AND
                Pro_Creditos.Estado       EQ 1
                NO-LOCK BREAK BY Pro_Creditos.Tip_Credito:
         W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
         IF FIRST-OF(Pro_Creditos.Tip_Credito) THEN
            Cmb_Productos:SCREEN-VALUE = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
       END.
   END.
   
   APPLY 'value-changed' TO Cmb_Productos.
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MostrarAportes wWin 
PROCEDURE MostrarAportes :
/*------------------------------------------------------------------------------
  Purpose:   Halla Aportes x Creditos y Promedio Ah.a la vista  
  Notes:     Junio 8/05 GAER.
------------------------------------------------------------------------------*/
  FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ 
                           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Asesoria,1,3)) 
                          NO-LOCK NO-ERROR.

  

  RUN Halla_CapitAporte.R (INPUT  Pro_Creditos.Cod_Credito,
                                  Asesoria.Nit:SCREEN-VALUE, 
                                  DEC(Asesoria.Monto:SCREEN-VALUE),0,
                            OUTPUT W_SdoApor, 
                            OUTPUT W_ReqPtmo,
                            OUTPUT W_PromedDD,
                            OUTPUT W_FaltApor).


    ASSIGN W_FaltApor:SCREEN-VALUE = STRING(W_FaltApor)
         W_ReqPtmo:SCREEN-VALUE  = STRING(W_ReqPtmo)
         W_PromedDD:SCREEN-VALUE = STRING(W_PromedDD)
         W_SdoApor:SCREEN-VALUE  = STRING(W_SdoApor).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE MostrarAportes_YaNo wWin 
PROCEDURE MostrarAportes_YaNo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:     Comentariado Junio 8/05 GAER , Reemplazado por Proced.MostrarAportes. 
------------------------------------------------------------------------------*/
 /*  FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ 
                           INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE IN FRAME F_Asesoria,1,3)) 
              NO-LOCK NO-ERROR.

   ASSIGN W_SdoApor:SCREEN-VALUE    = "0"
          W_ReqPtmo:SCREEN-VALUE    = "0"
          W_FaltApor:SCREEN-VALUE   = "0"
          W_PromedDD:SCREEN-VALUE   = "0"
          W_PromedDD = 0
          W_SdoApor  = 0
          W_ReqPtmo  = 0
          W_FaltApor = 0.

   FOR EACH Ahorros WHERE Ahorros.Nit        EQ Asesoria.Nit:SCREEN-VALUE                        
                      AND Ahorros.Tip_Ahorro EQ 4 NO-LOCK:              
       ASSIGN W_SdoApor = W_SdoApor + (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)                       
              W_SdoApor:SCREEN-VALUE = STRING(W_SdoApor).                                            
   END.

   FOR EACH Ahorros WHERE Ahorros.Nit                              EQ Asesoria.Nit:SCREEN-VALUE                        
                      AND Ahorros.Tip_Ahorro                       EQ 1
                      AND (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje) GT 0
                      AND (W_Fecha - Ahorros.Fec_Apertura)         GE 90 NO-LOCK:              
       RUN Halla_Promedio.   /*Al final de este mismo procedimiento*/                                            
   END.

   IF  INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) NE 7
   AND INTEGER(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)) NE 20 THEN DO:
      FIND FIRST Indicadores WHERE Indicadores.Indicador EQ 18 
                               AND Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.
      IF NOT AVAIL(Indicadores) THEN DO:                                   
         MESSAGE "El Indicador 18 SMMLV no existe...Rectifique por favor"       
                         VIEW-AS ALERT-BOX ERROR.                               
         RETURN ERROR.                                                          
      END. 

      IF DEC(Asesoria.Monto:SCREEN-VALUE) LE (34 * Indicadores.Valor) THEN DO: /*Hasta 34 SMMLV*/
         ASSIGN W_ReqPtmo               = DEC(Asesoria.Monto:SCREEN-VALUE) / 20
                W_ReqPtmo:SCREEN-VALUE  = STRING(W_ReqPtmo).

         IF (W_SdoApor * 20) LT DEC(Asesoria.Monto:SCREEN-VALUE) THEN
            ASSIGN W_FaltApor              = (DEC(Asesoria.Monto:SCREEN-VALUE) / 20) - W_SdoApor 
                   W_FaltApor:SCREEN-VALUE = STRING(W_FaltApor).                   
      END.
      ELSE IF DEC(Asesoria.Monto:SCREEN-VALUE) LE (67 * Indicadores.Valor) THEN DO: /*Hasta 67 SMMLV*/
         ASSIGN W_ReqPtmo               = DEC(Asesoria.Monto:SCREEN-VALUE) / 25
                W_ReqPtmo:SCREEN-VALUE  = STRING(W_ReqPtmo).

         IF (W_SdoApor * 25) LT DEC(Asesoria.Monto:SCREEN-VALUE) THEN
            ASSIGN W_FaltApor              = (DEC(Asesoria.Monto:SCREEN-VALUE) / 25) - W_SdoApor
                   W_FaltApor:SCREEN-VALUE = STRING(W_FaltApor).                   
      END.
      ELSE IF DEC(Asesoria.Monto:SCREEN-VALUE) GT (67 * Indicadores.Valor) THEN DO: /*Superior a 67 SMMLV*/
         ASSIGN W_ReqPtmo               = DEC(Asesoria.Monto:SCREEN-VALUE) / 30
                W_ReqPtmo:SCREEN-VALUE  = STRING(W_ReqPtmo).

         IF (W_SdoApor * 30) LT DEC(Asesoria.Monto:SCREEN-VALUE) THEN
            ASSIGN W_FaltApor              = (DEC(Asesoria.Monto:SCREEN-VALUE) / 30) - W_SdoApor 
                   W_FaltApor:SCREEN-VALUE = STRING(W_FaltApor).                   
      END.
   END.
   ELSE DO:
      ASSIGN W_ReqPtmo               = DEC(Asesoria.Monto:SCREEN-VALUE) * 1.10
             W_ReqPtmo:SCREEN-VALUE  = STRING(W_ReqPtmo).
      
      IF (W_SdoApor * .90) LT DEC(Asesoria.Monto:SCREEN-VALUE) THEN
          ASSIGN W_FaltApor              = (DEC(Asesoria.Monto:SCREEN-VALUE) * 1.10) - W_SdoApor
                 W_FaltApor:SCREEN-VALUE = STRING(W_FaltApor).
   END.
*/
END PROCEDURE.

/*--------Promedio Ult.trimestre Ah.a la vista*/
/*PROCEDURE Halla_Promedio:
    DEFI VAR SdoIni LIKE Mov_Ahorros.Sdo_Dispon INIT 0.
    DEFI VAR FecControl AS DATE INIT ?.
    DEFI VAR W_Dias     AS INTEG FORM "99999" INIT 0.
    DEFI VAR T_Dias     AS INTEG FORM "99999" INIT 0.

    FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Agencia      EQ Ahorros.Agencia      AND
                               Mov_Ahorros.Cod_Ahorro   EQ Ahorros.Cod_Ahorro   AND
                               Mov_Ahorros.Cue_Ahorros  EQ Ahorros.Cue_Ahorros  AND
                               Mov_Ahorros.Fecha + 91   EQ W_Fecha NO-LOCK
                      BY Mov_Ahorros.Hora:
        ASSIGN SdoIni     = Mov_Ahorros.Sdo_Dispon   /*Queda asignado el último de esta fecha*/
               W_PromedDD = Mov_Ahorros.Sdo_Dispon
               FecControl = Mov_Ahorros.Fecha.
    END.

    IF FecControl EQ ? THEN     /*No asignó el recorrido anterior*/
       FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Agencia   EQ Ahorros.Agencia      AND
                               Mov_Ahorros.Cod_Ahorro   EQ Ahorros.Cod_Ahorro   AND
                               Mov_Ahorros.Cue_Ahorros  EQ Ahorros.Cue_Ahorros  AND
                               Mov_Ahorros.Fecha + 90   LE W_Fecha NO-LOCK
                            BY Mov_Ahorros.Fecha BY Mov_Ahorros.Hora:
           ASSIGN SdoIni     = Mov_Ahorros.Sdo_Dispon   /*Queda asignado el último de la fecha respectiva*/
                  W_PromedDD = Mov_Ahorros.Sdo_Dispon
                  FecControl = Mov_Ahorros.Fecha.
    END.

    IF FecControl EQ ? THEN     /*No asignó el recorrido anterior*/
       FIND FIRST Mov_Ahorros WHERE Mov_Ahorros.Agencia EQ Ahorros.Agencia      AND
                               Mov_Ahorros.Cod_Ahorro   EQ Ahorros.Cod_Ahorro   AND
                               Mov_Ahorros.Cue_Ahorros  EQ Ahorros.Cue_Ahorros  AND
                               Mov_Ahorros.Fecha        GE W_Fecha - 90 NO-LOCK NO-ERROR.
    IF AVAIL(Mov_Ahorros) THEN
       ASSIGN SdoIni     = Mov_Ahorros.Sdo_Dispon   
              W_PromedDD = Mov_Ahorros.Sdo_Dispon                                                        
              FecControl = Mov_Ahorros.Fecha.                                                            

    IF FecControl LT (W_Fecha - 90) OR FecControl EQ ? THEN
       FecControl = (W_Fecha - 91).
    
    FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Agencia      EQ Ahorros.Agencia      AND
                               Mov_Ahorros.Cod_Ahorro   EQ Ahorros.Cod_Ahorro   AND
                               Mov_Ahorros.Cue_Ahorros  EQ Ahorros.Cue_Ahorros  NO-LOCK
                        BREAK  BY Mov_Ahorros.Fecha BY ROWID(Mov_Ahorros):
        IF LAST-OF(Mov_Ahorros.Fecha) AND (Mov_Ahorros.Fecha + 90) GE W_Fecha THEN DO:
           IF Mov_Ahorros.Fecha GT FecControl THEN
              ASSIGN W_Dias     = Mov_Ahorros.Fecha - FecControl - 1
                     T_Dias     = T_Dias + W_Dias
                     W_PromedDD = W_PromedDD + (W_Dias * SdoIni)
                     SdoIni     = Mov_Ahorros.Sdo_Dispon
                     FecControl = Mov_Ahorros.Fecha.

        END.
    END.

    IF T_Dias LT 90 THEN 
       ASSIGN W_Dias     = 90 - T_Dias
              W_PromedDD = W_PromedDD + (W_Dias * (Ahorros.Sdo_Dispon + Ahorros.Sdo_Canje)).

    ASSIGN W_PromedDD = W_PromedDD / 91.

    IF W_PromedDD GT 0 THEN
       ASSIGN W_PromedDD:SCREEN-VALUE IN FRAME F_Asesoria = STRING(W_PromedDD).
    ELSE ASSIGN W_PromedDD              = 0
                W_PromedDD:SCREEN-VALUE = "0".
END PROCEDURE.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Asesoria wWin 
PROCEDURE Mostrar_Asesoria :
DO WITH FRAME F_Asesoria:
  Asesoria.Clase_Producto:SCREEN-VALUE  = STRING(Asesoria.Clase_Producto).
  APPLY 'value-changed' TO Asesoria.Clase_Producto IN FRAME F_Asesoria.
  
  CASE Asesoria.Clase_Producto:
      WHEN 1 THEN DO:
          FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Asesoria.Cod_Producto NO-LOCK NO-ERROR.
          IF AVAILABLE Pro_Ahorros THEN DO:
             CASE Pro_Ahorros.Tip_Ahorro:
                 WHEN 2 THEN Cmb_TipoProductos:SCREEN-VALUE = "2 - Contractual".
                 WHEN 3 THEN Cmb_TipoProductos:SCREEN-VALUE = "3 - A Termino".
             END CASE.
             APPLY 'value-changed' TO Cmb_TipoProductos.
             Cmb_Productos:SCREEN-VALUE = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
          END.
      END.
      OTHERWISE DO: /*Credito o PreScoring*/
          FIND Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Asesoria.Cod_Producto NO-LOCK NO-ERROR.
          IF AVAILABLE Pro_Creditos THEN DO:
             Cmb_TipoProductos:SCREEN-VALUE = Cmb_TipoProductos:ENTRY(Pro_Creditos.Tip_Credito).
             Cmb_Productos:SCREEN-VALUE = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
          END.
      END.
  END CASE.
  
  FIND Clientes WHERE Clientes.Nit EQ Asesoria.Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Clientes THEN
     ASSIGN W_NomTitular:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     
  FIND Usuarios WHERE Usuarios.Usuario EQ Asesoria.Usuario NO-LOCK NO-ERROR.
  IF AVAILABLE Usuarios THEN
     W_NomAsesor:SCREEN-VALUE = Usuarios.Nombre.
     
  ASSIGN Asesoria.Cuota:SCREEN-VALUE           = STRING(Asesoria.Cuota)
         Asesoria.Fec_Asesoria:SCREEN-VALUE    = STRING(Asesoria.Fec_Asesoria)
         Asesoria.For_Liquidacion:SCREEN-VALUE = STRING(Asesoria.FOR_Liquidacion)
         Asesoria.Fec_Apertura:SCREEN-VALUE    = STRING(Asesoria.Fec_Apertura)
         Asesoria.Id_Resultado:SCREEN-VALUE    = STRING(Asesoria.Id_Resultado)
         Asesoria.Monto:SCREEN-VALUE           = STRING(Asesoria.Monto)
         Asesoria.Nit:SCREEN-VALUE             = Asesoria.Nit         
         Asesoria.Num_Asesoria:SCREEN-VALUE    = STRING(Asesoria.Num_Asesoria)
         Cmb_PerLiquidacion:SCREEN-VALUE       = Cmb_PerLiquidacion:ENTRY(Asesoria.Per_Liquidacion)
         Asesoria.Plazo:SCREEN-VALUE           = STRING(Asesoria.Plazo)
         Asesoria.Tasa:SCREEN-VALUE            = STRING(Asesoria.Tasa)
         Asesoria.Usuario:SCREEN-VALUE         = Asesoria.Usuario
         Asesoria.Val_IngresosMes:SCREEN-VALUE = string(Asesoria.Val_IngresosMes)
         Asesoria.Val_EgresosMes:SCREEN-VALUE  = STRING(Asesoria.Val_EgresosMes).

  /* ASSIGN 
       Asesoria.Nit:SCREEN-VALUE IN FRAME FRAME-PreScoring              = Asesoria.Nit
       Asesoria.Monto:SCREEN-VALUE IN FRAME FRAME-PreScoring            = STRING(Asesoria.Monto)
       FPlazo:SCREEN-VALUE IN FRAME FRAME-PreScoring                    = STRING(Asesoria.Plazo)
       FCtaValSol50:SCREEN-VALUE IN FRAME FRAME-PreScoring              = STRING(Asesoria.cuota)
       FTasa:SCREEN-VALUE IN FRAME FRAME-PreScoring                     = STRING(Asesoria.Tasa)
       Asesoria.Val_IngresosMes:SCREEN-VALUE IN FRAME FRAME-PreScoring  = string(Asesoria.Val_IngresosMes)
       FIngBruto:SCREEN-VALUE IN FRAME FRAME-PreScoring                 = string(Asesoria.Val_IngresosMes)
       FEgreMes50:SCREEN-VALUE IN FRAME FRAME-PreScoring                = STRING(Asesoria.Val_EgresosMes)
       FEgreNomina:SCREEN-VALUE IN FRAME FRAME-PreScoring               = STRING(Asesoria.Val_EgresosMes)
       W_NomTitular:SCREEN-VALUE IN FRAME FRAME-PreScoring              = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
    */     
   APPLY 'leave' TO Asesoria.Plazo.
 END.
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_General wWin 
PROCEDURE Mostrar_General :
DO WITH FRAME F_Asesoria:

     RUN Mostrar_Perliquidacion.
     IF INTEGER(Asesoria.For_Liquidacion:SCREEN-VALUE) EQ 1 THEN
        RUN NAEF IN W_ManFin (Ahorros.Tasa / 100,INPUT W_Per, OUTPUT W_Tasper).
     ELSE
        IF INTEGER(Asesoria.For_Liquidacion:SCREEN-VALUE) EQ 2 THEN
           RUN NVEF IN W_ManFin (Asesoria.Tasa / 100,INPUT W_Per,OUTPUT W_Tasper).
     AS_Tasa:SCREEN-VALUE IN FRAME F_Asesoria    = STRING(W_Tasper * 100).

     ASSIGN Tasa_Nominal:SCREEN-VALUE IN FRAME F_Asesoria = STRING(Asesoria.Tasa * W_Per).
            /*Interes_PorPagar:SCREEN-VALUE = STRING(DECIMAL(Asesoria.Tasa:SCREEN-VALUE) *
                                            Asesoria.Monto / 100).*/

    IF Pro_Ahorro.Id_PerLiquidacion EQ 1 THEN DO:
        Cmb_PerLiquidacion:SCREEN-VALUE       = Cmb_PerLiquidacion:ENTRY(INTEGER(Pro_Ahorros.Per_Liquidacion)).
        DISABLE Cmb_PerLiquidacion.
    END.
    ELSE DO:
        Cmb_PerLiquidacion:SCREEN-VALUE       = Cmb_PerLiquidacion:ENTRY(INTEGER(Asesoria.FOR_Liquidacion)).
        ENABLE Cmb_PerLiquidacion.
    END.
    
    IF Pro_Ahorros.Id_Forliquidacion EQ 1 THEN DO:
       ASSIGN Asesoria.For_Liquidacion:SCREEN-VALUE = STRING(Asesoria.For_Liquidacion).
       DISABLE Asesoria.FOR_Liquidacion.
    END.
    ELSE DO:
       ASSIGN Asesoria.For_Liquidacion:SCREEN-VALUE = STRING(Asesoria.For_Liquidacion).
       ENABLE Asesoria.FOR_Liquidacion.
    END.
    IF Pro_Ahorros.Id_PerLiquidacion EQ 2 THEN
       Cmb_PerLiquidacion:SCREEN-VALUE = Cmb_PerLiquidacion:ENTRY(1).
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_PerLiquidacion wWin 
PROCEDURE Mostrar_PerLiquidacion :
IF Asesoria.Per_Liquidacion EQ 0 THEN 
        ASSIGN W_DiaPer = 1     
               W_Per    = 365.
     ELSE
        IF Asesoria.Per_Liquidacion EQ 4 THEN  
           ASSIGN W_DiaPer = 30
                  W_Per    = 12.
        ELSE
           IF Asesoria.Per_Liquidacion EQ 5 THEN
              ASSIGN W_DiaPer = 60
                     W_Per    = 6.
           ELSE
              IF Asesoria.Per_Liquidacion EQ 6 THEN
                 ASSIGN W_DiaPer = 90
                        W_Per    = 4.
              ELSE
                 IF Asesoria.Per_Liquidacion EQ 8 THEN
                    ASSIGN W_DiaPer = 180 
                           W_Per    = 2.
                 ELSE
                    IF Asesoria.Per_Liquidacion EQ 9 THEN
                       ASSIGN W_DiaPer = 1
                              W_Per    = 360.
                    ELSE
                       IF Asesoria.Per_Liquidacion EQ 10 THEN 
                          ASSIGN W_DiaPer = Asesoria.Plazo
                                 W_Per    = (360 / Asesoria.Plazo).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Periodo wWin 
PROCEDURE Periodo :
DEFINE VAR W_PerLiq LIKE Asesoria.Per_Liquidacion.
    DEFI   VAR W_DiaCor AS INTEG FORM "99".
    
    ASSIGN W_PerLiq = INTEGER(SUBSTRING(Cmb_PerLiquidacion:SCREEN-VALUE IN FRAME F_Asesoria,1,2)).
           
    IF Pro_Ahorros.Tip_Ahorro EQ 2 THEN
       W_PerLiq = 2.
        
    CASE W_PerLiq:
        WHEN 1 THEN ASSIGN W_Fec = W_Fec + 1.
        WHEN 2 THEN ASSIGN W_Mes = MONTH(W_Fec) + 1.
        WHEN 3 THEN ASSIGN W_Mes = MONTH(W_Fec) + 3.
        WHEN 4 THEN ASSIGN W_Mes = MONTH(W_Fec) + 6.
        WHEN 5 THEN ASSIGN W_Mes = MONTH(W_Fecha) 
                           W_Fec = (W_Fec + 364).
        WHEN 6 THEN ASSIGN W_Fec = W_Fec + INTEGER(Asesoria.Plazo:SCREEN-VALUE IN FRAME F_Asesoria)
                           W_Mes = MONTH(W_Fec).
    END CASE.
    
    IF W_PerLiq NE 1 AND W_PerLiq NE 5 AND W_PerLiq NE 6 THEN
       RUN FecOk1.
    
/*    IF W_PerLiq NE 1 AND W_PerLiq NE 6 THEN DO:
       RUN Colocar_Dia (INPUT-OUTPUT W_Fec).
    END.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*
  -------------------------------------------------------------------*/
  DEFI VAR NomPdo AS CHAR FORM "X(40)".
  
  IF Rs_Inf EQ 1 THEN DO:
     RUN Informe_Usuario.

     RETURN.
  END.
  
  {Incluido\RepEncabezado.i}

  NomPdo = Asesoria.Plazo:LABEL IN FRAME F_Asesoria.
  IF Asesoria.Clase_Producto:SCREEN-VALUE EQ "1" THEN
     NomPdo = Cmb_PerLiquidacion:SCREEN-VALUE.

  DEFINE VAR W_NomTipoCliente AS CHARACTER FORMAT "X(20)".
  DEFINE VAR W_NomEstado      AS CHARACTER FORMAT "X(10)".
  DEFINE VAR W_NomTipoContrat AS CHARACTER FORMAT "X(14)".
  DEFINE VAR W_NomVinculo     AS CHARACTER FORMAT "X(20)".
  DEFINE VAR W_NomSexo        AS CHARACTER FORMAT "X(6)".
  DEFINE VAR W_NomRet         AS CHARACTER FORMAT "X(2)".
  DEFINE VAR W_NomGran        AS CHARACTER FORMAT "X(2)".

  W_Reporte   = "REPORTE   : ASESORIA DE PRODUCTOS - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  /*W_EncColumna = "CLIENTE: " + Asesoria.Nit:SCREEN-VALUE IN FRAME F_Asesoria + " - " + W_NomTitular:SCREEN-VALUE.*/
     
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.

    DO WITH FRAME F_Asesoria:
       DEFINE VAR W_ClaPro  AS CHARACTER FORMAT "X(10)".
       DEFINE VAR W_NomAge  AS CHARACTER FORMAT "X(50)".
       DEFINE VAR W_NomAse  AS CHARACTER FORMAT "X(50)".
       DEFINE VAR W_ForLiqA AS CHARACTER FORMAT "X(10)".

       W_NomAge = STRING(W_Agencia,"999") + " - " + W_Nom_Agencia.
       W_NomAse = string(Asesoria.Usuario:SCREEN-VALUE,"999") + " - " + W_NomAsesor:SCREEN-VALUE.

       IF Asesoria.FOR_Liquidacion:SCREEN-VALUE EQ "1" THEN W_ForLiqA = "Anticipado".
       ELSE W_ForliqA = "Vencido".

       IF Asesoria.Clase_Producto:SCREEN-VALUE EQ "1" THEN W_ClaPro = "Ahorros".
       ELSE W_ClaPro = "Créditos".

       DISPLAY "INFORMACIÓN GENERAL-------------------------------------------------------------------------------------------" AT 1 SKIP(1)
                "Agencia              : "            AT 1
                W_NomAge                             AT 25 FORMAT "X(50)"
                "Asesor               : "            AT 1
                W_NomAse                             AT 25 FORMAT "X(50)"
                "Clase de Producto    : "            AT 1
                W_ClaPro                             AT 25 FORMAT "X(20)"
                "Tipo de Producto     : "            AT 1
                Cmb_TipoProductos:SCREEN-VALUE       AT 25 FORMAT "X(40)"
                "Producto             : "            AT 1
                Cmb_Productos:SCREEN-VALUE           AT 25 FORMAT "X(40)"
                "Id. del Titular      : "            AT 1
                Asesoria.Nit:SCREEN-VALUE            AT 25 FORMAT "X(14)"
                "Nombre del Titular   : "            AT 1
                W_NomTitular:SCREEN-VALUE            AT 25 FORMAT "X(40)"
                "Teléfono"                           AT 1
                Clientes.Tel_Comercial               AT 25
                "Número de la Asesoria: "            AT 1
                Asesoria.Num_Asesoria:SCREEN-VALUE   AT 25 
                "Fecha de la Asesoria : "            AT 1
                Asesoria.Fec_Asesoria:SCREEN-VALUE   AT 25 FORMAT "X(10)" SKIP(1)
                "INFORMACION DEL PRODUCTO ASESORADO----------------------------------------------------------------------------" AT 1 
                "Forma de Liquidacion : "            AT 1
                W_ForLiqA                            AT 25 FORMAT "X(40)"
                "Periodo Liquidacion  : "            AT 1
                 NomPdo                              AT 25 FORMAT "X(40)"
                "Fecha de Apertura    : "            AT 1
                Asesoria.Fec_Apertura:SCREEN-VALUE   AT 25 FORMAT "X(40)"
                "Monto                : "            AT 1
                Asesoria.Monto:SCREEN-VALUE          AT 25 FORMAT "X(40)"
                "Cuota                : "            AT 1
                Asesoria.Cuota:SCREEN-VALUE          AT 25 FORMAT "X(40)"
                "Plazo                : "            AT 1
                Asesoria.Plazo:SCREEN-VALUE          AT 25 FORMAT "X(40)"
                "Tasa del Periodo     : "            AT 1
                Asesoria.Tasa:SCREEN-VALUE           AT 25 FORMAT "X(40)"
                "Tasa Nominal         : "            AT 1
                Tasa_Nominal:SCREEN-VALUE            AT 25 FORMAT "X(40)"
                "Interes por Pagar    : "            AT 1
                Interes_PorPagar:SCREEN-VALUE        AT 25 FORMAT "X(40)"
           WITH FRAME AH WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.

      IF W_Ok THEN DO:
         IF Asesoria.Clase_Producto:SCREEN-VALUE EQ "1" THEN
            DISPLAY "PROYECCIÓN DEL PRODUCTO------------------------------------------------------------------------------------------" AT 1 SKIP(1)
          /*                  1         2         3         4         5         6         7         8
                   1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
                  "Fecha           Cuota                    Intereses            Int.Acumulado       Retención                 Capital" AT 1
              WITH FRAME AS113 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
         ELSE 
            DISPLAY "PROYECCIÓN DEL PRODUCTO--------------------------------------------------------------------------------------------" AT 1 SKIP(1)
          /*                  1         2         3         4         5         6         7         8
                   1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
                  "Fecha         Cuota  Int-PdoGracia       Intereses           Int.Acumulado       Capital                Saldo Deuda" AT 1
              WITH FRAME AS3 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.

          FOR EACH TmpAho BY TmpAho.T_Fecha:
            DISPLAY T_Fecha   AT 1    FORM "99/99/99"
                    T_Valor   AT 10   FORM ">>>>,>>>,>>9"
                    T_IntGra  AT 22   FORM ">>>>,>>>,>>9"
                    T_Interes AT 38
                    T_IntTot  AT 59 
                    T_Reten   AT 80    
                    T_Capital AT 98
                    
            WITH FRAME AS2 WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
          END.

      END.
    END.

    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proyeccion_Ahorros wWin 
PROCEDURE Proyeccion_Ahorros :
DO WITH FRAME F_Asesoria:
   DEFI VAR TFinal  LIKE TmpAho.T_Capital INIT 0.
   DEFI VAR TFecFin LIKE TmpAho.T_Fecha.
   DEFI VAR TIntTot LIKE TmpAho.T_IntTot INIT 0.

   W_TipProdWork = INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE,1,1)).
   RUN Buscar_Base_Retencion.

   IF W_TipProdWork EQ 2 THEN 
      W_Valor = DECIMAL(Asesoria.Cuota:SCREEN-VALUE).
   ELSE 
      W_Valor = DECIMAL(Asesoria.Monto:SCREEN-VALUE).

   FOR EACH TmpAho:
       DELETE Tmpaho.
   END.       
    
   ASSIGN W_Cap    = 0
          W_IntTot = 0
          W_Int    = 0.
          
   IF Asesoria.For_Liquidacion:SCREEN-VALUE EQ "1" THEN
    W_Fec = DATE(Asesoria.Fec_Apertura:SCREEN-VALUE).
   ELSE
      W_Fec = DATE(Asesoria.Fec_Apertura:SCREEN-VALUE) + 30.
      
      
    IF W_TipProdWork EQ 3 OR W_TipProdWork EQ 2 THEN DO:
      RUN ConvPlazo.
      
      IF W_TipProdWork EQ 2 THEN
         W_Plazo = ROUND(INTEGER(Asesoria.Plazo:SCREEN-VALUE) / 30,0).
                                              
      DO I = 1 TO W_Plazo BY 1:
         IF I GT 1 THEN
            Run Periodo.
         IF W_TipProdWork EQ 3 THEN DO:
            IF Pro_Ahorros.Tip_Interes EQ 1 THEN
               RUN Termino.
         END.
         
         CREATE TmpAho.
         ASSIGN TmpAho.T_Fecha   = W_Fec
                TmpAho.T_Interes = W_Int
                TmpAho.T_IntTot  = W_IntTot + W_Int - TmpAho.T_Reten
                W_IntTot         = TmpAho.T_IntTot.
                
          IF W_porce GT 0 AND W_BasRet GT 0 AND W_BasRet LT TmpAho.T_Interes THEN 
             TmpAho.T_Reten = (TmpAho.T_Interes - W_BasRet) * (W_porce / 100).
         ELSE
            TmpAho.T_Reten = 0.
             
         /*no se pa que sirve*/
          IF W_TipProdWork EQ 2 THEN
            ASSIGN TmpAho.T_Valor = W_Valor.
         ELSE
            ASSIGN TmpAho.T_Capital = W_Valor.
         /*no sepa que sirve*/     
      END.
      
      IF W_TipProdWork EQ 2 THEN DO:
         WInt = 0.
         FOR EACH TmpAho BY TmpAho.T_Fecha:
             RUN Contractual.
         END.

         FIND LAST TmpAho WHERE TmpAho.T_Fecha GT W_Fecha NO-ERROR.  /*Mayo 23/05 GAER, Ultima cuota con total*/
         IF AVAIL(TmpAho) THEN DO:
            ASSIGN TFinal  = TmpAho.T_Capital + TmpAho.T_Interes
                   TFecFin = TmpAho.T_Fecha
                   TIntTot = TmpAho.T_IntTot.

            CREATE TmpAho.
            ASSIGN TmpAho.T_Capital = TFinal
                   TmpAho.T_Fecha   = TFecFin
                   TmpAho.T_IntTot  = TIntTot.
         END.
      END.
      
      OPEN QUERY B_Proyeccion FOR EACH TmpAho NO-LOCK INDEXED-REPOSITION.
        
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proyecc_Credito wWin 
PROCEDURE Proyecc_Credito :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR I           AS INTEG FORM "99999"     INIT 0.
  DEFI VAR SdoFin      LIKE Creditos.Sdo_Capital INIT 0.
  DEFI VAR INT_MGracia LIKE Creditos.Cuota       INIT 0.
  DEFI VAR W_FecP      AS DATE.
  DEFI VAR P_Monto     LIKE Asesoria.Monto.
  DEFI VAR P_Plazo     LIKE Asesoria.Plazo.
  DEFI VAR P_Tasa      LIKE Creditos.Tasa.
  
  FOR EACH TmpAho: DELETE Tmpaho. END.                                  
  CLOSE QUERY B_Proyeccion.
  DO WITH FRAME F_Asesoria: END.

  ASSIGN W_FecP  = DATE(Asesoria.Fec_Apert:SCREEN-VALUE)
         P_Monto = DEC(Asesoria.Monto:SCREEN-VALUE)
         P_Plazo = INTEG(Asesoria.Plazo:SCREEN-VALUE)
         P_Tasa  = DEC(Asesoria.Tasa:SCREEN-VALUE) / 100.

  IF Pro_Creditos.Id_PerGracia AND Pro_Creditos.Dia_Gracia GT 0 THEN
     ASSIGN INT_MGracia = ROUND((((P_Monto * P_Tasa) / W_DiaCred) * Dia_Gracia) / P_Plazo,0)
            W_FecP      = W_FecP  + 30.

  CREATE TmpAho.
  ASSIGN TmpAho.T_Fecha   = W_FecP
         W_Fec            = W_FecP
         TmpAho.T_Capital = DEC(Asesoria.Monto:SCREEN-VALUE)
         TmpAho.T_Interes = 0
         TmpAho.T_IntTot  = 0
         W_IntTot         = 0
         W_Cap            = DEC(Asesoria.Monto:SCREEN-VALUE)
         W_Mes            = MONTH(TmpAho.T_Fecha) + 1.
  
  DO I = 1 TO INTEG(Asesoria.Plazo:SCREEN-VALUE) + 3:
     IF Rs_PerPago EQ 4 THEN 
        RUN FecVcto.
     ELSE 
        W_Fec = W_Fec + W_DiaCred.
     
     CREATE TmpAho.                                               
     ASSIGN TmpAho.T_Fecha   = W_Fec
            TmpAho.T_Valor   = DEC(Asesoria.Cuota:SCREEN-VALUE)
            TmpAho.T_Interes = ROUND(W_Cap * DEC(Asesoria.Tasa:SCREEN-VALUE) / 100,0)
            W_IntTot         = W_IntTot + TmpAho.T_Interes
            TmpAho.T_IntTot  = W_IntTot                                  
            TmpAho.T_Capital = W_Cap - (TmpAho.T_Valor - (TmpAho.T_Interes + INT_MGracia))
            TmpAho.T_IntGra  = INT_MGracia
            TmpAho.T_Reten   = TmpAho.T_Valor - (TmpAho.T_Interes + INT_MGracia)
            W_Cap            = TmpAho.T_Capital
            W_Mes            = MONTH(W_Fec) + 1.  
            
     IF W_Cap LT TmpAho.T_Valor AND W_Cap GT 0 THEN 
        ASSIGN SdoFin = W_Cap.
            
     IF W_Cap LE 0 THEN DO:
        IF W_Cap LT 0 THEN
           ASSIGN TmpAho.T_Valor   = SdoFin + TmpAho.T_Interes
                  TmpAho.T_Reten   = SdoFin
                  TmpAho.T_Capital = 0
                  W_Cap            = 0.

        LEAVE.       
     END.
  END.
  
  OPEN QUERY B_Proyeccion FOR EACH TmpAho NO-LOCK INDEXED-REPOSITION.

END PROCEDURE.   
               
PROCEDURE FecVcto:                    
   DEFI VAR W_Ano    AS INTEG FORM "9999".
   DEFI VAR W_DiaIni AS INTEG FORM "99". 
   
   ASSIGN W_DiaIni = DAY(DATE(Asesoria.Fec_Apertura:SCREEN-VALUE IN FRAME F_Asesoria))
          W_Ano    = YEAR(W_Fec).
   
   IF W_Mes GT 12 THEN
      ASSIGN W_Mes = W_Mes - 12
             W_Ano = YEAR(W_Fec) + 1.
             
   IF W_DiaIni GT 30 THEN
      W_DiaIni = 30.

   IF W_DiaIni GT 28 AND W_Mes EQ 2 THEN
      W_DiaIni = 28.
      
   ASSIGN W_Fec = DATE (W_Mes,W_DiaIni,W_Ano).                                      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE putValores wWin 
PROCEDURE putValores :
/*------------------------------------------------------------------------------
  Purpose:     Asignar valores capturados en Frame-PreScoring a Frame F_Aseoria
                para que sean almacendos en la tabla en el evento Guardar.
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /*  ASSIGN 
        Asesoria.Nit:SCREEN-VALUE IN FRAME F_Asesoria = Asesoria.Nit:SCREEN-VALUE IN FRAME FRAME-PreScoring
        Asesoria.Monto:SCREEN-VALUE IN FRAME F_Asesoria = Asesoria.Monto:SCREEN-VALUE IN FRAME FRAME-PreScoring
        Asesoria.Plazo:SCREEN-VALUE IN FRAME F_Asesoria = FPlazo:SCREEN-VALUE IN FRAME FRAME-PreScoring
        Asesoria.cuota:SCREEN-VALUE IN FRAME F_Asesoria = FCtaValSol50:SCREEN-VALUE IN FRAME FRAME-PreScoring
        Asesoria.Tasa:SCREEN-VALUE IN FRAME F_Asesoria = FTasa:SCREEN-VALUE IN FRAME FRAME-PreScoring         
        Asesoria.Val_IngresosMes:SCREEN-VALUE IN FRAME F_Asesoria = Asesoria.Val_IngresosMes:SCREEN-VALUE IN FRAME FRAME-PreScoring
        Asesoria.Val_EgresosMes:SCREEN-VALUE IN FRAME F_Asesoria = FEgreMes50:SCREEN-VALUE IN FRAME FRAME-PreScoring.
    */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Termino wWin 
PROCEDURE Termino :
/*------------------------------------------------------------------------------
  Objetivo :     Liquida los intereses de los productos de ahorro a término.
------------------------------------------------------------------------------*/
/* RUN Periodo.*/
DO WITH FRAME F_Asesoria:
 IF Pro_Ahorros.Tip_Interes EQ 1 THEN DO:
    ASSIGN W_Cap    = DECIMAL(Asesoria.Monto:SCREEN-VALUE)
           W_Int    = (W_Cap * DECIMAL(Asesoria.Tasa:SCREEN-VALUE)) / 100.
    /*IF Pro_Ahorros.Tip_Ahorro EQ 2 AND THEN*/
       
    
/*           W_IntTot = (W_IntTot + W_Int).*/
/*                    W_IntGen = W_IntTot.*/
/*    ELSE
 *        W_Cap    = DECIMAL(Ahorros.Monto_Apertura:SCREEN-VALUE).
 *                 W_Int    = ((W_Cap * DECIMAL(Ahorros.Tasa:SCREEN-VALUE)) / 100)
 *                 W_IntTot = (W_IntTot + W_Int)
 *                 W_IntGen = W_IntTot.*/
 END.
 ELSE DO:
    W_Tot = DECIMAL(Asesoria.Monto:SCREEN-VALUE).
 END.
    /*IF Pro_Ahorros.For_Liquidacion EQ 2 THEN DO:
 *        IF I EQ 1 THEN
 *           ASSIGN W_Cap    = DECIMAL(Ahorros.Cuota:SCREEN-VALUE)
 *                  W_IntTot = 0
 *                  W_IntGen = 0
 *                  W_Ret    = 0.
 *        ELSE
 *           ASSIGN W_Cap    = W_Tot + w_intgen
 *                  W_Int    = ((W_cap * DECIMAL(Ahorros.Tasa:SCREEN-VALUE)) / 100)
 *                  W_IntGen = (W_IntGen + W_Int)
 *                  W_IntTot = W_IntTot + W_Int.
 *     END.  
 *     ELSE
 *        ASSIGN W_Cap    = W_Tot + w_intgen
 *               W_Int    = ((W_cap * DECIMAL(Ahorros.Tasa:SCREEN-VALUE)) / 100)
 *               W_IntGen = (W_IntGen + W_Int)
 *               W_IntTot = W_IntTot + W_Int.*/
 END.        
 /*IF Id_Ret EQ YES THEN DO:          
 *     IF W_Int GT W_BasRet THEN
 *        ASSIGN W_Ret    = ((W_Int * W_porce) / 100)
 *               W_IntRet = (W_IntRet + W_Ret).
 *     ELSE
 *        W_Ret = 0.
 *  END.
 *  IF Pro_Ahorros.Tip_Interes EQ 1 THEN
 *     ASSIGN W_Tot    = ((W_Cap + W_IntGen) - W_Ret)
 *            W_TotGen = W_Tot
 *            W_IntGen = (W_IntTot - W_Ret).
 *  ELSE
 *     ASSIGN W_Tot    = ((W_Cap + W_Int) - W_Ret)
 *            W_TotGen = W_Tot
 *            W_IntGen = (W_IntTot - W_Ret).*/
 END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

