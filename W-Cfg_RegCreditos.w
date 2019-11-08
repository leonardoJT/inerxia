&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME W-Cfg_RegCreditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Cfg_RegCreditos 
/*------------------------------------------------------------------------------
  File-Programa    : W-Cfg_RegCreditos.W
  Description      : Politicas y Reglamento por Linea de Credito, definidas
                     en la tabla Cfg_RegCredito; Aplicadas a la radicacion
                     de cada Solicitud de Credito.
  Author           : GAER
  Created          : Octubre 19/2005.
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */
   

/* Local Variable Definitions ---                                       */
  {Incluido/Variable.I "SHARED"}

   ON RETURN TAB.
    
  DEFI TEMP-TABLE CopCfg_RegCredito LIKE Cfg_RegCredito.

  DEFI VAR W_CedTemp LIKE Creditos.Nit.
  DEFI VAR W_SiTemp  AS LOG INIT FALSE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Config
&Scoped-define BROWSE-NAME BROWSE-1

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Cfg_RegCredito PRO_Creditos

/* Definitions for BROWSE BROWSE-1                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-1 Cfg_RegCredito.Cod_Credito Cfg_RegCredito.Tip_Credito Pro_Creditos.Nom_Producto Cfg_RegCredito.Id_Vigente Cfg_RegCredito.Agencia_Exigida Cfg_RegCredito.Id_Transitorio Cfg_RegCredito.Id_UnicoRotatorio Cfg_RegCredito.Id_Libranza Cfg_RegCredito.Id_Empleado Cfg_RegCredito.Vr_AporteMinimo Cfg_RegCredito.Vr_Salario Cfg_RegCredito.Id_Privilegiado Cfg_RegCredito.Id_MismaLinea Cfg_RegCredito.Porcentaje_Cancel Cfg_RegCredito.Plazo_MinMax[1] Cfg_RegCredito.Plazo_MinMax[2] Cfg_RegCredito.Monto_MinMax[1] Cfg_RegCredito.Monto_MinMax[2] Cfg_RegCredito.Observ   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-1   
&Scoped-define SELF-NAME BROWSE-1
&Scoped-define QUERY-STRING-BROWSE-1 FOR EACH Cfg_RegCredito NO-LOCK, ~
                                   EACH PRO_Creditos NO-LOCK WHERE                                  PRO_Creditos.Cod_Credito EQ Cfg_RegCredito.Cod_Credito INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-1 OPEN QUERY {&SELF-NAME} FOR EACH Cfg_RegCredito NO-LOCK, ~
                                   EACH PRO_Creditos NO-LOCK WHERE                                  PRO_Creditos.Cod_Credito EQ Cfg_RegCredito.Cod_Credito INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-1 Cfg_RegCredito PRO_Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-1 Cfg_RegCredito
&Scoped-define SECOND-TABLE-IN-QUERY-BROWSE-1 PRO_Creditos


/* Definitions for BROWSE BROWSE-3                                      */
&Scoped-define FIELDS-IN-QUERY-BROWSE-3 Pro_Creditos.Cod_Credito Pro_Creditos.Tip_Credito Pro_Creditos.Nom_Producto Pro_Creditos.Estado   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BROWSE-3   
&Scoped-define SELF-NAME BROWSE-3
&Scoped-define QUERY-STRING-BROWSE-3 FOR EACH Pro_Creditos WHERE Estado EQ 1 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BROWSE-3 OPEN QUERY {&SELF-NAME} FOR EACH Pro_Creditos WHERE Estado EQ 1 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BROWSE-3 Pro_Creditos
&Scoped-define FIRST-TABLE-IN-QUERY-BROWSE-3 Pro_Creditos


/* Definitions for FRAME F_Config                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Config ~
    ~{&OPEN-QUERY-BROWSE-1}

/* Definitions for FRAME F_Consul                                       */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consul ~
    ~{&OPEN-QUERY-BROWSE-3}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Cfg_RegCredito.Id_Vigente ~
Cfg_RegCredito.Id_Transitorio Cfg_RegCredito.Id_Libranza ~
Cfg_RegCredito.Id_Empleado Cfg_RegCredito.Id_Privilegiado ~
Cfg_RegCredito.Id_UnicoRotatorio Cfg_RegCredito.Id_LineaEspecial ~
Cfg_RegCredito.Id_MismaLinea Cfg_RegCredito.Agencia_Exigida ~
Cfg_RegCredito.Nro_DiasVencidos Cfg_RegCredito.DiasVencidos_Fiando ~
Cfg_RegCredito.Vr_MontoCodeud Cfg_RegCredito.Tiempo_Antig ~
Cfg_RegCredito.Tipo_Actividad Cfg_RegCredito.Vr_AporteMinimo ~
Cfg_RegCredito.Vr_Salario Cfg_RegCredito.Porcentaje_Cancel ~
Cfg_RegCredito.Rangos_Monto[1] Cfg_RegCredito.Rangos_Plazo[1] ~
Cfg_RegCredito.Rangos_Monto[2] Cfg_RegCredito.Rangos_Plazo[2] ~
Cfg_RegCredito.Rangos_Monto[3] Cfg_RegCredito.Rangos_Plazo[3] ~
Cfg_RegCredito.Rangos_Monto[4] Cfg_RegCredito.Rangos_Plazo[4] ~
Cfg_RegCredito.Rangos_Monto[5] Cfg_RegCredito.Rangos_Plazo[5] ~
Cfg_RegCredito.Edad_MinMax[1] Cfg_RegCredito.Edad_MinMax[2] ~
Cfg_RegCredito.Plazo_MinMax[1] Cfg_RegCredito.Plazo_MinMax[2] ~
Cfg_RegCredito.Monto_MinMax[1] Cfg_RegCredito.Monto_MinMax[2] ~
Cfg_RegCredito.Tipo_Cliente Cfg_RegCredito.Tip_Contrato 
&Scoped-define ENABLED-TABLES Cfg_RegCredito
&Scoped-define FIRST-ENABLED-TABLE Cfg_RegCredito
&Scoped-Define ENABLED-OBJECTS Btn_ConsTemp W_CedUnaVez Btn_Salvar BROWSE-1 ~
Btn_Ingr Btn_RestOrig RECT-302 RECT-303 RECT-304 
&Scoped-Define DISPLAYED-FIELDS Cfg_RegCredito.Cod_Credito ~
Cfg_RegCredito.Id_Vigente Cfg_RegCredito.Id_Transitorio ~
Cfg_RegCredito.Id_Libranza Cfg_RegCredito.Id_Empleado ~
Cfg_RegCredito.Id_Privilegiado Cfg_RegCredito.Id_UnicoRotatorio ~
Cfg_RegCredito.Id_LineaEspecial Cfg_RegCredito.Id_MismaLinea ~
Cfg_RegCredito.Agencia_Exigida Cfg_RegCredito.Nro_DiasVencidos ~
Cfg_RegCredito.DiasVencidos_Fiando Cfg_RegCredito.Vr_MontoCodeud ~
Cfg_RegCredito.Tiempo_Antig Cfg_RegCredito.Tipo_Actividad ~
Cfg_RegCredito.Vr_AporteMinimo Cfg_RegCredito.Vr_Salario ~
Cfg_RegCredito.Porcentaje_Cancel Cfg_RegCredito.Rangos_Monto[1] ~
Cfg_RegCredito.Rangos_Plazo[1] Cfg_RegCredito.Rangos_Monto[2] ~
Cfg_RegCredito.Rangos_Plazo[2] Cfg_RegCredito.Rangos_Monto[3] ~
Cfg_RegCredito.Rangos_Plazo[3] Cfg_RegCredito.Rangos_Monto[4] ~
Cfg_RegCredito.Rangos_Plazo[4] Cfg_RegCredito.Rangos_Monto[5] ~
Cfg_RegCredito.Rangos_Plazo[5] Cfg_RegCredito.Edad_MinMax[1] ~
Cfg_RegCredito.Edad_MinMax[2] Cfg_RegCredito.Plazo_MinMax[1] ~
Cfg_RegCredito.Plazo_MinMax[2] Cfg_RegCredito.Monto_MinMax[1] ~
Cfg_RegCredito.Monto_MinMax[2] Cfg_RegCredito.Tipo_Cliente ~
Cfg_RegCredito.Tip_Contrato 
&Scoped-define DISPLAYED-TABLES Cfg_RegCredito
&Scoped-define FIRST-DISPLAYED-TABLE Cfg_RegCredito
&Scoped-Define DISPLAYED-OBJECTS W_NomCte W_CedUnaVez NomLinea Tipo_Cred 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Cfg_RegCreditos AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_ConsTemp 
     LABEL "&Consultar Temporal" 
     SIZE 19.29 BY 1.12 TOOLTIP "Muestra en la Ventana Config.Temporal de la Linea Seleccionada".

DEFINE BUTTON Btn_Ingr 
     LABEL "&INGRESAR" 
     SIZE 16.57 BY 1.

DEFINE BUTTON Btn_RestOrig 
     LABEL "&Solo Salva en Temporal" 
     SIZE 19.43 BY 1.12 TOOLTIP "Solo Actualiza Reglamento temporal para la linea Seleccionada".

DEFINE BUTTON Btn_Salvar 
     LABEL "&Actualiza Reglamento" 
     SIZE 16.57 BY 1.04 TOOLTIP "Solo para Actualizar el Reglamento Vigente".

DEFINE VARIABLE NomLinea AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 41.14 BY .81
     BGCOLOR 1  NO-UNDO.

DEFINE VARIABLE Tipo_Cred AS CHARACTER FORMAT "X(20)":U 
     VIEW-AS FILL-IN 
     SIZE 30.29 BY .81
     BGCOLOR 1  NO-UNDO.

DEFINE VARIABLE W_CedUnaVez AS CHARACTER FORMAT "X(12)":U 
     VIEW-AS FILL-IN 
     SIZE 11.72 BY .77 TOOLTIP "Solo si es para una Ced/Nit por una vez"
     BGCOLOR 4 FGCOLOR 7  NO-UNDO.

DEFINE VARIABLE W_NomCte AS CHARACTER FORMAT "X(40)":U 
     VIEW-AS FILL-IN 
     SIZE 40.14 BY .73
     BGCOLOR 4 FGCOLOR 5  NO-UNDO.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL 
     SIZE 54 BY 3.19.

DEFINE RECTANGLE RECT-303
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL 
     SIZE 32.14 BY 4.85.

DEFINE RECTANGLE RECT-304
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL 
     SIZE 22.57 BY 6.04.

DEFINE BUTTON Btn_Regresa 
     LABEL "Regresar" 
     SIZE 10.29 BY 1.54
     FONT 5.

DEFINE BUTTON BUTTON-161 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 161" 
     SIZE 8 BY 1.35 TOOLTIP "Graba y regresa a la Ventana principal".

DEFINE VARIABLE W_Obs AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 200 SCROLLBAR-VERTICAL
     SIZE 37.86 BY 3.12 TOOLTIP "Por favor documente la modificacion al reglamento" NO-UNDO.

DEFINE BUTTON BUTTON-163 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 163" 
     SIZE 11.72 BY 2.15 TOOLTIP "Activa la Ventana principal".

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BROWSE-1 FOR 
      Cfg_RegCredito, 
      PRO_Creditos SCROLLING.

DEFINE QUERY BROWSE-3 FOR 
      Pro_Creditos SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-1 W-Cfg_RegCreditos _FREEFORM
  QUERY BROWSE-1 NO-LOCK DISPLAY
      Cfg_RegCredito.Cod_Credito FORMAT "999":U COLUMN-LABEL "Linea"
      Cfg_RegCredito.Tip_Credito FORMAT "9":U   COLUMN-LABEL "T"
      Pro_Creditos.Nom_Producto  FORM "X(35)"   COLUMN-LABEL "Descripcion de la Linea"
      Cfg_RegCredito.Id_Vigente FORMAT "9":U    COLUMN-LABEL "Vigente"
      Cfg_RegCredito.Agencia_Exigida FORMAT "999":U    COLUMN-LABEL "Agen.Exigida" 
      Cfg_RegCredito.Id_Transitorio FORMAT "9":U       COLUMN-LABEL "Transit"
      Cfg_RegCredito.Id_UnicoRotatorio FORMAT "9":U    COLUMN-LABEL "Uni-Rotat"
      Cfg_RegCredito.Id_Libranza FORMAT "9":U          COLUMN-LABEL "Libranza"
      Cfg_RegCredito.Id_Empleado FORMAT "9":U          COLUMN-LABEL "Empleados"
      Cfg_RegCredito.Vr_AporteMinimo FORMAT ">,>>>,>>>,>>9":U
      Cfg_RegCredito.Vr_Salario FORMAT ">,>>>,>>>,>>9":U
      Cfg_RegCredito.Id_Privilegiado FORMAT "9":U
      Cfg_RegCredito.Id_MismaLinea FORMAT "9":U
      Cfg_RegCredito.Porcentaje_Cancel FORMAT ">9.99":U
      Cfg_RegCredito.Plazo_MinMax[1] FORMAT ">>>9":U
      Cfg_RegCredito.Plazo_MinMax[2] FORMAT ">>>9":U
      Cfg_RegCredito.Monto_MinMax[1] FORMAT ">>>>,>>>,>>9":U
      Cfg_RegCredito.Monto_MinMax[2] FORMAT ">>>>,>>>,>>9":U
      Cfg_RegCredito.Observ  FORM "X(620)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 108.71 BY 6.73
         FGCOLOR 7 FONT 4 EXPANDABLE TOOLTIP "Con Click Captura la selección para actualizarla".

DEFINE BROWSE BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BROWSE-3 W-Cfg_RegCreditos _FREEFORM
  QUERY BROWSE-3 NO-LOCK DISPLAY
      Pro_Creditos.Cod_Credito FORMAT "999":U
      Pro_Creditos.Tip_Credito FORMAT "9":U  COLUMN-LABEL "Tipo"
      Pro_Creditos.Nom_Producto FORMAT "X(40)":U
      Pro_Creditos.Estado FORMAT "9":U       COLUMN-LABEL "Est"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 50.14 BY 13.42
         FONT 4 ROW-HEIGHT-CHARS .54 EXPANDABLE TOOLTIP "Con Doble Click captura la Linea".


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Consul
     BROWSE-3 AT ROW 1.15 COL 1.72
     Btn_Regresa AT ROW 14.92 COL 40.86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 7.14 ROW 2.12
         SIZE 52.29 BY 16.62
         BGCOLOR 8 FGCOLOR 0 FONT 4
         TITLE "Productos (Líneas de Crédito)".

DEFINE FRAME F_Config
     Btn_ConsTemp AT ROW 11.92 COL 89.29
     W_NomCte AT ROW 10.73 COL 43.43 COLON-ALIGNED NO-LABEL
     W_CedUnaVez AT ROW 10.69 COL 31.72 COLON-ALIGNED NO-LABEL
     Cfg_RegCredito.Cod_Credito AT ROW 1.31 COL 1 COLON-ALIGNED NO-LABEL FORMAT "999"
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81
          BGCOLOR 15 FGCOLOR 18 
     NomLinea AT ROW 1.31 COL 4.86 COLON-ALIGNED NO-LABEL
     Tipo_Cred AT ROW 1.31 COL 48 COLON-ALIGNED NO-LABEL
     Cfg_RegCredito.Id_Vigente AT ROW 1.5 COL 87.57 NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Vigente", 0,
"No-Vigente", 1
          SIZE 22 BY .58
          BGCOLOR 15 FGCOLOR 18 
     Cfg_RegCredito.Id_Transitorio AT ROW 2.27 COL 29.29 COLON-ALIGNED HELP
          ""
          LABEL "Transitorio: 0 No, 1 Si" FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 2.14 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Id_Libranza AT ROW 3.23 COL 29.29 COLON-ALIGNED
          LABEL "Por Libranza: 0 No, 1 Si" FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 2.14 BY .81 TOOLTIP "Por Libranza: 0 No, 1 Si"
          BGCOLOR 15 
     Cfg_RegCredito.Id_Empleado AT ROW 4.23 COL 29.14 COLON-ALIGNED
          LABEL "Para Empleados: 0 No, 1 Si" FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 2.14 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Id_Privilegiado AT ROW 5.23 COL 29.14 COLON-ALIGNED
          LABEL "Para Privilegiados: 0 No, 1 Si" FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 2.14 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Id_UnicoRotatorio AT ROW 6.15 COL 29.14 COLON-ALIGNED
          LABEL "UnicoRotatorio: 0 No, 1 Si" FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 2.14 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Id_LineaEspecial AT ROW 7.12 COL 29.14 COLON-ALIGNED
          LABEL "LineaEspecial: 0 No, 1 Si" FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 2.14 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Id_MismaLinea AT ROW 8.19 COL 29.14 COLON-ALIGNED
          LABEL "Solo Uno por MismaLinea: 0 No, 1 Si" FORMAT "9"
          VIEW-AS FILL-IN 
          SIZE 2.14 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Agencia_Exigida AT ROW 2.31 COL 74.57 COLON-ALIGNED
          LABEL "Solo para esta Agencia" FORMAT "999"
          VIEW-AS FILL-IN 
          SIZE 3.57 BY .81 TOOLTIP "Solo para esta Agencia, Con 0 para todas"
          BGCOLOR 15 
     Cfg_RegCredito.Nro_DiasVencidos AT ROW 3.27 COL 74 COLON-ALIGNED
          LABEL "Nro_DiasVencidos(Máximo)" FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 4.29 BY .81 TOOLTIP "0 No permite dias vencidos"
          BGCOLOR 15 
     Cfg_RegCredito.DiasVencidos_Fiando AT ROW 4.19 COL 74 COLON-ALIGNED
          LABEL "Máx.DiasVencidos Fiando A" FORMAT "9999"
          VIEW-AS FILL-IN 
          SIZE 4.43 BY .81 TOOLTIP "0 No permite dias vencidos"
          BGCOLOR 15 
     Cfg_RegCredito.Vr_MontoCodeud AT ROW 5.15 COL 65.86 COLON-ALIGNED
          LABEL "Tope Monto Exige Garantia" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 12.57 BY .81 TOOLTIP "0 No aplica, Valor a partir del cual se exige Garantia."
          BGCOLOR 15 
     Cfg_RegCredito.Tiempo_Antig AT ROW 6.12 COL 72.57 COLON-ALIGNED
          LABEL "Tiempo Antiguedad en Dias" FORMAT ">>,>>9"
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .81 TOOLTIP "0 No-Aplica"
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.57 BY 21.27
         BGCOLOR 17 FGCOLOR 0 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Config
     Cfg_RegCredito.Tipo_Actividad AT ROW 7.04 COL 66.14 COLON-ALIGNED
          LABEL "Para Tipo de Actividad" FORMAT "X(15)"
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .81 TOOLTIP "En Blanco No-Aplica, Empleado,Estudiante,Ama de Casa,Jubilado,Independiente"
          BGCOLOR 15 
     Cfg_RegCredito.Vr_AporteMinimo AT ROW 8.04 COL 66.29 COLON-ALIGNED
          LABEL "Exige AporteMínimo de $" FORMAT ">,>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Vr_Salario AT ROW 8.92 COL 66.29 COLON-ALIGNED
          LABEL "Salario-Cliente Mínimo de $"
          VIEW-AS FILL-IN 
          SIZE 12.14 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Porcentaje_Cancel AT ROW 9.77 COL 73.29 COLON-ALIGNED
          LABEL "Porcentaje de Cancelación" FORMAT ">9.99"
          VIEW-AS FILL-IN 
          SIZE 5 BY .73
          BGCOLOR 15 
     Cfg_RegCredito.Rangos_Monto[1] AT ROW 10.42 COL 4.14 COLON-ALIGNED NO-LABEL FORMAT ">>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13.43 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Rangos_Plazo[1] AT ROW 10.42 COL 22.29 COLON-ALIGNED NO-LABEL FORMAT ">,>>9"
          VIEW-AS FILL-IN 
          SIZE 5.86 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Rangos_Monto[2] AT ROW 11.31 COL 4.14 COLON-ALIGNED NO-LABEL FORMAT ">>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13.29 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Rangos_Plazo[2] AT ROW 11.35 COL 22.29 COLON-ALIGNED NO-LABEL FORMAT ">,>>9"
          VIEW-AS FILL-IN 
          SIZE 5.86 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Rangos_Monto[3] AT ROW 12.19 COL 4.14 COLON-ALIGNED NO-LABEL FORMAT ">>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13.29 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Rangos_Plazo[3] AT ROW 12.23 COL 22.29 COLON-ALIGNED NO-LABEL FORMAT ">,>>9"
          VIEW-AS FILL-IN 
          SIZE 5.86 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Rangos_Monto[4] AT ROW 13.04 COL 4.14 COLON-ALIGNED NO-LABEL FORMAT ">>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13.29 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Rangos_Plazo[4] AT ROW 13.08 COL 22.29 COLON-ALIGNED NO-LABEL FORMAT ">,>>9"
          VIEW-AS FILL-IN 
          SIZE 5.86 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Rangos_Monto[5] AT ROW 13.85 COL 4.14 COLON-ALIGNED NO-LABEL FORMAT ">>>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 13.29 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Rangos_Plazo[5] AT ROW 13.92 COL 22.29 COLON-ALIGNED NO-LABEL FORMAT ">,>>9"
          VIEW-AS FILL-IN 
          SIZE 5.86 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Edad_MinMax[1] AT ROW 11.77 COL 52.14 COLON-ALIGNED
          LABEL "Edad_Mínima en Años" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 5.29 BY .81 TOOLTIP "0 No-Aplica"
          BGCOLOR 15 
     Cfg_RegCredito.Edad_MinMax[2] AT ROW 11.81 COL 78 COLON-ALIGNED
          LABEL "Edad Máxima en Años" FORMAT ">>9"
          VIEW-AS FILL-IN 
          SIZE 5.57 BY .81 TOOLTIP "0 No-aplica"
          BGCOLOR 15 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.57 BY 21.27
         BGCOLOR 17 FGCOLOR 0 FONT 4.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Config
     Cfg_RegCredito.Plazo_MinMax[1] AT ROW 12.81 COL 52 COLON-ALIGNED
          LABEL "Plazo_Mínimo en Dias" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 5.57 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Plazo_MinMax[2] AT ROW 12.81 COL 78 COLON-ALIGNED
          LABEL "Plazo_Máximo en Dias" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 5.72 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Monto_MinMax[1] AT ROW 13.73 COL 46.57 COLON-ALIGNED
          LABEL "Monto_Mínimo $" FORMAT ">>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11.14 BY .81
          BGCOLOR 15 
     Cfg_RegCredito.Monto_MinMax[2] AT ROW 13.77 COL 72 COLON-ALIGNED
          LABEL "Monto_Máximo$" FORMAT ">>>>,>>>,>>9"
          VIEW-AS FILL-IN 
          SIZE 11.86 BY .81
          BGCOLOR 15 
     Btn_Salvar AT ROW 9.12 COL 90.57
     BROWSE-1 AT ROW 14.73 COL 1.29
     Btn_Ingr AT ROW 10.38 COL 90.72
     Cfg_RegCredito.Tipo_Cliente AT ROW 2.69 COL 87.43 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "No-Aplica", 0,
"Natural Mayor de Edad", 1,
"Natural Menor de Edad", 2,
"Juridica S.A", 3,
"Juridica C.A", 4
          SIZE 19.29 BY 2.62
     Cfg_RegCredito.Tip_Contrato AT ROW 5.96 COL 87.57 NO-LABEL
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Ninguno", 0,
"Indefinido", 1,
"Fijo", 2,
"Labor Contratada", 3,
"Prestación de Servicios", 4
          SIZE 19 BY 2.69
     Btn_RestOrig AT ROW 13.35 COL 89
     RECT-302 AT ROW 11.5 COL 33.57
     RECT-303 AT ROW 9.96 COL 1.29
     RECT-304 AT ROW 8.69 COL 87.57
     "Para Tipo de Cliente" VIEW-AS TEXT
          SIZE 18.29 BY .5 AT ROW 2.19 COL 87.57
          BGCOLOR 18 FGCOLOR 0 
     "Para Tipo de Contrato" VIEW-AS TEXT
          SIZE 18.14 BY .5 AT ROW 5.42 COL 87.72
          BGCOLOR 18 
     "Topes Máximos: MONTOS          PLAZOS" VIEW-AS TEXT
          SIZE 29.57 BY .5 AT ROW 9.81 COL 1.72
          BGCOLOR 18 FGCOLOR 0 
     "La Linea Está :" VIEW-AS TEXT
          SIZE 10.29 BY .5 AT ROW 1.04 COL 87.57
          BGCOLOR 18 FGCOLOR 0 
     "Ced/Nit por una Sola Vez" VIEW-AS TEXT
          SIZE 18 BY .5 AT ROW 10.15 COL 33.86
          BGCOLOR 18 
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 110.57 BY 21.27
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "LINEA DE CREDITO     -     Parámetros de Configuración".

DEFINE FRAME F_Obs
     W_Obs AT ROW 1.19 COL 2.43 NO-LABEL
     BUTTON-161 AT ROW 4.54 COL 29.29
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 45.43 ROW 12.23
         SIZE 42.43 BY 6
         BGCOLOR 17 FONT 5
         TITLE "Observación sobre La Modificación".

DEFINE FRAME F_Retorno
     BUTTON-163 AT ROW 2.04 COL 9.86
     "Visualización de ReglamentoTemporal" VIEW-AS TEXT
          SIZE 26 BY .62 AT ROW 1.23 COL 3.14
          BGCOLOR 7 FGCOLOR 15 FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.72 ROW 15.58
         SIZE 31.72 BY 5.23
         BGCOLOR 19 
         TITLE "Solo Activa Ventana principal".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-Cfg_RegCreditos ASSIGN
         HIDDEN             = YES
         TITLE              = "Políticas y Reglamento de Créditos, Programa W-Cfg_RegCreditos.W"
         HEIGHT             = 21.27
         WIDTH              = 110.57
         MAX-HEIGHT         = 22.35
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 22.35
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT W-Cfg_RegCreditos:LOAD-ICON("adeicon/workshp%.ico":U) THEN
    MESSAGE "Unable to load icon: adeicon/workshp%.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Cfg_RegCreditos
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Config
   Custom                                                               */
/* BROWSE-TAB BROWSE-1 Btn_Salvar F_Config */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Agencia_Exigida IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Cod_Credito IN FRAME F_Config
   NO-ENABLE EXP-LABEL EXP-FORMAT                                       */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.DiasVencidos_Fiando IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Edad_MinMax[1] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Edad_MinMax[2] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Id_Empleado IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Id_Libranza IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Id_LineaEspecial IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Id_MismaLinea IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Id_Privilegiado IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Id_Transitorio IN FRAME F_Config
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Id_UnicoRotatorio IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Monto_MinMax[1] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Monto_MinMax[2] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN NomLinea IN FRAME F_Config
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Nro_DiasVencidos IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Plazo_MinMax[1] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Plazo_MinMax[2] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Porcentaje_Cancel IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Rangos_Monto[1] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Rangos_Monto[2] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Rangos_Monto[3] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Rangos_Monto[4] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Rangos_Monto[5] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Rangos_Plazo[1] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Rangos_Plazo[2] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Rangos_Plazo[3] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Rangos_Plazo[4] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Rangos_Plazo[5] IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Tiempo_Antig IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Tipo_Actividad IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Tipo_Cred IN FRAME F_Config
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Vr_AporteMinimo IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Vr_MontoCodeud IN FRAME F_Config
   EXP-LABEL EXP-FORMAT                                                 */
/* SETTINGS FOR FILL-IN Cfg_RegCredito.Vr_Salario IN FRAME F_Config
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN W_NomCte IN FRAME F_Config
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Consul
   NOT-VISIBLE                                                          */
/* BROWSE-TAB BROWSE-3 1 F_Consul */
ASSIGN 
       FRAME F_Consul:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Obs
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Obs:HIDDEN           = TRUE
       FRAME F_Obs:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F_Retorno
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Retorno:HIDDEN           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Cfg_RegCreditos)
THEN W-Cfg_RegCreditos:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-1
/* Query rebuild information for BROWSE BROWSE-1
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Cfg_RegCredito NO-LOCK,
                            EACH PRO_Creditos NO-LOCK WHERE
                                 PRO_Creditos.Cod_Credito EQ Cfg_RegCredito.Cod_Credito INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-1 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BROWSE-3
/* Query rebuild information for BROWSE BROWSE-3
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Pro_Creditos WHERE Estado EQ 1 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BROWSE-3 */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Config
/* Query rebuild information for FRAME F_Config
     _Query            is NOT OPENED
*/  /* FRAME F_Config */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Consul
/* Query rebuild information for FRAME F_Consul
     _Query            is NOT OPENED
*/  /* FRAME F_Consul */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Obs
/* Query rebuild information for FRAME F_Obs
     _Query            is NOT OPENED
*/  /* FRAME F_Obs */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Retorno
/* Query rebuild information for FRAME F_Retorno
     _Query            is NOT OPENED
*/  /* FRAME F_Retorno */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Cfg_RegCreditos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Cfg_RegCreditos W-Cfg_RegCreditos
ON END-ERROR OF W-Cfg_RegCreditos /* Políticas y Reglamento de Créditos, Programa W-Cfg_RegCreditos.W */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Cfg_RegCreditos W-Cfg_RegCreditos
ON WINDOW-CLOSE OF W-Cfg_RegCreditos /* Políticas y Reglamento de Créditos, Programa W-Cfg_RegCreditos.W */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-1
&Scoped-define SELF-NAME BROWSE-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-1 W-Cfg_RegCreditos
ON MOUSE-SELECT-CLICK OF BROWSE-1 IN FRAME F_Config
DO:
  IF AVAIL(Cfg_RegCredito) THEN
     RUN Mostrar_Config.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BROWSE-3
&Scoped-define FRAME-NAME F_Consul
&Scoped-define SELF-NAME BROWSE-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BROWSE-3 W-Cfg_RegCreditos
ON MOUSE-SELECT-DBLCLICK OF BROWSE-3 IN FRAME F_Consul
DO:
  IF AVAIL(Pro_Creditos) THEN DO:
     FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ Pro_Credito.Cod_Credito NO-LOCK NO-ERROR.
     IF AVAIL(Cfg_RegCredito) THEN 
        MESSAGE "Ya Existe la configuracion para esta linea." SKIP
                "               Seleccionela del Browser de Consulta."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ELSE DO:
        RUN Nom_Tipo.
        ASSIGN Cfg_RegCredito.Cod_Credito:SCREEN-VALUE IN FRAME F_Config = STRING(Pro_Credito.Cod_Credito)
               NomLinea:SCREEN-VALUE = Pro_Credito.Nom_Producto.
     END.
  END.

  ASSIGN FRAME F_Consul:VISIBLE   = FALSE
         FRAME F_Config:SENSITIVE = TRUE.             
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Config
&Scoped-define SELF-NAME Btn_ConsTemp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ConsTemp W-Cfg_RegCreditos
ON CHOOSE OF Btn_ConsTemp IN FRAME F_Config /* Consultar Temporal */
DO:
  IF NOT AVAIL(Cfg_RegCredito) THEN DO:
     MESSAGE "No tiene seleccionada linea para la Consulta de la Temporal."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
  END.

  FIND FIRST Cfg_OrigRegCredito WHERE 
             Cfg_OrigRegCredito.Cod_Credito EQ Cfg_RegCredito.Cod_Credito NO-ERROR.
  IF NOT AVAIL(Cfg_OrigRegCredito) THEN DO:
     MESSAGE "La linea seleccionada No Existe en la Temporal."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
  END.

  FOR EACH CopCfg_RegCredito: DELETE CopCfg_RegCredito. END.
  CREATE CopCfg_RegCredito.
  BUFFER-COPY Cfg_RegCredito TO CopCfg_RegCredito.
  FIND CURRENT Cfg_RegCredito NO-ERROR.
  BUFFER-COPY Cfg_OrigRegCredito TO Cfg_RegCredito.

  RUN Mostrar_Config.

  ASSIGN FRAME F_Config:SENSITIVE = FALSE
         FRAME F_Retorno:VISIBLE  = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ingr
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingr W-Cfg_RegCreditos
ON CHOOSE OF Btn_Ingr IN FRAME F_Config /* INGRESAR */
DO:
  RELEASE Cfg_RegCredito.

  ASSIGN Cfg_RegCredito.Cod_Credito:SCREEN-VALUE      = "0"
         Cfg_RegCredito.Cod_Credito:SENSITIVE         = TRUE
         Cfg_RegCredito.Id_Vigente:SCREEN-VALUE       = "0" 
         Cfg_RegCredito.Edad_MinMax[1]:SCREEN-VALUE   = "0" 
         Cfg_RegCredito.Edad_MinMax[2]:SCREEN-VALUE   = "0"
         Cfg_RegCredito.Plazo_MinMax[1]:SCREEN-VALUE  = "0"
         Cfg_RegCredito.Plazo_MinMax[2]:SCREEN-VALUE  = "0"
         Cfg_RegCredito.Monto_MinMax[1]:SCREEN-VALUE  = "0"
         Cfg_RegCredito.Monto_MinMax[2]:SCREEN-VALUE  = "0"
         /*Cfg_RegCredito.Nro_Codeudores:SCREEN-VALUE  = "0"*/
         Cfg_RegCredito.Nro_DiasVencidos:SCREEN-VALUE = "0"
         Cfg_RegCredito.Tipo_Cliente:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Id_Empleado:SCREEN-VALUE      = "0"
         NomLinea:SCREEN-VALUE  = ""
         Tipo_Cred:SCREEN-VALUE = "".

  ASSIGN Cfg_RegCredito.Porcentaje_Cancel:SCREEN-VALUE = "0" 
         Cfg_RegCredito.Id_Libranza:SCREEN-VALUE       = "0"
         Cfg_RegCredito.Tiempo_Antig:SCREEN-VALUE      = "0"
         Cfg_RegCredito.Id_LineaEspecial:SCREEN-VALUE  = "0"
         Cfg_RegCredito.Tipo_Actividad:SCREEN-VALUE    = ""
         Cfg_RegCredito.Tip_Contrato:SCREEN-VALUE      = "0"
         Cfg_RegCredito.Id_MismaLinea:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Id_Privilegiado:SCREEN-VALUE   = "0"
         Cfg_RegCredito.Agencia_Exigida:SCREEN-VALUE   = "0"
         Cfg_RegCredito.Id_Transitorio:SCREEN-VALUE    = "0"
         Cfg_RegCredito.Vr_AporteMinimo:SCREEN-VALUE   = "0"
         Cfg_RegCredito.Id_UnicoRotatorio:SCREEN-VALUE = "0"
         Cfg_RegCredito.Vr_Salario:SCREEN-VALUE        = "0".

  ASSIGN Cfg_RegCredito.Rangos_Monto[1]:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Rangos_Plazo[1]:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Rangos_Monto[2]:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Rangos_Plazo[2]:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Rangos_Monto[3]:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Rangos_Plazo[3]:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Rangos_Monto[4]:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Rangos_Plazo[4]:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Rangos_Monto[5]:SCREEN-VALUE     = "0"
         Cfg_RegCredito.Rangos_Plazo[5]:SCREEN-VALUE     = "0"
         Cfg_RegCredito.DiasVencidos_Fiando:SCREEN-VALUE = "0"
         Cfg_RegCredito.Vr_MontoCodeud:SCREEN-VALUE      = "0".
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consul
&Scoped-define SELF-NAME Btn_Regresa
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Regresa W-Cfg_RegCreditos
ON CHOOSE OF Btn_Regresa IN FRAME F_Consul /* Regresar */
DO:
  ASSIGN FRAME F_Consul:VISIBLE   = FALSE
         FRAME F_Config:SENSITIVE = TRUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Config
&Scoped-define SELF-NAME Btn_RestOrig
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_RestOrig W-Cfg_RegCreditos
ON CHOOSE OF Btn_RestOrig IN FRAME F_Config /* Solo Salva en Temporal */
DO:      
  IF W_CedUnaVez NE " " THEN DO:
     MESSAGE "La Configuracion Temporal de la Linea sera modificada con la Ced/Nit :" W_CedUnaVez SKIP
           "                   Continue solo si esta Segura(o)...?."
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
           TITLE "CONFIRMAR MODIFICAR TEMPORAL"
           UPDATE W_RptaTP AS LOG.
     IF NOT W_RptaTP THEN 
        RETURN.
  END.
  ELSE DO:
     FIND FIRST Cfg_OrigRegCredito WHERE 
                Cfg_OrigRegCredito.Cod_Credito   EQ Cfg_RegCredito.Cod_Credito
            AND Cfg_OrigRegCredito.CedNit_UnaVez NE " " NO-LOCK NO-ERROR.
     IF NOT AVAIL(Cfg_OrigRegCredito) THEN DO:
        MESSAGE "No existe Temporal con Ced/Nit para Eliminarle la Ced/Nit...?" SKIP
                "Operacion rechazada."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN.
     END.
        
     MESSAGE "La Ced/Nit de La Configuracion Temporal de la Linea sera Eliminada." SKIP
           "                   Continue solo si esta Segura(o)...?."
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
           TITLE "CONFIRMAR ELIMINAR TEMPORAL"
           UPDATE W_RptaTE AS LOG.
     IF NOT W_RptaTE THEN 
        RETURN.           
  END.

  ASSIGN W_SiTemp = TRUE.
    
  RUN Salva_Temp. 

  ASSIGN W_SiTemp = FALSE.
    
  RUN Reab_Query.  

  APPLY "Mouse-Select-Click" TO BROWSE-1.
    
END.
    
    
  /* IF AVAIL(Cfg_RegCredito) THEN DO:
      MESSAGE "Esta Segura(o) de Restaurar la Configuracion Original de esta Linea,"  SKIP
              "               A La configuracion de la Tabla Operativa....?" SKIP
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
         TITLE "CONFIRMAR GRABACION EN CONFIG.ORIGINAL"
         UPDATE W_RptaOp AS LOG.
      IF W_RptaOp THEN DO:
         FIND FIRST Cfg_OrigRegCredito WHERE 
                    Cfg_OrigRegCredito.Cod_Credito EQ Cfg_RegCredito.Cod_Credito NO-LOCK NO-ERROR.
         IF NOT AVAIL(Cfg_OrigRegCredito) THEN DO:
            MESSAGE "No-Existe Configuracion Original para Esta Linea, Verifique el Por Que?..." SKIP 
                VIEW-AS ALERT-BOX ERROR.
            RETURN.
         END.

         FIND CURRENT Cfg_RegCredito NO-ERROR.
         BUFFER-COPY  Cfg_OrigRegCredito TO Cfg_RegCredito.
         
         RUN Mostrar_Config.

         RUN Reab_Query.         
      END.
   END.
   ELSE 
      MESSAGE "Debe Seleccionar Una Linea para poder Restaurar La Original."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar W-Cfg_RegCreditos
ON CHOOSE OF Btn_Salvar IN FRAME F_Config /* Actualiza Reglamento */
DO:
   IF W_CedUnaVez NE " " THEN DO:
      MESSAGE "Actualizar en la Configuracion Vigente de la Linea," SKIP
              "Y tiene Una Ced/Nit en Pantalla...?"
              "Operacion rechazada."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
      RETURN.
   END.

   IF AVAIL(Cfg_RegCredito) AND W_CedTemp NE " " THEN DO:
      MESSAGE "Actualizar en la Configuracion Vigente de la Linea," SKIP
              "Y Elimin¢ Ced/Nit de la Pantalla...?" SKIP
              "                   Continue solo si esta Segura(o)...?."
              VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
              TITLE "CONFIRMAR MODIFICAR REGLAMENTO" UPDATE W_RptaDRC AS LOG.
      IF NOT W_RptaDRC THEN 
         RETURN.
   END.
    
   MESSAGE "La Configuracion del Reglamento Vigente sera modificada." SKIP
           "                   Continue solo si esta Segura(o)...?."
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
           TITLE "CONFIRMAR MODIFICAR REGLAMENTO" UPDATE W_RptaRC AS LOG.
   IF NOT W_RptaRC THEN 
      RETURN.
    
   ASSIGN FRAME F_Config:SENSITIVE = FALSE.
   VIEW FRAME F_Obs.
   APPLY "Entry" TO W_Obs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Obs
&Scoped-define SELF-NAME BUTTON-161
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-161 W-Cfg_RegCreditos
ON CHOOSE OF BUTTON-161 IN FRAME F_Obs /* Button 161 */
DO:
  HIDE FRAME F_Obs.
  ASSIGN FRAME F_Config:SENSITIVE = TRUE.

  RUN Trigger_Salvar.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Retorno
&Scoped-define SELF-NAME BUTTON-163
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-163 W-Cfg_RegCreditos
ON CHOOSE OF BUTTON-163 IN FRAME F_Retorno /* Button 163 */
DO:
   BUFFER-COPY CopCfg_RegCredito TO Cfg_RegCredito.
   FIND CURRENT Cfg_RegCredito NO-LOCK NO-ERROR.

   RUN Mostrar_Config.

   ASSIGN FRAME F_Retorno:VISIBLE  = FALSE
          FRAME F_Config:SENSITIVE = TRUE.
          
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Config
&Scoped-define SELF-NAME Cfg_RegCredito.Cod_Credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_RegCredito.Cod_Credito W-Cfg_RegCreditos
ON LEAVE OF Cfg_RegCredito.Cod_Credito IN FRAME F_Config /* Código de Producto */
DO:
  IF SELF:SCREEN-VALUE GT "000" THEN 
     FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ INTEG(SELF:SCREEN-VALUE) NO-LOCK NO-ERROR.
  
  IF NOT AVAIL(Pro_Creditos) OR SELF:SCREEN-VALUE LE "000" THEN
     ASSIGN FRAME F_Config:SENSITIVE = FALSE
            FRAME F_Consul:VISIBLE   = TRUE.
  ELSE IF AVAIL(Pro_Creditos) THEN DO:
     RUN Nom_Tipo.
     ASSIGN Cfg_RegCredito.Cod_Credito:SCREEN-VALUE = STRING(Pro_Credito.Cod_Credito)
            NomLinea:SCREEN-VALUE                   = Pro_Credito.Nom_Producto. 

     FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ Pro_Credito.Cod_Credito NO-LOCK NO-ERROR.
     IF AVAIL(Cfg_RegCredito) THEN DO:
        MESSAGE "Ya Existe la configuracion para esta linea." SKIP
                "               Seleccionela del Browser de Consulta."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN Cfg_RegCredito.Cod_Credito:SCREEN-VALUE = "000"
               NomLinea:SCREEN-VALUE                   = "".
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cfg_RegCredito.Tipo_Actividad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cfg_RegCredito.Tipo_Actividad W-Cfg_RegCreditos
ON LEAVE OF Cfg_RegCredito.Tipo_Actividad IN FRAME F_Config /* Para Tipo de Actividad */
DO:
  IF SELF:SCREEN-VALUE NE "Empleado" AND
     SELF:SCREEN-VALUE NE "Estudiante" AND
     SELF:SCREEN-VALUE NE "Ama de Casa" AND
     SELF:SCREEN-VALUE NE "Jubilado"    AND
     SELF:SCREEN-VALUE NE "Independiente" AND 
     SELF:SCREEN-VALUE NE "" THEN DO:
     MESSAGE "Solo pueden ser:" SKIP
             "Empleado,Estudiante,Ama de Casa,Jubilado,Independiente," SKIP
             "O en Blanco." 
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     ASSIGN SELF:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CedUnaVez
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CedUnaVez W-Cfg_RegCreditos
ON LEAVE OF W_CedUnaVez IN FRAME F_Config
DO:
  ASSIGN W_CedUnaVez
         W_NomCte:SCREEN-VALUE = "".
    
  IF W_CedUnaVez NE " " THEN DO:
     FIND FIRST Clientes WHERE Clientes.Nit  EQ W_CedUnaVez 
                         AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
     IF AVAIL(Clientes) THEN                                                                     
        ASSIGN W_NomCte:SCREEN-VALUE = TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2) +    
                                       " " + TRIM(Clientes.Nombre).
     ELSE DO:
        MESSAGE "No Existe Activo en Clientes...?, Rectifique por Favor." 
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        ASSIGN W_CedUnaVez              = " "
               W_CedUnaVez:SCREEN-VALUE = "" 
               W_NomCte:SCREEN-VALUE    = "".    
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Obs
&Scoped-define SELF-NAME W_Obs
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Obs W-Cfg_RegCreditos
ON LEAVE OF W_Obs IN FRAME F_Obs
DO:
  ASSIGN W_Obs.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Config
&Scoped-define BROWSE-NAME BROWSE-1
&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Cfg_RegCreditos 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  RUN enable_UI.

  ON RETURN TAB.

  RELEASE Cfg_RegCredito.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
     WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Cfg_RegCreditos  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Cfg_RegCreditos)
  THEN DELETE WIDGET W-Cfg_RegCreditos.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Cfg_RegCreditos  _DEFAULT-ENABLE
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
  DISPLAY W_NomCte W_CedUnaVez NomLinea Tipo_Cred 
      WITH FRAME F_Config IN WINDOW W-Cfg_RegCreditos.
  IF AVAILABLE Cfg_RegCredito THEN 
    DISPLAY Cfg_RegCredito.Cod_Credito Cfg_RegCredito.Id_Vigente 
          Cfg_RegCredito.Id_Transitorio Cfg_RegCredito.Id_Libranza 
          Cfg_RegCredito.Id_Empleado Cfg_RegCredito.Id_Privilegiado 
          Cfg_RegCredito.Id_UnicoRotatorio Cfg_RegCredito.Id_LineaEspecial 
          Cfg_RegCredito.Id_MismaLinea Cfg_RegCredito.Agencia_Exigida 
          Cfg_RegCredito.Nro_DiasVencidos Cfg_RegCredito.DiasVencidos_Fiando 
          Cfg_RegCredito.Vr_MontoCodeud Cfg_RegCredito.Tiempo_Antig 
          Cfg_RegCredito.Tipo_Actividad Cfg_RegCredito.Vr_AporteMinimo 
          Cfg_RegCredito.Vr_Salario Cfg_RegCredito.Porcentaje_Cancel 
          Cfg_RegCredito.Rangos_Monto[1] Cfg_RegCredito.Rangos_Plazo[1] 
          Cfg_RegCredito.Rangos_Monto[2] Cfg_RegCredito.Rangos_Plazo[2] 
          Cfg_RegCredito.Rangos_Monto[3] Cfg_RegCredito.Rangos_Plazo[3] 
          Cfg_RegCredito.Rangos_Monto[4] Cfg_RegCredito.Rangos_Plazo[4] 
          Cfg_RegCredito.Rangos_Monto[5] Cfg_RegCredito.Rangos_Plazo[5] 
          Cfg_RegCredito.Edad_MinMax[1] Cfg_RegCredito.Edad_MinMax[2] 
          Cfg_RegCredito.Plazo_MinMax[1] Cfg_RegCredito.Plazo_MinMax[2] 
          Cfg_RegCredito.Monto_MinMax[1] Cfg_RegCredito.Monto_MinMax[2] 
          Cfg_RegCredito.Tipo_Cliente Cfg_RegCredito.Tip_Contrato 
      WITH FRAME F_Config IN WINDOW W-Cfg_RegCreditos.
  ENABLE Btn_ConsTemp W_CedUnaVez Cfg_RegCredito.Id_Vigente 
         Cfg_RegCredito.Id_Transitorio Cfg_RegCredito.Id_Libranza 
         Cfg_RegCredito.Id_Empleado Cfg_RegCredito.Id_Privilegiado 
         Cfg_RegCredito.Id_UnicoRotatorio Cfg_RegCredito.Id_LineaEspecial 
         Cfg_RegCredito.Id_MismaLinea Cfg_RegCredito.Agencia_Exigida 
         Cfg_RegCredito.Nro_DiasVencidos Cfg_RegCredito.DiasVencidos_Fiando 
         Cfg_RegCredito.Vr_MontoCodeud Cfg_RegCredito.Tiempo_Antig 
         Cfg_RegCredito.Tipo_Actividad Cfg_RegCredito.Vr_AporteMinimo 
         Cfg_RegCredito.Vr_Salario Cfg_RegCredito.Porcentaje_Cancel 
         Cfg_RegCredito.Rangos_Monto[1] Cfg_RegCredito.Rangos_Plazo[1] 
         Cfg_RegCredito.Rangos_Monto[2] Cfg_RegCredito.Rangos_Plazo[2] 
         Cfg_RegCredito.Rangos_Monto[3] Cfg_RegCredito.Rangos_Plazo[3] 
         Cfg_RegCredito.Rangos_Monto[4] Cfg_RegCredito.Rangos_Plazo[4] 
         Cfg_RegCredito.Rangos_Monto[5] Cfg_RegCredito.Rangos_Plazo[5] 
         Cfg_RegCredito.Edad_MinMax[1] Cfg_RegCredito.Edad_MinMax[2] 
         Cfg_RegCredito.Plazo_MinMax[1] Cfg_RegCredito.Plazo_MinMax[2] 
         Cfg_RegCredito.Monto_MinMax[1] Cfg_RegCredito.Monto_MinMax[2] 
         Btn_Salvar BROWSE-1 Btn_Ingr Cfg_RegCredito.Tipo_Cliente 
         Cfg_RegCredito.Tip_Contrato Btn_RestOrig RECT-302 RECT-303 RECT-304 
      WITH FRAME F_Config IN WINDOW W-Cfg_RegCreditos.
  {&OPEN-BROWSERS-IN-QUERY-F_Config}
  ENABLE BROWSE-3 Btn_Regresa 
      WITH FRAME F_Consul IN WINDOW W-Cfg_RegCreditos.
  {&OPEN-BROWSERS-IN-QUERY-F_Consul}
  DISPLAY W_Obs 
      WITH FRAME F_Obs IN WINDOW W-Cfg_RegCreditos.
  ENABLE W_Obs BUTTON-161 
      WITH FRAME F_Obs IN WINDOW W-Cfg_RegCreditos.
  {&OPEN-BROWSERS-IN-QUERY-F_Obs}
  ENABLE BUTTON-163 
      WITH FRAME F_Retorno IN WINDOW W-Cfg_RegCreditos.
  {&OPEN-BROWSERS-IN-QUERY-F_Retorno}
  VIEW W-Cfg_RegCreditos.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Config W-Cfg_RegCreditos 
PROCEDURE Mostrar_Config :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ Cfg_RegCredito.Cod_Credito NO-LOCK NO-ERROR.
  RUN Nom_Tipo.

  W_CedTemp = " ".
    
  DISPLAY Cfg_RegCredito.Cod_Credito Cfg_RegCredito.Id_Vigente 
          Cfg_RegCredito.Edad_MinMax[1] Cfg_RegCredito.Plazo_MinMax[1] 
          /*Cfg_RegCredito.Nro_Codeudores*/ Cfg_RegCredito.Nro_DiasVencidos 
          Cfg_RegCredito.Edad_MinMax[2] Cfg_RegCredito.Plazo_MinMax[2] 
          Cfg_RegCredito.Tipo_Cliente Cfg_RegCredito.Id_Empleado 
          Cfg_RegCredito.Porcentaje_Cancel Cfg_RegCredito.Id_Libranza 
          Cfg_RegCredito.Tiempo_Antig Cfg_RegCredito.Id_LineaEspecial 
          Cfg_RegCredito.Tipo_Actividad Cfg_RegCredito.Tip_Contrato 
          Cfg_RegCredito.Id_MismaLinea Cfg_RegCredito.Id_Privilegiado 
          Cfg_RegCredito.Agencia_Exigida Cfg_RegCredito.Id_Transitorio 
          Cfg_RegCredito.Vr_AporteMinimo Cfg_RegCredito.Id_UnicoRotatorio 
          Cfg_RegCredito.Vr_Salario Cfg_RegCredito.Monto_MinMax[1] 
          Cfg_RegCredito.Monto_MinMax[2] 
          Cfg_RegCredito.Rangos_Monto[1] Cfg_RegCredito.Rangos_Plazo[1] 
          Cfg_RegCredito.Rangos_Monto[2] Cfg_RegCredito.Rangos_Plazo[2] 
          Cfg_RegCredito.Rangos_Monto[3] Cfg_RegCredito.Rangos_Plazo[3] 
          Cfg_RegCredito.Rangos_Monto[4] Cfg_RegCredito.Rangos_Plazo[4] 
          Cfg_RegCredito.Rangos_Monto[5] Cfg_RegCredito.Rangos_Plazo[5] 
          Cfg_RegCredito.DiasVencidos_Fiando Cfg_RegCredito.Vr_MontoCodeud
      WITH FRAME F_Config IN WINDOW W-Cfg_RegCreditos.

  ASSIGN NomLinea:SCREEN-VALUE = Pro_Credito.Nom_Producto
         Cfg_RegCredito.Cod_Credito:SENSITIVE = FALSE.
    
  FIND FIRST Cfg_OrigRegCredito WHERE 
             Cfg_OrigRegCredito.Cod_Credito EQ Cfg_RegCredito.Cod_Credito NO-LOCK NO-ERROR.
  ASSIGN W_CedUnaVez              = Cfg_OrigRegCredito.CedNit_UnaVez
         W_CedUnaVez:SCREEN-VALUE = W_CedUnaVez.
  IF W_CedUnaVez NE " " THEN
     W_CedTemp = W_CedUnaVez.

  APPLY "Leave" TO W_CedUnaVez.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Nom_Tipo W-Cfg_RegCreditos 
PROCEDURE Nom_Tipo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF Pro_Creditos.Tip_Credito = 1 THEN
     ASSIGN Tipo_Cred:SCREEN-VALUE IN FRAME F_Config = "    CONSUMO".
  ELSE IF Pro_Creditos.Tip_Credito = 2 THEN
     ASSIGN Tipo_Cred:SCREEN-VALUE = "    COMERCIAL".
  ELSE IF Pro_Creditos.Tip_Credito = 3 THEN
     ASSIGN Tipo_Cred:SCREEN-VALUE = "    HIPOTECARIO".
  ELSE IF Pro_Creditos.Tip_Credito = 4 THEN
     ASSIGN Tipo_Cred:SCREEN-VALUE = "    MICROCREDITO".
    
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Reab_Query W-Cfg_RegCreditos 
PROCEDURE Reab_Query :
/*------------------------------------------------------------------------------
  Purpose:     
------------------------------------------------------------------------------*/
  DEFI VAR W_RowidCfg AS ROWID.

  FIND CURRENT Cfg_RegCredito NO-LOCK NO-ERROR.
  ASSIGN W_RowidCfg = ROWID(Cfg_RegCredito).                                                        
                                                                                                    
  CLOSE QUERY BROWSE-1.                                                                                    
  OPEN QUERY BROWSE-1 FOR EACH Cfg_RegCredito NO-LOCK,                                                     
                          EACH PRO_Creditos NO-LOCK WHERE                                                  
                          PRO_Creditos.Cod_Credito EQ Cfg_RegCredito.Cod_Credito INDEXED-REPOSITION.       
  REPOSITION BROWSE-1 TO ROWID W_RowidCfg.

  ASSIGN W_Obs = ""
         W_Obs:SCREEN-VALUE IN FRAME F_Obs = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salvar W-Cfg_RegCreditos 
PROCEDURE Salvar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFI VAR W_CopObs LIKE Cfg_OrigRegCredito.Observ INIT "".

  DO WITH FRAME F_Config. END.

  ASSIGN  Cfg_RegCredito.Cod_Credito Cfg_RegCredito.Id_Vigente 
          Cfg_RegCredito.Edad_MinMax[1] Cfg_RegCredito.Plazo_MinMax[1] 
          /*Cfg_RegCredito.Nro_Codeudores*/ Cfg_RegCredito.Nro_DiasVencidos 
          Cfg_RegCredito.Edad_MinMax[2] Cfg_RegCredito.Plazo_MinMax[2] 
          Cfg_RegCredito.Tipo_Cliente Cfg_RegCredito.Id_Empleado 
          Cfg_RegCredito.Porcentaje_Cancel Cfg_RegCredito.Id_Libranza 
          Cfg_RegCredito.Tiempo_Antig Cfg_RegCredito.Id_LineaEspecial.

  ASSIGN  Cfg_RegCredito.Tipo_Actividad Cfg_RegCredito.Tip_Contrato 
          Cfg_RegCredito.Id_MismaLinea Cfg_RegCredito.Id_Privilegiado 
          Cfg_RegCredito.Agencia_Exigida Cfg_RegCredito.Id_Transitorio 
          Cfg_RegCredito.Vr_AporteMinimo Cfg_RegCredito.Id_UnicoRotatorio 
          Cfg_RegCredito.Vr_Salario Cfg_RegCredito.Monto_MinMax[1] 
          Cfg_RegCredito.Monto_MinMax[2]. 

  ASSIGN  Cfg_RegCredito.Rangos_Monto[1] Cfg_RegCredito.Rangos_Plazo[1] 
          Cfg_RegCredito.Rangos_Monto[2] Cfg_RegCredito.Rangos_Plazo[2] 
          Cfg_RegCredito.Rangos_Monto[3] Cfg_RegCredito.Rangos_Plazo[3] 
          Cfg_RegCredito.Rangos_Monto[4] Cfg_RegCredito.Rangos_Plazo[4] 
          Cfg_RegCredito.Rangos_Monto[5] Cfg_RegCredito.Rangos_Plazo[5]
          Cfg_RegCredito.DiasVencidos_Fiando Cfg_RegCredito.Vr_MontoCodeud. 

  ASSIGN  Cfg_RegCredito.Observ = Cfg_RegCredito.Observ + "; FyUs." + STRING(W_Fecha,"999999") + "-" +
                                  W_Usuario + ": " + W_Obs.

  FIND FIRST Cfg_OrigRegCredito WHERE 
             Cfg_OrigRegCredito.Cod_Credito EQ Cfg_RegCredito.Cod_Credito NO-ERROR.

  IF NOT AVAIL(Cfg_OrigRegCredito) THEN DO:
     MESSAGE "La linea no Existia en La Tabla Temporal, Es Creada como copia de la Operativa."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
     CREATE Cfg_OrigRegCredito.
     BUFFER-COPY Cfg_RegCredito TO Cfg_OrigRegCredito.   
  END.     
 
  IF NOT W_SiTemp THEN
     MESSAGE "La Configuracion Vigente fue Salvada Ok."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Salva_Temp W-Cfg_RegCreditos 
PROCEDURE Salva_Temp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FOR EACH CopCfg_RegCredito: DELETE CopCfg_RegCredito. END.
  CREATE CopCfg_RegCredito.
  BUFFER-COPY Cfg_RegCredito TO CopCfg_RegCredito.
    
  FIND CURRENT Cfg_RegCredito NO-ERROR.  
  RUN Salvar.
    
  FIND FIRST Cfg_OrigRegCredito WHERE 
             Cfg_OrigRegCredito.Cod_Credito EQ Cfg_RegCredito.Cod_Credito NO-ERROR.
  BUFFER-COPY Cfg_RegCredito TO Cfg_OrigRegCredito. 
  ASSIGN Cfg_OrigRegCredito.CedNit_UnaVez = W_CedUnaVez.
  FIND CURRENT Cfg_OrigRegCredito NO-LOCK NO-ERROR.  
      
  BUFFER-COPY CopCfg_RegCredito TO Cfg_RegCredito.  
  FIND CURRENT Cfg_RegCredito NO-LOCK NO-ERROR.
    
  MESSAGE "La configuracion fue Salvada en la Tabla TEMPORAL Ok."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Trigger_Salvar W-Cfg_RegCreditos 
PROCEDURE Trigger_Salvar :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  IF W_Obs LE " " THEN DO:
     MESSAGE "Debe Documentar la Modificacion...Operacion Rechazada."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     RETURN.
  END.

  IF NOT AVAIL(Cfg_RegCredito) THEN DO: 
     FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ 
                INTEG(Cfg_RegCredito.Cod_Credito:SCREEN-VALUE IN FRAME F_Config) NO-LOCK NO-ERROR.
     IF NOT AVAIL(Pro_Creditos) THEN DO:
        MESSAGE "La Linea No Existe en la Config.de los Pro_Creditos." SKIP
                "Operacion rechazada."
            VIEW-AS ALERT-BOX ERROR.
        RETURN.
     END.

     FIND FIRST Cfg_RegCredito WHERE Cfg_RegCredito.Cod_Credito EQ Pro_Credito.Cod_Credito NO-LOCK NO-ERROR.
     IF AVAIL(Cfg_RegCredito) THEN DO:
        MESSAGE "Ya Existe la configuracion para esta linea." SKIP
                "               Seleccionela del Browser de Consulta."
            VIEW-AS ALERT-BOX ERROR.
        ASSIGN Cfg_RegCredito.Cod_Credito:SCREEN-VALUE = "000"
               NomLinea:SCREEN-VALUE                   = "".
        RETURN.
     END.
     ELSE CREATE Cfg_RegCredito.
  END.
  ELSE FIND CURRENT Cfg_RegCredito NO-ERROR.
               
  RUN Salvar.

  RUN Reab_Query.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

