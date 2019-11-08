&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 

/* oakley */

CREATE WIDGET-POOL.

DEFINE VAR Informe AS CHARACTER FORMAT "X(30)".
/* ***************************  Definitions  ************************** */

DEFINE VARIABLE Exceso_Ahorros AS DECIMAL INITIAL 3200000.
DEFINE VARIABLE Exceso_Creditos AS DECIMAL INITIAL 102000000.
DEFINE VARIABLE Fec_Validacion  AS DATE INITIAL TODAY.
DEFINE VARIABLE CuentaDb AS CHARACTER INITIAL "26100502".
DEFINE VARIABLE CuentaCr AS CHARACTER INITIAL "27059501".
DEFINE VARIABLE CuentaAs AS CHARACTER INITIAL "24959501".
DEFINE VARIABLE Pr_Ahorro AS DECIMAL FORMAT ">.999999" INITIAL 0.00040.
DEFINE VARIABLE Pr_AntCredito AS DECIMAL FORMAT ">.999999" INITIAL 0.00039.
DEFINE VARIABLE Pr_DesCredito AS DECIMAL FORMAT ">.999999" INITIAL 0.00039.
DEFINE VARIABLE Pr_PreCredito AS DECIMAL FORMAT ">.999999" INITIAL 0.00090.
DEFINE VARIABLE NitDB AS CHARACTER INITIAL "860524654".

{Incluido/VARIABLE.I "SHARED"}
{Incluido/VARCON.I "SHARED"}

DEFINE VARIABLE TotAhoNit AS DECIMAL.
DEFINE VARIABLE TotAntCreNit AS DECIMAL.
DEFINE VARIABLE TotDesCreNit AS DECIMAL.
DEFINE VARIABLE TotAntCreNit70 AS DECIMAL.
DEFINE VARIABLE TotDesCreNit70 AS DECIMAL.
DEFINE VAR W_Tit AS CHARACTER FORMAT "X(50)".
DEFINE VAR W_Inf AS CHARACTER FORMAT "X(3)".
DEFINE VAR W_Tip AS INTEGER FORMAT "9".
DEFINE VAR Doc AS INTEGER.
DEFINE VAR W_Ok AS LOGICAL.
DEFINE VAR AgeIni LIKE Agencias.Agencia.
DEFINE VAR AgeFin LIKE Agencias.Agencia.
DEFINE VAR TotalAhorros LIKE Ahorros.Sdo_Disponible.
DEFINE VAR TotalCreditos LIKE Ahorros.Sdo_Disponible.
 DEFINE VAR j AS DECIMAL.
 DEFINE VAR k AS INTEGER.

DEFINE TEMP-TABLE TAho
  FIELD TA_Agencia    LIKE Agencias.Agencia
  FIELD TA_NomAge     LIKE Agencias.Nombre
  FIELD TA_TotAhorros AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TA_TotTer     AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TA_TotJur     AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TA_Exceso     AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TA_AA70       AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TA_Seguro     AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TA_Estado     AS LOGICAL.

DEFINE TEMP-TABLE TCre
  FIELD TC_Agencia     LIKE Agencias.Agencia
  FIELD TC_NomAge LIKE Agencias.Nombre
  FIELD TC_TotAntCre   AS DECIMAL FORMAT ">>,>>>,>>>,>>9" /*Antes de fecha de validacion*/
  FIELD TC_TotDesCre   AS DECIMAL FORMAT ">>,>>>,>>>,>>9" /*Despues de fecha de validacion*/
  FIELD TC_Exceso      AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TC_Cr70        AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TC_Juridicas   AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TC_Atrasados   AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TC_Seguro      AS DECIMAL FORMAT ">>,>>>,>>>,>>9"
  FIELD TC_Pree        AS LOGICAL INITIAL NO
  FIELD TC_Estado      AS LOGICAL.
  
DEFINE TEMP-TABLE TInfo
  FIELD TI_Age LIKE Agencias.Agencia
  FIELD TI_Nit LIKE Clientes.Nit
  FIELD TI_Tip AS INTEGER FORMAT "9"
  FIELD TI_Ind AS CHARACTER FORMAT "X(3)" 
        /*para identificar en la tabla a cual exepcion pertenece
          EXC - Exceso
          MAY - Mayor de 70
          JUR - Persona Juridica
          ATR - Atrasado*/ 
  FIELD TI_Val1 LIKE Ahorros.Sdo_Disponible
  FIELD TI_Val2 LIKE Ahorros.Sdo_Disponible.

DEFINE TEMP-TABLE TmpCtble
       FIELD agen       AS INTEGER FORMAT "z9"
       FIELD sdo_cap    AS DECIMAL FORMAT "zzz,zzz,zzz,zz9.99"
       FIELD Nro_Cre    AS INTEGER FORMAT "zzz,zz9"
       FIELD AsoMay     AS INTEGER FORMAT "zzz,zz9"
       FIELD AsoMen     AS intEGER FORMAT "zzz,zz9"
       FIELD AsoJur     AS DECIMAL FORMAT "zzz,zz9"
       FIELD TotAso     AS DECIMAL FORMAT "Zzz,zz9"
       INDEX idxTmp Agen.


DEFI VAR wsdo_cap    AS DECIMAL FORMAT "zzz,zzz,zzz,zz9.99" INITIAL 0.00.
DEFI VAR wNro_Cre    AS INTEGER FORMAT "zzz,zz9" INITIAL 0.
DEFI VAR wAsoMay     AS INTEGER FORMAT "zzz,zz9" INITIAL 0.
DEFI VAR wAsoMen     AS intEGER FORMAT "zzz,zz9" INITIAL 0.
DEFI VAR wAsoJur     AS DECIMAL FORMAT "zzz,zz9" INITIAL 0.
DEFI VAR wTotAso     AS DECIMAL FORMAT "Zzz,zz9" INITIAL 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Info
&Scoped-define BROWSE-NAME BAho

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TAho TCre

/* Definitions for BROWSE BAho                                          */
&Scoped-define FIELDS-IN-QUERY-BAho TA_Age TA_NomAge TA_TotAhorros TA_Exceso TA_AA70 TA_TotTer TA_TotJur TA_Seguro   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BAho   
&Scoped-define SELF-NAME BAho
&Scoped-define QUERY-STRING-BAho FOR EACH TAho NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BAho OPEN QUERY {&SELF-NAME} FOR EACH TAho NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BAho TAho
&Scoped-define FIRST-TABLE-IN-QUERY-BAho TAho


/* Definitions for BROWSE BCre                                          */
&Scoped-define FIELDS-IN-QUERY-BCre TC_Age TC_NomAge TC_TotAntCre TC_TotDesCre TC_Exceso TC_Cr70 TC_Juridicas /* TC_Atrasados */ TC_Seguro   
&Scoped-define ENABLED-FIELDS-IN-QUERY-BCre   
&Scoped-define SELF-NAME BCre
&Scoped-define QUERY-STRING-BCre FOR EACH TCre NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-BCre OPEN QUERY {&SELF-NAME} FOR EACH TCre NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-BCre TCre
&Scoped-define FIRST-TABLE-IN-QUERY-BCre TCre


/* Definitions for FRAME F_Seg                                          */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Seg ~
    ~{&OPEN-QUERY-BAho}~
    ~{&OPEN-QUERY-BCre}

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RTipo BUTTON-177 BUTTON-178 
&Scoped-Define DISPLAYED-OBJECTS RTipo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-177 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     LABEL "Button 177" 
     SIZE 11 BY 1.88.

DEFINE BUTTON BUTTON-178 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 178" 
     SIZE 11 BY 1.88.

DEFINE VARIABLE RTipo AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ahorros + Aportes con Excendente", 1,
"Ahorros + Aportes de Clientes Mayores a 70 años", 2,
"Ahorros + Aportes de Clientes No Asociados", 3,
"Ahorros + Aportes de Entidades Juridicas", 4,
"Créditos con Excedente", 5,
"Créditos de Clientes Mayores a 70 Años", 6,
"Créditos de Personas Juridicas", 7,
"Créditos con Atraso Mayor a 1 Año", 8,
"Informe de Seguros de Ahorro", 9,
"Informe de Seguros de Crédito", 10,
"Informe de Totales de Asociados y Creditos", 11
     SIZE 47 BY 8.35 NO-UNDO.

DEFINE IMAGE IMAGE-2
     FILENAME "imagenes/clock05.ico":U
     SIZE 5 BY 1.35.

DEFINE RECTANGLE R1
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R2
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R3
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R4
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R5
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R6
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R7
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R8
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE RECTANGLE R9
     EDGE-PIXELS 2 GRAPHIC-EDGE  
     SIZE 2 BY 1.08
     BGCOLOR 15 .

DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 15 BY 1.12
     BGCOLOR 8 .

DEFINE BUTTON Btn_Contabilizar 
     LABEL "Contabilizar" 
     SIZE 21 BY 1.35.

DEFINE BUTTON Btn_Informe 
     LABEL "Informe Excensiones" 
     SIZE 21 BY 1.38.

DEFINE BUTTON Btn_Procesar 
     LABEL "Procesar" 
     SIZE 21 BY 1.35.

DEFINE BUTTON BUTTON-173 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 173" 
     SIZE 15 BY 1.38.

DEFINE BUTTON BUTTON-176 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 176" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(256)":U INITIAL "000 - Todas" 
     LABEL "Agencia a Procesar" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "000 - Todas" 
     DROP-DOWN-LIST
     SIZE 47.14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Top_Ahorro AS DECIMAL FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Tope Excedente Ahorros" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Top_Credito AS DECIMAL FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Tope Excedente de Créditos" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTA_70 AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTA_Aho AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTA_Exe AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTA_Jur AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTA_NAh AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Núm.Cuentas" 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTA_NCl AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTC_70 AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTC_Ant AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 13.57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTC_Cre AS DECIMAL FORMAT ">>>,>>>,>>>,>>>,>>9":U INITIAL 0 
     LABEL "Total de Creditos" 
     VIEW-AS FILL-IN 
     SIZE 25 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTC_Des AS DECIMAL FORMAT ">>>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTC_Exe AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTC_Jur AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.43 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TTC_NCr AS DECIMAL FORMAT ">>,>>9":U INITIAL 0 
     LABEL "Num.Creditos" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_PorAho AS DECIMAL FORMAT ">>9.99999":U INITIAL 0 
     LABEL "Porcentaje de Ahorros" 
     VIEW-AS FILL-IN 
     SIZE 8 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotSegAho AS DECIMAL FORMAT "->>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12.57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_TotSegCre AS DECIMAL FORMAT ">>,>>>,>>>,>>9":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 21 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY BAho FOR 
      TAho SCROLLING.

DEFINE QUERY BCre FOR 
      TCre SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE BAho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BAho wWin _FREEFORM
  QUERY BAho NO-LOCK DISPLAY
      TA_Age        COLUMN-LABEL "Age"
  TA_NomAge     COLUMN-LABEL "Nombre"        WIDTH 23
  TA_TotAhorros COLUMN-LABEL "Total Ahorros" WIDTH 13
  TA_Exceso     COLUMN-LABEL "Exceso"        WIDTH 13
  TA_AA70       COLUMN-LABEL "Cli.>70 Años" WIDTH 13
  TA_TotTer     COLUMN-LABEL "Cli. No Asoc" WIDTH 13
  TA_TotJur     COLUMN-LABEL "Ent.Juridicas" WIDTH 13
  TA_Seguro     COLUMN-LABEL "Total" WIDTH 13 FORMAT "->>,>>>,>>>,>>9"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 110 BY 5.65
         BGCOLOR 15 FONT 4 ROW-HEIGHT-CHARS .54 EXPANDABLE.

DEFINE BROWSE BCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS BCre wWin _FREEFORM
  QUERY BCre NO-LOCK DISPLAY
      TC_Age          COLUMN-LABEL "Age"
  TC_NomAge       COLUMN-LABEL "Nombre" WIDTH 20
  TC_TotAntCre    COLUMN-LABEL "Anteriores"   WIDTH 12
  TC_TotDesCre    COLUMN-LABEL "Posteriores"  WIDTH 12  
  TC_Exceso       COLUMN-LABEL "Exceso" WIDTH 12
  TC_Cr70         COLUMN-LABEL "70 Años" WIDTH 12
  TC_Juridicas    COLUMN-LABEL "Juridicas"        WIDTH 12
  /* TC_Atrasados    COLUMN-LABEL "Cre.Atrasados"    WIDTH 12 */
  TC_Seguro   COLUMN-LABEL "Total" WIDTH 14
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 110 BY 5.65
         BGCOLOR 15 FONT 4 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Seg
     Cmb_Agencias AT ROW 1.54 COL 20 COLON-ALIGNED
     BUTTON-173 AT ROW 1.54 COL 97
     Top_Ahorro AT ROW 3.04 COL 48.29 COLON-ALIGNED
     W_PorAho AT ROW 3.04 COL 83.43 COLON-ALIGNED
     BAho AT ROW 3.96 COL 3
     TTA_NAh AT ROW 9.62 COL 14 COLON-ALIGNED
     TTA_Aho AT ROW 9.62 COL 28.14 COLON-ALIGNED NO-LABEL
     TTA_Exe AT ROW 9.62 COL 41.72 COLON-ALIGNED NO-LABEL
     TTA_70 AT ROW 9.62 COL 55.29 COLON-ALIGNED NO-LABEL
     TTA_NCl AT ROW 9.62 COL 68.86 COLON-ALIGNED NO-LABEL
     TTA_Jur AT ROW 9.62 COL 82.43 COLON-ALIGNED NO-LABEL
     W_TotSegAho AT ROW 9.62 COL 96 COLON-ALIGNED NO-LABEL
     Top_Credito AT ROW 11.5 COL 53 COLON-ALIGNED
     BCre AT ROW 12.58 COL 3
     TTC_NCr AT ROW 18.23 COL 13 COLON-ALIGNED
     TTC_Ant AT ROW 18.23 COL 24 COLON-ALIGNED NO-LABEL
     TTC_Des AT ROW 18.23 COL 37.72 COLON-ALIGNED NO-LABEL
     TTC_Exe AT ROW 18.23 COL 50.29 COLON-ALIGNED NO-LABEL
     TTC_70 AT ROW 18.23 COL 62.86 COLON-ALIGNED NO-LABEL
     TTC_Jur AT ROW 18.23 COL 75.43 COLON-ALIGNED NO-LABEL
     W_TotSegCre AT ROW 18.23 COL 88 COLON-ALIGNED NO-LABEL
     TTC_Cre AT ROW 19.12 COL 25 COLON-ALIGNED
     BtnDone AT ROW 19.85 COL 95
     Btn_Procesar AT ROW 20.38 COL 3
     Btn_Contabilizar AT ROW 20.38 COL 25
     Btn_Informe AT ROW 20.38 COL 47
     BUTTON-176 AT ROW 21.19 COL 101
     "Resultados de Ahorros" VIEW-AS TEXT
          SIZE 20 BY .88 AT ROW 2.88 COL 4
          FGCOLOR 7 
     "Resultados de Créditos" VIEW-AS TEXT
          SIZE 23 BY .88 AT ROW 11.5 COL 3
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.14 BY 21.38
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F_Progreso
     IMAGE-2 AT ROW 1.15 COL 2
     R1 AT ROW 1.27 COL 23
     R2 AT ROW 1.27 COL 21
     R3 AT ROW 1.27 COL 19
     R4 AT ROW 1.27 COL 17
     R5 AT ROW 1.27 COL 15
     R6 AT ROW 1.27 COL 13
     R7 AT ROW 1.27 COL 11
     R8 AT ROW 1.27 COL 9
     R9 AT ROW 1.27 COL 7
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 71 ROW 1.27
         SIZE 25 BY 1.62
         BGCOLOR 17 .

DEFINE FRAME F_Info
     RTipo AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-177 AT ROW 1.54 COL 51
     BUTTON-178 AT ROW 6.65 COL 51
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 41 ROW 11.77
         SIZE 63 BY 9.69
         BGCOLOR 17 FONT 5
         TITLE "Informes de Excensiones".


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
         TITLE              = "SFG - Seguro de Crédito y Ahorro"
         HEIGHT             = 21.38
         WIDTH              = 113.14
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
ASSIGN FRAME F_Info:FRAME = FRAME F_Seg:HANDLE
       FRAME F_Progreso:FRAME = FRAME F_Seg:HANDLE.

/* SETTINGS FOR FRAME F_Info
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Info:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Seg
                                                                        */
/* BROWSE-TAB BAho W_PorAho F_Seg */
/* BROWSE-TAB BCre F_Info F_Seg */
/* SETTINGS FOR BUTTON Btn_Contabilizar IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Informe IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Top_Ahorro IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Top_Credito IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTA_70 IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTA_Aho IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTA_Exe IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTA_Jur IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTA_NAh IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTA_NCl IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTC_70 IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTC_Ant IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTC_Cre IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTC_Des IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTC_Exe IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTC_Jur IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN TTC_NCr IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_PorAho IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TotSegAho IN FRAME F_Seg
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_TotSegCre IN FRAME F_Seg
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BAho
/* Query rebuild information for BROWSE BAho
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TAho NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BAho */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE BCre
/* Query rebuild information for BROWSE BCre
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCre NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE BCre */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Seguro de Crédito y Ahorro */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Seguro de Crédito y Ahorro */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BAho
&Scoped-define FRAME-NAME F_Seg
&Scoped-define SELF-NAME BAho
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BAho wWin
ON ROW-DISPLAY OF BAho IN FRAME F_Seg
DO:
/*  ASSIGN W_TotSegAho = W_TotSegAho + TAho.TA_Seguro.
  DISPLAY W_TotSegAho WITH FRAME F_Seg.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BCre
&Scoped-define SELF-NAME BCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BCre wWin
ON ROW-DISPLAY OF BCre IN FRAME F_Seg
DO:
  /*ASSIGN W_TotSegCre = W_TotSegCre + TCre.TC_Seguro.
  DISPLAY W_TotSegCre WITH FRAME F_Seg.*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F_Seg /* Salir */
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


&Scoped-define SELF-NAME Btn_Contabilizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Contabilizar wWin
ON CHOOSE OF Btn_Contabilizar IN FRAME F_Seg /* Contabilizar */
DO:
  RUN Contabilizar.
  RUN Generar_Archivos.
  RUN InitializeObject.
  DISABLE Btn_Contabilizar Btn_Informe WITH FRAME F_Seg.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Informe
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Informe wWin
ON CHOOSE OF Btn_Informe IN FRAME F_Seg /* Informe Excensiones */
DO:
  VIEW FRAME F_Info.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Procesar wWin
ON CHOOSE OF Btn_Procesar IN FRAME F_Seg /* Procesar */
DO:
  FIND FIRST TAho WHERE TAho.TA_Estado EQ YES NO-ERROR.
  IF NOT AVAILABLE TAho THEN DO:
     FIND FIRST TCre WHERE TCre.TC_Estado EQ YES NO-ERROR.
     IF NOT AVAILABLE TCre THEN DO:
        MESSAGE "No existe ninguna Agencia para Procesar2" VIEW-AS ALERT-BOX.
        APPLY "entry" TO Cmb_Agencias IN FRAME F_Seg.
        RETURN NO-APPLY.
     END.
  END.
  FOR EACH TInfo: DELETE TInfo. END.
  FOR EACH TAho: ASSIGN TAho.TA_TotAhorros = 0
                        TAho.TA_TotJur     = 0
                        TAho.TA_TotTer     = 0
                        TAho.TA_Exceso     = 0
                        TAho.TA_AA70       = 0.
  END.
  FOR EACH TCre: ASSIGN TCre.TC_TotAntCre   = 0
                        TCre.TC_TotDesCre   = 0
                        TCre.TC_Exceso      = 0
                        TCre.TC_Cr70        = 0
                        TCre.TC_Juridicas   = 0
                        TCre.TC_Atrasados   = 0.
  END.
  
  ASSIGN FRAME F_Seg Cmb_Agencias.
  IF SUBSTRING(Cmb_Agencias,1,3) EQ "000" THEN DO:
      FIND FIRST TAho NO-ERROR.
      IF AVAILABLE TAho THEN AgeIni = TAho.TA_Age.
      FIND LAST TAho NO-ERROR.
      IF AVAILABLE TAho THEN AgeFin = TAho.TA_Age.
  END.
  ELSE
     ASSIGN AgeIni = INTEGER(SUBSTRING(Cmb_Agencias,1,3))
            AgeFin = AgeIni.

  IF Cmb_Agencias:NUM-ITEMS LT 2 THEN DO:
     MESSAGE "No se encuentra ninguna Agencias para procesar" VIEW-AS ALERT-BOX.
     APPLY "entry" TO Cmb_Agencias.
     RETURN NO-APPLY.
  END.

  VIEW FRAME F_Progreso.            
  RUN Procesar.
  HIDE FRAME F_Progreso.
  ENABLE Btn_Contabilizar Btn_Informe WITH FRAME F_Seg.
  ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
         R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15
         R9:BGCOLOR = 15.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-173
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-173 wWin
ON CHOOSE OF BUTTON-173 IN FRAME F_Seg /* Button 173 */
DO:
  RUN W-Infdia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Info
&Scoped-define SELF-NAME BUTTON-177
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-177 wWin
ON CHOOSE OF BUTTON-177 IN FRAME F_Info /* Button 177 */
DO:
  ASSIGN FRAME F_Info RTipo.
  CASE RTipo:
    WHEN 1 THEN ASSIGN W_Tip = 1 W_Inf = "EXC" W_Tit = "Ahorros + Aportes con Excedente a " + STRING(Exceso_Ahorros).
    WHEN 2 THEN ASSIGN W_Tip = 1 W_Inf = "MAY" W_Tit = "Ahorros + Aportes de Clientes Mayores a 70 Años ".
    WHEN 3 THEN ASSIGN W_Tip = 1 W_Inf = "TER" W_Tit = "Ahorros + Aportes de Clientes No Asociados".
    WHEN 4 THEN ASSIGN W_Tip = 1 W_Inf = "JUR" W_Tit = "Ahorros + Aportes de Entidades Juridicas".
    WHEN 5 THEN ASSIGN W_Tip = 2 W_Inf = "EXC" W_Tit = "Créditos con Excedente a " + STRING(Exceso_Creditos).
    WHEN 6 THEN ASSIGN W_Tip = 2 W_Inf = "MAY" W_Tit = "Creditos de Clientes Mayores a 70 Años ".
    WHEN 7 THEN ASSIGN W_Tip = 2 W_Inf = "JUR" W_Tit = "Creditos de Personas Juridicas".
    WHEN 8 THEN ASSIGN W_Tip = 2 W_Inf = "ATR" W_Tit = "Creditos Atrasados mas de 1 Año".
    WHEN 9 THEN ASSIGN W_Tip = 1 W_Inf = "SEG" W_Tit = "Seguro de Ahorros".
    WHEN 10 THEN ASSIGN W_Tip = 2 W_Inf = "SEG" W_Tit = "Seguro de Créditos".
    WHEN 11 THEN ASSIGN W_Tip = 2 W_Inf = "TOT" W_Tit = "Totales de Creditos".
  END CASE.
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_Pathspl + "Lst_Excensiones.lst".
  {incluido/imprimir.i "Listado"}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-178
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-178 wWin
ON CHOOSE OF BUTTON-178 IN FRAME F_Info /* Button 178 */
DO:
  HIDE FRAME F_Info.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME BAho
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_SecComprobante wWin 
PROCEDURE Asignar_SecComprobante :
IF AVAILABLE(Comprobantes) THEN
   DO:
     IF Comprobantes.Id_Consecutivo LT 3 THEN
        ASSIGN Comprobantes.Secuencia   = Comprobantes.Secuencia + 1
               Doc                      = Comprobantes.Secuencia.
     IF Doc EQ 0 THEN RETURN ERROR.
   END.
  ELSE RETURN ERROR.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Comprobante_AgeAdm wWin 
PROCEDURE Comprobante_AgeAdm :
DEFINE VARIABLE W_Com      LIKE Comprobantes.Comprobante.
 DEFINE VARIABLE ValorIgual AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
 DEFINE VARIABLE Comentario LIKE Mov_Contable.Comentario.
 
 FIND Comprobantes WHERE Comprobantes.Agencia     EQ 10  
                     AND Comprobantes.Comprobante EQ Varios.Comprobante NO-ERROR NO-WAIT.
 IF LOCKED Comprobantes THEN DO:
   REPEAT WHILE LOCKED Comprobantes:
      FIND Comprobantes WHERE Comprobantes.Agencia  EQ 10
                       AND Comprobantes.Comprobante EQ Varios.Comprobante NO-ERROR NO-WAIT.
   END.
 END.
 W_Com = Comprobantes.Cod_Formato.
 RUN Asignar_SecComprobante NO-ERROR.
 IF ERROR-STATUS:ERROR AND Doc EQ 0 THEN DO:
    MESSAGE "No se pudo Aumentar la secuencia del comprobante" SKIP
            "Se devuelve la operación de Seguros" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
 END.
 RELEASE Comprobantes.
 ASSIGN ValorIgual = 0.
 FOR EACH TAho WHERE TAho.TA_Estado EQ YES:
     IF TAho.TA_Seguro GT 0 THEN
      RUN Gra_Movimientos (INPUT 10, INPUT TAho.TA_Age,
                           INPUT Varios.Comprobante, INPUT CuentaCr,
                           INPUT "DB",
                           INPUT "Seguros de Ahorros", INPUT W_Usuario, 
                           INPUT TRUNCATE(TAho.TA_Seguro,0),
                           INPUT 999,INPUT Doc,INPUT STRING(TAho.TA_Age,"999")).
      FIND TCre WHERE TCre.TC_Age EQ TAho.TA_Age NO-ERROR.
      IF TCre.TC_Seguro GT 0 THEN
        RUN Gra_Movimientos (INPUT 10, INPUT TCre.TC_Age,
                             INPUT Varios.Comprobante, INPUT CuentaCr,
                             INPUT "DB",
                             INPUT "Seguros de Creditos", INPUT W_Usuario, 
                             INPUT TRUNCATE(TCre.TC_Seguro,0),
                             INPUT 999,INPUT Doc,INPUT STRING(TAho.TA_Age,"999")).
    ValorIgual = ValorIgual + TRUNCATE(TAho.TA_Seguro,0) + TRUNCATE(TCre.TC_Seguro,0).
 END. /*end del for each taho*/
 /*partida admon alcredito*/
 ASSIGN Comentario = "Valor Seguros Mes: " + STRING(MONTH(TODAY)).
 IF ValorIgual GT 0 THEN
   RUN Gra_Movimientos (INPUT 10, INPUT 10,
                        INPUT Varios.Comprobante, INPUT CuentaAs,
                        INPUT "CR",
                        INPUT Comentario, INPUT W_Usuario, INPUT ValorIgual,
                        INPUT 999,INPUT Doc,INPUT "860524654-6").

 FIND Formatos WHERE Formatos.Agencia      EQ 10
                  AND Formatos.Cod_Formato EQ W_Com 
                  NO-LOCK NO-ERROR.
 IF AVAILABLE(Formatos) THEN DO:
     RUN VALUE(Formatos.Nom_Proceso) (INPUT Varios.Comprobante,
                                      INPUT Doc, INPUT Doc,
                                      INPUT 10).
 END.
 ELSE DO:
    RUN MostrarMensaje IN W_Manija (INPUT 345, OUTPUT W_Rpta).
    RETURN ERROR.
 END.
/* RUN Grabar_Info (INPUT TAho.TA_Age, INPUT STRING("Cbt:" + STRING(W_Com) + " Num:" + STRING(Doc)), INPUT 1, 
                  INPUT "COM", INPUT (TAho.TA_Seguro + TCre.TC_Seguro), INPUT 0).*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Contabilizar wWin 
PROCEDURE Contabilizar :
DEFINE VAR W_Com LIKE Comprobantes.Cod_Formato.
DO TRANSACTION:
 FIND Varios WHERE Varios.Tipo EQ 8 AND Varios.Codigo EQ 11 NO-LOCK NO-ERROR.
 IF AVAILABLE Varios AND Varios.Comprobante EQ 0 THEN DO:
    MESSAGE "No se encuentra configurado el Comprobante" SKIP
            "en la tabla de varios para el proceso" SKIP(1)
            "Comuniquese con el Administrador del Sistema" VIEW-AS ALERT-BOX ERROR.
   RETURN ERROR.
 END.
 IF NOT AVAILABLE Varios THEN DO:
    MESSAGE "No Proceso no se encuentra matriculado en la" SKIP
            "tabla de varios." SKIP(1)
            "Comuniquese con el Administrador del Sistema" VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
 END.

 FOR EACH TAho WHERE TAho.TA_Estado EQ YES:
    FIND ProcDia WHERE ProcDia.Agencia     EQ TAho.TA_Age AND
                       ProcDia.Cod_Proceso EQ 11          AND
                       ProcDia.Fecha_Proc  EQ W_Fecha     AND
                       ProcDia.Estado      EQ 1.
    IF AVAILABLE ProcDia THEN ASSIGN ProcDia.Hor_Inicial = TIME
                                     ProcDia.Usuario    = W_Usuario.
    ELSE DO:
      MESSAGE "Inconsistencia en Configuracion del Proceso" SKIP
              "Detectada antes de realizarlo para la Agencia" SKIP
              "Agencia: " TAho.TA_Age VIEW-AS ALERT-BOX ERROR.
      RETURN ERROR.
    END.
    
    FIND Comprobantes WHERE Comprobantes.Agencia     EQ TAho.TA_Age  
                        AND Comprobantes.Comprobante EQ Varios.Comprobante NO-ERROR NO-WAIT.
    IF LOCKED Comprobantes THEN DO:
      REPEAT WHILE LOCKED Comprobantes:
         FIND Comprobantes WHERE Comprobantes.Agencia     EQ TAho.TA_Age
                          AND Comprobantes.Comprobante EQ Varios.Comprobante NO-ERROR NO-WAIT.
      END.
    END.
    W_Com = Comprobantes.Cod_Formato.
    RUN Asignar_SecComprobante NO-ERROR.
    IF ERROR-STATUS:ERROR AND Doc EQ 0 THEN DO:
       MESSAGE "No se pudo Aumentar la secuencia del comprobante" SKIP
               "Se devuelve la operación de Seguros" VIEW-AS ALERT-BOX ERROR.
       RETURN ERROR.
    END.
    RELEASE Comprobantes.
    RUN Grabar_Contable NO-ERROR.
    IF ERROR-STATUS:ERROR THEN
       RETURN ERROR.
    FIND Formatos WHERE Formatos.Agencia     EQ TAho.TA_Age
                     AND Formatos.Cod_Formato EQ W_Com 
                     NO-LOCK NO-ERROR.
    IF AVAILABLE(Formatos) THEN
       RUN VALUE(Formatos.Nom_Proceso) (INPUT Varios.Comprobante,
                                        INPUT Doc, INPUT Doc,
                                        INPUT TAho.TA_Age).
    ELSE DO:
       RUN MostrarMensaje IN W_Manija (INPUT 345, OUTPUT W_Rpta).
       RETURN ERROR.
    END.
    RUN Grabar_Info (INPUT TAho.TA_Age, INPUT STRING("Cbt:" + STRING(W_Com) + " Num:" + STRING(Doc)), INPUT 1, 
                     INPUT "COM", INPUT (TAho.TA_Seguro + TCre.TC_Seguro), INPUT 0).
  
    
    IF AVAILABLE ProcDia THEN ASSIGN ProcDia.Hor_Final = TIME
                                     ProcDia.Estado    = 2.
 END. /*end del for each taho*/
 RUN Comprobante_AgeAdm.
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
  DISPLAY Cmb_Agencias Top_Ahorro W_PorAho TTA_NAh TTA_Aho TTA_Exe TTA_70 
          TTA_NCl TTA_Jur W_TotSegAho Top_Credito TTC_NCr TTC_Ant TTC_Des 
          TTC_Exe TTC_70 TTC_Jur W_TotSegCre TTC_Cre 
      WITH FRAME F_Seg IN WINDOW wWin.
  ENABLE Cmb_Agencias BUTTON-173 BAho BCre BtnDone Btn_Procesar BUTTON-176 
      WITH FRAME F_Seg IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Seg}
  ENABLE IMAGE-2 R1 R2 R3 R4 R5 R6 R7 R8 R9 
      WITH FRAME F_Progreso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
  DISPLAY RTipo 
      WITH FRAME F_Info IN WINDOW wWin.
  ENABLE RTipo BUTTON-177 BUTTON-178 
      WITH FRAME F_Info IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Info}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Generar_Archivos wWin 
PROCEDURE Generar_Archivos :
/*{Incluido\RepEncabezado.i}*/
  DEFINE VAR W_Tit AS CHARACTER FORMAT "X(50)".
  DEFINE VAR WTotVal1 LIKE Ahorros.Sdo_Disponible.
  DEFINE VAR WTotVal2 LIKE Ahorros.Sdo_Disponible.
  DEFINE VAR WTotAge1 LIKE Ahorros.Sdo_Disponible.
  DEFINE VAR WTotAge2 LIKE Ahorros.Sdo_Disponible.
  DEFINE VAR WSinEXC LIKE Ahorros.Sdo_Disponible.
  DEFINE VAR W_ArcSalida AS CHARACTER FORMAT "X(50)".
  
  DEFINE VAR W_Nombre AS CHARACTER FORMAT "X(30)".
/*  W_Reporte   = "REPORTE   : " + W_Tit + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGE  NIT             NOMBRE                                        VALOR               SEGURO".
     
    W_Linea = FILL(W_Raya,132).
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.          */
    FOR EACH TInfo WHERE TInfo.TI_Val1 GT 0 NO-LOCK BREAK BY TInfo.TI_Tip BY TInfo.TI_Ind BY TInfo.TI_Age:
      IF FIRST-OF(TInfo.TI_Ind) THEN DO:
         IF TInfo.TI_Tip EQ 1 THEN DO:
            W_ArcSalida = W_Pathspl + "\Seguros_Aho_" + TInfo.TI_Ind + STRING(MONTH(TODAY)) + ".TXT".
            IF TInfo.TI_Ind EQ "CON" THEN
              W_ArcSalida = W_Pathspl + "\Comprobantes_Seguros" + STRING(MONTH(TODAY)) + ".TXT".
         END.
         ELSE
            W_ArcSalida = W_Pathspl + "\Seguros_Cre_" + TInfo.TI_Ind + STRING(MONTH(TODAY)) + ".TXT".
         OUTPUT TO VALUE(W_ArcSalida) NO-ECHO PAGE-SIZE 65 PAGED.
         IF TInfo.TI_Tip = 1 AND TInfo.TI_Ind = "EXC" THEN W_Tit = "Ahorros + Aportes con Excedente a " + STRING(Exceso_Ahorros).
         IF TInfo.TI_Tip = 1 AND TInfo.TI_Ind = "MAY" THEN W_Tit = "Ahorros + Aportes de Clientes Mayores a 70 Años ".
         IF TInfo.TI_Tip = 1 AND TInfo.TI_Ind = "TER" THEN W_Tit = "Ahorros + Aportes de Clientes No Asociados".
         IF TInfo.TI_Tip = 1 AND TInfo.TI_Ind = "JUR" THEN W_Tit = "Ahorros + Aportes de Entidades Juridicas".
         IF TInfo.TI_Tip = 2 AND TInfo.TI_Ind = "EXC" THEN W_Tit = "Créditos con Excedente a " + STRING(Exceso_Creditos).
         IF TInfo.TI_Tip = 2 AND TInfo.TI_Ind = "MAY" THEN W_Tit = "Creditos de Clientes Mayores a 70 Años ".
         IF TInfo.TI_Tip = 2 AND TInfo.TI_Ind = "JUR" THEN W_Tit = "Creditos de Personas Juridicas".
         IF TInfo.TI_Tip = 2 AND TInfo.TI_Ind = "ATR" THEN W_Tit = "Creditos Atrasados mas de 1 Año".
         IF TInfo.TI_Tip = 1 AND TInfo.TI_Ind = "SEG" THEN W_Tit = "Seguro de Ahorros".
         IF TInfo.TI_Tip = 2 AND TInfo.TI_Ind = "SEG" THEN W_Tit = "Seguro de Créditos".
         IF TInfo.TI_Tip = 1 AND TInfo.TI_Ind = "COM" THEN W_Tit = "Comprobantes Generados".
         DISPLAY W_Tit AT 1 
                 "_________________________________________________________________________________________________" AT 1
                 "AGE  NIT           NOMBRE                                      VALOR                     SEGURO" AT 1
                 "_________________________________________________________________________________________________" AT 1
                 WITH FRAME F_Tit WIDTH 132 USE-TEXT NO-BOX NO-LABELS STREAM-IO.
         
      END.
      FIND Clientes WHERE Clientes.Nit EQ TInfo.TI_Nit NO-LOCK NO-ERROR.
      W_Nombre = "No encontrado".
      IF W_Inf = "EXC" THEN
         IF W_Tip EQ 1 THEN WSinEXC = TInfo.TI_Val1 + Exceso_Ahorros.
         ELSE  WSinEXC = TInfo.TI_Val1 + Exceso_Creditos.
      ELSE WSinEXC = 0.
      IF AVAILABLE Clientes THEN W_Nombre = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
      DISPLAY TInfo.TI_Age     AT 1
              TInfo.TI_Nit     AT 5
              W_Nombre         AT 23
              TInfo.TI_Val1    AT 65
              TInfo.TI_Val2    AT 90 FORMAT ">>,>>>,>>>,>>>"
      WITH WIDTH 132 FRAME F-Aho NO-BOX USE-TEXT STREAM-IO NO-LABELS.
      ASSIGN WTotVal1 = WTotVal1 + TInfo.TI_Val1
             WTotVal2 = WTotVal2 + TInfo.TI_Val2.
      IF LAST-OF(TInfo.TI_Age) THEN DO:
        DISPLAY "Total Agencia: " AT 40
                TInfo.TI_Age AT 67 FORMAT "999"
                WTotAge1  AT 65
                WTotAge2  AT 90 FORMAT ">>,>>>,>>>,>>>"
        WITH FRAME F_TotAge WIDTH 132 USE-TEXT NO-BOX NO-LABELS.
      END.
      IF LAST-OF(TInfo.TI_Ind) THEN DO:
        DISPLAY "Total: " AT 50
                WTotVal1  AT 65
                WTotVal2  AT 90 FORMAT ">>,>>>,>>>,>>>"
        WITH FRAME F_Tot WIDTH 132 USE-TEXT NO-BOX NO-LABELS.
        OUTPUT CLOSE.
      END.
    END.  
    /*PAGE.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Contable wWin 
PROCEDURE Grabar_Contable :
DEFINE VARIABLE ValorIgual AS DECIMAL FORMAT "->>>,>>>,>>>,>>9".
 DEFINE VARIABLE Comentario LIKE Mov_Contable.Comentario.

IF TAho.TA_Seguro GT 0 THEN
 RUN Gra_Movimientos (INPUT TAho.TA_Age, INPUT TAho.TA_Age,
                      INPUT Varios.Comprobante, INPUT CuentaDB,
                      INPUT "DB",
                      INPUT "Seguros de Ahorros", INPUT W_Usuario, 
                      INPUT TRUNCATE(TAho.TA_Seguro,0),
                      INPUT 999,INPUT Doc,INPUT NitDB).
 FIND TCre WHERE TCre.TC_Age EQ TAho.TA_Age NO-ERROR.
 IF TCre.TC_Seguro GT 0 THEN
   RUN Gra_Movimientos (INPUT TCre.TC_Age, INPUT TCre.TC_Age,
                        INPUT Varios.Comprobante, INPUT CuentaDB,
                        INPUT "DB",
                        INPUT "Seguros de Creditos", INPUT W_Usuario, 
                        INPUT TRUNCATE(TCre.TC_Seguro,0),
                        INPUT 999,INPUT Doc,INPUT NitDB).

 ASSIGN Comentario = "Valor Seguros Mes: " + STRING(MONTH(TODAY))
        ValorIgual = TRUNCATE(TAho.TA_Seguro,0) + TRUNCATE(TCre.TC_Seguro,0).
 IF ValorIgual GT 0 THEN
   RUN Gra_Movimientos (INPUT TAho.TA_Age, INPUT 10,
                        INPUT Varios.Comprobante, INPUT CuentaCR,
                        INPUT "CR",
                        INPUT Comentario, INPUT W_Usuario, INPUT ValorIgual,
                        INPUT 999,INPUT Doc,INPUT "010").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Grabar_Info wWin 
PROCEDURE Grabar_Info :
DEFINE INPUT PARAMETER W_Age AS INTEGER FORMAT "999".
DEFINE INPUT PARAMETER W_Nit AS CHARACTER FORMAT "X(14)".
DEFINE INPUT PARAMETER W_Tip AS INTEGER FORMAT "9".
DEFINE INPUT PARAMETER W_Ind AS CHARACTER FORMAT "X(3)".
DEFINE INPUT PARAMETER W_Val1 LIKE Ahorros.Sdo_Disponible.
DEFINE INPUT PARAMETER W_Val2 LIKE Ahorros.Sdo_Disponible.

CREATE TInfo.
ASSIGN TInfo.TI_Age = W_Age
       TInfo.TI_Nit = W_Nit
       TInfo.TI_Tip = W_Tip
       TInfo.TI_Ind = W_Ind
       TInfo.TI_Val1 = W_Val1
       TInfo.TI_Val2 = W_Val2.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Gra_Movimientos wWin 
PROCEDURE Gra_Movimientos :
DEFINE INPUT PARAMETER P_Agencia LIKE Agencias.Agencia.
  DEFINE INPUT PARAMETER P_OfiDno  LIKE Mov_Contable.Destino.
  DEFINE INPUT PARAMETER T_Cbte    LIKE Mov_Contable.Comprobante.
  DEFINE INPUT PARAMETER P_Cuenta  LIKE Cuentas.Cuenta.
  DEFINE INPUT PARAMETER P_Nat     LIKE Cuentas.Naturaleza.
  DEFINE INPUT PARAMETER P_Coment  LIKE Mov_Contable.Comentario.
  DEFINE INPUT PARAMETER P_Usuario LIKE Usuarios.Usuario.
  DEFINE INPUT PARAMETER P_Valor   AS   DECIMAL FORMAT "->>>,>>>,>>>,>>9".
  DEFINE INPUT PARAMETER P_CC      LIKE Mov_Contable.Cen_Costos.
  DEFINE INPUT PARAMETER P_Docto   LIKE Mov_Contable.Num_Documento.
  DEFINE INPUT PARAMETER P_Nit     LIKE Mov_Contable.Nit.
  
  FIND Cuentas WHERE Cuentas.Cuenta EQ P_Cuenta NO-LOCK NO-ERROR.  
  CREATE Mov_Contable.
  ASSIGN Mov_Contable.Agencia       = P_Agencia
         Mov_Contable.Destino       = P_OfiDno
         Mov_Contable.Comprobante   = T_Cbte
         Mov_Contable.Cuenta        = P_Cuenta
         Mov_Contable.Fec_Contable  = W_Fecha
         Mov_Contable.Comentario    = P_Coment
         Mov_Contable.Usuario       = P_Usuario
         Mov_Contable.Cen_Costos    = P_CC
         Mov_Contable.Num_Documento = P_Docto
         Mov_Contable.Fec_Grabacion = TODAY
         Mov_contable.Enlace        = P_Cuenta
         Mov_Contable.Hora          = TIME
         Mov_Contable.Nit           = P_Nit.
  IF P_Nat EQ "DB" THEN Mov_Contable.Db = P_Valor.
  IF P_Nat EQ "CR" THEN Mov_Contable.Cr = P_Valor.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
FOR EACH TInfo: DELETE TInfo. END.
  FOR EACH TAho: ASSIGN TAho.TA_TotAhorros = 0
                        TAho.TA_TotJur     = 0
                        TAho.TA_TotTer     = 0
                        TAho.TA_Exceso     = 0
                        TAho.TA_AA70       = 0.
  END.
  FOR EACH TCre: ASSIGN TCre.TC_TotAntCre   = 0
                        TCre.TC_TotDesCre   = 0
                        TCre.TC_Exceso      = 0
                        TCre.TC_Cr70        = 0
                        TCre.TC_Juridicas   = 0
                        TCre.TC_Atrasados   = 0.
  END.

FOR EACH Agencias WHERE Agencias.Estado EQ 2 NO-LOCK:
     W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre) IN FRAME F_Seg.
     CREATE TAho. CREATE TCre.
     ASSIGN TAho.TA_Age = Agencias.Agencia
            TCre.TC_Age = Agencias.Agencia
            TAho.TA_NomAge = Agencias.Nombre
            TCre.TC_NomAge = Agencias.Nombre
            TAho.TA_Estado = YES
            TCre.TC_Estado = YES.
     FIND ProcDia WHERE
          ProcDia.Agencia     EQ Agencias.Agencia AND
          ProcDia.Cod_Proceso EQ 11               AND
          ProcDia.Fecha_proc  EQ W_Fecha NO-LOCK NO-ERROR.
     IF NOT AVAILABLE ProcDia THEN    ASSIGN TA_Estado = NO TC_Estado = NO.
     ELSE IF ProcDia.Estado EQ 2 THEN ASSIGN TA_Estado = NO TC_Estado = NO.
END.
FOR EACH TAho WHERE TAho.TA_Estado EQ NO: DELETE TAho. END.
FOR EACH TCre WHERE TCre.TC_Estado EQ NO: DELETE TCre. END.
ASSIGN W_PorAho = PR_Ahorro
       TOP_Ahorro = Exceso_Ahorros
       TOP_Credito = Exceso_Creditos.
RUN SUPER.
HIDE FRAME F_Info. 
HIDE FRAME F_Progreso.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prc_Ahorros wWin 
PROCEDURE Prc_Ahorros :
DEFINE VAR W_Asegurar LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR W_Seguro   LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR W_Restar   LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR W_Ok AS LOGICAL INITIAL YES.
   TotAhoNit = 0.
   FOR EACH Ahorros WHERE
            Ahorros.Cod_Ahorro     NE 11 AND
            Ahorros.Nit            EQ Clientes.Nit AND
           (Ahorros.Sdo_Disponible GT 0 OR
            Ahorros.Sdo_Canje      GT 0) NO-LOCK 
            BREAK BY Ahorros.Nit:
       ASSIGN TAho.TA_TotAhorros = TAho.TA_TotAhorros + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
              TotAhoNit = TotAhoNit + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
              TTA_Aho   = TTA_Aho   + Ahorros.Sdo_Disponible + Ahorros.Sdo_Canje
              TTA_NAh   = TTA_NAh   + 1.
          
   END.
   IF Clientes.Tipo_Vinculo EQ 2 THEN DO:
      ASSIGN TAho.TA_TotTer = TAho.TA_TotTer + TotAhoNit
             W_Restar       = W_Restar + TotAhoNit.
      RUN Grabar_Info (INPUT Clientes.Agencia, INPUT Clientes.Nit, INPUT 1, INPUT "TER", INPUT TotAhoNit, INPUT 0).
      W_Ok = NO.
   END.
   IF Clientes.Tipo_Cliente GT 2 AND W_Ok THEN DO:
      ASSIGN TAho.TA_TotJur = TAho.TA_TotJur + TotAhoNit
             W_Restar = W_Restar + TotAhoNit.
      RUN Grabar_Info (INPUT Clientes.Agencia, INPUT Clientes.Nit, INPUT 1, INPUT "JUR", INPUT TotAhoNit, INPUT 0).
      W_Ok = NO.
   END.
   IF (Clientes.Fec_Ingreso - Clientes.Fec_Nacimiento) / 360 GT 70 AND W_Ok THEN DO:
      ASSIGN TAho.TA_AA70 = TAho.TA_AA70 + TotAhoNit
             W_Restar = W_Restar + TotAhoNit.
      RUN Grabar_Info (INPUT Clientes.Agencia, INPUT Clientes.Nit, INPUT 1, INPUT "MAY", INPUT TotAhoNit, INPUT 0).
      W_Ok = NO.
   END.
   IF W_Ok THEN DO:
     IF (TotAhoNit - Exceso_Ahorros) GT 0 THEN DO:
        ASSIGN TAho.TA_Exceso = TAho.TA_Exceso + (TotAhoNit - Exceso_Ahorros)
               W_Restar = W_Restar + (TotAhoNit - Exceso_Ahorros).
        RUN Grabar_Info (INPUT Clientes.Agencia, INPUT Clientes.Nit, INPUT 1, 
                         INPUT "EXC", INPUT (TotAhoNit - Exceso_Ahorros), INPUT 0).
     END.
     ELSE W_Restar = 0.
   END.
   TAho.TA_Seguro = TAho.TA_Seguro + 
                      ((TotAhoNit - W_Restar) * Pr_Ahorro).
   W_Seguro = ((TotAhoNit - W_Restar) * Pr_Ahorro).
   W_Asegurar = TotAhoNit - W_Restar.
   RUN Grabar_Info (INPUT Clientes.Agencia, INPUT Clientes.Nit, INPUT 1, 
                    INPUT "SEG", INPUT W_Asegurar, INPUT W_Seguro).
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Prc_Creditos wWin 
PROCEDURE Prc_Creditos :
DEFINE VAR W_Fec70 LIKE Creditos.Fec_Desembolso INITIAL TODAY.
   DEFINE VAR W_Ok AS LOGICAL INITIAL YES.
   DEFINE VAR W_Asegurar LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR W_Seguro   LIKE Ahorros.Sdo_Disponible.
   DEFINE VAR W_Restar   LIKE Ahorros.Sdo_Disponible.

   ASSIGN TotAntCreNit = 0
          TotDesCreNit = 0
          TotAntCreNit70 = 0
          TotDesCreNit70 = 0.
   FOR EACH Creditos WHERE
            Creditos.Nit EQ Clientes.Nit AND
           (Creditos.Sdo_Capital    GT 0 OR
            Creditos.Int_Corrientes GT 0) NO-LOCK 
            BY Creditos.Nit:
       TTC_NCr = TTC_NCr + 1.
       IF Creditos.Fec_Desembolso LT Fec_Validacion THEN
          ASSIGN TCre.TC_TotAntCre = TCre.TC_TotAntCre + Creditos.Sdo_Capital
                 TotAntCreNit      = TotAntCreNit  + Creditos.Sdo_Capital.
       ELSE
          ASSIGN TCre.TC_TotDesCre = TCre.TC_TotDesCre + Creditos.Sdo_Capital
                 TotDesCreNit      = TotDesCreNit  + Creditos.Sdo_Capital.
       IF Clientes.Tipo_Cliente LE 2 THEN DO:
           IF MONTH(Creditos.Fec_Desembolso) EQ 2 AND DAY(Creditos.Fec_Desembolso) EQ 29 THEN
              W_Fec70 = DATE(STRING(DAY(Creditos.Fec_Desembolso)) + "/28/"
                        + STRING(YEAR(Creditos.Fec_Desembolso) - 70)).
           ELSE
              W_Fec70 = DATE(STRING(DAY(Creditos.Fec_Desembolso)) + "/" + 
                             STRING(MONTH(Creditos.Fec_Desembolso)) + "/"
                           + STRING(YEAR(Creditos.Fec_Desembolso) - 70)).
           IF Clientes.Fec_Nacimiento LE W_Fec70 THEN DO:
               IF Creditos.Fec_Desembolso LT Fec_Validacion THEN
                  ASSIGN TotAntCreNit70 = TotAntCreNit70  + Creditos.Sdo_Capital.
               ELSE
                  ASSIGN TotDesCreNit70 = TotDesCreNit70  + Creditos.Sdo_Capital.
           END.
       END.
   END.
   IF Clientes.Tipo_Cliente GT 2 THEN DO:
      ASSIGN TCre.TC_Juridicas = TCre.TC_Juridicas  + TotAntCreNit + TotDesCreNit
             W_Restar = W_Restar + TotAntCreNit + TotDesCreNit.
      RUN Grabar_Info (INPUT Clientes.Agencia, INPUT Clientes.Nit, 
                       INPUT 2, INPUT "JUR", INPUT TotAntCreNit + TotDesCreNit, INPUT 0).
      W_Ok = NO.
   END.
   IF (TotAntCreNit70 GT 0 OR TotDesCreNit70 GT 0) AND W_Ok THEN DO:
      ASSIGN TCre.TC_Cr70 = TCre.TC_Cr70 + TotAntCreNit70 + TotDesCreNit70
             W_Restar     = W_Restar + TotAntCreNit70 + TotDesCreNit70.
      RUN Grabar_Info (INPUT Clientes.Agencia, INPUT Clientes.Nit, 
                       INPUT 2, INPUT "MAY", INPUT TotAntCreNit70 + TotDesCreNit70, INPUT 0).
      W_Ok = NO.
   END.
   
/*   IF TCre.TC_Atrasados GT 0 THEN
      RUN Grabar_Info (INPUT Clientes.Agencia, INPUT Clientes.Nit, 
                       INPUT 2, INPUT "ATR", INPUT TotCreNit).*/
   IF W_Ok THEN DO:
     IF ((TotAntCreNit + TotDesCreNit) - Exceso_Creditos) GT 0 AND W_Ok THEN DO:
        RUN Grabar_Info (INPUT Clientes.Agencia, INPUT Clientes.Nit, 
                         INPUT 2, INPUT "EXC", INPUT (TotAntCreNit + TotDesCreNit) - Exceso_Creditos, INPUT 0).
        ASSIGN TCre.TC_Exceso = TCre.TC_Exceso + ((TotAntCreNit + TotDesCreNit) - Exceso_Creditos)
               W_Restar       = W_Restar + ((TotAntCreNit + TotDesCreNit) - Exceso_Creditos).
        W_Ok = NO.
     END.
     ELSE W_Restar = 0.
   END.
   IF Clientes.Id_Preexistentes THEN
      ASSIGN TCre.TC_Seguro = TCre.TC_Seguro + ((TotAntCreNit + TotDesCreNit) * Pr_PreCredito)
             W_Seguro       = ((TotAntCreNit + TotDesCreNit) * Pr_PreCredito)
             TCre.TC_Pree   = YES.
             
   ELSE
      ASSIGN TCre.TC_Seguro = TCre.TC_Seguro + (TotAntCreNit * Pr_AntCredito) + (TotDesCreNit * Pr_DesCredito)
             W_Seguro       = (TotAntCreNit * Pr_AntCredito) + (TotDesCreNit * Pr_DesCredito)
             TCre.TC_Pree   = NO.
   W_Asegurar = TotAntCreNit + TotDesCreNit - W_Restar.
   
   RUN Grabar_Info (INPUT Clientes.Agencia, INPUT Clientes.Nit, 
                    INPUT 2, INPUT "SEG", INPUT W_Asegurar, INPUT W_Seguro).
   ASSIGN TTC_Cre     = TTC_Cre + TotAntCreNit + TotDesCreNit
          TTC_Ant     = TTC_Ant + TotAntCreNit
          TTC_Des     = TTC_Des + TotDesCreNit.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Procesar wWin 
PROCEDURE Procesar :
DEFINE VAR WAge LIKE Clientes.Agencia.
ASSIGN W_TotSegCre = 0 TTC_Jur     = 0 TTC_70      = 0 TTC_Exe     = 0 TTC_Des     = 0 
       TTC_Ant     = 0 TTC_NCr     = 0 TTC_Cre     = 0
       W_TotSegAho = 0 TTA_NCl     = 0 TTA_70      = 0 TTA_Jur     = 0 TTA_Exe     = 0
       TTA_Aho     = 0 TTA_NAh     = 0. 
FOR EACH Clientes WHERE  
         Clientes.Agencia      GE AgeIni AND
         Clientes.Agencia      LE AGeFin /*AND
         Clientes.Tipo_Vinculo LT 3*/ NO-LOCK
         BREAK BY Clientes.Agencia BY Clientes.Nit:
   j = j + 1.
   RUN Progreso.            
   FIND TAho WHERE TAho.TA_Age EQ Clientes.Agencia NO-ERROR.
   IF NOT AVAILABLE TAho THEN NEXT.
   RUN Prc_Ahorros.
   FIND TCre WHERE TCre.TC_Age EQ Clientes.Agencia NO-ERROR.
   IF NOT AVAILABLE TCre THEN NEXT.
   RUN Prc_Creditos.
   IF LAST-OF(Clientes.Agencia) THEN
      ASSIGN W_TotSegCre = W_TotSegCre + TCre.TC_Seguro
             TTC_70      = TTC_70  + TCre.TC_Cr70
             TTC_Jur     = TTC_Jur + TCre.TC_Juridicas
             TTC_Exe     = TTC_Exe + TCre.TC_Exceso
             TTC_Des     = TTC_Des + TotDesCreNit
             TTA_NCl     = TTA_NCl + TAho.TA_TotTer
             TTA_Jur     = TTA_Jur + TAho.TA_TotJur
             TTA_70      = TTA_70  + TAho.TA_AA70
             TTA_Exe     = TTA_Exe + TAho.TA_Exceso
             W_TotSegAho = W_TotSegAho + TAho.TA_Seguro.
END.
OPEN QUERY BAho FOR EACH TAho NO-LOCK INDEXED-REPOSITION.
OPEN QUERY BCre FOR EACH TCre NO-LOCK INDEXED-REPOSITION.
DISPLAY W_TotSegCre TTC_70 TTC_Jur TTC_Exe TTC_Cre TTC_NCr TTA_Aho TTC_Ant TTC_Des
        W_TotSegAho TTA_70 TTA_Jur TTA_NCl TTA_Exe TTA_NAh WITH FRAME F_Seg.

RUN Totales_1.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}
  DEFINE VAR WTotVal1 LIKE Ahorros.Sdo_Disponible.
  DEFINE VAR WTotVal2 LIKE Ahorros.Sdo_Disponible.
  DEFINE VAR WSinEXC LIKE Ahorros.Sdo_Disponible.
  
  DEFINE VAR W_Nombre AS CHARACTER FORMAT "X(30)".
  W_Reporte   = "REPORTE   : " + W_Tit + " - FECHA: " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = "AGE  NIT             NOMBRE                                        VALOR               SEGURO".
     
  DEFINE VAR Tot1 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
  DEFINE VAR Tot2 AS DECIMAL FORMAT "->>>,>>>,>>>,>>>,>>9".
    W_Linea = FILL(W_Raya,132).
    VIEW FRAME F-Encabezado.
    VIEW FRAME F-Ftr.
    IF RTipo NE 11 THEN DO:
        FOR EACH TInfo WHERE TInfo.TI_Tip EQ W_Tip AND TInfo.TI_Ind EQ W_Inf AND 
                 TInfo.TI_Val1 GT 0 NO-LOCK BREAK BY TInfo.TI_Age:
          FIND Clientes WHERE Clientes.Nit EQ TInfo.TI_Nit NO-LOCK NO-ERROR.
          W_Nombre = "No encontrado".
          IF W_Inf = "EXC" THEN
             IF W_Tip EQ 1 THEN WSinEXC = TInfo.TI_Val1 + Exceso_Ahorros.
             ELSE  WSinEXC = TInfo.TI_Val1 + Exceso_Creditos.
          ELSE WSinEXC = 0.
          IF AVAILABLE Clientes THEN W_Nombre = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
          DISPLAY TInfo.TI_Age     AT 1
                  TInfo.TI_Nit     AT 5
                  W_Nombre         AT 23
                  TInfo.TI_Val1    AT 65
                  TInfo.TI_Val2    AT 90 FORMAT ">>,>>>,>>>,>>>"
          WITH WIDTH 132 FRAME F-Aho NO-BOX USE-TEXT STREAM-IO NO-LABELS.
          ASSIGN WTotVal1 = WTotVal1 + TInfo.TI_Val1
                 WTotVal2 = WTotVal2 + TInfo.TI_Val2
                 Tot1     = Tot1     + TInfo.TI_Val1
                 Tot2     = Tot2     + TInfo.TI_Val2.
          IF LAST-OF(TInfo.TI_Age) THEN DO:
              DISPLAY "Total Agencia: " AT 40
                      Tot1  AT 65
                      Tot2  AT 90 FORMAT ">>,>>>,>>>,>>>"
              WITH FRAME F_TotAge WIDTH 132 USE-TEXT NO-BOX NO-LABELS.
              ASSIGN Tot1 = 0 Tot2 = 0.
          END.
        END.  
        DISPLAY "Total: " AT 50
                WTotVal1  AT 65
                WTotVal2  AT 90 FORMAT ">>,>>>,>>>,>>>"
        WITH FRAME F_Tot WIDTH 132 USE-TEXT NO-BOX NO-LABELS.
        PAGE.
    END.
    ELSE DO:
        DISPLAY "Ag.    Saldos             NroCre  Mayores  Menores Juridic  TotAsociados" WITH FRAME j WIDTH 90.
        FOR EACH TmpCtble:
            DISPLAY TmpCtble.agen         FORMAT "z9"                  
                    TmpCtble.sdo_cap      FORMAT "zzz,zzz,zzz,zz9.99"  
                    TmpCtble.Nro_Cre      FORMAT "zzz,zz9"             
                    TmpCtble.AsoMay       FORMAT "zzz,zz9"             
                    TmpCtble.AsoMen       FORMAT "zzz,zz9"             
                    TmpCtble.AsoJur       FORMAT "zzz,zz9"             
                    TmpCtble.TotAso       FORMAT "Zzz,zz9"             
               WITH FRAME jj WIDTH 240 NO-LABELS USE-TEXT.
               ASSIGN  wsdo_cap  = wsdo_cap  + TmpCtble.sdo_cap   
                       wNro_Cre  = wNro_Cre  + TmpCtble.Nro_Cre   
                       wAsoMay   = wAsoMay   + TmpCtble.AsoMay    
                       wAsoMen   = wAsoMen   + TmpCtble.AsoMen    
                       wAsoJur   = wAsoJur   + TmpCtble.AsoJur    
                       wTotAso   = wTotAso   + TmpCtble.TotAso.    
        END.
        DISPLAY "  ============================================================ ".
        DISPLAY "     " wsdo_cap FORMAT "zzz,zzz,zzz,zz9.99" 
                  wNro_Cre FORMAT "zzz,zz9"            
                  wAsoMay  FORMAT "zzz,zz9"            
                  wAsoMen  FORMAT "zzz,zz9"            
                  wAsoJur  FORMAT "zzz,zz9"            
                  wTotAso  FORMAT "Zzz,zz9" WITH FRAME jj2 WIDTH 240 NO-LABELS USE-TEXT.         
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Progreso wWin 
PROCEDURE Progreso :
DO WITH FRAME F_Progreso:
  IF j EQ 75 THEN DO:
        j = 0.
        k = k + 1.
        ASSIGN R1:BGCOLOR = 15 R2:BGCOLOR = 15 R3:BGCOLOR = 15 R4:BGCOLOR = 15
               R5:BGCOLOR = 15 R6:BGCOLOR = 15 R7:BGCOLOR = 15 R8:BGCOLOR = 15.
               R9:BGCOLOR = 15.
        CASE k:
          WHEN 1 THEN
             R1:BGCOL = 18.
          WHEN 2 THEN
             R2:BGCOL = 18.
          WHEN 3 THEN
             R3:BGCOL = 18.
          WHEN 4 THEN
             R4:BGCOL = 18.
          WHEN 5 THEN
             R5:BGCOL = 18.
          WHEN 6 THEN
             R6:BGCOL = 18.
          WHEN 7 THEN
             R7:BGCOL = 18.
          WHEN 8 THEN
             R8:BGCOL = 18.
          WHEN 9 THEN
             R9:BGCOL = 18.
        END CASE.
        IF k = 9 THEN k = 0.
   END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Totales_1 wWin 
PROCEDURE Totales_1 :
DEFI VAR wsdo_cap    AS DECIMAL FORMAT "zzz,zzz,zzz,zz9.99" INITIAL 0.00.
DEFI VAR wNro_Cre    AS INTEGER FORMAT "zzz,zz9" INITIAL 0.
DEFI VAR wAsoMay     AS INTEGER FORMAT "zzz,zz9" INITIAL 0.
DEFI VAR wAsoMen     AS intEGER FORMAT "zzz,zz9" INITIAL 0.
DEFI VAR wAsoJur     AS DECIMAL FORMAT "zzz,zz9" INITIAL 0.
DEFI VAR wTotAso     AS DECIMAL FORMAT "Zzz,zz9" INITIAL 0.
FOR EACH TmpCtble: DELETE TmpCtble.
END.

FOR EACH creditos WHERE sdo_capital NE 0 AND estado = 2 NO-LOCK BREAK BY Creditos.agencia:
  IF FIRST-OF(creditos.agencia) THEN DO:
     ASSIGN wsdo_cap = 0   wNro_Cre = 0.
  END.

  ASSIGN wsdo_cap = wsdo_cap + Creditos.sdo_capital
         wnro_cre = wNro_Cre + 1.

  IF LAST-OF(Creditos.agencia) THEN DO:
     CREATE TmpCtble.
     ASSIGN TmpCtble.agen     = Creditos.agencia
            TmpCtble.sdo_cap  = wsdo_cap
            TmpCtble.Nro_Cre  = wNro_Cre.  
  END.
END.

DEFINE VAR ztotal AS INTEGER INITIAL 0.
FOR EACH ahorros WHERE (cod_ahorro = 5  OR cod_ahorro = 10 ) AND
                       (ahorros.sdo_disponible + sdo_canje) GT 0 AND
                       ahorros.estado = 1 NO-LOCK BREAK BY ahorros.agencia:
   IF FIRST-OF(ahorros.agencia) THEN DO:
      ASSIGN wAsoMay = 0  wAsoMen = 0 wAsoJur  = 0  ZTOTAL = 0.
   END.

   ASSIGN wNro_Cre = wNro_Cre + 1
          wTotAso  = wTotAso  + 1
          ztotal = ztotal + 1.

   FIND clientes WHERE cliente.nit = ahorros.nit NO-LOCK NO-ERROR.          
   IF AVAILABLE(clientes) THEN         
     IF ahorros.cod_ahorro = 10 THEN
        wAsoMen  = wAsoMen + 1.
     ELSE
       IF ahorros.cod_ahorro = 5 THEN DO:
         IF clientes.tipo_Cliente GT 2  THEN
           wAsoJur  = wAsoJur + 1.
         ELSE
           wAsoMay  = wAsoMay + 1.
       END.

   /*
   CASE clientes.tipo_Cliente:     
      WHEN 1 THEN wAsoMay  = wAsoMay + 1.                                       
      WHEN 2 THEN wAsoMen  = wAsoMen + 1.                                       
      OTHERWISE   wAsoJur  = wAsoJur + 1.                                       
   END CASE.                         */                                        
   ELSE                                                                      
      MESSAGE "Error!! no se encontro informacion de la cedula " AHORROS.nit   
      VIEW-AS ALERT-BOX INFO BUTTONS OK.                                        
   
   IF LAST-OF(ahorros.agencia) THEN DO:
      FIND TmpCtble WHERE TmpCtble.agen = ahorros.agencia NO-ERROR.
      IF AVAILABLE(TmpCtble) THEN DO:
         ASSIGN TmpCtble.AsoMay   = wAsoMay
                TmpCtble.AsoMen   = wAsoMen
                TmpCtble.AsoJur   = wAsoJur
                TmpCtble.TotAso   = ZTOTAL.
      END.
      ELSE
        MESSAGE "No se encontro la agencia " ahorros.agencia
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
   END.
END.
ASSIGN wsdo_cap = 0   wNro_Cre = 0    wAsoMay = 0    wAsoMen = 0 
       wAsoJur  = 0   wTotAso = 0.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

