&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dclientes.i"}.



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS vTableWin 
/*------------------------------------------------------------------------

  File:

  Description: from viewer.w - Template for SmartDataViewer objects

  Input Parameters:
      <none>

  Output Parameters:
      <none>

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
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

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dclientes.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Nit RowObject.Salario ~
RowObject.Gto_Arriendo RowObject.Act_vehiculo RowObject.Ing_arriendos ~
RowObject.Gto_Familiar RowObject.Act_inversion RowObject.Ing_financieros ~
RowObject.GtoFinanc_Indir RowObject.Act_casa RowObject.Ing_Honorarios ~
RowObject.Ing_Otros RowObject.Gto_obligacion RowObject.Sancionado ~
RowObject.Fec_IngEmpresa RowObject.Calificacion RowObject.Id_PuedeCodeudar ~
RowObject.Fec_Asociacion RowObject.Reportado_fiscalia ~
RowObject.Dias_Sancion RowObject.Fec_Calificacion ~
RowObject.Reportado_Procredito RowObject.Cod_Segmento ~
RowObject.Fec_Nacimiento RowObject.Aut_CentralRiesgo ~
RowObject.Reportado_Super RowObject.Fec_fallecido ~
RowObject.Id_Preexistentes RowObject.Fec_expedicion 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-3 RECT-4 RECT-9 RECT-10 RECT-5 ~
RECT-6 
&Scoped-Define DISPLAYED-FIELDS RowObject.Nit RowObject.FCliente ~
RowObject.Salario RowObject.Gto_Arriendo RowObject.Act_vehiculo ~
RowObject.Ing_arriendos RowObject.Gto_Familiar RowObject.Act_inversion ~
RowObject.Ing_financieros RowObject.GtoFinanc_Indir RowObject.Act_casa ~
RowObject.Ing_Honorarios RowObject.FTotEgresos RowObject.FTotActivos ~
RowObject.Ing_Otros RowObject.FTotIngresos RowObject.Gto_obligacion ~
RowObject.Sancionado RowObject.FReestructurado RowObject.Fec_IngEmpresa ~
RowObject.FPrivilegiado RowObject.Calificacion RowObject.Id_PuedeCodeudar ~
RowObject.Fec_Asociacion RowObject.Reportado_fiscalia ~
RowObject.Dias_Sancion RowObject.Fec_Calificacion ~
RowObject.Reportado_Procredito RowObject.Cod_Segmento ~
RowObject.Fec_Nacimiento RowObject.Aut_CentralRiesgo ~
RowObject.Reportado_Super RowObject.Fec_fallecido ~
RowObject.Id_Preexistentes RowObject.Fec_expedicion 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 118 BY 1.62.

DEFINE RECTANGLE RECT-10
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 2.15.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 7.81.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 40 BY 7.81.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 61 BY 7.81.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 57 BY 7.81.

DEFINE RECTANGLE RECT-9
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 38 BY 5.65.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Nit AT ROW 1.27 COL 27 COLON-ALIGNED HELP
          "Número documento de identificación" WIDGET-ID 18
          LABEL "Cliente" FORMAT "X(12)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 18 BY 1
          BGCOLOR 17 FGCOLOR 7 
     RowObject.FCliente AT ROW 1.27 COL 46 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 38 FORMAT "x(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 42 BY 1
          BGCOLOR 17 FGCOLOR 7 
     RowObject.Salario AT ROW 3.15 COL 12 COLON-ALIGNED HELP
          "Salario mensual" WIDGET-ID 114
          LABEL "Salario" FORMAT ">>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 25 BY 1
     RowObject.Gto_Arriendo AT ROW 3.42 COL 54 COLON-ALIGNED HELP
          "Gastos por pago de arriendo" WIDGET-ID 98
          LABEL "G. Arriendos" FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
     RowObject.Act_vehiculo AT ROW 3.42 COL 93 COLON-ALIGNED HELP
          "Valor del vehículo" WIDGET-ID 140
          LABEL "Val. Vehic." FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
     RowObject.Ing_arriendos AT ROW 4.23 COL 12 COLON-ALIGNED HELP
          "Ingresos por arriendos" WIDGET-ID 116
          LABEL "Arriendos" FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 25 BY 1
     RowObject.Gto_Familiar AT ROW 4.5 COL 54 COLON-ALIGNED HELP
          "Gasto mensual familiar del cliente" WIDGET-ID 100
          LABEL "G. Familiares" FORMAT ">>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
     RowObject.Act_inversion AT ROW 4.5 COL 93 COLON-ALIGNED HELP
          "Actividad de inversión" WIDGET-ID 92
          LABEL "Act. Invers." FORMAT ">>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
     RowObject.Ing_financieros AT ROW 5.31 COL 12 COLON-ALIGNED HELP
          "Ingresos financieros" WIDGET-ID 108
          LABEL "Financieros" FORMAT ">>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 25 BY 1
     RowObject.GtoFinanc_Indir AT ROW 5.58 COL 54 COLON-ALIGNED HELP
          "Gastos Financieros Indirectos" WIDGET-ID 96
          LABEL "G. Fin. Indir." FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
     RowObject.Act_casa AT ROW 5.58 COL 93 COLON-ALIGNED HELP
          "Valor de la propiedad" WIDGET-ID 90
          LABEL "Val. Prop." FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
     RowObject.Ing_Honorarios AT ROW 6.38 COL 12 COLON-ALIGNED HELP
          "Ingresos de honorarios" WIDGET-ID 110
          LABEL "Honorarios" FORMAT ">>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 25 BY 1
     RowObject.FTotEgresos AT ROW 6.65 COL 54 COLON-ALIGNED HELP
          "" WIDGET-ID 130
          LABEL "Tot. Egresos" FORMAT ">>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
          BGCOLOR 17 FGCOLOR 7 
     RowObject.FTotActivos AT ROW 6.65 COL 93 COLON-ALIGNED HELP
          "" WIDGET-ID 134
          LABEL "Tot. Activos" FORMAT ">>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
          BGCOLOR 17 FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.Ing_Otros AT ROW 7.46 COL 12 COLON-ALIGNED HELP
          "Otros Ingresos" WIDGET-ID 112
          LABEL "O. Ingresos" FORMAT ">>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 25 BY 1
     RowObject.FTotIngresos AT ROW 8.54 COL 12 COLON-ALIGNED HELP
          "" WIDGET-ID 128
          LABEL "Tot. Ingr." FORMAT ">>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 25 BY 1
          BGCOLOR 17 FGCOLOR 7 
     RowObject.Gto_obligacion AT ROW 9.08 COL 93 COLON-ALIGNED HELP
          "" WIDGET-ID 142
          LABEL "Sdo. Oblig." FORMAT ">>>,>>>,>>9"
          VIEW-AS FILL-IN NATIVE 
          SIZE 22 BY 1
          BGCOLOR 17 FGCOLOR 7 
     RowObject.Sancionado AT ROW 10.96 COL 4 HELP
          "El cliente por alguna razón ha sido o no ha sido sancionado" WIDGET-ID 172
          LABEL "Sancionado"
          VIEW-AS TOGGLE-BOX
          SIZE 14 BY .77
     RowObject.FReestructurado AT ROW 11.23 COL 42 COLON-ALIGNED HELP
          "" WIDGET-ID 174
          LABEL "Reestruct." FORMAT "x(15)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 16.43 BY 1
     RowObject.Fec_IngEmpresa AT ROW 11.5 COL 80 COLON-ALIGNED HELP
          "Fecha de ingreso de la empresa a la Organización" WIDGET-ID 192
          LABEL "Ing. Empresa" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY 1
     RowObject.FPrivilegiado AT ROW 11.77 COL 4 WIDGET-ID 176
          LABEL "Privilegiado"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .77
     RowObject.Calificacion AT ROW 12.31 COL 42 COLON-ALIGNED HELP
          "Calificación del cliente por la cartera" WIDGET-ID 150
          LABEL "Calif. Cliente" FORMAT "99999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 7.14 BY 1
     RowObject.Id_PuedeCodeudar AT ROW 12.58 COL 4 HELP
          "Id_PuedeCodeudar" WIDGET-ID 160
          LABEL "Puede Codeudar"
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .77
     RowObject.Fec_Asociacion AT ROW 12.58 COL 80 COLON-ALIGNED HELP
          "Fecha en que ingresa como asociado a la Organización" WIDGET-ID 184
          LABEL "Asoc. Desde" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 13 BY 1
     RowObject.Reportado_fiscalia AT ROW 13.38 COL 4 WIDGET-ID 166
          LABEL "Reportado a fiscalia"
          VIEW-AS TOGGLE-BOX
          SIZE 26 BY .77
     RowObject.Dias_Sancion AT ROW 13.38 COL 42 COLON-ALIGNED HELP
          "Número de dias a sancionar" WIDGET-ID 154
          LABEL "Días Sanción" FORMAT "999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 4.86 BY 1
     RowObject.Fec_Calificacion AT ROW 13.65 COL 80 COLON-ALIGNED HELP
          "Fecha de calificación del cliente" WIDGET-ID 186
          LABEL "F. Calificación" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY 1
     RowObject.Reportado_Procredito AT ROW 14.19 COL 4 HELP
          "Si es o no es reportado a Procrédito" WIDGET-ID 168
          LABEL "Reportado Procredito"
          VIEW-AS TOGGLE-BOX
          SIZE 25 BY .77
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.Cod_Segmento AT ROW 14.46 COL 42 COLON-ALIGNED HELP
          "Código del segmento al que pertenece" WIDGET-ID 152
          LABEL "Segmento" FORMAT "99999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 7.14 BY 1
     RowObject.Fec_Nacimiento AT ROW 14.73 COL 80 COLON-ALIGNED HELP
          "Fecha de nacimiento del cliente" WIDGET-ID 196
          LABEL "F. Nacimiento" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY 1
     RowObject.Aut_CentralRiesgo AT ROW 15 COL 4 WIDGET-ID 148
          LABEL "Autoriza Cons. C.Riesgo"
          VIEW-AS TOGGLE-BOX
          SIZE 26 BY .77
     RowObject.Reportado_Super AT ROW 15.81 COL 4 WIDGET-ID 170
          LABEL "Reportado Superbancaria"
          VIEW-AS TOGGLE-BOX
          SIZE 27 BY .77
     RowObject.Fec_fallecido AT ROW 15.81 COL 80 COLON-ALIGNED HELP
          "Fecha en la que fallecio el asociado" WIDGET-ID 190
          LABEL "F. Fallecido" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY 1
     RowObject.Id_Preexistentes AT ROW 16.62 COL 4 WIDGET-ID 156
          LABEL "Enf. Preexistentes"
          VIEW-AS TOGGLE-BOX
          SIZE 19.29 BY .77
     RowObject.Fec_expedicion AT ROW 16.88 COL 80 COLON-ALIGNED HELP
          "Fecha en que se originó el documento del cliente" WIDGET-ID 188
          LABEL "F. Expedición" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY 1
     "CONTROLES" VIEW-AS TEXT
          SIZE 12 BY .54 AT ROW 10.42 COL 25 WIDGET-ID 178
          FGCOLOR 9 
     "EGRESOS" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 2.62 COL 56 WIDGET-ID 122
          FGCOLOR 9 
     "INGRESOS" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 2.62 COL 14 WIDGET-ID 120
          FGCOLOR 9 
     "ACTIVOS" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 2.62 COL 95 WIDGET-ID 132
          FGCOLOR 9 
     "FECHAS" VIEW-AS TEXT
          SIZE 8 BY .54 AT ROW 10.42 COL 82 WIDGET-ID 198
          FGCOLOR 9 
     "PASIVOS" VIEW-AS TEXT
          SIZE 10 BY .54 AT ROW 8.27 COL 95 WIDGET-ID 144
          FGCOLOR 9 
     RECT-1 AT ROW 1 COL 1 WIDGET-ID 50
     RECT-3 AT ROW 2.62 COL 1 WIDGET-ID 88
     RECT-4 AT ROW 2.62 COL 41 WIDGET-ID 118
     RECT-9 AT ROW 2.62 COL 81 WIDGET-ID 124
     RECT-10 AT ROW 8.27 COL 81 WIDGET-ID 136
     RECT-5 AT ROW 10.42 COL 1 WIDGET-ID 146
     RECT-6 AT ROW 10.42 COL 62 WIDGET-ID 180
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dclientes.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dclientes.i}
      END-FIELDS.
   END-TABLES.
 */

/* This procedure should always be RUN PERSISTENT.  Report the error,  */
/* then cleanup and return.                                            */
IF NOT THIS-PROCEDURE:PERSISTENT THEN DO:
  MESSAGE "{&FILE-NAME} should only be RUN PERSISTENT.":U
          VIEW-AS ALERT-BOX ERROR BUTTONS OK.
  RETURN.
END.

&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW vTableWin ASSIGN
         HEIGHT             = 17.23
         WIDTH              = 118.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB vTableWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/viewer.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW vTableWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
   NOT-VISIBLE FRAME-NAME Size-to-Fit                                   */
ASSIGN 
       FRAME F-Main:SCROLLABLE       = FALSE
       FRAME F-Main:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN RowObject.Act_casa IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Act_inversion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Act_vehiculo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX RowObject.Aut_CentralRiesgo IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Calificacion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Cod_Segmento IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Dias_Sancion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.FCliente IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Fec_Asociacion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_Calificacion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_expedicion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_fallecido IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_IngEmpresa IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_Nacimiento IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX RowObject.FPrivilegiado IN FRAME F-Main
   NO-ENABLE EXP-LABEL                                                  */
/* SETTINGS FOR FILL-IN RowObject.FReestructurado IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FTotActivos IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FTotEgresos IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FTotIngresos IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.GtoFinanc_Indir IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Gto_Arriendo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Gto_Familiar IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Gto_obligacion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_Preexistentes IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Id_PuedeCodeudar IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Ing_arriendos IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Ing_financieros IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Ing_Honorarios IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Ing_Otros IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Nit IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX RowObject.Reportado_fiscalia IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR TOGGLE-BOX RowObject.Reportado_Procredito IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX RowObject.Reportado_Super IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Salario IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR TOGGLE-BOX RowObject.Sancionado IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK vTableWin 


/* ***************************  Main Block  *************************** */

  &IF DEFINED(UIB_IS_RUNNING) <> 0 &THEN          
    RUN initializeObject.
  &ENDIF         
  
  /************************ INTERNAL PROCEDURES ********************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI vTableWin  _DEFAULT-DISABLE
PROCEDURE disable_UI :
/*------------------------------------------------------------------------------
  Purpose:     DISABLE the User Interface
  Parameters:  <none>
  Notes:       Here we clean-up the user-interface by deleting
               dynamic widgets we have created and/or hide 
               frames.  This procedure is usually called when
               we are ready to "clean-up" after running.
------------------------------------------------------------------------------*/
  /* Hide all frames. */
  HIDE FRAME F-Main.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

