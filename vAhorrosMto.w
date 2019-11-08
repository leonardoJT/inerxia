&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dahorros.i"}.



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
&Scoped-define DATA-FIELD-DEFS "dahorros.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Estado RowObject.For_Liquidacion ~
RowObject.Fec_Cancelacion RowObject.Fec_ProLiquidacion ~
RowObject.Per_Liquidacion RowObject.Fec_Vencimiento RowObject.Plazo 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-6 
&Scoped-Define DISPLAYED-FIELDS RowObject.FAgencia RowObject.FCliente ~
RowObject.FProducto RowObject.Cue_Ahorros RowObject.Fec_Apertura ~
RowObject.Estado RowObject.For_Liquidacion RowObject.Fec_Cancelacion ~
RowObject.Fec_ProLiquidacion RowObject.Per_Liquidacion ~
RowObject.Fec_Vencimiento RowObject.Monto_Apertura RowObject.Plazo 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject


/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of the field level widgets                               */
DEFINE BUTTON BTN-Titulo 
     LABEL "" 
     SIZE 41 BY 1.12.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 78 BY 10.77.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BTN-Titulo AT ROW 1.27 COL 19.57 WIDGET-ID 32
     RowObject.FAgencia AT ROW 3.96 COL 19 COLON-ALIGNED HELP
          "" WIDGET-ID 86
          LABEL "Agencia" FORMAT "x(45)"
          VIEW-AS FILL-IN 
          SIZE 43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.FCliente AT ROW 5.04 COL 19.14 COLON-ALIGNED HELP
          "" WIDGET-ID 88
          LABEL "Cliente"
          VIEW-AS FILL-IN 
          SIZE 43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.FProducto AT ROW 6.12 COL 19.14 COLON-ALIGNED HELP
          "" WIDGET-ID 90
          LABEL "Producto" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Cue_Ahorros AT ROW 7.19 COL 19 COLON-ALIGNED HELP
          "Numero que identifica la cuenta de ahorros" WIDGET-ID 52
          LABEL "Cuenta" FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Fec_Apertura AT ROW 7.19 COL 52.14 COLON-ALIGNED HELP
          "Ingrese la Fecha de Apertura de la cuenta de ahorros" WIDGET-ID 58
          LABEL "Fecha de Apertura" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Estado AT ROW 8.27 COL 21 NO-LABEL WIDGET-ID 54
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Inactiva", 2
          SIZE 9.57 BY 1.58
          BGCOLOR 15 
     RowObject.For_Liquidacion AT ROW 8.27 COL 54 NO-LABEL WIDGET-ID 66
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Anticipado", 1,
"vencido", 2
          SIZE 10 BY 1.58
          BGCOLOR 15 
     RowObject.Fec_Cancelacion AT ROW 10.15 COL 19 COLON-ALIGNED HELP
          "Fecha de cancelacion de la cuenta" WIDGET-ID 60
          LABEL "Fec. Cancelación" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10 BY .81
          BGCOLOR 15 
     RowObject.Fec_ProLiquidacion AT ROW 10.15 COL 52.14 COLON-ALIGNED HELP
          "Esta es la fecha proxima de liquidación de intereses" WIDGET-ID 62
          LABEL "Fec. Prox. Liquidación" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10 BY .81
          BGCOLOR 15 
     RowObject.Per_Liquidacion AT ROW 11.23 COL 4.28 HELP
          "Ingrese el periodo de Liquidacion de Intereses en Días" WIDGET-ID 80
          LABEL "Periodo Liquidacion Int." FORMAT "99"
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEM-PAIRS "No Configurado",0,
                     "Diario",1,
                     "Mensual",2,
                     "Trimestral",3,
                     "Semestral",4,
                     "Anual",5,
                     "Al Vencimiento",6
          DROP-DOWN-LIST
          SIZE 20.86 BY 1 TOOLTIP "01-diario, 02-Mensual, 03-Trimestral, 04-Semestral, 05- Anual, 06-Alvencimiento"
          BGCOLOR 15 
     RowObject.Fec_Vencimiento AT ROW 11.23 COL 52 COLON-ALIGNED HELP
          "Fecha de Vencimiento" WIDGET-ID 64
          LABEL "Fec. Vencim." FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10 BY .81
          BGCOLOR 15 
     RowObject.Monto_Apertura AT ROW 12.31 COL 19 COLON-ALIGNED HELP
          "Valor de Apertura de la Cuenta" WIDGET-ID 74
          LABEL "Monto de Apertura" FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 21 BY .81
          BGCOLOR 18 FGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.Plazo AT ROW 12.31 COL 52 COLON-ALIGNED HELP
          "Ingrese el Plazo para el producto de ahorro" WIDGET-ID 78
          LABEL "Plazo" FORMAT "9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10 BY .81
          BGCOLOR 15 
     "AHORROS" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 1.54 COL 35 WIDGET-ID 30
          BGCOLOR 11 FONT 1
     "Estado:" VIEW-AS TEXT
          SIZE 5 BY .5 AT ROW 8.27 COL 16 WIDGET-ID 82
     "Forma de Liquidacion:" VIEW-AS TEXT
          SIZE 15 BY .65 AT ROW 8.27 COL 39 WIDGET-ID 84
     RECT-2 AT ROW 3.15 COL 1 WIDGET-ID 34
     RECT-6 AT ROW 1 COL 18.57 WIDGET-ID 36
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dahorros.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dahorros.i}
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
         HEIGHT             = 12.92
         WIDTH              = 78.
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

/* SETTINGS FOR BUTTON BTN-Titulo IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Cue_Ahorros IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.FAgencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FCliente IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN RowObject.Fec_Apertura IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Fec_Cancelacion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_ProLiquidacion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_Vencimiento IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR RADIO-SET RowObject.For_Liquidacion IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.FProducto IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Monto_Apertura IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR COMBO-BOX RowObject.Per_Liquidacion IN FRAME F-Main
   ALIGN-L EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN RowObject.Plazo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
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

