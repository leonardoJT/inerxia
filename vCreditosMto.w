&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dcreditos.i"}.



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

    DEFINE VARIABLE vhEstado AS HANDLE      NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dcreditos.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.For_Pago RowObject.Estado 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-6 
&Scoped-Define DISPLAYED-FIELDS RowObject.FAgencia RowObject.FNombre ~
RowObject.Nit RowObject.FCredito RowObject.Pagare RowObject.Num_Credito ~
RowObject.Fec_Desembolso RowObject.Monto RowObject.Plazo RowObject.Cuota ~
RowObject.Fec_UltPago RowObject.Sdo_Capital RowObject.For_Pago ~
RowObject.Estado 
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
     SIZE 60.29 BY 12.12.

DEFINE RECTANGLE RECT-6
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 43 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BTN-Titulo AT ROW 1.27 COL 10.72 WIDGET-ID 68
     RowObject.FAgencia AT ROW 3.69 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 10
          LABEL "Agencia" FORMAT "x(40)"
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.FNombre AT ROW 4.69 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 18
          LABEL "Cliente" FORMAT "x(30)"
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Nit AT ROW 5.85 COL 14 COLON-ALIGNED WIDGET-ID 76
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.FCredito AT ROW 6.92 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 12
          LABEL "Producto" FORMAT "x(35)"
          VIEW-AS FILL-IN 
          SIZE 42 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Pagare AT ROW 7.88 COL 14 COLON-ALIGNED HELP
          "número de pagaré que respalda el crédito" WIDGET-ID 28
          LABEL "Pagare" FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Num_Credito AT ROW 7.88 COL 39 COLON-ALIGNED HELP
          "Número de Crédito" WIDGET-ID 26
          LABEL "Num. Credito" FORMAT "999999999"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Fec_Desembolso AT ROW 8.96 COL 14 COLON-ALIGNED HELP
          "Fecha desembolso" WIDGET-ID 14
          LABEL "Fec. Desembolso" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Monto AT ROW 8.96 COL 39 COLON-ALIGNED HELP
          "Valor de Desembolso del crédito" WIDGET-ID 24
          LABEL "Monto" FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Plazo AT ROW 10.04 COL 14 COLON-ALIGNED HELP
          "plazo  del crédito" WIDGET-ID 30
          LABEL "Plazo" FORMAT ">>>9"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Cuota AT ROW 10.04 COL 39 COLON-ALIGNED HELP
          "Cuota Mensual del crédito" WIDGET-ID 2
          LABEL "Cuota" FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Fec_UltPago AT ROW 11.12 COL 14 COLON-ALIGNED HELP
          "fecha último pago Cuota" WIDGET-ID 16
          LABEL "Fec. Ult.Pago" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 12 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Sdo_Capital AT ROW 11.12 COL 39 COLON-ALIGNED HELP
          "Saldo de capital actual de deuda" WIDGET-ID 32
          LABEL "Saldo Capital" FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.For_Pago AT ROW 12.19 COL 16 NO-LABEL WIDGET-ID 20
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Caja", 1,
"Nómina", 2,
"DB Automático", 3
          SIZE 12 BY 1.62
          BGCOLOR 15 
     RowObject.Estado AT ROW 12.19 COL 41 NO-LABEL WIDGET-ID 4
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Aprobado", 1,
"Normal", 2,
"Cancelado", 3,
"Retirado", 4,
"Castigado", 5
          SIZE 17 BY 2.62
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     "CREDITOS" VIEW-AS TEXT
          SIZE 10 BY .5 AT ROW 1.54 COL 26.14 WIDGET-ID 74
          BGCOLOR 11 FONT 1
     "Estado Producto:" VIEW-AS TEXT
          SIZE 11.57 BY .5 AT ROW 12.31 COL 29 WIDGET-ID 36
     "Forma Pago:" VIEW-AS TEXT
          SIZE 8.57 BY .5 AT ROW 12.31 COL 7 WIDGET-ID 34
     RECT-2 AT ROW 3.15 COL 1 WIDGET-ID 70
     RECT-6 AT ROW 1 COL 9.72 WIDGET-ID 72
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17 FONT 4 WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dcreditos.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dcreditos.i}
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
         HEIGHT             = 14.27
         WIDTH              = 60.29.
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
/* SETTINGS FOR FILL-IN RowObject.Cuota IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.FAgencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FCredito IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Fec_Desembolso IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Fec_UltPago IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FNombre IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR RADIO-SET RowObject.For_Pago IN FRAME F-Main
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN RowObject.Monto IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Nit IN FRAME F-Main
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RowObject.Num_Credito IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Pagare IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Plazo IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Sdo_Capital IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F-Main
/* Query rebuild information for FRAME F-Main
     _Options          = "NO-LOCK"
     _Query            is NOT OPENED
*/  /* FRAME F-Main */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME RowObject.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.Estado vTableWin
ON VALUE-CHANGED OF RowObject.Estado IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE EQ "1" OR 
       SELF:SCREEN-VALUE EQ "4" OR 
       SELF:SCREEN-VALUE EQ "5" THEN DO:
        MESSAGE "El Valor de Estado Seleccionado" SKIP 
                "no se Encuentra Configurado para" SKIP
                "este Producto."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN resetRecord.
    END.
    ELSE DO:
        ASSIGN vhEstado = SELF:HANDLE.
        RUN fieldModified
        ( INPUT vhEstado /* HANDLE */).
    END.
    
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RowObject.For_Pago
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RowObject.For_Pago vTableWin
ON VALUE-CHANGED OF RowObject.For_Pago IN FRAME F-Main
DO:
    IF SELF:SCREEN-VALUE EQ "3" THEN DO:
        MESSAGE "El Valor de Forma de Pago" SKIP 
                "Seleccionado no se Encuentra" SKIP
                "Configurado para este Producto."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RUN resetRecord.
    END.
    ELSE DO:
        ASSIGN vhEstado = SELF:HANDLE.
        RUN fieldModified
        ( INPUT vhEstado /* HANDLE */).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

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

