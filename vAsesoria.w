&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME CURRENT-WINDOW
{adecomm/appserv.i}


/* Temp-Table and Buffer definitions                                    */
DEFINE TEMP-TABLE RowObject NO-UNDO
       {"dasesoria.i"}.



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

{incluido\Variable.i}

DEFINE VAR W_Rpta             AS LOGICAL.
DEFINE VARIABLE Tas_Nominal LIKE Solicitud.Tasa.
DEFINE VARIABLE viTipProd AS INTEGER     NO-UNDO.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartDataViewer
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER FRAME

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target

/* Include file with RowObject temp-table definition */
&Scoped-define DATA-FIELD-DEFS "dasesoria.i"

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS RowObject.Fec_Asesoria RowObject.Nit ~
RowObject.Monto RowObject.Cuota RowObject.Plazo RowObject.Per_Deduccion ~
RowObject.Val_EgresosMes RowObject.Fec_Apertura RowObject.Val_IngresosMes ~
RowObject.For_Liquidacion RowObject.Tasa RowObject.Estado ~
RowObject.Per_Liquidacion 
&Scoped-define ENABLED-TABLES RowObject
&Scoped-define FIRST-ENABLED-TABLE RowObject
&Scoped-Define ENABLED-OBJECTS RECT-1 RECT-2 BUTTON-2 BUTTON-4 BUTTON-168 
&Scoped-Define DISPLAYED-FIELDS RowObject.Fec_Asesoria RowObject.FUsuario ~
RowObject.Num_Asesoria RowObject.Clase_Producto RowObject.Nit ~
RowObject.FCliente RowObject.Monto RowObject.FAgencia RowObject.Cuota ~
RowObject.Plazo RowObject.FClase_Producto RowObject.Per_Deduccion ~
RowObject.FProducto RowObject.Val_EgresosMes RowObject.FPorcEndeud ~
RowObject.Fec_Apertura RowObject.Val_IngresosMes RowObject.For_Liquidacion ~
RowObject.Tasa RowObject.Estado RowObject.Usuario RowObject.Agencia ~
RowObject.Per_Liquidacion 
&Scoped-define DISPLAYED-TABLES RowObject
&Scoped-define FIRST-DISPLAYED-TABLE RowObject
&Scoped-Define DISPLAYED-OBJECTS FTip_Credito AS_Tasa 

/* Custom List Definitions                                              */
/* ADM-ASSIGN-FIELDS,List-2,List-3,List-4,List-5,List-6                 */
&Scoped-define ADM-ASSIGN-FIELDS AS_Tasa 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */


/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dpro_creditos AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynselect-Prod AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-168 
     LABEL "Button 168" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-2 
     LABEL "Button 2" 
     SIZE 15 BY 1.12.

DEFINE BUTTON BUTTON-4 
     LABEL "Button 4" 
     SIZE 15 BY 1.12.

DEFINE VARIABLE FTip_Credito AS INTEGER FORMAT "9":U INITIAL 1 
     LABEL "Tp. Producto" 
     VIEW-AS COMBO-BOX 
     LIST-ITEM-PAIRS "CONSUMO",1,
                     "COMERCIAL",2,
                     "HIPOTECARIO",3,
                     "MICROCREDITO",4
     DROP-DOWN-LIST
     SIZE 20 BY 1
     BGCOLOR 15 .

DEFINE VARIABLE AS_Tasa AS DECIMAL FORMAT ">9.9999999":U INITIAL 0 
     LABEL "Tasa" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .73
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107 BY 1.62.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 107 BY 1.62.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     RowObject.Fec_Asesoria AT ROW 1.69 COL 33 COLON-ALIGNED HELP
          "Ingrese la Fecha en que se brindo la asesoria" WIDGET-ID 24
          LABEL "Fecha" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     RowObject.FUsuario AT ROW 1.69 COL 56 COLON-ALIGNED HELP
          "" WIDGET-ID 54
          LABEL "Asesor" FORMAT "x(45)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 46.43 BY .81
          BGCOLOR 15 
     RowObject.Num_Asesoria AT ROW 1.81 COL 14 COLON-ALIGNED HELP
          "Ingrese el Consecutivo de Asesoria brindadas" WIDGET-ID 38
          LABEL "Asesoria" FORMAT "99999999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 10.57 BY .81
          BGCOLOR 15 
     RowObject.Clase_Producto AT ROW 2.62 COL 36 HELP
          "" NO-LABEL WIDGET-ID 4
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ahorro", 1,
"Crédito", 2
          SIZE 20 BY .81
     FTip_Credito AT ROW 3.42 COL 3.28 WIDGET-ID 70
     RowObject.Nit AT ROW 5.31 COL 14 COLON-ALIGNED HELP
          "Digite Identificación" WIDGET-ID 58
          LABEL "Nit" FORMAT "X(12)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 18 BY .81
          BGCOLOR 15 
     RowObject.FCliente AT ROW 5.31 COL 32 COLON-ALIGNED HELP
          "" NO-LABEL WIDGET-ID 20 FORMAT "x(50)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 51.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Monto AT ROW 6.38 COL 14 COLON-ALIGNED HELP
          "Ingrese el Monto del Prestamo" WIDGET-ID 34
          LABEL "Monto" FORMAT ">>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN NATIVE 
          SIZE 18 BY .81
          BGCOLOR 15 
     RowObject.FAgencia AT ROW 7.19 COL 62 COLON-ALIGNED HELP
          "" WIDGET-ID 16
          LABEL "Agencia" FORMAT "x(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 41.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Cuota AT ROW 7.46 COL 14 COLON-ALIGNED HELP
          "Ingrese la Cuota a pagar al producto asesorado" WIDGET-ID 10
          LABEL "Cuota" FORMAT ">>>,>>>,>>>,>>9"
          VIEW-AS FILL-IN NATIVE 
          SIZE 18 BY .81
          BGCOLOR 15 
     RowObject.Plazo AT ROW 8.54 COL 14 COLON-ALIGNED HELP
          "Ingrese el Plazo otorgado al producto asesorado" WIDGET-ID 44
          LABEL "Plazo (Meses)" FORMAT ">>>9"
          VIEW-AS FILL-IN NATIVE 
          SIZE 6 BY .81
          BGCOLOR 15 
     RowObject.FClase_Producto AT ROW 8.54 COL 63 COLON-ALIGNED HELP
          "" WIDGET-ID 18
          LABEL "Clase Prod." FORMAT "x(12)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 16.43 BY .81
          BGCOLOR 15 
     RowObject.Per_Deduccion AT ROW 9.62 COL 1 HELP
          "Ingrese el Periodo de deducción" WIDGET-ID 40
          LABEL "Per. Deducción" FORMAT "9"
          VIEW-AS COMBO-BOX INNER-LINES 4
          LIST-ITEM-PAIRS "SEMANAL",1,
                     "DECENAL",2,
                     "QUINCENAL",3,
                     "MENSUAL",4
          DROP-DOWN-LIST
          SIZE 24 BY 1
          BGCOLOR 15 
     RowObject.FProducto AT ROW 9.62 COL 63 COLON-ALIGNED HELP
          "" WIDGET-ID 30
          LABEL "Producto" FORMAT "x(40)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 41.43 BY .81
          BGCOLOR 15 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17  WIDGET-ID 100.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F-Main
     RowObject.Val_EgresosMes AT ROW 10.69 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 50
          LABEL "Egresos Mes" FORMAT ">>,>>>,>>>,>>9"
          VIEW-AS FILL-IN NATIVE 
          SIZE 17.43 BY .81
          BGCOLOR 15 
     RowObject.FPorcEndeud AT ROW 10.69 COL 63 COLON-ALIGNED HELP
          "" WIDGET-ID 76
          LABEL "%Endeudam." FORMAT ">>9.99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 8.29 BY .81
          BGCOLOR 18 FGCOLOR 15 
     RowObject.Fec_Apertura AT ROW 10.96 COL 88 COLON-ALIGNED HELP
          "Ingrese la Fecha de Apertura de la cuenta de ahorros" WIDGET-ID 22
          LABEL "Fec. Apertura" FORMAT "99/99/9999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 12.86 BY .81
          BGCOLOR 15 
     RowObject.Val_IngresosMes AT ROW 11.77 COL 14 COLON-ALIGNED HELP
          "" WIDGET-ID 52
          LABEL "Ingresos Mes" FORMAT ">>,>>>,>>>,>>9"
          VIEW-AS FILL-IN NATIVE 
          SIZE 17.43 BY .81
          BGCOLOR 15 
     RowObject.For_Liquidacion AT ROW 12.04 COL 90 HELP
          "Ingrese la Forma de Liquidacion de intereses" NO-LABEL WIDGET-ID 26
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Anticipado", 1,
"vencido", 2
          SIZE 12 BY 1.62
     BUTTON-2 AT ROW 12.31 COL 56 WIDGET-ID 60
     BUTTON-4 AT ROW 12.31 COL 71 WIDGET-ID 72
     RowObject.Tasa AT ROW 12.85 COL 14 COLON-ALIGNED HELP
          "Ingrese la Tasa del producto al cual se brindo asesoria" WIDGET-ID 46
          LABEL "Tasa" FORMAT ">>9.9999999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 14 BY .81
          BGCOLOR 15 
     AS_Tasa AT ROW 13.85 COL 14 COLON-ALIGNED WIDGET-ID 62
     RowObject.Estado AT ROW 14.19 COL 91 HELP
          "Ingrese el Estado en que se encuentra la asesoria" NO-LABEL WIDGET-ID 12
          VIEW-AS RADIO-SET VERTICAL
          RADIO-BUTTONS 
                    "Activa", 1,
"Cargada", 2,
"Inactiva", 3
          SIZE 11 BY 2.42
     RowObject.Usuario AT ROW 14.73 COL 67 COLON-ALIGNED HELP
          "Ingrese el Codigo de Usuario que brindo la asesoria" WIDGET-ID 48
          LABEL "Usuario" FORMAT "X(4)"
          VIEW-AS FILL-IN NATIVE 
          SIZE 8.43 BY .81
          BGCOLOR 10 
     RowObject.Agencia AT ROW 15.54 COL 67 COLON-ALIGNED HELP
          "Ingrese la Oficina donde se define el producto de creditos" WIDGET-ID 2
          LABEL "Agencia" FORMAT "999"
          VIEW-AS FILL-IN NATIVE 
          SIZE 4.86 BY .81
          BGCOLOR 10 
     RowObject.Per_Liquidacion AT ROW 16.08 COL 34 COLON-ALIGNED HELP
          "Ingrese el periodo de Liquidacion de Intereses en Días" WIDGET-ID 42
          LABEL "Periodo Liquidacion Int." FORMAT "99"
          VIEW-AS FILL-IN NATIVE 
          SIZE 3.72 BY .81
          BGCOLOR 15 
     BUTTON-168 AT ROW 18.77 COL 22 WIDGET-ID 78
     RECT-1 AT ROW 1.27 COL 2 WIDGET-ID 56
     RECT-2 AT ROW 3.15 COL 2 WIDGET-ID 74
     SPACE(0.00) SKIP(1.77)
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY USE-DICT-EXPS 
         SIDE-LABELS NO-UNDERLINE THREE-D NO-AUTO-VALIDATE 
         AT COL 1 ROW 1 SCROLLABLE 
         BGCOLOR 17  WIDGET-ID 100.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartDataViewer
   Data Source: "dasesoria.w"
   Allow: Basic,DB-Fields,Smart
   Container Links: Data-Target,Update-Source,TableIO-Target,GroupAssign-Source,GroupAssign-Target
   Frames: 1
   Add Fields to: Neither
   Other Settings: PERSISTENT-ONLY COMPILE APPSERVER
   Temp-Tables and Buffers:
      TABLE: RowObject D "?" NO-UNDO  
      ADDITIONAL-FIELDS:
          {dasesoria.i}
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
         HEIGHT             = 22.31
         WIDTH              = 122.72.
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

/* SETTINGS FOR FILL-IN RowObject.Agencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN AS_Tasa IN FRAME F-Main
   NO-ENABLE 1                                                          */
/* SETTINGS FOR RADIO-SET RowObject.Clase_Producto IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-HELP                                         */
/* SETTINGS FOR FILL-IN RowObject.Cuota IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR RADIO-SET RowObject.Estado IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.FAgencia IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FClase_Producto IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FCliente IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Fec_Apertura IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Fec_Asesoria IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR RADIO-SET RowObject.For_Liquidacion IN FRAME F-Main
   EXP-LABEL EXP-HELP                                                   */
/* SETTINGS FOR FILL-IN RowObject.FPorcEndeud IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.FProducto IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR COMBO-BOX FTip_Credito IN FRAME F-Main
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN RowObject.FUsuario IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Monto IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Nit IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Num_Asesoria IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR COMBO-BOX RowObject.Per_Deduccion IN FRAME F-Main
   ALIGN-L EXP-LABEL EXP-FORMAT EXP-HELP                                */
/* SETTINGS FOR FILL-IN RowObject.Per_Liquidacion IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Plazo IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Tasa IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Usuario IN FRAME F-Main
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN RowObject.Val_EgresosMes IN FRAME F-Main
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN RowObject.Val_IngresosMes IN FRAME F-Main
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

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME BUTTON-168
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-168 vTableWin
ON CHOOSE OF BUTTON-168 IN FRAME F-Main /* Button 168 */
DO:
  DISABLE RowObject.Num_Asesoria WITH FRAME {&FRAME-NAME}.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 vTableWin
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Button 2 */
DO: 
/*     RUN buscarIndicadores. */

MESSAGE "FTip_Credito " FTip_Credito:SCREEN-VALUE SKIP
    "prod " DYNAMIC-FUNCTION('columnValue':U IN h_dpro_creditos,
     INPUT "cod_credito")
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

/*     DYNAMIC-FUNCTION('setQueryWhere':U IN h_dpro_creditos, */
/*        INPUT "tip_Credito = " + vcTpProd).                 */
/*     DYNAMIC-FUNCTION('openQuery':U IN h_dpro_creditos).    */

/*     RUN Hallar_Tasa (INPUT Pro_Ahorros.Indicador, INPUT DECIMAL(Asesoria.Monto:SCREEN-VALUE),     */
/*                      INPUT DECIMAL(Asesoria.Plazo:SCREEN-VALUE), OUTPUT W_Tasa, OUTPUT W_Puntos). */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-4
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-4 vTableWin
ON CHOOSE OF BUTTON-4 IN FRAME F-Main /* Button 4 */
DO:
  MESSAGE FTip_Credito:SCREEN-VALUE
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FTip_Credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FTip_Credito vTableWin
ON VALUE-CHANGED OF FTip_Credito IN FRAME F-Main /* Tp. Producto */
DO:
    DYNAMIC-FUNCTION('setQueryWhere':U IN h_dpro_creditos,
       INPUT "tip_Credito = " + FTip_Credito:SCREEN-VALUE).                
    DYNAMIC-FUNCTION('openQuery':U IN h_dpro_creditos).   

    MESSAGE "aPLICA VALUE " FTip_Credito:SCREEN-VALUE
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE addRecord vTableWin 
PROCEDURE addRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN FTip_Credito:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects vTableWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dpro_creditos.wDB-AWARE':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedpro_creditosOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposyesToggleDataTargetsyes':U ,
             OUTPUT h_dpro_creditos ).
       RUN repositionObject IN h_dpro_creditos ( 5.04 , 96.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 6.29 ) */

       RUN constructObject (
             INPUT  'adm2/dynselect.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'AutoRefreshnoChangedEventDisplayedFieldFProductoKeyFieldCod_CreditoDataSourceFilterNumRows5OptionalnoOptionalString':U + '<none>' + 'LabelProductoSortyesViewAsBrowserToolTipFormat?HelpId0BrowseTitlePRODUCTOSBrowseFieldsCod_Credito,FProductoExitBrowseOnActionyesCancelBrowseOnExityesRepositionDataSourceyesDefineAnyKeyTriggeryesStartBrowseKeysFieldNameCod_ProductoDisplayFieldyesEnableFieldyesLocalFieldnoHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynselect-Prod ).
       RUN repositionObject IN h_dynselect-Prod ( 3.42 , 58.00 ) NO-ERROR.
       RUN resizeObject IN h_dynselect-Prod ( 1.00 , 46.00 ) NO-ERROR.

       /* Links to SmartDataField h_dynselect-Prod. */
       RUN addLink ( h_dpro_creditos , 'Data':U , h_dynselect-Prod ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynselect-Prod ,
             FTip_Credito:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE buscarIndicadores vTableWin 
PROCEDURE buscarIndicadores :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

/*     FIND Pro_Creditos WHERE                                                                                         */
/*       Pro_Creditos.Tip_Credito EQ INTEGER(SUBSTRING(Cmb_TipoProductos:SCREEN-VALUE IN FRAME {&FRAME-NAME},1,1)) AND */
/*       Pro_Creditos.Cod_Credito EQ DYNAMIC-FUNCTION('columnValue':U IN h_dpro_creditos, INPUT "Cod_Credito")         */
/*         NO-LOCK NO-ERROR.                                                                                           */
/*     IF AVAILABLE(pro_creditos) THEN                                                                                 */
/*         MESSAGE "ok. Pro_creditos"                                                                                  */
/*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                      */


     IF Pro_Creditos.Id_Tasa EQ 1 THEN DO:
        FIND FIRST Indicadores WHERE Indicadores.Indicador EQ Pro_Creditos.Cod_Tasa
                                 AND Indicadores.Estado    EQ 1 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE(Indicadores) THEN DO:
           MESSAGE "No exite un indicadores para la linea" SKIP
                   "de producto de crédito. Consulte con el Administrador" SKIP
                   "del sistema acerca de esta inconsistencia" VIEW-AS ALERT-BOX ERROR TITLE "Configuración del Indicador".
           APPLY "ENTRY" TO RowObject.Monto IN FRAME {&FRAME-NAME}.
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
     
     RowObject.Tasa:SCREEN-VALUE = STRING(Tas_Nominal / 12).
     CASE Per_Deduccion:SCREEN-VALUE:
         WHEN "1" THEN
             ASSIGN RowObject.Tasa:SCREEN-VALUE = STRING(Tas_Nominal / 52).
         WHEN "2" THEN
             ASSIGN RowObject.Tasa:SCREEN-VALUE = STRING(Tas_Nominal / 36).
         WHEN "3" THEN
             ASSIGN RowObject.Tasa:SCREEN-VALUE = STRING(Tas_Nominal / 24).
     END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE cancelRecord vTableWin 
PROCEDURE cancelRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN FTip_Credito:SENSITIVE IN FRAME {&FRAME-NAME} = FALSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE copyRecord vTableWin 
PROCEDURE copyRecord :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN FTip_Credito:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE displayFields vTableWin 
PROCEDURE displayFields :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcColValues AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcColValues).

  /* Code placed here will execute AFTER standard behavior.    */

  FIND FIRST pro_Creditos WHERE cod_credito EQ DYNAMIC-FUNCTION('columnValue':U IN h_dpro_creditos,
     INPUT "cod_credito") NO-LOCK NO-ERROR.
  ASSIGN FTip_Credito:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Pro_Creditos.Tip_Credito).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE hallarTasa vTableWin 
PROCEDURE hallarTasa :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


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
     END. /*IF NOT Indicadores.Rangos*/
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
        END. /*IF AVAILABLE Ran_Intereses */
        ELSE DO:
           RUN MostrarMensaje IN W_Manija (INPUT 257, OUTPUT W_Rpta).
           ASSIGN W_TasaW   = 0
                  W_PuntosW = 0.
        END. /*ELSE DO:*/
     END. /*ELSE DO:*/
  END. /*IF AVAILABLE Indicadores*/
  ELSE DO:
     MESSAGE "No se ha encontrado un Indicador Valido para" SKIP
             "el producto: " rowObject.Cod_producto SKIP
             "Rectifique o cree el indicador requerido" VIEW-AS ALERT-BOX ERROR.
  END. /* ELSE DO: IF AVAILABLE Indicadores */



END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject vTableWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */

  DEFINE VARIABLE vcTpProd AS CHARACTER   NO-UNDO.


/*   DYNAMIC-FUNCTION('setQueryWhere':U IN h_dpro_creditos,                         */
/*      INPUT "tip_Credito = " + FTip_Credito:SCREEN-VALUE IN FRAME {&FRAME-NAME}). */
/*   DYNAMIC-FUNCTION('openQuery':U IN h_dpro_creditos).                            */

  ASSIGN 
      RowObject.Clase_Producto:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "2"
      RowObject.per_Deduccion:SCREEN-VALUE = "4".  


/*   DYNAMIC-FUNCTION('setQueryWhere':U IN h_dpro_creditos,  */
/*      INPUT "tip_Credito = " + vcTpProd).                  */
/*   DYNAMIC-FUNCTION('openQuery':U IN h_dpro_creditos).     */


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE updateMode vTableWin 
PROCEDURE updateMode :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  DEFINE INPUT PARAMETER pcMode AS CHARACTER NO-UNDO.

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER( INPUT pcMode).

  /* Code placed here will execute AFTER standard behavior.    */

  ASSIGN FTip_Credito:SENSITIVE IN FRAME {&FRAME-NAME} = TRUE.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

