&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-ProCre 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
   DEFINE SHARED VAR W_Nom_Agencia    AS   CHARACTER FORMAT "X(60)".
   DEFINE SHARED VAR W_Nom_Entidad    AS   CHARACTER FORMAT "X(60)".
   DEFINE SHARED VAR W_Usuario        LIKE Usuarios.Usuario.
   DEFINE SHARED VAR W_Estacion       LIKE Estaciones.Estacion.
   DEFINE SHARED VAR W_Manija         AS   HANDLE.
   DEFINE SHARED VAR W_PathSpl        LIKE Entidad.Dir_Spl.
   DEFINE VAR W_Rpta                  AS LOGICAL.
   DEFINE VAR W_TamOfi                AS INTEGER INITIAL 0.
   DEFINE VAR W_OfiTra                LIKE Agencias.Agencia INITIAL 0.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-CarLar

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-270 BUTTON-1 Btn_Impresion Btn_Consulta ~
Btn_Ayuda 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-ProCre AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_cortolargo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_cortolargo AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda"
     FONT 9.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes\lupa":U
     LABEL "Co&nsulta" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "I&mpresión" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra interace de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes\informacion":U
     LABEL "Button 1" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-270
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE BUTTON Btn_Terminar 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.65.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-CarLar
     BUTTON-1 AT ROW 1.81 COL 103
     Btn_Impresion AT ROW 3.42 COL 103 HELP
          "Permite Generar la impresión de Cartera a largo y Corto Plazo"
     Btn_Consulta AT ROW 5.04 COL 103 HELP
          "Permite Generar la consulta a Corto y Largo Plazo"
     Btn_Ayuda AT ROW 20.38 COL 106 HELP
          "Permite Generar la consulta de la pantalla"
     RECT-270 AT ROW 1.54 COL 102
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.29 BY 21.12
         BGCOLOR 17 .

DEFINE FRAME Frm_consulta
     Btn_Terminar AT ROW 17.15 COL 34 HELP
          "Termina la Consulta de La cartera de Corto y largo plazo"
     "Nota:" VIEW-AS TEXT
          SIZE 5 BY .81 AT ROW 14.73 COL 3
          FGCOLOR 7 FONT 5
     "Escoja el filtro por el cual desea realizar la busqueda," VIEW-AS TEXT
          SIZE 35.29 BY .81 AT ROW 15.54 COL 3
          FONT 4
     "digite la palabra clave a buscar, luego presione ENTER." VIEW-AS TEXT
          SIZE 39 BY .62 AT ROW 16.35 COL 3
          FONT 4
     "(Clase 1: Ahorros / Clase 2: Créditos)" VIEW-AS TEXT
          SIZE 32 BY .81 AT ROW 1 COL 6
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 71.86 ROW 1.27
         SIZE 41.29 BY 18.96
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE FGCOLOR 0 "Consulta de Productos".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW W-ProCre ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración Contable de Cartera de Corto y Largo Plazo"
         HEIGHT             = 21.12
         WIDTH              = 114.29
         MAX-HEIGHT         = 21.19
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21.19
         VIRTUAL-WIDTH      = 114.29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-ProCre 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-ProCre
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-CarLar
   FRAME-NAME                                                           */
/* SETTINGS FOR FRAME Frm_consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME Frm_consulta:HIDDEN           = TRUE
       FRAME Frm_consulta:SELECTABLE       = TRUE
       FRAME Frm_consulta:MOVABLE          = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProCre)
THEN W-ProCre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON END-ERROR OF W-ProCre /* SFG - Configuración Contable de Cartera de Corto y Largo Plazo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON WINDOW-CLOSE OF W-ProCre /* SFG - Configuración Contable de Cartera de Corto y Largo Plazo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-ProCre
ON CHOOSE OF Btn_Ayuda IN FRAME F-CarLar
OR HELP OF {&WINDOW-NAME} DO:
  SYSTEM-HELP "Ayudas\Redecoop.HLP" CONTEXT 5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-ProCre
ON CHOOSE OF Btn_Consulta IN FRAME F-CarLar /* Consulta */
DO:
  VIEW FRAME Frm_Consulta.
  DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-CarLar.     
  FRAME {&frame-name}:SENSITIVE = FALSE.
  ENABLE ALL WITH FRAME Frm_Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-ProCre
ON CHOOSE OF Btn_Impresion IN FRAME F-CarLar /* Impresión */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  Listado = W_PathSpl + "L_corLar.LST".
  {Incluido/Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME Frm_consulta
&Scoped-define SELF-NAME Btn_Terminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Terminar W-ProCre
ON CHOOSE OF Btn_Terminar IN FRAME Frm_consulta /* Terminar Consulta */
DO:
  HIDE FRAME FRM_Consulta.
  ENABLE ALL WITH FRAME F-CarLar.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-CarLar
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-ProCre
ON CHOOSE OF BUTTON-1 IN FRAME F-CarLar /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-ProCre 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */

{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-ProCre  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE adm-current-page  AS INTEGER NO-UNDO.

  RUN get-attribute IN THIS-PROCEDURE ('Current-Page':U).
  ASSIGN adm-current-page = INTEGER(RETURN-VALUE).

  CASE adm-current-page: 

    WHEN 0 THEN DO:
       RUN init-object IN THIS-PROCEDURE (
             INPUT  'v-cf_cortolargo.w':U ,
             INPUT  FRAME F-CarLar:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_cortolargo ).
       RUN set-position IN h_v-cf_cortolargo ( 1.54 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.65 , 97.57 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'b-cf_cortolargo.w':U ,
             INPUT  FRAME Frm_consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_cortolargo ).
       RUN set-position IN h_b-cf_cortolargo ( 1.81 , 3.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_cortolargo ( 12.92 , 37.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-CarLar:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 9.88 , 102.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 10.23 , 12.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_cortolargo. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_cortolargo , 'Record':U , h_v-cf_cortolargo ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_cortolargo ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_cortolargo ,
             BUTTON-1:HANDLE IN FRAME F-CarLar , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_cortolargo ,
             Btn_Terminar:HANDLE IN FRAME Frm_consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             Btn_Consulta:HANDLE IN FRAME F-CarLar , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-ProCre  _ADM-ROW-AVAILABLE
PROCEDURE adm-row-available :
/*------------------------------------------------------------------------------
  Purpose:     Dispatched to this procedure when the Record-
               Source has a new row available.  This procedure
               tries to get the new row (or foriegn keys) from
               the Record-Source and process it.
  Parameters:  <none>
------------------------------------------------------------------------------*/

  /* Define variables needed by this internal procedure.             */
  {src/adm/template/row-head.i}

  /* Process the newly available records (i.e. display fields,
     open queries, and/or pass records on to any RECORD-TARGETS).    */
  {src/adm/template/row-end.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-ProCre  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProCre)
  THEN DELETE WIDGET W-ProCre.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-ProCre  _DEFAULT-ENABLE
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
  ENABLE RECT-270 BUTTON-1 Btn_Impresion Btn_Consulta Btn_Ayuda 
      WITH FRAME F-CarLar IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-F-CarLar}
  ENABLE Btn_Terminar 
      WITH FRAME Frm_consulta IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-Frm_consulta}
  VIEW W-ProCre.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-ProCre 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 
 DEFINE VAR W_NomPcto   AS CHARACTER FORMAT "X(30)"  INITIAL "".
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
    
 E_NumFila = 1.
 E_NumColumn = 9.
 E_Fila      = "001" + "P" 
             + "003" + "Cod"
             + "031" + "Nombre Producto                "
             + "012" + "Adm.Asoc    "
             + "012" + "Adm.No.Asoc "
             + "012" + "No.Adm.Asoc "
             + "012" + "No.Adm.No.As"
             + "004" + "RIni"
             + "004" + "RFin".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

  FOR EACH CortoLargo NO-LOCK BY CortoLargo.Clase_Producto:
      IF CortoLargo.Clase_Producto EQ 1 THEN DO:
         FIND FIRST Pro_Ahorros WHERE pro_ahorros.cod_ahorro EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Ahorros) THEN DO:
            ASSIGN W_NomPcto = Pro_Ahorros.Nom_Producto.
         END.
      END.
      ELSE DO:
         FIND FIRST Pro_Creditos WHERE pro_Creditos.cod_credito EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Creditos) THEN DO:
            ASSIGN W_NomPcto = Pro_Creditos.Nom_Producto.
         END.
      END.
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(CortoLargo.Clase_Producto,"9")
                  + "003" + STRING(CortoLargo.Cod_Producto,"999")
                  + "031" + STRING(W_NomPcto,"X(31)")
                  + "012" + STRING(CortoLargo.Cta_AsoAd,"X(14)")
                  + "012" + STRING(CortoLargo.Cta_NoaAd,"X(14)")
                  + "012" + STRING(CortoLargo.Cta_AsoNa,"X(14)")
                  + "012" + STRING(CortoLargo.Cta_NoaNa,"X(14)")
                  + "004" + STRING(CortoLargo.Plazo_Inicial,"9999")
                  + "004" + STRING(CortoLargo.Plazo_Final,"9999").
      {Incluido\imprimir_Excel.i}

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-ProCre 
PROCEDURE local-exit :
/* -----------------------------------------------------------
  Purpose:  Starts an "exit" by APPLYing CLOSE event, which starts "destroy".
  Parameters:  <none>
  Notes:    If activated, should APPLY CLOSE, *not* dispatch adm-exit.   
-------------------------------------------------------------*/
   APPLY "CLOSE":U TO THIS-PROCEDURE.
   
   RETURN.
       
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-ProCre 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
    W_Reporte    = "REPORTE   : CONFIGURAICON CUENTAS CORTO Y LARGO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "PRO COD AGE NOMBRE PRODUCTO                 ADM.ASOC     ADM.NO.ASOC  NO.ADM.ASOC  NO.ADM.NO.AS RINI RFIN".

/*         1         2         3         4         5         6         7         8         9         1
123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
PRO COD NOMBRE PRODUCTO                 ADM.ASOC     ADM.NO.ASOC  NO.ADM.ASOC  NO.ADM.NO.AS RINI RFIN
----------------------------------------------------------------------------------------------------
 1  002 Multidiario                     21150101     21150101                               0000 9999
 */

  DEFINE VAR W_NomPcto   AS CHARACTER FORMAT "X(30)"  INITIAL "".
  
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
         
  FOR EACH CortoLargo NO-LOCK BY CortoLargo.Clase_Producto:
      IF CortoLargo.Clase_Producto EQ 1 THEN DO:
         FIND FIRST Pro_Ahorros WHERE pro_ahorros.cod_ahorro EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Ahorros) THEN DO:
            ASSIGN W_NomPcto = Pro_Ahorros.Nom_Producto.
         END.
      END.
      ELSE DO:
         FIND FIRST Pro_Creditos WHERE pro_Creditos.cod_credito EQ CortoLargo.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Creditos) THEN DO:
            ASSIGN W_NomPcto = Pro_Creditos.Nom_Producto.
         END.
      END.
      DISPLAY CortoLargo.Clase_Producto AT 2   NO-LABEL
              CortoLargo.Cod_Producto   AT 5   NO-LABEL
              CortoLargo.Agencia        AT 9   NO-LABEL FORMAT "999"
              W_NomPcto                 AT 13   NO-LABEL FORMAT "X(31)"
              CortoLargo.Cta_AsoAd      AT 45  NO-LABEL FORMAT "X(16)"
              CortoLargo.Cta_NoaAd      AT 58  NO-LABEL FORMAT "X(16)"
              CortoLargo.Cta_AsoNa      AT 71  NO-LABEL FORMAT "X(16)"
              CortoLargo.Cta_NoaNa      AT 84  NO-LABEL FORMAT "X(16)"
              CortoLargo.Plazo_Inicial  AT 97 NO-LABEL
              CortoLargo.Plazo_Final    AT 101 NO-LABEL
      WITH FRAME F-Interes1 DOWN WIDTH 200 USE-TEXT STREAM-IO NO-BOX.
      DOWN WITH FRAME F-Interes1.
      ASSIGN W_NomPcto = "".
  END.  
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-ProCre  _ADM-SEND-RECORDS
PROCEDURE send-records :
/*------------------------------------------------------------------------------
  Purpose:     Send record ROWID's for all tables used by
               this file.
  Parameters:  see template/snd-head.i
------------------------------------------------------------------------------*/

  /* SEND-RECORDS does nothing because there are no External
     Tables specified for this SmartWindow, and there are no
     tables specified in any contained Browse, Query, or Frame. */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-ProCre 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state      AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

