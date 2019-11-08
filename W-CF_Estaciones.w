&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Win 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */

  DEFINE VARIABLE W_NomCar             AS CHARACTER FORMAT "X(30)" INITIAL "".
  DEFINE VARIABLE W_PriUsu             AS INTEGER FORMAT 9.
  DEFINE VARIABLE W_Estado             AS CHARACTER FORMAT "X(8)" INITIAL "".
  DEFINE VARIABLE W_Rowid              AS ROWID.
  DEFINE VARIABLE W-Procedure          AS CHARACTER.

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

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Main

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS BUTTON-52 Bt_Imprimir Bt_Buscar BUTTON-53 ~
RECT-236 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_p-updsav AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-cf_estaciones AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_estaciones AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Bt_Buscar 
     IMAGE-UP FILE "imagenes\lupa":U
     LABEL "Button 50" 
     SIZE 10 BY 1.65 TOOLTIP "Busqueda de información de la pantalla en uso".

DEFINE BUTTON Bt_Imprimir 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "Button 51" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la interface de salida de información (Reportes)".

DEFINE BUTTON BUTTON-52 
     IMAGE-UP FILE "imagenes\informacion3":U
     LABEL "Button 52" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra la pantalla de información de Sesión y los mensajes del usuario activo".

DEFINE BUTTON BUTTON-53 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "Button 53" 
     SIZE 4 BY 1.12 TOOLTIP "Ayuda".

DEFINE RECTANGLE RECT-236
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-52 AT ROW 1.81 COL 101
     Bt_Imprimir AT ROW 3.42 COL 101
     Bt_Buscar AT ROW 5.04 COL 101
     BUTTON-53 AT ROW 19.58 COL 104
     RECT-236 AT ROW 1.54 COL 100
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 20.85
         BGCOLOR 17 FONT 5.


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
  CREATE WINDOW W-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de Estaciones de Trabajo del Aplicativo"
         HEIGHT             = 20.92
         WIDTH              = 114.14
         MAX-HEIGHT         = 21.85
         MAX-WIDTH          = 114.14
         VIRTUAL-HEIGHT     = 21.85
         VIRTUAL-WIDTH      = 114.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Win 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
THEN W-Win:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON END-ERROR OF W-Win /* SFG - Configuración de Estaciones de Trabajo del Aplicativo */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Win W-Win
ON WINDOW-CLOSE OF W-Win /* SFG - Configuración de Estaciones de Trabajo del Aplicativo */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bt_Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bt_Buscar W-Win
ON CHOOSE OF Bt_Buscar IN FRAME F-Main /* Button 50 */
DO:
  ASSIGN W_Rowid = ?.
  RUN D-ConEst.r (INPUT-OUTPUT W_Rowid).
  IF W_Rowid EQ ? THEN
     RETURN NO-APPLY.     

  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT "Query", OUTPUT W-Procedure).
  RUN Reposiciona-query IN WIDGET-HANDLE(W-Procedure) (INPUT-OUTPUT W_Rowid).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Bt_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Bt_Imprimir W-Win
ON CHOOSE OF Bt_Imprimir IN FRAME F-Main /* Button 51 */
DO:
   DEFINE VAR Listado AS CHARACTER INITIAL "".
   
   Listado = W_PathSpl + "Estaciones.LST".
  {Incluido\Imprimir.I "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-52
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-52 W-Win
ON CHOOSE OF BUTTON-52 IN FRAME F-Main /* Button 52 */
DO:
  RUN W-InfDia.r.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Win 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Win  _ADM-CREATE-OBJECTS
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
             INPUT  'v-cf_estaciones.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_estaciones ).
       RUN set-position IN h_v-cf_estaciones ( 1.81 , 14.00 ) NO-ERROR.
       /* Size in UIB:  ( 19.42 , 75.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'p-updsav.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_p-updsav ).
       RUN set-position IN h_p-updsav ( 9.62 , 100.00 ) NO-ERROR.
       RUN set-size IN h_p-updsav ( 9.69 , 12.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'q-cf_estaciones.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-cf_estaciones ).
       RUN set-position IN h_q-cf_estaciones ( 6.12 , 4.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.50 , 7.72 ) */

       /* Links to SmartViewer h_v-cf_estaciones. */
       RUN add-link IN adm-broker-hdl ( h_p-updsav , 'TableIO':U , h_v-cf_estaciones ).
       RUN add-link IN adm-broker-hdl ( h_q-cf_estaciones , 'Record':U , h_v-cf_estaciones ).

       /* Links to SmartQuery h_q-cf_estaciones. */
       RUN add-link IN adm-broker-hdl ( h_q-cf_estaciones , 'Query':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_estaciones ,
             BUTTON-52:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_p-updsav ,
             Bt_Buscar:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Win  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Win  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Win)
  THEN DELETE WIDGET W-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Win  _DEFAULT-ENABLE
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
  ENABLE BUTTON-52 Bt_Imprimir Bt_Buscar BUTTON-53 RECT-236 
      WITH FRAME F-Main IN WINDOW W-Win.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-Win 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
    DEFINE VAR HoraE       AS CHARACTER FORMAT "X(8)".
    DEFINE VAR HoraS       AS CHARACTER FORMAT "X(8)".
 E_NumFila = 1.
 E_NumColumn = 9.
 E_Fila      = "003Age010Codigo    030Descripcion                   008FecCrea 008FecRet  008FecUltEn008HoraEnt 008FecUltSa008HoraSal ".
 
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).
 
/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).
 DEFI VAR FecRet AS CHARACTER FORMAT "x(8)".
 DEFI VAR FecUEn AS CHARACTER FORMAT "x(8)".
 DEFI VAR FecUSa AS CHARACTER FORMAT "x(8)".
 DEFI VAR FecCre AS CHARACTER FORMAT "x(8)".
 FOR EACH Estaciones BY Estaciones.Agencia:
     ASSIGN HoraE = ""
            HoraS  = ""
            FecRet = ""
            FecUEn = ""
            FecUSa = ""
            FecCre = "".
     IF Estaciones.Fec_Retiro NE ? THEN
         FecRet = STRING(Estaciones.Fec_Retiro,"99/99/99").
     IF Estaciones.Fec_UltEntrada NE ? THEN
         FecUEn = STRING(Estaciones.Fec_UltEntrada,"99/99/99").
     IF Estaciones.Fec_UltSalida NE ? THEN
         FecUSa = STRING(Estaciones.Fec_UltSalida,"99/99/99").
     IF Estaciones.Fec_Creacion NE ? THEN
         FecCre = STRING(Estaciones.Fec_Creacion,"99/99/99").
     IF Estaciones.Hora_UltEntrada NE ? THEN
       ASSIGN HoraE = STRING(Estaciones.Hora_UltEntrada,"hh:mm am").
     IF Estaciones.Hora_UltSalida NE ? THEN
       ASSIGN HoraS = STRING(Estaciones.Hora_UltSalida,"hh:mm am").
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Estaciones.Agencia,"999")
                  + "010" + STRING(Estaciones.Estacion,"X(10)")
                  + "030" + STRING(Estaciones.Descripcion,"X(30)")
                  + "008" + STRING(FecCre,"X(8)")
                  + "008" + STRING(FecRet,"X(8)")
                  + "008" + STRING(FecUEn,"X(8)")
                  + "008" + STRING(HoraE,"X(08)")
                  + "008" + STRING(FecUSa,"X(8)")
                  + "008" + STRING(HoraS,"X(08)").
            
      {Incluido\imprimir_Excel.i}
      
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Win 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Win 
PROCEDURE ProcesoImprimir :
{Incluido\RepEncabezado.i}

  DEFINE VAR HoraE       AS CHARACTER FORMAT "X(8)".
  DEFINE VAR HoraS       AS CHARACTER FORMAT "X(8)".

  W_Reporte    = "REPORTE   : ESTACIONES DE TRABAJO - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
  W_EncColumna = " AGE. COD.ESTACION  DESCRIPCIÒN ESTACIÓN       FEC.CREA FEC.RET. ULT.ENT. HR.ENTRA ULT.SAL. HR.SALE".

  DEFINE FRAME F-Esta
    Estaciones.Agencia          AT 2   VIEW-AS TEXT
    Estaciones.Estacion           AT 7
    Estaciones.Descripcion      AT 21 FORMAT "X(25)"
    Estaciones.Fec_Creacion     AT 48 FORMAT "99/99/99"
    Estaciones.Fec_Retiro       AT 57 FORMAT "99/99/99"
    Estaciones.Fec_UltEntrada   AT 66 FORMAT "99/99/99"
    HoraE                       AT 75
    Estaciones.Fec_UltSalida    AT 84 FORMAT "99/99/99"
    HoraS                       AT 95
    SKIP
  WITH DOWN WIDTH 132.
     
  DEFINE FRAME F-Usuarios WITH USE-TEXT FRAME F-Usuarios.

    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.
    FOR EACH Estaciones BY Estaciones.Agencia:
      ASSIGN HoraE = ""
             HoraS  = "".
      IF Estaciones.Hora_UltEntrada NE ? THEN
        ASSIGN HoraE = STRING(Estaciones.Hora_UltEntrada,"hh:mm am").
      IF Estaciones.Hora_UltSalida NE ? THEN
        ASSIGN HoraS = STRING(Estaciones.Hora_UltSalida,"hh:mm am").

      DISPLAY
        Estaciones.Agencia   
        Estaciones.Estacion    
        Estaciones.Descripcion 
        Estaciones.Fec_Creacion
        Estaciones.Fec_Retiro  
        Estaciones.Fec_UltEntrada
        HoraE                    
        Estaciones.Fec_UltSalida 
        HoraS                    
        SKIP
       WITH WIDTH 132 FRAME F-Esta USE-TEXT STREAM-IO NO-LABELS.
    END.
    PAGE.
    OUTPUT CLOSE.   
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Win  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Win 
PROCEDURE state-changed :
/* -----------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
-------------------------------------------------------------*/
  DEFINE INPUT PARAMETER p-issuer-hdl AS HANDLE NO-UNDO.
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

