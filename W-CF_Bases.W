&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Grupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Grupo 
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

  DEFINE VARIABLE W_Rowid              AS ROWID.
  DEFINE VARIABLE W-Procedure          AS CHARACTER.

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
&Scoped-Define ENABLED-OBJECTS BUTTON-2 Btn_Impresion Btn_Consulta ~
Btn_Ayuda RECT-228 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Grupo AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-cf_bases AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_bases AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "imagenes\interrogacion":U
     LABEL "" 
     SIZE 4 BY 1.08 TOOLTIP "Ayuda"
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes\lupa":U
     LABEL "Co&nsultar" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Impresion 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "I&mprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra interface de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-2 
     IMAGE-UP FILE "imagenes\informacion":U
     LABEL "Button 2" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra Información de Sesión y mensajes del usuario activo".

DEFINE RECTANGLE RECT-228
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-2 AT ROW 1.54 COL 102
     Btn_Impresion AT ROW 3.15 COL 102 HELP
          "Permite Generar la Impresion de Grupo"
     Btn_Consulta AT ROW 4.77 COL 102 HELP
          "Permite Generar la Consulta de Grupo"
     Btn_Ayuda AT ROW 19.58 COL 105 HELP
          "Permite Obtener la ayuda de esta Pantalla"
     RECT-228 AT ROW 1.27 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.86 BY 21
         BGCOLOR 17 FONT 4.


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
  CREATE WINDOW W-Grupo ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Creación y Modificación de Bases de Retención del Sistema"
         HEIGHT             = 20.92
         WIDTH              = 113.43
         MAX-HEIGHT         = 21
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 21
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Grupo 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Grupo
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Grupo)
THEN W-Grupo:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Grupo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Grupo W-Grupo
ON END-ERROR OF W-Grupo /* SFG - Creación y Modificación de Bases de Retención del Sistema */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Grupo W-Grupo
ON WINDOW-CLOSE OF W-Grupo /* SFG - Creación y Modificación de Bases de Retención del Sistema */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-Grupo
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
  SYSTEM-HELP "AYUDAS\CONFIGUR" CONTEXT 5.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-Grupo
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consultar */
DO:
ASSIGN W_Rowid = ?.
  RUN C-ConBases.r (INPUT-OUTPUT W_Rowid).
  IF W_Rowid EQ ? THEN
     RETURN NO-APPLY.     

  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT "Query", OUTPUT W-Procedure).
  RUN Reposiciona-query IN WIDGET-HANDLE(W-Procedure) (INPUT-OUTPUT W_Rowid). 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-Grupo
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  
  listado = W_PathSpl + "L_BaseRet.Lst".
  {Incluido\Imprimir.I "listado"}     
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 W-Grupo
ON CHOOSE OF BUTTON-2 IN FRAME F-Main /* Button 2 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Grupo 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Grupo  _ADM-CREATE-OBJECTS
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
             INPUT  'v-cf_bases.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_bases ).
       RUN set-position IN h_v-cf_bases ( 6.92 , 16.00 ) NO-ERROR.
       /* Size in UIB:  ( 5.92 , 68.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 9.08 , 101.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 9.96 , 12.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'q-cf_bases.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-cf_bases ).
       RUN set-position IN h_q-cf_bases ( 3.42 , 9.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.50 , 7.72 ) */

       /* Links to SmartViewer h_v-cf_bases. */
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_bases ).
       RUN add-link IN adm-broker-hdl ( h_q-cf_bases , 'Record':U , h_v-cf_bases ).

       /* Links to SmartQuery h_q-cf_bases. */
       RUN add-link IN adm-broker-hdl ( h_q-cf_bases , 'Query':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_bases ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             h_v-cf_bases , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Grupo  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Grupo  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Grupo)
  THEN DELETE WIDGET W-Grupo.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Grupo  _DEFAULT-ENABLE
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
  ENABLE BUTTON-2 Btn_Impresion Btn_Consulta Btn_Ayuda RECT-228 
      WITH FRAME F-Main IN WINDOW W-Grupo.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  VIEW W-Grupo.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-Grupo 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 3.
 E_Fila      = "003Gru030Nombre                        003Pri".
 
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

 FOR EACH Grupos NO-LOCK:
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Grupos.Grupo,"999")
                  + "030" + STRING(Grupos.Nombre,"X(30)")
                  + "003" + STRING(Grupos.Prioridad,"999").
      {Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Grupo 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Grupo 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
   
    DEFINE VAR Des1 AS CHARACTER FORMAT "X(50)".
    DEFINE VAR Des2 AS CHARACTER FORMAT "X(50)".
    
    W_Reporte    = "REPORTE   : PERFILES BASES DE RETENCIÓN - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
/*                  1234567890123456789012345678901234567890123456789012345678901234567890*/
    W_EncColumna = " CODIGO   NOMBRE                        PORCENTAJE".
  DEFI VAR W_Pri AS INTEGER FORMAT 9.        
  DEFINE FRAME F-BaseRet
    Base_Ret.Cod_Base   AT 3 
    Base_Ret.Nombre   AT 11
    Base_Ret.Porcentaje             AT 43
    WITH DOWN WIDTH 90.
     
    
    VIEW FRAME F-Encabezado.
    VIEW FRAME f-ftr.
    FOR EACH Base_Ret NO-LOCK: 
      DISPLAY Base_Ret.Cod_Base
              Base_Ret.Nombre  FORMAT "X(30)"
              Base_Ret.Porcentaje
      WITH WIDTH 132 FRAME F-Base_Ret USE-TEXT STREAM-IO NO-LABELS.
    END.  
    PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Grupo  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Grupo 
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

