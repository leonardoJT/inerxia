&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI ADM1
&ANALYZE-RESUME
/* Connected Databases 
*/
&Scoped-define WINDOW-NAME W-Usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS W-Usuario 
CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
DEFINE SHARED VAR W_Agencia AS INTEGER FORMAT "999".
DEFINE SHARED VAR W_Nom_Agencia AS CHARACTER FORMAT "X(60)".
DEFINE SHARED VAR W_Nom_Entidad AS CHARACTER FORMAT "X(60)".
DEFINE SHARED VAR W_Usuario AS CHARACTER.
DEFINE SHARED VAR W_Estacion AS CHARACTER.
DEFINE SHARED VAR W_Manija AS HANDLE.
DEFINE SHARED VAR W_PathSpl AS CHARACTER.
DEFINE VARIABLE W_Estado AS CHARACTER FORMAT "X(8)" INITIAL "".
DEFINE VARIABLE W_Rowid AS ROWID.
DEFINE VARIABLE W-Procedure AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F-Impresion

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_Tconsulta Btn_Aceptar Btn_Cancelar 
&Scoped-Define DISPLAYED-OBJECTS W_Tconsulta W_CCampos W_RanIni W_RanFin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-Usuario AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_q-cf_usuario AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_usuario AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Aceptar 
     IMAGE-UP FILE "imagenes\impresora2":U
     LABEL "Button 45" 
     SIZE 9 BY 1.65.

DEFINE BUTTON Btn_Cancelar 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 9 BY 1.88.

DEFINE VARIABLE W_CCampos AS CHARACTER FORMAT "X(30)":U INITIAL "Agencia" 
     LABEL "Campo" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Agencias","Grupos" 
     DROP-DOWN-LIST
     SIZE 24 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_RanFin AS INTEGER FORMAT ">>>9":U INITIAL 9999 
     LABEL "Final" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_RanIni AS INTEGER FORMAT ">>>9":U INITIAL 0 
     LABEL "Inicial" 
     VIEW-AS FILL-IN 
     SIZE 8.86 BY .81
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Tconsulta AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Todos los Usuarios", 1,
"Filtrar", 2
     SIZE 26 BY .85 NO-UNDO.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes\lupa":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "C&onsultar" 
     SIZE 10 BY 1.62 TOOLTIP "Busqueda de información  de la pantalla en uso"
     FONT 4.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes\impresora2":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "I&mprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra interface de salida de información (Reportes)"
     BGCOLOR 11 FONT 15.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes\informacion":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra la pantalla de Información de Sesión  y los mensajes del usuario activo"
     FONT 4.

DEFINE RECTANGLE RECT-224
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-1 AT ROW 1.81 COL 75
     Btn_Imprimir AT ROW 3.42 COL 75 HELP
          "Permite Generar la Impresion de Usuarios"
     Btn_Consulta AT ROW 5.04 COL 75 HELP
          "Permite Generar la Consulta de Usuarios"
     RECT-224 AT ROW 1.54 COL 74
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.29 BY 21.35
         BGCOLOR 17 .

DEFINE FRAME F-Impresion
     W_Tconsulta AT ROW 1.27 COL 3 HELP
          "Seleccione el Tipo de Consulta" NO-LABEL
     Btn_Aceptar AT ROW 1.27 COL 37
     W_CCampos AT ROW 2.35 COL 10 COLON-ALIGNED HELP
          "Seleccione el Campo para generar la consulta"
     Btn_Cancelar AT ROW 2.88 COL 37 HELP
          "Oprima el BOTON  para Terminar la consulta"
     W_RanIni AT ROW 3.69 COL 10 COLON-ALIGNED HELP
          "Ingrese el Valor Inicial"
     W_RanFin AT ROW 3.69 COL 25 COLON-ALIGNED HELP
          "Ingrese el Valor Final"
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 6.43 ROW 2.81
         SIZE 46.72 BY 4.73
         BGCOLOR 17 FONT 4
         TITLE "Informe".


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
  CREATE WINDOW W-Usuario ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Configuración de Usuarios del Sistema"
         HEIGHT             = 16.35
         WIDTH              = 86.57
         MAX-HEIGHT         = 23.08
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 23.08
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB W-Usuario 
/* ************************* Included-Libraries *********************** */

{src/adm/method/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW W-Usuario
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F-Impresion
   NOT-VISIBLE FRAME-NAME                                               */
ASSIGN 
       FRAME F-Impresion:HIDDEN           = TRUE
       FRAME F-Impresion:MOVABLE          = TRUE.

/* SETTINGS FOR COMBO-BOX W_CCampos IN FRAME F-Impresion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_RanFin IN FRAME F-Impresion
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_RanIni IN FRAME F-Impresion
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Usuario)
THEN W-Usuario:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-Usuario
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Usuario W-Usuario
ON END-ERROR OF W-Usuario /* SFG - Configuración de Usuarios del Sistema */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-Usuario W-Usuario
ON WINDOW-CLOSE OF W-Usuario /* SFG - Configuración de Usuarios del Sistema */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Aceptar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Aceptar W-Usuario
ON CHOOSE OF Btn_Aceptar IN FRAME F-Impresion /* Button 45 */
DO:
  DEFINE VAR Listado AS CHARACTER INITIAL "".
  DEFINE VAR Tamano  AS INTEGER   INITIAL 2.
  
  listado = W_PathSpl + "L_Usuar.Lst".
  {Incluido\Imprimir.i "Listado" Tamano}
  /*{Imprimir.I "listado"}*/  

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar W-Usuario
ON CHOOSE OF Btn_Cancelar IN FRAME F-Impresion /* Terminar Consulta */
DO:
  HIDE FRAME F-Impresion.
  ENABLE ALL WITH FRAME F-Main.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-Usuario
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consultar */
DO:
  ASSIGN W_Rowid = ?.

  RUN C-Usuarios.r (INPUT-OUTPUT W_Rowid).
  IF W_Rowid EQ ? THEN
     RETURN NO-APPLY.     

  RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE, INPUT "Query", OUTPUT W-Procedure).
  RUN Reposiciona-query IN WIDGET-HANDLE(W-Procedure) (INPUT-OUTPUT W_Rowid).
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir W-Usuario
ON CHOOSE OF Btn_Imprimir IN FRAME F-Main /* Imprimir */
DO:
  DISABLE ALL WITH FRAME F-Main.
  VIEW FRAME F-Impresion.     
  ENABLE Btn_Aceptar Btn_Cancelar W_Tconsulta WITH FRAME F-Impresion.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 W-Usuario
ON CHOOSE OF BUTTON-1 IN FRAME F-Main /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Impresion
&Scoped-define SELF-NAME W_CCampos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CCampos W-Usuario
ON ENTRY OF W_CCampos IN FRAME F-Impresion /* Campo */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CCampos W-Usuario
ON VALUE-CHANGED OF W_CCampos IN FRAME F-Impresion /* Campo */
DO:
  ASSIGN W_Ccampos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_RanFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_RanFin W-Usuario
ON ENTRY OF W_RanFin IN FRAME F-Impresion /* Final */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_RanFin W-Usuario
ON LEAVE OF W_RanFin IN FRAME F-Impresion /* Final */
DO:
  ASSIGN W_RanFin.
  IF W_RanFin < W_RanIni THEN
     ASSIGN W_RanFin = W_RanIni
            W_RanFin:SCREEN-VALUE = STRING(W_RanIni,"9999").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_RanIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_RanIni W-Usuario
ON ENTRY OF W_RanIni IN FRAME F-Impresion /* Inicial */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_RanIni W-Usuario
ON LEAVE OF W_RanIni IN FRAME F-Impresion /* Inicial */
DO:
  ASSIGN W_RanIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Tconsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Tconsulta W-Usuario
ON LEAVE OF W_Tconsulta IN FRAME F-Impresion
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Tconsulta W-Usuario
ON VALUE-CHANGED OF W_Tconsulta IN FRAME F-Impresion
DO:
  ASSIGN W_Tconsulta.
  DO WITH FRAME F-Impresion:
     ASSIGN W_RanIni:SCREEN-VALUE = "0"
            W_RanFin:SCREEN-VALUE = "9999"
            W_Ccampos:SCREEN-VALUE = "Agencias".
     CASE W_Tconsulta:
       WHEN 1 THEN
         DISABLE W_Ccampos W_RanIni W_RanFin.
       WHEN 2 THEN
        DO:
          ENABLE W_Ccampos W_RanIni W_RanFin WITH FRAME F-Impresion.
          ASSIGN W_RanIni  = 0
                 W_RanFin  = 9999
                 W_CCampos = "Agencias".
          APPLY "ENTRY" TO W_CCampos.
          RETURN NO-APPLY.
        END.
     END CASE.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK W-Usuario 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm/template/windowmn.i}
    SUBSCRIBE TO "v-cf_usuarioCntrol" IN h_v-cf_usuario.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects W-Usuario  _ADM-CREATE-OBJECTS
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
             INPUT  'v-cf_usuario.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_usuario ).
       RUN set-position IN h_v-cf_usuario ( 1.00 , 1.00 ) NO-ERROR.
       /* Size in UIB:  ( 14.04 , 72.14 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 7.27 , 74.14 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 9.96 , 12.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'q-cf_usuario.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  '':U ,
             OUTPUT h_q-cf_usuario ).
       RUN set-position IN h_q-cf_usuario ( 4.77 , 63.00 ) NO-ERROR.
       /* Size in UIB:  ( 1.50 , 7.72 ) */

       /* Links to SmartViewer h_v-cf_usuario. */
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_usuario ).
       RUN add-link IN adm-broker-hdl ( h_q-cf_usuario , 'Record':U , h_v-cf_usuario ).

       /* Links to SmartQuery h_q-cf_usuario. */
       RUN add-link IN adm-broker-hdl ( h_q-cf_usuario , 'Query':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_usuario ,
             BUTTON-1:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
    END. /* Page 0 */

  END CASE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-row-available W-Usuario  _ADM-ROW-AVAILABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI W-Usuario  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-Usuario)
  THEN DELETE WIDGET W-Usuario.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI W-Usuario  _DEFAULT-ENABLE
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
  ENABLE RECT-224 BUTTON-1 Btn_Imprimir Btn_Consulta 
      WITH FRAME F-Main IN WINDOW W-Usuario.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  DISPLAY W_Tconsulta W_CCampos W_RanIni W_RanFin 
      WITH FRAME F-Impresion IN WINDOW W-Usuario.
  ENABLE W_Tconsulta Btn_Aceptar Btn_Cancelar 
      WITH FRAME F-Impresion IN WINDOW W-Usuario.
  {&OPEN-BROWSERS-IN-QUERY-F-Impresion}
  VIEW W-Usuario.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImprimirDetalle W-Usuario 
PROCEDURE ImprimirDetalle :
IF Usuarios.Estado = 1 THEN
     W_Estado = "Activo".
  ELSE
     W_Estado = "Inactivo".
     
  DISPLAY Usuarios.Agencia        AT 1  NO-LABEL 
          Usuarios.Usuario        AT 5  NO-LABEL FORMAT "X(25)"
          Usuarios.Nombre         AT 32 NO-LABEL FORMAT "X(25)"
          Usuarios.Grupo          AT 58 NO-LABEL
          W_Estado                AT 65 NO-LABEL
  WITH DOWN WIDTH 132 FRAME F-Usuarios USE-TEXT STREAM-IO NO-LABELS NO-BOX.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Excel W-Usuario 
PROCEDURE Imprimir_Excel :
{Incluido\Def_Excel.i}
 /* MANDA ENCABEZADO DE HOJA DE CALCULO*/
 
 E_NumFila = 1.
 E_NumColumn = 4.
 E_Fila      = "003Age010Usuario   025Nombre                   005Grupo".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).


 FOR EACH Usuarios NO-LOCK
                   BY Usuarios.Agencia
                   BY Usuarios.Usuario
                   BY Usuarios.Grupo:
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Usuarios.Agencia,"999")
                  + "010" + STRING(Usuarios.Usuario,"X(25)")
                  + "025" + STRING(Usuarios.Nombre,"X(25)")
                  + "005" + STRING(Usuarios.Grupo,"99999").
      {Incluido\imprimir_Excel.i}
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-exit W-Usuario 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE local-initialize W-Usuario 
PROCEDURE local-initialize :
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
  SUBSCRIBE TO "v-cf_usuarioCntrol" IN h_v-cf_usuario.

  RUN dispatch IN THIS-PROCEDURE ( INPUT 'initialize':U ) .
    
  /* Code placed here will execute AFTER standard behavior.    */
    /*RUN adm-disable IN h_P-UPDSAV.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Pdc_Excel W-Usuario 
PROCEDURE Pdc_Excel :
DEFINE VARIABLE E_TipoLinea AS INTEGER   FORMAT 9 INITIAL 1. /* 1-ENCABEZADO 2-REGISTROS*/ 
 DEFINE VARIABLE E_IdGrafico AS LOGICAL.                      /* Yes - Graficar informacion
                                                                No  - No Graficar*/
 DEFINE VARIABLE E_NumColumn AS INTEGER   FORMAT 99.          /* Numero de Columnas que se mandaran*/
 DEFINE VARIABLE W_NumFila   AS INTEGER   FORMAT 9999.        /* Numero de la Fila que se manda*/
 DEFINE VARIABLE E_Fila      AS CHARACTER FORMAT "X(200)".    /* Informacion del informe a tabular*/


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir W-Usuario 
PROCEDURE ProcesoImprimir :
{INCLUIDO\RepEncabezado.I}    
    W_Reporte    = "REPORTE   : USUARIOS DEL SISTEMA - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna =     "AGE. USUARIO    NOMBRE                  GRUPO    ESTADO  ".
          
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  IF W_Tconsulta = 1 THEN
     FOR EACH Usuarios NO-LOCK BY Usuarios.Agencia
                               BY Usuarios.Usuario
                               BY Usuarios.Grupo:
         RUN ImprimirDetalle.
     END.  
  ELSE
    CASE W_Ccampos:
      WHEN "Agencias" THEN
        FOR EACH Usuarios NO-LOCK WHERE Usuarios.Agencia >= W_RanIni AND
                                        Usuarios.Agencia <= W_RanFin
                          BY Usuarios.Agencia
                          BY Usuarios.Usuario
                          BY Usuarios.Grupo:

            RUN ImprimirDetalle.
        END.
      WHEN "Grupos" THEN
        FOR EACH Usuarios NO-LOCK WHERE Usuarios.Grupo >= W_RanIni AND
                                        Usuarios.Grupo <= W_RanFin
                          BY Usuarios.Grupo:
            RUN ImprimirDetalle.
        END.
    END CASE.
  PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE send-records W-Usuario  _ADM-SEND-RECORDS
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE state-changed W-Usuario 
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE v-cf_usuarioCntrol W-Usuario 
PROCEDURE v-cf_usuarioCntrol :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER c AS CHAR NO-UNDO.
    c = c + CHR(1).
    /*IF trim(ENTRY(1,c,CHR(1))) = trim(ENTRY(2,c,CHR(1)))
    THEN RUN adm-disable IN h_P-UPDSAV.
    ELSE RUN adm-enable IN h_P-UPDSAV.*/
    RUN adm-enable IN h_P-UPDSAV.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

