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
  
  DEFINE VAR W_Ventana AS CHARACTER.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F-Consulta

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS Btn_Terminar 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR W-ProCre AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_b-cf_liqinteres AS HANDLE NO-UNDO.
DEFINE VARIABLE h_P-UPDSAV AS HANDLE NO-UNDO.
DEFINE VARIABLE h_v-cf_liqinteres AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Terminar 
     IMAGE-UP FILE "imagenes\volver":U
     LABEL "&Terminar Consulta" 
     SIZE 7 BY 1.62 TOOLTIP "Cierra esta ventana y vuelve a la pantalla anterior".

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
     LABEL "I&mprimir" 
     SIZE 10 BY 1.62 TOOLTIP "Muestra la interface de salida de información (Reportes)"
     FONT 15.

DEFINE BUTTON BUTTON-59 
     IMAGE-UP FILE "imagenes\informacion":U
     LABEL "Button 59" 
     SIZE 10 BY 1.65 TOOLTIP "Muestra la pantalla de información de sesión y los mensajes del usuario activo".

DEFINE RECTANGLE RECT-232
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 5.38.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F-Main
     BUTTON-59 AT ROW 1.81 COL 102
     Btn_Impresion AT ROW 3.42 COL 102 HELP
          "Permite generar la impresión"
     Btn_Consulta AT ROW 5.04 COL 102 HELP
          "Permite generar la consulta"
     Btn_Ayuda AT ROW 20.65 COL 104 HELP
          "Permite generar la ayuda de la pantalla"
     RECT-232 AT ROW 1.54 COL 101
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 113.72 BY 21.19
         BGCOLOR 17 .

DEFINE FRAME F-Consulta
     Btn_Terminar AT ROW 12.58 COL 32 HELP
          "Termina la consulta"
     "Nota:" VIEW-AS TEXT
          SIZE 6 BY .81 AT ROW 12.31 COL 2
          FGCOLOR 7 
     "Escoja el filtro por el cual desea" VIEW-AS TEXT
          SIZE 22 BY .81 AT ROW 12.31 COL 7
          FONT 4
     "realizar la busqueda y digite la" VIEW-AS TEXT
          SIZE 22 BY .62 AT ROW 13.12 COL 7
          FONT 4
     "la palabra clave y presione ENTER" VIEW-AS TEXT
          SIZE 24 BY .62 AT ROW 13.65 COL 7
          FONT 4
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 58 ROW 5.62
         SIZE 39.86 BY 14.35
         BGCOLOR 17 FGCOLOR 0 FONT 5
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
         TITLE              = "SFG - Configuracion Contable de la Liquidación de Intereses"
         HEIGHT             = 21.19
         WIDTH              = 113.72
         MAX-HEIGHT         = 21.19
         MAX-WIDTH          = 113.72
         VIRTUAL-HEIGHT     = 21.19
         VIRTUAL-WIDTH      = 113.72
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
/* SETTINGS FOR FRAME F-Consulta
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F-Consulta:HIDDEN           = TRUE
       FRAME F-Consulta:MOVABLE          = TRUE.

/* SETTINGS FOR FRAME F-Main
                                                                        */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(W-ProCre)
THEN W-ProCre:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME W-ProCre
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON END-ERROR OF W-ProCre /* SFG - Configuracion Contable de la Liquidación de Intereses */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W-ProCre W-ProCre
ON WINDOW-CLOSE OF W-ProCre /* SFG - Configuracion Contable de la Liquidación de Intereses */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda W-ProCre
ON CHOOSE OF Btn_Ayuda IN FRAME F-Main
OR HELP OF {&WINDOW-NAME}
DO:
    SYSTEM-HELP "AYUDAS\REDECOOP" CONTEXT 8.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta W-ProCre
ON CHOOSE OF Btn_Consulta IN FRAME F-Main /* Consulta */
DO:
    RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE,
                                           INPUT 'Cons':U,
                                           OUTPUT W_Ventana).
    RUN Activar IN WIDGET-HANDLE(W_Ventana) (INPUT TRUE).
    DISABLE ALL EXCEPT Btn_Ayuda WITH FRAME {&FRAME-NAME}.
    VIEW FRAME F-Consulta.
    ENABLE ALL WITH FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Impresion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Impresion W-ProCre
ON CHOOSE OF Btn_Impresion IN FRAME F-Main /* Imprimir */
DO:
    DEFINE VAR Listado AS CHARACTER INITIAL "".
    Listado = W_PathSpl + "L-LIQINT.LST".
    {incluido\imprimir.i "Listado"}
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
&Scoped-define SELF-NAME Btn_Terminar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Terminar W-ProCre
ON CHOOSE OF Btn_Terminar IN FRAME F-Consulta /* Terminar Consulta */
DO:
    RUN get-link-handle IN adm-broker-hdl (INPUT THIS-PROCEDURE,
                                           INPUT 'Cons':U,
                                         OUTPUT W_Ventana).
    RUN Activar IN WIDGET-HANDLE(W_Ventana) (INPUT FALSE).
    ENABLE ALL EXCEPT Btn_Ayuda WITH FRAME F-Main.
    HIDE FRAME F-Consulta.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Main
&Scoped-define SELF-NAME BUTTON-59
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-59 W-ProCre
ON CHOOSE OF BUTTON-59 IN FRAME F-Main /* Button 59 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F-Consulta
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
             INPUT  'v-cf_liqinteres.w':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_v-cf_liqinteres ).
       RUN set-position IN h_v-cf_liqinteres ( 1.54 , 3.00 ) NO-ERROR.
       /* Size in UIB:  ( 18.85 , 94.00 ) */

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'b-cf_liqinteres.w':U ,
             INPUT  FRAME F-Consulta:HANDLE ,
             INPUT  'Layout = ':U ,
             OUTPUT h_b-cf_liqinteres ).
       RUN set-position IN h_b-cf_liqinteres ( 1.00 , 1.00 ) NO-ERROR.
       RUN set-size IN h_b-cf_liqinteres ( 11.04 , 39.00 ) NO-ERROR.

       RUN init-object IN THIS-PROCEDURE (
             INPUT  'P-UPDSAV.W':U ,
             INPUT  FRAME F-Main:HANDLE ,
             INPUT  'Edge-Pixels = 2,
                     SmartPanelType = Save,
                     AddFunction = One-Record':U ,
             OUTPUT h_P-UPDSAV ).
       RUN set-position IN h_P-UPDSAV ( 9.62 , 101.00 ) NO-ERROR.
       RUN set-size IN h_P-UPDSAV ( 10.77 , 12.00 ) NO-ERROR.

       /* Links to SmartViewer h_v-cf_liqinteres. */
       RUN add-link IN adm-broker-hdl ( h_b-cf_liqinteres , 'Record':U , h_v-cf_liqinteres ).
       RUN add-link IN adm-broker-hdl ( h_P-UPDSAV , 'TableIO':U , h_v-cf_liqinteres ).
       RUN add-link IN adm-broker-hdl ( h_v-cf_liqinteres , 'Cons':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjust-tab-order IN adm-broker-hdl ( h_v-cf_liqinteres ,
             BUTTON-59:HANDLE IN FRAME F-Main , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_b-cf_liqinteres ,
             Btn_Terminar:HANDLE IN FRAME F-Consulta , 'BEFORE':U ).
       RUN adjust-tab-order IN adm-broker-hdl ( h_P-UPDSAV ,
             Btn_Consulta:HANDLE IN FRAME F-Main , 'AFTER':U ).
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
  ENABLE BUTTON-59 Btn_Impresion Btn_Consulta Btn_Ayuda RECT-232 
      WITH FRAME F-Main IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-F-Main}
  ENABLE Btn_Terminar 
      WITH FRAME F-Consulta IN WINDOW W-ProCre.
  {&OPEN-BROWSERS-IN-QUERY-F-Consulta}
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
 E_NumColumn = 4.
 E_Fila      =      "003" + "Cla"
                  + "012" + "Cod_Producto"
                  + "030" + "Nombre Producto               "
                  + "014" + "CtaDb_LiqAso  "
                  + "014" + "CtaCr_LiqAso  "
                  + "014" + "CtaDb_Liq     "
                  + "014" + "CtaCr_Liq     "
                  + "014" + "CtaDb_Ret     "
                  + "014" + "CtaCr_Ret     "
                  + "014" + "CtaDb_MoraAso "
                  + "014" + "CtaCr_MoraAso "
                  + "014" + "CtaDb_Mora    "
                  + "014" + "CtaCr_Mora    "
                  + "014" + "CtaInt_AntAso "
                  + "014" + "CtaInt_Ant    "
                  + "014" + "Cdba"
                  + "014" + "Base          ".
 RUN W-GraExcel.w (INPUT E_Fila, INPUT E_NumColumn, OUTPUT E_CmpGrafic).

/* launch Excel so it is visible to the user */
chExcelApp:Visible = TRUE.

/* create a new Workbook */
chWorkbook = chExcelApp:Workbooks:Add().

/* get the active Worksheet */
chWorkSheet = chExcelApp:Sheets:Item(1).

FOR EACH liqui_Int NO-LOCK BREAK BY Liqui_Int.Clase_Producto:
      IF Liqui_Int.Clase_Producto EQ 1 THEN DO:
         FIND FIRST Pro_Ahorros WHERE pro_ahorros.cod_ahorro EQ Liqui_Int.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Ahorros) THEN DO:
            ASSIGN W_NomPcto = Pro_Ahorros.Nom_Producto.
         END.
      END.
      ELSE DO:
         FIND Pro_Creditos WHERE pro_Creditos.Cod_Credito EQ Liqui_Int.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Creditos) THEN
            ASSIGN W_NomPcto = Pro_Creditos.Nom_Producto.
      END.
      E_Fila2     = "".
      E_Fila2     = "003" + STRING(Liqui_Int.Clase_Producto,"999")
                  + "012" + STRING(Liqui_Int.Cod_Producto,"X(12)")
                  + "030" + STRING(W_NomPcto,"X(30)")
                  + "014" + STRING(Liqui_Int.CtaDb_LiqAso,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaCr_LiqAso,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaDb_Liq,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaCr_Liq,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaDb_Ret,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaCr_Ret,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaDb_MoraAso,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaCr_MoraAso,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaDb_Mora,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaCr_Mora,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaInt_AntAso,"X(14)")
                  + "014" + STRING(Liqui_Int.CtaInt_Ant,"X(14)")
                  + "014" + STRING(Liqui_Int.Cod_base,">>>9")
                  + "014" + STRING(Liqui_Int.Base,"X(14)").

        
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
  W_Reporte    = "REPORTE   : CONFIGURACION LIQUIDACION INTERESES - " + STRING(TODAY) + " - " + STRING(TIME,"hh:mm am").

  DEFINE VAR W_NomPcto   AS CHARACTER FORMAT "X(30)"  INITIAL "".
  DEFINE VAR W_Tipo      AS CHARACTER FORMAT "X(10)"  INITIAL "".
  DEFINE VAR W_Ret       AS DECIMAL   FORMAT "99.99%" INITIAL 0.
  DEFINE FRAME F-Encabezado
      HEADER
        W_Nom_Entidad AT 2
        "PAGINA:"     AT 90 PAGE-NUMBER FORMAT ">>>9"
        W_Ubicacion   AT 2
        W_IdEstacion  AT 2
        W_IdReporta   AT 2
        W_Reporte     AT 2
        W_Linea       AT 1 FORMAT "X(100)"
        W_EncColumna  AT 1 FORMAT "X(318)"
        W_Linea2      AT  1  FORMAT "X(318)"
    WITH DOWN WIDTH 320 USE-TEXT PAGE-TOP FRAME F-Encabezado STREAM-IO.

  W_EncColumna = "PRO  COD    NOMBRE                       LQ.DB.ASO     LQ.CR.ASO     LQ.DEB      LQ.CRE                RET.DEB     RET.CRE           LQ.SBR.DB.ASO LQ.SBR.CR.ASO  LQ.SBR.DEB  LQ.SBR.CRE        IN.ANT.ASO       IN.ANT.NOASO   COD.BASE   BASE.RET".
  VIEW FRAME F-Encabezado.
  VIEW FRAME F-Ftr.
  FOR EACH liqui_Int NO-LOCK BREAK BY Liqui_Int.Clase_Producto:
      IF Liqui_Int.Clase_Producto EQ 1 THEN DO:
         FIND FIRST Pro_Ahorros WHERE pro_ahorros.cod_ahorro EQ Liqui_Int.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Ahorros) THEN DO:
            ASSIGN W_NomPcto = Pro_Ahorros.Nom_Producto.
         END.
         DISPLAY Liqui_Int.Clase_Producto     AT 2   NO-LABEL
                 Liqui_Int.Cod_Producto       AT 6   NO-LABEL
                 W_NomPcto                    AT 10  NO-LABEL
                 Liqui_Int.CtaDb_LiqAso       AT 42  NO-LABEL
                 Liqui_Int.CtaCr_LiqAso       AT 57  NO-LABEL
                 Liqui_Int.CtaDb_Liq          AT 72  NO-LABEL
                 Liqui_Int.CtaCr_Liq          AT 87  NO-LABEL
                 Liqui_Int.CtaDb_Ret          AT 102 NO-LABEL
                 Liqui_Int.CtaCr_Ret          AT 117 NO-LABEL
                 Liqui_Int.CtaDb_MoraAso      AT 132 NO-LABEL
                 Liqui_Int.CtaCr_MoraAso      AT 147 NO-LABEL
                 Liqui_Int.CtaDb_Mora         AT 162 NO-LABEL
                 Liqui_Int.CtaCr_Mora         AT 178 NO-LABEL
                 Liqui_Int.CtaInt_AntAso      AT 194 NO-LABEL
                 Liqui_Int.CtaInt_Ant         AT 209 NO-LABEL
                 INTEGER(Liqui_Int.Cod_base)  AT 224 NO-LABEL FORMAT ">>>9"
                 Liqui_Int.Base               AT 230 NO-LABEL
         WITH FRAME F-Interes1 DOWN WIDTH 350 USE-TEXT STREAM-IO NO-BOX.
         DOWN WITH FRAME F-Interes1.
      END.
      ELSE DO:
         FIND Pro_Creditos WHERE pro_Creditos.Cod_Credito EQ Liqui_Int.Cod_Producto NO-LOCK NO-ERROR.
         IF AVAILABLE(Pro_Creditos) THEN
            ASSIGN W_NomPcto = Pro_Creditos.Nom_Producto.
         W_EncColumna = "PRO  COD    NOMBRE                       LQ.DB.ASO     LQ.CR.ASO     LQ.DEB      LQ.CRE              LQ.MOR.DB.ASO LQ.MOR.CR.ASO LQ.MOR.DEB   LQ.MOR.ASO         DF.COB.DB.ASO DF.COB.CR.ASO  DF.COB.DEB  DF.COB.CRE      INT.ANT.ASOCIADO INT.ANT.NO.ASOCIADO".
         VIEW FRAME F-Encabezado.
         VIEW FRAME F-Ftr.
         DISPLAY Liqui_Int.Clase_Producto  AT 2   NO-LABEL
                 Liqui_Int.Cod_Producto    AT 6   NO-LABEL
                 W_NomPcto                 AT 10  NO-LABEL
                 Liqui_Int.CtaDb_LiqAso    AT 42  NO-LABEL
                 Liqui_Int.CtaCr_LiqAso    AT 57  NO-LABEL
                 Liqui_Int.CtaDb_Liq       AT 72  NO-LABEL
                 Liqui_Int.CtaCr_Liq       AT 87  NO-LABEL
                 Liqui_Int.CtaDb_MoraAso   AT 102 NO-LABEL
                 Liqui_Int.CtaCr_MoraAso   AT 117 NO-LABEL
                 Liqui_Int.CtaDb_Mora      AT 132 NO-LABEL
                 Liqui_Int.CtaCr_Mora      AT 147 NO-LABEL
                 Liqui_Int.CtaDb_DifCobAso AT 162 NO-LABEL
                 Liqui_Int.CtaCr_DifCobAso AT 178 NO-LABEL
                 Liqui_Int.CtaDb_DifCob    AT 194 NO-LABEL
                 Liqui_Int.CtaCr_DifCob    AT 209 NO-LABEL
                 Liqui_Int.CtaInt_AntAso   AT 224 NO-LABEL
                 Liqui_Int.CtaInt_Ant      AT 239 NO-LABEL     
         WITH FRAME F-Interes2 DOWN WIDTH 350 USE-TEXT STREAM-IO NO-BOX.
         DOWN WITH FRAME F-Interes2.
      END.
      IF LAST-OF(Liqui_Int.Clase_Producto) THEN DO:
         PAGE.
      END.   
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
  DEFINE INPUT PARAMETER p-state AS CHARACTER NO-UNDO.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

