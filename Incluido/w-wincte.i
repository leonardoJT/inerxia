&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12 GUI
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 

CREATE WIDGET-POOL.

 DEFINE VAR W_TipProg   AS   INTEGER FORMAT "9".
 DEFINE VAR P_Clasif    AS   INTEGER FORMAT "99".
 DEFINE VAR P_TipoCli   AS   INTEGER FORMAT "99".
 DEFINE VAR P_OfiIni    LIKE Oficinas.Oficina.
 DEFINE VAR P_OfiFin    LIKE Oficinas.Oficina.
 DEFINE VAR P_FecIni    AS   DATE    FORMAT "99/99/9999".
 DEFINE VAR P_FecFin    AS   DATE    FORMAT "99/99/9999".
 DEFINE VAR p_EmpIni    LIKE Empresas.Cod_Empresa.
 DEFINE VAR P_EmpFin    LIKE Empresas.Cod_Empresa.
 DEFINE VAR p_Para1ini  AS   INTEGER FORMAT "99999".
 DEFINE VAR p_Para1Fin  AS   INTEGER FORMAT "99999".
 DEFINE VAR W_Para2ini  AS   INTEGER FORMAT "99999".
 DEFINE VAR W_Para2Fin  AS   INTEGER FORMAT "99999".
 DEFINE VAR P_CauIni    LIKE Varios.Codigo.
 DEFINE VAR P_CauFin    LIKE Varios.Codigo.
 DEFINE VAR Op_salida   AS   CHARACTER.

 
 {incluido\variable.i "shared"}
 DEFINE VAR W_Rpta      AS   LOGICAL.
 DEFINE VAR W_HayError  AS   LOGICAL.
 DEFINE VAR W_OfStr     AS   CHARACTER FORMAT "X(30)".
 DEFINE VAR W_Clasif    AS   INTEGER FORMAT "99".
 DEFINE VAR W_TipoCli   AS   INTEGER FORMAT "99".
 DEFINE VAR Procname    AS   CHARACTER.
 DEFINE VAR W_Metodo    AS   LOGICAL.
 DEFINE VAR Op_salida1  AS   CHARACTER.

 DEFINE VAR W_OfiIni    LIKE Oficinas.Oficina.
 DEFINE VAR W_OfiFin    LIKE Oficinas.Oficina.
 DEFINE VAR W_OfiUltima LIKE Oficinas.Oficina. 
 DEFINE VAR W_CauIni    LIKE Varios.Codigo.
 DEFINE VAR W_CauFin    LIKE Varios.Codigo.
 DEFINE VAR W_ZonaIni   LIKE Zonas.Cod_zona.
 DEFINE VAR W_ZonaFin   LIKE Zonas.Cod_zona.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME DEFAULT-FRAME

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_CmbOfiIni W_TogEntidad W_CmbOfiFin ~
W_CmbClientes W_CmbClasifConso W_CmbZonaIni RECT-17 RECT-5 Btn_Cancelar ~
Btn_Imprimir Btn_Ayuda 
&Scoped-Define DISPLAYED-OBJECTS W_FecPan W_NomOfi W_HorPan W_TitInf ~
W_CmbOfiIni W_TogEntidad W_CmbOfiFin W_CmbClientes W_CmbClasifConso ~
W_CmbEmpIni W_EmpIni W_EmpFin W_CmbEmpFin W_FecIni W_CmbEstratoIni W_FecFin ~
W_CmbEstratofin W_CmbZonaIni W_CmbZonaFin 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 W_FecPan W_HorPan 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Ayuda 
     IMAGE-UP FILE "adeicon/cuehelp":U
     LABEL "" 
     SIZE 4 BY 1.08
     FONT 9.

DEFINE BUTTON Btn_Cancelar 
     LABEL "S&alir" 
     SIZE 11 BY 1.08.

DEFINE BUTTON Btn_Consulta 
     LABEL "Consultar" 
     SIZE 11 BY 1.12.

DEFINE BUTTON Btn_Imprimir 
     LABEL "Imprimir" 
     SIZE 11 BY 1.08
     FONT 15.

DEFINE VARIABLE W_CmbClasif AS CHARACTER FORMAT "X(256)":U INITIAL "Por Nit (Docto Identificación)" 
     LABEL "Ordenamiento" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Docto Identificación","Por Carnet","Por Apellido","Por Nombre" 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbClasifConso AS CHARACTER FORMAT "X(256)":U INITIAL "Por Nit (Docto Identificación)" 
     LABEL "Ordenamiento" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Docto Identificación","Por Apellido","Por Nombre" 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbClientes AS CHARACTER FORMAT "X(25)":U INITIAL "Todos los Clientes" 
     LABEL "Tipo de  Cliente" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "Todos los Clientes","Solo los Asociados","solo los No Asociados" 
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbEmpFin AS CHARACTER FORMAT "X(25)":U 
     LABEL "Final" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS " "
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbEmpIni AS CHARACTER FORMAT "X(25)":U 
     LABEL "Inicial" 
     VIEW-AS COMBO-BOX SORT INNER-LINES 5
     LIST-ITEMS " "
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbEstratofin AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "Estrato Final" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "1","2","3","4","5","6","7" 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbEstratoIni AS INTEGER FORMAT ">9":U INITIAL 1 
     LABEL "Estrato Inicial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "         1","         2","         3","         4","         5","         6","         7" 
     SIZE 6 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbFin AS CHARACTER FORMAT "X(25)":U 
     LABEL "Combo 2" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS " "
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbIni AS CHARACTER FORMAT "X(25)":U 
     LABEL "Combo 1" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS " "
     SIZE 21 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbOfiFin AS CHARACTER FORMAT "X(25)":U 
     LABEL "Final" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS " "
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbOfiIni AS CHARACTER FORMAT "X(25)":U 
     LABEL "Inicial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS " "
     SIZE 24 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbZonaFin AS CHARACTER FORMAT "X(30)":U 
     LABEL "Zona Final" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS " "
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE W_CmbZonaIni AS CHARACTER FORMAT "X(30)":U 
     LABEL "Zona Inicial" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS " "
     SIZE 25 BY 1 NO-UNDO.

DEFINE VARIABLE W_EmpFin AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Final" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE W_EmpIni AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Inicial" 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81 NO-UNDO.

DEFINE VARIABLE W_FecFin AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Final" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W_FecIni AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha Inicial" 
     VIEW-AS FILL-IN 
     SIZE 14 BY .81 NO-UNDO.

DEFINE VARIABLE W_FecPan AS DATE FORMAT "99/99/9999":U 
     LABEL "Fecha" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_HorPan AS CHARACTER FORMAT "X(10)":U 
     LABEL "Hora" 
     VIEW-AS FILL-IN 
     SIZE 11.86 BY .81
     BGCOLOR 3 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_NomOfi AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 56 BY 1.08
     BGCOLOR 3 FGCOLOR 15 FONT 12 NO-UNDO.

DEFINE VARIABLE W_Para1Fin AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Edad Final" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .73 NO-UNDO.

DEFINE VARIABLE W_Para1Ini AS INTEGER FORMAT ">>>>9":U INITIAL 0 
     LABEL "Edad Inicial" 
     VIEW-AS FILL-IN 
     SIZE 7 BY .81 NO-UNDO.

DEFINE VARIABLE W_TitInf AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY 1.08
     BGCOLOR 3 FGCOLOR 15 FONT 12 NO-UNDO.

DEFINE RECTANGLE RECT-17
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 13 BY 1.62.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 28 BY 1.62.

DEFINE VARIABLE W_TogEntidad AS LOGICAL INITIAL no 
     LABEL "Consolidado Entidad" 
     VIEW-AS TOGGLE-BOX
     SIZE 18 BY .77 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME DEFAULT-FRAME
     W_FecPan AT ROW 1.81 COL 70 COLON-ALIGNED
     W_NomOfi AT ROW 2.08 COL 4 COLON-ALIGNED NO-LABEL
     W_HorPan AT ROW 2.62 COL 70 COLON-ALIGNED
     W_TitInf AT ROW 3.69 COL 4 COLON-ALIGNED NO-LABEL
     W_CmbOfiIni AT ROW 6.12 COL 58 COLON-ALIGNED
     W_TogEntidad AT ROW 6.65 COL 24
     W_CmbOfiFin AT ROW 7.19 COL 58 COLON-ALIGNED
     W_CmbClientes AT ROW 9.08 COL 18 COLON-ALIGNED
     W_CmbClasif AT ROW 9.08 COL 58 COLON-ALIGNED
     W_CmbClasifConso AT ROW 9.08 COL 58 COLON-ALIGNED
     W_CmbEmpIni AT ROW 11.23 COL 18 COLON-ALIGNED
     W_EmpIni AT ROW 11.23 COL 18 COLON-ALIGNED
     W_EmpFin AT ROW 11.23 COL 58 COLON-ALIGNED
     W_CmbEmpFin AT ROW 11.23 COL 58 COLON-ALIGNED
     W_FecIni AT ROW 13.12 COL 18 COLON-ALIGNED
     W_CmbEstratoIni AT ROW 13.12 COL 18 COLON-ALIGNED
     W_FecFin AT ROW 13.12 COL 58 COLON-ALIGNED
     W_CmbEstratofin AT ROW 13.12 COL 58 COLON-ALIGNED
     W_Para1Ini AT ROW 14.19 COL 18 COLON-ALIGNED
     W_CmbZonaIni AT ROW 14.19 COL 18 COLON-ALIGNED
     W_CmbIni AT ROW 14.19 COL 18 COLON-ALIGNED
     W_Para1Fin AT ROW 14.19 COL 58 COLON-ALIGNED
     W_CmbFin AT ROW 14.19 COL 58 COLON-ALIGNED
     W_CmbZonaFin AT ROW 14.19 COL 58 COLON-ALIGNED
     Btn_Cancelar AT ROW 16.62 COL 10 HELP
          "Permite Regresar a la opción anterior"
     Btn_Consulta AT ROW 16.62 COL 60
     Btn_Imprimir AT ROW 16.62 COL 71 HELP
          "Permite Generar e Imprimir informes"
     Btn_Ayuda AT ROW 16.62 COL 82 HELP
          "Muestra la Ayuda pertinente a la pantalla"
     "Parámetros" VIEW-AS TEXT
          SIZE 41 BY .5 AT ROW 5.04 COL 5
          BGCOLOR 7 FGCOLOR 15 
     "                     Selección de Oficinas" VIEW-AS TEXT
          SIZE 40 BY .5 AT ROW 5.04 COL 46
          BGCOLOR 7 FGCOLOR 15 
     "Selección Tipo de Cliente y Ordenamiento" VIEW-AS TEXT
          SIZE 81 BY .5 AT ROW 8.27 COL 5
          BGCOLOR 7 FGCOLOR 15 
     "Selección de Empresas" VIEW-AS TEXT
          SIZE 81 BY .5 AT ROW 10.42 COL 5
          BGCOLOR 7 FGCOLOR 15 
     "Parámetros Adicionales" VIEW-AS TEXT
          SIZE 81 BY .46 AT ROW 12.31 COL 5
          BGCOLOR 7 FGCOLOR 15 
     RECT-17 AT ROW 16.35 COL 9
     RECT-5 AT ROW 16.35 COL 59
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 90.14 BY 17
         FONT 4.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Window
   Allow: Basic,Browse,DB-Fields,Window,Query
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW C-Win ASSIGN
         HIDDEN             = YES
         TITLE              = "Captura de Parámetros de Informes de Clientes"
         HEIGHT             = 16.77
         WIDTH              = 90.57
         MAX-HEIGHT         = 17.73
         MAX-WIDTH          = 91.43
         VIRTUAL-HEIGHT     = 17.73
         VIRTUAL-WIDTH      = 91.43
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME


/* ***************  Runtime Attributes and UIB Settings  ************** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME DEFAULT-FRAME
                                                                        */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX W_CmbClasif IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_CmbClasif:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX W_CmbEmpFin IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX W_CmbEmpIni IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR COMBO-BOX W_CmbEstratofin IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       W_CmbEstratofin:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX W_CmbEstratoIni IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       W_CmbEstratoIni:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX W_CmbFin IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_CmbFin:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX W_CmbIni IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_CmbIni:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR COMBO-BOX W_CmbZonaFin IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       W_CmbZonaFin:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN W_EmpFin IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       W_EmpFin:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN W_EmpIni IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
ASSIGN 
       W_EmpIni:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN W_FecFin IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecIni IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_FecPan IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN W_HorPan IN FRAME DEFAULT-FRAME
   NO-ENABLE 2                                                          */
/* SETTINGS FOR FILL-IN W_NomOfi IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Para1Fin IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_Para1Fin:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN W_Para1Ini IN FRAME DEFAULT-FRAME
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       W_Para1Ini:HIDDEN IN FRAME DEFAULT-FRAME           = TRUE.

/* SETTINGS FOR FILL-IN W_TitInf IN FRAME DEFAULT-FRAME
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 




/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Captura de Parámetros de Informes de Clientes */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Captura de Parámetros de Informes de Clientes */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Ayuda
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ayuda C-Win
ON CHOOSE OF Btn_Ayuda IN FRAME DEFAULT-FRAME
OR HELP OF {&WINDOW-NAME}
DO:
   RUN Ayuda.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME DEFAULT-FRAME /* Salir */
DO:
  &IF DEFINED (adm-panel) <> 0 &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imprimir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir C-Win
ON CHOOSE OF Btn_Imprimir IN FRAME DEFAULT-FRAME /* Imprimir */
DO:
  Op_Salida1 = "".
  RUN P-Dispos IN W_Manija (INPUT-OUTPUT procname,INPUT-OUTPUT op_salida1).
  Op_salida = Op_salida1.
  DEFINE VAR W_FecAux    AS DATE FORMAT "99/99/9999".
  DEFINE VAR W_ParaAux   AS INTEGER FORMAT ">>>>9".
  
  IF NOT W_TogEntidad THEN DO:
    IF W_OfiIni GT W_OfiFin THEN DO:
      APPLY "ENTRY" TO W_CmbOfiIni IN FRAME {&FRAME-NAME}.
      RETURN NO-APPLY.
    END.
  END.

  IF W_FecIni GT W_FecFin THEN
    ASSIGN W_FecAux   = W_FecIni
           W_FecIni   = W_FecFin
           W_FecFin   = W_FecAux.

  IF W_Para1Ini GT W_Para1Fin THEN 
    ASSIGN W_ParaAux  = W_Para1Ini
           W_Para1Ini = W_Para1Fin
           W_Para1Fin = W_ParaAux.
  IF {2} = 3 THEN DO:   /* Cuando es estrato y zona */
    ASSIGN W_Para1Ini  = W_CmbEstratoIni
           W_Para1Fin  = W_CmbEstratoFin
           W_Para2Ini  = w_ZonaIni
           W_Para2Fin  = W_ZonaFin.
  END.
  IF W_Para1Ini GT W_Para1Fin THEN 
    ASSIGN W_ParaAux  = W_Para1Ini
           W_Para1Ini = W_Para1Fin
           W_Para1Fin = W_ParaAux.
  
  IF w_Para2Ini GT w_Para2Fin THEN  /* hasta el momento solo lo utiliza Zonas */ 
    ASSIGN W_ParaAux  = w_Para2Ini
           w_Para2Ini = w_Para2Fin
           w_Para2Fin = W_ParaAux.

  IF W_EmpIni GT W_EmpFin THEN 
    ASSIGN W_ParaAux  = W_EmpIni
           W_EmpIni   = W_EmpFin
           W_EmpFin   = W_ParaAux.

  IF W_CauIni GT W_CauFin THEN
    ASSIGN W_ParaAux = W_CauIni
           W_CauIni  = W_CauFin
           W_CauFin  = W_ParaAux.

  ASSIGN P_Clasif   = W_Clasif
         P_TipoCli  = W_Tipocli.

  ASSIGN W_Metodo = SESSION:SET-WAIT-STATE ("GENERAL").
  RUN procesa.
  ASSIGN W_Metodo = SESSION:SET-WAIT-STATE ("").
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir C-Win
ON ENTRY OF Btn_Imprimir IN FRAME DEFAULT-FRAME /* Imprimir */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imprimir C-Win
ON LEAVE OF Btn_Imprimir IN FRAME DEFAULT-FRAME /* Imprimir */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbClasif
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbClasif C-Win
ON VALUE-CHANGED OF W_CmbClasif IN FRAME DEFAULT-FRAME /* Ordenamiento */
DO:
  W_Clasif = W_CmbClasif:LOOKUP(W_cmbClasif:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbClasifConso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbClasifConso C-Win
ON VALUE-CHANGED OF W_CmbClasifConso IN FRAME DEFAULT-FRAME /* Ordenamiento */
DO:
  W_Clasif = W_CmbClasifConso:LOOKUP(W_cmbClasifConso:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbClientes
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbClientes C-Win
ON VALUE-CHANGED OF W_CmbClientes IN FRAME DEFAULT-FRAME /* Tipo de  Cliente */
DO:
  W_TipoCli = W_CmbClientes:LOOKUP(W_cmbClientes:SCREEN-VALUE).
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbEmpFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEmpFin C-Win
ON LEAVE OF W_CmbEmpFin IN FRAME DEFAULT-FRAME /* Final */
DO:
    APPLY "VALUE-CHANGED" TO W_CmbEmpFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEmpFin C-Win
ON VALUE-CHANGED OF W_CmbEmpFin IN FRAME DEFAULT-FRAME /* Final */
DO:
  W_EmpFin = INTEGER(SUBSTRING(W_CmbEmpFin:SCREEN-VALUE,1,4)) NO-ERROR.
  IF W_EmpIni GT W_EmpFin THEN 
     ASSIGN W_EmpIni                 = W_EmpFin
            W_CmbEmpIni:SCREEN-VALUE = W_CmbEmpFin:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbEmpIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEmpIni C-Win
ON LEAVE OF W_CmbEmpIni IN FRAME DEFAULT-FRAME /* Inicial */
DO:
  APPLY "VALUE-CHANGED" TO W_CmbEmpIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEmpIni C-Win
ON VALUE-CHANGED OF W_CmbEmpIni IN FRAME DEFAULT-FRAME /* Inicial */
DO:
  W_EmpIni = INTEGER(SUBSTRING(W_CmbEmpIni:SCREEN-VALUE,1,4)) NO-ERROR.
  IF W_EmpIni GT W_EmpFin THEN 
     ASSIGN W_EmpFin                 = W_EmpIni
            W_CmbEmpFin:SCREEN-VALUE = W_CmbEmpIni:SCREEN-VALUE.
  APPLY "ENTRY" TO W_CmbEmpFin.            
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbEstratofin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEstratofin C-Win
ON LEAVE OF W_CmbEstratofin IN FRAME DEFAULT-FRAME /* Estrato Final */
DO:
  APPLY "VALUE-CHANGED" TO W_CmbEstratoFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEstratofin C-Win
ON VALUE-CHANGED OF W_CmbEstratofin IN FRAME DEFAULT-FRAME /* Estrato Final */
DO:
  ASSIGN W_CmbEstratoFin.
  IF W_CmbEstratoIni GT W_CmbEstratoFin THEN 
     ASSIGN W_CmbEstratoIni              = W_CmbEstratoFin
            W_CmbEstratoIni:SCREEN-VALUE = W_CmbEstratoFin:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbEstratoIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEstratoIni C-Win
ON LEAVE OF W_CmbEstratoIni IN FRAME DEFAULT-FRAME /* Estrato Inicial */
DO:
  APPLY "VALUE-CHANGED" TO W_CmbEstratoIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbEstratoIni C-Win
ON VALUE-CHANGED OF W_CmbEstratoIni IN FRAME DEFAULT-FRAME /* Estrato Inicial */
DO:
  ASSIGN W_CmbEstratoIni.
  IF W_CmbEstratoIni GT W_CmbEstratoFin THEN 
     ASSIGN W_CmbEstratoFin                 = W_CmbEstratoIni
            W_CmbEstratoFin:SCREEN-VALUE    = W_CmbEstratoIni:SCREEN-VALUE.
  APPLY "ENTRY" TO W_CmbEstratoFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbFin C-Win
ON LEAVE OF W_CmbFin IN FRAME DEFAULT-FRAME /* Combo 2 */
DO:
  APPLY "VALUE-CHANGED" TO W_Cmbfin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbFin C-Win
ON VALUE-CHANGED OF W_CmbFin IN FRAME DEFAULT-FRAME /* Combo 2 */
DO:
  W_CauFin = INTEGER(SUBSTRING(W_CmbFin:SCREEN-VALUE,1,5)) NO-ERROR.
  IF W_CauIni GT W_CauFin THEN 
       ASSIGN W_CauIni                 = W_CauFin
              W_CmbIni:SCREEN-VALUE = W_CmbFin:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbIni C-Win
ON LEAVE OF W_CmbIni IN FRAME DEFAULT-FRAME /* Combo 1 */
DO:
  APPLY "VALUE-CHANGED" TO W_CmbIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbIni C-Win
ON VALUE-CHANGED OF W_CmbIni IN FRAME DEFAULT-FRAME /* Combo 1 */
DO:
 DO WITH FRAME f-Main:
  W_CauIni = INTEGER(SUBSTRING(W_CmbIni:SCREEN-VALUE,1,5)) NO-ERROR.
  IF W_CauIni GT W_CauFin THEN 
       ASSIGN W_CauFin                 = W_CauIni
              W_CmbFin:SCREEN-VALUE    = W_CmbIni:SCREEN-VALUE.  
 end.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfiFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfiFin C-Win
ON LEAVE OF W_CmbOfiFin IN FRAME DEFAULT-FRAME /* Final */
DO:
  RUN Verif_Oficina.
  IF W_HayError THEN DO:
    APPLY "ENTRY" TO W_CmbOfiIni IN FRAME {&FRAME-NAME}.
    RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfiFin C-Win
ON VALUE-CHANGED OF W_CmbOfiFin IN FRAME DEFAULT-FRAME /* Final */
DO:
  DO WITH FRAME {&FRAME-NAME}:
    W_OfiFin = INTEGER(SUBSTRING(W_CmbOfiFin:SCREEN-VALUE,1,3)) NO-ERROR.
    IF W_OfiIni GT W_OfiFin THEN 
       ASSIGN W_OfiIni                 = W_OfiFin
              W_CmbOfiIni:SCREEN-VALUE = W_CmbOfiFin:SCREEN-VALUE.
    RUN Verif_Oficina.
    IF W_HayError THEN DO:
       APPLY "ENTRY" TO W_CmbOfiIni IN FRAME {&FRAME-NAME}.
       RETURN NO-APPLY.
    END.
    RUN Activar_Emp.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbOfiIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfiIni C-Win
ON LEAVE OF W_CmbOfiIni IN FRAME DEFAULT-FRAME /* Inicial */
DO:
  RUN Verif_Oficina.
  IF W_HayError THEN 
    RETURN NO-APPLY.
  APPLY "ENTRY" TO W_CmbOfiFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbOfiIni C-Win
ON VALUE-CHANGED OF W_CmbOfiIni IN FRAME DEFAULT-FRAME /* Inicial */
DO:
 DO WITH FRAME {&FRAME-NAME}:
    W_OfiIni = INTEGER(SUBSTRING(W_CmbOfiIni:SCREEN-VALUE,1,3)) NO-ERROR.
    IF W_OfiIni GT W_OfiFin THEN 
       ASSIGN W_OfiFin                 = W_OfiIni
              W_CmbOfiFin:SCREEN-VALUE = W_CmbOfiIni:SCREEN-VALUE.
    RUN Verif_Oficina.
    IF W_HayError THEN 
       RETURN NO-APPLY.
    RUN Activar_Emp.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbZonaFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbZonaFin C-Win
ON LEAVE OF W_CmbZonaFin IN FRAME DEFAULT-FRAME /* Zona Final */
DO:
  APPLY "VALUE-CHANGED" TO W_CmbZonaFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbZonaFin C-Win
ON VALUE-CHANGED OF W_CmbZonaFin IN FRAME DEFAULT-FRAME /* Zona Final */
DO:
  W_ZonaFin = INTEGER(SUBSTRING(W_CmbZonaFin:SCREEN-VALUE,1,4)) NO-ERROR.
  IF W_ZonaIni GT W_ZonaFin THEN 
     ASSIGN W_ZonaIni                = W_ZonaFin
            W_CmbZonaIni:SCREEN-VALUE = W_CmbZonaFin:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_CmbZonaIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbZonaIni C-Win
ON LEAVE OF W_CmbZonaIni IN FRAME DEFAULT-FRAME /* Zona Inicial */
DO:
  APPLY "VALUE-CHANGED" TO W_CmbZonaIni.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_CmbZonaIni C-Win
ON VALUE-CHANGED OF W_CmbZonaIni IN FRAME DEFAULT-FRAME /* Zona Inicial */
DO:
  W_ZonaIni = INTEGER(SUBSTRING(W_CmbZonaIni:SCREEN-VALUE,1,4)) NO-ERROR.
  IF W_ZonaIni GT W_ZonaFin THEN 
     ASSIGN W_ZonaFin                 = W_ZonaIni
            W_CmbZonaFin:SCREEN-VALUE = W_CmbZonaIni:SCREEN-VALUE.
  APPLY "ENTRY" TO W_CmbZonaFin.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_EmpFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_EmpFin C-Win
ON LEAVE OF W_EmpFin IN FRAME DEFAULT-FRAME /* Final */
DO:
  ASSIGN W_EmpFin.
  IF W_EmpIni GT W_EmpFin THEN 
     ASSIGN W_EmpIni              = W_EmpFin
            W_EmpIni:SCREEN-VALUE = W_EmpFin:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_EmpIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_EmpIni C-Win
ON LEAVE OF W_EmpIni IN FRAME DEFAULT-FRAME /* Inicial */
DO:
  ASSIGN W_EmpIni.
  IF W_EmpIni GT W_EmpFin THEN 
     ASSIGN W_EmpFin              = W_EmpIni
            W_EmpFin:SCREEN-VALUE = W_EmpIni:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecFin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecFin C-Win
ON LEAVE OF W_FecFin IN FRAME DEFAULT-FRAME /* Fecha Final */
DO:
  ASSIGN W_FecFin.
  IF W_FecIni GT W_FecFin THEN 
     ASSIGN W_FecIni              = W_FecFin
            W_FecIni:SCREEN-VALUE = W_FecFin:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_FecIni
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_FecIni C-Win
ON LEAVE OF W_FecIni IN FRAME DEFAULT-FRAME /* Fecha Inicial */
DO:
  ASSIGN W_FecIni.
  IF W_FecIni GT W_FecFin THEN 
     ASSIGN W_FecFin                 = W_FecIni
            W_FecFin:SCREEN-VALUE    = W_FecIni:SCREEN-VALUE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Para1Fin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Para1Fin C-Win
ON LEAVE OF W_Para1Fin IN FRAME DEFAULT-FRAME /* Edad Final */
DO:
 ASSIGN W_Para1Fin.
  IF W_Para1Ini GT W_Para1Fin THEN 
     ASSIGN W_Para1Ini              = W_Para1Fin
            W_Para1Ini:SCREEN-VALUE = W_Para1Fin:SCREEN-VALUE. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_Para1Ini
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_Para1Ini C-Win
ON LEAVE OF W_Para1Ini IN FRAME DEFAULT-FRAME /* Edad Inicial */
DO:
  ASSIGN W_Para1Ini.
  IF W_Para1Ini GT W_Para1Fin THEN 
     ASSIGN W_Para1Fin              = W_Para1Ini
            W_Para1Fin:SCREEN-VALUE = W_Para1Ini:SCREEN-VALUE.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME W_TogEntidad
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_TogEntidad C-Win
ON VALUE-CHANGED OF W_TogEntidad IN FRAME DEFAULT-FRAME /* Consolidado Entidad */
DO:
  ASSIGN W_TogEntidad.
  RUN Cambio_TogEntidad.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 


/* ***************************  Main Block  *************************** */

/* Set CURRENT-WINDOW: this will parent dialog-boxes and frames.        */
ASSIGN CURRENT-WINDOW                = {&WINDOW-NAME} 
       THIS-PROCEDURE:CURRENT-WINDOW = {&WINDOW-NAME}.

/* The CLOSE event can be used from inside or outside the procedure to  */
/* terminate it.                                                        */
ON CLOSE OF THIS-PROCEDURE 
   RUN disable_UI.

/* Best default for GUI applications is...                              */
PAUSE 0 BEFORE-HIDE.

/* Now enable the interface and wait for the exit condition.            */
/* (NOTE: handle ERROR and END-KEY so cleanup code will always fire.    */
MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
   ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
   ASSIGN  W_HorPan = STRING(TIME,"HH:MM AM").

  RUN enable_UI.
  DISPLAY W_Nom_Oficina @ W_NomOfi 
          W_Fecha       @ W_FecPan
          W_HorPan WITH FRAME {&FRAME-NAME}.
  RUN Ini_Vbles.
  W_TitInf = "{3}".
  DISPLAY W_TitInf WITH FRAME {&FRAME-NAME}.

  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Activar_Campos C-Win 
PROCEDURE Activar_Campos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    ASSIGN W_CmbEstratoIni:HIDDEN      = TRUE 
           W_CmbEstratoFin:HIDDEN      = TRUE 
           W_CmbZonaIni:HIDDEN         = TRUE 
           W_CmbZonaFin:HIDDEN         = TRUE.
    IF {2} = 1 THEN
      ASSIGN W_FecIni:SENSITIVE        = TRUE
             W_FecFin:SENSITIVE        = TRUE.
    ELSE
    IF {2} = 2 THEN    /* Fechas y Edades */
     ASSIGN W_FecIni:SENSITIVE         = TRUE
            W_Fecfin:SENSITIVE         = TRUE
            W_Para1Ini:HIDDEN          = FALSE
            W_Para1Fin:HIDDEN          = FALSE
            W_Para1Ini:SENSITIVE       = TRUE
            W_Para1Fin:SENSITIVE       = TRUE
            w_Para1Ini:LABEL           = "Edad Inicial"
            W_Para1Fin:LABEL           = "Edad Final".
    ELSE
    IF {2} = 3 THEN DO:  /*  Estratos y Zonas */
      ASSIGN W_FecIni:HIDDEN           = TRUE
             W_FecFin:HIDDEN           = TRUE
             W_Para1Ini:HIDDEN         = TRUE
             W_Para1Fin:HIDDEN         = TRUE
             W_CmbEstratoIni:HIDDEN    = FALSE
             W_CmbEstratoFin:HIDDEN    = FALSE
             W_CmbEstratoIni:SENSITIVE = TRUE
             W_CmbEstratoFin:SENSITIVE = TRUE
             W_CmbZonaIni:HIDDEN       = FALSE
             W_CmbZonaFin:HIDDEN       = FALSE
             W_CmbZonaIni:SENSITIVE    = TRUE
             W_CmbZonaFin:SENSITIVE    = TRUE.
       RUN Llenar_CmbZonas.
     END.
     ELSE
     IF {2} = 4 OR {2} = 5 THEN DO:   /*Causales de Ingreso o Retiros */
       ASSIGN W_FecIni:SENSITIVE       = TRUE
              W_Fecfin:SENSITIVE       = TRUE
              W_CmbIni:HIDDEN          = FALSE
              W_CmbFin:HIDDEN          = FALSE
              W_CmbIni:SENSITIVE       = TRUE
              W_CmbFin:SENSITIVE       = TRUE.
       RUN Cargar_Varios.
     END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Activar_Emp C-Win 
PROCEDURE Activar_Emp :
/*------------------------------------------------------------------------------
  Purpose: Procedimiento que se encarga de activar los campos de empresas.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 DO WITH FRAME {&FRAME-NAME}:
  IF {1} = 2 THEN DO:
    IF W_OfiIni = W_OfiFin THEN DO:
      ASSIGN W_CmbEmpIni:Sensitive = TRUE
             W_CmbEmpFin:Sensitive = TRUE
             W_EmpIni:SENSITIVE    = FALSE
             W_EmpFin:SENSITIVE    = FALSE
             w_EmpIni:HIDDEN       = TRUE
             W_EmpFin:HIDDEN       = TRUE
             W_CmbEmpIni:HIDDEN    = FALSE
             W_CmbEmpFin:HIDDEN    = FALSE.
      RUN Llenar_CmbEmp.
    END.
    ELSE
      ASSIGN W_EmpIni:SENSITIVE    = TRUE
             W_EmpFin:SENSITIVE    = TRUE
             W_CmbEmpIni:SENSITIVE = FALSE
             W_CmbEmpFin:SENSITIVE = FALSE
             w_EmpIni:HIDDEN       = FALSE
             W_EmpFin:HIDDEN       = FALSE
             W_CmbEmpIni:HIDDEN    = TRUE
             W_CmbEmpFin:HIDDEN    = TRUE.
  END.  
 END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects C-Win _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cambio_TogEntidad C-Win 
PROCEDURE Cambio_TogEntidad :
/*------------------------------------------------------------------------------
  Purpose: Activar campos del TogEntdidad, depende de la consulta que se desea.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
    IF W_TogEntidad THEN
      ASSIGN W_CmbOfiIni:Sensitive = FALSE
             W_CmbOfiFin:Sensitive = FALSE
             W_CmbEmpIni:Sensitive = FALSE
             W_CmbEmpFin:Sensitive = FALSE
             W_EmpIni:Sensitive    = FALSE
             W_EmpFin:Sensitive    = FALSE
             W_TipProg             = 0.
    ELSE
      ASSIGN W_CmbOfiIni:Sensitive = TRUE
             W_CmbOfiFin:Sensitive = TRUE
             W_TipProg             = 1.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cargar_Varios C-Win 
PROCEDURE Cargar_Varios :
/*------------------------------------------------------------------------------
  Purpose: Carga las causales de Ingreso desde varios.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_CmbIni:LIST-ITEMS = ""
            W_CmbFin:LIST-ITEMS = "".

   IF {2} = 4 THEN DO:
     ASSIGN W_CmbIni:LABEL     = "Causal Ingreso Inicial"
            W_CmbFin:LABEL     = "Causal Ingreso Final".

     FOR EACH Varios FIELDS (Tipo Clase Codigo Estado Descripcion)
                      WHERE Varios.Tipo   EQ "ING"
                        AND Varios.clase  EQ "INGR"
                        AND Varios.Estado EQ 1
                    NO-LOCK:
        ASSIGN W_OfStr = STRING(Varios.Codigo,"99999") + "-" + STRING(Varios.Descripcion,"X(30)")
               W_Rpta  = W_CmbIni:ADD-LAST(W_OfStr)
               w_Rpta  = W_CmbFin:ADD-LAST(W_Ofstr).
     END.
   END.
   ELSE DO:
     ASSIGN W_CmbIni:LABEL     = "Causal Retiro Inicial"
            W_CmbFin:LABEL     = "Causal Retiro Final".
     FOR EACH Varios FIELDS (Tipo Codigo Clase Estado Descripcion)
                      WHERE Varios.Tipo   EQ "RET"
                        AND Varios.clase  EQ "RETI"
                        AND Varios.Estado EQ 1
                    NO-LOCK:
        ASSIGN W_OfStr = STRING(Varios.Codigo,"99999") + "-" + STRING(Varios.Descripcion,"X(30)")
               W_Rpta  = W_CmbIni:ADD-LAST(W_OfStr)
               w_Rpta  = W_CmbFin:ADD-LAST(W_Ofstr).
     END.
   END.
   IF W_CmbEmpIni:NUM-ITEMS GE 0 THEN
     ASSIGN W_CmbIni:SCREEN-VALUE = W_CmbIni:ENTRY(1)
            W_CmbFin:SCREEN-VALUE = W_CmbFin:ENTRY(1)
            W_CauIni = INTEGER(SUBSTRING(W_CmbIni:SCREEN-VALUE,1,5))
            W_CauFin = INTEGER(SUBSTRING(W_CmbFin:SCREEN-VALUE,1,5)).
  END.  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
  THEN DELETE WIDGET C-Win.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win _DEFAULT-ENABLE
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
  DISPLAY W_FecPan W_NomOfi W_HorPan W_TitInf W_CmbOfiIni W_TogEntidad 
          W_CmbOfiFin W_CmbClientes W_CmbClasifConso W_CmbEmpIni W_EmpIni 
          W_EmpFin W_CmbEmpFin W_FecIni W_CmbEstratoIni W_FecFin W_CmbEstratofin 
          W_CmbZonaIni W_CmbZonaFin 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  ENABLE W_CmbOfiIni W_TogEntidad W_CmbOfiFin W_CmbClientes W_CmbClasifConso 
         W_CmbZonaIni RECT-17 RECT-5 Btn_Cancelar Btn_Imprimir Btn_Ayuda 
      WITH FRAME DEFAULT-FRAME IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-DEFAULT-FRAME}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ini_Vbles C-Win 
PROCEDURE Ini_Vbles :
/*------------------------------------------------------------------------------
  Purpose: Procedimiento que carga los respectivos combos.    
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}: 
     ASSIGN W_OfiIni        = W_Oficina
            W_OfiFin        = W_Oficina
            W_TipoCli       = 1
            W_Clasif        = 1
            w_EmpIni:HIDDEN = TRUE
            W_EmpFin:HIDDEN = TRUE.

     ASSIGN  W_CmbClasif:SCREEN-VALUE      = W_CmbClasif:ENTRY(1)
             W_CmbClasifConso:SCREEN-VALUE = W_CmbClasif:ENTRY(1).

     IF {1} = 2 THEN DO:  /* cuando es por empresa */
       ASSIGN W_TogEntidad:SENSITIVE        = FALSE
              W_cmbClasifConso:SENSITIVE    = FALSE
              W_CmbClasifConso:HIDDEN       = TRUE
              W_CmbClasif:SENSITIVE         = TRUE
              W_CmbClasif:HIDDEN            = FALSE
              W_CmbClasif:SCREEN-VALUE      = W_CmbClasif:ENTRY(1).
       RUN Llenar_CmbEmp.
     END.
     ELSE DO:
       IF {1} = 0 THEN     /* consolidado por entidad */
         ASSIGN W_TogEntidad = TRUE
                W_TogEntidad:CHECKED = TRUE.
       ELSE
         ASSIGN W_TogEntidad = FALSE.
                W_TogEntidad:CHECKED = FALSE.
       DISPLAY W_TogEntidad.
       W_TogEntidad:SENSITIVE = TRUE.
       RUN Cambio_TogEntidad.
     END.

     FIND LAST Oficinas WHERE Oficinas.Estado NE 3 NO-LOCK NO-ERROR.
     IF AVAILABLE(Oficinas) THEN 
        W_OfiUltima = Oficinas.Oficina.
     ELSE
        W_OfiUltima = 999.

     ASSIGN W_CmbOfiIni:LIST-ITEMS = ""
            W_CmbOfiFin:LIST-ITEMS = "".

     FOR EACH Oficinas FIELDS(Estado Oficina Nombre) WHERE Oficinas.Estado <> 3 NO-LOCK:
        ASSIGN W_OfStr = STRING(Oficinas.Oficina,"999") + "-" + STRING(Oficinas.nombre,"X(20)")
               W_Rpta  = W_CmbOfiIni:ADD-LAST(W_OfStr)
               W_Rpta  = W_CmbOfiFin:ADD-LAST(W_OfStr).

        IF Oficinas.Oficina EQ W_Oficina THEN
           ASSIGN W_CmbOfiIni:SCREEN-VALUE = W_OfStr
                  W_CmbOfiFin:SCREEN-VALUE = W_OfStr.
     END.
     RUN Activar_Campos.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_CmbEmp C-Win 
PROCEDURE Llenar_CmbEmp :
/*------------------------------------------------------------------------------
  Purpose: Permite llenar los combos con la oficina seleccionada.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_CmbEmpIni:LIST-ITEMS = ""
            W_CmbEmpFin:LIST-ITEMS = ""
             w_EmpIni:HIDDEN       = TRUE
             W_EmpFin:HIDDEN       = TRUE.

     FOR EACH Empresas FIELDS (Oficina Estado Cod_Empresa Nombre_Codigo) WHERE Empresas.Oficina EQ W_OfiIni
                         AND Empresas.Estado   EQ 1
                     NO-LOCK:
        ASSIGN W_OfStr = STRING(Empresas.Cod_Empresa, "9999") + "-" + STRING(Empresas.Nombre_Codigo,"X(20)")
               W_Rpta  = W_CmbEmpIni:ADD-LAST(W_OfStr)
               w_Rpta  = W_CmbEmpFin:ADD-LAST(W_Ofstr).
     END.
     IF W_CmbEmpIni:NUM-ITEMS GE 0 THEN
       ASSIGN W_CmbEmpIni:SCREEN-VALUE = W_CmbEmpIni:ENTRY(1)
              W_CmbEmpFin:SCREEN-VALUE = W_CmbEmpFin:ENTRY(1).
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Llenar_CmbZonas C-Win 
PROCEDURE Llenar_CmbZonas :
/*------------------------------------------------------------------------------
  Purpose: carga el combo de zonas.     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DO WITH FRAME {&FRAME-NAME}:
     ASSIGN W_CmbZonaIni:LIST-ITEMS = ""
            W_CmbZonaFin:LIST-ITEMS = "".

     FOR EACH Zonas FIELDS (Cod_zona Nombre) WHERE Zonas.Estado EQ 1 NO-LOCK:
        ASSIGN W_OfStr = STRING(Zonas.Cod_Zona,"9999") + "-" + STRING(Zonas.Nombre,"X(20)")
               W_Rpta  = W_CmbZonaIni:ADD-LAST(W_OfStr)
               w_Rpta  = W_CmbZonaFin:ADD-LAST(W_Ofstr).
     END.
     IF W_CmbZonaIni:NUM-ITEMS GE 0 THEN
       ASSIGN W_CmbZonaIni:SCREEN-VALUE = W_CmbZonaIni:ENTRY(1)
              W_CmbZonaFin:SCREEN-VALUE = W_CmbZonaFin:ENTRY(1).
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Verif_Oficina C-Win 
PROCEDURE Verif_Oficina :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    W_HayError = FALSE.
    IF W_OfiIni GT W_OfiFin THEN
       W_HayError = TRUE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


