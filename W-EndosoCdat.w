&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12 GUI
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS C-Win 
/*------------------------------------------------------------------------

  File: 

  Description: 

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  Author: 

  Created: 

------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
{Incluido\variable.i "shared"}
/* ***************************  Definitions  ************************** */

/* Parameters Definitions ---                                           */

/* Local Variable Definitions ---                                       */

 /*para buscar cuentas destino y cuentas debito automatico*/
  DEFINE VARIABLE Puntero AS ROWID.
  DEFINE VAR W_Age  LIKE Ahorros.Agencia.
  DEFINE VAR W_Pro  LIKE Ahorros.Cod_Ahorro.
  DEFINE VAR W_Nit  LIKE Ahorros.Nit.
  DEFINE VAR W_Cue  LIKE Ahorros.Cue_Ahorros.
  DEFINE VAR W_vlr  LIKE Ahorros.Sdo_Disponible INITIAL 0.
  DEFINE VARIABLE P_Nit      LIKE Clientes.Nit.
  DEFINE VARIABLE p_Nombre   LIKE Clientes.Nombre.
  DEFINE VARIABLE P_Apellido LIKE Clientes.Apellido1.
  DEFINE VARIABLE P_AgeCli   LIKE Clientes.Agencia.
  DEFINE VARIABLE W_Error       AS LOGICAL.
  DEFINE VARIABLE W_Autorizo  LIKE Usuarios.Usuario.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Window
&Scoped-define DB-AWARE no

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME Frm_endoso
&Scoped-define BROWSE-NAME Brw_ahorros

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES Ahorros

/* Definitions for BROWSE Brw_ahorros                                   */
&Scoped-define FIELDS-IN-QUERY-Brw_ahorros Ahorros.Agencia Ahorros.Cue_Ahorros Ahorros.Sdo_Disponible Ahorros.Tasa Ahorros.Plazo Ahorros.fec_Apertura Ahorros.fec_vencimiento   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Brw_ahorros   
&Scoped-define SELF-NAME Brw_ahorros
&Scoped-define QUERY-STRING-Brw_ahorros FOR EACH Ahorros             WHERE Ahorros.Nit = "Clientes.nit"             AND ahorros.tip_ahorro = 3             AND ahorros.estado EQ 1             AND ahorros.sdo_disponible GT 0 NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Brw_ahorros OPEN QUERY {&SELF-NAME} FOR EACH Ahorros             WHERE Ahorros.Nit = "Clientes.nit"             AND ahorros.tip_ahorro = 3             AND ahorros.estado EQ 1             AND ahorros.sdo_disponible GT 0 NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Brw_ahorros Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-Brw_ahorros Ahorros


/* Definitions for FRAME Frm_endoso                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-Frm_endoso ~
    ~{&OPEN-QUERY-Brw_ahorros}
&Scoped-define SELF-NAME Frm_endoso
&Scoped-define QUERY-STRING-Frm_endoso FOR EACH Ahorros SHARE-LOCK
&Scoped-define OPEN-QUERY-Frm_endoso OPEN QUERY {&SELF-NAME} FOR EACH Ahorros SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-Frm_endoso Ahorros
&Scoped-define FIRST-TABLE-IN-QUERY-Frm_endoso Ahorros


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-FIELDS Ahorros.Nit 
&Scoped-define ENABLED-TABLES Ahorros
&Scoped-define FIRST-ENABLED-TABLE Ahorros
&Scoped-Define ENABLED-OBJECTS Brw_ahorros btn_procesar Btn_Cancelar ~
BtnDone Btn_Consulta BUTTON-1 BUTTON-95 RECT-274 RECT-309 RECT-310 
&Scoped-Define DISPLAYED-FIELDS Ahorros.Nit Clientes.Nit 
&Scoped-define DISPLAYED-TABLES Ahorros Clientes
&Scoped-define FIRST-DISPLAYED-TABLE Ahorros
&Scoped-define SECOND-DISPLAYED-TABLE Clientes
&Scoped-Define DISPLAYED-OBJECTS W_NomTitular W_Nuevo 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-6 Btn_Cancelar BtnDone Btn_Consulta BUTTON-1 BUTTON-95 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR C-Win AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 10 BY 1.62
     BGCOLOR 8 FONT 4.

DEFINE BUTTON Btn_Cancelar 
     LABEL "&Cancelar" 
     SIZE 10 BY 1.65
     FONT 4.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 3" 
     SIZE 10 BY 1.62.

DEFINE BUTTON btn_procesar 
     LABEL "&Endosar" 
     SIZE 10 BY 1.65
     FONT 4.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-95 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 95" 
     SIZE 4 BY 1.12.

DEFINE VARIABLE W_NomTitular AS CHARACTER FORMAT "X(50)":U 
     LABEL "Nombre del Titular" 
     VIEW-AS FILL-IN 
     SIZE 46 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Nuevo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nuevo Portador" 
     VIEW-AS FILL-IN 
     SIZE 46 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-274
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 12 BY 3.77.

DEFINE RECTANGLE RECT-309
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86 BY 5.12.

DEFINE RECTANGLE RECT-310
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 86 BY 2.42.

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Brw_ahorros FOR 
      Ahorros SCROLLING.

DEFINE QUERY Frm_endoso FOR 
      Ahorros SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Brw_ahorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Brw_ahorros C-Win _FREEFORM
  QUERY Brw_ahorros NO-LOCK DISPLAY
      Ahorros.Agencia          FORMAT "999":U                    LABEL "Agencia"
      Ahorros.Cue_Ahorros      FORMAT "X(14)":U                  LABEL "Nro. Titulo"
      Ahorros.Sdo_Disponible   FORMAT "->>>>>,>>>,>>>,>>9.99":U  LABEL "Valor"
      Ahorros.Tasa             FORMAT ">>9.9999999":U            LABEL "Tasa"
      Ahorros.Plazo            FORMAT ">,>>9":U                  LABEL "Plazo"
      Ahorros.fec_Apertura     LABEL "Fec_Aper"
      Ahorros.fec_vencimiento  LABEL "Fec_Vcto"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 83.57 BY 2.96
         FONT 5 ROW-HEIGHT-CHARS .54 EXPANDABLE.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME Frm_endoso
     Brw_ahorros AT ROW 3.15 COL 3.43
     Ahorros.Nit AT ROW 1.88 COL 4.14 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 FONT 5
     Clientes.Nit AT ROW 7.73 COL 4 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 17 BY .81
          BGCOLOR 15 
     btn_procesar AT ROW 5.31 COL 90
     Btn_Cancelar AT ROW 6.96 COL 90
     BtnDone AT ROW 8.58 COL 90
     Btn_Consulta AT ROW 3.15 COL 90
     BUTTON-1 AT ROW 1.54 COL 90
     BUTTON-95 AT ROW 10.73 COL 93
     W_NomTitular AT ROW 1.81 COL 39 COLON-ALIGNED
     W_Nuevo AT ROW 7.73 COL 39 COLON-ALIGNED
     RECT-274 AT ROW 1.27 COL 89
     RECT-309 AT ROW 1.27 COL 2
     RECT-310 AT ROW 6.92 COL 2
     "  Información del nuevo Portador o Titular" VIEW-AS TEXT
          SIZE 36 BY .81 AT ROW 6.65 COL 4
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 100.43 BY 11.08
         BGCOLOR 17 FONT 5.


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
         TITLE              = "Endoso de CDAT - W-EndosoCdat.w"
         HEIGHT             = 11.08
         WIDTH              = 100.72
         MAX-HEIGHT         = 20.92
         MAX-WIDTH          = 114.29
         VIRTUAL-HEIGHT     = 20.92
         VIRTUAL-WIDTH      = 114.29
         RESIZE             = yes
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         KEEP-FRAME-Z-ORDER = yes
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME



/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW C-Win
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME Frm_endoso
   NOT-VISIBLE Custom                                                   */
/* BROWSE-TAB Brw_ahorros 1 Frm_endoso */
/* SETTINGS FOR BUTTON BtnDone IN FRAME Frm_endoso
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME Frm_endoso
   6                                                                    */
/* SETTINGS FOR BUTTON Btn_Consulta IN FRAME Frm_endoso
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-1 IN FRAME Frm_endoso
   6                                                                    */
/* SETTINGS FOR BUTTON BUTTON-95 IN FRAME Frm_endoso
   6                                                                    */
/* SETTINGS FOR FILL-IN Clientes.Nit IN FRAME Frm_endoso
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_NomTitular IN FRAME Frm_endoso
   NO-ENABLE                                                            */
ASSIGN 
       W_NomTitular:READ-ONLY IN FRAME Frm_endoso        = TRUE.

/* SETTINGS FOR FILL-IN W_Nuevo IN FRAME Frm_endoso
   NO-ENABLE                                                            */
ASSIGN 
       W_Nuevo:READ-ONLY IN FRAME Frm_endoso        = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(C-Win)
THEN C-Win:HIDDEN = no.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Brw_ahorros
/* Query rebuild information for BROWSE Brw_ahorros
     _START_FREEFORM
 OPEN QUERY {&SELF-NAME} FOR EACH Ahorros
            WHERE Ahorros.Nit = "Clientes.nit"
            AND ahorros.tip_ahorro = 3
            AND ahorros.estado EQ 1
            AND ahorros.sdo_disponible GT 0 NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Where[1]         = "Clientes.Nit = ""ahorros.nit""
 AND ahorros.tip_ahorro = 3"
     _Query            is OPENED
*/  /* BROWSE Brw_ahorros */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME Frm_endoso
/* Query rebuild information for FRAME Frm_endoso
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Ahorros SHARE-LOCK.
     _END_FREEFORM
     _Query            is OPENED
*/  /* FRAME Frm_endoso */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME C-Win
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON END-ERROR OF C-Win /* Endoso de CDAT - W-EndosoCdat.w */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL C-Win C-Win
ON WINDOW-CLOSE OF C-Win /* Endoso de CDAT - W-EndosoCdat.w */
DO:
  /* This event will close the window and terminate the procedure.  */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Frm_endoso
&Scoped-define BROWSE-NAME Brw_ahorros
&Scoped-define SELF-NAME Brw_ahorros
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Brw_ahorros C-Win
ON VALUE-CHANGED OF Brw_ahorros IN FRAME Frm_endoso
DO :
/*    OPEN QUERY Brw_Ahorros FOR EACH Ahorros WHERE
          Ahorros.Nit EQ Clientes.nit:SCREEN-VALUE AND Ahorros.Estado EQ 1 AND tip_ahorro EQ 3 NO-LOCK.
*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone C-Win
ON CHOOSE OF BtnDone IN FRAME Frm_endoso /* Salir */
DO:
  &IF "{&PROCEDURE-TYPE}" EQ "SmartPanel" &THEN
    &IF "{&ADM-VERSION}" EQ "ADM1.1" &THEN
      RUN dispatch IN THIS-PROCEDURE ('exit').
    &ELSE
      RUN exitObject.
    &ENDIF
  &ELSE
      APPLY "CLOSE":U TO THIS-PROCEDURE.
  &ENDIF
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar C-Win
ON CHOOSE OF Btn_Cancelar IN FRAME Frm_endoso /* Cancelar */
DO:
  RELEASE ahorros. 
  RELEASE mov_ahorros. 
  RUN inicializar_variables.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta C-Win
ON CHOOSE OF Btn_Consulta IN FRAME Frm_endoso /* Button 3 */
DO:
 
  RUN C-Ahorros.r (INPUT "", 
                   OUTPUT W_Age, OUTPUT W_Pro, OUTPUT W_Nit, OUTPUT W_Cue).
 
 FIND FIRST Ahorros WHERE Ahorros.Agencia EQ W_Age AND
                     Ahorros.Cod_Ahorro   EQ 3     AND
                     Ahorros.Estado       EQ 1     AND
                     Ahorros.Nit          EQ W_Nit NO-LOCK NO-ERROR.
  IF AVAILABLE Ahorros THEN DO: 
     puntero = ROWID(Ahorros).
 /* Llenar el browser*/
  END.
  ELSE
      MESSAGE "Cliente sin Producto CDAT activo"
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_procesar C-Win
ON CHOOSE OF btn_procesar IN FRAME Frm_endoso /* Endosar */
DO:
DEFI VAR W_RowidAh AS ROWID.
DEFI VAR W_SiErr   AS LOG.
RUN P-ValiDarTrans IN W_Manija (OUTPUT W_SiErr,OUTPUT W_Autorizo).
IF NOT W_SiErr THEN DO:
  RETURN.
END.
Endosos:
DO TRANSACTION ON ERROR UNDO endosos:
   IF TRIM(Ahorros.nit:SCREEN-VALUE) NE " " AND TRIM(Clientes.nit:SCREEN-VALUE) NE " " AND TRIM(Ahorros.nit:SCREEN-VALUE) NE TRIM(Clientes.nit:SCREEN-VALUE) THEN 
   DO: 
     FIND CURRENT Ahorros  NO-ERROR.
     IF AVAILABLE(ahorros) THEN 
       DO:
          ASSIGN ahorros.nit = W_Nit.
          CREATE Mov_Ahorros.
          ASSIGN Mov_Ahorros.cod_operacion  = 999999999
                 Mov_ahorros.cod_ahorro     = Ahorros.Cod_Ahorro
                 Mov_Ahorros.Cue_Ahorros    = Ahorros.Cue_Ahorros
                 Mov_ahorros.nit            = Ahorros.Nit
                 Mov_Ahorros.NomApell_Trans = W_Nuevo
                 Mov_Ahorros.Cue_Ahorros    = W_Cue
                 Mov_Ahorros.Sdo_Disponible = W_Vlr
                 Mov_Ahorros.Descrip        = 'Endoso tit.Vlr.- Ant. nit ' + Ahorros.Nit:SCREEN-VALUE
                 Mov_Ahorros.Usuario        = W_Usuario
                 Mov_ahorros.cod_ahorro     = Ahorros.Cod_Ahorro
                 Mov_Ahorros.Usuario        = W_Usuario
                 Mov_Ahorros.Val_Efectivo   = W_Vlr
                 Mov_Ahorros.Val_Cheque     = Ahorros.Sdo_Disponible
                 Mov_Ahorros.Val_Efectivo   = Ahorros.Sdo_Disponible
                 Mov_Ahorros.Fecha          = TODAY
                 Mov_Ahorros.Hora           = TIME
                 Mov_Ahorros.Age_Fuente     = W_Agencia
                 Mov_Ahorros.Age_Destino    = W_Agencia
                 Mov_Ahorros.Agencia        = Ahorros.Agencia.
        /* Comentariado el cambio en historicos, solo en el manejo de CDAT  */         
        /*
          FIND CURRENT ahorros NO-LOCK NO-ERROR.
          RELEASE ahorros.
          FIND FIRST Mov_Ahorros WHERE Mov_Ahorros.Nit EQ W_Nit AND Mov_Ahorros.Cue_Ahorros EQ W_cue NO-LOCK NO-ERROR. 
          FOR EACH Mov_Ahorros WHERE Mov_Ahorros.Nit EQ W_Nit AND Mov_Ahorros.Cue_Ahorros EQ W_cue:
            IF AVAILABLE(Mov_Ahorros) THEN 
             Mov_ahorros.nit = W_Nit.      
          END.
          FIND CURRENT mov_ahorros NO-LOCK NO-ERROR.
          RELEASE Mov_Ahorro. */
       END.
     ELSE
     DO:
        MESSAGE "No se pudo realizar la operacion "
            "Intentelo Nuevamente"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        APPLY "choose" TO btn_cancelar.
        RETURN NO-APPLY.
     END.

   END.
   ELSE
      DO:
       MESSAGE "No hay consistencia en los datos" SKIP(1)
           "Realice nuevamente la operacion"
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
       APPLY "choose" TO btn_cancelar.
       RETURN NO-APPLY.
      END.

 RELEASE ahorros.
 RELEASE clientes.



 MESSAGE " Termino la operacion exitosamente " SKIP(1)
   "El señor(a): " W_Nuevo:SCREEN-VALUE " es el nuevo " SKIP(1)
   "poseedor del titulo #" W_Cue " por valor de : " W_Vlr 
   VIEW-AS ALERT-BOX INFO BUTTONS OK.
 APPLY "choose" TO btn_cancelar.
 RETURN NO-APPLY.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 C-Win
ON CHOOSE OF BUTTON-1 IN FRAME Frm_endoso /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Clientes.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Clientes.Nit C-Win
ON LEAVE OF Clientes.Nit IN FRAME Frm_endoso /* Nit */
DO:
  DO WITH FRAME F_Endoso:
    FIND Clientes WHERE Clientes.Nit EQ Clientes.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) AND TRIM(Ahorros.nit:SCREEN-VALUE) NE TRIM(Clientes.nit:SCREEN-VALUE)  THEN
       W_Nuevo:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
    ELSE DO:
       RUN C-Clientes.R(INPUT 2, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
       ASSIGN Clientes.Nit:SCREEN-VALUE = P_Nit
              W_Nuevo:SCREEN-VALUE    = CAPS(TRIM(P_Nombre) + " " + P_Apellido).
       FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
    END.
     
    IF Clientes.Estado EQ 2 AND Clientes.Fec_Retiro NE ? THEN DO:
       MESSAGE "No se pueden crear cuentas de ahorro para clientes Retirados" SKIP
               VIEW-AS ALERT-BOX WARNING.
       APPLY "choose" TO Btn_Cancelar.
       RETURN NO-APPLY.               
    END.
     
    IF AVAILABLE(Clientes) AND TRIM(Ahorros.nit:SCREEN-VALUE) NE TRIM(Clientes.nit:SCREEN-VALUE)  THEN
    DO:
      IF Clientes.Tipo_Vinculo GT 2 THEN DO:
         MESSAGE W_NomTitular " No es un cliente de la Cooperativa" SKIP
                 "La persona o empresa debe estar matriculado como" SKIP
                 "Cliente o Asociado. Rectifique!" VIEW-AS ALERT-BOX.
         APPLY 'choose' TO btn_Cancelar.
         RETURN NO-APPLY. 
      END.
    
      FIND FIRST pro_ahorros WHERE pro_ahorros.tip_ahorro EQ 3 NO-LOCK NO-ERROR.
    
      IF Pro_Ahorros.Id_Asociado EQ 2 AND Clientes.Tipo_Vinculo EQ 2 THEN DO:
          MESSAGE "Este producto es Exclusivo para Asociados de la Cooperativa" SKIP
                  "Esta persona o Empresa No Cumple con la condición de Asociado." SKIP
                  "Rectifique!" VIEW-AS ALERT-BOX.
          APPLY 'choose' TO btn_Cancelar.
         RETURN NO-APPLY.
      END.
     
      IF Pro_Ahorros.Id_Asociado EQ 3 AND Clientes.Tipo_Vinculo EQ 1 THEN DO:
          MESSAGE "Este producto es Exclusivo para personas o empresas" skip
                  "No Asociadas de la Cooperativa" SKIP
                  "Esta persona o Empresa No Cumple con la condición de No Asociado." SKIP
                  "Rectifique!" VIEW-AS ALERT-BOX.
          APPLY 'choose' TO btn_Cancelar.
          RETURN NO-APPLY. 
      END.
     
      IF Brw_Ahorros:NUM-SELECTED-ROWS > 0 THEN                 
         ASSIGN W_Age  = Ahorros.Agencia
                W_Pro  = Ahorros.Cod_Ahorro
                W_Cue  = Ahorros.Cue_Ahorros
                W_Vlr  = Ahorros.Sdo_Disponible
                W_Nit  = Clientes.Nit:SCREEN-VALUE.
         
      RELEASE Clientes.
      btn_procesar:SENSITIVE = TRUE.
       /*APPLY 'Value-Changed' TO Cmb_TipoProductos.*/
    END.
    ELSE
    DO:
      MESSAGE "No se puede endosar al mismo titular de la cuenta, " SKIP(1)
         "por lo que se cancela la transacciòn"
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
      APPLY 'choose' TO btn_Cancelar.
      RETURN NO-APPLY. 
   END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Ahorros.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Ahorros.Nit C-Win
ON LEAVE OF Ahorros.Nit IN FRAME Frm_endoso /* Nit */
DO:
  DO WITH FRAME F_Endoso:
     /* btn_procesar:SENSITIVE = FALSE. */
     FIND Clientes WHERE Clientes.Nit EQ Ahorros.Nit:SCREEN-VALUE NO-LOCK NO-ERROR.

     IF AVAILABLE(Clientes) THEN
        W_NomTitular:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
     ELSE DO:
        RUN C-Clientes.R(INPUT 2, INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
        ASSIGN Ahorros.Nit:SCREEN-VALUE = P_Nit
               W_Nomtitular:SCREEN-VALUE    = CAPS(TRIM(P_Nombre) + " " + P_Apellido).
        FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
     END.
     
     FIND FIRST Ahorros WHERE 
             Ahorros.Tip_Ahorro     EQ 3 AND
             Ahorros.Nit            EQ SELF:SCREEN-VALUE AND
             Ahorros.sdo_disponible GT 0 AND
             Ahorros.Estado         EQ 1 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE Ahorros THEN DO:
        MESSAGE "No puede realizar endoso porque este cliente" SKIP
                W_NomTitular:SCREEN-VALUE "no tiene ningùn CDAT activo" SKIP
                "Rectifique la cedula!!" VIEW-AS ALERT-BOX ERROR.
        APPLY "choose" TO Btn_Cancelar IN FRAME Frm_Endoso.
        RETURN NO-APPLY. 
      END.

      RELEASE ahorros.
      ASSIGN W_Nuevo:SENSITIVE = TRUE.
      ASSIGN clientes.nit:SENSITIVE = TRUE. 
      OPEN QUERY Brw_Ahorros FOR EACH Ahorros WHERE
          Ahorros.Nit EQ Clientes.nit AND Ahorros.Estado EQ 1 AND 
          ahorros.tip_ahorro EQ 3 AND ahorros.sdo_disponible GT 0 NO-LOCK.
      APPLY "CHOOSE" TO Brw_ahorros.
     /*APPLY 'Value-Changed' TO Cmb_TipoProductos.*/
  END.
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
  RUN enable_UI.
  RUN inicializar_variables.
  IF NOT THIS-PROCEDURE:PERSISTENT THEN
    WAIT-FOR CLOSE OF THIS-PROCEDURE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI C-Win  _DEFAULT-DISABLE
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI C-Win  _DEFAULT-ENABLE
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

  {&OPEN-QUERY-Frm_endoso}
  GET FIRST Frm_endoso.
  DISPLAY W_NomTitular W_Nuevo 
      WITH FRAME Frm_endoso IN WINDOW C-Win.
  IF AVAILABLE Ahorros THEN 
    DISPLAY Ahorros.Nit 
      WITH FRAME Frm_endoso IN WINDOW C-Win.
  IF AVAILABLE Clientes THEN 
    DISPLAY Clientes.Nit 
      WITH FRAME Frm_endoso IN WINDOW C-Win.
  ENABLE Brw_ahorros Ahorros.Nit btn_procesar Btn_Cancelar BtnDone Btn_Consulta 
         BUTTON-1 BUTTON-95 RECT-274 RECT-309 RECT-310 
      WITH FRAME Frm_endoso IN WINDOW C-Win.
  {&OPEN-BROWSERS-IN-QUERY-Frm_endoso}
  VIEW C-Win.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE inicializar_variables C-Win 
PROCEDURE inicializar_variables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  ASSIGN
    Ahorros.Nit:SCREEN-VALUE IN FRAME FRM_endoso   = ""
    btn_procesar:SENSITIVE = FALSE
    Clientes.Nit:SCREEN-VALUE = ""
    W_Nuevo:SCREEN-VALUE = ' '
    w_NomTitular:SCREEN-VALUE = ' '
    W_Nuevo:SENSITIVE = FALSE
    clientes.nit:SENSITIVE = FALSE.
  OPEN QUERY Brw_Ahorros FOR EACH Ahorros WHERE
      Ahorros.Nit EQ Clientes.nit AND Ahorros.Estado EQ 1 AND tip_ahorro EQ 3 NO-LOCK.
  APPLY "choose" TO ahorros.nit.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE validar_transaccion C-Win 
PROCEDURE validar_transaccion :
/*------------------------------------------------------------------------------
  Purpose:     Seguridad en la operaci¢n de cambio de titular para el producto 3 
               de ahorros - CDAT
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER T_OfiVal  LIKE Agencias.Agencia.
  DEFINE INPUT  PARAMETER T_GrpVal  LIKE Grupos.Grupo.
  DEFINE INPUT  PARAMETER T_UsuVal  LIKE Usuarios.Usuario.
  DEFINE INPUT  PARAMETER T_OpeVal  LIKE Operacion.Cod_Operacion.
  DEFINE OUTPUT PARAMETER T_Validar AS LOGICAL.
  DEFINE OUTPUT PARAMETER T_NomOpe  LIKE Operacion.Nom_Operacion.
  
  DEFINE VAR T_Clave AS LOGICAL.
  
  ASSIGN T_Validar = FALSE T_Clave = FALSE.
  FIND Operacion WHERE Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Operacion) THEN DO:
     ASSIGN T_NomOpe = Operacion.Nom_Operacion
            T_Clave  = Operacion.Id_Clave.
  END.
  ELSE ASSIGN T_NomOpe = "".
  
  IF T_Clave THEN DO:
     MESSAGE "La Operación "  T_NomOpe   SKIP
             "Requiere Clave de SuperUsuario."
     VIEW-AS ALERT-BOX INFORMATION BUTTONS OK
     TITLE "Validación En Taquilla".
     RUN P-ValiDarTrans IN W_Manija (OUTPUT W_Error,OUTPUT W_Autorizo).
     IF W_Error EQ FALSE THEN DO:
        ASSIGN T_Validar = TRUE.
     END.
  END.
  
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 3
                           AND   Res_Operacion.Usuario       EQ T_UsuVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 2
                           AND   Res_Operacion.Grupo         EQ T_GrpVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
  FIND FIRST Res_Operacion WHERE Res_Operacion.Restriccion   EQ 1
                           AND   Res_Operacion.Agencia       EQ T_OfiVal
                           AND   Res_Operacion.Cod_Operacion EQ T_OpeVal NO-LOCK NO-ERROR.
  IF AVAILABLE(Res_Operacion) THEN DO:
     ASSIGN T_Validar = TRUE.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

