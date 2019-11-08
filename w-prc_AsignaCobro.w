&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM1
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
CREATE WIDGET-POOL.

    {Incluido\VARIABLE.I "SHARED"}
    {Incluido\VARCON.I "SHARED"}

    DEFINE VAR TC_IntCte AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
    DEFINE VAR TC_IntAnt AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
    DEFINE VAR TC_DifCob AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
    DEFINE VAR TC_IntMor AS DECIMAL FORMAT ">>>,>>>,>>9.99" .
    DEFINE VAR WK_CtaProDeb    LIKE Cuentas.Cuenta .
    DEFINE VAR WK_CtaProIntGto LIKE Cuentas.Cuenta .
    DEFINE VAR WK_CtaProCosGto LIKE Cuentas.Cuenta .
    DEFINE VAR W_cuprot        LIKE creditos.cod_credito .

DEFINE VARIABLE RangoMora AS INTEGER INITIAL 0.
DEFINE VAR Dias AS INTEGER FORMAT "999" INITIAL 0.
DEFINE VAR Ages AS INTEGER FORMAT "99" INITIAL 0 .
DEFINE VAR i AS INTEGER FORMAT "999" NO-UNDO.
DEFINE VAR W_Cierremes AS LOGICAL INITIAL NO .
DEFI VAR W_IntMora   AS DECIMAL FORMAT ">>>,>>>,>>>,>>9" .                                                       
DEFI VAR W_TasaMr       LIKE Creditos.Tasa .                                                                        

DEFINE VAR W_Aportes LIKE Ahorros.Sdo_Disponible .
DEFINE VAR W_ApoDesc LIKE Ahorros.Sdo_Disponible .

DEFI VAR W_UsuAbo LIKE Usuarios.Usuario INIT "" .
DEFI VAR W_UsuLib LIKE Usuarios.Usuario INIT "" .

DEFI TEMP-TABLE CopMov_Inst LIKE Mov_Instancias
     FIELD W_RowidMI AS ROWID
     INDEX IxPpal Nit Cuenta estado Num_Solicitud.


/*operaciones*/    
DEFINE VAR Op_Capital LIKE Operacion.Cod_Operacion INITIAL "020101001".
DEFINE VAR Op_Mora    LIKE Operacion.Cod_Operacion INITIAL "020101002".
DEFINE VAR Op_IntCor  LIKE Operacion.Cod_Operacion INITIAL "020101003".
DEFINE VAR Op_IntDif  LIKE Operacion.Cod_Operacion INITIAL "020101004".
DEFINE VAR Op_IntAnt  LIKE Operacion.Cod_Operacion INITIAL "020101005".

DEFINE VAR TA_IntCte AS DECIMAL FORMAT "->>,>>>,>>9.99".
DEFINE VAR TA_IntAnt AS DECIMAL FORMAT "->>,>>>,>>9.99".
DEFINE VAR TA_DifCob AS DECIMAL FORMAT "->>,>>>,>>9.99".    
DEFINE VAR TA_IntMor AS DECIMAL FORMAT "->>,>>>,>>9.99".    
DEFINE VAR TA_MorDif AS DECIMAL FORMAT "->>,>>>,>>9.99".

  DEFINE VAR W_NumCbt      LIKE Comprobantes.Secuencia.
  DEFINE VAR W_Cbte        LIKE Comprobantes.Comprobante.
  DEFINE VAR W_TasaWk      LIKE Creditos.Tasa.
  DEFINE VAR W_TasaUs      LIKE Creditos.Tasa.
  DEFINE VAR W_CopiaDia    AS   INTEGER INITIAL 0 FORMAT "9".  
  DEFINE VAR W_MultDia     AS   INTEGER INITIAL 0 FORMAT "9".
  DEFINE VAR W_FechaT      AS   DATE.
  DEFINE VAR W_Ok AS LOGICAL.
  DEFINE VAR W_Error AS LOGICAL.
  DEFINE VAR g_Texto AS CHARACTER FORMAT "X(20)".
  DEFINE VAR WAgeI LIKE Agencias.Agencia.
  DEFINE VAR WAgeF LIKE Agencias.Agencia.
  DEFINE VAR W_HayAlgunErr AS   LOGICAL INITIAL FALSE.
  DEFINE VAR W_ErrorCbte        AS   LOGICAL.
  DEFINE VAR W_SiProceso        AS   LOGICAL.               
  DEFINE VAR WTUsura            LIKE  Indicadores.Tasa.
  
  DEFINE VAR W_DiaDdc   LIKE Pro_Creditos.Per_GarPer.
  DEFINE VAR W_Ord      LIKE Instancias.Orden_Instancia.
  DEFI   VAR W_CtaAju   LIKE Cuentas.Cuenta.

/*guarda los usuarios disponibles para la siguiente instancia*/
  DEFINE TEMP-TABLE TProIns
    FIELD TP_Agencia LIKE Agencias.Agencia
    FIELD TP_Orden LIKE Instancias.Orden_Instancia
    FIELD TP_Instancia LIKE Instancias.Instancia
    FIELD TP_NomInstan AS CHARACTER FORMAT "X(30)"
    FIELD TP_Usuario   LIKE Usuarios.Usuario
    FIELD TP_NomUsuar  AS CHARACTER FORMAT "X(30)"
    FIELD TP_Cantidad  AS INTEGER FORMAT "999".
    

DEFINE VAR AgeLiq AS INTEGER FORMAT "999" EXTENT 100.

/*variable para manejar la instancia en la que debe quedar un credito*/
DEFINE VAR W_AsigInst      LIKE Instancias.Instancia.

DEFINE TEMP-TABLE TCbtes
  FIELD CAge LIKE Agencias.Agencia
  FIELD CNum LIKE Comprobantes.Secuencia.

DEFI TEMP-TABLE TArrastre
     FIELD Ced LIKE Creditos.Nit
     FIELD tip LIKE creditos.tip_credito
     FIELD arrastre LIKE creditos.categoria
     FIELD NCRE     LIKE CREDITOS.NUM_CREDITO
     INDEX x1 ced tip .


DEFINE VARIABLE W_UsuDes LIKE Usuarios.Usuario.

/************************************************/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Lq

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-290 Cmb_Agencias BUTTON-166 ~
Btn_Ejecutar BtnDone 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Agencias WDia WMes WAno wproc 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     LABEL "&Salir" 
     SIZE 14 BY 1.38
     BGCOLOR 8 .

DEFINE BUTTON Btn_Ejecutar 
     LABEL "Ejecutar" 
     SIZE 14 BY 1.38.

DEFINE BUTTON BUTTON-166 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 166" 
     SIZE 14 BY 1.38.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(35)":U INITIAL "000 - Todas las Agencias" 
     LABEL "Agencia" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     LIST-ITEMS "000 - Todas las Agencias" 
     DROP-DOWN-LIST
     SIZE 58 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WAno AS INTEGER FORMAT "9999":U INITIAL 0 
     LABEL "Año" 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WDia AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Día" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WMes AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Mes" 
     VIEW-AS FILL-IN 
     SIZE 4 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE wproc AS INTEGER FORMAT "99":U INITIAL 0 
     LABEL "Procesando la Agencia" 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-290
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 16 BY 6.19.

DEFINE VARIABLE P_Age AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 5 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE P_Nit AS CHARACTER FORMAT "X(14)":U 
     VIEW-AS FILL-IN 
     SIZE 15.29 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE P_NroCre AS INTEGER FORMAT "999999999":U INITIAL 0 
     VIEW-AS FILL-IN 
     SIZE 12 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_Mensaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 32 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Lq
     Cmb_Agencias AT ROW 1.27 COL 10 COLON-ALIGNED
     WDia AT ROW 2.88 COL 10 COLON-ALIGNED
     WMes AT ROW 2.88 COL 19 COLON-ALIGNED
     WAno AT ROW 2.88 COL 29 COLON-ALIGNED
     wproc AT ROW 3.96 COL 36 COLON-ALIGNED
     BUTTON-166 AT ROW 4.77 COL 55
     Btn_Ejecutar AT ROW 6.12 COL 55
     BtnDone AT ROW 8.81 COL 55
     "Asigna Cobros mar/2008" VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 3.15 COL 54 WIDGET-ID 2
          FONT 3
     RECT-290 AT ROW 4.23 COL 54
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1.38
         SIZE 71.29 BY 9.46
         BGCOLOR 17 FONT 5
         DEFAULT-BUTTON BtnDone.

DEFINE FRAME F_Progreso
     P_Age AT ROW 2.04 COL 5 NO-LABEL
     P_NroCre AT ROW 2.04 COL 8 COLON-ALIGNED NO-LABEL
     P_Nit AT ROW 2.04 COL 19.72 COLON-ALIGNED NO-LABEL
     W_Mensaje AT ROW 3.12 COL 3 COLON-ALIGNED NO-LABEL
     "Age" VIEW-AS TEXT
          SIZE 4 BY .62 AT ROW 1.23 COL 6
     "Nro.Credito" VIEW-AS TEXT
          SIZE 12 BY .62 AT ROW 1.23 COL 10.29
     "Nit Cliente" VIEW-AS TEXT
          SIZE 11 BY .62 AT ROW 1.23 COL 22
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 12 ROW 5.85
         SIZE 36.57 BY 4.04
         BGCOLOR 17 
         TITLE "Progreso".


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
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Proceso de Asignacion cobros"
         HEIGHT             = 9.85
         WIDTH              = 71.29
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW wWin
  VISIBLE,,RUN-PERSISTENT                                               */
/* REPARENT FRAME */
ASSIGN FRAME F_Progreso:FRAME = FRAME F_Lq:HANDLE.

/* SETTINGS FOR FRAME F_Lq
   FRAME-NAME                                                           */
/* SETTINGS FOR FILL-IN WAno IN FRAME F_Lq
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WDia IN FRAME F_Lq
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WMes IN FRAME F_Lq
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN wproc IN FRAME F_Lq
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Progreso
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Progreso:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN P_Age IN FRAME F_Progreso
   NO-ENABLE ALIGN-L                                                    */
ASSIGN 
       P_Age:HIDDEN IN FRAME F_Progreso           = TRUE.

/* SETTINGS FOR FILL-IN P_Nit IN FRAME F_Progreso
   NO-ENABLE                                                            */
ASSIGN 
       P_Nit:HIDDEN IN FRAME F_Progreso           = TRUE.

/* SETTINGS FOR FILL-IN P_NroCre IN FRAME F_Progreso
   NO-ENABLE                                                            */
ASSIGN 
       P_NroCre:HIDDEN IN FRAME F_Progreso           = TRUE.

/* SETTINGS FOR FILL-IN W_Mensaje IN FRAME F_Progreso
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Proceso de Asignacion cobros */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Proceso de Asignacion cobros */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME F_Lq /* Salir */
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


&Scoped-define SELF-NAME Btn_Ejecutar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ejecutar wWin
ON CHOOSE OF Btn_Ejecutar IN FRAME F_Lq /* Ejecutar */
ASSIGN FRAME F_Lq Cmb_Agencias WMes WAno.
ASSIGN W_Error  = FALSE.
SESSION:SET-WAIT-STATE("General").
  FIND FIRST Usuarios WHERE Usuarios.Id_CobroJurid EQ 1
                        AND Usuarios.Estado        EQ 1 NO-LOCK NO-ERROR.                      
  IF NOT AVAILABLE(Usuarios) THEN 
     MESSAGE "No se halló Usuario Activo para asignar los Cobros jurídicos," SKIP
               "Serán asignados en las demás instancias." 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ASSIGN W_UsuAbo = Usuarios.Usuario WHEN AVAIL(usuarios).

  FIND FIRST Usuarios WHERE Usuarios.Id_Cartera EQ 1
                        AND Usuarios.Estado     EQ 1 NO-LOCK NO-ERROR.                      
  IF NOT AVAILABLE(Usuarios) THEN 
     MESSAGE "No se halló Usuario Activo para asignar los Cobros por Libranzas," SKIP
               "Serán asignados en las demás instancias." 
           VIEW-AS ALERT-BOX INFO BUTTONS OK.
  ASSIGN W_UsuLib = Usuarios.Usuario WHEN AVAIL(usuarios).
  
  ASSIGN W_CierreMes = NO                                                                
         W_MultDia   = 1  /*Inicia con un día para todos y el dia 30 fin mes*/                                                
         W_Aportes   = 0.                                                                                                     
  
  RUN Temporal_MovInst. 

  RUN Liquidacion NO-ERROR.
  SESSION:SET-WAIT-STATE("").
  IF W_SiProceso THEN 
      MESSAGE "Fin Proceso. Ok" W_siproceso VIEW-AS ALERT-BOX.
  ELSE MESSAGE "Proceso con Errorres, Debe corregirlo y Reprocesar." VIEW-AS ALERT-BOX.

  SESSION:SET-WAIT-STATE("").

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK wWin 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Instancia wWin 
PROCEDURE Asignar_Instancia :
/*
 Julio 8/05 Gaer, Se agregó filtro excepto Libranzas y CobroJurídico.
----------------------------------------------------------------------*/
DEFINE VAR W_OrdSiguiente  LIKE Instancias.Orden_Instancia.

DEFI VAR K AS INTEG FORM "999".
DEFI VAR W_SiIns AS LOG INIT FALSE.

FOR EACH TProIns: DELETE TProIns. END.  

IF /*(Creditos.FOR_pago EQ 2 AND W_UsuLib NE "")    OR */
   (Creditos.Abogado       AND W_UsuAbo NE "") THEN DO: 
   RUN Asigna_Usuario.           
   RETURN.             
END.

IF Creditos.Estado EQ 2 THEN DO:
  /* 1. este ciclo verifica si ya esta creado para esa instancia.*/
  FOR EACH cfg_Instancias WHERE /*busca la instancia del rango de mora */
           Cfg_Instancias.Agencia        EQ Creditos.Agencia AND
           Cfg_Instancias.Tipo_Instancia EQ 2                AND
           Cfg_Instancias.Plazo_Minimo   LE RangoMora        AND
           Cfg_Instancias.Plazo_Maximo   GE RangoMora        AND
           Cfg_Instancias.Monto_Minimo   LE Creditos.Monto   AND
           Cfg_Instancias.Monto_Maximo   GE Creditos.Monto   AND 
           Cfg_Instancias.Estado         EQ 1                AND
           Cfg_Instancias.Usuario        NE W_UsuAbo         AND
           Cfg_Instancias.Usuario        NE W_UsuLib     NO-LOCK:

      ASSIGN W_AsigInst = Cfg_Instancias.Instancia.

      FIND FIRST CopMov_Inst WHERE CopMov_Inst.Nit           EQ Creditos.Nit                 AND
                                   CopMov_Inst.Cuenta        EQ STRING(Creditos.Num_Credito) AND
                                   CopMov_Inst.Estado        EQ NO                           AND
                                   CopMov_Inst.Num_Solicitud EQ Creditos.Num_Solicitud       AND
                                   CopMov_Inst.Instancia     EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
      IF AVAIL(CopMov_Inst) THEN DO:
         W_UsuDes = CopMov_Inst.Usuario.
         
         /*Julio 27/05 GAER, Por que No debe cambiar el usuario
         IF CopMov_Inst.Usuario NE Cfg_Instancias.Usuario THEN DO:
            FIND FIRST Mov_Instancias WHERE ROWID(Mov_Instancias) EQ CopMov_Inst.W_RowidMI NO-ERROR.
            ASSIGN Mov_Instancias.Usuario = Cfg_Instancias.Usuario
                   W_UsuDes = CopMov_Inst.Usuario.
         END.*/
         RETURN.  
      END.
  END.

  /* 2. Si llega aca no esta creada, entonces verifica si es nuevo o esta en otra instancia */
  FIND FIRST CopMov_Inst WHERE CopMov_Inst.Nit           EQ Creditos.Nit                 AND
                               CopMov_Inst.Cuenta        EQ STRING(Creditos.Num_Credito) AND
                               CopMov_Inst.Estado        EQ NO                           AND
                               CopMov_Inst.Num_Solicitud EQ Creditos.Num_Solicitud      
                            NO-LOCK NO-ERROR.
  IF AVAIL(CopMov_Inst) THEN DO:
     W_UsuDes = CopMov_Inst.Usuario.
     FIND FIRST Mov_Instancias WHERE ROWID(Mov_Instancias) EQ CopMov_Inst.W_RowidMI NO-ERROR.
     RUN Instancia_old.  /* Ya está asignado */
     RETURN.
  END.
    
  /*Solo pasa si no se encuentra en ninguna instancia y esta atrasado entonces lo crea en la primera de cobros*/
  FIND FIRST Instancias WHERE                                                                           
       Instancias.Tipo_Instancia EQ 2 AND                                                              
       Instancias.Instancia      EQ W_AsigInst AND                                                       
       Instancias.Tipo_Producto  EQ 2 AND                                                                
       Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.                                                  
  IF AVAILABLE Instancias THEN DO:                                                                       
     W_ord = Instancias.Orden_Instancia.                                                                 
     /*Encuentra los usuarios configurados para las instancias siguientes, y que cumplen los limites*/   
     RUN Instancia_new.                                                                                  
  END.                                                                                                       
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asigna_Usuario wWin 
PROCEDURE Asigna_Usuario :
/*------------------------------------------------------------------------------
  Purpose:   Cobro-Jurídico y Pagos X libranza asigna directamente a los Usuarios 
             que las administran en las instancias corresponbdientes.  
  Notes:     Julio 8/05 GAER    
------------------------------------------------------------------------------*/
 DEFI VAR W_UsuAsig LIKE Usuarios.Usuario.

 IF Creditos.Abogado THEN
    ASSIGN W_UsuAsig = W_UsuAbo.
 ELSE 
    ASSIGN W_UsuAsig = W_UsuLib.

 FOR EACH cfg_Instancias WHERE /*busca la instancia del rango de mora */
           Cfg_Instancias.Agencia        EQ Creditos.Agencia AND
           Cfg_Instancias.Tipo_Instancia EQ 2                      AND
           Cfg_Instancias.Plazo_Minimo   LE Creditos.Dias_Atraso   AND
           Cfg_Instancias.Plazo_Maximo   GE Creditos.Dias_Atraso   AND
           Cfg_Instancias.Monto_Minimo   LE Creditos.Monto AND
           Cfg_Instancias.Monto_Maximo   GE Creditos.Monto AND
           Cfg_Instancias.Estado         EQ 1              AND
           Cfg_Instancias.Usuario        EQ W_UsuAsig NO-LOCK:

      FIND FIRST CopMov_Inst WHERE CopMov_Inst.Nit           EQ Creditos.Nit                 AND
                                   CopMov_Inst.Cuenta        EQ STRING(Creditos.Num_Credito) AND
                                   CopMov_Inst.Estado        EQ NO                           AND
                                   CopMov_Inst.Num_Solicitud EQ Creditos.Num_Solicitud       AND
                                   CopMov_Inst.Instancia     EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
      IF AVAIL(CopMov_Inst) AND CopMov_Inst.Usuario EQ W_UsuAsig THEN DO:
         W_UsuDes = CopMov_Inst.Usuario.      
         RETURN.   /*Retorna por Estar correctamente asignado.*/
      END.

      IF AVAIL(CopMov_Inst) THEN DO: /*De lo contrario si existe, borra la actual.*/
         FIND FIRST Mov_Instancias WHERE ROWID(Mov_Instancias) EQ CopMov_Inst.W_RowidMI NO-ERROR.
         ASSIGN Mov_instancias.Fec_Retiro  = W_fecha
                Mov_instancias.Hora_Retiro = TIME
                Mov_Instancias.Estado      = YES.
      END.

      /*y crea la nueva*/
      CREATE Mov_Instancias.                                             
      ASSIGN Mov_Instancias.Fec_Ingreso   = W_Fecha                      
             Mov_Instancias.Hora_Ingreso  = TIME                         
             Mov_Instancias.Nit           = Creditos.Nit                 
             Mov_Instancias.Num_Solicitud = Creditos.Num_Solicitud       
             Mov_Instancias.Usuario       = W_UsuAsig
             Mov_Instancias.Instancia     = Cfg_Instancias.Instancia         
             Mov_Instancias.Cuenta        = STRING(Creditos.Num_Credito) 
             Mov_Instancias.Agencia       = Cfg_Instancias.Agencia
             W_UsuDes                     = W_UsuAsig.
      RETURN.
  END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Calificar wWin 
PROCEDURE Calificar :
/* ejecuta arrastre de creditos */
FOR EACH creditos WHERE tip_credito < 5 AND sdo_capital > 0 GROUP BY nit :
    FIND FIRST tarrastre WHERE tarrastre.ced = creditos.nit AND tarrastre.tip = creditos.tip_credito NO-ERROR.
    IF AVAILABLE(tarrastre) THEN DO:
       IF tarrastre.arrastre < creditos.categoria THEN
          tarrastre.arrastre = creditos.categoria.
    END.
    ELSE DO:
        CREATE tarrastre.
        ASSIGN tarrastre.ced = creditos.nit
               tarrastre.tip = creditos.tip_credito
               tarrastre.arrastre = creditos.categoria
               tarrastre.ncre     = creditos.num_credito.
    END.
END.

/* actualiza arrastre en creditos */
FOR EACH creditos WHERE tip_credito < 5 AND sdo_capital GT 0 BY nit:
    FIND FIRST tarrastre WHERE tarrastre.ced = creditos.nit AND tarrastre.tip = creditos.tip_credito NO-ERROR.
    IF AVAILABLE(tarrastre) THEN DO:
       creditos.categoriames = tarrastre.arrastre.
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI wWin  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
  THEN DELETE WIDGET wWin.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI wWin  _DEFAULT-ENABLE
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
  DISPLAY Cmb_Agencias WDia WMes WAno wproc 
      WITH FRAME F_Lq IN WINDOW wWin.
  ENABLE RECT-290 Cmb_Agencias BUTTON-166 Btn_Ejecutar BtnDone 
      WITH FRAME F_Lq IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Lq}
  DISPLAY P_Age P_NroCre P_Nit W_Mensaje 
      WITH FRAME F_Progreso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Progreso}
  VIEW wWin.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject wWin 
PROCEDURE exitObject :
/*------------------------------------------------------------------------------
  Purpose:  Window-specific override of this procedure which destroys 
            its contents and itself.
    Notes:  
------------------------------------------------------------------------------*/

  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
DEFINE VAR i AS INTEGER INITIAL 1.
/*RUN SUPER.*/
  W_FechaT = W_fecha.

  /* Code placed here will execute AFTER standard behavior.    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Instancia_new wWin 
PROCEDURE Instancia_new :
/*Julio 8/05 Gaer, Se agregó filtro excepto Libranzas y CobroJurídico.
---------------------------------------------------------------------------*/      
     FOR EACH Cfg_Instancias WHERE 
         Cfg_Instancias.Agencia        EQ Creditos.Agencia AND
         Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia AND
         Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
         Cfg_Instancias.Plazo_Minimo   LE RangoMora AND
         Cfg_Instancias.Plazo_Maximo   GE RangoMora AND
         Cfg_Instancias.Monto_Minimo   LE Creditos.Monto AND
         Cfg_Instancias.Monto_Maximo   GE Creditos.Monto AND 
         Cfg_Instancias.Estado         EQ 1              AND
         Cfg_Instancias.Usuario        NE W_UsuLib       AND
         Cfg_Instancias.Usuario        NE W_UsuAbo   NO-LOCK:
            CREATE TProIns.
            ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                   TProIns.TP_Instancia = Cfg_Instancias.Instancia
                   TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                   TProIns.Tp_Cantidad  = 0
                   TProIns.TP_Agencia   = Cfg_Instancias.Agencia.

            FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
            IF AVAILABLE Instancias THEN TProIns.TP_NomInstan = Instancias.Nom_Instancia.
            ELSE TProIns.TP_NomInstan = "Instancia No encontrada".

            FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
            IF AVAILABLE Usuarios THEN 
               TProIns.TP_NomUsuar  = Usuario.Nombre.
            ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".
       END.

       FIND FIRST TProIns NO-ERROR.
       IF NOT AVAILABLE TProIns THEN DO:
          /*MESSAGE "No se encontro ningún Usuario" SKIP
                  "Para asignar la solicitud a la" SKIP
                  "Proxima instancia." VIEW-AS ALERT-BOX.*/
          RETURN ERROR.
       END.
       /*Por cada usuario encuentra el numero de creditos que esta procesando para las instancias a seguir*/
       /*esto con el fin de escoger el que menos Creditos este procesando y distribuirlos equitativamente*/
       FOR EACH TproIns:
           FOR EACH Mov_Instancias WHERE
                    Mov_Instancias.Usuario   EQ TProIns.TP_Usuario AND
                    Mov_instancias.Instancia EQ TproIns.TP_Instancia AND
                    Mov_Instancias.Estado    EQ NO NO-LOCK:
                    TproIns.TP_Cantidad = TproIns.TP_Cantidad + 1.
           END.
       END.

       FOR EACH TProIns BREAK BY TproIns.TP_Instancia BY TproIns.TP_Cantidad:
           IF FIRST-OF(TProIns.TP_Instancia) AND FIRST-OF(TProIns.TP_Cantidad) THEN DO:
              /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
              CREATE Hoja_Vida.
              ASSIGN Hoja_Vida.Tipo        = 9 
                     Hoja_Vida.Codigo      = 2  
                     Hoja_Vida.Instancia   = TProIns.TP_Instancia
                     Hoja_Vida.DoctoRefer  = Creditos.Num_Credito
                     Hoja_Vida.Nit         = Creditos.Nit
                     Hoja_Vida.Usuario     = TProIns.TP_Usuario
                     Hoja_Vida.Fec_Grabacion = TODAY
                     Hoja_Vida.Hora_Grabacion = TIME
                     Hoja_Vida.Asunto_Cumplido = YES.
                     Hoja_Vida.Observacion = "Entra a Instancia: " + STRING(TProIns.TP_Instancia) +
                        " - " + TProIns.TP_NomInstan +
                        " - Usuario Responsable: " + TProIns.TP_Usuario + " - " + TProIns.TP_NomUsuar.
              CREATE Mov_Instancias.
              ASSIGN Mov_Instancias.Fec_Ingreso   = W_Fecha
                     Mov_Instancias.Hora_Ingreso  = TIME
                     Mov_Instancias.Nit           = Creditos.Nit
                     Mov_Instancias.Num_Solicitud = Creditos.Num_Solicitud
                     Mov_Instancias.Usuario       = TProIns.TP_Usuario
                     Mov_Instancias.Instancia     = TProIns.TP_Instancia
                     Mov_Instancias.Cuenta        = STRING(Creditos.Num_Credito)
                     Mov_Instancias.Agencia       = TProIns.TP_Agencia
                     W_UsuDes                     = TProIns.TP_Usuario.
           END.
     END. /**/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Instancia_old wWin 
PROCEDURE Instancia_old :
/* Cierra instancia */
ASSIGN Mov_instancias.fec_retiro  = W_fecha
       Mov_instancias.Hora_retiro = TIME
       Mov_Instancias.Estado      = YES.
/*Cambio Alexander Moncada 18/01/2005*/
FIND Instancias WHERE 
     Instancias.Tipo_Instancia EQ 2 AND
     Instancias.Instancia      EQ W_AsigInst AND
     Instancias.Tipo_Producto  EQ 2 AND
     Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.

RELEASE Mov_Instancias.

IF AVAILABLE Instancias THEN DO:
   W_ord = Instancias.Orden_Instancia.
   RUN Instancia_new.
   /*FIND NEXT Instancias WHERE
             Instancias.Tipo_Instancia  EQ 2 AND
             Instancias.Orden_Instancia GT W_Ord AND
             Instancias.Tipo_Producto   EQ 2 AND
             Instancias.Estado          EQ 1 USE-INDEX idx_orden NO-LOCK NO-ERROR.
   IF AVAILABLE Instancias THEN RUN Instancia_new.*/
END. 
/*fin cambio*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Liquidacion wWin 
PROCEDURE Liquidacion :
DEFINE VARIABLE I AS INTEGER.
MESSAGE "Aplica arrastre " VIEW-AS ALERT-BOX.
RUN calificar.  /* aplica arrastre */
MESSAGE "Asigna instancia " VIEW-AS ALERT-BOX.
DO i = 1 TO 60:
  FOR EACH Creditos WHERE creditos.agencia = i and
                          Creditos.Estado    eq 2 AND
                          Creditos.Tip_Credito LT 5  
                        BREAK BY Creditos.Nit BY creditos.categoriames DESC:
     RangoMora = 0.
     IF Creditos.Sdo_Capital LE 0 THEN NEXT.
     /*IF FIRST-OF(Creditos.categoriames) THEN DO:*/
     IF Creditos.categoriames eq "A" THEN RangoMora = 30.
     IF Creditos.categoriames eq "B" THEN RangoMora = 60.
     IF Creditos.categoriames eq "C" THEN RangoMora = 90.
     IF Creditos.categoriames eq "D" THEN RangoMora = 120.
     IF Creditos.categoriames eq "E" THEN RangoMora = 180.
     IF Creditos.Dias_Atraso GT 0 OR Creditos.Abogado THEN DO:
        RUN Asignar_Instancia NO-ERROR.
     END.
     ELSE DO:  /* BORRA INSTANCIA ACTUAL */
              FIND FIRST CopMov_Inst WHERE CopMov_Inst.Nit           EQ Creditos.Nit                 AND
                                           CopMov_Inst.Cuenta        EQ STRING(Creditos.Num_Credito) AND
                                           CopMov_Inst.Estado        EQ NO                           AND
                                           CopMov_Inst.Num_Solicitud EQ Creditos.Num_Solicitud  NO-LOCK NO-ERROR.
              IF AVAIL(CopMov_Inst) THEN DO:
                 FIND FIRST Mov_Instancias WHERE ROWID(Mov_Instancias) EQ CopMov_Inst.W_RowidMI NO-ERROR.
                 ASSIGN Mov_instancias.Fec_Retiro  = W_fecha
                     Mov_instancias.Hora_Retiro = TIME
                     Mov_Instancias.Estado      = YES.
              END.
     END.
     ASSIGN P_Age:SCREEN-VALUE IN FRAME F_Progreso     = STRING(Creditos.Agencia,"999")
            P_NroCre:SCREEN-VALUE IN FRAME F_Progreso  = STRING(Creditos.Num_Credito,"999999999")
            P_Nit:SCREEN-VALUE IN FRAME F_Progreso     = Creditos.Nit
            W_Mensaje:SCREEN-VALUE IN FRAME F_Progreso = "Asignando Cartera"
            W_IntMora                                  = 0.
  END.
END. /*end del do*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Temporal_MovInst wWin 
PROCEDURE Temporal_MovInst :
/*------------------------------------------------------------------------------
  Purpose:  Crea temporal con Mov_instancias para recorrerlas con indices OK.   
  Marzo 9/05 GAER.
------------------------------------------------------------------------------*/
  DEFI VAR J AS INTEG FORM "999".

  VIEW FRAME F_Progreso.

  FIND FIRST CopMov_Inst NO-LOCK NO-ERROR.
  IF AVAIL(CopMov_Inst) THEN
     RETURN.

  W_Mensaje:SCREEN-VALUE IN FRAME F_Progreso = "Generando Temporal Mov-Inst.".

  FOR EACH Mov_Instancias WHERE (mov_instancia.instancia GE 100 AND 
                                 mov_instancia.instancia LE 140) OR 
                                 mov_instancia.instancia = 920:
      DELETE mov_instancia.
      /*CREATE CopMov_Inst.
      BUFFER-COPY Mov_Instancias TO CopMov_Inst.
      ASSIGN CopMov_Inst.W_RowidMI = ROWID(Mov_Instancias).*/
  END.
     
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

