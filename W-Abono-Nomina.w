&ANALYZE-SUSPEND _VERSION-NUMBER AB_v9r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME WImportar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS WImportar 

CREATE WIDGET-POOL.

/* ***************************  Definitions  ************************** */
{Incluido/Variable.I "SHARED"}

DEFINE VAR W_Ok AS LOGICAL.
DEFINE NEW SHARED VAR j AS DECIMAL.
DEFINE NEW SHARED VAR E AS DECIMAL.

DEFINE TEMP-TABLE Tmp_Abono
    FIELD Nit AS CHARACTER
    FIELD Valor AS CHARACTER
    FIELD Tipo AS INTEGER.

DEFINE TEMP-TABLE Tmp_totales

    /* oakley */

  FIELD Agencia            LIKE clientes.agencia
  FIELD saldo              LIKE ahorros.sdo_disponible
  FIELD cod_producto       LIKE Ahorros.Cod_ahorro.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of first Frame and/or Browse and/or first Query                 */
&Scoped-define FRAME-NAME F_Importar

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS EErrores txt_sucursales Btn_Importar ~
Btn_Salir RECT-302 
&Scoped-Define DISPLAYED-OBJECTS EErrores txt_sucursales RI 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR WImportar AS WIDGET-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Errores 
     LABEL "Ver Errores >>" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_Importar 
     LABEL "Importar Archivo abono" 
     SIZE 23 BY 1.08.

DEFINE BUTTON btn_procesar 
     LABEL "Procesar" 
     SIZE 17 BY 1.08.

DEFINE BUTTON Btn_Salir 
     LABEL "Salir" 
     SIZE 15 BY 1.65.

DEFINE VARIABLE EErrores AS CHARACTER 
     VIEW-AS EDITOR NO-WORD-WRAP SCROLLBAR-HORIZONTAL SCROLLBAR-VERTICAL LARGE
     SIZE 51 BY 11.31
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE VARIABLE RI AS DECIMAL FORMAT ">>>,>>>,>>9":U INITIAL 0 
     LABEL "Reg.Importados" 
     VIEW-AS FILL-IN 
     SIZE 17 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE txt_sucursales AS CHARACTER FORMAT "X(256)":U 
     LABEL "Contrapartida  Sucursales y Agencia" 
     VIEW-AS FILL-IN 
     SIZE 17 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-302
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL 
     SIZE 53 BY 4.31.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_Importar
     EErrores AT ROW 2.35 COL 57 NO-LABEL
     txt_sucursales AT ROW 2.88 COL 34 COLON-ALIGNED
     Btn_Importar AT ROW 3.96 COL 14
     RI AT ROW 5.31 COL 34 COLON-ALIGNED
     btn_procesar AT ROW 6.92 COL 17
     Btn_Errores AT ROW 6.92 COL 42
     Btn_Salir AT ROW 8.54 COL 42
     RECT-302 AT ROW 2.35 COL 2
     "Ingrese las cuentas contable para el abono" VIEW-AS TEXT
          SIZE 39.43 BY .88 AT ROW 1.27 COL 4.57
          FGCOLOR 7 
     "Listado de errores ocurridos durante la importación" VIEW-AS TEXT
          SIZE 48 BY .88 AT ROW 1.27 COL 58
          FGCOLOR 7 
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 109.14 BY 13.15
         BGCOLOR 17 FONT 5.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW WImportar ASSIGN
         HIDDEN             = YES
         TITLE              = "Abono Nomina"
         HEIGHT             = 9.42
         WIDTH              = 56
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.14
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.14
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = no
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.
/* END WINDOW DEFINITION                                                */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB WImportar 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR WINDOW WImportar
  VISIBLE,,RUN-PERSISTENT                                               */
/* SETTINGS FOR FRAME F_Importar
                                                                        */
/* SETTINGS FOR BUTTON Btn_Errores IN FRAME F_Importar
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON btn_procesar IN FRAME F_Importar
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN RI IN FRAME F_Importar
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WImportar)
THEN WImportar:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME WImportar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WImportar WImportar
ON END-ERROR OF WImportar /* Abono Nomina */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL WImportar WImportar
ON WINDOW-CLOSE OF WImportar /* Abono Nomina */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Errores
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Errores WImportar
ON CHOOSE OF Btn_Errores IN FRAME F_Importar /* Ver Errores >> */
DO:
  IF SELF:LABEL EQ "Ver Errores >>" THEN DO:
    WImportar:WIDTH = 109.14.
    SELF:LABEL = "<< Ocultar".
  END.
  ELSE DO:
    WImportar:WIDTH = 54.
    SELF:LABEL = "Ver Errores >>".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Importar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Importar WImportar
ON CHOOSE OF Btn_Importar IN FRAME F_Importar /* Importar Archivo abono */
DO:
DEFINE VAR j AS DECIMAL FORMAT ">>>,>>,>>9".
DEFINE VAR W_Pathspl LIKE Entidad.Dir_Spl.
DEFINE VAR OkPressed AS LOGICAL.
DEFINE VAR Archivo AS CHARACTER FORMAT "X(50)".

  
  ASSIGN rI:SCREEN-VALUE = "0".
  DISABLE Btn_Errores WITH FRAME F_Importar.
  IF txt_sucursales:SCREEN-VALUE EQ ? THEN DO:
     MESSAGE "Se debe ingresar la cuenta contrapartida de la Cuenta Sucursales y Agencia" 
     VIEW-AS ALERT-BOX ERROR.
     APPLY "entry" TO Btn_Importar.
     RETURN NO-APPLY.
  END.


  SYSTEM-DIALOG GET-FILE Archivo
         TITLE      "Escoja el archivo de nomina ..."
         INITIAL-DIR W_Pathspl
         FILTERS    "Archivos Texto (*.txt)"   "*.txt",
                    "Fuente de Datos (*.d)"   "*.d"
         MUST-EXIST
         USE-FILENAME
         UPDATE OKpressed.
  INPUT FROM VALUE(Archivo).
  IF OkPressed AND Archivo NE "" THEN DO:
     RUN _SetCurs.r ("WAIT").
     FOR EACH Tmp_Abono:
          DELETE Tmp_Abono.
     END.
     ASSIGN J = 0. /* E = 0.*/
     OS-DELETE VALUE(W_Pathspl + "\errores.e").
     OUTPUT TO VALUE(W_Pathspl + "\errores.e").
     REPEAT:
        j = j + 1.
        CREATE tmp_abono. 
        IMPORT tmp_abono NO-ERROR.
        IF ERROR-STATUS:ERROR THEN DO:
           DISPLAY "Error en Registro: " j WITH NO-LABELS.
        END.
      OUTPUT CLOSE.
      RUN _SetCurs.r ("ARROW").
     END.
  END.
  INPUT CLOSE.
  W_ok = Eerrores:READ-FILE(W_Pathspl + "\errores.e") IN FRAME F_Importar.   
  IF Eerrores:SCREEN-VALUE NE "" THEN DO:
     ENABLE Btn_Errores WITH FRAME F_Importar.
  END.
  ELSE
     ENABLE btn_procesar WITH FRAME F_Importar.
  ASSIGN Ri:SCREEN-VALUE = STRING(j).
  MESSAGE "Proceso Culminado" VIEW-AS ALERT-BOX.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME btn_procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL btn_procesar WImportar
ON CHOOSE OF btn_procesar IN FRAME F_Importar /* Procesar */
DO :
DEFINE VAR W_NumSeq AS INTEGER.
DEFINE VAR W_GASTO AS DECIMAL.
DEFINE VAR w_operacion AS INTEGER.
DEFINE VAR w_comprobante LIKE comprobantes.comprobante.

  W_NumSeq = 20012005.
  W_Operacion = 010101001. 
  w_comprobante = 3.


  FOR EACH tmp_abono:
      FIND clientes WHERE cliente.nit = TMP_ABONO.NIT NO-ERROR.
      IF AVAILABLE clientes  THEN DO:
          
              FIND TMP_TOTALES WHERE tmp_totales.agencia = clientes.agencia AND 
                                     tmp_totales.cod_producto = tmp_abono.tipo NO-ERROR.
              IF AVAILABLE tmp_totales THEN DO:
                 TMP_TOTALES.saldo = TMP_TOTALES.saldo + Round(decimal(tmp_abono.valor),0).
              END.
              ELSE DO:
                  CREATE TMP_TOTALES.
                  ASSIGN TMP_TOTALES.Agencia = clientes.agencia
                         TMP_TOTALES.cod_producto = tmp_abono.tipo
                         TMP_TOTALES.saldo = Round(decimal(tmp_abono.valor),0).

              END.
              FIND FIRST AHORROS WHERE AHORROS.NIT = TMP_ABONO.NIT AND ahorros.cod_ahorro = integer(TMP_ABONO.tipo) AND Ahorros.Tip_ahorro = 1 NO-ERROR.
              IF AVAILABLE AHORROS THEN DO:
                 /*registro en mov_ahorros*/
                 CREATE Mov_Ahorros.
                        ASSIGN Mov_Ahorros.Cod_Operacion =  w_operacion
                                Mov_ahorros.cod_ahorro    = Ahorros.Cod_Ahorro
                                Mov_Ahorros.Cue_Ahorros   = Ahorros.Cue_Ahorros
                                Mov_ahorros.nit           = Ahorros.Nit
                                Mov_Ahorros.Fecha         = TODAY
                                Mov_Ahorros.Hora          = TIME
                                Mov_Ahorros.Cpte          = w_comprobante
                                Mov_Ahorros.Num_Documento = STRING(W_NumSeq)
                                Mov_Ahorros.Agencia       = Ahorros.Agencia
                                Mov_Ahorros.Age_Fuente    = clientes.agencia
                                Mov_Ahorros.Age_Destino   = clientes.agencia
                                Mov_Ahorros.Usuario       = W_Usuario
                                Mov_Ahorros.Val_Efectivo  = Round(decimal(tmp_abono.valor),0)
                                Mov_Ahorros.Descrip       = "Abono nomina".
  
                 /*INGRESA LE VALOR Al sdo disponible*/
                 Ahorros.Sdo_Disponible = Ahorros.Sdo_Disponible + decimal(tmp_abono.VALOR).
              END.
              ELSE DO:
                  MESSAGE "No se encontro cuenta de ahorro para el nit  = " + TMP_ABONO.NIT
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  APPLY "entry" TO Btn_Importar.
                  RETURN NO-APPLY.
              END.
      END.
      ELSE DO:
          MESSAGE "CLIENTE CON NIT = " + TMP_ABONO.NIT + " NO EXISTE"
              VIEW-AS ALERT-BOX INFO BUTTONS OK.
          APPLY "entry" TO Btn_Importar.
          RETURN NO-APPLY.
      END.
  END.
  
  FOR EACH tmp_totales :
      FIND Cortolargo WHERE CortoLargo.Agencia = tmp_totales.agencia and
                            CortoLargo.Clase_Producto EQ 1 AND
                            CortoLargo.Cod_Producto = tmp_totales.cod_producto NO-ERROR.
      IF  AVAILABLE CORTOlARGO  THEN DO:
              w_comprobante = CortoLargo.Comprobante. 
              /*buscamos el numero de consecutivo */
              FIND comprobantes WHERE Comprobantes.agencia     = CortoLargo.Agencia AND 
                                      Comprobantes.comprobante = CortoLargo.Comprobante NO-ERROR.
              IF AVAILABLE(comprobantes) THEN DO:
                  ASSIGN W_NumSeq = COMPROBANTES.SECUENCIA + 1
                         COMPROBANTES.SECUENCIA = COMPROBANTES.SECUENCIA + 1.
              END.
              ELSE DO:
                  MESSAGE "Error al buscar comprobante."
                      VIEW-AS ALERT-BOX INFO BUTTONS OK.
                  APPLY "entry" TO Btn_Importar.
                  RETURN NO-APPLY.
              END.
              /*creamos las cuentas de ahorros para las agencias*/
              CREATE Mov_Contable.
              ASSIGN Mov_Contable.Agencia = tmp_totales.agencia
                     Mov_Contable.Comprobante = w_comprobante
                     Mov_Contable.Cuenta = CortoLargo.Cta_AsoAd
                     Mov_Contable.Fec_Contable = W_Fecha
                     Mov_Contable.Comentario = "Abono Nomina"
                     Mov_Contable.Usuario = W_Usuario
                     Mov_contable.Nit = string(tmp_totales.agencia)
                     Mov_Contable.Cen_Costos = 999
                     Mov_Contable.Destino = tmp_totales.agencia
                     Mov_Contable.Num_Documento = W_NumSeq
                     Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
                     Mov_Contable.Fec_Grabacion = W_fecha
                     Mov_Contable.Hora = TIME
                     Mov_Contable.Estacion = W_Estacion
                     Mov_Contable.CR = Round(decimal(tmp_totales.saldo),0) NO-ERROR.

              /*creamos las cuentas de ahorros para las agencias*/
              CREATE Mov_Contable.
              ASSIGN Mov_Contable.Agencia = tmp_totales.agencia
                     Mov_Contable.Comprobante = w_comprobante
                     Mov_Contable.Cuenta = CortoLargo.Cta_SYA
                     Mov_Contable.Fec_Contable = W_Fecha
                     Mov_Contable.Comentario = "Abono Nomina"
                     Mov_Contable.Usuario = W_Usuario
                     Mov_contable.Nit = string(W_agencia)
                     Mov_Contable.Cen_Costos = 999
                     Mov_Contable.Destino = tmp_totales.agencia
                     Mov_Contable.Num_Documento = W_NumSeq
                     Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
                     Mov_Contable.Fec_Grabacion = TODAY
                     Mov_Contable.Hora = TIME
                     Mov_Contable.Estacion = W_Estacion
                     Mov_Contable.DB = Round(decimal(tmp_totales.saldo),0) NO-ERROR.

              /*cuadramos la cunta sucursales y agencia de la administrativa*/
              CREATE Mov_Contable.
              ASSIGN Mov_Contable.Agencia = w_agencia
                     Mov_Contable.Comprobante = w_comprobante
                     Mov_Contable.Cuenta = CortoLargo.Cta_SYA
                     Mov_Contable.Fec_Contable = W_Fecha
                     Mov_Contable.Comentario = "Abono Nomina"
                     Mov_Contable.Usuario = W_Usuario
                     Mov_contable.Nit = string(tmp_totales.agencia)
                     Mov_Contable.Cen_Costos = 999
                     Mov_Contable.Destino = w_agencia
                     Mov_Contable.Num_Documento = W_NumSeq
                     Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
                     Mov_Contable.Fec_Grabacion = w_fecha
                     Mov_Contable.Hora = TIME
                     Mov_Contable.Estacion = W_Estacion
                     Mov_Contable.CR = Round(decimal(tmp_totales.saldo),0) NO-ERROR.

              /*cONTRAPARTIDA GASTO*/
              CREATE Mov_Contable.
              ASSIGN Mov_Contable.Agencia = w_agencia
                     Mov_Contable.Comprobante = w_comprobante
                     Mov_Contable.Cuenta = txt_sucursales:SCREEN-VALUE
                     Mov_Contable.Fec_Contable = W_Fecha
                     Mov_Contable.Comentario = "Abono Nomina"
                     Mov_Contable.Usuario = W_Usuario
                     Mov_contable.Nit = string(w_agencia)
                     Mov_Contable.Cen_Costos = 999
                     Mov_Contable.Destino = w_agencia
                     Mov_Contable.Num_Documento = W_NumSeq
                     Mov_Contable.Doc_Referencia = STRING(W_NumSeq)
                     Mov_Contable.Fec_Grabacion = w_fecha
                     Mov_Contable.Hora = time
                     Mov_Contable.Estacion = W_Estacion
                     Mov_Contable.db = Round(decimal(tmp_totales.saldo),0) NO-ERROR.
          END.
          ELSE DO:
              MESSAGE "Error de configuracion de cuentas contables"
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.

              APPLY "entry" TO Btn_Importar.
              RETURN NO-APPLY.
          END.
      END.
  END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Salir
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir WImportar
ON CHOOSE OF Btn_Salir IN FRAME F_Importar /* Salir */
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


&UNDEFINE SELF-NAME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK WImportar 


/* ***************************  Main Block  *************************** */

/* Include custom  Main Block code for SmartWindows. */
{src/adm2/windowmn.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects WImportar  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE disable_UI WImportar  _DEFAULT-DISABLE
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
  IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(WImportar)
  THEN DELETE WIDGET WImportar.
  IF THIS-PROCEDURE:PERSISTENT THEN DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE enable_UI WImportar  _DEFAULT-ENABLE
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
  DISPLAY EErrores txt_sucursales RI 
      WITH FRAME F_Importar IN WINDOW WImportar.
  ENABLE EErrores txt_sucursales Btn_Importar Btn_Salir RECT-302 
      WITH FRAME F_Importar IN WINDOW WImportar.
  {&OPEN-BROWSERS-IN-QUERY-F_Importar}
  VIEW WImportar.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE exitObject WImportar 
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

