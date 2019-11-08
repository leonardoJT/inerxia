&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
/* Connected Databases 
          bdcentral        PROGRESS
*/
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------
  File: W-Proceso_saro.W
  Description: from cntnrwin.w - ADM SmartWindow Template
  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/
CREATE WIDGET-POOL.
{src/adm2/widgetprto.i}

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.i "SHARED"}

DEF VAR W_VigIns       AS INT FORMAT "9999" NO-UNDO.
DEF VAR W_NvaHV        AS LOG NO-UNDO.
DEF VAR W_Nuevo        AS LOG NO-UNDO.
DEF VAR W_Primera      AS INT FORMAT '99999'  NO-UNDO.
DEF VAR W_Negadas      AS INT FORMAT '99999'  NO-UNDO.
DEF VAR W_Ultima       AS INT FORMAT '99999'  NO-UNDO.
DEF VAR W_TipoInforme  AS CHAR FORMAT "X(10)" NO-UNDO.
DEF VAR W_SucAgen      AS LOG NO-UNDO.
DEF VAR W_Ok           AS LOG NO-UNDO.
DEF VAR W_SiMInst      AS LOG INIT FALSE NO-UNDO.  

DEF VAR Id_Agregar     AS CHAR FORMAT "X(2)" NO-UNDO.
DEF VAR Puntero        AS ROWID NO-UNDO.
DEF VAR choice         AS LOG NO-UNDO.
DEF VAR Aprueba        AS LOG INITIAL NO NO-UNDO.
DEF VAR Orden_InsPan   AS INT FORMAT '999' NO-UNDO.
DEF VAR Longitud       AS DEC NO-UNDO.
DEF VAR Dias           AS DEC NO-UNDO.
DEF VAR ImpNegada      AS LOG INITIAL NO NO-UNDO.

DEF VAR P_Nit          AS CHAR NO-UNDO. 
DEF VAR p_Nombre       AS CHAR NO-UNDO. 
DEF VAR P_Apellido     AS CHAR NO-UNDO.  
DEF VAR P_AgeCli       AS CHAR NO-UNDO.  

DEF VAR TP_Instancia   AS INT INITIAL 0 NO-UNDO.
DEF VAR TP_InsProducto AS INT INITIAL 0 NO-UNDO.

DEF VAR v-cod-nivel    AS INT NO-UNDO.
DEF VAR v-cod-FactorO  AS INT NO-UNDO.
DEF VAR v-cod-Factor   AS INT NO-UNDO.
DEF VAR i              AS INT.

/*TABLA TEMPORAL DE LA CONSULTA DELBROWS */
DEF TEMP-TABLE Consulta NO-UNDO
      FIELD Num_Referencia  AS INT
      FIELD AgeSaro         AS INT
      FIELD Nit             AS CHAR
      FIELD Estado          AS INT
      FIELD Nombre          AS CHAR FORMAT "X(40)"
      FIELD Fec_Ingreso     AS DATE 
      FIELD Hor_Ingreso     AS CHAR FORMAT "X(15)"
      FIELD Orden           AS INT
      FIELD Monto           AS DEC 
      FIELD Vigencia        AS INT FORMAT "9999"
      FIELD Usuario         LIKE Clientes.Usuario.

/*Tablas temporales manejo de las instancias*/
DEF TEMP-TABLE TProIns  NO-UNDO
    FIELD TP_Agencia   LIKE Agencias.Agencia
    FIELD TP_Orden     LIKE Instancias.Orden_Instancia
    FIELD TP_Instancia LIKE Instancias.Instancia
    FIELD TP_NomInstan AS CHAR FORMAT "X(30)"
    FIELD TP_Usuario   LIKE Usuarios.Usuario
    FIELD TP_NomUsuar  AS CHAR FORMAT "X(30)"
    FIELD TP_Cantidad  AS INT FORMAT "999"
    FIELD TP_Abogado   LIKE Instancias.Id_Abogado.
  
  /*Guardar las instancias de una solicitud*/
  DEF TEMP-TABLE TCerradas NO-UNDO 
    FIELD Instancia        AS INT
    FIELD INom_Instancia   AS CHAR FORMAT "X(20)"
    FIELD Fec_Ingreso      AS DATE 
    FIELD Fec_Retiro       AS DATE 
    FIELD Hora_Ingreso     AS INT 
    FIELD Hora_Retiro      AS INT
    FIELD Estado           AS LOG
    FIELD Num_Solicitud    AS INT
    FIELD Usuario          AS CHAR FORMAT "X(12)"
    FIELD INom_Usuario     AS CHAR FORMAT "X(30)"
    FIELD Descripcion      AS CHAR FORMAT "x(500)".



/*Tabals temporales reportes */
DEF TEMP-TABLE TmpI NO-UNDO
    FIELD ILinea AS INT FORMAT "99"
    FIELD ITexto AS CHAR FORMAT "X(125)".

DEF TEMP-TABLE CTmpI NO-UNDO
    FIELD ILinea AS INT FORMAT "99"
    FIELD ITexto AS CHAR FORMAT "X(50)"
    FIELD Ingr   LIKE Clientes.Ing_Otros 
    FIELD Egr    LIKE Clientes.Ing_Otros
    FIELD Puntos LIKE Clientes.Puntaje
    FIELD Endeu  LIKE Clientes.Capacidad_Pago.

DEF TEMP-TABLE CCTmpI NO-UNDO
    FIELD ILinea AS INT FORMAT "99"
    FIELD ITexto AS CHAR FORMAT "X(20)"
    FIELD Deud   AS CHAR FORMAT "X(20)" 
    FIELD Cod1   AS CHAR FORMAT "X(20)"
    FIELD Cod2   AS CHAR FORMAT "X(20)"
    FIELD Cod3   AS CHAR FORMAT "X(20)".

/*Contiene los usuarios por instancia*/  
  DEF TEMP-TABLE TUXI NO-UNDO
    FIELD Instanc  LIKE Mov_Instancias.Instancia
    FIELD Agencia  LIKE Usuarios.Agencia
    FIELD Usuario  LIKE Usuarios.Usuario
    FIELD Nombre   LIKE Usuarios.Nombre
    FIELD Cantidad AS INT FORMAT "999"
    FIELD Proceso  AS LOG.



/*
/*------------------------------------------------------------------------
  File: W-Proceso_saro.W
  Description: from cntnrwin.w - ADM SmartWindow Template
  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

CREATE WIDGET-POOL.

DEF BUFFER TEmpresas FOR Empresas.
DEF BUFFER TClientes FOR Clientes.

/* ***************************  Definitions  ************************** */
{INCLUIDO\VARIABLE.i "SHARED"}

  DEF VAR WSeguroCartera LIKE Ahorros.Sdo_Disponible NO-UNDO.
  DEF VAR WAjusteAhorros LIKE Ahorros.Sdo_Disponible NO-UNDO.
  DEF VAR ImpNegada      AS LOG INITIAL NO NO-UNDO.
  DEF VAR choice         AS LOG NO-UNDO.
  DEF VAR W_NvaAdm       AS LOG NO-UNDO.
  DEF VAR W_NvaHV        AS LOG NO-UNDO.
  DEF VAR W_NvoCD        AS LOG NO-UNDO.
  DEF VAR W_Pdt          AS CHAR FORMAT "X(80)" NO-UNDO.
  DEF VAR Puntero        AS ROWID NO-UNDO.
  DEF VAR Longitud       AS DEC NO-UNDO.
  DEF VAR W_Ultima       AS INT FORMAT '99999' NO-UNDO.
  DEF VAR W_Primera      AS INT FORMAT '99999' NO-UNDO.
  DEF VAR W_Negadas      AS INT FORMAT '99999' NO-UNDO.
  DEF VAR W_SucAgen      LIKE Usuarios.Id_OpeOfi.
  DEF VAR Id_Agregar     AS CHAR FORMAT "X(2)" NO-UNDO.
  DEF VAR W_TipoInforme  AS CHAR FORMAT "X(10)" NO-UNDO.
  DEF VAR Orden_InsPan   AS INT NO-UNDO FORMAT '999'.
  DEF VAR W_VigIns       AS INT FORMAT "9999".
  DEF VAR Wk_PerPagEmp   AS INT FORMAT "9" INITIAL 0.
  DEF VAR Aprueba        AS LOG INITIAL NO.
  DEF VAR TP_Instancia   AS INT INITIAL 0 NO-UNDO.
  DEF VAR TP_InsProducto AS INT INITIAL 0 NO-UNDO.

  /*utilizada para controlar la grabacion del creditos en la verificacion de garantias*/
  DEF VAR control_grabar  AS LOG.
  DEF VAR W_SiMInst       AS LOG INIT FALSE.
  DEF VAR W_RowIdTx       AS ROWID. 
  

/*para buscar un cliente*/
  DEF VAR P_Nit         LIKE Clientes.Nit.
  DEF VAR p_Nombre      LIKE Clientes.Nombre.
  DEF VAR P_Apellido    LIKE Clientes.Apellido1.
  DEF VAR P_AgeCli      LIKE Clientes.Agencia.
  DEF VAR W_Nuevo       AS LOG.
  
  DEF VAR W_Contenido     AS CHAR FORMAT "X(400)".
  DEF VAR W_NroCodeu      AS INTEG FORM "9".
  DEF VAR W_NBien         LIKE Garantias.Identificacion_Bien.
  DEF VAR RowidGar_Global AS ROWID.

  DEF TEMP-TABLE TCred_ACanc NO-UNDO
    FIELD Agen    LIKE Creditos.Agencia 
    FIELD Pto     LIKE Creditos.Cod_Credito 
    FIELD NumC    LIKE Creditos.Num_Credito 
    FIELD FecD    LIKE Creditos.Fec_Desembolso 
    FIELD SdoC    LIKE Creditos.Sdo_Capital 
    FIELD CuoC    LIKE Creditos.Cuota
    FIELD SdoTD   LIKE Creditos.Sdo_Capital 
    FIELD CSiNo   AS LOG INIT NO FORM "Si/No".

/*guarda los usuarios disponibles para la siguiente instancia*/
  DEF TEMP-TABLE TProIns  NO-UNDO
    FIELD TP_Agencia   LIKE Agencias.Agencia
    FIELD TP_Orden     LIKE Instancias.Orden_Instancia
    FIELD TP_Instancia LIKE Instancias.Instancia
    FIELD TP_NomInstan AS CHAR FORMAT "X(30)"
    FIELD TP_Usuario   LIKE Usuarios.Usuario
    FIELD TP_NomUsuar  AS CHAR FORMAT "X(30)"
    FIELD TP_Cantidad  AS INT FORMAT "999"
    FIELD TP_Abogado   LIKE Instancias.Id_Abogado.
    

/*Contiene los usuarios por instancia*/  
  DEF TEMP-TABLE TUXI NO-UNDO
    FIELD Instanc  LIKE Mov_Instancias.Instancia
    FIELD Agencia  LIKE Usuarios.Agencia
    FIELD Usuario  LIKE Usuarios.Usuario
    FIELD Nombre   LIKE Usuarios.Nombre
    FIELD Cantidad AS INT FORMAT "999"
    FIELD Proceso  AS LOG.
    
/*para guardar las instancias de una solicitud*/
  DEF TEMP-TABLE TCerradas
    FIELD Instancia      LIKE Mov_Instancias.Instancia
    FIELD INom_Instancia AS CHAR FORMAT "X(20)"
    FIELD Fec_Ingreso    LIKE Mov_Instancias.Fec_Ingreso
    FIELD Fec_Retiro     LIKE Mov_Instancias.Fec_Retiro
    FIELD Hora_Ingreso   LIKE Mov_Instancias.Hora_Ingreso
    FIELD Hora_Retiro    LIKE Mov_Instancias.Hora_Retiro
    FIELD Estado         LIKE Mov_Instancias.Estado
    FIELD Num_Solicitud  LIKE Mov_Instancias.Num_Solicitud    
    FIELD Usuario        LIKE Mov_Instancias.Usuario
    FIELD INom_Usuario   AS CHAR FORMAT "X(30)"
    FIELD Descripcion    LIKE Mov_Instancias.Descripcion.
  
  /*TABLA TEMPORAL DE LA CONSULTA DELBROWS */
  DEF TEMP-TABLE Consulta
      FIELD Num_Referencia  AS INT
      FIELD AgeSaro         AS INT
      FIELD Nit             AS CHAR
      FIELD Estado          AS INT
      FIELD Nombre          AS CHAR FORMAT "X(40)"
      FIELD Fec_Ingreso     AS DATE 
      FIELD Hor_Ingreso     AS CHAR FORMAT "X(15)"
      FIELD Orden           AS INT
      FIELD Monto           AS DEC 
      FIELD Vigencia        AS INT FORMAT "9999".


/*para buscar una cuenta de ahorro*/
  DEF VAR A_Nit LIKE Ahorros.Nit.
  DEF VAR A_Age LIKE Ahorros.Agencia.
  DEF VAR A_Pro LIKE Ahorros.Cod_Ahorro.
  DEF VAR A_NitW LIKE Ahorros.Nit.
  DEF VAR A_Cue LIKE Ahorros.Cue_Ahorros.
  
  DEF VAR i AS INT.
  DEF VAR W_Ok AS LOG.
  DEF VAR W_TipoProducto LIKE Pro_Creditos.Tip_Credito.
  DEF VAR Dias AS DEC.
  
  
  DEF TEMP-TABLE TIns LIKE Cfg_Instancias.
  
   DEF VAR W_Consulta   AS   LOG.
   DEF VAR W_TotCuoEsp  LIKE Pro_Especiales.Ran_FinCuota.
   DEF VAR W_NomTer     LIKE  Terceros.Nombre.
   DEF VAR W_Tercero    AS CHAR.
   DEF VAR W_NvaAse     AS LOG INITIAL FALSE.
   DEF VAR W_Rpta1      AS LOG INITIAL FALSE.
   DEF VAR W_NomLin     AS CHAR FORMAT "X(15)".
   DEF VAR W_P          AS INT.
   DEF VAR W_ConDed     AS INT INITIAL 1.
   DEF VAR W_Ind        AS INT INITIAL 0.  
   DEF VAR W_WidSel     AS WIDGET-HANDLE.
   DEF VAR W_Cerrado    AS INT INITIAL 0.
   DEF VAR W_VlrAux     AS DEC FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEF VAR W_PerDed     AS INT INITIAL 1.
   DEF VAR W_Codfor     LIKE  Formatos.Cod_Formato INITIAL 0.
   DEF VAR W_Interplazo AS DEC FORMAT "->>>>>>>>>>>9" INITIAL 0.   
   DEF VAR W_TasEfe     AS DEC FORMAT ">>9.999999".
   DEF VAR W_TasaCont   AS DEC FORMAT ">>9.999999".
   DEF VAR W_Liquidar   AS LOG INITIAL FALSE.
   DEF VAR W_Existe     AS LOG INITIAL FALSE.
   DEF VAR W_NroPer     AS INT INITIAL 1.
   DEF VAR W_MonMul     AS DEC FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEF VAR W_Razon      AS DEC FORMAT "->>>>>>>>>>>9.999" INITIAL 0.
   DEF VAR P_Band       AS LOG INITIAL FALSE.
   DEF VAR W_TasNom     AS DEC FORMAT "->>>>>>>>>>>9.999" INITIAL 0.
   DEF VAR W_ErrIndica  AS LOG.
   DEF VAR W_TotPorDed  AS DEC FORMAT ">>9.9999" INITIAL 0.
   DEF VAR Suma         AS DEC FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEF VAR W_Suma       AS    DEC FORMAT ">>>,>>>,>>>,>>9" INITIAL 0.
   DEF VAR W_PlaPer     AS    INT INITIAL 0.   
   DEF VAR W_PlazoDias  AS    INT INITIAL 0.   
   DEF VAR W_DesAse     LIKE  Varios.Codigo.
   DEF VAR W_MonMin     LIKE  Pro_Creditos.Val_Montominimo.
   DEF VAR W_MonMax     LIKE  Pro_Creditos.Val_Montomaximo.
   DEF VAR W_PlaMin     LIKE  Pro_Creditos.Pla_Minimo.
   DEF VAR W_PlaMax     LIKE  Pro_Creditos.Pla_Maximo.    
  
   
   
   DEF VAR W_ClaTas     LIKE  Pro_Creditos.Tip-Tasa.
   DEF VAR W_PNegocia   LIKE  Ran_Interes.Pun_Negociables.
   DEF VAR W_TasDif     LIKE  Pro_Creditos.Id_TasDiferencial.
   DEF VAR W_TotExtras  LIKE  Pro_Creditos.Val_Montomaximo.
   DEF VAR W_DiaPer     AS INT FORMAT "9999".
   DEF VAR W_PerLiqui   AS INT FORMAT "99".
   DEF VAR W_Per        AS DEC FORMAT "999.9999999".
   DEF VAR W_CreMixto   AS DEC.
   DEF VAR Wk_Edad      AS INT FORMAT "999".
   DEF VAR WFactorCod   AS INT FORMAT "999".

DEF TEMP-TABLE TmpI
  FIELD ILinea AS INT FORMAT "99"
  FIELD ITexto AS CHAR FORMAT "X(125)".

DEF TEMP-TABLE CTmpI
  FIELD ILinea AS INT FORMAT "99"
  FIELD ITexto AS CHAR FORMAT "X(50)"
  FIELD Ingr   LIKE Clientes.Ing_Otros 
  FIELD Egr    LIKE Clientes.Ing_Otros
  FIELD Puntos LIKE Clientes.Puntaje
  FIELD Endeu  LIKE Clientes.Capacidad_Pago.

DEF TEMP-TABLE CCTmpI
  FIELD ILinea AS INT FORMAT "99"
  FIELD ITexto AS CHAR FORMAT "X(20)"
  FIELD Deud   AS CHAR FORMAT "X(20)" 
  FIELD Cod1   AS CHAR FORMAT "X(20)"
  FIELD Cod2   AS CHAR FORMAT "X(20)"
  FIELD Cod3   AS CHAR FORMAT "X(20)".

  */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME F_Saro
&Scoped-define BROWSE-NAME Br_Cerradas

/* Internal Tables (found by Frame, Query & Browse Queries)             */
&Scoped-define INTERNAL-TABLES TCerradas Consulta Saro Mov_Instancias

/* Definitions for BROWSE Br_Cerradas                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Cerradas TCerradas.Instancia TCerradas.INom_Instancia TCerradas.Usuario TCerradas.INom_Usuario TCerradas.Fec_Retiro TCerradas.Descripcion   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Cerradas   
&Scoped-define SELF-NAME Br_Cerradas
&Scoped-define QUERY-STRING-Br_Cerradas FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Cerradas OPEN QUERY {&SELF-NAME} FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Cerradas TCerradas
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Cerradas TCerradas


/* Definitions for BROWSE Br_Consulta                                   */
&Scoped-define FIELDS-IN-QUERY-Br_Consulta Consulta.Num_Referencia Consulta.AgeSaro Consulta.Estado Consulta.Usuario Consulta.Nombre Consulta.Fec_Ingreso Consulta.Hor_Ingreso Consulta.Monto Consulta.Vigencia   
&Scoped-define ENABLED-FIELDS-IN-QUERY-Br_Consulta   
&Scoped-define SELF-NAME Br_Consulta
&Scoped-define QUERY-STRING-Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Referencia INDEXED-REPOSITION
&Scoped-define OPEN-QUERY-Br_Consulta OPEN QUERY {&SELF-NAME} FOR EACH Consulta NO-LOCK BY Consulta.Num_Referencia INDEXED-REPOSITION.
&Scoped-define TABLES-IN-QUERY-Br_Consulta Consulta
&Scoped-define FIRST-TABLE-IN-QUERY-Br_Consulta Consulta


/* Definitions for FRAME F_Cerradas                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Cerradas ~
    ~{&OPEN-QUERY-Br_Cerradas}

/* Definitions for FRAME F_ConHV                                        */

/* Definitions for FRAME F_Consulta                                     */
&Scoped-define OPEN-BROWSERS-IN-QUERY-F_Consulta ~
    ~{&OPEN-QUERY-Br_Consulta}

/* Definitions for FRAME F_Cuantia                                      */
&Scoped-define FIELDS-IN-QUERY-F_Cuantia Saro.Nit Saro.Cuantia_Riesgo ~
Saro.Cuantia_Recup_Tot Saro.Cuantia_Recup_Seg Saro.Nit_seguro 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Cuantia Saro.Nit 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Cuantia Saro
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Cuantia Saro
&Scoped-define QUERY-STRING-F_Cuantia FOR EACH Saro SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Cuantia OPEN QUERY F_Cuantia FOR EACH Saro SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Cuantia Saro
&Scoped-define FIRST-TABLE-IN-QUERY-F_Cuantia Saro


/* Definitions for FRAME F_Estado                                       */
&Scoped-define FIELDS-IN-QUERY-F_Estado Saro.Estado_Evento ~
Saro.Descrip_Anulado 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Estado Saro.Estado_Evento 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Estado Saro
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Estado Saro
&Scoped-define QUERY-STRING-F_Estado FOR EACH Saro SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Estado OPEN QUERY F_Estado FOR EACH Saro SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Estado Saro
&Scoped-define FIRST-TABLE-IN-QUERY-F_Estado Saro


/* Definitions for FRAME F_Producto                                     */
&Scoped-define FIELDS-IN-QUERY-F_Producto Saro.Clase_Producto 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Producto Saro.Clase_Producto 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Producto Saro
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Producto Saro
&Scoped-define QUERY-STRING-F_Producto FOR EACH Saro SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Producto OPEN QUERY F_Producto FOR EACH Saro SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Producto Saro
&Scoped-define FIRST-TABLE-IN-QUERY-F_Producto Saro


/* Definitions for FRAME F_Sarouno                                      */
&Scoped-define FIELDS-IN-QUERY-F_Sarouno Saro.Num_Referencia ~
Saro.Fec_Grabacion Saro.Hora_Grabacion Saro.Fec_Des_Even Saro.Hora_Des_Even ~
Saro.Fec_Ini_Even Saro.Hora_Ini_Even Saro.Fec_Fin_Even Saro.Hora_Fin_Even ~
Saro.Canal_Servicio Saro.Descrip_Evento 
&Scoped-define ENABLED-FIELDS-IN-QUERY-F_Sarouno Saro.Fec_Ini_Even ~
Saro.Hora_Ini_Even Saro.Fec_Fin_Even Saro.Hora_Fin_Even 
&Scoped-define ENABLED-TABLES-IN-QUERY-F_Sarouno Saro
&Scoped-define FIRST-ENABLED-TABLE-IN-QUERY-F_Sarouno Saro
&Scoped-define QUERY-STRING-F_Sarouno FOR EACH Mov_Instancias SHARE-LOCK, ~
      EACH Saro WHERE Saro.Num_Referencia = Mov_Instancias.Num_Solicitud SHARE-LOCK
&Scoped-define OPEN-QUERY-F_Sarouno OPEN QUERY F_Sarouno FOR EACH Mov_Instancias SHARE-LOCK, ~
      EACH Saro WHERE Saro.Num_Referencia = Mov_Instancias.Num_Solicitud SHARE-LOCK.
&Scoped-define TABLES-IN-QUERY-F_Sarouno Mov_Instancias Saro
&Scoped-define FIRST-TABLE-IN-QUERY-F_Sarouno Mov_Instancias
&Scoped-define SECOND-TABLE-IN-QUERY-F_Sarouno Saro


/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS RECT-2 RECT-3 Cmb_Instancias BUTTON-1 ~
Btn_ProcesarInstancia T_Refresh Btn_Consulta Btn_Salvar Btn_Deshacer ~
Btn_Ingresar Btn_Cancelar BUTTON-9 BUTTON-2 BUTTON-4 
&Scoped-Define DISPLAYED-OBJECTS Cmb_Instancias T_Refresh NomUsuario 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Btn_Salvar Btn_Deshacer Btn_Ingresar Btn_Cancelar ~
BUTTON-9 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for OCX Containers                            */
DEFINE VARIABLE CtrlFrame AS WIDGET-HANDLE NO-UNDO.
DEFINE VARIABLE chCtrlFrame AS COMPONENT-HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BUTTON-153 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 153" 
     SIZE 9 BY 1.88.

DEFINE VARIABLE E_Agregar AS CHARACTER 
     VIEW-AS EDITOR MAX-CHARS 990 SCROLLBAR-VERTICAL
     SIZE 55 BY 5.38
     BGCOLOR 15 FONT 5 NO-UNDO.

DEFINE BUTTON Btn_OutCerradas 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 143" 
     SIZE 8 BY 1.62.

DEFINE BUTTON BUTTON-154 
     LABEL "Ver solo Instancia y Descripción" 
     SIZE 25 BY 1.12.

DEFINE BUTTON Btn_OutConHV 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 153" 
     SIZE 9 BY 1.62.

DEFINE BUTTON Btn_OutConsulta 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 133" 
     SIZE 8 BY 1.62.

DEFINE VARIABLE Buscar AS CHARACTER FORMAT "X(30)":U 
     LABEL "" 
     VIEW-AS FILL-IN 
     SIZE 19 BY .81
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE VG_Alta AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .54
     BGCOLOR 12 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE VG_Media AS CHARACTER FORMAT "X(50)":U 
      VIEW-AS TEXT 
     SIZE 28.29 BY .62
     BGCOLOR 14 FONT 4 NO-UNDO.

DEFINE VARIABLE VG_Normal AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 29 BY .65
     BGCOLOR 10 FGCOLOR 0 FONT 4 NO-UNDO.

DEFINE VARIABLE R_Organizar AS INTEGER 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Referencia", 1,
"Agencia", 2,
"Usuario", 3,
"Nombre", 4,
"Fecha", 5,
"Vigencia", 6
     SIZE 69 BY .81 NO-UNDO.

DEFINE RECTANGLE RECT-223
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 92 BY 1.35.

DEFINE RECTANGLE RECT-287
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 30 BY 1.08
     BGCOLOR 14 .

DEFINE RECTANGLE RECT-288
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 31 BY 1.08
     BGCOLOR 10 .

DEFINE RECTANGLE RECT-289
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 31 BY 1.08
     BGCOLOR 12 .

DEFINE BUTTON Btn_OutScoringCuantia 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_outscoringCuantia" 
     SIZE 7 BY 1.62.

DEFINE BUTTON Btn_Salir_Cuantia 
     LABEL "Salir sin escoger" 
     SIZE 25 BY 1.12.

DEFINE VARIABLE NomNit AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomNit_Seguros AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 54 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_OutScoring-3 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_outscoring 2" 
     SIZE 7 BY 1.62.

DEFINE BUTTON Btn_Salir_Linea-2 
     LABEL "Salir sin escoger" 
     SIZE 25 BY 1.12.

DEFINE BUTTON Btn_NvoHv 
     LABEL "Ingresar" 
     SIZE 11 BY 1.35.

DEFINE BUTTON Btn_SalvaHV 
     LABEL "Salvar" 
     SIZE 11 BY 1.35.

DEFINE BUTTON BUTTON-149 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 149" 
     SIZE 11 BY 1.65.

DEFINE BUTTON BUTTON-150 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     LABEL "Button 150" 
     SIZE 11 BY 1.62.

DEFINE BUTTON BUTTON-152 
     LABEL "Cancelar" 
     SIZE 11 BY 1.38.

DEFINE BUTTON Btn_OutScoring 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 121" 
     SIZE 7 BY 1.62.

DEFINE VARIABLE S_InfoProducto AS CHARACTER 
     VIEW-AS SELECTION-LIST SINGLE SCROLLBAR-VERTICAL 
     SIZE 51.57 BY 7.81
     BGCOLOR 15 FONT 2 NO-UNDO.

DEFINE BUTTON Btn_AgregarTxt 
     LABEL "Agregar Texto" 
     SIZE 15 BY 1.62.

DEFINE BUTTON Btn_GraInstancia 
     LABEL "Grabar" 
     SIZE 15 BY 1.65.

DEFINE BUTTON Btn_Imp 
     LABEL "&Imprimir" 
     SIZE 14.86 BY 1.5.

DEFINE BUTTON Btn_insVolver 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 135" 
     SIZE 15 BY 1.62.

DEFINE BUTTON BUTTON-142 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 142" 
     SIZE 10 BY 1.88.

DEFINE VARIABLE Vigencia AS CHARACTER FORMAT "X(15)":U 
     LABEL "Tiempo vigente de la instancia" 
     VIEW-AS FILL-IN 
     SIZE 11 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE WHora_Ingreso AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE Whora_Retiro AS CHARACTER FORMAT "X(15)":U 
     VIEW-AS FILL-IN 
     SIZE 10.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 4 NO-UNDO.

DEFINE VARIABLE W_Instancia AS CHARACTER FORMAT "X(45)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W_UsuarioInstancia AS CHARACTER FORMAT "X(30)":U 
     VIEW-AS FILL-IN 
     SIZE 47 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE BUTTON Btn_OutScoring-2 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Btn_outscoring 2" 
     SIZE 7 BY 1.62.

DEFINE BUTTON Btn_Salir_Linea 
     LABEL "Salir sin escoger" 
     SIZE 25 BY 1.12.

DEFINE VARIABLE FPor_Ln1 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FPor_Ln2 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FPor_Ln3 AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE FTotalPorLn AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 6 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE TLinea1 AS LOGICAL INITIAL no 
     LABEL "Banca Personal y Minorista" 
     VIEW-AS TOGGLE-BOX
     SIZE 28.86 BY .77 NO-UNDO.

DEFINE VARIABLE TLinea2 AS LOGICAL INITIAL no 
     LABEL "Banca Comercial" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .77 NO-UNDO.

DEFINE VARIABLE TLinea3 AS LOGICAL INITIAL no 
     LABEL "Actividades Institucionales" 
     VIEW-AS TOGGLE-BOX
     SIZE 24 BY .77 NO-UNDO.

DEFINE BUTTON Btn_OutProductos 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "x" 
     SIZE 8 BY 1.62.

DEFINE BUTTON Btn_Salir_Producto 
     LABEL "Salir sin escoger" 
     SIZE 25 BY 1.12.

DEFINE VARIABLE Cmb_Productos AS CHARACTER FORMAT "X(70)":U 
     LABEL "Producto" 
     VIEW-AS COMBO-BOX INNER-LINES 30
     DROP-DOWN-LIST
     SIZE 39 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Tip_Credito LIKE Pro_Creditos.Tip_Credito
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "Consumo", 1,
"Comercial", 2,
"Hipotecario", 3,
"Microcrédito", 4
     SIZE 52 BY 1.08 NO-UNDO.

DEFINE BUTTON Btn_Cancelar 
     LABEL "Cancelar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Consulta 
     IMAGE-UP FILE "imagenes/lupa.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/lupa.bmp":U
     LABEL "Button 10" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Deshacer 
     LABEL "Deshacer" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Imprimir 
     IMAGE-UP FILE "imagenes/impresora2.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/impresora2.bmp":U
     LABEL "Button 8" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_Ingresar 
     LABEL "Ingresar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON Btn_ProcesarInstancia 
     LABEL "Procesar Instancia" 
     SIZE 34 BY 1.12.

DEFINE BUTTON Btn_Salvar 
     LABEL "Salvar" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-1 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/informacion.bmp":U
     LABEL "Button 1" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-2 
     LABEL "Salir" 
     SIZE 10 BY 1.62.

DEFINE BUTTON BUTTON-4 
     IMAGE-UP FILE "imagenes/interrogacion.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/interrogacion.bmp":U
     LABEL "Button 4" 
     SIZE 5 BY 1.12.

DEFINE BUTTON BUTTON-9 
     LABEL "Borrar" 
     SIZE 10 BY 1.62.

DEFINE VARIABLE Cmb_Instancias AS CHARACTER FORMAT "X(256)":U 
     LABEL "Instancias" 
     VIEW-AS COMBO-BOX INNER-LINES 10
     DROP-DOWN-LIST
     SIZE 34.14 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE VARIABLE NomUsuario AS CHARACTER FORMAT "X(40)":U 
      VIEW-AS TEXT 
     SIZE 54 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-2
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 5.38.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 4 GRAPHIC-EDGE  NO-FILL   
     SIZE 12 BY 10.23.

DEFINE VARIABLE T_Refresh AS LOGICAL INITIAL no 
     LABEL "Refrescar Automaticamente" 
     VIEW-AS TOGGLE-BOX
     SIZE 31 BY .77 NO-UNDO.

DEFINE BUTTON Btn_Cuantia 
     LABEL "Cuantias" 
     SIZE 15 BY 1.12
     FONT 5.

DEFINE BUTTON Btn_Estado 
     LABEL "Estado del Evento" 
     SIZE 20 BY 1.12.

DEFINE BUTTON Btn_LnOperativas 
     LABEL "Lineas Operativas" 
     SIZE 20 BY 1.12.

DEFINE BUTTON Btn_Producto 
     IMAGE-UP FILE "imagenes/arwdwn.gif":U
     LABEL "" 
     SIZE 3 BY .54.

DEFINE VARIABLE Cmb_Agencias AS CHARACTER FORMAT "X(35)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 1 BY 1
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Cmb_Generacion AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 3
     LIST-ITEM-PAIRS "","0",
                     "Generan Pérdida y Afectan Est Resultados","1",
                     "Generan Pérdida y No Afectan Est Resultados","2",
                     "No Generan Pérdida y No Afectan Est Resultados","3"
     DROP-DOWN-LIST
     SIZE 41 BY 1
     BGCOLOR 15 FGCOLOR 0 FONT 5 NO-UNDO.

DEFINE VARIABLE Cmb_Proceso AS CHARACTER FORMAT "X(50)":U 
     LABEL "Proceso" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 30 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb_Rieposibles AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 56 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Cmb_zonas AS CHARACTER FORMAT "X(30)":U 
     LABEL "Zonas" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     DROP-DOWN-LIST
     SIZE 32 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE Nivel2 AS CHARACTER FORMAT "X(70)":U 
     LABEL "Factor Riesgo" 
     VIEW-AS FILL-IN 
     SIZE 40.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Nivel3 AS CHARACTER FORMAT "X(70)":U 
     LABEL "Factor Ubicacion" 
     VIEW-AS FILL-IN 
     SIZE 40.57 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE Nom_Producto AS CHARACTER FORMAT "X(70)":U 
     LABEL "Producto" 
     VIEW-AS FILL-IN 
     SIZE 45.43 BY .81
     BGCOLOR 18 FGCOLOR 15 FONT 5 NO-UNDO.

DEFINE VARIABLE W-Agencias AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 26 BY .92
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE W-ClaseRiesgo AS CHARACTER FORMAT "X(70)":U 
     VIEW-AS FILL-IN 
     SIZE 40 BY .81 TOOLTIP "Clase de Riesgo"
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-292
     EDGE-PIXELS 2 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 4.58.

DEFINE BUTTON Btn_OutUltima 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "Button 153" 
     SIZE 11 BY 1.62.

DEFINE BUTTON Btn_SalvaUltima 
     LABEL "Salvar" 
     SIZE 11 BY 1.35.

DEFINE VARIABLE Cmb_Negadas AS CHARACTER FORMAT "X(256)":U 
     LABEL "Motivo Negación" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEMS "00001 - Capacidad de Pago","00002 - Mora Comercial","00003 - Estabilidad Laboral","00004 - Inconsis. Informaci. ","00005 - Concepto de Agencia","00006 - Crédito Recién Desemb.","00007 - Verificación de Inf.","00008 - Edad","00009 - Codeudor","00010 - Falta Documentación","00011 - Pago Centrales de R.","00012 - Docum. Desactualiza.","00013 - Docum. Falsa","00014 - Garantía Insuficiente","00015 - Capac. Pago de Codeud.","00016 - Mora Comercial Codeud.","00017 - Estab. Laboral Codeud." 
     DROP-DOWN-LIST
     SIZE 29 BY 1
     BGCOLOR 15 FGCOLOR 0  NO-UNDO.

DEFINE VARIABLE W_MenDes AS CHARACTER FORMAT "X(80)":U 
      VIEW-AS TEXT 
     SIZE 89 BY .62
     BGCOLOR 0  NO-UNDO.

DEFINE VARIABLE RADIO-SET_Estado_Evento AS INTEGER INITIAL 1 
     VIEW-AS RADIO-SET HORIZONTAL
     RADIO-BUTTONS 
          "En estudio", 1,
"Aprobada", 2,
"Negada", 3,
"Condicionada", 4
     SIZE 56 BY .69 NO-UNDO.

DEFINE RECTANGLE RECT-224
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 55 BY 1.88
     BGCOLOR 0 .

DEFINE RECTANGLE RECT-225
     EDGE-PIXELS 2 GRAPHIC-EDGE    
     SIZE 91 BY .96
     BGCOLOR 0 .

/* Query definitions                                                    */
&ANALYZE-SUSPEND
DEFINE QUERY Br_Cerradas FOR 
      TCerradas SCROLLING.

DEFINE QUERY Br_Consulta FOR 
      Consulta SCROLLING.

DEFINE QUERY F_Cuantia FOR 
      Saro SCROLLING.

DEFINE QUERY F_Estado FOR 
      Saro SCROLLING.

DEFINE QUERY F_Producto FOR 
      Saro SCROLLING.

DEFINE QUERY F_Sarouno FOR 
      Mov_Instancias, 
      Saro SCROLLING.
&ANALYZE-RESUME

/* Browse definitions                                                   */
DEFINE BROWSE Br_Cerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Cerradas wWin _FREEFORM
  QUERY Br_Cerradas NO-LOCK DISPLAY
      TCerradas.Instancia FORMAT "999":U COLUMN-LABEL "Ins"
      TCerradas.INom_Instancia COLUMN-LABEL "Nombre Instancia" FORMAT "x(30)"
      TCerradas.Usuario FORMAT "X(12)":U COLUMN-LABEL "Usuario"
      TCerradas.INom_Usuario COLUMN-LABEL "Nombre Usuario"
      TCerradas.Fec_Retiro FORMAT "99/99/9999":U   COLUMN-LABEL "Fec.Retiro"
      TCerradas.Descripcion COLUMN-LABEL "Descripcion" FORMAT "X(200)"
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 94 BY 5.92
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_ConHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_ConHV wWin _STRUCTURED
  
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS SIZE 95 BY 6.73
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.

DEFINE BROWSE Br_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _DISPLAY-FIELDS Br_Consulta wWin _FREEFORM
  QUERY Br_Consulta NO-LOCK DISPLAY
      Consulta.Num_Referencia FORMAT "99999999":U COLUMN-LABEL "Referencia"    
    Consulta.AgeSaro
    Consulta.Estado      COLUMN-LABEL "Est" 
    Consulta.Usuario     FORMAT "X(12)":U
    Consulta.Nombre      FORM "X(36)"
    Consulta.Fec_Ingreso FORMAT "99/99/9999":U COLUMN-LABEL "Fecha"
    Consulta.Hor_Ingreso COLUMN-LABEL "Hora"
    Consulta.Monto       COLUMN-LABEL "Monto"
    Consulta.Vigencia    COLUMN-LABEL "Vigen."
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME
    WITH NO-ROW-MARKERS SEPARATORS MULTIPLE SIZE 92 BY 13.19
         BGCOLOR 15 FONT 4 FIT-LAST-COLUMN.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME F_HojaVida
     Hoja_Vida.Fec_Grabacion AT ROW 1.27 COL 70 COLON-ALIGNED
          LABEL "Fecha Grabación"
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     BUTTON-150 AT ROW 1.27 COL 86
     Hoja_Vida.Asunto_Cumplido AT ROW 1.54 COL 4
          LABEL "Asunto Cumplido"
          VIEW-AS TOGGLE-BOX
          SIZE 18 BY .81
     Hoja_Vida.Observacion AT ROW 2.62 COL 4 NO-LABEL
          VIEW-AS EDITOR LARGE
          SIZE 80 BY 7.08
          BGCOLOR 15 
     Btn_SalvaHV AT ROW 3.15 COL 86
     Btn_NvoHv AT ROW 4.5 COL 86
     BUTTON-152 AT ROW 5.85 COL 86
     BUTTON-149 AT ROW 8 COL 86
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 12.04
         SIZE 98 BY 9.96
         BGCOLOR 17 FONT 5
         TITLE "Hoja de Vida".

DEFINE FRAME F_InfoProducto
     S_InfoProducto AT ROW 1.27 COL 1.72 NO-LABEL
     Btn_OutScoring AT ROW 9.15 COL 43.72
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 44.43 ROW 7.73
         SIZE 53.57 BY 10.77
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Información del Producto".

DEFINE FRAME F_Ultima
     Btn_SalvaUltima AT ROW 1.27 COL 85
     Btn_OutUltima AT ROW 2.62 COL 85
     RADIO-SET_Estado_Evento AT ROW 3.27 COL 21 HELP
          "Estado Evento" NO-LABEL
     Cmb_Negadas AT ROW 4.08 COL 53 COLON-ALIGNED
     W_MenDes AT ROW 5.31 COL 4 COLON-ALIGNED NO-LABEL
     "Se ha llegado a la última instancia en el proceso de solicitud," VIEW-AS TEXT
          SIZE 53 BY .81 AT ROW 1.54 COL 22
          BGCOLOR 0 FGCOLOR 15 
     "Cambie el estado de la Solicitud según el estudio realizado" VIEW-AS TEXT
          SIZE 53 BY .62 AT ROW 2.35 COL 22
          BGCOLOR 0 FGCOLOR 15 
     RECT-224 AT ROW 1.27 COL 21
     RECT-225 AT ROW 5.15 COL 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         PAGE-TOP SIDE-LABELS TOP-ONLY NO-UNDERLINE THREE-D 
         AT COL 3 ROW 4.5
         SIZE 99 BY 17.5
         BGCOLOR 18 FGCOLOR 15 FONT 5
         TITLE "Estado de la Solicitud".

DEFINE FRAME F_Saro
     Cmb_Instancias AT ROW 1.27 COL 10 COLON-ALIGNED
     BUTTON-1 AT ROW 1.54 COL 103.72
     Btn_ProcesarInstancia AT ROW 2.35 COL 12
     T_Refresh AT ROW 2.35 COL 48
     Btn_Imprimir AT ROW 3.15 COL 103.72
     Btn_Consulta AT ROW 4.77 COL 103.72
     Btn_Salvar AT ROW 9.62 COL 103.43
     Btn_Deshacer AT ROW 11.23 COL 103.43
     Btn_Ingresar AT ROW 12.85 COL 103.43
     Btn_Cancelar AT ROW 14.46 COL 103.43
     BUTTON-9 AT ROW 16.08 COL 103.43
     BUTTON-2 AT ROW 17.69 COL 103.43
     BUTTON-4 AT ROW 20.38 COL 105
     NomUsuario AT ROW 1.27 COL 45 COLON-ALIGNED NO-LABEL
     RECT-2 AT ROW 1.27 COL 102.72
     RECT-3 AT ROW 9.35 COL 102.43
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 114.72 BY 22.46
         BGCOLOR 17 FONT 5.

DEFINE FRAME F_Estado
     Saro.Estado_Evento AT ROW 1.35 COL 4.72 NO-LABEL WIDGET-ID 86
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "En Proceso", 1,
"Aprobada", 2,
"Negada", 3
          SIZE 44.29 BY .69
     Saro.Descrip_Anulado AT ROW 2.62 COL 2.57 NO-LABEL WIDGET-ID 14
          VIEW-AS EDITOR MAX-CHARS 1300 SCROLLBAR-VERTICAL
          SIZE 50.43 BY 4.58
          BGCOLOR 15 FGCOLOR 0 FONT 5
     Btn_OutScoring-3 AT ROW 7.96 COL 41
     Btn_Salir_Linea-2 AT ROW 8.08 COL 7.14 WIDGET-ID 76
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 42.43 ROW 10.08
         SIZE 55 BY 9.77
         BGCOLOR 18 FGCOLOR 0 FONT 5
         TITLE "Estado del Evento" WIDGET-ID 300.

DEFINE FRAME F_Instancias
     BUTTON-142 AT ROW 1.27 COL 3
     Mov_Instancias.Fec_Ingreso AT ROW 1.27 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     WHora_Ingreso AT ROW 1.27 COL 85 COLON-ALIGNED NO-LABEL
     W_Instancia AT ROW 1.42 COL 12 COLON-ALIGNED NO-LABEL
     W_UsuarioInstancia AT ROW 2.35 COL 12 COLON-ALIGNED NO-LABEL
     Mov_Instancias.Fec_Retiro AT ROW 2.35 COL 73 COLON-ALIGNED
          VIEW-AS FILL-IN 
          SIZE 11.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Whora_Retiro AT ROW 2.35 COL 85 COLON-ALIGNED NO-LABEL
     Vigencia AT ROW 3.42 COL 48 COLON-ALIGNED
     Mov_Instancias.Estado AT ROW 3.54 COL 3
          LABEL "Cerrar Instancia"
          VIEW-AS TOGGLE-BOX
          SIZE 17 BY .77
     Mov_Instancias.Descripcion AT ROW 4.5 COL 3 NO-LABEL
          VIEW-AS EDITOR MAX-CHARS 1200 SCROLLBAR-VERTICAL LARGE
          SIZE 77 BY 9.69
          BGCOLOR 15 
     Btn_GraInstancia AT ROW 4.5 COL 82
     Btn_AgregarTxt AT ROW 6.38 COL 82
     Btn_Imp AT ROW 9.69 COL 82
     Btn_insVolver AT ROW 12.58 COL 82
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3.14 ROW 7.27
         SIZE 98 BY 14.54
         BGCOLOR 17 FONT 5
         TITLE "Procesar Instancias".

DEFINE FRAME F_Producto
     Saro.Clase_Producto AT ROW 1.19 COL 3 HELP
          "Clase de producto de Ahorro o Crédito" NO-LABEL
          VIEW-AS RADIO-SET HORIZONTAL
          RADIO-BUTTONS 
                    "Ahorros", 1,
"Credito", 2
          SIZE 52 BY 1.08
     Tip_Credito AT ROW 2.27 COL 3 HELP
          "Tipo de Producto de Crédito" NO-LABEL WIDGET-ID 2
     Cmb_Productos AT ROW 3.62 COL 14 COLON-ALIGNED
     Btn_OutProductos AT ROW 4.96 COL 48
     Btn_Salir_Producto AT ROW 5.23 COL 16
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 40.43 ROW 11.31
         SIZE 60 BY 6.73
         BGCOLOR 17 FONT 5
         TITLE "Productos".

DEFINE FRAME F_LinOperativas
     TLinea1 AT ROW 2.08 COL 3.14 WIDGET-ID 2
     FPor_Ln1 AT ROW 2.08 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 8
     TLinea2 AT ROW 2.92 COL 3.14 WIDGET-ID 4
     FPor_Ln2 AT ROW 3.04 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 10
     TLinea3 AT ROW 3.85 COL 3.14 WIDGET-ID 6
     FTotalPorLn AT ROW 3.92 COL 42.29 COLON-ALIGNED NO-LABEL WIDGET-ID 78
     FPor_Ln3 AT ROW 3.96 COL 31 COLON-ALIGNED NO-LABEL WIDGET-ID 12
     Btn_OutScoring-2 AT ROW 5.19 COL 43.72
     Btn_Salir_Linea AT ROW 5.35 COL 7.14 WIDGET-ID 76
     "Total %" VIEW-AS TEXT
          SIZE 7 BY .77 AT ROW 2.92 COL 43.86 WIDGET-ID 80
          FGCOLOR 7 FONT 9
     "%" VIEW-AS TEXT
          SIZE 4.14 BY .77 AT ROW 1.23 COL 34.86 WIDGET-ID 74
          FGCOLOR 7 FONT 9
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 46.29 ROW 7.69
         SIZE 53.57 BY 7.27
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Lineas Operativas (Nivel 1)" WIDGET-ID 200.

DEFINE FRAME F_Agregar
     E_Agregar AT ROW 1.27 COL 2 NO-LABEL
     BUTTON-153 AT ROW 6.92 COL 48
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 28 ROW 10.73
         SIZE 57 BY 8.88
         BGCOLOR 17 
         TITLE "Texto a ser Agregado".

DEFINE FRAME F_Cerradas
     Br_Cerradas AT ROW 1.27 COL 3
     Btn_OutCerradas AT ROW 7.73 COL 89
     BUTTON-154 AT ROW 8 COL 62
     "La instancia activa se encuentra en letra color rojo" VIEW-AS TEXT
          SIZE 43 BY .81 AT ROW 7.19 COL 3
          FGCOLOR 7 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 4.5
         SIZE 98 BY 9.42
         BGCOLOR 17 FGCOLOR 0 FONT 4
         TITLE "Consulta de Instancias Procesadas y Actuales".

DEFINE FRAME F_ConHV
     Br_ConHV AT ROW 1.27 COL 3
     Btn_OutConHV AT ROW 8.27 COL 89
     "   Los Mensajes en Rojo estan pendientes por cumplirse" VIEW-AS TEXT
          SIZE 85 BY 1.08 AT ROW 8.54 COL 3
          BGCOLOR 0 FGCOLOR 15 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 3 ROW 12.04
         SIZE 98 BY 9.96
         BGCOLOR 17 FONT 4
         TITLE "Asuntos Pendientes".

DEFINE FRAME F_Cuantia
     Saro.Nit AT ROW 1.5 COL 11.14 COLON-ALIGNED HELP
          "Nit del Origen del Riesgo" WIDGET-ID 108
          LABEL "Nit Resp" FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 26 BY .81
          BGCOLOR 15 FGCOLOR 0 
     NomNit AT ROW 2.42 COL 13.14 NO-LABEL WIDGET-ID 112
     Saro.Cuantia_Riesgo AT ROW 3.42 COL 15 COLON-ALIGNED WIDGET-ID 106
          VIEW-AS FILL-IN 
          SIZE 22.43 BY .81
          BGCOLOR 15 
     Saro.Cuantia_Recup_Tot AT ROW 4.5 COL 15 COLON-ALIGNED HELP
          "Cuantia Total Recuperada" WIDGET-ID 104
          LABEL "Recuperada" FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 22.43 BY .81
          BGCOLOR 15 
     Saro.Cuantia_Recup_Seg AT ROW 5.58 COL 15 COLON-ALIGNED HELP
          "Valor recuperado por seguros" WIDGET-ID 102
          LABEL "Recup Seguros" FORMAT "->>>>>,>>>,>>>,>>9.99"
          VIEW-AS FILL-IN 
          SIZE 22.43 BY .81
          BGCOLOR 15 
     Saro.Nit_seguro AT ROW 6.92 COL 11.14 COLON-ALIGNED HELP
          "Nit del seguro" WIDGET-ID 110
          LABEL "Nit Seguro" FORMAT "X(14)"
          VIEW-AS FILL-IN 
          SIZE 26 BY .81
          BGCOLOR 18 FGCOLOR 15 
     NomNit_Seguros AT ROW 7.96 COL 11.14 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     Btn_OutScoringCuantia AT ROW 9.08 COL 52 WIDGET-ID 98
     Btn_Salir_Cuantia AT ROW 9.62 COL 10 WIDGET-ID 100
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 24.14 ROW 5.38
         SIZE 72.29 BY 11.04
         BGCOLOR 17 FGCOLOR 0 FONT 5
         TITLE "Cuantias Evento Riesgo" WIDGET-ID 400.

DEFINE FRAME F_Consulta
     Br_Consulta AT ROW 2.08 COL 4
     R_Organizar AT ROW 15.54 COL 25 NO-LABEL
     Btn_OutConsulta AT ROW 16.62 COL 88
     Buscar AT ROW 17.15 COL 19 COLON-ALIGNED
     VG_Normal AT ROW 1.23 COL 3 COLON-ALIGNED NO-LABEL
     VG_Media AT ROW 1.27 COL 33.72 COLON-ALIGNED NO-LABEL
     VG_Alta AT ROW 1.27 COL 64 COLON-ALIGNED NO-LABEL
     "Organizada por..." VIEW-AS TEXT
          SIZE 16 BY .81 AT ROW 15.54 COL 7
          FGCOLOR 7 
     "" VIEW-AS TEXT
          SIZE 45 BY .81 AT ROW 17.15 COL 42
          FGCOLOR 7 
     RECT-223 AT ROW 15.27 COL 4
     RECT-287 AT ROW 1 COL 35
     RECT-288 AT ROW 1 COL 4
     RECT-289 AT ROW 1 COL 65
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 2.29 ROW 4.27
         SIZE 99 BY 18.31
         BGCOLOR 17 FONT 5
         TITLE "Eventos Disponibles".

DEFINE FRAME F_Sarouno
     W-Agencias AT ROW 1.5 COL 9 COLON-ALIGNED NO-LABEL WIDGET-ID 148
     W-ClaseRiesgo AT ROW 7.08 COL 17 COLON-ALIGNED NO-LABEL WIDGET-ID 146
     Cmb_Agencias AT ROW 1.46 COL 34 COLON-ALIGNED NO-LABEL WIDGET-ID 2
     Saro.Num_Referencia AT ROW 1.46 COL 47.14 COLON-ALIGNED HELP
          "Ingrese el Numero de Referencia del credito" WIDGET-ID 8
          LABEL "Referencia" FORMAT "99999999"
          VIEW-AS FILL-IN 
          SIZE 11 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Saro.Fec_Grabacion AT ROW 1.81 COL 75.86 COLON-ALIGNED HELP
          "Fecha de Grabacion del evento" WIDGET-ID 10
          LABEL "Grabación" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Saro.Hora_Grabacion AT ROW 1.81 COL 86 COLON-ALIGNED HELP
          "Hora Grabacion Evento" NO-LABEL WIDGET-ID 44 FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .81
          BGCOLOR 18 FGCOLOR 15 
     Saro.Fec_Des_Even AT ROW 2.77 COL 75.86 COLON-ALIGNED HELP
          "Ingrese la Fecha del descubrimiento del Evento" WIDGET-ID 12
          LABEL "Descubrimiento" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Saro.Hora_Des_Even AT ROW 2.77 COL 86 COLON-ALIGNED HELP
          "Hora Descubrimiento  Evento" NO-LABEL WIDGET-ID 40 FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .81
          BGCOLOR 15 FGCOLOR 0 FONT 5
     Cmb_Rieposibles AT ROW 3.65 COL 1 COLON-ALIGNED NO-LABEL WIDGET-ID 114
     Saro.Fec_Ini_Even AT ROW 3.73 COL 75.86 COLON-ALIGNED HELP
          "Ingrese la Fecha del Inicio del Evento" WIDGET-ID 16
          LABEL "Inicio" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 FONT 5
     Saro.Hora_Ini_Even AT ROW 3.77 COL 86 COLON-ALIGNED HELP
          "Hora Inicio Evento" NO-LABEL WIDGET-ID 46 FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Saro.Fec_Fin_Even AT ROW 4.69 COL 75.86 COLON-ALIGNED HELP
          "Ingrese la Fecha del Final del Evento" WIDGET-ID 18
          LABEL "Final" FORMAT "99/99/9999"
          VIEW-AS FILL-IN 
          SIZE 10 BY .81
          BGCOLOR 15 
     Saro.Hora_Fin_Even AT ROW 4.69 COL 86 COLON-ALIGNED HELP
          "Hora Fin Evento" NO-LABEL WIDGET-ID 42 FORMAT "X(8)"
          VIEW-AS FILL-IN 
          SIZE 9.43 BY .81
          BGCOLOR 15 FGCOLOR 0 
     Cmb_Proceso AT ROW 7.08 COL 67.43 COLON-ALIGNED WIDGET-ID 90
     Cmb_zonas AT ROW 8.54 COL 6 COLON-ALIGNED WIDGET-ID 118
     Nom_Producto AT ROW 8.54 COL 49.14 COLON-ALIGNED WIDGET-ID 88
     Btn_Producto AT ROW 8.58 COL 96.86 WIDGET-ID 86
     Saro.Canal_Servicio AT ROW 10.42 COL 6 COLON-ALIGNED HELP
          "Canal Servicio" WIDGET-ID 92
          LABEL "Canal" FORMAT "X(50)"
          VIEW-AS FILL-IN 
          SIZE 42 BY .92
          BGCOLOR 15 FGCOLOR 0 
     Cmb_Generacion AT ROW 10.54 COL 53.86 COLON-ALIGNED NO-LABEL WIDGET-ID 108
     Saro.Descrip_Evento AT ROW 11.85 COL 2.86 NO-LABEL WIDGET-ID 14
          VIEW-AS EDITOR MAX-CHARS 1300 SCROLLBAR-VERTICAL
          SIZE 94.14 BY 5.65
          BGCOLOR 15 FGCOLOR 0 FONT 5
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.57 ROW 3.81
         SIZE 100.29 BY 19.31
         BGCOLOR 17 FONT 5.

/* DEFINE FRAME statement is approaching 4K Bytes.  Breaking it up   */
DEFINE FRAME F_Sarouno
     Btn_Cuantia AT ROW 18 COL 12.57 WIDGET-ID 130
     Btn_LnOperativas AT ROW 18 COL 35.57 WIDGET-ID 98
     Btn_Estado AT ROW 18 COL 64.29 WIDGET-ID 116
     Nivel3 AT ROW 4.77 COL 16.72 COLON-ALIGNED WIDGET-ID 138
     Nivel2 AT ROW 5.92 COL 16.72 COLON-ALIGNED WIDGET-ID 140
     "Agencia:" VIEW-AS TEXT
          SIZE 8 BY .69 AT ROW 1.62 COL 2.57 WIDGET-ID 150
     "Clase Riesgo:" VIEW-AS TEXT
          SIZE 13 BY 1.08 AT ROW 6.92 COL 5 WIDGET-ID 152
     "Eventos" VIEW-AS TEXT
          SIZE 15.72 BY .77 AT ROW 2.88 COL 3.14 WIDGET-ID 122
          FGCOLOR 7 FONT 5
     "Tipo Perdida" VIEW-AS TEXT
          SIZE 13 BY .77 AT ROW 9.5 COL 56.14 WIDGET-ID 110
          FGCOLOR 7 FONT 5
     "DD/MM/AAAA  HH:MM:SS" VIEW-AS TEXT
          SIZE 20.29 BY .5 AT ROW 1.35 COL 77.72 WIDGET-ID 74
          FGCOLOR 7 FONT 4
     "Fechas:" VIEW-AS TEXT
          SIZE 8.43 BY .81 AT ROW 1 COL 62.57 WIDGET-ID 144
          FGCOLOR 7 
     RECT-292 AT ROW 1.27 COL 61 WIDGET-ID 142
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1.57 ROW 3.81
         SIZE 100.29 BY 19.31
         BGCOLOR 17 FONT 5
         TITLE "Identificación de Riesgo SARO".


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Other Settings: COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "Proceso de SARO"
         HEIGHT             = 22.62
         WIDTH              = 114.43
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 146.29
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 146.29
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _INCLUDED-LIB wWin 
/* ************************* Included-Libraries *********************** */

{src/adm2/containr.i}

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




/* ***********  Runtime Attributes and AppBuilder Settings  *********** */

&ANALYZE-SUSPEND _RUN-TIME-ATTRIBUTES
/* SETTINGS FOR FRAME F_Agregar
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Agregar:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Cerradas
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_Cerradas TEXT-5 F_Cerradas */
ASSIGN 
       FRAME F_Cerradas:HIDDEN           = TRUE.

ASSIGN 
       Br_Cerradas:ALLOW-COLUMN-SEARCHING IN FRAME F_Cerradas = TRUE.

/* SETTINGS FOR FRAME F_ConHV
   NOT-VISIBLE                                                          */
/* BROWSE-TAB Br_ConHV TEXT-6 F_ConHV */
ASSIGN 
       FRAME F_ConHV:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Consulta
                                                                        */
/* BROWSE-TAB Br_Consulta RECT-289 F_Consulta */
/* SETTINGS FOR FILL-IN VG_Alta IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VG_Media IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN VG_Normal IN FRAME F_Consulta
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Cuantia
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Cuantia:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN Saro.Cuantia_Recup_Seg IN FRAME F_Cuantia
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN Saro.Cuantia_Recup_Tot IN FRAME F_Cuantia
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN Saro.Cuantia_Riesgo IN FRAME F_Cuantia
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Saro.Nit IN FRAME F_Cuantia
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Saro.Nit_seguro IN FRAME F_Cuantia
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN NomNit IN FRAME F_Cuantia
   NO-ENABLE ALIGN-L                                                    */
/* SETTINGS FOR FILL-IN NomNit_Seguros IN FRAME F_Cuantia
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Estado
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Estado:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR Saro.Descrip_Anulado IN FRAME F_Estado
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_HojaVida
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_HojaVida:HIDDEN           = TRUE.

/* SETTINGS FOR TOGGLE-BOX Hoja_Vida.Asunto_Cumplido IN FRAME F_HojaVida
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Hoja_Vida.Fec_Grabacion IN FRAME F_HojaVida
   NO-ENABLE EXP-LABEL                                                  */
ASSIGN 
       Hoja_Vida.Observacion:READ-ONLY IN FRAME F_HojaVida        = TRUE.

/* SETTINGS FOR FRAME F_InfoProducto
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_InfoProducto:HIDDEN           = TRUE.

/* SETTINGS FOR FRAME F_Instancias
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Instancias:HIDDEN           = TRUE.

/* SETTINGS FOR EDITOR Mov_Instancias.Descripcion IN FRAME F_Instancias
   NO-ENABLE                                                            */
ASSIGN 
       Mov_Instancias.Descripcion:READ-ONLY IN FRAME F_Instancias        = TRUE.

/* SETTINGS FOR TOGGLE-BOX Mov_Instancias.Estado IN FRAME F_Instancias
   EXP-LABEL                                                            */
/* SETTINGS FOR FILL-IN Mov_Instancias.Fec_Ingreso IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Mov_Instancias.Fec_Retiro IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Vigencia IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN WHora_Ingreso IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Whora_Retiro IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_Instancia IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN W_UsuarioInstancia IN FRAME F_Instancias
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_LinOperativas
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_LinOperativas:HIDDEN           = TRUE.

/* SETTINGS FOR FILL-IN FPor_Ln1 IN FRAME F_LinOperativas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FPor_Ln2 IN FRAME F_LinOperativas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FPor_Ln3 IN FRAME F_LinOperativas
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN FTotalPorLn IN FRAME F_LinOperativas
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Producto
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Producto:HIDDEN           = TRUE.

/* SETTINGS FOR RADIO-SET Saro.Clase_Producto IN FRAME F_Producto
   EXP-HELP                                                             */
/* SETTINGS FOR RADIO-SET Tip_Credito IN FRAME F_Producto
   LIKE = bdcentral.Pro_Creditos. EXP-HELP                              */
/* SETTINGS FOR FRAME F_Saro
   FRAME-NAME                                                           */
/* SETTINGS FOR BUTTON Btn_Cancelar IN FRAME F_Saro
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Deshacer IN FRAME F_Saro
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Imprimir IN FRAME F_Saro
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Ingresar IN FRAME F_Saro
   2                                                                    */
/* SETTINGS FOR BUTTON Btn_Salvar IN FRAME F_Saro
   2                                                                    */
/* SETTINGS FOR BUTTON BUTTON-9 IN FRAME F_Saro
   2                                                                    */
/* SETTINGS FOR FILL-IN NomUsuario IN FRAME F_Saro
   NO-ENABLE                                                            */
/* SETTINGS FOR FRAME F_Sarouno
   NOT-VISIBLE Custom                                                   */
ASSIGN 
       FRAME F_Sarouno:HIDDEN           = TRUE.

/* SETTINGS FOR BUTTON Btn_Cuantia IN FRAME F_Sarouno
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_Estado IN FRAME F_Sarouno
   NO-ENABLE                                                            */
/* SETTINGS FOR BUTTON Btn_LnOperativas IN FRAME F_Sarouno
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Saro.Canal_Servicio IN FRAME F_Sarouno
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
ASSIGN 
       Saro.Canal_Servicio:READ-ONLY IN FRAME F_Sarouno        = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Agencias IN FRAME F_Sarouno
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_Agencias:HIDDEN IN FRAME F_Sarouno           = TRUE.

ASSIGN 
       Cmb_zonas:HIDDEN IN FRAME F_Sarouno           = TRUE.

/* SETTINGS FOR EDITOR Saro.Descrip_Evento IN FRAME F_Sarouno
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Saro.Fec_Des_Even IN FRAME F_Sarouno
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN Saro.Fec_Fin_Even IN FRAME F_Sarouno
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Saro.Fec_Grabacion IN FRAME F_Sarouno
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN Saro.Fec_Ini_Even IN FRAME F_Sarouno
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Saro.Hora_Des_Even IN FRAME F_Sarouno
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN Saro.Hora_Fin_Even IN FRAME F_Sarouno
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Saro.Hora_Grabacion IN FRAME F_Sarouno
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN Saro.Hora_Ini_Even IN FRAME F_Sarouno
   EXP-LABEL EXP-FORMAT EXP-HELP                                        */
/* SETTINGS FOR FILL-IN Nivel2 IN FRAME F_Sarouno
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Nivel3 IN FRAME F_Sarouno
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Saro.Num_Referencia IN FRAME F_Sarouno
   NO-ENABLE EXP-LABEL EXP-FORMAT EXP-HELP                              */
/* SETTINGS FOR FILL-IN W-Agencias IN FRAME F_Sarouno
   NO-ENABLE                                                            */
ASSIGN 
       W-Agencias:READ-ONLY IN FRAME F_Sarouno        = TRUE.

/* SETTINGS FOR FILL-IN W-ClaseRiesgo IN FRAME F_Sarouno
   NO-ENABLE                                                            */
ASSIGN 
       W-ClaseRiesgo:READ-ONLY IN FRAME F_Sarouno        = TRUE.

/* SETTINGS FOR FRAME F_Ultima
   NOT-VISIBLE                                                          */
ASSIGN 
       FRAME F_Ultima:HIDDEN           = TRUE.

/* SETTINGS FOR COMBO-BOX Cmb_Negadas IN FRAME F_Ultima
   NO-DISPLAY NO-ENABLE                                                 */
ASSIGN 
       Cmb_Negadas:HIDDEN IN FRAME F_Ultima           = TRUE.

/* SETTINGS FOR FILL-IN W_MenDes IN FRAME F_Ultima
   NO-ENABLE                                                            */
IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME


/* Setting information for Queries and Browse Widgets fields            */

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Cerradas
/* Query rebuild information for BROWSE Br_Cerradas
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Cerradas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK BROWSE Br_Consulta
/* Query rebuild information for BROWSE Br_Consulta
     _START_FREEFORM
OPEN QUERY {&SELF-NAME} FOR EACH Consulta NO-LOCK BY Consulta.Num_Referencia INDEXED-REPOSITION.
     _END_FREEFORM
     _Options          = "NO-LOCK INDEXED-REPOSITION"
     _Query            is OPENED
*/  /* BROWSE Br_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Agregar
/* Query rebuild information for FRAME F_Agregar
     _Query            is NOT OPENED
*/  /* FRAME F_Agregar */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cerradas
/* Query rebuild information for FRAME F_Cerradas
     _Query            is NOT OPENED
*/  /* FRAME F_Cerradas */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Consulta
/* Query rebuild information for FRAME F_Consulta
     _Query            is NOT OPENED
*/  /* FRAME F_Consulta */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Cuantia
/* Query rebuild information for FRAME F_Cuantia
     _TblList          = "bdcentral.Saro"
     _Query            is OPENED
*/  /* FRAME F_Cuantia */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Estado
/* Query rebuild information for FRAME F_Estado
     _TblList          = "bdcentral.Saro"
     _Query            is OPENED
*/  /* FRAME F_Estado */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_HojaVida
/* Query rebuild information for FRAME F_HojaVida
     _Query            is NOT OPENED
*/  /* FRAME F_HojaVida */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Instancias
/* Query rebuild information for FRAME F_Instancias
     _Query            is NOT OPENED
*/  /* FRAME F_Instancias */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Producto
/* Query rebuild information for FRAME F_Producto
     _TblList          = "bdcentral.Saro"
     _Query            is NOT OPENED
*/  /* FRAME F_Producto */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Saro
/* Query rebuild information for FRAME F_Saro
     _Query            is NOT OPENED
*/  /* FRAME F_Saro */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Sarouno
/* Query rebuild information for FRAME F_Sarouno
     _TblList          = "bdcentral.Mov_Instancias,bdcentral.Saro WHERE bdcentral.Mov_Instancias ..."
     _TblOptList       = ",,"
     _JoinCode[2]      = "Saro.Num_Referencia = Mov_Instancias.Num_Solicitud"
     _Query            is NOT OPENED
*/  /* FRAME F_Sarouno */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _QUERY-BLOCK FRAME F_Ultima
/* Query rebuild information for FRAME F_Ultima
     _Query            is NOT OPENED
*/  /* FRAME F_Ultima */
&ANALYZE-RESUME

 


/* **********************  Create OCX Containers  ********************** */

&ANALYZE-SUSPEND _CREATE-DYNAMIC

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN

CREATE CONTROL-FRAME CtrlFrame ASSIGN
       FRAME           = FRAME F_Saro:HANDLE
       ROW             = 2.27
       COLUMN          = 3
       HEIGHT          = 1.35
       WIDTH           = 5
       HIDDEN          = yes
       SENSITIVE       = yes.

PROCEDURE adm-create-controls:
      CtrlFrame:NAME = "CtrlFrame":U .
/* CtrlFrame OCXINFO:CREATE-CONTROL from: {F0B88A90-F5DA-11CF-B545-0020AF6ED35A} type: PSTimer */
      CtrlFrame:MOVE-AFTER(BUTTON-1:HANDLE IN FRAME F_Saro).

END PROCEDURE.

&ENDIF

&ANALYZE-RESUME /* End of _CREATE-DYNAMIC */


/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* Proceso de SARO */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* Proceso de SARO */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Producto wWin
ON ENTRY OF FRAME F_Producto /* Productos */
DO:
  DISABLE Tip_Credito.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F_Sarouno
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F_Sarouno wWin
ON ENTRY OF FRAME F_Sarouno /* Identificación de Riesgo SARO */
DO:
    /* RUN Instancia_Hidden. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Hoja_Vida.Asunto_Cumplido
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Asunto_Cumplido wWin
ON VALUE-CHANGED OF Hoja_Vida.Asunto_Cumplido IN FRAME F_HojaVida /* Asunto Cumplido */
DO:
  ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Cerradas
&Scoped-define FRAME-NAME F_Cerradas
&Scoped-define SELF-NAME Br_Cerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cerradas wWin
ON MOUSE-SELECT-DBLCLICK OF Br_Cerradas IN FRAME F_Cerradas
DO:
  APPLY "choose" TO Btn_OutCerradas IN FRAME F_Cerradas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Cerradas wWin
ON ROW-DISPLAY OF Br_Cerradas IN FRAME F_Cerradas
DO:
  IF TCerradas.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) OR
     TCerradas.Fec_Retiro EQ ? THEN DO:
     Tcerradas.Instancia:FGCOL IN BROWSE BR_Cerradas = 12.
     TCerradas.INom_Instancia:FGCOL IN BROWSE BR_Cerradas = 12.
     TCerradas.Usuario:FGCOL IN BROWSE BR_Cerradas = 12.
     TCerradas.INom_Usuario:FGCOL IN BROWSE BR_Cerradas = 12.
     TCerradas.Fec_Retiro:FGCOL IN BROWSE BR_Cerradas = 12.
     TCerradas.Descripcion:FGCOL IN BROWSE BR_Cerradas = 12.
  END.
  ELSE DO:
     Tcerradas.Instancia:FGCOL IN BROWSE BR_Cerradas = 0.
     TCerradas.INom_Instancia:FGCOL IN BROWSE BR_Cerradas = 0.
     TCerradas.Usuario:FGCOL IN BROWSE BR_Cerradas = 0.
     TCerradas.INom_Usuario:FGCOL IN BROWSE BR_Cerradas = 0.
     TCerradas.Fec_Retiro:FGCOL IN BROWSE BR_Cerradas = 0.
     TCerradas.Descripcion:FGCOL IN BROWSE BR_Cerradas = 0.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_ConHV
&Scoped-define FRAME-NAME F_ConHV
&Scoped-define SELF-NAME Br_ConHV
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_ConHV wWin
ON MOUSE-SELECT-DBLCLICK OF Br_ConHV IN FRAME F_ConHV
DO:
  APPLY "choose" TO Btn_OutConHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define BROWSE-NAME Br_Consulta
&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Br_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Consulta wWin
ON MOUSE-SELECT-DBLCLICK OF Br_Consulta IN FRAME F_Consulta
DO:
  APPLY "choose" TO Btn_OutConsulta. 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Br_Consulta wWin
ON ROW-DISPLAY OF Br_Consulta IN FRAME F_Consulta
DO:
  IF Consulta.Estado EQ 4 THEN DO:
     ASSIGN Consulta.Num_Referencia:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.AgeSaro:BGCOL IN BROWSE Br_Consulta = 12 
            Consulta.Usuario:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 12.
     ASSIGN Consulta.Num_Referencia:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.AgeSaro:FGCOL IN BROWSE Br_Consulta = 15 
            Consulta.Usuario:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Nombre:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Fec_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Hor_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Monto:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Vigencia:FGCOL IN BROWSE Br_Consulta = 15.
  END.


  IF Consulta.Vigencia LE (W_VigIns / 2) THEN DO:
     ASSIGN Consulta.Num_Referencia:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.AgeSaro:BGCOL IN BROWSE Br_Consulta = 10 
            Consulta.Usuario:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 10
            Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 10.
     ASSIGN Consulta.Num_Referencia:FGCOL IN BROWSE Br_Consulta = 0
            Consulta.AgeSaro:FGCOL IN BROWSE Br_Consulta = 0 
            Consulta.Usuario:FGCOL IN BROWSE Br_Consulta = 0
            Consulta.Nombre:FGCOL IN BROWSE Br_Consulta = 0
            Consulta.Fec_Ingreso:FGCOL IN BROWSE Br_Consulta = 0
            Consulta.Hor_Ingreso:FGCOL IN BROWSE Br_Consulta = 0
            Consulta.Monto:FGCOL IN BROWSE Br_Consulta = 0
            Consulta.Vigencia:FGCOL IN BROWSE Br_Consulta = 0.
  END.

  IF Consulta.Vigencia GT (W_VigIns / 2) AND Consulta.Vigencia LE W_VigIns THEN DO:
     ASSIGN Consulta.Num_Referencia:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.AgeSaro:BGCOL IN BROWSE Br_Consulta = 14 
            Consulta.Usuario:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 14
            Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 14.
  END.

  IF Consulta.Vigencia GT W_VigIns THEN DO:
     ASSIGN Consulta.Num_Referencia:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.AgeSaro:BGCOL IN BROWSE Br_Consulta = 12 
            Consulta.Usuario:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Nombre:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Fec_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Hor_Ingreso:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Monto:BGCOL IN BROWSE Br_Consulta = 12
            Consulta.Vigencia:BGCOL IN BROWSE Br_Consulta = 12.
     ASSIGN Consulta.Num_Referencia:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.AgeSaro:FGCOL IN BROWSE Br_Consulta = 15 
            Consulta.Usuario:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Nombre:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Fec_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Hor_Ingreso:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Monto:FGCOL IN BROWSE Br_Consulta = 15
            Consulta.Vigencia:FGCOL IN BROWSE Br_Consulta = 15.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_AgregarTxt
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_AgregarTxt wWin
ON CHOOSE OF Btn_AgregarTxt IN FRAME F_Instancias /* Agregar Texto */
DO:
  IF NOT W_NvaHV THEN DO:
     
     Id_Agregar = "IN".
     E_Agregar:SCREEN-VALUE IN FRAME F_Agregar = "".
     ENABLE ALL WITH FRAME F_Agregar.
     VIEW FRAME F_Agregar.
     APPLY "entry" TO E_Agregar IN FRAME F_Agregar.
     RETURN NO-APPLY.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Saro
&Scoped-define SELF-NAME Btn_Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cancelar wWin
ON CHOOSE OF Btn_Cancelar IN FRAME F_Saro /* Cancelar */
DO:
  W_Nuevo = NO.
  IF W_Primera EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_saro,1,5)) THEN
      ENABLE Btn_Ingresar WITH FRAME F_saro.
  ELSE
      DISABLE Btn_Ingresar WITH FRAME F_saro.

  FIND Saro WHERE ROWID(Saro) EQ Puntero NO-LOCK NO-ERROR.
  IF AVAILABLE Saro THEN DO: 
     RUN Mostrar_Saro.
     /*DISABLE Saro.Nit WITH FRAME F_Sarouno.*/
  END.
  ELSE DO:
     MESSAGE "No se ha encontrado el Evento" VIEW-AS ALERT-BOX.
     APPLY "choose" TO Btn_Consulta IN FRAME F_saro.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Consulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Consulta wWin
ON CHOOSE OF Btn_Consulta IN FRAME F_Saro /* Button 10 */
DO:
  IF W_Nuevo THEN
     RETURN.

  APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Saro.
  
  FRAME F_Consulta:HIDDEN = NO.
  
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Sarouno
&Scoped-define SELF-NAME Btn_Cuantia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Cuantia wWin
ON CHOOSE OF Btn_Cuantia IN FRAME F_Sarouno /* Cuantias */
DO:
    VIEW FRAME F_Cuantia.
    IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 640 THEN DO:
       DISABLE ALL WITH FRAME F_Cuantia.
       ENABLE Btn_OutScoringCuantia WITH FRAME f_cUANTIA.
    END.
    ELSE
       ENABLE ALL WITH FRAME F_Cuantia.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Saro
&Scoped-define SELF-NAME Btn_Deshacer
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Deshacer wWin
ON CHOOSE OF Btn_Deshacer IN FRAME F_Saro /* Deshacer */
DO:
  FIND Saro WHERE ROWID(Saro) EQ puntero NO-LOCK NO-ERROR.
  IF AVAILABLE Saro THEN 
     RUN Mostrar_Saro.
  IF W_Primera EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_saro,1,5)) THEN
    ENABLE Btn_Ingresar WITH FRAME F_saro.
  ELSE
    DISABLE Btn_Ingresar WITH FRAME F_saro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Sarouno
&Scoped-define SELF-NAME Btn_Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Estado wWin
ON CHOOSE OF Btn_Estado IN FRAME F_Sarouno /* Estado del Evento */
DO:
  VIEW FRAME F_Estado.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_GraInstancia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_GraInstancia wWin
ON CHOOSE OF Btn_GraInstancia IN FRAME F_Instancias /* Grabar */
DO:
  DO TRANSACTION:
        IF Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias EQ "" THEN DO:
            MESSAGE "No se puede grabar si no se ha entrado" SKIP
                    "el concepto de la instancia. Entre el Concepto!"
            VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO Mov_Instancias.Descripcion IN FRAME F_Instancias.
            RETURN NO-APPLY.
        END.

        MESSAGE "                  Segura(o) de Procesar la Instancia...?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR SALVAR"
        UPDATE W_SiNos AS LOGICAL.
            
        IF NOT W_SiNoS THEN
            RETURN.

        FIND FIRST clientes WHERE clientes.nit = saro.nit
        NO-LOCK NO-ERROR.
            
        FIND FIRST Mov_Instancias WHERE Mov_Instancias.Instancia = INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5))
                                    AND Mov_Instancias.Nit       = Saro.Nit
                                    AND Mov_Instancias.Num_Solicitud EQ INTEGER(Saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno) 
                                    AND Mov_Instancias.Usuario   = W_Usuario
                                    AND Mov_Instancias.Estado    = NO
        NO-ERROR.

        IF W_Negadas = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5))
            THEN DO:
            MESSAGE "El Evento negado dejara de aparecer en las consultas"
                VIEW-AS ALERT-BOX.
            ASSIGN Mov_Instancias.Fec_Retiro = W_Fecha
                   Mov_Instancias.Estado     = YES.
            FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
            APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Saro.
            RELEASE saro.
        END.
        ELSE DO:
            IF AVAILABLE Mov_Instancias THEN DO:
                ASSIGN FRAME F_Instancias
                       Mov_Instancias.Descripcion.
                FIND TCerradas WHERE TCerradas.Instancia     = INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5))
                                 AND TCerradas.Num_Solicitud = INT(Saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno) 
                NO-ERROR.
                IF AVAILABLE TCerradas THEN
                    ASSIGN TCerradas.Descripcion = Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias.

                ASSIGN Mov_Instancias.Fec_Retiro = ?
                       Mov_Instancias.Hora_Retiro = 0.

                    
                IF Mov_Instancias.Estado:SCREEN-VALUE IN FRAME F_Instancias EQ "YES" THEN DO:
                       
                    FIND FIRST Hoja_Vida WHERE Hoja_Vida.Tipo       EQ 9 
                                           AND Hoja_Vida.Codigo     EQ 1 
                                           AND Hoja_Vida.Instancia  EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_saro,1,5)) 
                                           AND Hoja_Vida.DoctoRefer EQ INTEGER(Saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno) 
                                           AND Hoja_Vida.Nit        EQ Clientes.Nit
                                           AND Hoja_Vida.Asunto_Cumplido EQ NO 
                                           AND Hoja_Vida.Usuario   EQ W_Usuario NO-ERROR.     
                    IF AVAILABLE Hoja_Vida THEN DO:    
                        Mov_Instancias.Estado = NO.
                        MESSAGE "El Evento aun tiene Asuntos pendientes por resolver" SKIP
                                "en la hoja de vida. No se permite pasar a la siguiente" SKIP
                                "instancia si estos asuntos no se han cumplido." SKIP(1)
                                "Desea ver los asuntos por cumplir de la Solicitud?"
                            VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.
                        HIDE FRAME F_Instancias.
                        VIEW FRAME F_Sarouno.
                        ENABLE ALL WITH FRAME F_saro.
                        DISABLE NomUsuario WITH FRAME F_Saro.
                        /*IF choice THEN DO: 
                            APPLY "choose" TO Btn_HojaVida IN FRAME F_Sarouno.
                            APPLY "entry" TO Hoja_Vida.Observacion IN FRAME F_HojaVida.
                            RETURN NO-APPLY.
                        END.
                        ELSE DO:
                            APPLY "entry" TO Btn_HojaVida IN FRAME F_Sarouno.
                            RETURN NO-APPLY.
                        END. */
                    END.                                                                                                                    
                    ELSE DO:
                        HIDE FRAME F_Instancias.
                        IF W_Ultima NE INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
                /*012005*/ W_Negadas NE INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) THEN DO:
                            RUN Asignar_Proxima_Instancia NO-ERROR.
                            IF ERROR-STATUS:ERROR THEN DO:
                                UNDO.
                            END.
    
                            FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
    
                            FOR EACH Mov_Instancias WHERE
                                     Mov_Instancias.Instancia EQ INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
                                     Mov_Instancias.Nit       EQ Saro.nit AND
                                     Mov_Instancias.Num_Solicitud EQ INTEGER(saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno) AND
                                     Mov_Instancias.Usuario   EQ W_Usuario:
                                ASSIGN Mov_Instancias.Fec_Retiro  = W_Fecha
                                       Mov_Instancias.Hora_Retiro = TIME
                                       Mov_Instancias.Estado      = YES
                                       Mov_Instancias.Agencia     = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Sarouno,1,3)).
                            END.
                            
                            FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
                            RELEASE Mov_Instancias.
                            APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Saro.
                            RELEASE saro.
                        END.
                        ELSE DO:
                            APPLY "value-changed" TO saro.Estado IN FRAME F_Estado.
                            ENABLE ALL WITH FRAME F_Ultima.
             
                            VIEW FRAME F_Ultima.
            
                            APPLY "entry" TO saro.Estado IN FRAME F_Estado.
                            RETURN NO-APPLY.
                        END. 
                            
                    END. /*FIN else do*/
                END. /*IF Mov instancia.estado*/
                FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
            END. /*Fin available Mov Ins*/
            ELSE DO:                                                                               
                MESSAGE "No se encontro la instancia con" SKIP
                        "Instancia: " Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro SKIP
                        "Nit: " clientes.Nit                                      SKIP 
                        "NumSolicitud: " saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno
                    VIEW-AS ALERT-BOX ERROR.
            END.
        END. /*else do principal negadas */
  END.  /*Transaccion*/
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Btn_Imp
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Imp wWin
ON CHOOSE OF Btn_Imp IN FRAME F_Instancias /* Imprimir */
DO:
/*   DEFINE VAR Listado AS CHARACTER FORM "X(35)" INITIAL "".                                                    */
/*   ASSIGN Listado   = W_Pathspl + "ComentInst2-" + W_Usuario + STRING(RANDOM(2000,10000))  + ".lst"            */
/*          W_SiMInst = TRUE.                                                                                    */
/*                                                                                                               */
/*   FIND FIRST Mov_Instancias WHERE                                                                             */
/*       Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND */
/*       Mov_Instancias.Nit       EQ STRING(saro.Nit:SCREEN-VALUE IN FRAME F_Sarouno) AND                 */
/*       Mov_Instancias.Num_Solicitud EQ DECIMAL(saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno) AND  */
/*       Mov_Instancias.Usuario   EQ W_Usuario AND                                                               */
/*       Mov_Instancias.Estado    EQ NO                                                                          */
/*       NO-LOCK NO-ERROR.                                                                                       */
/*                                                                                                               */
/*   {Incluido/Imprimir.i "Listado"}.                                                                            */
/*                                                                                                               */
/*   W_SiMInst = FALSE.                                                                                          */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Saro
&Scoped-define SELF-NAME Btn_Ingresar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Ingresar wWin
ON CHOOSE OF Btn_Ingresar IN FRAME F_Saro /* Ingresar */
DO:
  DO WITH FRAME F_Sarouno:
      DISABLE Btn_Ingresar WITH FRAME F_Saro.
      FRAME F_Consulta:HIDDEN = YES.
      IF FRAME F_Consulta:HIDDEN EQ YES THEN
         HIDE FRAME F_Consulta.
      IF FRAME F_Sarouno:HIDDEN EQ YES THEN DO:
         VIEW FRAME F_Sarouno.
      END.

      IF AVAILABLE Saro THEN 
        Puntero = ROWID(Saro).
      
      W_Nuevo = YES. 
    
      RELEASE Saro.

      RUN Inicializar_Variables.
      ENABLE {&List-1} WITH FRAME F_Sarouno.

      IF W_SucAgen THEN DO:
         /*ENABLE Cmb_Agencias WITH FRAME F_Sarouno.*/
         APPLY "entry" TO Cmb_Agencias.
         RETURN NO-APPLY.
      END.
      ELSE DO:
         DISABLE Cmb_Agencias WITH FRAME F_Sarouno.
         RETURN NO-APPLY.
      END. 
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Btn_insVolver
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_insVolver wWin
ON CHOOSE OF Btn_insVolver IN FRAME F_Instancias /* Button 135 */
DO:
  ENABLE ALL WITH FRAME F_Saro.
  DISABLE NomUsuario WITH FRAME F_Saro.
  HIDE FRAME F_Instancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Sarouno
&Scoped-define SELF-NAME Btn_LnOperativas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_LnOperativas wWin
ON CHOOSE OF Btn_LnOperativas IN FRAME F_Sarouno /* Lineas Operativas */
DO:
    VIEW FRAME F_LinOperativas.
    IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 640 THEN DO:
       DISABLE ALL WITH FRAME F_LinOperativas.
       ENABLE Btn_OutScoring-2 WITH FRAME F_LinOperativas.
    END.
    ELSE
       ENABLE ALL WITH FRAME F_LinOperativas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Btn_NvoHv
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_NvoHv wWin
ON CHOOSE OF Btn_NvoHv IN FRAME F_HojaVida /* Ingresar */
DO:
  ENABLE Btn_SalvaHV Hoja_Vida.Asunto_Cumplido Hoja_Vida.Observacion WITH FRAME F_HojaVida.
  DISABLE Btn_NvoHV WITH FRAME F_HojaVida.
  ASSIGN Hoja_Vida.Asunto_Cumplido:SCREEN-VALUE = "no"
         Hoja_Vida.Observacion:SCREEN-VALUE     = ""
         Hoja_Vida.Fec_Grabacion:SCREEN-VALUE   = STRING(w_fecha).
  HIDE FRAME F_ConHV.
  W_NvaHV = YES.
  Hoja_Vida.Observacion:READ-ONLY = NO.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cerradas
&Scoped-define SELF-NAME Btn_OutCerradas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutCerradas wWin
ON CHOOSE OF Btn_OutCerradas IN FRAME F_Cerradas /* Button 143 */
DO:
  IF AVAILABLE TCerradas THEN DO:
     FIND Mov_Instancias WHERE 
          Mov_Instancias.Instancia EQ TCerradas.Instancia AND
          Mov_Instancias.Usuario EQ TCerradas.Usuario NO-LOCK NO-ERROR.
     IF AVAILABLE Mov_Instancias THEN DO:
       ASSIGN Mov_Instancias.Estado:SCREEN-VALUE IN FRAME F_Instancias = STRING(Mov_Instancias.Estado)
              Mov_Instancias.Fec_Ingreso:SCREEN-VALUE = STRING(Mov_Instancias.Fec_Ingreso)
              WHora_Ingreso:SCREEN-VALUE = STRING(Mov_Instancia.Hora_Ingreso,"HH:MM:SS AM")
              Mov_Instancia.Descripcion:SCREEN-VALUE = Mov_Instancia.Descripcion
              Mov_Instancias.Fec_Retiro:SCREEN-VALUE = ""
              W_instancia:SCREEN-VALUE = Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro
              W_UsuarioInstancia:SCREEN-VALUE = NomUsuario:SCREEN-VALUE IN FRAME F_Saro.
       Vigencia:SCREEN-VALUE = STRING(w_fecha - TCerradas.Fec_Ingreso) + " Dias".
       IF Mov_Instancias.Estado EQ YES THEN 
         DISABLE Mov_Instancias.Estado /*Mov_Instancias.Descripcion*/ Btn_GraInstancia WITH FRAME F_Instancias.
       ELSE
         ENABLE Mov_Instancias.Estado Mov_Instancias.Descripcion Btn_GraInstancia WITH FRAME F_Instancias.
     END.
          
  END.
  HIDE FRAME F_Cerradas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Btn_OutConsulta
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutConsulta wWin
ON CHOOSE OF Btn_OutConsulta IN FRAME F_Consulta /* Button 133 */
DO:
    IF AVAILABLE Consulta 
    THEN DO:
        RELEASE saro.
        FIND FIRST saro 
            WHERE 
                Saro.Num_Referencia = Consulta.Num_Referencia
            AND saro.Estado        EQ 3 NO-ERROR.
        IF AVAIL(saro) 
        THEN DO:
            MESSAGE "El Evento YA FUE NEGADA, Desea Reconsinderar el Estado...? " 
                    VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE choice.
            IF Choice 
            THEN ASSIGN saro.Estado = 1.
            ELSE RETURN.
        END.
        FIND FIRST saro 
            WHERE 
                Saro.Num_Referencia = Consulta.Num_Referencia
            AND saro.Estado        NE 3 NO-ERROR.
        IF LOCKED saro 
        THEN DO:
            MESSAGE "Este Evento esta siendo accesada por otro" SKIP
                    "asesor. por este motivo no podra ser modificada"
                VIEW-AS ALERT-BOX INFORMATION.
            APPLY "choose" TO Btn_Consulta IN FRAME F_Saro.
        END.
        ELSE DO:
            IF AVAILABLE saro 
            THEN  RUN Mostrar_saro.
            ELSE  RETURN.
        END.
        HIDE FRAME F_Consulta.
        VIEW FRAME F_Sarouno.
    END.
    ELSE DO:
        IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) EQ 790 
        THEN DO:
            MESSAGE "No Hay evento seleccionada para trabajar o" SKIP
                    "el usuario no tiene eventos pendientes " SKIP
                    "por procesar en esta instancia." SKIP(1)
                    "Desea crear un Evento?" VIEW-AS ALERT-BOX WARNING BUTTONS YES-NO UPDATE choice.
            IF choice THEN APPLY "choose" TO Btn_Ingresar.
        END.
        ELSE DO:
            MESSAGE "No Hay Evento seleccionada para trabajar o" SKIP
                    "el usuario no tiene Eventos pendientes " SKIP
                    "por procesar en esta instancia." VIEW-AS ALERT-BOX WARNING.
        END.
    END.
    RUN Instancia_Hidden.        
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Producto
&Scoped-define SELF-NAME Btn_OutProductos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutProductos wWin
ON CHOOSE OF Btn_OutProductos IN FRAME F_Producto /* x */
DO:
  DO WITH FRAME F_Producto:
     Nom_Producto:SCREEN-VALUE IN FRAME F_Sarouno = Cmb_Productos:SCREEN-VALUE.
  END.
    
  HIDE FRAME F_Producto.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_InfoProducto
&Scoped-define SELF-NAME Btn_OutScoring
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutScoring wWin
ON CHOOSE OF Btn_OutScoring IN FRAME F_InfoProducto /* Button 121 */
DO:
  ENABLE ALL WITH FRAME F_Saro.
  DISABLE NomUsuario WITH FRAME F_Saro.
  HIDE FRAME F_InfoProducto.
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_LinOperativas
&Scoped-define SELF-NAME Btn_OutScoring-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutScoring-2 wWin
ON CHOOSE OF Btn_OutScoring-2 IN FRAME F_LinOperativas /* Btn_outscoring 2 */
DO:
  HIDE FRAME F_LinOperativas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Estado
&Scoped-define SELF-NAME Btn_OutScoring-3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutScoring-3 wWin
ON CHOOSE OF Btn_OutScoring-3 IN FRAME F_Estado /* Btn_outscoring 2 */
DO:
  HIDE FRAME F_Estado.
  ENABLE ALL WITH FRAME F_Saro.
  DISABLE NomUsuario Btn_Ingresar WITH FRAME F_Saro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cuantia
&Scoped-define SELF-NAME Btn_OutScoringCuantia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutScoringCuantia wWin
ON CHOOSE OF Btn_OutScoringCuantia IN FRAME F_Cuantia /* Btn_outscoringCuantia */
DO:
  HIDE FRAME F_LinOperativas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME Btn_OutUltima
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_OutUltima wWin
ON CHOOSE OF Btn_OutUltima IN FRAME F_Ultima /* Button 153 */
DO:
  HIDE FRAME F_Ultima.
  
  ENABLE ALL WITH FRAME F_Saro.
  DISABLE NomUsuario Btn_Ingresar WITH FRAME F_Saro.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Saro
&Scoped-define SELF-NAME Btn_ProcesarInstancia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_ProcesarInstancia wWin
ON CHOOSE OF Btn_ProcesarInstancia IN FRAME F_Saro /* Procesar Instancia */
DO:
 DEFI VAR W_NroDias AS INTEG FORM "99999".

 IF FRAME F_Sarouno:HIDDEN EQ YES THEN DO:
    MESSAGE "Al momento no existe ningun evento a la cual" SKIP
            "se le pueda procesar la instancia." SKIP(1)
            "Escoja de la lista de consulta un Evento" SKIP
            "haciendo doble click sobre ella" VIEW-AS ALERT-BOX INFORMATION.
    APPLY "entry" TO Cmb_Instancias IN FRAME F_Saro.
    RETURN NO-APPLY.
 END.
 
 OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK INDEXED-REPOSITION.
 IF Br_Consulta:NUM-ENTRIES IN FRAME F_Consulta EQ 0 THEN DO:
    MESSAGE "Al momento no existe ningun Evento a la cual" SKIP
            "se le pueda procesar la instancia." VIEW-AS ALERT-BOX INFORMATION.
    APPLY "entry" TO Cmb_Instancias IN FRAME F_Saro.
    RETURN NO-APPLY.
 END.
 /* MESSAGE saro.Nit
     VIEW-AS ALERT-BOX INFO BUTTONS OK. */
 FIND Clientes WHERE Clientes.Nit EQ saro.Nit NO-LOCK NO-ERROR. 

 HIDE FRAME F_Consultas.
 DISABLE ALL WITH FRAME F_Saro.
 IF AVAILABLE Saro THEN DO:
    FIND Mov_Instancias WHERE 
         Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
         Mov_Instancias.Nit       EQ Clientes.Nit AND
         Mov_Instancias.Num_Solicitud EQ INTEGER(Saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno) AND
         Mov_Instancias.Estado    EQ NO NO-LOCK NO-ERROR.
    IF AVAILABLE Mov_Instancias THEN DO:
       DO WITH FRAME F_Instancias:
          ASSIGN Mov_Instancias.Estado:SCREEN-VALUE = STRING(Mov_Instancias.Estado)
                 Mov_Instancias.Fec_Ingreso:SCREEN-VALUE = STRING(Mov_Instancias.Fec_Ingreso)
                 WHora_Ingreso:SCREEN-VALUE = STRING(Mov_Instancia.Hora_Ingreso,"HH:MM:SS AM")
                 Mov_Instancia.Descripcion:SCREEN-VALUE = Mov_Instancia.Descripcion
                 Mov_Instancias.Fec_Retiro:SCREEN-VALUE = ""
                 W_instancia:SCREEN-VALUE = Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro
                 W_UsuarioInstancia:SCREEN-VALUE = NomUsuario:SCREEN-VALUE IN FRAME F_Saro.
          Vigencia:SCREEN-VALUE = STRING(w_fecha - Mov_Instancias.Fec_Ingreso) + " Dias".
          ENABLE Mov_Instancias.Estado Mov_Instancias.Descripcion
                 Btn_GraInstancia Btn_InsVolver WITH FRAME F_Instancias.
          RUN Buscar_Instancias_Cerradas.
       END.

       VIEW FRAME F_Instancias.
       APPLY "entry" TO Mov_Instancias.Estado IN FRAME F_Instancias.
       RETURN NO-APPLY.
    END.
    ELSE DO:
      MESSAGE "El evento no esta disponible" SKIP
              "o no se ha cerrado la instancia anterior" SKIP
              "escoja un Evento de la lista de " SKIP
              "Evento disponibles!" VIEW-AS ALERT-BOX WARNING.
      ENABLE ALL WITH FRAME F_Saro.
      DISABLE NomUsuario WITH FRAME F_Saro.
    END.
 END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Sarouno
&Scoped-define SELF-NAME Btn_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Producto wWin
ON CHOOSE OF Btn_Producto IN FRAME F_Sarouno
DO:
  VIEW FRAME F_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cuantia
&Scoped-define SELF-NAME Btn_Salir_Cuantia
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir_Cuantia wWin
ON CHOOSE OF Btn_Salir_Cuantia IN FRAME F_Cuantia /* Salir sin escoger */
DO:
  HIDE FRAME F_LinOperativas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_LinOperativas
&Scoped-define SELF-NAME Btn_Salir_Linea
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir_Linea wWin
ON CHOOSE OF Btn_Salir_Linea IN FRAME F_LinOperativas /* Salir sin escoger */
DO:
  HIDE FRAME F_LinOperativas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Estado
&Scoped-define SELF-NAME Btn_Salir_Linea-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir_Linea-2 wWin
ON CHOOSE OF Btn_Salir_Linea-2 IN FRAME F_Estado /* Salir sin escoger */
DO:
  HIDE FRAME F_LinOperativas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Producto
&Scoped-define SELF-NAME Btn_Salir_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salir_Producto wWin
ON CHOOSE OF Btn_Salir_Producto IN FRAME F_Producto /* Salir sin escoger */
DO:
  HIDE FRAME F_Producto.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Saro
&Scoped-define SELF-NAME Btn_Salvar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Salvar wWin
ON CHOOSE OF Btn_Salvar IN FRAME F_Saro /* Salvar */
DO:
    DO WITH FRAME F_Sarouno:
        
        /*fin validaciones pantalla*/
        IF W_Nuevo THEN DO: 
            CREATE Saro.
            ASSIGN Saro.Num_Referencia = NEXT-VALUE(Sec_Saro)
                   Saro.Estado        = 1
                   Saro.Usuario       = W_Usuario.

            ASSIGN Saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno = STRING(saro.Num_Referencia).
        END.
        ELSE DO:
            FIND FIRST saro WHERE saro.Agencia        = INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3)) 
                              AND saro.Num_Referencia = INTEGER(saro.Num_Referencia:SCREEN-VALUE)  
            EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE saro THEN DO:
                MESSAGE "No existe el Evento a la cual se le" SKIP
                        "quieren hacer los cambios. Rectifique" SKIP
                        "EL Evento consultandola y haciendo" SKIP
                        "de nuevo los cambios!" VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO Cmb_Instancias IN FRAME F_Saro.
                RETURN.
            END.
        END.

        ASSIGN FRAME F_Sarouno
                     saro.Fec_Grabacion
                     Saro.Hora_Grabacion
                     saro.Fec_Des_even
                     Saro.Hora_Des_Even 
                     saro.Descrip_evento
                     saro.Agencia          = INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE,1,3))
                     Saro.Cod_Evento       = STRING(v-cod-factorO)  /*evento */
                     saro.Clase_ROperativo = INT(SUBSTRING(W-ClaseRiesgo:SCREEN-VALUE,1,3))
                     saro.Clase_Proceso    = INT(SUBSTRING(Cmb_Proceso:SCREEN-VALUE,1,3))
                     Saro.Canal_Servicio
                     Saro.Zona_Geografica  = INT(SUBSTRING(Cmb_zonas:SCREEN-VALUE,11,3))
                     Saro.Tip_perdida      = INT(Cmb_Generacion:SCREEN-VALUE).

                     


        /*Ubicacion */
        Saro.Cod_Factor1 = ''.
        DO i = 1 TO  LENGTH(Nivel3:SCREEN-VALUE) :
          IF SUBSTRING(Nivel3:SCREEN-VALUE,i,1) = "-" THEN LEAVE.
             Saro.Cod_Factor1 = Saro.Cod_Factor1 + SUBSTRING(Nivel3:SCREEN-VALUE,i,1).

        END.

        /*Factor de Riesgo*/
        Saro.Cod_Factor2 = ''.
        DO i = 1 TO  LENGTH(Nivel2:SCREEN-VALUE) :
          IF SUBSTRING(Nivel2:SCREEN-VALUE,i,1) = "-" THEN LEAVE.
             Saro.Cod_Factor2 = Saro.Cod_Factor2 + SUBSTRING(Nivel2:SCREEN-VALUE,i,1).

        END.

         /*                               
        MESSAGE 'tipo perdida ' Cmb_Generacion 
                'factor 1' Saro.Cod_Factor1 'factor 2' Saro.Cod_Factor2
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
           */

        IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) > 610 THEN
            ASSIGN FRAME F_Sarouno       
                        Saro.Fec_Ini_Even   
                        Saro.Hora_Ini_Even.
                        

        IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) > 620 THEN
            ASSIGN FRAME F_Sarouno       
                        Saro.Fec_Fin_Even 
                        Saro.Hora_Fin_Even
                        Saro.Tip_perdida = INT(Cmb_Generacion:SCREEN-VALUE).
                    
    END. /*fin frame sarouno*/
   
    DO WITH FRAME F_Producto:
        ASSIGN saro.Clase_producto = 1
               saro.Clase_producto = INT(Saro.Clase_Producto:SCREEN-VALUE)
               Saro.Tip_Credito    = INT(Tip_Credito:SCREEN-VALUE)
               saro.Cod_Producto   = INT(SUBSTRING(Cmb_Productos:SCREEN-VALUE,1,3)).
    END.

    DO WITH FRAME F_cuantia:

        /*
        IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) > 610 THEN 
            IF saro.Nit:SCREEN-VALUE EQ "" THEN DO:
                MESSAGE "No se puede Salvar el evento" SKIP
                        "Falta ingresar el Nit" VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO saro.Nit.
                RETURN.
            END.
        
        IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 620 THEN 
            IF DEC(saro.Cuantia_Riesgo:SCREEN-VALUE) EQ 0 THEN DO:
                MESSAGE "No se puede Salvar el evento" SKIP
                        "falta ingresar el Monto del Riesgo" VIEW-AS ALERT-BOX ERROR.
                APPLY "entry" TO saro.Cuantia_Riesgo.
                RETURN.
            END.
        */


        ASSIGN Saro.Nit
               Saro.Cuantia_Riesgo
               Saro.Cuantia_Recup_Tot 
               saro.Cuantia_Recup_Seg 
               Saro.Nit_seguro. 
                        
    END.
    
 
    DO WITH FRAME F_LinOperativas:
     /*
     IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) > 620 THEN
     */
         /*ASSIGN Saro.Lineas_Operat[1] = 
                Saro.Porc_Even[1]
                Saro.Lineas_Operat[2]
                Saro.Porc_Even[2]
                Saro.Lineas_Operat[3] 
                Saro.Porc_Even[3].*/
         
    END.

    DO WITH FRAME F_Estado:
        IF INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) > 630 THEN DO:
            ASSIGN Saro.Estado_Evento
                   Saro.Descrip_Anulado.

            IF Saro.Estado_Evento = 3 THEN
                ASSIGN Saro.Fec_Anu_Even  = TODAY
                        Saro.Hora_Anu_Even = STRING(TIME)
                        Saro.Usuario       = W_Usuario.

            IF Saro.Estado_Evento = 2 THEN
                ASSIGN Saro.Hora_Aprobacion = STRING(TIME)
                        Saro.Fec_Aprobacion  = TODAY
                        Saro.Usu_Anulacion   = W_Usuario.

        END.
    END. /*fin frame estado */
 

    IF W_Nuevo THEN DO:
        CREATE Mov_Instancias.
        ASSIGN Mov_Instancias.Fec_Ingreso    = w_fecha
               Mov_Instancias.Hora_Ingreso   = TIME
               Mov_Instancias.Num_Solicitud  = DECIMAL(saro.Num_Referencia:SCREEN-VALUE)
               Mov_Instancias.Usuario        = W_Usuario.
               Mov_Instancias.Instancia      = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)).
               Mov_Instancias.Agencia        = W_Agencia.

        CREATE Consulta.
        ASSIGN Consulta.Agesaro     = saro.Agencia
           Consulta.Estado          = saro.Estado
           Consulta.Usuario         = Saro.Usuario
           Consulta.Num_Referencia  = Mov_Instancias.Num_Solicitud
           Consulta.Fec_Ingreso     = Mov_Instancias.Fec_Ingreso
           Consulta.Hor_Ingreso     = STRING(Mov_Instancias.Hora_Ingreso,"HH:MM:SS am").

        /* FIND Clientes WHERE Clientes.Nit EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.*/
        FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Mov_Instancias.Usuario NO-LOCK NO-ERROR.
        IF AVAILABLE Usuarios THEN
            ASSIGN Consulta.usuario    = Usuarios.usuario
                   Consulta.Nombre     = Usuarios.Nombre.           
    END. /*fin si es nuevo*/
 
    W_Nuevo = NO.

    FIND FIRST Mov_Instancias WHERE Mov_Instancias.Agencia       = Saro.Agencia
                                AND Mov_Instancias.Instancia     = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5))
                                AND Mov_Instancias.Usuario       = W_Usuario
                                AND Mov_Instancias.Num_Solicitud = Saro.Num_Referencia
    NO-ERROR.
    IF AVAILABLE Mov_Instancias AND Mov_Instancias.Nit <> Saro.nit THEN
        ASSIGN Mov_Instancias.Nit = Saro.nit.

 
    IF W_Primera EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) THEN
        ENABLE Btn_Ingresar WITH FRAME F_Saro.
    ELSE
        DISABLE Btn_Ingresar WITH FRAME F_Saro.
  

END.  /*fin del do principal o del trigrees BTn_salvar*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Ultima
&Scoped-define SELF-NAME Btn_SalvaUltima
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_SalvaUltima wWin
ON CHOOSE OF Btn_SalvaUltima IN FRAME F_Ultima /* Salvar */
DO:
    /*
DO WITH FRAME F_Ultima:
  FIND Clientes WHERE Clientes.Nit    EQ saro.Nit 
                  AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Clientes) THEN DO:
     MESSAGE "Falta el Cliente con Estado Activo para realizar la operaciòn..."
         VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.

  CASE saro.Estado:SCREEN-VALUE:
    WHEN "1" THEN DO:
       MESSAGE "Como es la última instancia, se espera que se tome una" SKIP
               "desición acerca de esta. La Solicitud No puede quedar" SKIP
               "En Estudio. Rectifique su decisión!"
               VIEW-AS ALERT-BOX ERROR.
       APPLY "entry" TO saro.Estado.
       RETURN NO-APPLY.
    END.
    WHEN "2" THEN DO:
       MESSAGE "               Segura(o) de APROBAR la saro...?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Aprobada" 
           UPDATE W_SiConf AS LOGICAL.
       IF NOT W_SiConf THEN
          RETURN.

       control_grabar = TRUE.
       
       IF control_grabar THEN
          RUN Grabar_Credito.
    END.

    WHEN "3" THEN DO:
        MESSAGE "               Segura(o) de NEGAR la saro...?"
           VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Confirmar Negada" 
           UPDATE W_SiNeg AS LOGICAL.
        IF NOT W_SiNeg THEN
          RETURN.
    
        RUN Negar_saro.
    END.

    WHEN "4" THEN DO: 
      FIND FIRST Tuxi WHERE ROWID(Tuxi) EQ W_RowIdTx NO-ERROR.
      IF NOT AVAILABLE Tuxi THEN DO:
         MESSAGE "No se encuentra disponible el usuario" SKIP
            "para condicionar la saro." SKIP
            "No se permite la Operacion." VIEW-AS ALERT-BOX.    
         RETURN.
      END.

    END.
  END CASE.

END.  */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME Buscar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Buscar wWin
ON LEAVE OF Buscar IN FRAME F_Consulta
DO:
  CASE R_Organizar:SCREEN-VALUE:
    WHEN "1" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Num_Referencia EQ DECIMAL(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.Num_Referencia INDEXED-REPOSITION.
    END.
    WHEN "2" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.AgeSaro EQ DECIMAL(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.AgeSaro INDEXED-REPOSITION.
    END.
    WHEN "3" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Nit EQ SELF:SCREEN-VALUE
                NO-LOCK  BY Consulta.Nit INDEXED-REPOSITION.
    END.
    WHEN "4" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Nombre BEGINS SELF:SCREEN-VALUE
                NO-LOCK  BY Consulta.Nombre INDEXED-REPOSITION.
    END.
    WHEN "5" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Fec_Ingreso EQ DATE(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.Fec_ingreso INDEXED-REPOSITION.
    END.
    WHEN "6" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta WHERE
                Consulta.Vigencia EQ INTEGER(SELF:SCREEN-VALUE)
                NO-LOCK  BY Consulta.Vigencia INDEXED-REPOSITION.
    END.
  END CASE.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Saro
&Scoped-define SELF-NAME BUTTON-1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-1 wWin
ON CHOOSE OF BUTTON-1 IN FRAME F_Saro /* Button 1 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME BUTTON-142
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-142 wWin
ON CHOOSE OF BUTTON-142 IN FRAME F_Instancias /* Button 142 */
DO:
  OPEN QUERY Br_Cerradas FOR EACH TCerradas NO-LOCK INDEXED-REPOSITION.
  ENABLE ALL WITH FRAME F_Cerradas.
  VIEW FRAME F_Cerradas.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME BUTTON-149
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-149 wWin
ON CHOOSE OF BUTTON-149 IN FRAME F_HojaVida /* Button 149 */
DO:
  ENABLE ALL WITH FRAME F_Saro.
  DISABLE NomUsuario WITH FRAME F_Saro.
  HIDE FRAME F_HojaVida.
  FRAME F_Consulta:HIDDEN = YES.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-150
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-150 wWin
ON CHOOSE OF BUTTON-150 IN FRAME F_HojaVida /* Button 150 */
DO:
  ENABLE ALL WITH FRAME F_conHV.
  
 VIEW FRAME F_ConHV.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-152
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-152 wWin
ON CHOOSE OF BUTTON-152 IN FRAME F_HojaVida /* Cancelar */
DO:
  DISABLE Btn_SalvaHV WITH FRAME F_HojaVida.
  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Agregar
&Scoped-define SELF-NAME BUTTON-153
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON CHOOSE OF BUTTON-153 IN FRAME F_Agregar /* Button 153 */
DO:
  HIDE FRAME F_Agregar.
  IF E_Agregar:SCREEN-VALUE NE "" THEN DO:
     IF Id_Agregar EQ "HV" THEN DO:
        Hoja_Vida.Observacion:SCREEN-VALUE IN FRAME F_HojaVida = Hoja_Vida.Observacion:SCREEN-VALUE + 
        ". Fecha: " + STRING(w_fecha) + " :" + E_Agregar:SCREEN-VALUE IN FRAME F_Agregar. 
        ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
     END.
     IF Id_Agregar EQ "IN" THEN DO:
        FIND FIRST Mov_Instancias WHERE 
                   Mov_Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
                   Mov_Instancias.Usuario   EQ W_Usuario AND 
                   Mov_Instancias.Estado    EQ NO NO-ERROR.  
        Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias = 
            Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias + 
            ". Fecha: " + STRING(W_Fecha) + " :" + E_Agregar:SCREEN-VALUE IN FRAME F_Agregar.
        ASSIGN Mov_Instancias.Descripcion.
        FIND CURRENT Mov_Instancias NO-LOCK NO-ERROR.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON ENTRY OF BUTTON-153 IN FRAME F_Agregar /* Button 153 */
DO:
  ON RETURN RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-153 wWin
ON LEAVE OF BUTTON-153 IN FRAME F_Agregar /* Button 153 */
DO:
  ON RETURN TAB.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cerradas
&Scoped-define SELF-NAME BUTTON-154
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-154 wWin
ON CHOOSE OF BUTTON-154 IN FRAME F_Cerradas /* Ver solo Instancia y Descripción */
DO:
  IF SELF:LABEL EQ "Ver solo Instancia y Descripción" THEN DO:
     TCerradas.Instancia:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.INom_Instancia:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Usuario:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.INom_Usuario:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.Fec_Retiro:VISIBLE IN BROWSE br_cerradas = NO.
     TCerradas.Descripcion:VISIBLE IN BROWSE br_cerradas = YES.
     SELF:LABEL = "Todas las columnas".
  END.
  ELSE DO:
     TCerradas.Instancia:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.INom_Instancia:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Usuario:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.INom_Usuario:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Fec_Retiro:VISIBLE IN BROWSE br_cerradas = YES.
     TCerradas.Descripcion:VISIBLE IN BROWSE br_cerradas = YES.
     SELF:LABEL = "Ver solo Instancia y Descripción".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Saro
&Scoped-define SELF-NAME BUTTON-2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-2 wWin
ON CHOOSE OF BUTTON-2 IN FRAME F_Saro /* Salir */
DO:
   /*DEBUGGER:INITIATE().
   DEBUGGER:SET-BREAK().
     */
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


&Scoped-define SELF-NAME BUTTON-9
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-9 wWin
ON CHOOSE OF BUTTON-9 IN FRAME F_Saro /* Borrar */
DO:
  IF AVAIL(Saro) THEN DO:
     MESSAGE "Está Segura(o) de Anular el Evento Seleccionada...?"
         VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "CONFIRMAR ANULACION" 
         UPDATE W_RpataAn AS LOGICAL.
     IF NOT W_RpataAn THEN
        RETURN.
     DISABLE TRIGGERS FOR LOAD OF Saro.
     FIND CURRENT Saro NO-ERROR.
     
     FOR EACH Mov_Instancias WHERE Mov_Instancias.Num_Solicitud = Saro.Num_Referencia :
       DELETE Mov_Instancias.
     END.

     /*se borra la Saro*/
     DELETE Saro.

     MESSAGE "EL Evento fue Eliminada...El programa Finaliza y regresa al Menú."
         VIEW-AS ALERT-BOX INFO BUTTONS OK.
     APPLY "CHOOSE" TO BUTTON-2.
  END.

  ELSE MESSAGE "No Existe Evento Seleccionada para Anular...?"
         VIEW-AS ALERT-BOX.   
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Producto
&Scoped-define SELF-NAME Saro.Clase_Producto
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Saro.Clase_Producto wWin
ON VALUE-CHANGED OF Saro.Clase_Producto IN FRAME F_Producto /* Clase de Producto */
DO:
    DO WITH FRAME F_Producto:
        Cmb_Productos:LIST-ITEMS = "".
        DISABLE Tip_Credito.

        IF INT(Saro.Clase_Producto:SCREEN-VALUE) = 1 THEN DO:
            
            FOR EACH Pro_Ahorros WHERE Pro_Ahorros.Estado      EQ 1 NO-LOCK:
                W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto).
                IF AVAILABLE Saro AND Saro.Cod_Producto = Pro_Ahorros.Cod_Ahorro THEN
                Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
            END.
            DISABLE Tip_Credito.
        END.
        ELSE DO:
            ENABLE Tip_Credito.
        END.

    END.  
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Sarouno
&Scoped-define SELF-NAME Cmb_Agencias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Agencias wWin
ON LEAVE OF Cmb_Agencias IN FRAME F_Sarouno
DO:
IF W_Nuevo THEN DO:
  FIND Cfg_Instancias WHERE
       Cfg_instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia AND
       Cfg_Instancias.Agencia   EQ INTEGER(SUBSTRING(SELF:SCREEN-VALUE IN FRAME F_Sarouno,1,3)) AND
       Cfg_Instancias.Instancia EQ W_Primera AND
       Cfg_Instancias.Usuario   EQ W_Usuario AND
       Cfg_Instancias.Estado    EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAILABLE Cfg_instancias THEN DO:
     MESSAGE "El usuario no tiene configurada la instancia" SKIP(1)
             Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro SKIP(1)
             "En la Agencia: " SELF:SCREEN-VALUE VIEW-AS ALERT-BOX ERROR.
     W_Nuevo = NO.
     HIDE FRAME F_Sarouno.
     APPLY "choose" TO Btn_Consulta IN FRAME F_Saro.
  END.
END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Saro
&Scoped-define SELF-NAME Cmb_Instancias
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Instancias wWin
ON VALUE-CHANGED OF Cmb_Instancias IN FRAME F_Saro /* Instancias */
DO:
    ENABLE Btn_Salvar WITH FRAME F_Saro.
    FIND FIRST Instancias NO-LOCK 
        WHERE 
            Instancias.Instancia EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) NO-ERROR.
    MESSAGE Instancias.Instancia VIEW-AS ALERT-BOX INFO BUTTONS OK.
    IF AVAILABLE Instancias 
    THEN DO:
        ASSIGN  Orden_InsPan = Instancias.Orden
                W_VigIns     = Instancias.TMI
                VG_Normal:SCREEN-VALUE IN FRAME F_Consulta  = "Prioridad Normal hasta: " + STRING(Instancias.TMI / 2) + " Dias"
                VG_Media:SCREEN-VALUE IN FRAME F_Consulta   = "Prioridad Media desde: " + STRING(Instancias.TMI / 2) + " Hasta : "
                    + STRING(Instancias.TMI) + " Días"
                VG_Alta:SCREEN-VALUE IN FRAME F_Consulta  = "Prioridad Alta desde: " + STRING(Instancias.TMI + 1) + " Días".
        DO WITH FRAME  F_Saro:
            IF Instancias.Instancia EQ W_Primera 
            THEN DO:
                ENABLE Btn_Ingresar.
                DISABLE Cmb_Generacion.
            END.
            ELSE DO:
                DISABLE Btn_Ingresar  
                WITH FRAME F_Saro.
            END.
        END.
    END.
    RUN Solicitudes_X_Instancia. 
    HIDE FRAME F_SaroUno.
    VIEW FRAME F_Consulta.
    RUN Instancia_Hidden.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Sarouno
&Scoped-define SELF-NAME Cmb_Proceso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Proceso wWin
ON LEAVE OF Cmb_Proceso IN FRAME F_Sarouno /* Proceso */
DO:
  IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 610 THEN DO:
     ASSIGN Cmb_Proceso:SCREEN-VALUE = "".
  END.
  ELSE DO:
     IF SELF:SCREEN-VALUE = "" THEN DO:
       MESSAGE 'Proceso de Riesgos no debe ser en Blanco'
           VIEW-AS ALERT-BOX ERROR.
        
       APPLY "choose" TO Btn_Cancelar IN FRAME F_Saro.
       RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Proceso wWin
ON VALUE-CHANGED OF Cmb_Proceso IN FRAME F_Sarouno /* Proceso */
DO:
  IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 610 THEN DO:
     ASSIGN Cmb_Proceso:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Producto
&Scoped-define SELF-NAME Cmb_Productos
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON LEAVE OF Cmb_Productos IN FRAME F_Producto /* Producto */
DO:
  APPLY "CHOOSE" TO Btn_OutProductos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Productos wWin
ON VALUE-CHANGED OF Cmb_Productos IN FRAME F_Producto /* Producto */
DO:
  APPLY "CHOOSE" TO Btn_OutProductos.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Sarouno
&Scoped-define SELF-NAME Cmb_Rieposibles
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_Rieposibles wWin
ON VALUE-CHANGED OF Cmb_Rieposibles IN FRAME F_Sarouno
DO:
    IF SELF:SCREEN-VALUE = "" THEN DO:
      MESSAGE 'Listado de Evento  no debe ser en Blanco'
              VIEW-AS ALERT-BOX ERROR.

          APPLY "choose" TO Btn_Cancelar IN FRAME F_Saro.
          RETURN NO-APPLY.
            
     END.
     ELSE DO:
      
       ASSIGN v-cod-factorO = 0
              v-cod-factor  = 0
              v-cod-nivel   = 0.
        
       
       /*DO i = 1 TO  LENGTH(SELF:SCREEN-VALUE) :
         IF SUBSTRING(SELF:SCREEN-VALUE,i,1) = "-" THEN LEAVE.
         v-cod-factorO = v-cod-factorO + INT(SUBSTRING(SELF:SCREEN-VALUE,i,1)).

       END.*/
       v-cod-factorO = INTEGER(SUBSTRING(SELF:SCREEN-VALUE,1,6)).
       FIND FIRST Pro_Saro WHERE Pro_saro.Estado     = 1
                          AND Pro_Saro.Cod_Factor = v-cod-factorO
       NO-LOCK NO-ERROR.
       IF AVAILABLE Pro_Saro THEN DO:
           /*Ubicacion del Factor Interno o Externo */
           FIND FIRST varios WHERE Varios.tipo   = 38 
                               AND Varios.Codigo = Pro_Saro.Cod_UbFactor
           NO-LOCK NO-ERROR.
           IF AVAILABLE varios THEN DO:
             Nivel3:SCREEN-VALUE IN FRAME F_Sarouno = STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion.  
           END.
    
           /*Factor de Riesgo */
           FIND FIRST varios WHERE Varios.tipo   = 37 
                               AND Varios.Codigo = Pro_Saro.Cod_EveFactor
           NO-LOCK NO-ERROR.
           IF AVAILABLE varios THEN DO:
             Nivel2:SCREEN-VALUE IN FRAME F_Sarouno = STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion.  
           END.
    
           /*Clase de Riesgos*/
           FIND FIRST varios WHERE Varios.tipo   = 35
                               AND Varios.Codigo = Pro_Saro.Clase_ROperativo
           NO-LOCK NO-ERROR.
           IF AVAILABLE varios THEN DO:
             ASSIGN /* Cmb_Cla_Riesgo:SCREEN-VALUE IN FRAME F_Sarouno = STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion  */
                    W-ClaseRiesgo:SCREEN-VALUE IN FRAME F_Sarouno = STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion.  
           END.
    
    
       END.

       
     END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Cmb_zonas
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_zonas wWin
ON LEAVE OF Cmb_zonas IN FRAME F_Sarouno /* Zonas */
DO:
  IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 610 THEN DO:
     ASSIGN Cmb_Zonas:SCREEN-VALUE = "".
  END.
  ELSE DO:
     IF SELF:SCREEN-VALUE = "" THEN DO:
       MESSAGE 'Clase de Riesgo Operativo no debe ser en Blanco'
           VIEW-AS ALERT-BOX ERROR.
        
       APPLY "choose" TO Btn_Cancelar IN FRAME F_Saro.
       RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Cmb_zonas wWin
ON VALUE-CHANGED OF Cmb_zonas IN FRAME F_Sarouno /* Zonas */
DO:
  IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 610 THEN DO:
     ASSIGN Cmb_Zonas:SCREEN-VALUE = "".
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Saro
&Scoped-define SELF-NAME CtrlFrame
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL CtrlFrame wWin OCX.Tick
PROCEDURE CtrlFrame.PSTimer.Tick .
/*
IF T_Refresh:SCREEN-VALUE IN FRAME F_Saro EQ "YES" THEN DO:
  IF FRAME F_Consulta:HIDDEN EQ NO THEN
     APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Saro.
END. 
  */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cuantia
&Scoped-define SELF-NAME Saro.Cuantia_Recup_Seg
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Saro.Cuantia_Recup_Seg wWin
ON LEAVE OF Saro.Cuantia_Recup_Seg IN FRAME F_Cuantia /* Recup Seguros */
DO:
  DO WITH FRAME F_Cuantia:
      IF Saro.Cuantia_Recup_Seg:SCREEN-VALUE <> "0" AND 
         INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 630 THEN DO:
         
         Saro.Nit_seguro:BGCOLOR = 15.
         Saro.Nit_seguro:FGCOLOR = 0.

         ENABLE Saro.Nit_seguro.
    
      END.
      ELSE DO:
          Saro.Nit_seguro:BGCOLOR = 18.
          Saro.Nit_seguro:FGCOLOR = 15.
          DISABLE Saro.Nit_seguro.
      END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Instancias
&Scoped-define SELF-NAME Mov_Instancias.Estado
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Mov_Instancias.Estado wWin
ON VALUE-CHANGED OF Mov_Instancias.Estado IN FRAME F_Instancias /* Cerrar Instancia */
OR ANY-KEY OF SELF DO:
  IF SELF:SCREEN-VALUE EQ "yes" THEN
    APPLY "choose" TO Btn_AgregarTXT IN FRAME F_Instancias.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Cuantia
&Scoped-define SELF-NAME Saro.Nit
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Saro.Nit wWin
ON LEAVE OF Saro.Nit IN FRAME F_Cuantia /* Nit Resp */
DO:
      DEFI VAR RowidSoli AS ROWID.
      DEFI VAR W_Fec70 AS DATE.
      DEFI VAR W_IdMy70 AS LOGICAL INITIAL NO.

      IF Cmb_Agencias:SENSITIVE IN FRAME F_sarouno EQ NO THEN 
            APPLY "leave" TO Cmb_Agencias.  

      IF SELF:SCREEN-VALUE EQ "" THEN DO:
          MESSAGE "Debe Entrarse el número de identificación" SKIP
                  "del posible Responsable del Evento. Rectifique!!" SKIP(1)
                  "Desea abrir la Consulta de Clientes?" VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO UPDATE choice.
          IF choice THEN DO:
              RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
              ASSIGN NomNit:SCREEN-VALUE = P_Nombre + " " + P_Apellido
              SELF:SCREEN-VALUE   = P_Nit.
              FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
          END.
          ELSE DO:
              HIDE FRAME F_sarouno.
              W_Nuevo = NO.
              APPLY "choose" TO Btn_Consulta IN FRAME F_Saro.
          END.
      END.
      ELSE DO:
          FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
          IF AVAILABLE Clientes THEN 
              NomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
          ELSE DO:
              RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
              ASSIGN NomNit:SCREEN-VALUE = P_Nombre + " " + P_Apellido
              SELF:SCREEN-VALUE   = P_Nit.
              FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
          END.
      END.
  
  /*
  IF AVAILABLE Clientes THEN DO:
     Wk_Edad = YEAR(W_Fecha) - YEAR(Clientes.Fec_Nacimiento).
     
     IF MONTH(Clientes.Fec_Nacimiento) LT MONTH(W_fecha) THEN.
     ELSE IF  MONTH(Clientes.Fec_Nacimiento) GT MONTH(W_fecha) THEN
          Wk_edad = Wk_edad - 1.
     ELSE IF  MONTH(Clientes.Fec_Nacimiento) EQ MONTH(W_fecha)
          AND DAY(Clientes.Fec_Nacimiento)   GT DAY(W_fecha) THEN 
              Wk_edad = Wk_edad - 1.

/*      ASSIGN WX_Edad              = WK_Edad          */
/*             WX_Edad:SCREEN-VALUE = STRING(WX_Edad). */

     IF W_Nuevo EQ YES AND Clientes.Fec_Retiro NE ? AND Clientes.Estado EQ 2 THEN DO:
        MESSAGE "No se pueden crear solicitudes de crédito" SKIP
                "a clientes ya retirados!" SKIP(1)
                "Se cancela el proceso de solicitud Actual" VIEW-AS ALERT-BOX ERROR.
        APPLY "choose" TO Btn_Cancelar IN FRAME F_Saro.
        RETURN NO-APPLY.
     END.
     IF Clientes.Tipo_Vinculo GT 2 THEN DO:
        MESSAGE "La persona o Empresa no esta matriculada" SKIP
                "Como cliente de la cooperativa." SKIP
                "No se podrá crear una solicitud de esta" SKIP
                "hasta no cambiar este estado y entrar " SKIP
                "toda la información que se requiere!" VIEW-AS ALERT-BOX WARNING.
       ASSIGN Saro.Nit:SCREEN-VALUE = ""
              NomNit:SCREEN-VALUE = "".
       APPLY "entry" TO Cmb_Agencias.
       RETURN NO-APPLY.
     END.

     IF (W_fecha - Clientes.Fec_ultActual)  GT 180 THEN DO:
        MESSAGE "Asociado No tiene la informacion" SKIP
                "Actualizada en el Ultimo semestre." SKIP
                "No se podrá crear una solicitud de esta" SKIP
                 "hasta no actualizar la información que se requiere!" VIEW-AS ALERT-BOX WARNING.
       ASSIGN Saro.Nit:SCREEN-VALUE = ""
              NomNit:SCREEN-VALUE = "".
       APPLY "entry" TO Cmb_Agencias.
       RETURN NO-APPLY.
     END.

     FIND FIRST Ahorros WHERE Ahorros.Nit        EQ Saro.Nit:SCREEN-VALUE AND
                              Ahorros.Tip_Ahorro EQ 4 AND
                              Ahorros.Estado     EQ 1 AND
                              Ahorros.Sdo_Dispon + Sdo_Canje GT 0 NO-LOCK NO-ERROR.
     /*
     IF W_Nuevo AND AVAIL(Ahorros) AND Ahorros.Agencia NE W_agencia AND W_Agencia NE 4 THEN DO:
        MESSAGE "La solicitud solo puede Radicarse en la Agencia del Cliente..." SKIP
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN NO-APPLY.
     END.
     */
     /*IF W_Agencia EQ 11 THEN 
        ASSIGN Solicitud.Tip_Credito:SCREEN-VALUE IN FRAME F_Producto = "4".*/

     RowidSoli = ROWID(Saro).

     FIND FIRST saro WHERE Saro.Nit           EQ Saro.Nit:SCREEN-VALUE  
                            AND Saro.Estado        LE 1
                            AND Saro.Num_Referencia NE INTEG(Saro.Num_Referencia:SCREEN-VALUE) 
                            AND ROWID(Saro)        NE RowidSoli     NO-LOCK NO-ERROR.
     IF AVAILABLE(Saro) THEN
        MESSAGE "Este cliente esta tramitando actualmente otra solicitud" SKIP
            "en la agencia: " Saro.agencia
            VIEW-AS ALERT-BOX TITLE "PRECAUCION".   

     FIND FIRST Saro WHERE ROWID(Saro) EQ RowidSoli NO-LOCK NO-ERROR.

     IF Clientes.Tipo_Cliente LE 2 THEN DO:
         IF MONTH(W_Fecha) EQ 2 AND DAY(W_Fecha) EQ 29 THEN
            W_Fec70 = DATE(STRING(DAY(W_Fecha)) + "/28/"
                      + STRING(YEAR(W_Fecha) - 70)).
         ELSE
            W_Fec70 = DATE(STRING(DAY(W_Fecha)) + "/" + 
                           STRING(MONTH(W_Fecha)) + "/"
                         + STRING(YEAR(W_Fecha) - 70)).
         IF Clientes.Fec_Nacimiento LE W_Fec70 THEN DO:
            W_IdMy70 = YES.
         END.
     END.

     FIND FIRST Creditos WHERE Creditos.Nit         EQ P_Nit
                           AND Creditos.Sdo_Capital GT 0
                           AND Creditos.Fec_Pago    LT W_Fecha NO-LOCK NO-ERROR.
     IF AVAIL(Creditos) THEN
        MESSAGE "El solicitante tiene Crèditos en Mora en la Cooperativa..."
            VIEW-AS ALERT-BOX TITLE "Alerta por Morosidad".

     RUN Llenar_InfoCliente.
  END.

  IF AVAILABLE Clientes THEN DO:
     Wk_PerPagEmp = 4.
     FIND FIRST Empresas WHERE Empresas.Cod_Empresa EQ Clientes.Cod_Empresa NO-LOCK NO-ERROR.
     IF AVAILABLE Empresas THEN 
        Wk_PerPagEmp = Empresas.FOR_Pago.
     IF W_Nuevo THEN DO:
       IF AVAILABLE Saro AND Saro.FOR_Pago NE 2 THEN DO:
          Saro.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "1".
          W_ForPago:SCREEN-VALUE IN FRAME F_SaroUno = "Caja".
          Cmb_PerPago:SCREEN-VALUE = Cmb_PerPago:ENTRY(4).
          DISABLE Cmb_PerPago WITH FRAME F_SaroUno.
             IF Wk_PerPagEmp NE 0 AND W_Nuevo THEN DO:
                Cmb_PerPago:SCREEN-VALUE = Cmb_PerPago:ENTRY(Wk_PerPagEmp).
                Saro.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "2".
                W_ForPago:SCREEN-VALUE IN FRAME F_SaroUno = "Nómina".
                DISABLE Cmb_PerPago WITH FRAME F_SaroUno.
             END.
             ELSE DO:
                Cmb_PerPago:SCREEN-VALUE = Cmb_PerPago:ENTRY(Saro.FOR_Pago).
                Saro.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = STRING(Saro.FOR_Pago).
                CASE Saro.FOR_Pago:
                  WHEN 1 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Saro = "Caja".
                  WHEN 2 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Saro = "Nómina".
                  WHEN 3 THEN W_ForPago:SCREEN-VALUE IN FRAME F_Saro = "Deb. Automático".
                END CASE.
                DISABLE Cmb_PerPago WITH FRAME F_SaroUno.
             END.
       END.
     END.
     ELSE DO:
        CASE Saro.Per_Pago:
             WHEN 1 THEN 
                  ASSIGN Cmb_PerPago:SCREEN-VALUE = "1 - Semanal".
             WHEN 2 THEN 
                  ASSIGN Cmb_PerPago:SCREEN-VALUE = "2 - Decadal".
             WHEN 3 THEN 
                  ASSIGN Cmb_PerPago:SCREEN-VALUE = "3 - Quincenal".     
             WHEN 4 THEN 
                  ASSIGN Cmb_PerPago:SCREEN-VALUE = "4 - Mensual".
        END CASE.
        /*Cmb_PerPago:SCREEN-VALUE = Cmb_PerPago:ENTRY(Solicitud.FOR_Pago).*/
        
        Saro.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = STRING(Saro.FOR_Pago).
        CASE Saro.FOR_Pago:
          WHEN 1 THEN W_ForPago:SCREEN-VALUE IN FRAME F_SaroUno = "Caja".
          WHEN 2 THEN W_ForPago:SCREEN-VALUE IN FRAME F_SaroUno = "Nómina".
          WHEN 3 THEN W_ForPago:SCREEN-VALUE IN FRAME F_SaroUno = "Deb. Automático".
        END CASE.
        DISABLE Cmb_PerPago WITH FRAME F_SaroUno.
     END.
  END.  
    */

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Saro.Nit wWin
ON VALUE-CHANGED OF Saro.Nit IN FRAME F_Cuantia /* Nit Resp */
DO:
  ASSIGN SELF:SCREEN-VALUE = "".
 
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME Saro.Nit_seguro
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Saro.Nit_seguro wWin
ON LEAVE OF Saro.Nit_seguro IN FRAME F_Cuantia /* Nit Seguro */
DO:
  IF SELF:SCREEN-VALUE EQ "" THEN DO:
      MESSAGE "Debe Entrarse el número de identificación" SKIP
              "de la aseguradora. Rectifique!!" SKIP(1)
              "Desea abrir la Consulta de Clientes?" 
          VIEW-AS ALERT-BOX ERROR BUTTONS YES-NO UPDATE choice.
      IF choice THEN DO:
          RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
          ASSIGN NomNit_Seguros:SCREEN-VALUE = P_Nombre + " " + P_Apellido
          SELF:SCREEN-VALUE   = P_Nit.
          FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         HIDE FRAME F_Cuantia.
         W_Nuevo = NO.
         APPLY "choose" TO Btn_Consulta IN FRAME F_Saro.
      END.
  END.
  ELSE DO:
     FIND Clientes WHERE Clientes.Nit EQ SELF:SCREEN-VALUE NO-LOCK NO-ERROR.
     IF AVAILABLE Clientes THEN 
         NomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.
     ELSE DO:
         RUN C-Clientes.R(INPUT 1,INPUT W_Agencia, OUTPUT P_Nit,OUTPUT P_Nombre, OUTPUT P_Apellido, OUTPUT P_AgeCli).
         ASSIGN NomNit_Seguros:SCREEN-VALUE = P_Nombre + " " + P_Apellido
         SELF:SCREEN-VALUE   = P_Nit.
         FIND Clientes WHERE Clientes.Nit EQ P_Nit NO-LOCK NO-ERROR.
     END.
  END.


END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_HojaVida
&Scoped-define SELF-NAME Hoja_Vida.Observacion
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON ENTRY OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  longitud = SELF:LENGTH.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON LEAVE OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  IF Longitud NE SELF:LENGTH THEN ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON MOUSE-SELECT-CLICK OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  IF NOT W_NvaHV THEN DO:
     Id_Agregar = "HV".
     E_Agregar:SCREEN-VALUE IN FRAME F_Agregar = "".
     MESSAGE "No se permite el cambio del texto seleccionado" SKIP
             "Desea agregar algun mensaje de texto al asunto?"
             VIEW-AS ALERT-BOX INFORMATION BUTTONS YES-NO UPDATE choice.
     IF choice THEN DO:
        ENABLE ALL WITH FRAME F_Agregar.
        VIEW FRAME F_Agregar.
        APPLY "entry" TO E_Agregar IN FRAME F_Agregar.
        RETURN NO-APPLY.
     END.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Hoja_Vida.Observacion wWin
ON VALUE-CHANGED OF Hoja_Vida.Observacion IN FRAME F_HojaVida /* Observación */
DO:
  ENABLE Btn_SalvaHV WITH FRAME F_HojaVida.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Consulta
&Scoped-define SELF-NAME R_Organizar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL R_Organizar wWin
ON VALUE-CHANGED OF R_Organizar IN FRAME F_Consulta
DO:
  CASE SELF:SCREEN-VALUE:
    WHEN "1" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Referencia".
    END.
    WHEN "2" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.AgeSaro INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Agencia".
    END.
    WHEN "3" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Nit INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Nit".
    END.
    WHEN "4" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Nombre INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Nombre".
    END.
    WHEN "5" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Fec_ingreso INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Fec.Ingreso".
    END.
    WHEN "6" THEN DO:
      OPEN QUERY Br_Consulta FOR EACH Consulta
                NO-LOCK  BY Consulta.Vigencia INDEXED-REPOSITION.
      Buscar:LABEL IN FRAME F_Consulta = "Buscar x Vigencia".
    END.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Producto
&Scoped-define SELF-NAME Tip_Credito
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Tip_Credito wWin
ON VALUE-CHANGED OF Tip_Credito IN FRAME F_Producto
DO:
    DO WITH FRAME F_Producto:
        Cmb_Productos:LIST-ITEMS = "".
    
        IF INT(Saro.Clase_Producto:SCREEN-VALUE) = 2 THEN DO:
            ENABLE Tip_Credito.
            FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ INT(Tip_Credito:SCREEN-VALUE) AND
                                        Pro_Creditos.Estado  EQ 1 NO-LOCK:
                W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
                IF AVAILABLE Saro AND Saro.Cod_Producto EQ Pro_Creditos.Cod_Credito THEN
                    Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
            END.
        END.
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_LinOperativas
&Scoped-define SELF-NAME TLinea1
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TLinea1 wWin
ON VALUE-CHANGED OF TLinea1 IN FRAME F_LinOperativas /* Banca Personal y Minorista */
DO:
  MESSAGE TLinea1
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  IF TLinea1 = YES THEN DO:
     FPor_Ln1:BGCOLOR = 15.
     FPor_Ln1:FGCOLOR = 0. 
     ENABLE FPor_Ln1.
  END.

END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TLinea2
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TLinea2 wWin
ON VALUE-CHANGED OF TLinea2 IN FRAME F_LinOperativas /* Banca Comercial */
DO:
  IF TLinea2 = YES THEN DO:
     FPor_Ln2:BGCOLOR = 15.
     FPor_Ln2:FGCOLOR = 0. 
     ENABLE FPor_Ln2.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME TLinea3
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL TLinea3 wWin
ON VALUE-CHANGED OF TLinea3 IN FRAME F_LinOperativas /* Actividades Institucionales */
DO:
  IF TLinea3 = YES THEN DO:
     FPor_Ln3:BGCOLOR = 15.
     FPor_Ln3:FGCOLOR = 0. 
     ENABLE FPor_Ln3.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME F_Saro
&Scoped-define BROWSE-NAME Br_Cerradas
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Negadas wWin 
PROCEDURE Asignar_Negadas :
DEFINE VAR W_Ord           LIKE Instancias.Orden_Instancia.
DEFINE VAR W_OrdSiguiente  LIKE Instancias.Orden_Instancia.

IF saro.Estado EQ 3 THEN DO:
  FIND FIRST Mov_Instancias WHERE
             Mov_Instancias.Instancia     NE INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
             Mov_Instancias.Estado        EQ NO NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_Instancias THEN DO:
     FIND  Instancias WHERE Instancias.Tipo_Instancia = TP_Instancia 
                        AND Instancias.Instancia      = Mov_Instancias.Instancia 
                        AND Instancias.Tipo_Producto  = TP_InsProducto 
                        AND Instancias.Estado         = 1 NO-LOCK NO-ERROR.
     IF AVAILABLE Instancias AND Instancias.Orden_Instancia LT Orden_InsPan THEN DO:
        MESSAGE "Para pasar a la siguiente instancia" SKIP
                "deben estar cerradas las instancias" SKIP
                "anteriores. al momento se encuentra" SKIP
                "abierta la instancia: " Mov_Instancias.Instancia SKIP
                "Vigente en el Usuario: " Mov_Instancias.Usuario
                 VIEW-AS ALERT-BOX ERROR.
        ENABLE ALL WITH FRAME F_Saro.
        DISABLE NomUsuario WITH FRAME F_Saro.
        ENABLE ALL WITH FRAME F_Consulta.
        OPEN QUERY Br_Consulta FOR EACH Consulta
                   NO-LOCK INDEXED-REPOSITION.
        Buscar:LABEL IN FRAME F_Consulta = "Buscar Solicitud".
       RETURN ERROR.
     END.
  END.
  
  DO:
    FIND Instancias WHERE 
         Instancias.Tipo_Instancia = TP_Instancia AND 
         Instancias.Instancia      = INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
         Instancias.Tipo_Producto  = TP_InsProducto AND
         Instancias.Estado         = 1 NO-LOCK NO-ERROR.
    IF AVAILABLE Instancias THEN DO:
       W_ord = Instancias.Orden_Instancia.
       FIND Instancias WHERE
            Instancias.Tipo_Instancia = TP_Instancia   AND
            Instancias.Tipo_Producto  = TP_InsProducto AND
            Instancias.Instancia       EQ W_Negadas AND
            Instancias.Estado          EQ 1 USE-INDEX idx_orden NO-LOCK NO-ERROR.
       IF AVAILABLE Instancias THEN DO: 
          FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ saro.Cod_Producto 
          NO-LOCK NO-ERROR.

          FOR EACH TProIns: DELETE TProIns. END.
          /*Encuentra los usuarios configurados para las instancias siguientes, y que cumplen los limites*/
          FOR EACH Cfg_Instancias WHERE 
                   Cfg_Instancias.Agencia        EQ INT(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Sarouno,1,3)) AND
                   Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia AND
                   Cfg_Instancias.Instancia      EQ Instancias.Instancia AND
                   Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                   Cfg_Instancias.Plazo_Minimo   LE Dias AND
                   Cfg_Instancias.Plazo_Maximo   GE Dias AND
                   Cfg_Instancias.Monto_Minimo   LE saro.Cuantia_Riesgo AND
                   Cfg_Instancias.Monto_Maximo   GE saro.Cuantia_Riesgo AND 
                   Cfg_Instancias.Estado         EQ 1 NO-LOCK:
               
               FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario
                                     AND Usuarios.Agencia EQ Cfg_Instancias.Agencia 
                                     AND Usuarios.Estado EQ 1
                   NO-LOCK NO-ERROR.
              
               CREATE TProIns.
               ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                      TProIns.TP_Instancia = Cfg_Instancias.Instancia
                      TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                      TProIns.Tp_Cantidad  = 0
                      TProIns.TP_Agencia   = Cfg_Instancias.Agencia.

               FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
               IF AVAILABLE Instancias THEN 
                  TProIns.TP_NomInstan = Instancias.Nom_Instancia.
               ELSE TProIns.TP_NomInstan = "Instancia No encontrada".

               IF AVAILABLE Usuarios THEN 
                  TProIns.TP_NomUsuar  = Usuario.Nombre.
               ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".

               ASSIGN TProIns.TP_Abogado = Instancias.Id_Abogado WHEN AVAIL(Instancias).
          END.

          FIND FIRST TProIns NO-ERROR.
          IF NOT AVAILABLE TProIns THEN DO:
             MESSAGE "No se encontro ningún Usuario para asignar" SKIP
                     "la solicitud a la Proxima instancia." VIEW-AS ALERT-BOX.
             FIND Mov_Instancias WHERE 
                  Mov_Instancias.Instancia     EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
                  Mov_Instancias.Num_Solicitud EQ saro.Num_Referencia NO-LOCK NO-ERROR.
             IF AVAILABLE Mov_Instancias THEN DO:
                ASSIGN Mov_Instancias.Estado = NO
                       Mov_Instancias.Fec_Retiro = ?.

                ENABLE ALL WITH FRAME F_Saro.
                DISABLE NomUsuario WITH FRAME F_Saro.
                ENABLE ALL WITH FRAME F_Consulta.
                APPLY "choose" TO Btn_Consulta IN FRAME F_Saro.
             END.
             RETURN ERROR.
          END.

          /*Por cada usuario encuentra el numero de creditos que esta procesando para las instancias a seguir*/
          /*esto con el fin de escoger el que menos solicitudes este procesando y distribuir bien las solicitudes*/
          FOR EACH TproIns:
              FOR EACH Mov_Instancias WHERE
                       Mov_instancias.Instancia EQ TproIns.TP_Instancia AND
                       Mov_Instancias.Usuario   EQ TProIns.TP_Usuario  AND
                       Mov_Instancias.Estado    EQ NO    NO-LOCK:
                  TproIns.TP_Cantidad = TproIns.TP_Cantidad + 1.
              END.
          END.

          CREATE Hoja_Vida.
          ASSIGN Hoja_Vida.Tipo       = 9 
                 Hoja_Vida.Codigo     = 1  
                 Hoja_Vida.Instancia  = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5))
                 Hoja_Vida.Usuario    = W_Usuario
                 Hoja_Vida.Fec_Grabacion = w_fecha
                 Hoja_Vida.Hora_Grabacion = TIME + 100
                 Hoja_Vida.Observacion = 
                 "Se cierra la instancia: " + Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro +
                 " - Procesada por Usuario: " + W_Usuario
                 Hoja_Vida.Asunto_Cumplido = YES.
          
          FOR EACH TProIns BREAK BY TproIns.TP_Instancia BY TproIns.TP_Cantidad:
              IF FIRST-OF(TProIns.TP_Instancia) AND FIRST-OF(TProIns.TP_Cantidad) THEN DO:
                 /*se crea registro en hoja de vida para usuario que procesa la isntancia actual*/
                 /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
                 CREATE Hoja_Vida.
                 ASSIGN Hoja_Vida.Tipo            = 9 
                        Hoja_Vida.Codigo          = 1  
                        Hoja_Vida.Instancia       = TProIns.TP_Instancia
                        Hoja_Vida.Usuario         = TProIns.TP_Usuario
                        Hoja_Vida.Fec_Grabacion   = W_Fecha
                        Hoja_Vida.Hora_Grabacion  = TIME
                        Hoja_Vida.Asunto_Cumplido = YES.
                        Hoja_Vida.Observacion = "Entra a Instancia: " + STRING(TProIns.TP_Instancia) +
                           " - " + TProIns.TP_NomInstan +
                           " - Usuario Responsable: " + TProIns.TP_Usuario + " - " + TProIns.TP_NomUsuar.
                 FIND FIRST Mov_Instancias WHERE Mov_Instancias.Instancia     EQ TProIns.TP_Instancia 
                 NO-LOCK NO-ERROR.
                 IF NOT AVAILABLE Mov_Instancias THEN DO:
                   CREATE Mov_Instancias.
                   ASSIGN Mov_Instancias.Fec_Ingreso   = w_fecha
                          Mov_Instancias.Hora_Ingreso  = TIME
                          Mov_Instancias.Usuario       = TProIns.TP_Usuario
                          Mov_Instancias.Instancia     = TProIns.TP_Instancia
                          Mov_Instancias.Agencia       = TProIns.TP_Agencia.
                 END.
              END.
          END.

       END.
    END.
  END.
END.

/*si la solicitud ha sido condicionada, para que se vaya a la ultima*/
IF saro.Estado EQ 4 THEN DO:
  CREATE Hoja_Vida.
  ASSIGN Hoja_Vida.Tipo            = 9 
         Hoja_Vida.Codigo          = 1  
         Hoja_Vida.Instancia       = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5))
         Hoja_Vida.Usuario         = W_Usuario
         Hoja_Vida.Fec_Grabacion   = W_Fecha
         Hoja_Vida.Hora_Grabacion  = TIME
         Hoja_Vida.Asunto_Cumplido = YES.
   FIND Instancias WHERE  Instancias.Instancia      = INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) 
                     AND  Instancias.Tipo_Instancia = TP_Instancia 
   NO-LOCK NO-ERROR.
   IF AVAILABLE Instancias THEN
         Hoja_Vida.Observacion = 
         "Se cierra la instancia condicionada: " + Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro +
         " - Procesada por Usuario: " + W_Usuario + " - " + NomUsuario:SCREEN-VALUE IN FRAME F_Saro.
         
  /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
 FIND FIRST Mov_Instancias WHERE Mov_Instancias.Instancia     EQ W_Ultima 
 NO-ERROR.
 IF NOT AVAILABLE Mov_Instancias THEN 
    CREATE Mov_Instancias.
 
 CREATE Hoja_Vida.
 ASSIGN Hoja_Vida.Tipo         = 9 
         Hoja_Vida.Codigo      = 1  
         Hoja_Vida.Instancia   = W_Ultima
         Hoja_Vida.Usuario     = Mov_Instancias.Usuario
         Hoja_Vida.Fec_Grabacion = w_fecha
         Hoja_Vida.Hora_Grabacion = TIME
         Hoja_Vida.Asunto_Cumplido = YES.
 FIND Instancias WHERE Instancias.Tipo_Instancia = TP_Instancia AND 
                       Instancias.Id_Negadas     = YES AND
                       Instancias.Estado         = 1 AND
                       Instancias.Tipo_Producto  = TP_InsProducto NO-LOCK NO-ERROR.
 IF AVAILABLE Instancias THEN DO:
     Hoja_Vida.Observacion = "Entra a negadas: " + STRING(Instancias.Instancia,"999") 
       + " - " + Instancias.Nom_Instancia +
       " - Usuario Responsable: " + STRING(Mov_Instancia.Usuario).
 END.
 
 ASSIGN Mov_Instancias.Fec_Ingreso   = W_Fecha.
        Mov_Instancias.Hora_Ingreso  = TIME.
        Mov_Instancias.Estado        = NO.
        Mov_Instancias.Fec_Retiro    = ?.
        Mov_Instancias.Hora_Retiro   = 0.
  
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Primera wWin 
PROCEDURE Asignar_Primera :
DEFINE VAR W_Ord           LIKE Instancias.Orden_Instancia.
DEFINE VAR W_OrdSiguiente  LIKE Instancias.Orden_Instancia.
DEFINE VAR Asignado AS LOGICAL INITIAL NO.

IF saro.Estado EQ 2 THEN DO:
  FIND FIRST Mov_Instancias WHERE Mov_Instancias.Estado EQ NO NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_Instancias THEN DO:
     MESSAGE "Para pasar a la siguiente instancia" SKIP
             "deben estar cerradas las instancias" SKIP
             "anteriores. al momento se encuentra" SKIP
             "abierta la instancia: " Mov_Instancias.Instancia SKIP
             "Vigente en el Usuario: " Mov_Instancias.Usuario
             VIEW-AS ALERT-BOX ERROR.
     RETURN ERROR.
  END.
  
  FIND FIRST Pro_Creditos   WHERE Pro_Creditos.Cod_Credito   EQ saro.Cod_Producto NO-LOCK NO-ERROR.
  
                                                               
  FIND FIRST Instancias WHERE Instancias.Tipo_Instancia = TP_Instancia AND  /* Busca por orden  */
                              Instancias.Tipo_Producto  = TP_InsProducto AND
                              Instancias.Estado             EQ 1 AND 
                              Instancia.Orden_Instancia     EQ 1 NO-LOCK NO-ERROR.

  IF AVAILABLE Instancias THEN DO:
          W_ord = Instancias.Orden_Instancia.
          FOR EACH TProIns: DELETE TProIns. END.
          /*Encuentra los usuarios configurados para las instancias siguientes, y que cumplen los limites*/
          FOR EACH Cfg_Instancias WHERE 
                   Cfg_Instancias.Agencia        EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Sarouno,1,3)) AND
                   Cfg_Instancias.Instancia      EQ Instancias.Instancia       AND
                   Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia  AND
                   Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                   Cfg_Instancias.Plazo_Minimo   LE Dias AND
                   Cfg_Instancias.Plazo_Maximo   GE Dias AND
                   Cfg_Instancias.Monto_Minimo   LE saro.Cuantia_Riesgo AND
                   Cfg_Instancias.Monto_Maximo   GE saro.Cuantia_Riesgo AND 
                   Cfg_Instancias.Estado         EQ 1 NO-LOCK:

               CREATE TProIns.
               ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                      TProIns.TP_Instancia = Cfg_Instancias.Instancia
                      TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                      TProIns.Tp_Cantidad  = 0
                      TProIns.TP_Agencia   = CFG_Instancias.Agencia.

               FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
               IF AVAILABLE Instancias THEN 
                  TProIns.TP_NomInstan = Instancias.Nom_Instancia.
               ELSE TProIns.TP_NomInstan = "Instancia No encontrada".

               FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
               IF AVAILABLE Usuarios THEN 
                  TProIns.TP_NomUsuar  = Usuario.Nombre.
               ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".

              
          END.

          FIND FIRST TProIns NO-ERROR.
          IF NOT AVAILABLE TProIns THEN DO:
             
             FIND FIRST TProIns NO-ERROR.
             IF NOT AVAILABLE TProIns THEN DO:
                MESSAGE "No se ha encontrado Ninguna Configuración" SKIP
                     "donde un usuario tenga asignada la primera" SKIP
                     "instancia de Desembolso." SKIP(1)
                     "Se cancela la operación de aprobación"
                     VIEW-AS ALERT-BOX ERROR.
               RETURN ERROR.
             END.
          END.

          /*Por cada usuario encuentra el numero de creditos que esta procesando para las instancias a seguir*/
          /*esto con el fin de escoger el que menos solicitudes este procesando y distribuir bien las solicitudes*/
          FOR EACH TproIns:
              FOR EACH Mov_Instancias WHERE
                       Mov_instancias.Instancia EQ TproIns.TP_Instancia AND
                       Mov_Instancias.Usuario   EQ TProIns.TP_Usuario AND
                       Mov_Instancias.Estado    EQ NO NO-LOCK:
                       TproIns.TP_Cantidad = TproIns.TP_Cantidad + 1.
              END.
          END.

          FOR EACH TProIns BREAK BY TproIns.TP_Instancia BY TproIns.TP_Cantidad:
              IF FIRST-OF(TProIns.TP_Instancia) AND FIRST-OF(TProIns.TP_Cantidad) THEN DO:
                 CREATE Hoja_Vida.
                 ASSIGN Hoja_Vida.Tipo        = 9 
                        Hoja_Vida.Codigo      = 1  
                        Hoja_Vida.Instancia   = TProIns.TP_Instancia
                        Hoja_Vida.Usuario     = TProIns.TP_Usuario
                        Hoja_Vida.Fec_Grabacion = w_fecha
                        Hoja_Vida.Hora_Grabacion = TIME + 100
                        Hoja_Vida.Asunto_Cumplido = YES.
                        Hoja_Vida.Observacion = "Entra a Instancia: " + STRING(TProIns.TP_Instancia) +
                           " - " + TProIns.TP_NomInstan +
                           " - Usuario Responsable: " + TProIns.TP_Usuario + " - " + TProIns.TP_NomUsuar.
                 CREATE Mov_Instancias.
                 ASSIGN Mov_Instancias.Fec_Ingreso   = w_fecha
                        Mov_Instancias.Hora_Ingreso  = TIME
                        Mov_Instancias.Cuenta        = STRING(saro.Num_Referencia)
                        Mov_Instancias.Usuario       = TProIns.TP_Usuario
                        Mov_Instancias.Instancia     = TProIns.TP_Instancia
                        Mov_Instancias.Agencia       = TPRoIns.TP_Agencia
                        Mov_Instancias.Estado        = NO
                        Asignado = YES.
                 LEAVE.  /*Nov.30/05 GAER*/
              END.
          END.

          IF NOT Asignado THEN DO:
              MESSAGE "No se ha encontrado la configuración de un usuario" SKIP
                      "en las instancias de desembolso!. Avise al administrador" SKIP
                      "para que configure la instacia de desembolso para un usuario" VIEW-AS ALERT-BOX.
              RETURN ERROR.
          END.
  END.  
  ELSE DO:
    MESSAGE "No existe La Config.de Instancia(Primera) para pasar al Desembolso."
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Asignar_Proxima_Instancia wWin 
PROCEDURE Asignar_Proxima_Instancia :
DEFINE VAR W_Ord           LIKE Instancias.Orden_Instancia.
DEFINE VAR W_OrdSiguiente  LIKE Instancias.Orden_Instancia.
DEFINE VAR W_AS AS LOGICAL INITIAL NO.

    
DEFINE VAR PMin   LIKE Cfg_Instancias.PLazo_Minimo INITIAL 0.
DEFINE VAR PMax   LIKE Cfg_Instancias.PLazo_Maximo INITIAL 999999999.
DEFINE VAR MMin   LIKE Cfg_Instancias.Monto_Minimo INITIAL 1.
DEFINE VAR MMax   LIKE Cfg_Instancias.Monto_Maximo INITIAL 99999999.
DEFINE VAR ENDGLO LIKE Solicitud.Monto INITIAL 0.
    
FIND FIRST clientes WHERE cliente.nit = saro.nit NO-LOCK NO-ERROR.    

IF saro.Estado EQ 1 OR saro.Estado EQ 4 THEN DO:
  FIND FIRST Mov_Instancias WHERE
             Mov_Instancias.Instancia     NE INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
             Mov_Instancias.Num_Solicitud EQ INT(Saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno) AND
             Mov_instancias.Nit           EQ clientes.Nit AND
             Mov_Instancias.Estado        EQ NO NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_Instancias THEN DO:
     FIND  Instancias WHERE
           Instancias.Tipo_Instancia = TP_Instancia AND 
           Instancias.Instancia       EQ Mov_Instancias.Instancia AND
           Instancias.Tipo_Producto   EQ TP_InsProducto AND
           Instancias.Estado          EQ 1 NO-LOCK NO-ERROR.
     IF AVAILABLE Instancias AND Instancias.Orden_Instancia LT Orden_InsPan THEN DO:
        MESSAGE "Para pasar a la siguiente instancia" SKIP
                "deben estar cerradas las instancias" SKIP
                "anteriores. al momento se encuentra" SKIP
                "abierta la instancia: " Mov_Instancias.Instancia SKIP
                "Vigente en el Usuario: " Mov_Instancias.Usuario
                 VIEW-AS ALERT-BOX ERROR.
        ENABLE ALL WITH FRAME F_Saro.
        DISABLE NomUsuario WITH FRAME F_Saro.
        ENABLE ALL WITH FRAME F_Consulta.
        OPEN QUERY Br_Consulta FOR EACH Consulta
                   NO-LOCK   INDEXED-REPOSITION.
        Buscar:LABEL IN FRAME F_Consulta = "Buscar Solicitud".
       RETURN ERROR.
     END.
  END.
  
 /*Cambio verificamos  la existencia de codeudores y Garantias   04/01/2005  John Moncada y Alexander Moncada*/ 
  FIND Clientes WHERE Clientes.Nit    EQ saro.Nit 
                  AND Clientes.Estado EQ 1 NO-LOCK NO-ERROR.
  IF NOT AVAIL(Clientes) THEN DO:
     MESSAGE "Falta el Cliente con Estado Activo para realizar la operaciòn..."
         VIEW-AS ALERT-BOX ERROR.
     RETURN NO-APPLY.
  END.

  
  FIND FIRST Mov_Instancias WHERE
             Mov_Instancias.Instancia     NE INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
             Mov_Instancias.Num_Solicitud EQ INTEGER(saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno) AND
             Mov_instancias.Nit           EQ clientes.Nit AND
             Mov_Instancias.Estado        EQ NO NO-LOCK NO-ERROR.
  IF AVAILABLE Mov_Instancias THEN DO:
     FIND  Instancias WHERE
           Instancias.Tipo_Instancia  EQ TP_Instancia AND 
           Instancias.Instancia       EQ Mov_Instancias.Instancia AND
           Instancias.Tipo_Producto   EQ TP_InsProducto AND
           Instancias.Estado          EQ 1 NO-LOCK NO-ERROR.
     /*
     IF (AVAIL(instancias) AND Instancias.Id_Scoring AND NOT Instancias.primera) THEN DO:
         RUN Verificar_relaciones_Garantias NO-ERROR.
     END.
     */
  END.
  RELEASE clientes.
/*Fin Cambio*/ 
  DO:
    FIND Instancias WHERE 
         Instancias.Tipo_Instancia = TP_Instancia AND 
         Instancias.Instancia      EQ INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
         Instancias.Tipo_Producto  EQ TP_InsProducto AND
         Instancias.Estado         EQ 1 NO-LOCK NO-ERROR.
    
    IF AVAILABLE Instancias THEN DO:
       W_ord = Instancias.Orden_Instancia.
       FIND NEXT Instancias WHERE
                 Instancias.Tipo_Instancia = TP_Instancia AND 
                 Instancias.Orden_Instancia GT W_Ord AND
                 Instancias.Tipo_Producto   EQ TP_InsProducto AND
                 Instancias.Estado          EQ 1 USE-INDEX idx_orden NO-LOCK NO-ERROR.
       IF AVAILABLE Instancias THEN DO: 
          FIND FIRST Pro_Creditos WHERE Pro_Creditos.Cod_Credito EQ saro.Cod_Producto 
          NO-LOCK NO-ERROR.
          EMPTY TEMP-TABLE TProIns NO-ERROR.

          /*
          /*Encuentra los usuarios configurados para las instancias siguientes, 
            y que cumplen los limites*/
          IF NOT Pro_Creditos.Id_AprobAgencia THEN DO:
              
              FOR EACH Cfg_Instancias WHERE 
                       Cfg_Instancias.Agencia        EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Sarouno,1,3)) AND
                       Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia AND
                       Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                       Cfg_Instancias.Plazo_Minimo   LE Dias AND
                       Cfg_Instancias.Plazo_Maximo   GE Dias AND 
                       Cfg_Instancias.Monto_Minimo   LE saro.Cuantia_Riesgo AND
                       Cfg_Instancias.Monto_Maximo   GE saro.Cuantia_Riesgo AND 
                       Cfg_Instancias.Estado         EQ 1 NO-LOCK:

                   FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario
                                         AND Usuarios.Agencia EQ Cfg_Instancias.Agencia 
                                         AND Usuarios.estado  EQ 1 NO-LOCK NO-ERROR.

                   IF AVAILABLE Usuarios AND Usuarios.Agencia EQ Cfg_Instancias.Agencia THEN
                      NEXT.

                   CREATE TProIns.
                   ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                          TProIns.TP_Instancia = Cfg_Instancias.Instancia
                          TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                          TProIns.Tp_Cantidad  = 0
                          TProIns.TP_Agencia   = Cfg_Instancias.Agencia.

                   FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia NO-LOCK NO-ERROR.
                   IF AVAILABLE Instancias THEN 
                      TProIns.TP_NomInstan = Instancias.Nom_Instancia.
                   ELSE TProIns.TP_NomInstan = "Instancia No encontrada".

                   IF AVAILABLE Usuarios THEN 
                      TProIns.TP_NomUsuar  = Usuario.Nombre.
                   ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".

                   ASSIGN TProIns.TP_Abogado = Instancias.Id_Abogado WHEN AVAIL(Instancias).
              END.
          END.
          ELSE DO:*/

             FOR EACH Cfg_Instancias WHERE 
                      Cfg_Instancias.Agencia        EQ INTEGER(SUBSTRING(Cmb_Agencias:SCREEN-VALUE IN FRAME F_Sarouno,1,3)) AND  /* w_agencia AND */
                      Cfg_Instancias.Tipo_Instancia EQ Instancias.Tipo_Instancia AND
                      Cfg_Instancias.Orden          EQ Instancias.Orden_Instancia AND
                      Cfg_Instancias.Plazo_Minimo   LE 1     AND
                      Cfg_Instancias.Plazo_Maximo   GE 99999 AND 
                      Cfg_Instancias.Monto_Minimo   LE MMin  AND
                      /*Cfg_Instancias.Monto_Maximo   GE MMax  AND */
                      Cfg_Instancias.Estado         EQ 1 NO-LOCK:

                FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario
                                      AND Usuarios.estado  EQ 1 
                NO-LOCK NO-ERROR.

                /*
                /* NO PERMITE QUE LA INSTANCIA PASE A UN USUARIO IGUAL AL ACTUAL.*/
                IF Cfg_Instancias.Agencia GE 4 AND Cfg_Instancias.Agencia LE 10 THEN /* BOGOTA*/
                  IF AVAIL(Usuarios) AND Usuarios.Usuario EQ W_Usuario THEN  
                     NEXT.  
                */

                /*
                /* SE CONDICIONA PARA QUE VALIDE SOLO LAS QUE NO SON DE BOGOTA */
                /* CUANJDO SE IMPLEMENTE TODO EL PAIS QUEDA SIN EL IF SIGUIENTE */
                IF Pro_Creditos.Id_AprobAgencia AND AVAIL(Usuarios) AND                  
                   Usuarios.Usuario NE W_Usuario                    AND
                  (Cfg_Instancias.Agencia GT 10 OR Cfg_Instancias.Agencia LT 4 ) /*AND Pro_Creditos.Cod_Credito EQ 7*/
                    THEN  NEXT.    
                ELSE DO:*/
                    IF AVAILABLE(usuarios) THEN DO: /* Agregue esta condicion antes de grabar */
                        CREATE TProIns.
                        ASSIGN TProIns.TP_Orden     = Cfg_Instancias.Orden
                               TProIns.TP_Instancia = Cfg_Instancias.Instancia
                               TProIns.TP_Usuario   = Cfg_Instancias.Usuario
                               TProIns.Tp_Cantidad  = 0
                               TProIns.TP_Agencia   = Cfg_Instancias.Agencia.
                    END.
                    ELSE NEXT.
                /*END.*/

                FIND Instancias WHERE Instancias.Instancia EQ Cfg_Instancias.Instancia 
                NO-LOCK NO-ERROR.
                IF AVAILABLE Instancias THEN 
                    TProIns.TP_NomInstan = Instancias.Nom_Instancia.
                ELSE TProIns.TP_NomInstan = "Instancia No encontrada".

                IF AVAILABLE Usuarios THEN 
                    TProIns.TP_NomUsuar  = Usuario.Nombre.
                ELSE TProIns.TP_NomUsuar  = "Usuario No encontrado".

                ASSIGN TProIns.TP_Abogado = Instancias.Id_Abogado WHEN AVAIL(Instancias).
             END.  /*fin del for each*/
          /*END.  /*Else do*/ */

          FIND FIRST TProIns NO-ERROR.
          IF NOT AVAILABLE TProIns THEN DO:
             MESSAGE "No se encontro ningún Usuario" SKIP
                     "Para asignar el evento a la Proxima instancia." VIEW-AS ALERT-BOX.
             FIND Mov_Instancias WHERE 
                  Mov_Instancias.Instancia     EQ INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) AND
                  Mov_Instancias.Num_Solicitud EQ Saro.Num_Referencia NO-LOCK NO-ERROR.
             IF AVAILABLE Mov_Instancias THEN DO:
                ASSIGN Mov_Instancias.Estado = NO
                       Mov_Instancias.Fec_Retiro = ?.

                ENABLE ALL WITH FRAME F_Saro.
                DISABLE NomUsuario WITH FRAME F_Saro.
                ENABLE ALL WITH FRAME F_Consulta.
                APPLY "choose" TO Btn_Consulta IN FRAME F_Saro.
             END.
             RETURN ERROR.
          END.

          /*Por cada usuario encuentra el numero de creditos que esta
            procesando para las instancias a seguir esto con el fin de
            escoger el que menos solicitudes este procesando y
            distribuir bien las solicitudes*/
          FOR EACH TproIns:
              FOR EACH Mov_Instancias WHERE
                       Mov_instancias.Instancia EQ TproIns.TP_Instancia AND
                       Mov_Instancias.Usuario   EQ TProIns.TP_Usuario  AND
                       Mov_Instancias.Estado    EQ NO    NO-LOCK:
                  TproIns.TP_Cantidad = TproIns.TP_Cantidad + 1.
              END.
          END.

          CREATE Hoja_Vida.
          ASSIGN Hoja_Vida.Tipo       = 9 
                 Hoja_Vida.Codigo     = 1  
                 Hoja_Vida.Instancia  = INTEGER(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5))
                 Hoja_Vida.Usuario    = W_Usuario
                 Hoja_Vida.Fec_Grabacion  = w_fecha
                 Hoja_Vida.Hora_Grabacion = TIME + 100
                 Hoja_Vida.Observacion    = 
                 "Se cierra la instancia: " + Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro +
                 " - Procesada por Usuario: " + W_Usuario
                 Hoja_Vida.Asunto_Cumplido = YES.
          
          FOR EACH TProIns BREAK BY TproIns.TP_Instancia BY TproIns.TP_Cantidad:
              
              IF FIRST-OF(TProIns.TP_Instancia) AND FIRST-OF(TProIns.TP_Cantidad) THEN DO:
                 /*se crea registro en hoja de vida para usuario que procesa la isntancia actual*/
                 /*se crea registro en hoja de vida para el usuario al cual se le asigna la solicitud*/
                 CREATE Hoja_Vida.
                 ASSIGN Hoja_Vida.Tipo        = 9 
                        Hoja_Vida.Codigo      = 1  
                        Hoja_Vida.Instancia   = TProIns.TP_Instancia
                        Hoja_Vida.Usuario     = TProIns.TP_Usuario
                        Hoja_Vida.Fec_Grabacion = w_fecha
                        Hoja_Vida.Hora_Grabacion = TIME
                        Hoja_Vida.Asunto_Cumplido = YES.
                        Hoja_Vida.Observacion = "Entra a Instancia: " + STRING(TProIns.TP_Instancia) +
                           " - " + TProIns.TP_NomInstan +
                           " - Usuario Responsable: " + TProIns.TP_Usuario + " - " + TProIns.TP_NomUsuar.
                 
                 FIND FIRST Mov_Instancias WHERE Mov_Instancias.Instancia     = TProIns.TP_Instancia 
                                             AND Mov_Instancias.Num_Solicitud = INTEGER(Saro.Num_Referencia:SCREEN-VALUE) 
                                             AND Mov_Instancias.Nit           = Saro.Nit:SCREEN-VALUE IN FRAME F_Cuantia 
                 NO-LOCK NO-ERROR.
                 
                 CREATE Mov_Instancias.
                 ASSIGN Mov_Instancias.Nit           = Saro.Nit:SCREEN-VALUE IN FRAME F_Cuantia
                        Mov_Instancias.Num_Solicitud = DEC(Saro.Num_Referencia:SCREEN-VALUE)
                        Mov_Instancias.Instancia     = TProIns.TP_Instancia
                        Mov_Instancias.Agencia       = TProIns.TP_Agencia
                        Mov_Instancias.Fec_Ingreso   = W_Fecha
                        Mov_Instancias.Hora_Ingreso  = TIME
                        Mov_Instancias.Usuario       = TProIns.TP_Usuario
                        Mov_Instancias.Estado        = NO.
              END.
          END.

          ENABLE ALL WITH FRAME F_Saro.
          DISABLE NomUsuario WITH FRAME F_Saro.
          ENABLE ALL WITH FRAME F_Consulta.
          APPLY "choose" TO Btn_Consulta IN FRAME F_Saro.
          W_AS = YES.
       END.
    END.
  END.
END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Buscar_Instancias_Cerradas wWin 
PROCEDURE Buscar_Instancias_Cerradas :
FOR EACH TCerradas: DELETE TCerradas. END.
FOR EACH Mov_Instancias WHERE
         Mov_Instancias.Num_Solicitud EQ saro.Num_Referencia AND
         Mov_Instancias.Nit           EQ saro.Nit NO-LOCK:
    CREATE TCerradas.
    ASSIGN TCerradas.Instancia      = Mov_Instancias.Instancia
           TCerradas.Estado         = Mov_Instancias.Estado
           TCerradas.Num_Solicitud  = Mov_Instancias.Num_Solicitud
           TCerradas.Usuario        = Mov_Instancias.Usuario
           TCerradas.Fec_Ingreso    = Mov_Instancias.Fec_Ingreso
           TCerradas.Fec_Retiro     = Mov_Instancias.Fec_Retiro
           TCerradas.Hora_Ingreso   = Mov_Instancias.Hora_Ingreso
           TCerradas.Hora_Retiro    = Mov_Instancias.Hora_Retiro
           TCerradas.Descripcion    = Mov_Instancias.Descripcion.
    FIND Instancias WHERE Instancias.Instancia EQ Mov_Instancias.Instancia NO-LOCK NO-ERROR.
    IF AVAILABLE Instancias THEN TCerradas.INom_Instancia = Instancias.Nom_Instancia.
    FIND Usuarios WHERE Usuarios.Usuario EQ Mov_Instancias.Usuario NO-LOCK NO-ERROR.
    IF AVAILABLE Usuario THEN TCerradas.INom_Usuario = Usuarios.Nombre.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE control_load wWin  _CONTROL-LOAD
PROCEDURE control_load :
/*------------------------------------------------------------------------------
  Purpose:     Load the OCXs    
  Parameters:  <none>
  Notes:       Here we load, initialize and make visible the 
               OCXs in the interface.                        
------------------------------------------------------------------------------*/

&IF "{&OPSYS}" = "WIN32":U AND "{&WINDOW-SYSTEM}" NE "TTY":U &THEN
DEFINE VARIABLE UIB_S    AS LOGICAL    NO-UNDO.
DEFINE VARIABLE OCXFile  AS CHARACTER  NO-UNDO.

OCXFile = SEARCH( "W-Proceso_Saro.wrx":U ).
IF OCXFile = ? THEN
  OCXFile = SEARCH(SUBSTRING(THIS-PROCEDURE:FILE-NAME, 1,
                     R-INDEX(THIS-PROCEDURE:FILE-NAME, ".":U), "CHARACTER":U) + "wrx":U).

IF OCXFile <> ? THEN
DO:
  ASSIGN
    chCtrlFrame = CtrlFrame:COM-HANDLE
    UIB_S = chCtrlFrame:LoadControls( OCXFile, "CtrlFrame":U)
  .
  RUN initialize-controls IN THIS-PROCEDURE NO-ERROR.
END.
ELSE MESSAGE "W-Proceso_Saro.wrx":U SKIP(1)
             "The binary control file could not be found. The controls cannot be loaded."
             VIEW-AS ALERT-BOX TITLE "Controls Not Loaded".

&ENDIF

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
  DISPLAY Cmb_Instancias T_Refresh NomUsuario 
      WITH FRAME F_Saro IN WINDOW wWin.
  ENABLE RECT-2 RECT-3 Cmb_Instancias BUTTON-1 Btn_ProcesarInstancia T_Refresh 
         Btn_Consulta Btn_Salvar Btn_Deshacer Btn_Ingresar Btn_Cancelar 
         BUTTON-9 BUTTON-2 BUTTON-4 
      WITH FRAME F_Saro IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Saro}
  DISPLAY W-Agencias W-ClaseRiesgo Cmb_Rieposibles Cmb_Proceso Cmb_zonas 
          Nom_Producto Cmb_Generacion Nivel3 Nivel2 
      WITH FRAME F_Sarouno IN WINDOW wWin.
  IF AVAILABLE Saro THEN 
    DISPLAY Saro.Num_Referencia Saro.Fec_Grabacion Saro.Hora_Grabacion 
          Saro.Fec_Des_Even Saro.Hora_Des_Even Saro.Fec_Ini_Even 
          Saro.Hora_Ini_Even Saro.Fec_Fin_Even Saro.Hora_Fin_Even 
          Saro.Canal_Servicio Saro.Descrip_Evento 
      WITH FRAME F_Sarouno IN WINDOW wWin.
  ENABLE Cmb_Rieposibles Saro.Fec_Ini_Even Saro.Hora_Ini_Even Saro.Fec_Fin_Even 
         Saro.Hora_Fin_Even Cmb_Proceso Cmb_zonas Nom_Producto Btn_Producto 
         Cmb_Generacion RECT-292 
      WITH FRAME F_Sarouno IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Sarouno}
  DISPLAY R_Organizar Buscar VG_Normal VG_Media VG_Alta 
      WITH FRAME F_Consulta IN WINDOW wWin.
  ENABLE RECT-223 RECT-287 RECT-288 RECT-289 Br_Consulta R_Organizar 
         Btn_OutConsulta Buscar 
      WITH FRAME F_Consulta IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Consulta}
  DISPLAY RADIO-SET_Estado_Evento W_MenDes 
      WITH FRAME F_Ultima IN WINDOW wWin.
  ENABLE RECT-224 RECT-225 Btn_SalvaUltima Btn_OutUltima 
         RADIO-SET_Estado_Evento 
      WITH FRAME F_Ultima IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Ultima}
  ENABLE Br_Cerradas Btn_OutCerradas BUTTON-154 
      WITH FRAME F_Cerradas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cerradas}

  {&OPEN-QUERY-F_Cuantia}
  GET FIRST F_Cuantia.
  DISPLAY NomNit NomNit_Seguros 
      WITH FRAME F_Cuantia IN WINDOW wWin.
  IF AVAILABLE Saro THEN 
    DISPLAY Saro.Nit Saro.Cuantia_Riesgo Saro.Cuantia_Recup_Tot 
          Saro.Cuantia_Recup_Seg Saro.Nit_seguro 
      WITH FRAME F_Cuantia IN WINDOW wWin.
  ENABLE Saro.Nit Btn_OutScoringCuantia Btn_Salir_Cuantia 
      WITH FRAME F_Cuantia IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Cuantia}
  DISPLAY WHora_Ingreso W_Instancia W_UsuarioInstancia Whora_Retiro Vigencia 
      WITH FRAME F_Instancias IN WINDOW wWin.
  IF AVAILABLE Mov_Instancias THEN 
    DISPLAY Mov_Instancias.Fec_Ingreso Mov_Instancias.Fec_Retiro 
          Mov_Instancias.Estado Mov_Instancias.Descripcion 
      WITH FRAME F_Instancias IN WINDOW wWin.
  ENABLE BUTTON-142 Mov_Instancias.Estado Btn_GraInstancia Btn_AgregarTxt 
         Btn_Imp Btn_insVolver 
      WITH FRAME F_Instancias IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Instancias}
  DISPLAY TLinea1 FPor_Ln1 TLinea2 FPor_Ln2 TLinea3 FTotalPorLn FPor_Ln3 
      WITH FRAME F_LinOperativas IN WINDOW wWin.
  ENABLE TLinea1 TLinea2 TLinea3 Btn_OutScoring-2 Btn_Salir_Linea 
      WITH FRAME F_LinOperativas IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_LinOperativas}
  DISPLAY S_InfoProducto 
      WITH FRAME F_InfoProducto IN WINDOW wWin.
  ENABLE S_InfoProducto Btn_OutScoring 
      WITH FRAME F_InfoProducto IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_InfoProducto}

  {&OPEN-QUERY-F_Estado}
  GET FIRST F_Estado.
  IF AVAILABLE Saro THEN 
    DISPLAY Saro.Estado_Evento Saro.Descrip_Anulado 
      WITH FRAME F_Estado IN WINDOW wWin.
  ENABLE Saro.Estado_Evento Btn_OutScoring-3 Btn_Salir_Linea-2 
      WITH FRAME F_Estado IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Estado}
  DISPLAY E_Agregar 
      WITH FRAME F_Agregar IN WINDOW wWin.
  ENABLE E_Agregar BUTTON-153 
      WITH FRAME F_Agregar IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Agregar}
  DISPLAY Tip_Credito Cmb_Productos 
      WITH FRAME F_Producto IN WINDOW wWin.
  IF AVAILABLE Saro THEN 
    DISPLAY Saro.Clase_Producto 
      WITH FRAME F_Producto IN WINDOW wWin.
  ENABLE Saro.Clase_Producto Tip_Credito Cmb_Productos Btn_OutProductos 
         Btn_Salir_Producto 
      WITH FRAME F_Producto IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_Producto}
  IF AVAILABLE Hoja_Vida THEN 
    DISPLAY Hoja_Vida.Fec_Grabacion Hoja_Vida.Asunto_Cumplido 
          Hoja_Vida.Observacion 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  ENABLE BUTTON-150 Hoja_Vida.Asunto_Cumplido Hoja_Vida.Observacion Btn_SalvaHV 
         Btn_NvoHv BUTTON-152 BUTTON-149 
      WITH FRAME F_HojaVida IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_HojaVida}
  ENABLE Btn_OutConHV 
      WITH FRAME F_ConHV IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-F_ConHV}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imprimir_Conceptos wWin 
PROCEDURE Imprimir_Conceptos :
DEFINE VAR i AS INTEGER.
DEFINE VAR WCon AS CHARACTER FORMAT "X(100)" EXTENT 12.
DEFINE VAR Lin_Analista AS CHARACTER FORMAT "X(120)".
  DISPLAY "--------------------------------------------------------------------------------------------------------------"
          "Conceptos Emitidos en las Instancias" SKIP
          "--------------------------------------------------------------------------------------------------------------"
  WITH FRAME ftitins WIDTH 132 NO-LABELS USE-TEXT STREAM-IO NO-BOX.

  FOR EACH Mov_Instancias WHERE Mov_Instancias.Nit           EQ saro.Nit
                            AND Mov_Instancias.Num_Solicitud EQ saro.Num_Referencia  NO-LOCK 
                          BREAK BY Mov_Instancia.Instancia BY Fec_Ingreso BY Hora_Ingreso:
      FIND Usuarios WHERE Usuarios.Usuario EQ  Mov_Instancias.Usuario NO-LOCK NO-ERROR.
      FIND Instancias WHERE Instancias.Instancia      EQ Mov_Instancias.Instancia AND
                            Instancias.Tipo_Instancia = TP_Instancia NO-LOCK  NO-ERROR.
      IF AVAILABLE Usuarios AND AVAIL(Instancias) THEN 
         Lin_analista = " (" + Instancias.Nom_Instancia + ") :" + TRIM(Usuarios.Nombre).
      
      ASSIGN WCon[1]  = SUBSTRING(Mov_Instancias.Descripcion,1,100)
             WCon[2]  = SUBSTRING(Mov_Instancias.Descripcion,101,100)
             WCon[3]  = SUBSTRING(Mov_Instancias.Descripcion,201,100) 
             WCon[4]  = SUBSTRING(Mov_Instancias.Descripcion,301,100) 
             WCon[5]  = SUBSTRING(Mov_Instancias.Descripcion,401,100)
             WCon[6]  = SUBSTRING(Mov_Instancias.Descripcion,501,100)
             WCon[7]  = SUBSTRING(Mov_Instancias.Descripcion,601,100)
             WCon[8]  = SUBSTRING(Mov_Instancias.Descripcion,701,100)
             WCon[9]  = SUBSTRING(Mov_Instancias.Descripcion,801,100)
             WCon[10] = SUBSTRING(Mov_Instancias.Descripcion,901,100)
             WCon[11] = SUBSTRING(Mov_Instancias.Descripcion,1001,100)
             WCon[12] = SUBSTRING(Mov_Instancias.Descripcion,1101,100).

      /*DISPLAY Lin_Analista WITH FRAME fcodeT WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
      DO i = 1 TO 12 BY 1:
        IF WCon[i] NE "" THEN DO:
            DISPLAY WCon[i]
            WITH FRAME fcode WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
        END.
        ELSE
            DISPLAY SKIP(1)
            WITH FRAME fcode2 WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.

      END.
      ASSIGN WCon = "".*/
      DISPLAY Lin_Analista SKIP
           mov_instancias.descripcion VIEW-AS EDITOR SIZE 105 BY 3 AT 3
              /*WCon[1] SKIP
              WCon[2] SKIP
              WCon[3] SKIP
              WCon[4] SKIP
              WCon[5] SKIP
              WCon[6]  SKIP
              WCon[7]  SKIP
              WCon[8]  SKIP
              WCon[9]  SKIP
              WCon[10] SKIP
              WCon[11] SKIP
              WCon[12] SKIP*/
      WITH FRAME fcode WIDTH 132 NO-LABELS NO-BOX USE-TEXT STREAM-IO.
  END.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Imp_MovInst wWin 
PROCEDURE Imp_MovInst :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  FIND FIRST Usuarios WHERE Usuarios.Usuario EQ Mov_Instancias.Usuario NO-LOCK NO-ERROR.

  DISPLAY SKIP (2)
          "Asociado  : "
          saro.Nit
          " "
          SKIP(1)
          "Detalle de la Instancia : "  SKIP
          Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro FORM "X(40)" SKIP(1)
          "Usuario   : "
          Mov_Instancias.Usuario         FORM "X(8)"
          Usuarios.Nombre                FORM "X(35)"
          "          Estado        : "
          Mov_Instancias.Estado                     SKIP
          "Solicitud : "
          Mov_Instancias.Num_Solicitud
          "                                              Fecha Ingreso : "
          Mov_Instancias.Fec_Ingreso     FORM "99/99/9999"               SKIP
          "No.Credito: "
          Mov_Instancias.Cuenta
          "                                        Fecha Retiro  : "
          Mov_Instancias.Fec_Retiro      FORM "99/99/9999" SKIP(2)

          SUBSTRING(Mov_Instancias.Descrip,1,  100)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,101,200)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,201,300)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,301,400)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,401,500)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,501,600)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,601,700)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,701,800)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,801,900)        FORM "X(100)"
          SUBSTRING(Mov_Instancias.Descrip,901,990)        FORM "X(100)"
       WITH DOWN WIDTH 150 FRAME F_MInst NO-BOX NO-LABELS STREAM-IO USE-TEXT.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe wWin 
PROCEDURE Informe :
{Incluido\RepEncabezado.i}
    DEFINE VAR Fir1             AS CHARACTER FORMAT "X(15)".
    DEFINE VAR Fir2             AS CHARACTER FORMAT "X(15)".
    DEFINE VAR W_FirSub         AS CHARACTER FORMAT "X(11)".
    DEFINE VAR W_Cliente        AS CHARACTER FORMAT "X(60)".
    DEFINE VAR Lin_1            AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_2            AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_2_1          AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_analista     AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_analista_F   AS CHARACTER FORMAT "X(100)" EXTENT 10.
    DEFINE VAR Lin_Aprueba_F    AS CHARACTER FORMAT "X(100)" EXTENT 5.
    DEFINE VAR Lin_analista_1   AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_analista_2   AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_analista_3   AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_analista_4   AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_analista_5   AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_analista_5B   AS CHARACTER FORMAT "X(100)" EXTENT 5.
    DEFINE VAR Lin_aprueba      AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_aprueba_1    AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_aprueba_2    AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_aprueba_3    AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_aprueba_4    AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_aprueba_5    AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_5            AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_5_1          AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_5_2          AS CHARACTER FORMAT "X(100)".
    DEFINE VAR Lin_5_3          AS CHARACTER FORMAT "X(100)".
    DEFINE VAR v_nombre_aprueba AS CHARACTER FORMAT "X(60)".
    DEFINE VAR v_nombre_Analista AS CHARACTER FORMAT "X(60)".
    DEFINE VAR v_nit_aprueba    LIKE usuario.nit.
    DEFINE VAR v_nit_Analista   LIKE usuario.nit.
    DEFINE VAR N_etapas_analisis  AS integer.

    DEFINE VAR W_AproNeg AS CHARACTER FORMAT "X(30)" INIT "CONDICIONADA".
    DEFI   VAR W_RowidI  AS ROWID. 
    DEFI   VAR W_NroI    AS INTEG FORM "9".
    DEFI   VAR W_Primero AS LOG INIT FALSE.

    ImpNegada = NO.
    IF saro.Estado EQ 2 THEN
       W_AproNeg = "A P R O B A D A".
    ELSE IF saro.Estado EQ 3 THEN
        W_AproNeg = "N  E  G  A  D  A".
            
    
    FIND usuarios WHERE usuarios.usuario =   Mov_Instancias.Usuario NO-LOCK NO-ERROR.
    IF AVAILABLE usuarios THEN DO:
              Lin_1 = trim(usuarios.nombre).
              v_nit_Aprueba =  usuario.nit.
    END.
    FIND instancias WHERE instancias.instancia = Mov_Instancias.Instancia NO-LOCK NO-ERROR.
    IF AVAILABLE(instancias) THEN
       Lin_1 =  Lin_1 + " (" + Instancias.Nom_Instancia + ")".
    ASSIGN Lin_2   =   trim(SUBSTRING(Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias,1,100))
           Lin_2_1 =   trim(SUBSTRING(Mov_Instancias.Descripcion:SCREEN-VALUE IN FRAME F_Instancias,101,100))
           W_RowidI = ROWID(Mov_Instancias)
           W_NroI   = 1.
    /*variable utilizada para mirar si hay 2 etapas de analisis, como es el caso de los creditos condicionados*/
    N_etapas_analisis = 0.
    FOR EACH Mov_Instancias WHERE Mov_Instancias.Nit           EQ saro.Nit
                              AND Mov_Instancias.Num_Solicitud EQ saro.Num_Referencia  NO-LOCK 
                            BREAK BY Mov_Instancia.Instancia BY Fec_Ingreso BY Hora_Ingreso:
        FIND Usuarios WHERE Usuarios.Usuario EQ  Mov_Instancias.Usuario NO-LOCK NO-ERROR.
        FIND Instancias WHERE Instancias.Instancia      EQ Mov_Instancias.Instancia AND
                              Instancias.Tipo_Instancia = TP_Instancia NO-LOCK  NO-ERROR.
        IF  LAST-OF(Mov_Instancia.Instancia) 
        AND AVAILABLE(Instancias)
        AND NOT Instancias.Ultima AND Instancias.Id_Scoring  THEN DO:
           /*comentarios de la instancia analisis*/
           IF AVAILABLE Usuarios THEN 
              Lin_analista = TRIM(Usuarios.Nombre)  +  " (" + Instancias.Nom_Instancia + ")".
           ASSIGN Lin_analista_1 =  SUBSTRING(Mov_Instancias.Descripcion,1,100)
                  Lin_analista_2 =  SUBSTRING(Mov_Instancias.Descripcion,101,100)
                  Lin_analista_3 =  SUBSTRING(Mov_Instancias.Descripcion,201,100)
                  Lin_analista_4 =  SUBSTRING(Mov_Instancias.Descripcion,301,100)
                  Lin_analista_5 =  SUBSTRING(Mov_Instancias.Descripcion,401,100)
                  Lin_analista_F [1] = SUBSTRING(Mov_Instancias.Descripcion,501,100)
                  Lin_analista_F [2] = SUBSTRING(Mov_Instancias.Descripcion,601,100)
                  Lin_analista_F [3] = SUBSTRING(Mov_Instancias.Descripcion,701,100)
                  Lin_analista_F [4] = SUBSTRING(Mov_Instancias.Descripcion,801,100)
                  Lin_analista_F [5] = SUBSTRING(Mov_Instancias.Descripcion,901,100)
                  Lin_analista_F [6] = SUBSTRING(Mov_Instancias.Descripcion,1001,100)
                  Lin_analista_F [7] = SUBSTRING(Mov_Instancias.Descripcion,1101,100)
                  Lin_analista_F [8] = SUBSTRING(Mov_Instancias.Descripcion,1201,100)
                  Lin_analista_F [9] = SUBSTRING(Mov_Instancias.Descripcion,1301,90)
                  v_nit_Analista =  Usuario.nit.
        END.

        /*comentarios de la instancia concepto final*/
        IF  LAST-OF(Mov_Instancia.Instancia)
        AND AVAILABLE(Instancias) AND Instancias.Ultima THEN DO:
            IF AVAILABLE Usuarios THEN 
                Lin_aprueba   = TRIM(usuarios.nombre) + " (" + Instancias.Nom_Instancia + ")".
            ASSIGN Lin_aprueba_1 =  SUBSTRING(Mov_Instancias.Descripcion,1,100)
                   Lin_aprueba_2 =  SUBSTRING(Mov_Instancias.Descripcion,101,100)
                   Lin_aprueba_3 =  SUBSTRING(Mov_Instancias.Descripcion,201,100)
                   Lin_aprueba_4 =  SUBSTRING(Mov_Instancias.Descripcion,301,100)
                   Lin_aprueba_5 =  SUBSTRING(Mov_Instancias.Descripcion,401,100)
                   Lin_Aprueba_F [1] = SUBSTRING(Mov_Instancias.Descripcion,501,100)
                   Lin_Aprueba_F [2] = SUBSTRING(Mov_Instancias.Descripcion,601,100)
                   Lin_Aprueba_F [3] = SUBSTRING(Mov_Instancias.Descripcion,701,100)
                   Lin_Aprueba_F [4] = SUBSTRING(Mov_Instancias.Descripcion,801,100)
                   Lin_Aprueba_F [5] = SUBSTRING(Mov_Instancias.Descripcion,901,90)
                   v_nit_Aprueba =  usuario.nit.
        END.
    END.
    
    FIND Mov_Instancias WHERE ROWID(Mov_Instancias) EQ W_RowidI NO-LOCK NO-ERROR.
    
    ASSIGN W_Reporte = "REPORTE   : SOLICITUD DE CREDITO....CONCEPTO FINAL : " +  STRING(W_AproNeg,"X(20)") +
                       "       FECHA: " + STRING(w_fecha) + " - " + STRING(TIME,"hh:mm am")
           W_EncColumna = "Cliente Solicitante         :   " + W_Cliente + " (" + 
                           STRING((W_Fecha - Clientes.Fec_Nacimiento) / 360,"99") + " Años)".
    
 VIEW FRAME F-Encabezado.
 /*VIEW FRAME F-Ftr.*/

 FOR EACH TmpI:   DELETE TmpI. END.
 FOR EACH CTmpI:  DELETE CTmpI. END.
 FOR EACH CCTmpI: DELETE CCTmpI. END.

 RUN Informacion_saro.  

 /*RUN Garantias_Imprimir.*/
                                                                                                                           
 FOR EACH TmpI BREAK BY TmpI.ILinea: 
   DISPLAY TmpI.ITexto SKIP(0)                                                                                                       
   WITH FRAME FINFO WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.
 END.
 
                                                                                                                           
 /*FOR EACH CTmpI BREAK BY CTmpI.ILinea: 
    IF NOT W_Primero THEN DO:
       IF      saro.Per_Pago EQ 1 THEN CTmpI.Egr = CTmpI.Egr + (saro.Cuota * 4.3).
       ELSE IF saro.Per_Pago EQ 2 THEN CTmpI.Egr = CTmpI.Egr + (saro.Cuota * 3).
       ELSE IF saro.Per_Pago EQ 3 THEN CTmpI.Egr = CTmpI.Egr + (saro.Cuota * 2).
       ELSE                                 CTmpI.Egr = CTmpI.Egr + saro.Cuota.
       W_Primero = TRUE.
    END.

    DISPLAY CTmpI.ITexto                                                                                                    
            CTmpI.Endeu                                                                                                     
            CTmpI.Ingr                                                                                                      
            CTmpI.Egr                                                                                                      
            CTmpI.Punto SKIP(0)                                                                                         
       WITH FRAME FGarCont WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.                                                  
 END.*/
                                                                                                                           
/* IF saro.Estado NE 3 THEN DO:
    DISPLAY SKIP(1)                                                                                                        
        "VARIABLES                  DEUDOR                 CODEUDOR-1              CODEUDOR-2          CODEUDOR-3" SKIP(0) 
          WITH FRAME FVarCont WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.                                               
                                                                                                                           
    FOR EACH CCTmpI BREAK BY CCTmpI.ILinea:                                                                                
      DISPLAY CCTmpI.ITexto                                                                                                
              "     "                                                                                                      
              CCTmpI.Deud                                                                                                  
              CCTmpI.Cod1                                                                                                  
              CCTmpI.Cod2                                                                                                  
              CCTmpI.Cod3     SKIP(0)                                                                                      
      WITH FRAME FVCont WIDTH 132 NO-LABELS USE-TEXT NO-BOX STREAM-IO.                                                 
    END.                                                                                                                   
 END.*/
 /*Encontramos los nombres de las personas que aprueban  y analisan*/
 FIND clientes WHERE clientes.nit = v_nit_Aprueba NO-LOCK NO-ERROR.
 IF AVAILABLE(clientes)THEN
    v_nombre_Aprueba = trim(clientes.nombre) + " " + TRIM(clientes.apellido1) + " " + TRIM(clientes.apellido2).

 FIND clientes WHERE clientes.nit = v_nit_Analista NO-LOCK NO-ERROR.
 IF AVAILABLE(clientes)THEN
    v_nombre_Analista = trim(clientes.nombre) + " " + TRIM(clientes.apellido1) + " " + TRIM(clientes.apellido2).
 
 W_FirSub = "Firma:".
 /*IF saro.Cuantia_Riesgo GE (W_SMLV * 20) AND  saro.Cuantia_Riesgo LE (W_SMLV * 26) THEN
    W_FirSub =  "Supernumerario". 

 IF saro.Cuantia_Riesgo GT (W_SMLV * 26) THEN
    W_FirSub =  "Gerente".*/ 

 IF saro.Cuantia_Riesgo GE (W_SMLV * 20) THEN /* Deshabilitado el 22 oct - vacaciones Subgerente */
    W_FirSub = "Subgerente:".

    ASSIGN Fir1 = "Jefe de Crédito:__________________"
           Fir2 = "Analista:_________________________".
 RUN Imprimir_Conceptos.

 DISPLAY  SKIP(5)
          Fir1              AT 2  
          W_FirSub AT 42 "______________________"  AT 55 
          Fir2              AT 80 SKIP
          "C.C. NIT."                              AT 42 SKIP
          v_nombre_Aprueba                        AT 2  
          v_nombre_Analista                       AT 80
     WITH WIDTH 150 FRAME F-Totales NO-LABEL NO-BOX NO-UNDERLINE USE-TEXT STREAM-IO.
     
 /*DISPLAY SKIP(1)
      "Comentarios de Instancias y Concepto : " AT 1
      "----------------------------------------------------------------------------------------" AT 1
      Lin_analista      AT 1
      Lin_analista_1    AT 1
      Lin_analista_2    AT 1
      Lin_analista_3    AT 1
      Lin_analista_4    AT 1
      Lin_analista_5    AT 1 
      Lin_analista_F [1] AT 1
      Lin_analista_F [2] AT 1
      Lin_analista_F [3] AT 1
      Lin_analista_F [4] AT 1
      Lin_analista_F [5] AT 1 
      Lin_analista_F [6] AT 1
      Lin_analista_F [7] AT 1
      Lin_analista_F [8] AT 1
      Lin_analista_F [9] AT 1 SKIP (1)
      Lin_aprueba       AT 1
      Lin_aprueba_1     AT 1
      Lin_aprueba_2     AT 1
      Lin_aprueba_3     AT 1
      Lin_aprueba_4     AT 1
      Lin_aprueba_5     AT 1 
      Lin_aprueba_F[1]  AT 1 
      Lin_aprueba_F[2]  AT 1
      Lin_aprueba_F[3]  AT 1
      Lin_aprueba_F[4]  AT 1
      Lin_aprueba_F[5]  AT 1 SKIP(1)
      Fir1              AT 2  
      W_FirSub AT 42 "______________________"  AT 55 
      Fir2              AT 80 SKIP
      "C.C. NIT."                              AT 42 SKIP
       v_nombre_Aprueba                        AT 2  
       v_nombre_Analista                       AT 80
      WITH WIDTH 150 FRAME F-Totales NO-LABEL NO-BOX NO-UNDERLINE USE-TEXT STREAM-IO.*/
 Aprueba = NO.
/* PAGE.*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe3 wWin 
PROCEDURE Informe3 :
{Incluido\RepEncabezado.i}
    DEFINE VAR W_Cliente AS CHARACTER FORMAT "X(60)".
    DEFINE VAR T_Plazo   AS CHARACTER FORMAT "X(30)".
    DEFINE VAR T_Dedu    AS CHARACTER FORMAT "X(30)".
    
    
 
    W_Reporte   = "REPORTE   : SOLICITUD DE CREDITO - FECHA: " + STRING(w_fecha) + " - " + STRING(TIME,"hh:mm am").
    W_EncColumna = "Cliente Solicitante         :   " + W_Cliente.
    
 VIEW FRAME F-Encabezado.
 VIEW FRAME F-Ftr.


   DISPLAY 
  /* 1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890*/
     "=============================================DATOS GENERALES DE LA SOLICITUD==============================================" AT 1
     "Agencia de Radicación       : " AT 1
     Cmb_Agencias:SCREEN-VALUE IN FRAME F_Sarouno       AT 33 FORMAT "X(30)"
     "Instancia Actual            : " AT 1
     Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro AT 33  FORMAT "X(30)"
     "Usuario Actualmente Procesa : " AT 65
     NomUsuario:SCREEN-VALUE IN FRAME F_Saro AT 98  FORMAT "X(30)" 
     
     "=============================================DETALLE DE VALORES DE SOLICITUD==============================================" AT 1
     "==============================================DATOS COMPLEMENTARIOS=================================================" AT 1
   WITH FRAME F_Sol WIDTH 132 NO-BOX USE-TEXT NO-LABELS STREAM-IO.
 
  
 PAGE.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Informe_Imp wWin 
PROCEDURE Informe_Imp :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VAR W_Dispositivo AS CHARACTER INITIAL " ".
  DEFINE VAR W_sw          AS LOGICAL. 
  DEFI   VAR lista         AS CHAR FORM "X(35)".

  Lista = W_PathSpl + "AprobNeg-" + STRING(saro.Num_Referencia).
  
  RUN P-DisPos IN W_Manija (INPUT-OUTPUT Lista,INPUT-OUTPUT W_Dispositivo).
  IF W_Dispositivo = "" THEN
     RETURN.

  RUN _SetCurs.r ("WAIT").
  OUTPUT TO VALUE(Lista) NO-ECHO PAGED PAGE-SIZE 80.

  RUN Informe.

  OUTPUT CLOSE.        
  RUN _SetCurs.r ("ARROW").
  IF W_Dispositivo = "P" THEN  
    RUN Pantalla IN W_Manija (INPUT Lista).
  ELSE                                                  
    IF W_Dispositivo = "I" THEN
       RUN adecomm/_osprint.r ( INPUT  ?, INPUT Lista,INPUT  ?,INPUT  1,INPUT  1,
                                          INPUT  99999,OUTPUT W_sw).
  /*IF W_Dispositivo <> "A" THEN
     OS-DELETE VALUE(Lista).*/

  IF W_Dispositivo = "E" THEN
     RUN Imprimir_Excel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Inicializar_Variables wWin 
PROCEDURE Inicializar_Variables :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DO WITH FRAME F_Sarouno:
   
   FIND Agencias WHERE Agencias.Agencia EQ W_Agencia NO-LOCK NO-ERROR.
   IF AVAILABLE Agencias THEN DO:
      ASSIGN Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre
             W-Agencias:SCREEN-VALUE   = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.
      APPLY "value-changed" TO Cmb_Agencias.
   END.

   IF NOT W_Nuevo THEN 
      saro.Num_Referencia:SCREEN-VALUE = "0".
   ELSE 
      saro.Num_Referencia:SCREEN-VALUE = "     ".
   
   RUN Instancia_Hidden.

   ASSIGN saro.Fec_grabacion:SCREEN-VALUE  = STRING(w_fecha)
          Saro.Hora_Grabacion:SCREEN-VALUE = STRING(TIME,'HH:MM:SS') 
          saro.Fec_Des_Even:SCREEN-VALUE   = STRING(w_fecha)
          Saro.Hora_Des_Even:SCREEN-VALUE  = STRING(TIME,'HH:MM:SS')
          saro.Descrip_evento:SCREEN-VALUE = ""
          Nom_Producto:SCREEN-VALUE        = ""
          saro.Fec_Ini_Even:SCREEN-VALUE   = STRING(w_fecha)
          saro.Hora_Ini_Even:SCREEN-VALUE  = STRING(TIME,'HH:MM:SS')
          W-ClaseRiesgo:SCREEN-VALUE      = ""
          Cmb_Zonas:SCREEN-VALUE           = ""
          saro.Fec_Fin_Even:SCREEN-VALUE   = STRING(w_fecha)
          saro.Hora_Fin_Even:SCREEN-VALUE  = STRING(TIME,'HH:MM:SS')
          Cmb_Generacion:SCREEN-VALUE      = ""
          Cmb_Rieposibles:SCREEN-VALUE     = ""
          Nivel3:SCREEN-VALUE              = ""
          Nivel2:SCREEN-VALUE              = ""
          Saro.Canal_Servicio:SCREEN-VALUE = "Oficina".


  
          
END.

DO WITH FRAME F_Cuantia:
    ASSIGN saro.Nit:SCREEN-VALUE            = ""
           NomNit:SCREEN-VALUE              = ""
           Saro.Cuantia_Riesgo:SCREEN-VALUE = ""
           Saro.Cuantia_Recup_Tot:SCREEN-VALUE = "0"
           Saro.Cuantia_Recup_Seg:SCREEN-VALUE = "0"
           Saro.Nit_seguro:SCREEN-VALUE        = ""
           NomNit_Seguros:SCREEN-VALUE         = "".


END.
                       
DO WITH FRAME F_Producto:
   ASSIGN Saro.Clase_Producto:SCREEN-VALUE = "1".
   DISABLE Tip_Credito.

   ASSIGN saro.Clase_producto:SCREEN-VALUE = "".
        

END.





END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*Buscando la instancia 8 para ser mostrada en este procedimeiento, tipo del 
  producto 3 = Especial */
    
  ASSIGN TP_Instancia   = 8
         TP_InsProducto = 3.

  FOR EACH Instancias WHERE Instancias.Tipo_Instancia = TP_Instancia 
                        AND Instancias.Estado         = 1 
                        AND Instancias.Tipo_Producto  = TP_InsProducto 
      NO-LOCK BREAK BY Instancias.Orden:
  
      /*Asigna en variables cual es la primera y ultima instancia*/
      IF Instancias.Ultima  THEN W_Ultima = Instancias.Instancia.
      IF Instancias.Primera THEN W_Primera = Instancias.Instancia.

      /*Y si hay alguna intsncia negada */
      IF Instancias.Id_Negadas THEN W_Negadas = Instancias.Instancia.

      /*Busca si la instancia esta es vigente para el usuario */
      FIND FIRST Cfg_Instancias WHERE Cfg_Instancias.Tipo_Instancia = Instancias.Tipo_Instancia  
                                  AND Cfg_Instancias.Instancia      = Instancias.Instancia 
                                  AND Cfg_Instancias.Usuario        = W_Usuario 
                                  AND Cfg_Instancias.Estado  EQ 1 
      NO-LOCK NO-ERROR.
      IF AVAILABLE Cfg_Instancias THEN DO:
         W_Ok = Cmb_Instancias:ADD-LAST(STRING(Instancias.Instancia,"99999") + " - " + Instancias.Nom_Instancia) 
         IN FRAME F_Saro.
      END.
  END.
  IF W_Ultima EQ 0 THEN DO:
     MESSAGE "No se ha definido la ultima instancia en el proceso de" SKIP
             "saro. define una instancia con el parametro ultimo" 
       VIEW-AS ALERT-BOX.
  END.

  IF W_Primera EQ 0 THEN DO:
     MESSAGE "No se ha definido la primera instancia en el proceso de" SKIP
             "saro. define una instancia con el parametro primera" 
     VIEW-AS ALERT-BOX.
  END.

  FIND Usuarios WHERE Usuarios.Usuario EQ W_Usuario NO-LOCK NO-ERROR.
  ASSIGN NomUsuario = W_Usuario + " - " + Usuarios.Nombre
         W_SucAgen  = Usuarios.Id_OpeOfi.
  
  
  DO WITH FRAME F_Sarouno:
    FOR EACH Agencias WHERE Agencia.Estado EQ 1 NO-LOCK:
      W_Ok = Cmb_Agencias:ADD-LAST(STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre).
    END.

    FOR EACH Pro_Saro WHERE Pro_Saro.estado = 1
                        AND Pro_Saro.tipo   = 2 NO-LOCK:
      W_Ok = Cmb_Rieposibles:ADD-LAST(STRING(Pro_Saro.Cod_Factor,"999999") + " - " + Pro_Saro.nombre).
    END.    

    Cmb_Rieposibles:LIST-ITEMS = "," + Cmb_Rieposibles:LIST-ITEMS.

    /* FOR EACH varios WHERE Varios.tipo = 35 NO-LOCK:
      W_Ok = Cmb_Cla_Riesgo:ADD-LAST(STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion).
    END.
    Cmb_Cla_Riesgo:LIST-ITEMS  = "," + Cmb_Cla_Riesgo:LIST-ITEMS. */

    FOR EACH varios WHERE Varios.tipo = 36 NO-LOCK:
      W_Ok = Cmb_Proceso:ADD-LAST(STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion).
    END.
    Cmb_Proceso:LIST-ITEMS = "," + Cmb_Proceso:LIST-ITEMS.

    FOR EACH zonas WHERE Zonas.Estado = 1 NO-LOCK:
      W_Ok = Cmb_Zonas:ADD-LAST(STRING(Zonas.Cod_zona,"999") + " - " + Zonas.Nombre).
    END.
    Cmb_Zonas:LIST-ITEMS = "," + Cmb_zonas:LIST-ITEMS.          
  END.                             
  RUN SUPER.

  Cmb_Instancias:SCREEN-VALUE = Cmb_Instancias:ENTRY(1). 
  RUN Solicitudes_X_Instancia.
  APPLY "entry" TO Cmb_Instancias IN FRAME F_Saro.
  
  RUN Instancia_Hidden.

  HIDE FRAME F_AsentarInstancia.
  HIDE FRAME F_Cerradas.
  HIDE FRAME F_CredACanc.
  HIDE FRAME F_Agregar.

  WWin:TITLE = "Proceso de SARO - Agencia Actual: " + STRING(W_Agencia).
  Buscar:LABEL IN FRAME F_Consulta = "Buscar x Riesgo".
  APPLY "value-changed" TO Cmb_Instancias IN FRAME F_Saro.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Instancia_Hidden wWin 
PROCEDURE Instancia_Hidden :
/*
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    DEF VAR i AS INTEGER NO-UNDO.
    i = INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)).
    DO WITH FRAME F_sarouno:
        IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 610 
        THEN DO:
            Saro.Fec_Des_Even:BGCOLOR = 15.
            Saro.Fec_Des_Even:FGCOLOR = 0.     
            Saro.Hora_Des_Even:BGCOLOR = 15.
            Saro.Hora_Des_Even:FGCOLOR = 0.     
            Saro.Descrip_Evento:BGCOLOR = 15.
            Saro.Descrip_Evento:FGCOLOR = 0.     
            ENABLE Saro.Fec_Des_Even Saro.Hora_Des_Even  Saro.Descrip_Evento.
            DISABLE Btn_Cuantia Btn_LnOperativas Btn_Estado.
        END.
        IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) <> 610 
        THEN DO:
            Saro.Fec_Des_Even:BGCOLOR   = 18.
            Saro.Fec_Des_Even:FGCOLOR   = 15.     
            Saro.Hora_Des_Even:BGCOLOR  = 18.
            Saro.Hora_Des_Even:FGCOLOR  = 15.     
            Saro.Descrip_Evento:BGCOLOR = 15.
            Saro.Descrip_Evento:FGCOLOR = 15.     
    
            DISABLE Saro.Fec_Des_Even Saro.Hora_Des_Even Saro.Descrip_Evento. 
        END.
        IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 620 
        THEN DO:
            ASSIGN  Cmb_Generacion:HIDDEN IN FRAME F_Sarouno         = TRUE
                    Saro.Fec_Fin_Even:HIDDEN IN FRAME F_sarouno      = TRUE 
                    Saro.Hora_Fin_Even:HIDDEN IN FRAME F_sarouno     = TRUE.
            ASSIGN  Saro.Fec_Ini_Even:HIDDEN IN FRAME F_Sarouno      = NO
                    Saro.Hora_Ini_Even:HIDDEN IN FRAME F_Sarouno     = NO
                    W-ClaseRiesgo:HIDDEN IN FRAME F_Sarouno          = NO
                    Nom_Producto:HIDDEN IN FRAME F_Sarouno           = NO
                    Btn_Producto:HIDDEN IN FRAME F_Sarouno           = NO
                    Cmb_Proceso:HIDDEN IN FRAME F_Sarouno            = NO
                    Saro.Canal_Servicio:HIDDEN IN FRAME F_sarouno    = NO
                    Cmb_Zonas:HIDDEN IN FRAME F_Sarouno              = NO.
            IF saro.Hora_Ini_Even:SCREEN-VALUE  = "" 
            THEN    saro.Hora_Ini_Even:SCREEN-VALUE  = STRING(TIME,'HH:MM:SS').
                    Saro.Descrip_Evento:FGCOLOR = 15.
                    Saro.Canal_Servicio:BGCOLOR = 15.
                    Saro.Canal_Servicio:FGCOLOR = 0.
                    Cmb_Zonas:BGCOLOR = 15.
                    Cmb_Zonas:FGCOLOR = 0.
            ENABLE  Saro.Fec_Ini_Even  
                    Saro.Hora_Ini_Even
                    /* W-ClaseRiesgo */
                    Btn_Producto
                    Cmb_Proceso
                    Saro.Canal_Servicio
                    Cmb_Zonas. 
            DISABLE Saro.Fec_Fin_Even 
                    Saro.Hora_Fin_Even 
                    Cmb_Generacion
                    Btn_LnOperativas
                    Btn_Estado.
        END.
        IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) > 620 
        THEN DO:
            Saro.Canal_Servicio:BGCOLOR = 18.
            Saro.Canal_Servicio:FGCOLOR = 15.
            Cmb_Zonas:BGCOLOR = 18.
            Cmb_Zonas:FGCOLOR = 15.
        
            DISABLE Saro.Canal_Servicio Cmb_Zonas. 
        END.
    
        IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 630 
        THEN DO:
            ASSIGN Btn_Estado:HIDDEN IN FRAME F_Sarouno             = TRUE.
            
            ASSIGN  Saro.Fec_Ini_Even:HIDDEN IN FRAME F_Sarouno      = NO
                    Saro.Hora_Ini_Even:HIDDEN IN FRAME F_Sarouno     = NO
                    /* Cmb_Cla_Riesgo:HIDDEN IN FRAME F_Sarouno      = NO */
                    Nom_Producto:HIDDEN IN FRAME F_Sarouno           = NO
                    Btn_Producto:HIDDEN IN FRAME F_Sarouno           = NO
                    Cmb_Proceso:HIDDEN IN FRAME F_Sarouno            = NO
                    Saro.Canal_Servicio:HIDDEN IN FRAME F_sarouno    = NO
                    Cmb_Zonas:HIDDEN IN FRAME F_Sarouno              = NO.
        
            ASSIGN  Cmb_Generacion:HIDDEN IN FRAME F_Sarouno         = FALSE
                    Saro.Fec_Fin_Even:HIDDEN IN FRAME F_sarouno      = FALSE
                    Saro.Hora_Fin_Even:HIDDEN IN FRAME F_sarouno     = FALSE
                    Btn_Cuantia:HIDDEN IN FRAME F_sarouno            = FALSE
                    Btn_Cuantia:VISIBLE IN FRAME F_sarouno           = TRUE
                    Btn_LnOperativas:HIDDEN IN FRAME F_sarouno       = FALSE
                    Btn_LnOperativas:VISIBLE IN FRAME F_sarouno      = TRUE.
        
            IF saro.Hora_Fin_Even:SCREEN-VALUE  = "" 
            THEN saro.Hora_Fin_Even:SCREEN-VALUE  = STRING(TIME,'HH:MM:SS').
            ENABLE  Saro.Fec_Ini_Even  
                    Saro.Hora_Ini_Even
                    Saro.Fec_Fin_Even 
                    Saro.Hora_Fin_Even 
                    /* Cmb_Cla_Riesgo */
                    Btn_Producto
                    Cmb_Proceso
                    Cmb_Generacion
                    Btn_LnOperativas
                    Btn_Cuantia WITH FRAME F_Sarouno.
            DISABLE Btn_Estado.
        END.
        IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) = 640 
        THEN DO:
            DISABLE ALL WITH FRAME F_Sarouno.
            ENABLE Btn_Estado.
        END.
    END. /*fin del do principal*/
        
/*   
W-Agencias             Saro.Fec_Des_Even    Saro.Hora_Fin_Even    Cmb_Generacion     
W-ClaseRiesgo          Saro.Hora_Des_Even   Cmb_Proceso           Saro.Descrip_Evento
Cmb_Agencias           Cmb_Rieposibles      Cmb_zonas             Btn_Cuantia        
Saro.Num_Referencia    Saro.Fec_Ini_Even    Nom_Producto          Btn_LnOperativas   
Saro.Fec_Grabacion     Saro.Hora_Ini_Even   Btn_Producto          Btn_Estado         
Saro.Hora_Grabacion    Saro.Fec_Fin_Even    Saro.Canal_Servicio   Nivel3             
                                                                   Nivel2             
*/                                              
    DO WITH FRAME F_Sarouno:
        CASE i:
            WHEN 610
            THEN .
            WHEN 620
            THEN do:
                DISABLE
                W-Agencias             Saro.Fec_Des_Even    Saro.Hora_Fin_Even    Cmb_Generacion     
                W-ClaseRiesgo          Saro.Hora_Des_Even   Cmb_Proceso           Saro.Descrip_Evento
                Cmb_Agencias           Cmb_Rieposibles      Cmb_zonas                     
                Saro.Num_Referencia    Saro.Fec_Ini_Even    Nom_Producto             
                Saro.Fec_Grabacion     Saro.Hora_Ini_Even   Btn_Producto          Btn_Estado         
                Saro.Hora_Grabacion    Saro.Fec_Fin_Even    Saro.Canal_Servicio   Nivel3.             
                ENABLE Btn_Cuantia Btn_LnOperativas.
            END.
            WHEN 630
            THEN do:
                DISABLE
                W-Agencias             Saro.Fec_Des_Even    Saro.Hora_Fin_Even    Cmb_Generacion     
                W-ClaseRiesgo          Saro.Hora_Des_Even   Cmb_Proceso           Saro.Descrip_Evento
                Cmb_Agencias           Cmb_Rieposibles      Cmb_zonas                     
                Saro.Num_Referencia    Saro.Fec_Ini_Even    Nom_Producto             
                Saro.Fec_Grabacion     Saro.Hora_Ini_Even   Btn_Producto          Btn_Estado         
                Saro.Hora_Grabacion    Saro.Fec_Fin_Even    Saro.Canal_Servicio   Nivel3.             
                ENABLE Btn_Cuantia Btn_LnOperativas.
            END.
            WHEN 640
            THEN do:
                DISABLE
                W-Agencias             Saro.Fec_Des_Even    Saro.Hora_Fin_Even    Cmb_Generacion     
                W-ClaseRiesgo          Saro.Hora_Des_Even   Cmb_Proceso           Saro.Descrip_Evento
                Cmb_Agencias           Cmb_Rieposibles      Cmb_zonas                     
                Saro.Num_Referencia    Saro.Fec_Ini_Even    Nom_Producto             
                Saro.Fec_Grabacion     Saro.Hora_Ini_Even   Btn_Producto          Btn_Estado         
                Saro.Hora_Grabacion    Saro.Fec_Fin_Even    Saro.Canal_Servicio   Nivel3.             
                ENABLE Btn_Cuantia Btn_LnOperativas.
            END.
        END CASE. /* CASE i: */
    END.
            
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Mostrar_Saro wWin 
PROCEDURE Mostrar_Saro :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
    IF Saro.Estado EQ 4 THEN
        MESSAGE "Esta Evento fue Retornada del Proceso Concepto Final," SKIP
                "Por favor Priorice su proceso, si cumple los nuevos requisitos."
            VIEW-AS ALERT-BOX INFO BUTTONS OK.

    DO WITH FRAME F_Sarouno:
        FIND Agencias WHERE Agencias.Agencia EQ Saro.Agencia NO-LOCK NO-ERROR.
        IF AVAILABLE Agencias THEN
            ASSIGN Cmb_Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre
                   W-Agencias:SCREEN-VALUE = STRING(Agencias.Agencia,"999") + " - " + Agencias.Nombre.

        DISPLAY Saro.Num_Referencia 
                Saro.Fec_Grabacion
                Saro.Hora_Grabacion
                Saro.Fec_Des_Even
                Saro.Hora_Des_Even  
                Saro.Canal_Servicio
                Saro.Descrip_Even.
        ASSIGN v-cod-factorO = INTEGER(Saro.Cod_Evento).
        /*Evento de Saro*/
        FIND FIRST Pro_saro WHERE Pro_saro.Estado = 1
                              AND Pro_Saro.Cod_Factor = INT(Saro.Cod_Evento)
        NO-LOCK NO-ERROR.
        IF AVAILABLE Pro_saro THEN DO:
          Cmb_Rieposibles:SCREEN-VALUE IN FRAME F_Sarouno = STRING(Pro_Saro.Cod_Factor,"999999") + " - " + Pro_Saro.nombre.
        END.

        /*Ubicacion del Factor Interno o Externo */
        FIND FIRST varios WHERE Varios.tipo   = 38 
                            AND Varios.Codigo = INT(Saro.Cod_Factor1)
        NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN DO:
          Nivel3:SCREEN-VALUE IN FRAME F_Sarouno = STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion.  
        END.
    
        /*Factor de Riesgo */
        FIND FIRST varios WHERE Varios.tipo   = 37 
                            AND Varios.Codigo = INT(Saro.Cod_Factor2)
        NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN DO:
           Nivel2:SCREEN-VALUE IN FRAME F_Sarouno = STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion.  
        END.
    
        /*Clase de Riesgo*/
        FIND FIRST varios WHERE Varios.tipo   = 35
                            AND varios.Codigo = Saro.Clase_ROperativo
        NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN
           ASSIGN /* Cmb_Cla_Riesgo:SCREEN-VALUE = STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion */
                  W-ClaseRiesgo:SCREEN-VALUE IN FRAME F_Sarouno = STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion.


        /*Proceso*/
        FIND FIRST varios WHERE Varios.tipo   = 36
                            AND varios.Codigo = saro.Clase_Proceso
        NO-LOCK NO-ERROR.
        IF AVAILABLE varios THEN
           Cmb_Proceso:SCREEN-VALUE = STRING(Varios.Codigo,"999") + " - " + Varios.Descripcion.

        /*zona Geografica*/
        FOR EACH zonas WHERE Zonas.Estado = 1 
                         AND Zonas.Cod_zona = Saro.Zona_Geografica 
          NO-LOCK:
          Cmb_zonas:SCREEN-VALUE = STRING(Zonas.Cod_zona,"999") + " - " + Zonas.Nombre.
        END.
        
        /*clase de perdida*/
        Cmb_Generacion:SCREEN-VALUE = STRING(Saro.Tip_perdida).
        


        IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) >= 620 THEN DO:
            
            DISPLAY Saro.Fec_Ini_Even  
                    Saro.Hora_Ini_Even. 
    
        END.

        IF INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) >= 630 THEN DO:
            DISPLAY Saro.Fec_Fin_Even 
                    Saro.Hora_Fin_Even.
              
                  /*Cmb_Generacion*/
        END.
  
    
        APPLY "value-changed" TO Cmb_agencias.
        ENABLE {&List-1}.
   
    END. /*fin do frame sario uno*/

   
    DO WITH FRAME F_Cuantia:
        /*nombre del tercero*/
            FIND FIRST clientes WHERE clientes.nit = saro.nit
            NO-LOCK NO-ERROR.
            IF AVAILABLE clientes THEN
                 NomNit:SCREEN-VALUE = Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.

            DISPLAY Saro.Nit
                    Saro.Cuantia_Riesgo
                    Saro.Cuantia_Recup_Tot
                    Saro.Cuantia_Recup_Seg.
    END.
      


    DO WITH FRAME F_Producto:
        

        ASSIGN Saro.Clase_Producto:SCREEN-VALUE = STRING(Saro.Clase_Producto).
        APPLY "value-changed" TO Saro.Clase_Producto.
        IF Saro.Clase_Producto = 2  THEN DO:
            IF Saro.Cod_Producto NE ? THEN DO:
               FIND Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ Saro.Tip_Credito  
                                   AND Pro_Creditos.Cod_Credito EQ Saro.Cod_Producto 
               NO-LOCK NO-ERROR.
               IF AVAILABLE Pro_Creditos THEN DO:
                   ASSIGN Cmb_Productos:LIST-ITEMS = ''.
                   FOR EACH Pro_Creditos WHERE Pro_Creditos.Tip_Credito EQ Saro.Tip_Credito AND
                                               Pro_Creditos.Estado  EQ 1 NO-LOCK:
                      W_Ok = Cmb_Productos:ADD-LAST(STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto).
                      IF AVAILABLE Saro AND Saro.Cod_Producto EQ Pro_Creditos.Cod_Credito THEN
                         Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Creditos.Cod_Credito,"999") + " - " + Pro_Creditos.Nom_Producto.
                   END.
               END.
               ELSE 
                   MESSAGE "No se encuentra el producto de créditos en el evento del Saro" VIEW-AS ALERT-BOX ERROR.
            END.
        END.

        IF Saro.Clase_Producto = 1  THEN DO:
            FIND Pro_Ahorros WHERE Pro_Ahorros.Estado     EQ 1 
                               AND Pro_Ahorros.Cod_Ahorro = Saro.Cod_Producto
            NO-LOCK NO-ERROR.
            IF AVAILABLE Pro_Ahorros THEN DO:
                Cmb_Productos:SCREEN-VALUE IN FRAME F_Producto = STRING(Pro_Ahorros.Cod_Ahorro,"999") + " - " + Pro_Ahorros.Nom_Producto.
            END.
            ELSE 
                MESSAGE "No se encuentra el producto del evento de SARO en el evento del Saro" 
                    VIEW-AS ALERT-BOX ERROR.
        END.
        

        APPLY "value-changed" TO Cmb_Productos.
        APPLY "choose" TO Btn_OutProductos.
    END. /*fin forma F_producto*/


    APPLY "choose" TO Btn_Salvar IN FRAME F_saro.





END PROCEDURE.

/*
DEFINE VAR x1 AS CHARACTER FORMAT "X(40)".
DEFI   VAR J  AS INTEG FORM "9".
*/

  

  /*
DO WITH FRAME F_Desembolso:
   ASSIGN Saro.Desembolso:SCREEN-VALUE = STRING(Saro.Desembolso)
          Saro.Age_Desembolso:SCREEN-VALUE = STRING(Saro.Age_Desembolso)
          Saro.Cod_Desembolso:SCREEN-VALUE = STRING(Saro.Cod_Desembolso)
          Saro.Cue_Desembolso:SCREEN-VALUE = Saro.Cue_Desembolso.
          
   IF Saro.Cue_Desembolso NE "" THEN DO:
      FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Saro.Cod_Desembolso NO-LOCK NO-ERROR.
      IF AVAILABLE Pro_Ahorros THEN ASSIGN W_NomProDesembolso:SCREEN-VALUE = Pro_Ahorros.Nom_Producto.
      FIND Agencias WHERE Agencias.Agencia EQ Saro.Age_Desembolso NO-LOCK NO-ERROR.
      IF AVAILABLE Agencias THEN ASSIGN W_NomAgeDesembolso:SCREEN-VALUE = Agencias.Nombre.
   END.
 END.

 DO WITH FRAME F_ForPago:
   ASSIGN Saro.FOR_Pago:SCREEN-VALUE = STRING(Saro.FOR_Pago)
          Saro.Age_DebAutomatico:SCREEN-VALUE = STRING(Saro.Age_DebAutomatico)
          Saro.Cod_DebAutomatico:SCREEN-VALUE = STRING(Saro.Cod_DebAutomatico)
          Saro.Cue_DebAutomatico:SCREEN-VALUE = Saro.Cue_DebAutomatico.
   CASE Saro.FOR_Pago:
     WHEN 1 THEN ASSIGN W_ForPAgo:SCREEN-VALUE IN FRAME F_Saro = "Caja"
                        Saro.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "1".
     WHEN 2 THEN ASSIGN W_ForPAgo:SCREEN-VALUE IN FRAME F_Saro = "Nómina"
                        Saro.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "2".
     WHEN 3 THEN ASSIGN W_ForPAgo:SCREEN-VALUE IN FRAME F_Saro = "Débito Automático"
                        Saro.FOR_Pago:SCREEN-VALUE IN FRAME F_ForPago = "3".
 END CASE.
   
 IF Saro.Cue_DebAutomatico NE "" THEN DO:
      FIND Pro_Ahorros WHERE Pro_Ahorros.Cod_Ahorro EQ Saro.Cod_DebAutomatico NO-LOCK NO-ERROR.
      IF AVAILABLE Pro_Ahorros THEN ASSIGN W_NomCodDebAutomatico:SCREEN-VALUE = Pro_Ahorros.Nom_Producto.
      FIND Agencias WHERE Agencias.Agencia EQ Saro.Age_DebAutomatico NO-LOCK NO-ERROR.
      IF AVAILABLE Agencias THEN ASSIGN W_NomAgeDebAutomatico:SCREEN-VALUE = Agencias.Nombre.
   END.
   CASE Saro.Desembolso:
     WHEN 1 THEN W_Desembolso:SCREEN-VALUE IN FRAME F_Saro = "Efectivo".
     WHEN 2 THEN W_Desembolso:SCREEN-VALUE IN FRAME F_Saro = "Cheque".
     WHEN 3 THEN W_Desembolso:SCREEN-VALUE IN FRAME F_Saro = "Cuenta de Ahorros".
     WHEN 4 THEN W_Desembolso:SCREEN-VALUE IN FRAME F_Saro = "Orden a Terceros".
   END CASE.
 END.
    */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*
------------------------------------------------------------------*/
  IF W_SiMInst THEN DO:
     RUN Imp_MovInst.
     W_SiMInst = FALSE.
     RETURN.
  END.
    
  IF W_TipoInforme EQ "Proyeccion" THEN 
     RUN Proyeccion.
  ELSE 
     RUN Informe.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE SInfo wWin 
PROCEDURE SInfo :
/*DEFINE INPUT PARAMETER texto AS CHARACTER FORMAT "X(60)".
    W_Ok = S_InfoCliente:ADD-LAST(texto) IN FRAME F_InfoCliente.
    */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Solicitudes_X_Instancia wWin 
PROCEDURE Solicitudes_X_Instancia :
/*Borando Tabla Temporal*/  
  FOR EACH Consulta: DELETE Consulta. END.

  FOR EACH Mov_Instancias WHERE Mov_Instancias.Instancia EQ INT(SUBSTRING(Cmb_Instancias:SCREEN-VALUE IN FRAME F_Saro,1,5)) 
                            AND Mov_Instancias.Usuario   EQ W_Usuario 
                            AND Mov_Instancias.Estado    EQ NO NO-LOCK:

    FIND FIRST saro WHERE Saro.Num_Referencia EQ Mov_Instancias.Num_Solicitud 
                      AND Saro.Nit            EQ Mov_Instancias.Nit          
                      AND Saro.Estado         NE 2 
    NO-LOCK NO-ERROR.
    IF AVAILABLE Saro THEN DO:
       CREATE Consulta.
       ASSIGN Consulta.AgeSaro        = saro.Agencia
              Consulta.Estado         = saro.Estado
              Consulta.Num_Referencia = Mov_Instancias.Num_Solicitud
              Consulta.Fec_Ingreso    = Mov_Instancias.Fec_Ingreso
              Consulta.Hor_Ingreso    = STRING(Mov_Instancias.Hora_Ingreso,"HH:MM:SS am")
              Consulta.Vigencia       = w_fecha - Mov_Instancias.Fec_Ingreso
              Consulta.Monto          = saro.Cuantia_Riesgo
              Consulta.Usuario        = Saro.Usuario.
       /* FIND Clientes WHERE Clientes.Nit EQ Mov_Instancias.Nit NO-LOCK NO-ERROR.*/
       FIND Usuarios WHERE Usuarios.Usuario EQ Mov_Instancias.Usuario NO-LOCK NO-ERROR.
       IF AVAILABLE Usuarios THEN
          ASSIGN Consulta.Usuario     = Usuarios.usuario
                 Consulta.Nombre      = Usuarios.Nombre.
           /* Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2.*/
    END.
    ELSE DO:
       MESSAGE "El Movimiento de Instancias apunta a un Evento inexistente. " SKIP
               "Comunique esta anomalia al Administrador"       SKIP
               "Num.Referencia: "  Mov_Instancias.Num_Solicitud SKIP
               "Tercero: " Mov_Instancias.Nit 
               VIEW-AS ALERT-BOX.
    END.
  END.

  CASE R_Organizar:SCREEN-VALUE IN FRAME F_Consulta:
    WHEN "1" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Num_Referencia INDEXED-REPOSITION.
    WHEN "2" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.AgeSaro INDEXED-REPOSITION.
    WHEN "3" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Usuario INDEXED-REPOSITION.
    WHEN "4" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Nombre INDEXED-REPOSITION.
    WHEN "5" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Fec_Ingreso INDEXED-REPOSITION.
    WHEN "6" THEN OPEN QUERY Br_Consulta FOR EACH Consulta NO-LOCK BY Consulta.Vigencia DESCENDING INDEXED-REPOSITION.
  END CASE.
  
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE TmpL wWin 
PROCEDURE TmpL :
DEFINE INPUT PARAMETER Linea AS INTEGER FORMAT "99".
DEFINE INPUT PARAMETER Texto AS CHARACTER FORMAT "X(125)".

CREATE TmpI.
ASSIGN TmpI.ILinea = Linea
       TmpI.ITexto = Texto.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Usuarios_ant_instancia wWin 
PROCEDURE Usuarios_ant_instancia :
/*------------------------------------------------------------------------------
  Purpose: ubica los usuarios que corresponden a la instancia anterior.     
  Parameters:  <none>
  Notes: Para identificar la instancia de analisis se opta por buscar la instancia donde se encuentra habilitada la opcion de scoring y esta diferente a ultima      
  fecha: 07/01/2005
------------------------------------------------------------------------------*/

FOR EACH TUxi: DELETE Tuxi. END.
  
  find FIRST instancia  WHERE instancia.id_scoring AND NOT ultima NO-LOCK NO-ERROR.
  IF AVAIL(instancias) THEN DO:
      FOR EACH Cfg_Instancias WHERE
          Cfg_Instancias.Tipo_Instancia = TP_Instancia AND 
          Cfg_Instancias.Instancia      EQ Instancias.instancia AND
          Cfg_Instancias.Estado         EQ 1 NO-LOCK:
  
        FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario NO-LOCK NO-ERROR.
          IF AVAILABLE Usuarios THEN DO:
               CREATE TUxi.
                ASSIGN  Tuxi.Agencia = Usuarios.Agencia
                        Tuxi.Usuario = Usuarios.Usuario
                        Tuxi.Nombre  = Usuarios.Nombre.
                FOR EACH Mov_Instancias WHERE
                         Mov_instancias.Instancia EQ Cfg_Instancias.Instancia AND
                         Mov_Instancias.Usuario   EQ Usuarios.Usuario AND
                         Mov_Instancias.Estado    EQ NO NO-LOCK:
                         Tuxi.Cantidad = Tuxi.Cantidad + 1.
                END.
                FOR EACH  Mov_Instancias WHERE 
                          Mov_Instancias.Instancia EQ Cfg_Instancias.Instancia AND
                          Mov_Instancias.Num_Solicitud EQ INT(Saro.Num_Referencia:SCREEN-VALUE IN FRAME F_Sarouno) NO-LOCK:
                          FIND Tuxi WHERE TUxi.Usuario EQ Mov_Instancias.Usuario NO-ERROR.
                          IF AVAILABLE Tuxi THEN Tuxi.Proceso = YES.
                END.
                
        END.
      END.
  END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Usuarios_X_Instancia wWin 
PROCEDURE Usuarios_X_Instancia :
FOR EACH TUxi: DELETE Tuxi. END.
  

  SESSION:SET-WAIT-STATE("General").

  FOR EACH Cfg_Instancias WHERE
            Cfg_Instancias.Agencia        EQ saro.Agencia AND
            Cfg_Instancias.Tipo_Instancia = TP_Instancia AND
           (Cfg_Instancias.Instancia      EQ 20 OR Cfg_Instancias.Instancia EQ 30 OR
            Cfg_Instancias.Instancia      EQ 70) AND 
            Cfg_Instancias.Estado         EQ 1  NO-LOCK:

         FIND Usuarios WHERE Usuarios.Usuario EQ Cfg_Instancias.Usuario 
                        AND  Usuarios.estado  EQ 1 NO-LOCK NO-ERROR.
         IF AVAILABLE Usuarios THEN DO:                                                                                         
            CREATE TUxi.                                                                                                        
            ASSIGN Tuxi.Agencia = saro.Agencia                                                                              
                   Tuxi.Usuario = Usuarios.Usuario                                                                              
                   Tuxi.Nombre  = Usuarios.Nombre
                   Tuxi.Instanc = Cfg_Instancias.Instancia.
                                                                                                                                
            FOR EACH Mov_Instancias WHERE                                                                                       
                     Mov_instancias.Instancia EQ Cfg_Instancias.Instancia AND                                                   
                     Mov_Instancias.Usuario   EQ Usuarios.Usuario AND                                                           
                     Mov_Instancias.Estado    EQ NO NO-LOCK:                                                                    
               Tuxi.Cantidad = Tuxi.Cantidad + 1.                                                                               
            END.                                                                                                                
                                                                                                                                
                                                                                                                            
         END.        
  END.     

  SESSION:SET-WAIT-STATE("").
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

