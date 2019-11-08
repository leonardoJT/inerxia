&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12 GUI ADM2
&ANALYZE-RESUME
&Scoped-define WINDOW-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS wWin 
/*------------------------------------------------------------------------

  File: 

  Description: from cntnrwin.w - ADM SmartWindow Template

  Input Parameters:
      <none>

  Output Parameters:
      <none>

  History: New V9 Version - January 15, 1998
          
------------------------------------------------------------------------*/
/*          This .W file was created with the Progress AB.              */
/*----------------------------------------------------------------------*/

/* Create an unnamed pool to store all the widgets created 
     by this procedure. This is a good default which assures
     that this procedure's triggers and internal procedures 
     will execute in this procedure's storage, and that proper
     cleanup will occur on deletion of the procedure. */

CREATE WIDGET-POOL.
/* ***************************  Definitions  ************************** */
/* {incluido\Variable.i "SHARED"}  */
/* {Incluido\VARCON.I "SHARED"} */
/* /* variable.i*/                                                                                           */
DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion.
DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario   INITIAL "339".
DEFINE {1} VAR W_Clave       LIKE Usuarios.Clave     FORMAT "X(16)".
DEFINE {1} VAR W_Prioridad   LIKE Usuarios.Prioridad INITIAL "".
DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia   INITIAL 0.
DEFINE {1} VAR W_Ciudad      LIKE Agencia.Ciudad     INITIAL 0.
DEFINE {1} VAR W_Nom_Agencia   AS CHARACTER FORMAT "X(60)".
DEFINE {1} VAR W_UbiDatos      AS CHAR INITIAL "D".
/*DEFINE {1} VAR W_Ninv        LIKE Inversion.Nro_inversion.*/
DEFINE {1} VAR W_Nom_Entidad   AS CHARACTER FORMAT "X(60)".
DEFINE {1} VAR W_Entidad     LIKE Entidad.Entidad.
DEFINE {1} VAR W_NitGlobal   LIKE Clientes.Nit INITIAL "".
DEFINE {1} VAR W_SMLV        LIKE Indicadores.Valor INITIAL 0.
DEFINE {1} VAR W_Manija        AS HANDLE.
DEFINE {1} VAR W_ManFin        AS HANDLE.
DEFINE {1} VAR W_ManTaq        AS HANDLE.
DEFINE {1} VAR W_Nivel       LIKE Cuentas.Nivel.
DEFINE {1} VAR W_CtaMay      LIKE Cuentas.Cuenta.
DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.
DEFINE {1} VAR W_ficina        AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1.
DEFINE {1} VAR W_Path        LIKE Entidad.Dir_Programas.
DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl          INITIAL "c:\info_juriscoop\".
DEFINE {1} VAR W_Eleccion      AS LOGICAL.
DEFINE {1} VAR W_CedGral     LIKE Clientes.Nit.
DEFINE {1} VAR W_CenCosGral  LIKE Cen_Costos.Cen_Costos.
DEFINE {1} VAR W_Cadena        AS CHARACTER FORMAT "X(9)"  INITIAL "SIFINCOOP".
/*DEFINE     VAR Agencia_Cnt     AS INTEGER FORMAT "999".*/
DEFINE {1} VAR P-Valida        AS LOGICAL.
DEFINE {1} VAR W_VCodPcto    LIKE Ahorros.Cod_Ahorro.
DEFINE {1} VAR W_VCueAhorro  LIKE Ahorros.Cue_Ahorros.
DEFINE {1} VAR W_Solicitud   LIKE Solicitud.Num_Solicitud.
DEFINE {1} VAR W_PagareS     LIKE Creditos.Pagare.
DEFINE {1} VAR P_SdoTot      LIKE Creditos.Sdo_Capital.
DEFINE {1} VAR W_OfiCierre   LIKE Agencias.Agencia.
/***************************/
/****************************
   Variables globales contables
**********************************************************************************/
DEFINE {1} VAR W_ManCon AS HANDLE.
DEFINE {1} VAR W_FecIni AS DATE FORMAT "99/99/9999" INITIAL TODAY.
DEFINE {1} VAR W_FecFin AS DATE FORMAT "99/99/9999" INITIAL TODAY.
DEFINE {1} VAR W_Mes    AS INTEGER FORMAT "99".
DEFINE {1} VAR W_MesFin AS INTEGER FORMAT "99".
DEFINE {1} VAR W_UsuFin LIKE Usuarios.Usuario.
DEFINE {1} VAR W_UsuIni LIKE Usuarios.Usuario.
DEFINE {1} VAR W_CtaIni LIKE Cuentas.Cuenta INITIAL "".
DEFINE {1} VAR W_CtaFin LIKE Cuentas.Cuenta INITIAL "".
DEFINE {1} VAR W_ComTra LIKE Comprobantes.Comprobante INITIAL 0.
DEFINE {1} VAR W_ComIni LIKE Comprobantes.Comprobante INITIAL 0.
DEFINE {1} VAR W_ComFin LIKE Comprobantes.Comprobante INITIAL 0.  
DEFINE {1} VAR W_CenTra LIKE Cen_Costos.Cen_Costos    INITIAL 0.
DEFINE {1} VAR W_OfiIni LIKE Agencias.Agencia         INITIAL 0.
DEFINE {1} VAR W_OfiFin LIKE Agencias.Agencia         INITIAL 0.
DEFINE {1} VAR W_CenIni LIKE Cen_Costos.Cen_Costos    INITIAL 0.
DEFINE {1} VAR W_CenFin LIKE Cen_Costos.Cen_Costos    INITIAL 0.  
DEFINE {1} VAR W_NitIni LIKE Terceros.Nit INITIAL "".
DEFINE {1} VAR W_NitFin LIKE Terceros.Nit INITIAL "".
DEFINE {1} VAR W_BaseInf  AS DECIMAL INITIAL 0 FORMAT "->>>,>>>,>>>,>>9".
DEFINE {1} VAR W_Ope      AS CHARACTER FORMAT "X(13)"  VIEW-AS COMBO-BOX 
      LIST-ITEMS "Sin Seleccion","Mayor Que","Menor Que","Igual" SIZE 13 BY 4.5.
DEFINE {1} VAR TotDctoDeb AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
DEFINE {1} VAR TotDctoCre AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
DEFINE {1} VAR TotGralDeb AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
DEFINE {1} VAR TotGralCre AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
DEFINE {1} VAR VlrDeb     AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
DEFINE {1} VAR VlrCre     AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
DEFINE {1} VAR TotOfCre   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
DEFINE {1} VAR TotOfDeb   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
DEFINE {1} VAR TotCoCre   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
DEFINE {1} VAR TotCoDeb   AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9" .
DEFINE {1} VAR W_SaldoAct AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99" .
DEFINE {1} VAR W_SaldoAnt AS DECIMAL INITIAL 0 FORMAT "-ZZZ,ZZZ,ZZZ,ZZZ,ZZZ,ZZ9.99" .  
DEFINE {1} VAR W_PorReex  AS DECIMAL INITIAL 0 FORMAT "-ZZ9.99".
DEFINE {1} VAR W_Raya1    AS CHAR    INITIAL "" FORMAT "X(130)".
DEFINE {1} VAR W_Raya2    AS CHAR    INITIAL "" FORMAT "X(130)".
DEFINE {1} VAR W_NomMes   AS CHAR    INITIAL "" FORMAT "X(20)".
DEFINE {1} VAR W_NomCta   AS CHAR    INITIAL "".        
DEFINE {1} VAR W_Opcion   AS CHAR    INITIAL "P".
DEFINE {1} VAR W_Destino  AS CHAR    INITIAL "".
DEFINE {1} VAR W_NomOfi LIKE Agencias.Nombre.
DEFINE {1} VAR W_NomCen LIKE Cen_Costos.Nombre INITIAL "Consolidado".
DEFINE {1} VAR W_NomCom LIKE Comprobantes.Nombre INITIAL "Consolidado".
DEFINE {1} VAR W_NomIni LIKE Cuentas.Nombre.    
DEFINE {1} VAR W_NomFin LIKE Cuentas.Nombre.
DEFINE {1} VAR W_NoInNit  AS CHAR FORMAT "X(40)" INITIAL "".
DEFINE {1} VAR W_NoFiNit  AS CHAR FORMAT "X(40)" INITIAL "".
DEFINE {1} VAR W_Rtipo    AS INTEGER INITIAL 1 LABEL  "Tipo Informe " VIEW-AS RADIO-SET HORIZONTAL
                      RADIO-BUTTONS "Detallado/Nit",1,"Resumido/Cuenta",2 SIZE 30 BY 0.81.
DEFINE {1} VAR W_Ret    AS INTEGER INITIAL 1 VIEW-AS RADIO-SET HORIZONTAL
                      RADIO-BUTTONS "Iva",1,"Retencion fuente",2,"Pagos a Terceros", 3 SIZE 40 BY 0.71.
DEFINE {1} VAR W_RForma   AS INTEGER INITIAL 0 LABEL "Formato       " VIEW-AS RADIO-SET HORIZONTAL
                      RADIO-BUTTONS "-/+",0,"(",1,"-/+$",2,"($",3 SIZE 25 BY 0.81.
DEFINE {1} VAR W_Rpta     AS LOGICAL.
DEFINE {1} VAR W_Validacion AS INTEGER INITIAL -1.
DEFINE {1} VAR W_Raya     AS CHAR INITIAL "-" FORMAT "X".
DEFINE {1} VAR W_OfiTra   AS INTEGER FORMAT ">>9" VIEW-AS FILL-IN.
DEFINE {1} VAR W_Id_Paag  AS LOGICAL INITIAL FALSE LABEL "Sin Ajuste" VIEW-AS TOGGLE-BOX SIZE 10 BY 1.
DEFINE {1} VAR W_Id_Defla AS LOGICAL INITIAL FALSE LABEL "Deflactado" VIEW-AS TOGGLE-BOX SIZE 10 BY 1.
DEFINE {1} VAR W_TitRet  AS CHARACTER FORMAT "X(40)".
/********************/

DEFINE VARIABLE W_pag AS INTEGER    NO-UNDO.
DEFINE VARIABLE W_NmesInf   AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE vccodigo    AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE OKpressed   AS LOGICAL                  INITIAL TRUE.
DEFINE VARIABLE viTotReg    AS INTEGER                  INITIAL 0  NO-UNDO.
DEFINE VARIABLE Archivo     AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnomage    AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnomgru    AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcresponsa  AS CHARACTER FORMAT "X(60)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcactivo    AS CHARACTER FORMAT "X(60)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcactivoU   AS CHARACTER FORMAT "X(60)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcconcep    AS CHARACTER FORMAT "X(20)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcestado    AS CHARACTER FORMAT "X(12)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnomres    AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnomcen    AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcNom       AS CHARACTER                           NO-UNDO.
DEFINE VARIABLE vcespacio   AS CHARACTER FORMAT "X(5)"  INITIAL "  " NO-UNDO.
DEFINE VARIABLE vcMes       AS CHARACTER EXTENT 12      INITIAL 
       ["Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
        "Agosto","Septiembre","Octubre","Noviembre","Diciembre"]   NO-UNDO.
DEFINE VARIABLE Listado     AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vdcompras   AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdmejoras   AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdvaldepacu AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdneto      AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdcoshisto  AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vdsdodepre  AS DECIMAL INITIAL 0 NO-UNDO.
DEFINE VARIABLE vclis       AS CHARACTER FORMAT "X(80)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vclis1      AS CHARACTER FORMAT "X(80)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vrsCarga    AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE VARIABLE vcNitAnt    AS CHARACTER FORMAT "X(14)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcNitNue    AS CHARACTER FORMAT "X(14)" INITIAL "" NO-UNDO.

DEFINE TEMP-TABLE TemAct_Fijo
    FIELD Agencia                          LIKE act_fijo.Agencia
    FIELD Codigo                           /*LIKE act_fijo.Codigo*/ AS CHARACTER FORMAT "X(15)"
    FIELD Nombre                           LIKE act_fijo.Nombre           
    FIELD Clase                            LIKE act_fijo.Clase            
    FIELD Grupo                            LIKE act_fijo.Grupo            
    FIELD Descripcion                      LIKE act_fijo.Descripcion      
    FIELD Nit_Proveedor                    LIKE act_fijo.Nit_Proveedor    
    FIELD Fec_Compra                       LIKE act_fijo.Fec_Compra       
    FIELD Nro_Factura                      LIKE act_fijo.Nro_Factura      
    FIELD Ord_Compra                       LIKE act_fijo.Ord_Compra       
    FIELD Val_Comercial                    LIKE act_fijo.Val_Comercial    
    FIELD Val_Compra                       LIKE act_fijo.Val_Compra       
    FIELD Fec_Garantia                     LIKE act_fijo.Fec_Garantia     
    FIELD Val_Garantia                     LIKE act_fijo.Val_Garantia     
    FIELD Nit_Seguro                       LIKE act_fijo.Nit_Seguro       
    FIELD Nro_Seguro                       LIKE act_fijo.Nro_Seguro       
    FIELD Vto_Seguro                       LIKE act_fijo.Vto_Seguro       
    FIELD Nit_Mantenimiento                LIKE act_fijo.Nit_Mantenimiento
    FIELD Pol_Mantenimiento                LIKE act_fijo.Pol_Mantenimiento
    FIELD Fec_VctoMante                    LIKE act_fijo.Fec_VctoMante    
    FIELD Id_Pignorado                     LIKE act_fijo.Id_Pignorado     
    FIELD Nit_Pignorado                    LIKE act_fijo.Nit_Pignorado    
    FIELD Fec_IniPignora                   LIKE act_fijo.Fec_IniPignora   
    FIELD Fec_FinPignora                   LIKE act_fijo.Fec_FinPignora   
    FIELD Nit_Responsable                  LIKE act_fijo.Nit_Responsable  
    FIELD Cen_Costos                       LIKE act_fijo.Cen_Costos       
    FIELD Fec_Asignacion                   LIKE act_fijo.Fec_Asignacion   
    FIELD Val_Reposicion                   LIKE act_fijo.Val_Reposicion   
    FIELD Mejoras                          LIKE act_fijo.Mejoras          
    FIELD Anos_Adepreciar                  LIKE act_fijo.Anos_Adepreciar  
    FIELD Per_Depreciado                   LIKE act_fijo.Per_Depreciado   
    FIELD Fec_Venta                        LIKE act_fijo.Fec_Venta        
    FIELD Fec_debaja                       LIKE act_fijo.Fec_debaja       
    FIELD Fec_Retiro                       LIKE act_fijo.Fec_Retiro       
    FIELD Estado                           LIKE act_fijo.Estado           
    FIELD Cos_Historico                    LIKE act_fijo.Cos_Historico    
    FIELD Val_Salvamento                   LIKE act_fijo.Val_Salvamento   
    FIELD Neto                             LIKE act_fijo.Neto             
    FIELD ValDepMes                        LIKE act_fijo.ValDepMes        
    FIELD ValDepAcum                       LIKE act_fijo.ValDepAcum       
    FIELD Clas_ActDestino                  LIKE act_fijo.Clas_ActDestino  
    FIELD Cod_ActDestino                   LIKE act_fijo.Cod_ActDestino   
    FIELD Clas_OrigRecurso                 LIKE act_fijo.Clas_OrigRecurso 
    FIELD Cod_OrigRecurso                  LIKE act_fijo.Cod_OrigRecurso  
    FIELD Clas_UsoBien                     LIKE act_fijo.Clas_UsoBien     
    FIELD Cod_UsoBien                      LIKE act_fijo.Cod_UsoBien      
    FIELD Val_Avaluo                       LIKE act_fijo.Val_Avaluo       
    FIELD Fec_Avaluo                       LIKE act_fijo.Fec_Avaluo       
    FIELD Nit_Avaluo                       LIKE act_fijo.Nit_Avaluo       
    FIELD Fec_IniDepre                     LIKE act_fijo.Fec_IniDepre     
    FIELD Nit_Arrendatario                 LIKE act_fijo.Nit_Arrendatario 
    FIELD TipNro_contrato                  LIKE act_fijo.TipNro_contrato  
    FIELD Fec_Contrato                     LIKE act_fijo.Fec_Contrato     
    FIELD CtaIngresoArr                    LIKE act_fijo.CtaIngresoArr    
    FIELD CtaOtrosIng                      LIKE act_fijo.CtaOtrosIng      
    FIELD Val_Arriendo                     LIKE act_fijo.Val_Arriendo     
    FIELD Val_Valorizacion                 LIKE act_fijo.Val_Valorizacion 
    FIELD Val_Provision                    LIKE act_fijo.Val_Provision    
    FIELD Sdo_Provision                    LIKE act_fijo.Sdo_Provision    
    FIELD Id_Prestamo                      LIKE act_fijo.Id_Prestamo      
    FIELD Sdo_Depre                        LIKE act_fijo.Sdo_Depre        
    INDEX IAct_Fijo Codigo Grupo Clase.

DEFINE TEMP-TABLE TemResultado
    FIELD Agencia         LIKE act_fijo.agencia
    FIELD Codigo          LIKE act_fijo.codigo
    FIELD Grupo           LIKE act_fijo.grupo
    FIELD CenCosto        LIKE act_fijo.cen_costo
    FIELD Nombre          AS CHARACTER FORMAT "X(40)"
    FIELD Estado          AS INTEGER /*LIKE actfijo.estado*/  /*1-Activo, 2-Retirado, 3-Depreciado*/
    FIELD Anos_Adepreciar AS INTEGER
    FIELD Per_Depreciado  AS INTEGER
    FIELD CostoHis        AS DECIMAL FORMAT "->>>,>>>,>>>,>>>" /*LIKE act_fijo.cos_historico*/
    FIELD Sdodepre        LIKE act_fijo.Sdo_Depre
    FIELD Detalle         AS CHARACTER FORMAT "X(20)"
    INDEX IResul Agencia Codigo.

DEFINE TEMP-TABLE TemTraslado
    FIELD TAgeActu  LIKE act_fijo.agencia
    FIELD TCodigo   LIKE act_fijo.codigo   
    FIELD TGrupo    LIKE act_fijo.grupo    
    FIELD TAgencia  LIKE act_fijo.agencia  
    FIELD TCenCosto LIKE act_fijo.cen_costo
    FIELD TNombre   LIKE act_fijo.nombre
    FIELD TResponsa LIKE act_fijo.nit_responsable
    INDEX ITras TGrupo TCodigo.

DEFINE TEMP-TABLE TemResultado1 /* Salida Traslados*/
    FIELD TAgeActu  LIKE act_fijo.agencia
    FIELD TCodigo   AS CHARACTER FORMAT "X(14)" /*LIKE act_fijo.codigo */
    FIELD TNombre   AS CHARACTER FORMAT "X(40)" /*COLUMN-LABEL "Nombre"*/
    FIELD TGrupo    LIKE act_fijo.grupo    
    FIELD TAgencia  LIKE act_fijo.agencia  
    FIELD TCenCosto LIKE act_fijo.cen_costo
    FIELD TResponsa AS CHARACTER FORMAT "X(14)" /*LIKE act_fijo.nit_responsable*/
    FIELD TNomRes   AS CHARACTER FORMAT "X(60)"
    FIELD TObserva  AS CHARACTER FORMAT "X(30)"
    INDEX IResul1 TAgencia TGrupo TCodigo.

DEFINE TEMP-TABLE TemDardeBaja
    FIELD TAgeActu  LIKE act_fijo.agencia
    FIELD TCodigo   AS CHARACTER FORMAT "X(20)" /* LIKE act_fijo.codigo FORMAT "X(20)"*/
    FIELD TGrupo    LIKE act_fijo.grupo
/*     FIELD TCenCosto LIKE act_fijo.cen_costo */
    FIELD TAclara   AS CHARACTER FORMAT "X(100)"
    INDEX ITras TGrupo TCodigo.

DEFINE TEMP-TABLE TemResultado2  /* Salida Bajas*/                  
    FIELD TAgeActu  LIKE act_fijo.agencia         
    FIELD TCodigo   LIKE act_fijo.codigo          
    FIELD TCenCosto LIKE act_fijo.cen_costo       
    FIELD TEstado   AS INTEGER                  /*1-Activo, 2-Retirado, 3-Depreciado*/
    FIELD TNombre   AS CHARACTER FORMAT "X(40)"   
    FIELD TECodigo  AS CHARACTER FORMAT "X(15)"   
    FIELD TGrupo    LIKE act_fijo.grupo           
    FIELD TAclara   AS CHARACTER FORMAT "X(100)"  
    INDEX IResul2 TGrupo TCodigo.

DEFINE TEMP-TABLE TemCenCostosAF
   FIELD Agencia         LIKE CenCostosAF.Agencia
   FIELD Cen_Costos      LIKE CenCostosAF.Cen_Costos  
   FIELD Nombre          LIKE CenCostosAF.Nombre      
   FIELD Fec_Creacion    LIKE CenCostosAF.Fec_Creacion
   FIELD Fec_Retiro      LIKE CenCostosAF.Fec_Retiro  
   FIELD Estado          LIKE CenCostosAF.Estado      
   FIELD Nit_Respon      LIKE CenCostosAF.Nit_Respon  
   INDEX ICenCostos Agencia Cen_Costos.

DEFINE TEMP-TABLE TemResultadoCosto
    FIELD Agencia        LIKE CenCostosAF.Agencia    
    FIELD Cen_Costos     LIKE CenCostosAF.Cen_Costos 
    FIELD Nombre         LIKE CenCostosAF.Nombre 
    FIELD Fec_Creacion   LIKE CenCostosAF.Fec_Creacion
    FIELD Estado         LIKE CenCostosAF.Estado      
    FIELD Nit_Respon     LIKE CenCostosAF.Nit_Respon  
    FIELD Detalle        AS CHARACTER FORMAT "X(20)"
    INDEX IResul Agencia Cen_Costos.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE SmartWindow
&Scoped-define DB-AWARE no

&Scoped-define ADM-CONTAINER WINDOW

&Scoped-define ADM-SUPPORTED-LINKS Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source

/* Name of designated FRAME-NAME and/or first browse and/or first query */
&Scoped-define FRAME-NAME fMain

/* Standard List Definitions                                            */
&Scoped-Define ENABLED-OBJECTS W_orden B_pagina BtnDone BUTTON-143 ~
Btn_Excel RECT-1 RECT-3 RECT-4 RECT-5 RECT-7 
&Scoped-Define DISPLAYED-OBJECTS W_orden 

/* Custom List Definitions                                              */
/* List-1,List-2,List-3,List-4,List-5,List-6                            */
&Scoped-define List-2 Msaje 

/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* ***********************  Control Definitions  ********************** */

/* Define the widget handle for the window                              */
DEFINE VAR wWin AS WIDGET-HANDLE NO-UNDO.

/* Definitions of handles for SmartObjects                              */
DEFINE VARIABLE h_dact_fijo AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dynbrowser AS HANDLE NO-UNDO.
DEFINE VARIABLE h_dyntoolbar AS HANDLE NO-UNDO.
DEFINE VARIABLE h_folder AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vact_fijo_aseguradora AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vact_fijo_enca-2 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vact_fijo_historico AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vact_fijo_proveedor-3 AS HANDLE NO-UNDO.
DEFINE VARIABLE h_vact_fijo_responsable AS HANDLE NO-UNDO.
DEFINE VARIABLE h_wfact_fijo AS HANDLE NO-UNDO.

/* Definitions of the field level widgets                               */
DEFINE BUTTON BtnDone DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 6 BY 1.5 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON Btn_Excel 
     IMAGE-UP FILE "imagenes/logoexcel.bmp":U
     LABEL "Excel" 
     SIZE 6 BY 1.5
     FONT 8.

DEFINE BUTTON BUTTON-143 
     IMAGE-UP FILE "imagenes/informacion.bmp":U
     LABEL "Button 143" 
     SIZE 6 BY 1.5.

DEFINE BUTTON B_pagina DEFAULT 
     IMAGE-UP FILE "imagenes/135.ico":U
     LABEL "&Consulta" 
     SIZE 6 BY 1.5 TOOLTIP "Consulta".

DEFINE VARIABLE W_orden AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS COMBO-BOX INNER-LINES 2
     LIST-ITEM-PAIRS "Código","codigo",
                     "Nombre","Nombre",
                     "Agencia","Agencia",
                     "Grupo","Grupo"
     DROP-DOWN-LIST
     SIZE 29 BY 1
     BGCOLOR 15  NO-UNDO.

DEFINE RECTANGLE RECT-1
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 128.57 BY 16.69.

DEFINE RECTANGLE RECT-3
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 39 BY 3.15.

DEFINE RECTANGLE RECT-4
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 78.86 BY 3.15.

DEFINE RECTANGLE RECT-5
     EDGE-PIXELS 3 GRAPHIC-EDGE  NO-FILL   
     SIZE 33 BY 1.92.

DEFINE RECTANGLE RECT-7
     EDGE-PIXELS 5 GRAPHIC-EDGE  NO-FILL   
     SIZE 75.14 BY 1.92.

DEFINE BUTTON BTN-Cancelar DEFAULT 
     IMAGE-UP FILE "imagenes/volver.bmp":U
     LABEL "" 
     SIZE 5 BY 1.35 TOOLTIP "Salir"
     BGCOLOR 8 .

DEFINE BUTTON BTN-Procesar 
     IMAGE-UP FILE "imagenes/proceso.bmp":U
     IMAGE-INSENSITIVE FILE "imagenes/proceso_g.bmp":U
     LABEL "&Activar Cupo" 
     SIZE 5 BY 1.35 TOOLTIP "Activar Cupo Rotativo".

DEFINE VARIABLE F-NitAnterior AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nit Anterior" 
     VIEW-AS FILL-IN 
     SIZE 17.57 BY .81 NO-UNDO.

DEFINE VARIABLE F-NitNuevo AS CHARACTER FORMAT "X(256)":U 
     LABEL "Nit Nuevo" 
     VIEW-AS FILL-IN 
     SIZE 17.86 BY .81 NO-UNDO.

DEFINE VARIABLE F-NomAnterior AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE F-NomNuevo AS CHARACTER FORMAT "X(256)":U 
     VIEW-AS FILL-IN 
     SIZE 50 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE Msaje AS CHARACTER FORMAT "X(50)":U 
     VIEW-AS FILL-IN 
     SIZE 57 BY .81
     BGCOLOR 18 FGCOLOR 15  NO-UNDO.

DEFINE VARIABLE RS-CARGA AS INTEGER 
     VIEW-AS RADIO-SET VERTICAL
     RADIO-BUTTONS 
          "Ingresos", 1,
"Traslados", 2,
"Bajas", 3,
"Centro de Costos", 4,
"Cambio de Aseguradora", 5
     SIZE 26 BY 2.96 NO-UNDO.


/* ************************  Frame Definitions  *********************** */

DEFINE FRAME fMain
     W_orden AT ROW 2.08 COL 5 COLON-ALIGNED HELP
          "Seleccione opción de ordenación" NO-LABEL
     B_pagina AT ROW 1.85 COL 106
     BtnDone AT ROW 1.85 COL 112
     BUTTON-143 AT ROW 1.85 COL 100 WIDGET-ID 68
     Btn_Excel AT ROW 1.85 COL 94 WIDGET-ID 56
     "Ordenar Por:" VIEW-AS TEXT
          SIZE 15 BY .62 AT ROW 1.27 COL 13
          FGCOLOR 2 FONT 6
     RECT-1 AT ROW 4.23 COL 1.43
     RECT-3 AT ROW 1 COL 2
     RECT-4 AT ROW 1 COL 42
     RECT-5 AT ROW 1.62 COL 5
     RECT-7 AT ROW 1.62 COL 43.86
    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 1 ROW 1
         SIZE 129.43 BY 20.27
         BGCOLOR 17 FONT 5 WIDGET-ID 100.

DEFINE FRAME FR-Proceso
     RS-CARGA AT ROW 1.54 COL 6 NO-LABEL WIDGET-ID 44
     F-NitAnterior AT ROW 1.81 COL 41.43 COLON-ALIGNED WIDGET-ID 54
     F-NomAnterior AT ROW 1.81 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 56
     F-NitNuevo AT ROW 2.88 COL 41.29 COLON-ALIGNED WIDGET-ID 50
     F-NomNuevo AT ROW 2.88 COL 60 COLON-ALIGNED NO-LABEL WIDGET-ID 52
     BTN-Procesar AT ROW 3.96 COL 88 WIDGET-ID 96
     BTN-Cancelar AT ROW 3.96 COL 94 WIDGET-ID 98
     Msaje AT ROW 5.08 COL 23.43 NO-LABEL WIDGET-ID 38
    WITH 1 DOWN KEEP-TAB-ORDER OVERLAY 
         SIDE-LABELS NO-UNDERLINE THREE-D 
         AT COL 5 ROW 12.31
         SIZE 113 BY 6.19
         BGCOLOR 17 
         TITLE BGCOLOR 17 "Procesos de Cargue Masiva" WIDGET-ID 200.


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: SmartWindow
   Allow: Basic,Browse,DB-Fields,Query,Smart,Window
   Container Links: Data-Target,Data-Source,Page-Target,Update-Source,Update-Target,Filter-target,Filter-Source
   Design Page: 7
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
IF SESSION:DISPLAY-TYPE = "GUI":U THEN
  CREATE WINDOW wWin ASSIGN
         HIDDEN             = YES
         TITLE              = "SFG - Detalle"
         HEIGHT             = 20.27
         WIDTH              = 129.43
         MAX-HEIGHT         = 28.81
         MAX-WIDTH          = 160
         VIRTUAL-HEIGHT     = 28.81
         VIRTUAL-WIDTH      = 160
         MAX-BUTTON         = no
         RESIZE             = no
         SCROLL-BARS        = no
         STATUS-AREA        = yes
         BGCOLOR            = ?
         FGCOLOR            = ?
         THREE-D            = yes
         MESSAGE-AREA       = no
         SENSITIVE          = yes.
ELSE {&WINDOW-NAME} = CURRENT-WINDOW.

&IF '{&WINDOW-SYSTEM}' NE 'TTY' &THEN
IF NOT wWin:LOAD-ICON("imagenes/cubos.ico":U) THEN
    MESSAGE "Unable to load icon: imagenes/cubos.ico"
            VIEW-AS ALERT-BOX WARNING BUTTONS OK.
&ENDIF
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
ASSIGN FRAME FR-Proceso:FRAME = FRAME fMain:HANDLE.

/* SETTINGS FOR FRAME fMain
   FRAME-NAME Custom                                                    */
/* SETTINGS FOR FRAME FR-Proceso
                                                                        */
/* SETTINGS FOR FILL-IN F-NomAnterior IN FRAME FR-Proceso
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN F-NomNuevo IN FRAME FR-Proceso
   NO-ENABLE                                                            */
/* SETTINGS FOR FILL-IN Msaje IN FRAME FR-Proceso
   NO-ENABLE ALIGN-L 2                                                  */
ASSIGN 
       Msaje:HIDDEN IN FRAME FR-Proceso           = TRUE.

IF SESSION:DISPLAY-TYPE = "GUI":U AND VALID-HANDLE(wWin)
THEN wWin:HIDDEN = yes.

/* _RUN-TIME-ATTRIBUTES-END */
&ANALYZE-RESUME

 



/* ************************  Control Triggers  ************************ */

&Scoped-define SELF-NAME wWin
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON END-ERROR OF wWin /* SFG - Detalle */
OR ENDKEY OF {&WINDOW-NAME} ANYWHERE DO:
  /* This case occurs when the user presses the "Esc" key.
     In a persistently run window, just ignore this.  If we did not, the
     application would exit. */
  IF THIS-PROCEDURE:PERSISTENT THEN RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL wWin wWin
ON WINDOW-CLOSE OF wWin /* SFG - Detalle */
DO:
  /* This ADM code must be left here in order for the SmartWindow
     and its descendents to terminate properly on exit. */
  APPLY "CLOSE":U TO THIS-PROCEDURE.
  RETURN NO-APPLY.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME FR-Proceso
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL FR-Proceso wWin
ON GO OF FRAME FR-Proceso /* Procesos de Cargue Masiva */
DO:
  F-NitAnterior:SENSITIVE IN FRAME FR-Proceso = FALSE.
  F-NitNuevo:SENSITIVE    IN FRAME FR-Proceso = FALSE.
  F-NomAnterior:SENSITIVE IN FRAME FR-Proceso = FALSE.
  BTN-Procesar:SENSITIVE  IN FRAME FR-Proceso = FALSE. 
  BTN-Cancelar:SENSITIVE  IN FRAME FR-Proceso = FALSE.

  F-NitAnterior:VISIBLE IN FRAME FR-Proceso = FALSE.
  F-NitNuevo:VISIBLE    IN FRAME FR-Proceso = FALSE.
  F-NomAnterior:VISIBLE IN FRAME FR-Proceso = FALSE.
  BTN-Procesar:VISIBLE  IN FRAME FR-Proceso = FALSE. 
  BTN-Cancelar:VISIBLE  IN FRAME FR-Proceso = FALSE.

  ASSIGN Msaje:VISIBLE    IN FRAME FR-Proceso = FALSE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FR-Proceso
&Scoped-define SELF-NAME BTN-Cancelar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Cancelar wWin
ON CHOOSE OF BTN-Cancelar IN FRAME FR-Proceso
DO:
 ASSIGN F-NitAnterior = ""
        vcNitAnt      = ""
        F-NitNuevo    = ""
        vcNitNue      = ""
        F-NomAnterior = ""
        F-NomNuevo    = "".
 ASSIGN Msaje:VISIBLE = FALSE.
 ASSIGN RS-CARGA:SENSITIVE IN FRAME FR-Proceso = TRUE.
 APPLY "Entry" TO RS-Carga IN FRAME FR-Proceso.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BTN-Procesar
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BTN-Procesar wWin
ON CHOOSE OF BTN-Procesar IN FRAME FR-Proceso /* Activar Cupo */
DO:
IF F-NitAnterior NE "" OR F-NitNuevo NE "" OR F-NomAnterior NE "" OR F-NomNuevo NE "" THEN DO:
/* IF INTEGER(W_Credito:SCREEN-VALUE) NE 0 THEN DO: */
  MESSAGE "Desea Actualizar la Aseguradora....?"
        VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Actualizar Aseguradora Activos Fijos" UPDATE choice AS LOGICAL.
  IF NOT choice THEN 
      RETURN.
  ELSE DO:
      TranConta:
      REPEAT TRANSACTION ON ERROR UNDO TranConta, LEAVE TranConta:
          FOR EACH Act_Fijo WHERE Act_Fijo.Nit_Seguro EQ vcNitAnt
              EXCLUSIVE:
              UPDATE Act_Fijo.Nit_Seguro = vcNitNue.
              ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Actualiza. Agencia : " + STRING(Act_Fijo.Agencia,"xxxx") + " - Codigo : " + Act_Fijo.Codigo.
          END.
          ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Aseguradora Actualizada En Los Activos Fijos Con Exito...".
          LEAVE.
      END.  /*Fin Tx*/
      END.
  END.
ELSE 
   MESSAGE "Revisar de Nuevo Los Nits De Las Aseguradoras"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.
END.
F-NitAnterior:VISIBLE IN FRAME FR-Proceso = FALSE.
F-NitNuevo:VISIBLE    IN FRAME FR-Proceso = FALSE.
F-NomAnterior:VISIBLE IN FRAME FR-Proceso = FALSE.
BTN-Procesar:VISIBLE  IN FRAME FR-Proceso = FALSE. 
BTN-Cancelar:VISIBLE  IN FRAME FR-Proceso = FALSE.
ASSIGN Msaje:VISIBLE  IN FRAME FR-Proceso = FALSE.
ASSIGN RS-CARGA:SENSITIVE IN FRAME FR-Proceso = TRUE.
APPLY "Entry" TO RS-Carga IN FRAME FR-Proceso.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME BtnDone
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BtnDone wWin
ON CHOOSE OF BtnDone IN FRAME fMain
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


&Scoped-define SELF-NAME Btn_Excel
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL Btn_Excel wWin
ON CHOOSE OF Btn_Excel IN FRAME fMain /* Excel */
DO:
    DEFINE VARIABLE viCnt AS INTEGER INITIAL 0 NO-UNDO.


    DYNAMIC-FUNCTION('setRebuildOnRepos':U IN h_dact_fijo,
                     INPUT NO /* LOGICAL */).  

    RUN fetchLast IN h_dact_fijo.
    ASSIGN viCnt = DYNAMIC-FUNCTION('getLastRowNum':U IN h_dact_fijo).

    MESSAGE "Se va a generar archivo Excel con " SKIP
        viCnt " registros." SKIP
        "Esto puede tardar unos segundos." SKIP
        "Desea continuar?"
        VIEW-AS ALERT-BOX INFO BUTTONS YES-NO 
        TITLE "Generar a Excel" UPDATE vlgenerar AS LOGICAL.


    DYNAMIC-FUNCTION('setRebuildOnRepos':U IN h_dact_fijo,
                     INPUT YES /* LOGICAL */).  

    IF vlgenerar THEN 
        RUN transferToExcel IN h_dact_fijo
        ( INPUT " " /* CHARACTER */, /* "Nit Agencia " */
          INPUT YES /* LOGICAL */,
          INPUT YES /* LOGICAL */,
          INPUT viCnt /* INTEGER */).
    ELSE
        RETURN.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME BUTTON-143
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL BUTTON-143 wWin
ON CHOOSE OF BUTTON-143 IN FRAME fMain /* Button 143 */
DO:
  RUN W-InfDia.r NO-ERROR.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME B_pagina
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_pagina wWin
ON CHOOSE OF B_pagina IN FRAME fMain /* Consulta */
DO:
    IF SELF:LABEL = "&Consulta" THEN DO:
        ASSIGN SELF:LABEL = "&Edición"
            SELF:TOOLTIP = "Edición".
        RUN SelectPage(7).
    END.
    ELSE DO:
        ASSIGN SELF:LABEL = "&Consulta"
            SELF:TOOLTIP = "Consulta".
        RUN SelectPage(1).
    END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL B_pagina wWin
ON ENTRY OF B_pagina IN FRAME fMain /* Consulta */
DO:
  ASSIGN W_pag = getCurrentPage().
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME FR-Proceso
&Scoped-define SELF-NAME F-NitAnterior
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-NitAnterior wWin
ON LEAVE OF F-NitAnterior IN FRAME FR-Proceso /* Nit Anterior */
DO:
  ASSIGN F-NitAnterior.
  FIND FIRST Clientes WHERE 
      (Clientes.Nit EQ F-NitAnterior:SCREEN-VALUE) 
       NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN DO:
     F-NomAnterior:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
     ASSIGN vcNitAnt = Clientes.Nit.
  END.
  ELSE DO:
     MESSAGE "Cliente No Existe..."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
/*      W_Credito:SCREEN-VALUE = "".    */
/*      btn_procesar:SENSITIVE = FALSE. */
/*      btn_cancelar:SENSITIVE = FALSE. */
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME F-NitNuevo
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL F-NitNuevo wWin
ON LEAVE OF F-NitNuevo IN FRAME FR-Proceso /* Nit Nuevo */
DO:
  ASSIGN F-NitNuevo.
  FIND FIRST Clientes WHERE 
      (Clientes.Nit EQ F-NitNuevo:SCREEN-VALUE) 
       NO-LOCK NO-ERROR.
  IF AVAILABLE(Clientes) THEN DO:
     F-NomNuevo:SCREEN-VALUE = CAPS(Clientes.Nombre + " " + Clientes.Apellido1 + " " + Clientes.Apellido2).
     ASSIGN vcNitNue = Clientes.Nit.
  END.
  ELSE DO:
     MESSAGE "Cliente No Existe..."
          VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define SELF-NAME RS-CARGA
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL RS-CARGA wWin
ON MOUSE-SELECT-CLICK OF RS-CARGA IN FRAME FR-Proceso
DO:
 CASE (rs-carga:screen-value):
   WHEN "1" THEN DO:
       ASSIGN Msaje:VISIBLE = TRUE.
       RUN Ingresos.
       ASSIGN Msaje:VISIBLE = FALSE.
   END.
   WHEN "2" THEN DO:
       ASSIGN Msaje:VISIBLE = TRUE.
       RUN Traslados.
       ASSIGN Msaje:VISIBLE = FALSE.
   END.
   WHEN "3" THEN DO:
       ASSIGN Msaje:VISIBLE = TRUE.
       RUN Bajas.
       ASSIGN Msaje:VISIBLE = FALSE.
   END.
   WHEN "4" THEN DO:
       ASSIGN Msaje:VISIBLE = TRUE.
       RUN CenCostos.
       ASSIGN Msaje:VISIBLE = FALSE.
   END.
   WHEN "5" THEN DO:
       F-NitAnterior:VISIBLE IN FRAME FR-Proceso = TRUE.
       F-NitNuevo:VISIBLE    IN FRAME FR-Proceso = TRUE.
       F-NomAnterior:VISIBLE IN FRAME FR-Proceso = TRUE.
       BTN-Procesar:VISIBLE  IN FRAME FR-Proceso = TRUE. 
       BTN-Cancelar:VISIBLE  IN FRAME FR-Proceso = TRUE.
       RS-CARGA:SENSITIVE IN FRAME FR-Proceso = FALSE.
       ASSIGN Msaje:VISIBLE = FALSE.
   END.
   OTHERWISE
       RETURN.
  END CASE.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&Scoped-define FRAME-NAME fMain
&Scoped-define SELF-NAME W_orden
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CONTROL W_orden wWin
ON VALUE-CHANGED OF W_orden IN FRAME fMain
DO:
  DYNAMIC-FUNCTION('setQuerySort':U IN h_dact_fijo, INPUT SELF:SCREEN-VALUE).
  DYNAMIC-FUNCTION('OpenQuery':U IN h_dact_fijo).
END.

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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza_Bajas wWin 
PROCEDURE Actualiza_Bajas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TemDardeBaja
    NO-LOCK BREAK BY TAgeActu BY TCodigo:
    ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Agencia: " + STRING(TemDardeBaja.TAgeActu) + 
           "          " + "Código : " + TemDardeBaja.TCodigo.
    ASSIGN Msaje.

    FIND FIRST Act_Fijo WHERE 
         Act_Fijo.Agencia   EQ TemDardeBaja.TAgeActu  AND
         Act_Fijo.Grupo     EQ TemDardeBaja.TGrupo    AND
         Act_Fijo.Codigo    EQ TemDardeBaja.TCodigo   NO-ERROR.
    IF AVAILABLE(Act_Fijo) THEN DO:
       UPDATE Act_Fijo.Descripcion = TemDardeBaja.TAclara
              Act_Fijo.Fec_debaja  = TODAY
              Act_Fijo.Estado      = 2. /* Retirado*/

       CREATE TemResultado2.
       UPDATE TemResultado2.TAgeActu  = Act_Fijo.Agencia
              TemResultado2.TCodigo   = Act_Fijo.Codigo
              TemResultado2.TCenCosto = Act_Fijo.Cen_Costos
              TemResultado2.TEstado   = Act_Fijo.Estado
              TemResultado2.TNombre   = Act_Fijo.Nombre
              TemResultado2.TECodigo  = "Actualizado" 
              TemResultado2.TGrupo    = Act_Fijo.Grupo
              TemResultado2.TAclara   = Act_Fijo.Descripcion.
    END.
    ELSE DO:
        CREATE TemResultado2.
        UPDATE TemResultado2.TAgeActu  = TemDardeBaja.TAgeActu  /*TemDardeBaja.TAgencia*/
               TemResultado2.TCodigo   = TemDardeBaja.TCodigo
               TemResultado2.TCenCosto = 0
               TemResultado2.TEstado   = 0
               TemResultado2.TNombre   = "No Existe En Activos"
               TemResultado2.TECodigo  = "No Existe..." 
               TemResultado2.TGrupo    = TemDardeBaja.TGrupo
               TemResultado2.TAclara   = TemDardeBaja.TAclara.   
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza_CenCostos wWin 
PROCEDURE Actualiza_CenCostos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TemCenCostosAF NO-LOCK:
    FIND FIRST CenCostosAF WHERE
         CenCostosAF.Agencia    EQ TemCenCostosAF.Agencia  AND 
         CenCostosAF.Cen_Costos EQ TemCenCostosAF.Cen_Costos
         NO-LOCK NO-ERROR.
    IF AVAILABLE(CenCostosAF) THEN DO:
       UPDATE CenCostosAF.Nombre       =   TemCenCostosAF.Nombre       
              CenCostosAF.Fec_Creacion =   TemCenCostosAF.Fec_Creacion 
              CenCostosAF.Estado       =   TemCenCostosAF.Estado       
              CenCostosAF.Nit_Respon   =   TemCenCostosAF.Nit_Respon. 

       CREATE TemResultadoCosto.
       UPDATE TemResultadoCosto.Agencia      = CenCostosAF.Agencia     
              TemResultadoCosto.Cen_Costos   = CenCostosAF.Cen_Costos  
              TemResultadoCosto.Nombre       = CenCostosAF.Nombre      
              TemResultadoCosto.Fec_Creacion = CenCostosAF.Fec_Creacion
              TemResultadoCosto.Estado       = CenCostosAF.Estado      
              TemResultadoCosto.Nit_Respon   = CenCostosAF.Nit_Respon  
              TemResultadoCosto.Detalle      = "Ya Existe".
    END.
    ELSE DO:
        CREATE CenCostosAF.
        UPDATE CenCostosAF.Agencia      =   TemCenCostosAF.Agencia     
               CenCostosAF.Cen_Costos   =   TemCenCostosAF.Cen_Costos  
               CenCostosAF.Nombre       =   TemCenCostosAF.Nombre      
               CenCostosAF.Fec_Creacion =   TemCenCostosAF.Fec_Creacion
               CenCostosAF.Fec_Retiro   =   TemCenCostosAF.Fec_Retiro  
               CenCostosAF.Estado       =   TemCenCostosAF.Estado      
               CenCostosAF.Nit_Respon   =   TemCenCostosAF.Nit_Respon. 

        CREATE TemResultadoCosto.
        UPDATE TemResultadoCosto.Agencia      = CenCostosAF.Agencia     
               TemResultadoCosto.Cen_Costos   = CenCostosAF.Cen_Costos  
               TemResultadoCosto.Nombre       = CenCostosAF.Nombre      
               TemResultadoCosto.Fec_Creacion = CenCostosAF.Fec_Creacion
               TemResultadoCosto.Estado       = CenCostosAF.Estado      
               TemResultadoCosto.Nit_Respon   = CenCostosAF.Nit_Respon  
               TemResultadoCosto.Detalle      = "Ingresado..".
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza_Ingresos wWin 
PROCEDURE Actualiza_Ingresos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
ASSIGN viTotReg = 0.
FOR EACH TemAct_Fijo NO-LOCK:
    ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Agencia: " + STRING(TemAct_Fijo.Agencia) + 
           "          " + "Código : " + TemAct_Fijo.Codigo.
    ASSIGN Msaje.

    FIND FIRST Act_Fijo WHERE
         Act_Fijo.Agencia   EQ TemAct_Fijo.Agencia  AND 
         Act_Fijo.Codigo    EQ TemAct_Fijo.Codigo   AND
         Act_Fijo.Grupo     EQ TemAct_Fijo.Grupo NO-LOCK NO-ERROR.
    IF AVAILABLE(Act_Fijo) THEN DO:
       CREATE TemResultado.
       UPDATE TemResultado.Agencia         = Act_Fijo.Agencia
              TemResultado.Codigo          = Act_Fijo.Codigo
              TemResultado.Grupo           = Act_Fijo.Grupo
              TemResultado.CenCosto        = Act_Fijo.Cen_Costo    
              TemResultado.Nombre          = Act_Fijo.Nombre       
              TemResultado.Estado          = Act_Fijo.Estado                
              TemResultado.Anos_Adepreciar = Act_Fijo.Anos_Adepreciar
              TemResultado.Per_Depreciado  = Act_Fijo.Per_Depreciado 
              TemResultado.CostoHis        = Act_Fijo.Cos_Historico
              TemResultado.Sdodepre        = Act_Fijo.Sdo_depre    
              TemResultado.Detalle         = "Ya Existe".
    END.
    ELSE DO:
        CREATE Act_Fijo.
        UPDATE act_fijo.Agencia           =   TemAct_Fijo.Agencia
               act_fijo.Codigo            =   TemAct_Fijo.Codigo
               act_fijo.Nombre            =   TemAct_Fijo.Nombre
               act_fijo.Clase             =   TemAct_Fijo.Clase
               act_fijo.Grupo             =   TemAct_Fijo.Grupo
               act_fijo.Descripcion       =   TemAct_Fijo.Descripcion
               act_fijo.Nit_Proveedor     =   TemAct_Fijo.Nit_Proveedor
               act_fijo.Fec_Compra        =   TemAct_Fijo.Fec_Compra
               act_fijo.Nro_Factura       =   TemAct_Fijo.Nro_Factura
               act_fijo.Ord_Compra        =   TemAct_Fijo.Ord_Compra
               act_fijo.Val_Comercial     =   TemAct_Fijo.Val_Comercial
               act_fijo.Val_Compra        =   TemAct_Fijo.Val_Compra
               act_fijo.Fec_Garantia      =   TemAct_Fijo.Fec_Garantia
               act_fijo.Val_Garantia      =   TemAct_Fijo.Val_Garantia
               act_fijo.Nit_Seguro        =   TemAct_Fijo.Nit_Seguro
               act_fijo.Nro_Seguro        =   TemAct_Fijo.Nro_Seguro
               act_fijo.Vto_Seguro        =   TemAct_Fijo.Vto_Seguro
               act_fijo.Nit_Mantenimiento =   TemAct_Fijo.Nit_Mantenimiento
               act_fijo.Pol_Mantenimiento =   TemAct_Fijo.Pol_Mantenimiento
               act_fijo.Fec_VctoMante     =   TemAct_Fijo.Fec_VctoMante
               act_fijo.Id_Pignorado      =   TemAct_Fijo.Id_Pignorado
               act_fijo.Nit_Pignorado     =   TemAct_Fijo.Nit_Pignorado
               act_fijo.Fec_IniPignora    =   TemAct_Fijo.Fec_IniPignora
               act_fijo.Fec_FinPignora    =   TemAct_Fijo.Fec_FinPignora
               act_fijo.Nit_Responsable   =   TemAct_Fijo.Nit_Responsable
               act_fijo.Cen_Costos        =   TemAct_Fijo.Cen_Costos
               act_fijo.Fec_Asignacion    =   TemAct_Fijo.Fec_Asignacion
               act_fijo.Val_Reposicion    =   TemAct_Fijo.Val_Reposicion
               act_fijo.Mejoras           =   TemAct_Fijo.Mejoras
               act_fijo.Anos_Adepreciar   =   TemAct_Fijo.Anos_Adepreciar
               act_fijo.Per_Depreciado    =   TemAct_Fijo.Per_Depreciado
               act_fijo.Fec_Venta         =   TemAct_Fijo.Fec_Venta
               act_fijo.Fec_debaja        =   TemAct_Fijo.Fec_debaja
               act_fijo.Fec_Retiro        =   TemAct_Fijo.Fec_Retiro
               act_fijo.Estado            =   TemAct_Fijo.Estado
               act_fijo.Cos_Historico     =   TemAct_Fijo.Cos_Historico
               act_fijo.Val_Salvamento    =   TemAct_Fijo.Val_Salvamento
               act_fijo.Neto              =   TemAct_Fijo.Neto
               act_fijo.ValDepMes         =   TemAct_Fijo.ValDepMes
               act_fijo.ValDepAcum        =   TemAct_Fijo.ValDepAcum
               act_fijo.Clas_ActDestino   =   TemAct_Fijo.Clas_ActDestino
               act_fijo.Cod_ActDestino    =   TemAct_Fijo.Cod_ActDestino
               act_fijo.Clas_OrigRecurso  =   TemAct_Fijo.Clas_OrigRecurso
               act_fijo.Cod_OrigRecurso   =   TemAct_Fijo.Cod_OrigRecurso
               act_fijo.Clas_UsoBien      =   TemAct_Fijo.Clas_UsoBien
               act_fijo.Cod_UsoBien       =   TemAct_Fijo.Cod_UsoBien
               act_fijo.Val_Avaluo        =   TemAct_Fijo.Val_Avaluo
               act_fijo.Fec_Avaluo        =   TemAct_Fijo.Fec_Avaluo
               act_fijo.Nit_Avaluo        =   TemAct_Fijo.Nit_Avaluo
               act_fijo.Fec_IniDepre      =   TemAct_Fijo.Fec_IniDepre
               act_fijo.Nit_Arrendatario  =   TemAct_Fijo.Nit_Arrendatario
               act_fijo.TipNro_contrato   =   TemAct_Fijo.TipNro_contrato
               act_fijo.Fec_Contrato      =   TemAct_Fijo.Fec_Contrato
               act_fijo.CtaIngresoArr     =   TemAct_Fijo.CtaIngresoArr
               act_fijo.CtaOtrosIng       =   TemAct_Fijo.CtaOtrosIng
               act_fijo.Val_Arriendo      =   TemAct_Fijo.Val_Arriendo
               act_fijo.Val_Valorizacion  =   TemAct_Fijo.Val_Valorizacion
               act_fijo.Val_Provision     =   TemAct_Fijo.Val_Provision
               act_fijo.Sdo_Provision     =   TemAct_Fijo.Sdo_Provision
               act_fijo.Id_Prestamo       =   TemAct_Fijo.Id_Prestamo
               act_fijo.Sdo_Depre         =   TemAct_Fijo.Sdo_Depre.

        CREATE TemResultado.
        UPDATE TemResultado.Agencia         = Act_Fijo.Agencia
               TemResultado.Codigo          = Act_Fijo.Codigo
               TemResultado.Grupo           = Act_Fijo.Grupo
               TemResultado.CenCosto        = Act_Fijo.Cen_Costo
               TemResultado.Nombre          = Act_Fijo.Nombre
               TemResultado.Estado          = Act_Fijo.Estado
               TemResultado.Anos_Adepreciar = Act_Fijo.Anos_Adepreciar
               TemResultado.Per_Depreciado  = Act_Fijo.Per_Depreciado
               TemResultado.CostoHis        = Act_Fijo.Cos_Historico
               TemResultado.Sdodepre        = Act_Fijo.Sdo_depre
               TemResultado.Detalle         = "Ingresado..".
    END.
END.
ASSIGN Msaje = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Actualiza_Traslados wWin 
PROCEDURE Actualiza_Traslados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

FOR EACH TemTraslado 
    NO-LOCK BREAK BY TemTraslado.TGrupo BY TemTraslado.TCodigo:
    ASSIGN Msaje:SCREEN-VALUE IN FRAME FR-Proceso = "Agencia: " + STRING(TemTraslado.TAgencia) + 
           "          " + "Código : " + TemTraslado.TCodigo.
    ASSIGN Msaje.

    FIND FIRST Act_Fijo WHERE 
         Act_Fijo.Agencia  EQ TemTraslado.TAgeActu  AND
         Act_Fijo.Codigo   EQ TemTraslado.TCodigo 
    NO-ERROR.
    IF AVAILABLE(Act_Fijo) THEN DO:
       UPDATE Act_Fijo.Agencia    = TemTraslado.TAgencia
              Act_Fijo.Cen_Costos = TemTraslado.TCenCosto.
       IF TRIM(TemTraslado.Tnombre) NE ""  THEN
          UPDATE Act_Fijo.Nombre          = TemTraslado.TNombre.
       IF TRIM(TemTraslado.TResponsa) NE "" THEN
          UPDATE Act_Fijo.Nit_Responsable = TemTraslado.TResponsa.

       CREATE TemResultado1.
       UPDATE TemResultado1.TAgeActu  = Act_Fijo.Agencia
              TemResultado1.TCodigo   = Act_Fijo.Codigo
              TemResultado1.TNombre   = Act_Fijo.Nombre
              TemResultado1.TGrupo    = Act_Fijo.Grupo
              TemResultado1.TAgencia  = Act_Fijo.Agencia
              TemResultado1.TCenCosto = Act_Fijo.Cen_Costos
              TemResultado1.TResponsa = Act_Fijo.Nit_Responsable
              TemResultado1.TObserva  = "Actualizado".
    END.
    ELSE DO:
      CREATE TemResultado1.
      UPDATE TemResultado1.TAgeActu  = TemTraslado.TAgeActu
             TemResultado1.TCodigo   = TemTraslado.TCodigo
             TemResultado1.TNombre   = "No Existe"
             TemResultado1.TGrupo    = TemTraslado.TGrupo
             TemResultado1.TAgencia  = TemTraslado.TAgencia
             TemResultado1.TCenCosto = TemTraslado.TCenCosto
             TemResultado1.TResponsa = TemTraslado.TResponsa
             TemResultado1.TObserva  = "No Existe Código y/o Grupo".
    END.
END.
ASSIGN Msaje = "".
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE adm-create-objects wWin  _ADM-CREATE-OBJECTS
PROCEDURE adm-create-objects :
/*------------------------------------------------------------------------------
  Purpose:     Create handles for all SmartObjects used in this procedure.
               After SmartObjects are initialized, then SmartLinks are added.
  Parameters:  <none>
------------------------------------------------------------------------------*/
  DEFINE VARIABLE currentPage  AS INTEGER NO-UNDO.

  ASSIGN currentPage = getCurrentPage().

  CASE currentPage: 

    WHEN 0 THEN DO:
       RUN constructObject (
             INPUT  'dact_fijo.wDB-AWARE':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'AppServiceASInfoASUsePrompt?CacheDuration0CheckCurrentChangedyesDestroyStatelessyesDisconnectAppServernoServerOperatingModeNONEShareDatanoUpdateFromSourcenoForeignFieldsObjectNamedact_fijoOpenOnInityesPromptColumns(NONE)PromptOnDeleteyesRowsToBatch200RebuildOnReposnoToggleDataTargetsyes':U ,
             OUTPUT h_dact_fijo ).
       RUN repositionObject IN h_dact_fijo ( 1.27 , 122.00 ) NO-ERROR.
       /* Size in AB:  ( 1.50 , 7.72 ) */

       RUN constructObject (
             INPUT  'wfact_fijo.w':U ,
             INPUT  {&WINDOW-NAME} ,
             INPUT  'LogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_wfact_fijo ).
       /* Position in AB:  ( 2.85 , 121.29 ) */
       /* Size in AB:  ( 3.65 , 8.43 ) */

       RUN constructObject (
             INPUT  'vact_fijo_enca.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vact_fijo_enca-2 ).
       RUN repositionObject IN h_vact_fijo_enca-2 ( 4.50 , 4.00 ) NO-ERROR.
       /* Size in AB:  ( 6.46 , 124.00 ) */

       RUN constructObject (
             INPUT  'adm2/dyntoolbar.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EdgePixels2DeactivateTargetOnHidenoDisabledActionsFlatButtonsyesMenunoShowBorderyesToolbaryesActionGroupsTableio,Navigation,FunctionTableIOTypeUpdateSupportedLinksNavigation-source,Tableio-sourceToolbarBandsToolbarAutoSizenoToolbarDrawDirectionHorizontalLogicalObjectNameDisabledActionsHiddenActionsResetHiddenToolbarBandsHiddenMenuBandsMenuMergeOrder0RemoveMenuOnHidenoCreateSubMenuOnConflictyesNavigationTargetNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dyntoolbar ).
       RUN repositionObject IN h_dyntoolbar ( 2.08 , 44.86 ) NO-ERROR.
       RUN resizeObject IN h_dyntoolbar ( 1.00 , 44.86 ) NO-ERROR.

       RUN constructObject (
             INPUT  'adm2/folder.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'FolderLabels':U + 'Proveedor|Aseguradora|Responsable|Historico|Procesos|Informes' + 'FolderTabWidth0FolderFont-1HideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_folder ).
       RUN repositionObject IN h_folder ( 11.15 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_folder ( 9.23 , 124.00 ) NO-ERROR.

       /* Links to SmartDataObject h_dact_fijo. */
       RUN addLink ( h_dyntoolbar , 'Navigation':U , h_dact_fijo ).
       RUN addLink ( h_wfact_fijo , 'Filter':U , h_dact_fijo ).

       /* Links to SmartWindow h_wfact_fijo. */
       RUN addLink ( h_dact_fijo , 'Data':U , h_wfact_fijo ).

       /* Links to SmartDataViewer h_vact_fijo_enca-2. */
       RUN addLink ( h_dact_fijo , 'Data':U , h_vact_fijo_enca-2 ).
       RUN addLink ( h_vact_fijo_enca-2 , 'Update':U , h_dact_fijo ).
       RUN addLink ( h_dyntoolbar , 'TableIO':U , h_vact_fijo_enca-2 ).

       /* Links to SmartFolder h_folder. */
       RUN addLink ( h_folder , 'Page':U , THIS-PROCEDURE ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dyntoolbar ,
             W_orden:HANDLE IN FRAME fMain , 'AFTER':U ).
       RUN adjustTabOrder ( h_folder ,
             h_dyntoolbar , 'AFTER':U ).
       RUN adjustTabOrder ( h_vact_fijo_enca-2 ,
             h_folder , 'AFTER':U ).
    END. /* Page 0 */
    WHEN 1 THEN DO:
       RUN constructObject (
             INPUT  'vact_fijo_proveedor.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vact_fijo_proveedor-3 ).
       RUN repositionObject IN h_vact_fijo_proveedor-3 ( 12.58 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 8.00 , 64.43 ) */

       /* Links to SmartDataViewer h_vact_fijo_proveedor-3. */
       RUN addLink ( h_wfact_fijo , 'Data':U , h_vact_fijo_proveedor-3 ).
       RUN addLink ( h_vact_fijo_proveedor-3 , 'Update':U , h_wfact_fijo ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vact_fijo_proveedor-3 ,
             Btn_Excel:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 1 */
    WHEN 2 THEN DO:
       RUN constructObject (
             INPUT  'vact_fijo_aseguradora.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vact_fijo_aseguradora ).
       RUN repositionObject IN h_vact_fijo_aseguradora ( 12.31 , 8.00 ) NO-ERROR.
       /* Size in AB:  ( 8.00 , 64.43 ) */

       /* Links to SmartDataViewer h_vact_fijo_aseguradora. */
       RUN addLink ( h_wfact_fijo , 'Data':U , h_vact_fijo_aseguradora ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vact_fijo_aseguradora ,
             Btn_Excel:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 2 */
    WHEN 3 THEN DO:
       RUN constructObject (
             INPUT  'vact_fijo_responsable.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vact_fijo_responsable ).
       RUN repositionObject IN h_vact_fijo_responsable ( 11.27 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 10.00 , 64.43 ) */

       /* Links to SmartDataViewer h_vact_fijo_responsable. */
       RUN addLink ( h_wfact_fijo , 'Data':U , h_vact_fijo_responsable ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vact_fijo_responsable ,
             Btn_Excel:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 3 */
    WHEN 4 THEN DO:
       RUN constructObject (
             INPUT  'vact_fijo_historico.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'EnabledObjFldsToDisable?ModifyFields(All)DataSourceNamesUpdateTargetNamesLogicalObjectNameLogicalObjectNamePhysicalObjectNameDynamicObjectnoRunAttributeHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_vact_fijo_historico ).
       RUN repositionObject IN h_vact_fijo_historico ( 12.31 , 6.00 ) NO-ERROR.
       /* Size in AB:  ( 7.81 , 104.00 ) */

       /* Links to SmartDataViewer h_vact_fijo_historico. */
       RUN addLink ( h_dact_fijo , 'Data':U , h_vact_fijo_historico ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_vact_fijo_historico ,
             Btn_Excel:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 4 */
    WHEN 7 THEN DO:
       RUN constructObject (
             INPUT  'adm2/dynbrowser.w':U ,
             INPUT  FRAME fMain:HANDLE ,
             INPUT  'DisplayedFieldsAgencia,Codigo,Cen_Costos,Estado,Nombre,Descripcion,Nro_Factura,Nro_Seguro,Nit_Proveedor,Nit_Responsable,Fec_AsignacionEnabledFieldsBrowseColumnTypesBrowseColumnItemsBrowseColumnItemPairsBrowseColumnInnerLinesBrowseColumnSortsBrowseColumnMaxCharsBrowseColumnAutoCompletionsBrowseColumnUniqueMatchesBrowseColumnDelimitersScrollRemotenoNumDown0CalcWidthnoMaxWidth80FetchOnReposToEndyesUseSortIndicatoryesSearchFieldDataSourceNamesdact_fijoUpdateTargetNames?LogicalObjectNameHideOnInitnoDisableOnInitnoObjectLayout':U ,
             OUTPUT h_dynbrowser ).
       RUN repositionObject IN h_dynbrowser ( 12.31 , 4.00 ) NO-ERROR.
       RUN resizeObject IN h_dynbrowser ( 5.92 , 125.00 ) NO-ERROR.

       /* Links to SmartDataBrowser h_dynbrowser. */
       RUN addLink ( h_dact_fijo , 'Data':U , h_dynbrowser ).

       /* Adjust the tab order of the smart objects. */
       RUN adjustTabOrder ( h_dynbrowser ,
             Btn_Excel:HANDLE IN FRAME fMain , 'AFTER':U ).
    END. /* Page 7 */

  END CASE.
  /* Select a Startup page. */
  IF currentPage eq 0
  THEN RUN selectPage IN THIS-PROCEDURE ( 1 ).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Bajas wWin 
PROCEDURE Bajas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE TemDardeBaja.
EMPTY TEMP-TABLE TemResultado2.

RUN Proceso_Baja.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cargue_Bajas wWin 
PROCEDURE Cargue_Bajas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
SYSTEM-DIALOG GET-FILE Archivo
    TITLE      ""
    FILTERS    "*.csv"   "*.csv"
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
  
IF OKpressed = TRUE THEN DO:
    INPUT FROM VALUE(Archivo).
    REPEAT:
         CREATE TemDardeBaja.
         IMPORT DELIMITER ";" TemDardeBaja.
         IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
            RETURN ERROR.
         END.
         IF TemDardeBaja.TAgeActu EQ 0 OR TemDardeBaja.TCodigo   EQ "" OR
            TemDardeBaja.TGrupo   EQ 0 THEN
            DELETE TemDardeBaja.
    END.
    INPUT CLOSE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cargue_CenCostos wWin 
PROCEDURE Cargue_CenCostos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
SYSTEM-DIALOG GET-FILE Archivo
    TITLE      ""
    FILTERS    "*.csv"   "*.csv"
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
  
IF OKpressed = TRUE THEN DO:
    INPUT FROM VALUE(Archivo).
    REPEAT:
         CREATE TemCenCostosAF.
         IMPORT DELIMITER ";" TemCenCostosAF.
         IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
            RETURN ERROR.
         END.
         IF TemCenCostosAF.Agencia EQ 0  OR TemCenCostosAF.Cen_Costos   EQ 0 OR 
            TemCenCostosAF.Nombre  EQ "" OR TemCenCostosAF.Fec_Creacion EQ ? OR
            TemCenCostosAF.Estado  NE 1  OR TemCenCostosAF.Nit_Respon   EQ "" 
            THEN DO:
              DELETE TemCenCostosAF.
              NEXT.
         END.
    END.
    INPUT CLOSE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cargue_Ingresos wWin 
PROCEDURE Cargue_Ingresos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
SYSTEM-DIALOG GET-FILE Archivo
    TITLE      ""
    FILTERS    "*.csv"   "*.csv"
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
  
IF OKpressed = TRUE THEN DO:
    INPUT FROM VALUE(Archivo).
    REPEAT:
         CREATE TemAct_Fijo.
         IMPORT DELIMITER ";" TemAct_Fijo.
         IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
            RETURN ERROR.
         END.
         IF TemAct_Fijo.Agencia EQ 0 OR TemAct_Fijo.Codigo EQ "" OR TemAct_Fijo.Nombre EQ "" OR
            TemAct_Fijo.Clase   NE "" OR TemAct_Fijo.Grupo  EQ 0  OR  TemAct_Fijo.Fec_IniDepre EQ ? OR
            TemAct_Fijo.Fec_Compra EQ ? OR TemAct_Fijo.val_compra LT 0 OR TemAct_Fijo.Nit_Seguro EQ "" OR
            TemAct_Fijo.Nro_Seguro EQ "" OR TemAct_Fijo.Vto_Seguro EQ ? OR 
            TemAct_Fijo.Nit_Responsable EQ "" OR TemAct_Fijo.Cen_Costos EQ 0 OR TemAct_Fijo.Anos_Adepreciar EQ 0 OR
            TemAct_Fijo.Per_Depreciado LT 0 OR TemAct_Fijo.Anos_Adepreciar LT TemAct_Fijo.Per_Depreciado OR
            TemAct_Fijo.Estado LT 0
            THEN DO:
                 DELETE TemAct_Fijo.
                 NEXT.
            END.
    END.
    INPUT CLOSE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Cargue_Traslados wWin 
PROCEDURE Cargue_Traslados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
SYSTEM-DIALOG GET-FILE Archivo
    TITLE      ""
    FILTERS    "*.csv"   "*.csv"
    MUST-EXIST
    USE-FILENAME
    UPDATE OKpressed.
  
IF OKpressed = TRUE THEN DO:
    INPUT FROM VALUE(Archivo).
    REPEAT:
         CREATE TemTraslado.
         IMPORT DELIMITER ";" TemTraslado.
         IF ERROR-STATUS:ERROR THEN DO:
            MESSAGE "El Archivo tiene Problemas...Revise por favor" VIEW-AS ALERT-BOX.
            RETURN ERROR.
         END.
         IF TemTraslado.TCodigo EQ "" OR TemTraslado.TGrupo  EQ 0 THEN DO:
                DELETE TemTraslado.
                NEXT.
         END.
    END.
    INPUT CLOSE.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE CenCostos wWin 
PROCEDURE CenCostos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE TemCenCostosAF.
EMPTY TEMP-TABLE TemResultadoCosto.
RUN Proceso_CenCostos.

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
  DISPLAY W_orden 
      WITH FRAME fMain IN WINDOW wWin.
  ENABLE W_orden B_pagina BtnDone BUTTON-143 Btn_Excel RECT-1 RECT-3 RECT-4 
         RECT-5 RECT-7 
      WITH FRAME fMain IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-fMain}
  DISPLAY RS-CARGA F-NitAnterior F-NomAnterior F-NitNuevo F-NomNuevo Msaje 
      WITH FRAME FR-Proceso IN WINDOW wWin.
  ENABLE RS-CARGA F-NitAnterior F-NitNuevo BTN-Procesar BTN-Cancelar 
      WITH FRAME FR-Proceso IN WINDOW wWin.
  {&OPEN-BROWSERS-IN-QUERY-FR-Proceso}
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

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpBajasAct_Fijo wWin 
PROCEDURE ImpBajasAct_Fijo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT TO VALUE(listado).
FOR EACH TemResultado2 BY TemResultado2.TAgeActu BY TemResultado2.TCodigo:
    ASSIGN viTotReg  = viTotReg + 1.
    ASSIGN vcactivo  = TRIM(TemResultado2.TCodigo)   + " - " + TRIM(TemResultado2.TNombre).

    CASE TemResultado2.TEstado:
    WHEN 1 THEN
        ASSIGN vcestado = "1-Activo".
    WHEN 2 THEN
        ASSIGN vcestado = "2-Retirado".
    WHEN 3 THEN
        ASSIGN vcestado = "3-Depreciado".
    OTHERWISE
        ASSIGN vcestado = "".
    END.

    FORM 
        TemResultado2.TAgeActu  COLUMN-LABEL "Agencia"           FORMAT "ZZZ"           
        TemResultado2.TGrupo    COLUMN-LABEL "Grupo"             FORMAT "99999"    
        vcactivo                COLUMN-LABEL "Activo De Baja"    FORMAT "X(50)"             
        TemResultado2.TCenCosto COLUMN-LABEL "C.Costo"           FORMAT "ZZZZZ" 
        SPACE(4)
        vcestado                COLUMN-LABEL "Estado"            FORMAT "X(14)"
        TemResultado2.TAclara   COLUMN-LABEL "Aclaracion"        FORMAT "X(80)" 
        WITH FRAME FTemConta DOWN COLUMN 1 WIDTH 280
             NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    DISPLAY 
        TemResultado2.TAgeActu 
        TemResultado2.TGrupo   
        vcactivo
        TemResultado2.TCenCosto
        vcestado
        TemResultado2.TAclara  
        WITH FRAME FTemConta.
    DOWN WITH FRAME FTemConta.
END.
DISPLAY
   "Total de Activos : " viTotReg
  WITH FRAME FRep3 NO-LABEL.
DOWN WITH FRAME FRep3.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpBajasAct_Fijo1 wWin 
PROCEDURE ImpBajasAct_Fijo1 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT TO VALUE(listado).
{Incluido\RepEncabezado.I}
ASSIGN W_Reporte = "Reporte   : Dar de Bajas Activos Fijos.    Fecha : " + STRING(W_Fecha,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS")
       W_EncColumna = "Concepto                    Activo de Baja                      C.Costo    Estado       Aclaracion ".
VIEW FRAME F-Encabezado.
/* VIEW FRAME f-ftr. */
ASSIGN viTotReg = 0.

FOR EACH TemResultado2 BREAK BY TemResultado2.TAgeActu
    BY TemResultado2.TGrupo:

    ASSIGN viTotReg  = viTotReg + 1.
    ASSIGN vcactivo  = TRIM(TemResultado2.TCodigo)   + " - " + TRIM(TemResultado2.TNombre).

    CASE TemResultado2.TEstado:
    WHEN 1 THEN
        ASSIGN vcestado = "1-Activo".
    WHEN 2 THEN
        ASSIGN vcestado = "2-Retirado".
    WHEN 3 THEN
        ASSIGN vcestado = "3-Depreciado".
    OTHERWISE
        ASSIGN vcestado = "".
    END.

    FORM
        TemResultado2.TAgeActu  COLUMN-LABEL "Agencia"           FORMAT "ZZZ"                  
        TemResultado2.TGrupo    COLUMN-LABEL "Grupo"             FORMAT "99999" 
        vcactivo               COLUMN-LABEL "Activo De Baja"    FORMAT "X(50)" 
        TemResultado2.TCenCosto COLUMN-LABEL "C.Costo"           FORMAT "ZZZZZ" 
        SPACE(4)                                                               
        vcestado               COLUMN-LABEL "Estado"            FORMAT "X(14)" 
        TemResultado2.TAclara   COLUMN-LABEL "Aclaracion"        FORMAT "X(80)" 
        WITH FRAME FRep DOWN COLUMN 1 WIDTH 240
            NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT NO-LABELS STREAM-IO.

    IF FIRST-OF(TemResultado2.TAgeActu) THEN DO:
       ASSIGN vcnomage = "".
       FIND Agencias WHERE Agencias.Agencia EQ TemResultado2.TAgeActu
       NO-LOCK NO-ERROR.
       IF AVAILABLE (Agencias) THEN
          ASSIGN vcnomage = Agencias.Nombre.
       FORM                                          
          vcNom   FORMAT "X(60)"
         WITH FRAME FAge NO-BOX NO-LABEL WIDTH 60.
       DISPLAY ("Agencia: " + (STRING(Agencias.Agencia,"ZZZ")) + " - "+ CAPS(vcnomage)) @ vcNom  
         WITH FRAME FAge.                                                     
       DOWN WITH FRAME FAge.
    END.

    IF FIRST-OF(TemResultado2.TGrupo) THEN DO:
       FIND Varios WHERE Varios.Tipo EQ 7 AND Varios.Codigo EQ TemResultado2.TGrupo
       NO-LOCK NO-ERROR.
       IF AVAILABLE Varios THEN 
          ASSIGN vcnomgru = Varios.Descripcion.
       FORM                                          
          vcNom   FORMAT "X(50)"
         WITH FRAME FAge NO-BOX NO-LABEL WIDTH 60.
       DISPLAY ("Grupo: " + (STRING(Varios.Codigo,"99999")) + " - "+ CAPS(vcnomgru)) @ vcNom         
         WITH FRAME FAge.                                                     
       DOWN WITH FRAME FAge.

    END.
    DISPLAY 
        TemResultado2.TAgeActu
        TemResultado2.TGrupo   
        vcactivo              
        TemResultado2.TCenCosto
        vcestado              
        TemResultado2.TAclara  
      WITH FRAME FRep.
    DOWN WITH FRAME FRep.

    IF LAST-OF (TemResultado2.TAgeActu) OR LAST-OF(TemResultado2.TGrupo) THEN DO:
       DISPLAY ("") @ vcNom SKIP
         WITH FRAME FRep.
       DOWN WITH FRAME FRep.
    END.
END.
DISPLAY
   "Total de Activos : " viTotReg
  WITH FRAME FRep1 NO-LABEL.
DOWN WITH FRAME FRep1.
OUTPUT CLOSE.

END PROCEDURE.

/* DEFINE VAR W_Dispositivo AS CHARACTER INITIAL "A".  */
/* DEFINE VAR W_sw          AS LOGICAL.                */
/*                                                     */
/* /* RUN _SetCurs.p ("WAIT"). */                      */
/* OUTPUT TO VALUE({1}) NO-ECHO PAGED PAGE-SIZE 81.    */
/* RUN ProcesoImprimir.                                */
/* OUTPUT CLOSE.                                       */
/* /* RUN _SetCurs.p ("ARROW"). */                     */
/*                                                     */
/*                                                     */
/* END PROCEDURE.                                      */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpIngresoAct_Fijo wWin 
PROCEDURE ImpIngresoAct_Fijo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 OUTPUT TO VALUE(listado).
 /* {Incluido\RepEncabezado.I} */
 {Incluido\RepHeader.i}
 
 ASSIGN W_NmesInf = vcMes[MONTH(W_Fecha)] + " de " + STRING(YEAR(W_Fecha)).
 ASSIGN W_Reporte = "Reporte   : Cargue Masiva Activos Fijos.    Fecha : " + STRING(W_Fecha,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS").
 VIEW FRAME F-Encabezado.
 
 ASSIGN viTotReg = 0.
 FOR EACH TemResultado BREAK BY TemResultado.Agencia
     BY TemResultado.Codigo:
 
     ASSIGN viTotReg  = viTotReg + 1.
     ASSIGN vccodigo = TemResultado.Codigo.
 
     CASE TemResultado.Estado:
         WHEN 1 THEN
             ASSIGN vcestado = "1-Activo".
         WHEN 2 THEN
             ASSIGN vcestado = "2-Retirado".
         WHEN 3 THEN
             ASSIGN vcestado = "3-Depreciado".
         OTHERWISE
             ASSIGN vcestado = "".
     END.
 
     FORM
         TemResultado.Agencia         COLUMN-LABEL "Agencia"            FORMAT "ZZZZ"
         vccodigo                     COLUMN-LABEL "Codigo"             FORMAT "X(15)"
         TemResultado.Grupo           COLUMN-LABEL "Grupo"              FORMAT "ZZZZZ"
         TemResultado.CenCosto        COLUMN-LABEL "C.Costo"            FORMAT "ZZZZZ"
         TemResultado.Nombre          COLUMN-LABEL "Nombre"             FORMAT "X(35)"
         vcestado                     COLUMN-LABEL "Estado"             FORMAT "X(14)"
         TemResultado.Anos_Adepreciar COLUMN-LABEL "Años.Depre (Meses)" FORMAT "ZZZ"
         TemResultado.Per_Depreciado  COLUMN-LABEL "Peri.Depre (Meses)" FORMAT "ZZZ"
         TemResultado.CostoHis        COLUMN-LABEL "Cos.Historico"      FORMAT "->>>,>>>,>>>,>>9"
         TemResultado.Sdodepre        COLUMN-LABEL "Sdo.Depreciacion"   FORMAT "->>>,>>>,>>>,>>9"
         SPACE(4)
         TemResultado.Detalle         COLUMN-LABEL "Observación"        FORMAT "X(20)"
         WITH FRAME FRep1 DOWN COLUMN 1 WIDTH 240
             NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
     DISPLAY
         TemResultado.Agencia
         vccodigo
         TemResultado.Grupo
         TemResultado.CenCosto
         TemResultado.Nombre
         vcestado
         TemResultado.Anos_Adepreciar
         TemResultado.Per_Depreciado 
         TemResultado.CostoHis
         TemResultado.Sdodepre
         TemResultado.Detalle
       WITH FRAME FRep1.
     DOWN WITH FRAME FRep1.
 
 END.
 DISPLAY
    "Total de Activos : " viTotReg
   WITH FRAME FRep3 NO-LABEL.
 DOWN WITH FRAME FRep3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpTemResultadoCenCostos wWin 
PROCEDURE ImpTemResultadoCenCostos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT TO VALUE(listado).
{Incluido\RepEncabezado.I}

ASSIGN W_NmesInf = vcMes[MONTH(W_Fecha)] + " de " + STRING(YEAR(W_Fecha)).
ASSIGN W_Reporte = "Reporte   : Cargue CenCostos Activos Fijos.    Fecha : " + STRING(W_Fecha,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS").
VIEW FRAME F-Encabezado.

ASSIGN viTotReg = 0.
FOR EACH TemResultadoCosto BREAK 
    BY TemResultadoCosto.Agencia
    BY TemResultadoCosto.Cen_Costos:
    
    ASSIGN viTotReg = viTotReg + 1.
    ASSIGN vcnomcen = TemResultadoCosto.Nombre.
    CASE TemResultadoCosto.Estado:
        WHEN 1 THEN
            ASSIGN vcestado = "1-Activo".
        WHEN 2 THEN
            ASSIGN vcestado = "2-Retirado".
        WHEN 3 THEN
            ASSIGN vcestado = "3-Depreciado".
        OTHERWISE
            ASSIGN vcestado = "".
    END.
    FIND FIRST Clientes WHERE Clientes.Nit EQ TemResultadoCosto.Nit_Respon NO-LOCK NO-ERROR.
    IF AVAILABLE (Clientes) THEN
       ASSIGN vcnomres = TRIM(Clientes.Nombre) + " " + TRIM(Clientes.Apellido1) + " " + TRIM(Clientes.Apellido2).
    ELSE
       ASSIGN vcnomres = "".
    FORM
        TemResultadoCosto.Agencia         COLUMN-LABEL "Agencia"         FORMAT "ZZZZZ"
        TemResultadoCosto.Cen_Costos      COLUMN-LABEL "C.Costo"         FORMAT "ZZZZZ"
        vcnomcen                          COLUMN-LABEL "Nombre"          FORMAT "X(40)"
        TemResultadoCosto.Fec_Creacion    COLUMN-LABEL "Fec.Creacion"    FORMAT "99/99/9999"   
        vcestado                          COLUMN-LABEL "Estado"          FORMAT "X(14)"
        TemResultadoCosto.Nit_Respon      COLUMN-LABEL "Responsable"     FORMAT "X(14)"
        vcnomres                          COLUMN-LABEL "Nom.Responsable" FORMAT "X(40)"
        SPACE(4)
        TemResultadoCosto.Detalle         COLUMN-LABEL "Observación"  FORMAT "X(20)"
        WITH FRAME FRep1 DOWN COLUMN 1 WIDTH 200
            NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    DISPLAY
        TemResultadoCosto.Agencia
        TemResultadoCosto.Cen_Costos
        vcnomcen       
        TemResultadoCosto.Fec_Creacion 
        vcestado
        TemResultadoCosto.Nit_Respon 
        vcnomres
        TemResultadoCosto.Detalle
      WITH FRAME FRep1.
    DOWN WITH FRAME FRep1.

END.
DISPLAY
   "Total de Cen.Costos : " viTotReg
  WITH FRAME FRep3 NO-LABEL.
DOWN WITH FRAME FRep3.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ImpTrasladosAct_Fijo wWin 
PROCEDURE ImpTrasladosAct_Fijo :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
OUTPUT TO VALUE(listado).
FOR EACH TemResultado1 BY TemResultado1.TAgencia BY TemResultado1.TGrupo BY TemResultado1.TCodigo:
    ASSIGN viTotReg   = viTotReg + 1.
    ASSIGN vcresponsa = TRIM(TemResultado1.TResponsa) + " - " + TRIM(TemResultado1.TNomRes).
    FORM 
        TemResultado1.TAgencia  COLUMN-LABEL "Agencia"      FORMAT "ZZZ"           
        TemResultado1.TGrupo    COLUMN-LABEL "Grupo"        FORMAT "99999"    
        TemResultado1.TCodigo   COLUMN-LABEL "Codigo"       FORMAT "X(15)"             
        TemResultado1.TNombre   COLUMN-LABEL "Nombre"       FORMAT "X(40)" 
        TemResultado1.TCenCosto COLUMN-LABEL "C.Costo"      FORMAT "ZZZZZ" 
        SPACE(4)
        vcresponsa              COLUMN-LABEL "Responsable"  FORMAT "X(50)"
        TemResultado1.TObserva  COLUMN-LABEL "Observacion"  FORMAT "X(30)" 
        WITH FRAME FTemConta DOWN COLUMN 1 WIDTH 240
             NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT STREAM-IO.
    DISPLAY 
        TemResultado1.TAgencia 
        TemResultado1.TGrupo   
        TemResultado1.TCodigo  
        TemResultado1.TNombre
        TemResultado1.TCenCosto
        vcresponsa
        TemResultado1.TObserva 
        WITH FRAME FTemConta.
    DOWN WITH FRAME FTemConta.
END.
DISPLAY
   "Total de Activos : " viTotReg
  WITH FRAME FRep3 NO-LABEL.
DOWN WITH FRAME FRep3.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Ingresos wWin 
PROCEDURE Ingresos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE TemAct_Fijo.
EMPTY TEMP-TABLE TemResultado.

RUN Proceso_Cargue.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE initializeObject wWin 
PROCEDURE initializeObject :
/*------------------------------------------------------------------------------
  Purpose:     Super Override
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

  /* Code placed here will execute PRIOR to standard behavior. */

  RUN SUPER.

  /* Code placed here will execute AFTER standard behavior.    */


  ASSIGN W_orden:SCREEN-VALUE IN FRAME {&FRAME-NAME} = "Codigo".
  RUN cancelObject IN h_wfact_fijo.
  
END PROCEDURE.


/* DYNAMIC-FUNCTION('setBGColor':U IN h_dyntoolbar,         */
/*    INPUT 17).                                            */
/* DYNAMIC-FUNCTION('setBGColor':U IN h_folder,             */
/*    INPUT 19).                                            */
/* DYNAMIC-FUNCTION('setDisabledActions':U IN h_dyntoolbar, */
/*    INPUT "add,delete,update,cancel" /* CHARACTER */).    */
/*                                                          */
/* END PROCEDURE.                                           */

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ProcesoImprimir wWin 
PROCEDURE ProcesoImprimir :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
{Incluido\RepEncabezado.I}
ASSIGN W_Reporte = "Reporte   : Traslado de Activos Fijos.    Fecha : " + STRING(W_Fecha,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS")
       W_EncColumna = "Código             Nombre                               C.Costo          Responsable                               Observación".
VIEW FRAME F-Encabezado.
VIEW FRAME f-ftr.
ASSIGN viTotReg = 0.

FOR EACH TemResultado1 
    BREAK BY TemResultado1.TAgencia BY TemResultado1.TGrupo BY TemResultado1.TCodigo:
    ASSIGN viTotReg   = viTotReg + 1.
    ASSIGN vcresponsa = TRIM(TemResultado1.TResponsa) + " - " + TRIM(TemResultado1.TNomRes).
    FORM
        TemResultado1.TCodigo   COLUMN-LABEL "Codigo"      FORMAT "X(14)"
        TemResultado1.TNombre   COLUMN-LABEL "Nombre"      FORMAT "X(40)"
        TemResultado1.TCenCosto COLUMN-LABEL "C.Costo"     FORMAT "ZZZZZ"
        SPACE(4)
        vcresponsa             COLUMN-LABEL "Responsable" FORMAT "X(50)"
        TemResultado1.TObserva  COLUMN-LABEL "Observacion" FORMAT "X(30)"
        WITH FRAME FRep DOWN COLUMN 1 WIDTH 240
            NO-ATTR-SPACE NO-VALIDATE NO-BOX USE-TEXT NO-LABELS STREAM-IO.

    IF FIRST-OF(TemResultado1.TAgencia) THEN DO:
       ASSIGN vcnomage = "".
       FIND Agencias WHERE Agencias.Agencia EQ TemResultado1.TAgencia
       NO-LOCK NO-ERROR.
       IF AVAILABLE (Agencias) THEN
          ASSIGN vcnomage = Agencias.Nombre.
       FORM                                          
          vcNom   FORMAT "X(60)"
         WITH FRAME FAge NO-BOX NO-LABEL WIDTH 60.
       DISPLAY ("Agencia: " + (STRING(Agencias.Agencia,"ZZZ")) + " - "+ CAPS(vcnomage)) @ vcNom  
         WITH FRAME FAge.                                                     
       DOWN WITH FRAME FAge.
    END.

    IF FIRST-OF(TemResultado1.TGrupo) THEN DO:
       FIND Varios WHERE Varios.Tipo EQ 7 AND Varios.Codigo EQ TemResultado1.TGrupo
       NO-LOCK NO-ERROR.
       IF AVAILABLE Varios THEN 
          ASSIGN vcnomgru = Varios.Descripcion.
       FORM                                          
          vcNom   FORMAT "X(50)"
         WITH FRAME FAge NO-BOX NO-LABEL WIDTH 60.
       DISPLAY ("Grupo: " + (STRING(Varios.Codigo,"99999")) + " - "+ CAPS(vcnomgru)) @ vcNom         
         WITH FRAME FAge.                                                     
       DOWN WITH FRAME FAge.

    END.
    DISPLAY 
        TemResultado1.TCodigo 
        TemResultado1.TNombre
        TemResultado1.TCenCosto
        vcresponsa
/*         TRIM(TemResultado1.TResponsa) @ TemResultado1.TResponsa */
/*         TRIM(TemResultado1.TNomRes)   @ TemResultado1.TNomRes   */
        TemResultado1.TObserva
      WITH FRAME FRep.
    DOWN WITH FRAME FRep.

    IF LAST-OF (TemResultado1.TAgencia) OR LAST-OF(TemResultado1.TGrupo) THEN DO:
       DISPLAY ("") @ vcNom SKIP
         WITH FRAME FRep.
       DOWN WITH FRAME FRep.
    END.
END.
DISPLAY
   "Total de Activos : " viTotReg
  WITH FRAME FRep1 NO-LABEL.
DOWN WITH FRAME FRep1.
OUTPUT CLOSE.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso_Baja wWin 
PROCEDURE Proceso_Baja :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
TranConta:
REPEAT TRANSACTION ON ERROR UNDO TranConta, LEAVE TranConta:
  RUN Cargue_Bajas NO-ERROR. 
  IF OKpressed = FALSE THEN
     RETURN. 
  RUN Actualiza_Bajas NO-ERROR.

  ASSIGN listado = W_PathSpl + "BajasActFijos1-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".lst".
  ASSIGN vclis = listado.
  RUN ImpBajasAct_Fijo.
  
  ASSIGN listado = W_PathSpl + "BajasActFijos2-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".lst".
  ASSIGN vclis1 = listado.
  RUN ImpBajasAct_Fijo1.
  MESSAGE "Achivo Plano Generado : " vclis SKIP
          "                                         " vclis1
      VIEW-AS ALERT-BOX.

  LEAVE.
END.  /*Fin Tx*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso_Cargue wWin 
PROCEDURE Proceso_Cargue :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
TranConta:
REPEAT TRANSACTION ON ERROR UNDO TranConta, LEAVE TranConta:
  RUN Cargue_Ingresos NO-ERROR. 
  IF OKpressed = FALSE THEN
     RETURN. 
  RUN Actualiza_Ingresos NO-ERROR.

  ASSIGN listado = W_PathSpl + "CargueActFijos-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".lst".
  MESSAGE "Archivo Plano Generado : " listado
      VIEW-AS ALERT-BOX.
  RUN ImpIngresoAct_Fijo. /* NO-ERROR.*/
  OUTPUT CLOSE.

  LEAVE.
END.  /*Fin Tx*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso_CenCostos wWin 
PROCEDURE Proceso_CenCostos :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
TranConta:
REPEAT TRANSACTION ON ERROR UNDO TranConta, LEAVE TranConta:
  RUN Cargue_CenCostos NO-ERROR. 
  IF OKpressed = FALSE THEN
     RETURN. 
  RUN Actualiza_CenCostos NO-ERROR.

  ASSIGN listado = W_PathSpl + "CargueCenCostos-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".lst".
  RUN ImpTemResultadoCenCostos. 
  MESSAGE "Archivo Plano Generado : " listado
      VIEW-AS ALERT-BOX.
  OUTPUT CLOSE.

  LEAVE.
 END.  /*Fin Tx*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Proceso_Traslados wWin 
PROCEDURE Proceso_Traslados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
TranConta:
REPEAT TRANSACTION ON ERROR UNDO TranConta, LEAVE TranConta:
  RUN Cargue_Traslados NO-ERROR. 
  IF OKpressed = FALSE THEN
     RETURN. 
  RUN Actualiza_Traslados      NO-ERROR.
  RUN Val_ResponsableTraslados NO-ERROR.

  ASSIGN listado = W_PathSpl + "TrasActFijos1-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".lst".
  ASSIGN vclis = listado.
  RUN ImpTrasladosAct_Fijo.

  ASSIGN listado = W_PathSpl + "TrasActFijos2-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".lst".
  ASSIGN vclis1 = listado.
  {Incluido\ImpArch.i "Listado" Tamano}  /*ProcesoImprimir*/
  MESSAGE "Archivo Plano Generado : " vclis SKIP
          "                                          " vclis1
      VIEW-AS ALERT-BOX.

  LEAVE.
END.  /*Fin Tx*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Traslados wWin 
PROCEDURE Traslados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
EMPTY TEMP-TABLE TemTraslado.
EMPTY TEMP-TABLE TemResultado1.

RUN Proceso_Traslados.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_ResponsableBajas wWin 
PROCEDURE Val_ResponsableBajas :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Val_ResponsableTraslados wWin 
PROCEDURE Val_ResponsableTraslados :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
FOR EACH TemResultado1 BY TemResultado1.TResponsa:
    FIND FIRST Clientes WHERE Clientes.Nit EQ TemResultado1.TResponsa
    NO-LOCK NO-ERROR.
    IF AVAILABLE(Clientes) THEN
       UPDATE TemResultado1.TNomRes = TRIM(nombre) + " " + TRIM(apellido1) + " " + TRIM(apellido2).
    ELSE
       UPDATE TemResultado1.TNomRes = "No Existe en Clientes..".
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

