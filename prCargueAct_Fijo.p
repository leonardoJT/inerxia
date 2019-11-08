DEFINE VARIABLE W_NmesInf   AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE vccodigo    AS CHARACTER FORMAT "X(15)" NO-UNDO.
DEFINE VARIABLE OKpressed   AS LOGICAL                  INITIAL TRUE.
DEFINE VARIABLE viTotReg    AS INTEGER                  INITIAL 0  NO-UNDO.
DEFINE VARIABLE Archivo     AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnomage    AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcnomgru    AS CHARACTER FORMAT "X(40)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcactivo    AS CHARACTER FORMAT "X(60)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcactivoU   AS CHARACTER FORMAT "X(60)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcconcep    AS CHARACTER FORMAT "X(20)" INITIAL "" NO-UNDO.
DEFINE VARIABLE vcestado    AS CHARACTER FORMAT "X(12)" INITIAL "" NO-UNDO.

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

/* {Incluido\VARIABLE.I "SHARED"} */
/* {Incluido\VARCON.I "SHARED"} */
/********************************/
  DEFINE VARIABLE W_Usuario   LIKE usuarios.usuario       INITIAL "339". /* 308 - Contabiliza*/
  DEFINE VARIABLE W_Fecha     AS DATE   INITIAL TODAY.
  DEFINE VARIABLE W_PathSpl   AS CHARACTER FORMAT "X(20)" INITIAL "c:\info_juriscoop\".
  DEFINE VARIABLE W_Agencia   LIKE Agencia.Agencia        INITIAL "024". /*"035".*/

    DEFINE {1} VAR W_Estacion    LIKE Estaciones.Estacion INITIAL "5".
/*     DEFINE {1} VAR W_Usuario     LIKE Usuarios.Usuario. */
    DEFINE {1} VAR W_Clave       LIKE Usuarios.Clave FORMAT "X(16)".
    DEFINE {1} VAR W_Prioridad   LIKE Usuarios.Prioridad INITIAL "".
/*     DEFINE {1} VAR W_Agencia     LIKE Usuarios.Agencia INITIAL 0.  */
    DEFINE {1} VAR W_Ciudad      LIKE Agencia.Ciudad INITIAL 0.
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
/*     DEFINE {1} VAR W_Fecha         AS DATE FORMAT "99/99/9999" INITIAL TODAY.  */
    DEFINE {1} VAR W_ficina        AS CHARACTER FORMAT "X(40)" VIEW-AS COMBO-BOX INNER-LINES 4 SIZE 40 BY 1.
    DEFINE {1} VAR W_Path        LIKE Entidad.Dir_Programas.
/*     DEFINE {1} VAR W_Pathspl     LIKE Entidad.Dir_Spl.  */
    DEFINE {1} VAR W_Eleccion      AS LOGICAL.
    DEFINE {1} VAR W_CedGral     LIKE Clientes.Nit.
    DEFINE {1} VAR W_CenCosGral  LIKE Cen_Costos.Cen_Costos.
    DEFINE {1} VAR W_Cadena        AS CHARACTER FORMAT "X(9)" INITIAL "SIFINCOOP".
    /*DEFINE     VAR Agencia_Cnt     AS INTEGER FORMAT "999".*/
    DEFINE {1} VAR P-Valida        AS LOGICAL.
    DEFINE {1} VAR W_VCodPcto    LIKE Ahorros.Cod_Ahorro.
    DEFINE {1} VAR W_VCueAhorro  LIKE Ahorros.Cue_Ahorros.
    DEFINE {1} VAR W_Solicitud   LIKE Solicitud.Num_Solicitud.
    DEFINE {1} VAR W_PagareS     LIKE Creditos.Pagare.
    DEFINE {1} VAR P_SdoTot      LIKE Creditos.Sdo_Capital.
    DEFINE {1} VAR W_OfiCierre   LIKE Agencias.Agencia.
  /********************************************************************************** 
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

EMPTY TEMP-TABLE TemAct_Fijo.
EMPTY TEMP-TABLE TemResultado.

RUN Proceso.

PROCEDURE Proceso:
TranConta:
REPEAT TRANSACTION ON ERROR UNDO TranConta, LEAVE TranConta:
  RUN Cargue NO-ERROR. 
  IF OKpressed = FALSE THEN
     RETURN. 
  RUN Actualiza NO-ERROR.

  ASSIGN listado = W_PathSpl + "CargueActFijos-" + STRING(W_Fecha,"99999999") + "-" + STRING(TIME) + "-" + STRING(W_Usuario,"999") + ".lst".
  MESSAGE "Archivo Plano Generado : " listado
      VIEW-AS ALERT-BOX.
  RUN ImpTemResultado. /* NO-ERROR.*/

  LEAVE.
 END.  /*Fin Tx*/
END PROCEDURE.

PROCEDURE Cargue:
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

PROCEDURE Actualiza:
ASSIGN viTotReg = 0.
FOR EACH TemAct_Fijo NO-LOCK:
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
END PROCEDURE.

PROCEDURE ImpTemResultado:
    OUTPUT TO VALUE(listado).
    /* {Incluido\RepEncabezado.I} */
    {Incluido\RepHeader.i}
    
    ASSIGN W_NmesInf = vcMes[MONTH(W_Fecha)] + " de " + STRING(YEAR(W_Fecha)).
    ASSIGN W_Reporte = "Reporte   : Cargue Masiva Activos Fijos.    Fecha : " + STRING(W_Fecha,"99/99/9999") + "     Hora :" + STRING(TIME,"HH:MM:SS").
    /*        W_EncColumna = "Agencia    Codigo   Grupo   C.Costo        Nombre     Estado    Cos.Historico   Sdo.Deprecia         Detalle".              */
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
/*     VIEW FRAME f-ftr. */
    
    DISPLAY
       "Total de Activos : " viTotReg
      WITH FRAME FRep3 NO-LABEL.
    DOWN WITH FRAME FRep3.
    OUTPUT CLOSE.
END PROCEDURE.
