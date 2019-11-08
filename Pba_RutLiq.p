DEFI VAR P_ImpAplic LIKE ahorros.sdo_dispon.


RUN RutGMF.R (INPUT  TRUE,1,1,1,4,"890981459-4", 
              INPUT  "890981459-4",010302001,14000,   
              INPUT  5,                                                        
              INPUT  STRING(9999),"LiqInt.Ahorro",1,0,                              
              OUTPUT P_ImpAplic).                                                            
         IF ERROR-STATUS:ERROR THEN DO:                                                               
            MESSAGE "El programa RutGMF.P...Retornó ERROR, no se permite la operación." SKIP
                VIEW-AS ALERT-BOX ERROR.                                                              
            RETURN ERROR.                                                                             
         END. 
/*
PROCEDURE RutGmf:
 DEFINE INPUT  PARAMETER P_GrabarEE       AS LOGICAL NO-UNDO.              /*False NO Actualiza, True SI*/
  DEFINE INPUT  PARAMETER P_AgeTx          LIKE Agencias.Agencia.
  DEFINE INPUT  PARAMETER P_Age            LIKE Agencias.Agencia.
  DEFINE INPUT  PARAMETER P_Tipo           AS INTEG FORM "9".   /*1 Captac, 2 Colocac, 3 Contable, 4 Iversiones*/
  DEFINE INPUT  PARAMETER P_Pcto           LIKE Ahorros.Cod_Ahorro.
  DEFINE INPUT  PARAMETER P_Ced            LIKE Ahorros.Nit.
  DEFINE INPUT  PARAMETER P_NumCta         LIKE Ahorros.Cue_Ahorro.  /*Cue_Ahorro o Cta-Contable*/
  DEFINE INPUT  PARAMETER P_CodOpe         LIKE Operacion.Cod_Operacion.  /*Solo Para las contables*/
  DEFINE INPUT  PARAMETER P_VrTx           LIKE Creditos.Cuota.   /*Valor base a aplicarle el GMF*/
  DEFINE INPUT  PARAMETER P_Cpte           LIKE Mov_Ahorros.Cpte.
  DEFINE INPUT  PARAMETER P_NroDoc         LIKE Mov_Ahorros.Num_Documento.
  DEFINE INPUT  PARAMETER P_Desc           LIKE Mov_Ahorros.Descrip.
  DEFINE INPUT  PARAMETER P_CapInt         AS INTEG FORM "9".   /*0 Capital, 1 Intereses...Solo Ahorros*/
  DEFINE INPUT  PARAMETER P_EfeCheq        AS INTEG FORM "9".   /*0 Efectivo, 1 Cheque, 2 Traslado*/

  /*Paràmetros Output*/
  DEFINE OUTPUT PARAMETER P_VlrImpto      LIKE Creditos.Cuota INITIAL 0.

MESSAGE P_GrabarEE  
         P_AgeTx    
         P_Age      
         P_Tipo     
         P_Pcto     
         P_Ced      
         P_NumCta   
         P_CodOpe   
         P_VrTx     
         P_Cpte     
         P_NroDoc   
         P_Desc     
         P_CapInt   
         P_EfeCheq  
    VIEW-AS ALERT-BOX INFO BUTTONS OK.



DEFI VAR CtasConfig LIKE Cuentas.Cuenta EXTENT 5.
 /* DEFI VAR W_OpCgoAho LIKE Operacion.Cod_Operac INIT 010102001.*/
  DEFI VAR GtoImpto   LIKE Creditos.Cuota INIT 0.
  DEFI VAR ClienImpto LIKE Creditos.Cuota INIT 0.
  DEFI VAR PorAplic   LIKE Deducible.Valor.
  DEFI VAR W_Valor    LIKE Creditos.Cuota INITIAL 0.
  DEFI VAR BaseClient LIKE Creditos.Cuota INITIAL 0.
  DEFI VAR BaseEntid  LIKE Creditos.Cuota INITIAL 0.
  DEFI VAR W_Cta      LIKE Cuentas.Cuenta.
  DEFI VAR W_AcuRet   LIKE Mov_GMF.VrAcum_RetMes INITIAL 0.
  DEFI VAR W_Calc     LIKE Mov_GMF.VrAcum_RetMes INITIAL 0.
 
  
IF P_Ced EQ "890981459-4" THEN
   MESSAGE "llegó a la RutGmf"
       VIEW-AS ALERT-BOX INFO BUTTONS OK.

END.
  */
