DEFINE INPUT  PARAMETER W_Age AS INTEGER.
DEFINE INPUT  PARAMETER W_Age1 AS INTEGER.
DEFINE INPUT  PARAMETER W_Mes AS INTEGER.
DEFINE INPUT  PARAMETER W_Ano AS INTEGER.
DEFINE OUTPUT PARAMETER W_Operador1 AS CHARACTER.
DEFINE OUTPUT PARAMETER W_Operador2 AS CHARACTER.
DEFINE OUTPUT PARAMETER W_Operando  AS CHARACTER.
DEFINE OUTPUT PARAMETER W_Cuenta    LIKE Cuentas.Cuenta.
DEFINE OUTPUT PARAMETER W_Resultado AS CHARACTER.

DEFINE VARIABLE W_Op1 AS CHARACTER.
DEFINE VARIABLE W_Op2 AS CHARACTER.
DEFINE VARIABLE W_Op  AS CHARACTER.
DEFINE VARIABLE W_Cu  LIKE Cuentas.Cuenta.
DEFINE VARIABLE W_Res AS CHARACTER.
DEFINE VARIABLE W_Tot    AS INTEGER FORMAT '9999' INITIAL 0.
DEFINE VARIABLE W_TotPor AS DECIMAL INITIAL 0.
DEFINE VARIABLE W_TotGen AS DECIMAL INITIAL 0.
/************************************************/
IF SEARCH('ind_depcar.r') NE ? THEN
  RUN ind_depcar.r (INPUT W_Age,INPUT W_Age1,INPUT W_Mes ,INPUT W_Ano, OUTPUT W_Op1 ,OUTPUT W_Op2 ,OUTPUT W_Op ,OUTPUT W_Cu ,OUTPUT W_Res).
 ELSE
  W_Res = '0'.
W_TotPor = W_TotPor + DECIMAL(W_Res). 
IF DECIMAL(W_Res) < 101 THEN
  W_Tot = W_Tot + 1.  
 ELSE
  IF DECIMAL(W_Res) GE 101 AND DECIMAL(W_Res) < 120 THEN
    W_Tot = W_Tot + 2.  
   ELSE 
    IF DECIMAL(W_Res) GE 120 AND DECIMAL(W_Res) < 140 THEN
       W_Tot = W_Tot + 3.  
     ELSE  
      IF DECIMAL(W_Res) GE 140 AND DECIMAL(W_Res) < 200 THEN
        W_Tot = W_Tot + 4.  
       ELSE 
        IF DECIMAL(W_Res) GE 200 THEN
          W_Tot = W_Tot + 5.  
/***********************************************/
IF SEARCH('ind_caract.r') NE ? THEN
  RUN ind_caract.r (INPUT W_Age,INPUT W_Age1,INPUT W_Mes ,INPUT W_Ano, OUTPUT W_Op1 ,OUTPUT W_Op2 ,OUTPUT W_Op ,OUTPUT W_Cu ,OUTPUT W_Res).
 ELSE
  W_Res = '0'.
W_TotPor = W_TotPor + DECIMAL(W_Res). 
IF DECIMAL(W_Res) < 70 THEN
  W_Tot = W_Tot + 1.  
 ELSE 
  IF DECIMAL(W_Res) GE 70 AND DECIMAL(W_Res) < 60 THEN
    W_Tot = W_Tot + 2.  
   ELSE 
    IF DECIMAL(W_Res) GE 60 AND DECIMAL(W_Res) < 55 THEN
      W_Tot = W_Tot + 3.  
     ELSE 
      IF DECIMAL(W_Res) GE 55 AND DECIMAL(W_Res) < 50 THEN
        W_Tot = W_Tot + 4.  
       ELSE 
        IF DECIMAL(W_Res) GE 50 THEN
         W_Tot = W_Tot + 5.  
/***********************************************/
IF SEARCH('ind_actfact.r') NE ? THEN
  RUN ind_actfact.r (INPUT W_Age,INPUT W_Age1,INPUT W_Mes ,INPUT W_Ano, OUTPUT W_Op1 ,OUTPUT W_Op2 ,OUTPUT W_Op ,OUTPUT W_Cu ,OUTPUT W_Res).
 ELSE
  W_Res = '0'.
W_TotPor = W_TotPor + DECIMAL(W_Res). 
IF DECIMAL(W_Res) < 11 THEN
  W_Tot = W_Tot + 1.  
 ELSE 
  IF DECIMAL(W_Res) GE 11 AND DECIMAL(W_Res) < 12 THEN
    W_Tot = W_Tot + 2.  
   ELSE 
    IF DECIMAL(W_Res) GE 12 AND DECIMAL(W_Res) < 15 THEN
      W_Tot = W_Tot + 3.  
     ELSE 
      IF DECIMAL(W_Res) GE 15 AND DECIMAL(W_Res) < 17 THEN
        W_Tot = W_Tot + 4.  
       ELSE 
        IF DECIMAL(W_Res) GE 17 THEN
          W_Tot = W_Tot + 5.  
/***********************************************/
IF SEARCH('ind_marsuf.r') NE ? THEN
  RUN ind_caract.r (INPUT W_Age,INPUT W_Age1,INPUT W_Mes ,INPUT W_Ano, OUTPUT W_Op1 ,OUTPUT W_Op2 ,OUTPUT W_Op ,OUTPUT W_Cu ,OUTPUT W_Res).
 ELSE
  W_Res = '0'.
W_TotPor = W_TotPor + DECIMAL(W_Res). 
IF DECIMAL(W_Res) < 120 THEN
  W_Tot = W_Tot + 1.  
 ELSE 
  IF DECIMAL(W_Res) GE 120 AND DECIMAL(W_Res) < 110 THEN
    W_Tot = W_Tot + 2.  
   ELSE 
    IF DECIMAL(W_Res) GE 110 AND DECIMAL(W_Res) < 100 THEN
      W_Tot = W_Tot + 3.  
     ELSE 
      IF DECIMAL(W_Res) GE 100 AND DECIMAL(W_Res) < 90 THEN
        W_Tot = W_Tot + 4.  
       ELSE 
        IF DECIMAL(W_Res) GE 90 THEN
         W_Tot = W_Tot + 5.  
/***********************************************/
IF W_TotPor NE 0 THEN
  W_TotGen = W_TotPor / W_Tot.
 ELSE
  W_TotGen = 0.
  
ASSIGN W_Resultado = STRING(W_TotGen,'->>>,>99.99')  
       W_Operador1 = STRING(W_TotPor,'->>,>>>,999.99')
       W_Operador2 = STRING(W_Tot,'999')
       W_Operando = '/'
       W_Cuenta = 'Costo Agencia'. 
