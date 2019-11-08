 
 
 DEFINE INPUT-OUTPUT PARAMETER P_Cuenta AS CHARACTER.
 DEFINE INPUT-OUTPUT PARAMETER P_Digito AS INTEGER.

 DEF VAR miContador AS INTEGER INITIAL 0.
 DEF VAR miArregloPA  AS INTEGER EXTENT 15.
 DEF VAR miArregloDA  AS INTEGER EXTENT 15.
 ASSIGN miArregloPA[1]  = 3  
        miArregloPA[2]  = 7  
        miArregloPA[3]  = 13  
        miArregloPA[4]  = 17  
        miArregloPA[5]  = 19  
        miArregloPA[6]  = 23  
        miArregloPA[7]  = 29  
        miArregloPA[8]  = 37  
        miArregloPA[9]  = 41  
        miArregloPA[10]  = 43  
        miArregloPA[11] = 47 
        miArregloPA[12] = 53 
        miArregloPA[13] = 59 
        miArregloPA[14] = 67 
        miArregloPA[15] = 71. 



    DEF VAR miResiduo  AS INTEGER INITIAL 0.  
    DEF VAR W_Suma AS INTEGER INITIAL 0.
    DEF VAR miChequeo  AS INTEGER INITIAL 0.
    miArregloDA = 0.
  
    REPEAT miContador = 1  TO 15:
      IF miContador LE LENGTH(P_Cuenta) THEN
        miArregloDA[miContador] = INTEGER(SUBSTRING(P_Cuenta,LENGTH(P_Cuenta) + 1 - miContador,1)).
      ELSE
        miArregloDA[miContador] = 0.
    END.
    REPEAT micontador = 1 TO 15:
        W_Suma = W_Suma + ( miArregloDA[miContador] * miArregloPA[miContador] ).
    END.
    IF W_Suma MODULO 11 = 0 THEN
      P_Digito = 0.
    ELSE
      P_Digito = 11 - (W_Suma MODULO 11).
    IF P_Digito GT 9 THEN
       P_Digito = P_Digito - 9.
    P_Cuenta = P_Cuenta + STRING(P_Digito).   


