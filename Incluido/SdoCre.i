  /**********  HALLAR TOTAL SALDO DE UN PRESTAMO (Crédito)  ****************
  *************************************************************************/
   P_SdoTot = Creditos.Sdo_Capital      + Creditos.Int_MorCobrar 
              + Creditos.Int_DifCobro   + Creditos.Int_Corrientes.
   IF Creditos.Int_Anticipado GT 0 THEN
      P_SdoTot = P_SdoTot + Creditos.Int_Anticipado.
   
