
      IF E_NumFila EQ 1 THEN
      DO:
        DO I = 1 TO E_NumColumn BY 1:
          Car = INTEGER(SUBSTRING(E_Fila,Pos,3)).
          ValCol = SUBSTRING(Columnas,i,1).
          chWorkSheet:Columns(ValCol):ColumnWidth = INTEGER(Car).
          ValCol = ValCol + "1".
          chWorkSheet:Range(ValCol):Value = STRING(SUBSTRING(E_Fila,Pos + 3,Car)).
          Pos = Pos + Car + 3.
        END.
        chWorkSheet:Range("A1:" + SUBSTRING(Columnas,E_NumColumn,1) + "1"):Font:Bold = TRUE.
      END.
      Pos = 1.
      E_NumFila = E_NumFila + 1.
      DO I = 1 TO E_NumColumn BY 1:
        Car = INTEGER(SUBSTRING(E_Fila2,Pos,3)).
        ValCol = SUBSTRING(Columnas,i,1) + STRING(E_NumFila).
        chWorkSheet:Range(ValCol):Value = STRING(SUBSTRING(E_Fila2,Pos + 3,Car)).
        Pos = Pos + Car + 3.
      END.
END. 

/* create embedded chart using the data in the Worksheet */
/*chWorksheetRange = chWorksheet:Range("B1:C11").
chWorksheet:ChartObjects:Add(10,150,425,300):Activate.
chExcelApp:ActiveChart:ChartWizard(chWorksheetRange, 3, 1, 2, 1, 1, TRUE,
    "RANGO DE SALDOS   ", "VALORES     ", "OCURRENCIAS ").

/* create chart using the data in the Worksheet */
chChart=chExcelApp:Charts:Add().
chChart:Name = "Rango de saldos ".
chChart:Type = 11.


 /* CARGA EXCEL PARA QUE SEA VISIBLE PARA EL USUARIO */*/
 chExcelApp:Visible = TRUE.



   /* release com-handles */
   RELEASE OBJECT chExcelApp.      
   RELEASE OBJECT chWorkbook.
   RELEASE OBJECT chWorksheet.
/*   RELEASE OBJECT chChart.*/



