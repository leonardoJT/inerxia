FOR EACH cuentas WHERE cuenta = '14912703':
    MESSAGE 'paso1' cuenta nombre cta_homol
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    ASSIGN cta_homol = '14912703'.

    MESSAGE 'paso2' cuenta nombre cta_homol
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
    
END.
