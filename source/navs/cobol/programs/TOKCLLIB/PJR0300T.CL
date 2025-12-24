/. ***TEST****************************************************  ./
/. *   サカタのタネ　                                        *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム                     *  ./
/. *   JOB-ID      :    PJR02900                             *  ./
/. *   JOB-NAME    :    受領返品計上伝票確認（伝票纏め対応） *  ./
/. ***********************************************************  ./
    PGM (P1-?JIKKBN)

/.##INパラメタ##./
    PARA  ?JIKKBN ,STRING*1,IN,VALUE-' '
            /.実行種別                  ./
            /.  ' '    :   通常起動     ./
            /.  '1'    :   纏め機動　   ./

/.--------------------------------------------------------------./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?PGMES    ,STRING*5                     /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ  ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PFU30500'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./

    VAR ?BUMON    ,STRING*4,VALUE-'    '         /.部門CD./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '     /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '           /.担当者CD./
    VAR ?TOKCD    ,STRING*8,VALUE-'00000000'     /.取引先CD./
    VAR ?TENCD    ,STRING*5,VALUE-'00000'        /.店舗CD  ./
    VAR ?KENDT    ,STRING*8,VALUE-'00000000'     /.検収日  ./
    VAR ?DENNO    ,STRING*9,VALUE-'000000000'    /.伝票番号./
    VAR ?PGNM     ,STRING*40                     /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                     /.      2    ./
    VAR ?KEKA2    ,STRING*40                     /.      3    ./
    VAR ?KEKA3    ,STRING*40                     /.      4    ./
    VAR ?KEKA4    ,STRING*40                     /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM  :=  '受領返品計上伝票確認'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKDTLIB/TOKMDLIB/TOKKLIB/TOKSOLIB/
            TOKDLIB

/.##部門ｺｰﾄﾞ担当者ｺｰﾄﾞ取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0
          THEN
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##受領返品計上伝票確認（伝票纏め対応）##./
SJR0300I:

    ?STEP :=   'SJR0300I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?JIKKBN = '1' THEN
        OVRF FILE-COMRHEL1,TOFILE-COMRMHL2.TOKDTLIB
    END

    CALL      SJR030TI.TOKSOLIB,PARA-(?JIKKBN,?BUMON,?TANCD,
              ?TOKCD,?TENCD,?KENDT,?DENNO)
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【受領返品計上伝票確認】'
              GOTO ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC


ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '受領返品計上伝票確認が異常終了'
    ?KEKA2 :=  'しました。ログ採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC
