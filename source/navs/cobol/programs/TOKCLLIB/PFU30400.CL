/. ***TEST****************************************************  ./
/. *   サカタのタネ　                                        *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム                     *  ./
/. *   JOB-ID      :    PFU30400                             *  ./
/. *   JOB-NAME    :    社内振替発注入力　　　　　           *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?PGMES    ,STRING*5                     /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ  ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PFU30400'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./

    VAR ?SOKO     ,STRING*2,VALUE-'  '           /.実行倉庫ＣＤ./
    VAR ?DSOKO    ,STRING*2,VALUE-'  '           /.代表倉庫ＣＤ./
    VAR ?PGNM     ,STRING*40                     /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                     /.      2    ./
    VAR ?KEKA2    ,STRING*40                     /.      3    ./
    VAR ?KEKA3    ,STRING*40                     /.      4    ./
    VAR ?KEKA4    ,STRING*40                     /.      5    ./
    VAR ?DELDATE  ,STRING*8,VALUE-'00000000'
    VAR ?OPR1     ,STRING*50,VALUE-              /.ﾒｯｾｰｼﾞ1    ./
        '                                                  '
    VAR ?OPR2     ,STRING*50,VALUE-              /.ﾒｯｾｰｼﾞ2    ./
        '                                                  '
    VAR ?OPR3     ,STRING*50,VALUE-              /.ﾒｯｾｰｼﾞ3    ./
        '                                                  '
    VAR ?OPR4     ,STRING*50,VALUE-              /.ﾒｯｾｰｼﾞ4    ./
        '                                                  '
    VAR ?OPR5     ,STRING*50,VALUE-              /.ﾒｯｾｰｼﾞ5    ./
        '                                                  '

    VAR ?BUMON    ,STRING*4,VALUE-'    '         /.部門CD./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '     /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '           /.担当者CD./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?PGNM := '社内振替発注入力'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKDTLIB/TOKKLIB/TOKDLIB/
            ZAIEXCEL/TOKMDLIB/TOKSOLIB

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

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKO,?DSOKO)
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4  :=  '【倉庫ＣＤ取得】'
              GOTO ABEND
    END

/.##社内振替発注入力##./
SFU3040I:

    ?STEP :=   'SFU3040I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      SFU3040I.TOKSOLIB,PARA-(?BUMON,?TANCD)

    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4  :=  '【社内振替発注入力】'
              GOTO ABEND
         END
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC


ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '社内振替発注入力が異常終了'
    ?KEKA2 :=  'しました。ログ採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
  /.?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM ./
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