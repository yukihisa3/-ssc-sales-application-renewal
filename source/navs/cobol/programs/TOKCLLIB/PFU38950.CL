/. ***TEST****************************************************  ./
/. *   サカタのタネ　                                        *  ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム                     *  ./
/. *   JOB-ID      :    PFU38950                             *  ./
/. *   JOB-NAME    :    ＡＣＯＳ振替累積データ　消し込み     *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PFU38950'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*1,VALUE-' '           /.店舗ﾏｽﾀ  ./
    VAR ?P2       ,STRING*1,VALUE-' '            /.商品ﾏｽﾀ  ./
    VAR ?P3       ,STRING*1,VALUE-' '           /.ﾛｹﾏｽﾀ./
    VAR ?P4       ,STRING*1,VALUE-' '            /.検品ｸﾞﾙｰﾌﾟﾏｽﾀ./
    VAR ?P5       ,STRING*1,VALUE-' '            /.倉庫別出荷取引先./
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
    VAR ?TANTO    ,STRING*8,VALUE-'        '     /.担当者CD./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNM :=  'ＡＣＯＳ振替累積データ　消し込み'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG   MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKDTLIB/TOKKLIB/TOKDLIB
             /ZAIEXCEL

/.##部門ｺｰﾄﾞ担当者ｺｰﾄﾞ取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANTO)
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

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

/.##実行確認画面##./
SFU38950:

    ?STEP :=   'SFU38950'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      SFU3895I.TOKSOLIB,PARA-(?DELDATE)

    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4  :=  '【実行確認】'
              GOTO ABEND
         END
    END

/.##消し込み前データ退避##./
STEP020:

    ?STEP :=   'STEP020 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    DLTFILE   FILE-FRACOSFB.BAKFLIB

    CPYFILE   FILE-FRACOSF.TOKDTLIB,
              TOFILE-FRACOSFB.BAKFLIB,
              CRTFILE-@YES

    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        ?KEKA4 := '【消込前データ退避】'
        GOTO  ABEND
    END

/.##ＡＣＯＳ振替累積データ消込##./
SFU3896B:

    ?STEP :=   'SFU3896B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SFU3896B.TOKSOLIB,PARA-(?DELDATE)
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【ＡＣＯＳ振替累積データ消込】'
              GOTO ABEND
    END

RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ＡＣＯＳ振替累積データ消込処理'
    ?KEKA2 :=  ''
    ?KEKA3 :=  '　　　正常終了しました'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC


ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ＡＣＯＳ振替累積データ消込処理が異常終'
    ?KEKA2 :=  '了しました。ログ採取し，ＮＡＶへ連絡し'
    ?KEKA3 :=  'て下さい。'
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