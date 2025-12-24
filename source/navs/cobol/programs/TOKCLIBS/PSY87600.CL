/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＤＣＭＪＡＰＡＮ　オンラインシステム *  ./
/. *   JOB-ID      :    PSY87600                             *  ./
/. *   JOB-NAME    :    カーマ出荷リスト出力                 *  ./
/. ***********************************************************  ./
    PGM (P1-?CHK)
/.###ﾊﾟﾗﾒﾀｴﾘｱ定義####./
    PARA ?CHK     ,STRING*1,IN,VALUE-' '        /.起動FLG./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY87600'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*5,VALUE-'00000'       /.店舗(開始)./
    VAR ?P7       ,STRING*5,VALUE-'00000'       /.店舗(終了)./
    VAR ?P8       ,STRING*8,VALUE-'00000000'    /.納品日(開始)./
    VAR ?P9       ,STRING*8,VALUE-'00000000'    /.納品日(終了)./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'カーマ出荷リスト出力'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P4,?P5)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##カーマ出荷リスト発行指示##./
SSY8760I:

    ?STEP :=   'SSY8760I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
/.##本社ﾚｰｻﾞｰへ出力##./
    IF ?CHK = '1' THEN
       SNDMSG MSG-'##本社出力##',TO-XCTL.@ORGPROF,JLOG-@YES
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    END
/.##ｶﾄｰﾚｯｸﾚｰｻﾞｰへ出力##./
    IF ?CHK = '2' THEN
       SNDMSG MSG-'##カト出力##',TO-XCTL.@ORGPROF,JLOG-@YES
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRK.XUCL
    END
/.##片岡ﾚｰｻﾞｰへ出力##./
    IF ?CHK = '3' THEN
       SNDMSG MSG-'##片岡出力##',TO-XCTL.@ORGPROF,JLOG-@YES
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPTK.XUCL
    END
/.##ｽﾄｯｸﾌｫｰﾑへ出力##./
    IF ?CHK = '4' THEN
       SNDMSG MSG-'##スト出力##',TO-XCTL.@ORGPROF,JLOG-@YES
       OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIBO
    END
/.##西尾ﾚｰｻﾞｰへ出力##./
    IF ?CHK = '5' THEN
       SNDMSG MSG-'##西尾出力##',TO-XCTL.@ORGPROF,JLOG-@YES
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRN.XUCL
    END
/.##甘木ﾚｰｻﾞｰへ出力##./
    IF ?CHK = '6' THEN
       SNDMSG MSG-'##甘木出力##',TO-XCTL.@ORGPROF,JLOG-@YES
       OVRPRTF FILE-XU04LP,TOFILE-AMAGILBP.XUCL
    END
/.##ﾌﾊﾞｻﾐﾚｰｻﾞｰへ出力##./
    IF ?CHK = '7' THEN
       SNDMSG MSG-'##フバサミ出力##',TO-XCTL.@ORGPROF,JLOG-@YES
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRF.XUCL
    END
    OVRF      FILE-DJSYUKL1,TOFILE-DJSYUKL1.TOKKLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY8760I.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P5,?P6,?P7
                                         ,?P8,?P9)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END

/.##カーマ出荷リスト発行##./
SSY8741L:

    ?STEP :=   'SSY8761L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DJSYUKL7,TOFILE-DJSYUKL7.TOKKLIB
    CALL      PGM-SSY8761L.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P6,?P7
                                         ,?P8,?P9)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷リスト出力'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '出荷リスト出力が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '出荷リスト出力が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
