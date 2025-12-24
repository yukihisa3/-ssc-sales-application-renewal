/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    カーマオンラインシステム             *  ./
/. *   JOB-ID      :    PSY91140                             *  ./
/. *   JOB-NAME    :    欠品情報報告書発行　　　　           *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY91140'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*5,VALUE-'00000'       /.センターＣＤ./
    VAR ?P7       ,STRING*5,VALUE-'00000'       /.センターＣＤ./
    VAR ?P8       ,STRING*2,VALUE-'00'          /.倉庫(保存)./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '欠品情報報告書発行（カーマ）'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKDLIB/TOKELIBO
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
    ?P8    :=   ?P4

/.##欠品ﾌｧｲﾙ初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-KMKEPNF.TOKDLIB
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  '欠品集計初期化'
              GOTO ABEND
    END

/.##ダイキ　欠品情報報告書範囲指定##./
SSY7956I:

    ?STEP :=   'SSY7956I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF      FILE-KMKEPNL1,TOFILE-KMKEPNL1.TOKDLIB
    CALL      PGM-SSY9114I.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P5,?P6,?P7)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END
    ?MSGX :=  '##受信日       = ' &&  ?P1  &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##受信時刻     = ' &&  ?P2  &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##受信取引先   = ' &&  ?P3  &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##倉庫ＣＤ     = ' &&  ?P4  &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##代表倉庫ＣＤ = ' &&  ?P5  &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##センター開始 = ' &&  ?P6  &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##センター終了 = ' &&  ?P7  &&  ' ##'
    SNDMSG    ?MSGX,TO-XCTL

/.##欠品情報報告書##./
SSY7957L:

    ?STEP :=   'SSY7957L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KMKEPNL1,TOFILE-KMKEPNL1.TOKDLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CASE  ?P8  OF
       # 'TU' #
          ?MSGX :=  '##　ＡＷ札幌配送センター出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHAL,
          OUTQ-XXPAWSQL,MEDLIB-TOKELIBO
       # '6A' #
          ?MSGX :=  '##　片岡配送センター　　出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTX6A,
          OUTQ-XXPSKRQL,MEDLIB-TOKELIBO
       # '63' #
          ?MSGX :=  '##　フバサミ配送センター出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTH6F,
          OUTQ-XXPFUBQL,MEDLIB-TOKELIBO
       # '84' #
          ?MSGX :=  '##　鴻巣配送センター　　出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHKO,
          OUTQ-XXKOUSQL,MEDLIB-TOKELIBO
       # 'TV' #
          ?MSGX :=  '##　西尾配送センター　　出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHNO,
          OUTQ-XXNISSQL,MEDLIB-TOKELIBO
       # 'T5' #
          ?MSGX :=  '##　ＫＡ岡山配送センター出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHTL,
          OUTQ-XXPSKRQL,MEDLIB-TOKELIBO
       # 'E1' #
          ?MSGX :=  '##　甘木配送センター　　出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHKL,
          OUTQ-XXPAMGQL,MEDLIB-TOKELIBO
       # 'T4' #
          ?MSGX :=  '##　花の海配送センター１出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHHL,
          OUTQ-XXPHANQL,MEDLIB-TOKELIBO
       # 'S4' #
          ?MSGX :=  '##　花の海配送センター２出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHHL,
          OUTQ-XXPHANQL,MEDLIB-TOKELIBO
       # '01' #
          ?MSGX :=  '##　本社　　　　　　　　出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-SYSPRT02,
          OUTQ-XOUTQ1,MEDLIB-TOKELIBO
    ELSE
          ?MSGX :=  '##　出力判定不能　本社へ出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-SYSPRT02,
          OUTQ-XOUTQ1,MEDLIB-TOKELIBO
    END

    CALL      PGM-SSY9115L.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P6,?P7)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '欠品情報報告書'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
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