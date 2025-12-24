/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＤＣＭ仕入先統合　　　　　　　　　   *  ./
/. *   JOB-ID      :    PNSY0500                             *  ./
/. *   JOB-NAME    :    荷個数明細書発行（納品日範囲）       *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PNSY0500'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*8,VALUE-'00000000'    /.納品日（開始）./
    VAR ?P7       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P8       ,STRING*8,VALUE-'00000000'    /.納品日（終了）./
    VAR ?P9       ,STRING*5,VALUE-'00000'       /.店舗（開始）./
    VAR ?P10      ,STRING*5,VALUE-'00000'       /.店舗（終了）./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '荷個数明細書発行'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKMDLIB/ZAIEXCEL/TOKFLIB/TOKKLIB
           /TOKDTLIB
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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?P7,?P5)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##荷個数明細書範囲指定##./
NSY0500I:

    ?STEP :=   'SSY9205I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-NSY0500I.TOKSOLIB,PARA-(?P7,?P5,?P1,?P2,?P3,?P4
                                         ,?P6,?P8,?P9,?P10)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END
    ?MSGX :=  '*受信日　＝　'  &&  ?P1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*受信時間＝　'  &&  ?P2
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*取引先　＝　'  &&  ?P3
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*倉庫　　＝　'  &&  ?P4
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*納品日開＝　'  &&  ?P6
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*納品日終＝　'  &&  ?P8
    SNDMSG    ?MSGX,TO-XCTL

/.##荷個数明細書##./
NSY0510L:

    ?STEP :=   'NSY0510L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DNCENTL1,TOFILE-DNCENTL1.TOKDTLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CASE  ?P7  OF
       # 'TU' #
          ?MSGX :=  '##　ＡＷ札幌配送センター出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHAL,
          OUTQ-XXPAWSQL,MEDLIB-TOKMDLIB
       # 'TK' #
          ?MSGX :=  '##　さくら配送センター　出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHSL,
          OUTQ-XXPSKRQL,MEDLIB-TOKMDLIB
       # '63' #
          ?MSGX :=  '##　フバサミ配送センター出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTH6F,
          OUTQ-XXPFUBQL,MEDLIB-TOKMDLIB
       # '84' #
          ?MSGX :=  '##　鴻巣配送センター　　出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHKO,
          OUTQ-XXKOUSQL,MEDLIB-TOKMDLIB
       # 'TV' #
          ?MSGX :=  '##　西尾配送センター　　出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHNO,
          OUTQ-XXNISSQL,MEDLIB-TOKMDLIB
       # 'T5' #
          ?MSGX :=  '##　ＫＡ岡山配送センター出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHTL,
          OUTQ-XXPKATQL,MEDLIB-TOKMDLIB
       # 'E1' #
          ?MSGX :=  '##　甘木配送センター　　出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHKL,
          OUTQ-XXPAMGQL,MEDLIB-TOKMDLIB
       # 'T4' #
          ?MSGX :=  '##　花の海配送センター１出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHHL,
          OUTQ-XXPHANQL,MEDLIB-TOKMDLIB
       # 'S4' #
          ?MSGX :=  '##　花の海配送センター２出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-PRINTHHL,
          OUTQ-XXPHANQL,MEDLIB-TOKMDLIB
       # '01' #
          ?MSGX :=  '##　本社　　　　　　　　出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-SYSPRT02,
          OUTQ-XOUTQ1,MEDLIB-TOKMDLIB
    ELSE
          ?MSGX :=  '##　出力判定不能　本社へ出力　##'
          SNDMSG    ?MSGX,TO-XCTL
          OVRPRTF FILE-PRTF,TOFILE-PRTF.XUCL,DEV-SYSPRT02,
          OUTQ-XOUTQ1,MEDLIB-TOKELIBO
    END
    CALL      PGM-NSY0510L.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P6,?P8,
                                          ?P9,?P10)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '荷個数明細書'
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