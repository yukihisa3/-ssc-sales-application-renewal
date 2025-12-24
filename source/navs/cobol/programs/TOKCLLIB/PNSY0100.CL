/. ***********************************************************  ./
/. *     サカタのタネ　ＮＡＶＳ基幹システム　　　　　        *  ./
/. *   SYSTEM-NAME :    ＤＣＭ仕入先統合　納品予定システム   *  ./
/. *   JOB-ID      :    PNSY0100                             *  ./
/. *   JOB-NAME    :    納品予定データ作成　　　　　　　　　 *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PNSY0100'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*8,VALUE-'00000000'    /.納品日./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '新ＤＣＭ納品予定データ抽出'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKDTLIB/TOKKLIB
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

/.##ＤＣＭ納品予定データ抽出指示##./
NSY0100I:

    ?STEP :=   'SSY8721I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-DNJOHOL2,TOFILE-DNJOHOL2.TOKDTLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-NSY0100I.TOKSOLIB
             ,PARA-(?P1,?P2,?P3,?P4,?P6,?P5)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END
    ?MSGX :=  '#バッチ日付   = ' && ?P1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '#バッチ時刻   = ' && ?P2
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '#バッチ取引先 = ' && ?P3
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '倉庫ＣＤ      = ' && ?P4
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '代表倉庫      = ' && ?P5
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '納品日        = ' && ?P6
    SNDMSG    ?MSGX,TO-XCTL

/.##ＤＣＭ納品予定データ削除##./
NSY0100B:

    ?STEP :=   'NSY0110B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DNSYUKL1,TOFILE-DNSYUKL1.TOKDTLIB
    CALL      PGM-NSY0110B.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P6)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'データ削除'
              GOTO ABEND END

/.##新ＤＣＭ納品予定データ抽出##./
NSY0120B:

    ?STEP :=   'NSY0120B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENL1,TOFILE-SHTDENL1.TOKFLIB
    OVRF      FILE-DNJOHOL3,TOFILE-DNJOHOL3.TOKDTLIB
    OVRF      FILE-DNSYUKL3,TOFILE-DNSYUKL3.TOKDTLIB
    CALL      PGM-NSY0120B.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P6)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'データ抽出'
              GOTO ABEND END

/.##納品予定データ送信Ｆ作成##./
SSY8705B:

    ?STEP :=   'SSY8705B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DNSYUKL3,TOFILE-DNSYUKL3.TOKDTLIB
    OVRF      FILE-DNJOHOL1,TOFILE-DNJOHOL1.TOKDTLIB
     IF      ?P3 = '00000880' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00000882' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00000883' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00013938' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00139381' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00100403' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00001427' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSK.TOKKLIB
     END
     IF      ?P3 = '00014272' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSK.TOKKLIB
     END
     IF      ?P3 = '00014273' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSK.TOKKLIB
     END
     IF      ?P3 = '00017137' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSK.TOKKLIB
     END
     IF      ?P3 = '00171371' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSK.TOKKLIB
     END
     IF      ?P3 = '01004031' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSK.TOKKLIB
     END
     IF      ?P3 = '00100441' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00100427' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSK.TOKKLIB
     END
     IF      ?P3 = '00100404' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00100442' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00100428' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSK.TOKKLIB
     END
     IF      ?P3 = '00001731' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00001732' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSZ.TOKKLIB
     END
     IF      ?P3 = '00007601' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSK.TOKKLIB
     END
     IF      ?P3 = '00007602' THEN
              OVRF  FILE-SNDDCMJP,TOFILE-SNDDCMSK.TOKKLIB
     END
    CALL      PGM-NSY0130B.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P6)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '送信データ作成'
              GOTO ABEND END

/.##ＤＣＭＪＡＰＡＮ欠品データ抽出##./
SSY0806B:

    ?STEP :=   'SSY0806B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-DJJOHOL1,TOFILE-DNJOHOL1.TOKDTLIB
    OVRF      FILE-ONLKEPL1,TOFILE-ONLKEPL1.TOKDTLIB
    CALL      PGM-SSY0806B.TOKSOLIB,PARA-(?P1,?P2,?P3,?P4,?P6)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＤＣＭＪＡＰＡＮ欠品データ抽出'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '確認後、送信処理を開始して下さい。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
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