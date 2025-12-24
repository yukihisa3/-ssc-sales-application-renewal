/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＤＣＭＪＡＰＡＮ　納品予定システム   *  ./
/. *   JOB-ID      :    PSY8862N                             *  ./
/. *   JOB-NAME    :    欠品明細書発行（発注単位区分）　　　 *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSY8862N'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?P1       ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?P2       ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?P3       ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?P4       ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?P5       ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?P6       ,STRING*8,VALUE-'00000000'    /.納品日./
    VAR ?P7       ,STRING*2,VALUE-'00'          /.倉庫(PRT)./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ＤＣＭＪＡＰＡＮ欠品明細書'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKDTLIB
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
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END
    ?P7  :=  ?P4
    ?MSGX    :=  '## PRT出力倉庫CD = ' && ?P7 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ＤＣＭＪＡＰＡＮ欠品明細書発行指示##./
SSY8862I:

    ?STEP :=   'SSY8862I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-DJJOHOL2,TOFILE-DNSYUKL2.TOKDTLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY8862I.TOKELIBO
             ,PARA-(?P1,?P2,?P3,?P4,?P6,?P5)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END
    ?MSGX    :=  '## 指定倉庫CD = ' && ?P4 && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ＤＣＭＪＡＰＡＮ欠品明細データ初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-KPSYUKF.TOKKLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'データ初期化'
              GOTO ABEND END

/.##ＤＣＭＪＡＰＡＮ欠品明細データ抽出##./
SSY8863B:

    ?STEP :=   'SSY8863B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-KPSYUKL1,TOFILE-KPSYUKL1.TOKKLIB
    OVRF      FILE-DJSYUKL3,TOFILE-DNSYUKL3.TOKDTLIB
    CALL      PGM-SSY8863B.TOKELIBO,PARA-(?P1,?P2,?P3,?P4,?P6)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'データ抽出'
              GOTO ABEND END


/.##ＤＣＭＪＡＰＡＮ欠品明細書発行##./
SSY8866B:

    ?STEP :=   'SSY8866B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRPRTF   FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    CASE  ?P7  OF
       # 'TU' #
       ?MSGX :=  '##　ＡＷ札幌配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTTU.TOKELIBO,MEDLIB-TOKELIBO
       # '6A' #
       ?MSGX :=  '##　片岡配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT6A.TOKELIBO,MEDLIB-TOKELIBO
       # '63' #
       ?MSGX :=  '##　フバサミ配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT63.TOKELIBO,MEDLIB-TOKELIBO
       # '84' #
       ?MSGX :=  '##　鴻巣配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT84.TOKELIBO,MEDLIB-TOKELIBO
       # 'TV' #
       ?MSGX :=  '##　西尾配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTTV.TOKELIBO,MEDLIB-TOKELIBO
       # 'T5' #
       ?MSGX :=  '##　ＫＡ岡山配送センター出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTT5.TOKELIBO,MEDLIB-TOKELIBO
       # 'E1' #
       ?MSGX :=  '##　甘木配送センター　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRTE1.TOKELIBO,MEDLIB-TOKELIBO
       # '01' #
       ?MSGX :=  '##　本社　　　　　　　　出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT01.TOKELIBO,MEDLIB-TOKELIBO
    ELSE
       ?MSGX :=  '##　出力判定不能　本社へ出力　##'
       SNDMSG    ?MSGX,TO-XCTL
       OVRPRTF FILE-XU04LP,TOFILE-LBPPRT01.TOKELIBO,MEDLIB-TOKELIBO
    END
    OVRF      FILE-KPSYUKL3,TOFILE-KPSYUKL3.TOKKLIB
    CALL      PGM-SSY8867L.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '欠品明細書発行'
              GOTO ABEND END


RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '欠品明細書の発行が可能です。'
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