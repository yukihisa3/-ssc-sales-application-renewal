/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    Ｊ本田オンラインシステム             *  ./
/. *   JOB-ID      :    PSY71050                             *  ./
/. *   JOB-NAME    :    Ｊ本田入荷予定データ作成             *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSY71050'    /.ﾌﾟﾛｸﾞﾗﾑID./
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
    ?PGNM := 'Ｊ本田入荷予定データ抽出'
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

/.##Ｊ本田入荷予定データ作成指示##./
SSY7105I:

    ?STEP :=   'SSY7105I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-HDJOHOL2,TOFILE-HDJOHOL2.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY7105I.TOKELIB,PARA-(?P1,?P2,?P3,?P4,?P6,?P5)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4 :=  '抽出条件入力'
              GOTO ABEND
         END
    END

/.##Ｊ本田出荷確定データ抽出##./
SSY7106B:

    ?STEP :=   'SSY7106B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HDSYUKL4,TOFILE-HDSYUKL4.TOKFLIB
    OVRF      FILE-HDJOHOL1,TOFILE-HDJOHOL1.TOKFLIB
    OVRF      FILE-HDHEADL1,TOFILE-HDHEADL1.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-SSY7106B.TOKELIB,PARA-(?P1,?P2,?P3,?P4,?P6)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'データ抽出'
              GOTO ABEND END

/.##Ｊ本田入荷予定データ作成##./
SSY7107B:

    ?STEP :=   'SSY7107B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HDHEADL1,TOFILE-HDHEADL1.TOKFLIB
    OVRF      FILE-HDNYKYSF,TOFILE-HDNYKYSF.TOKFLIB
    CALL      PGM-SSY7107B.TOKELIB,PARA-(?P1,?P2,?P3,?P4,?P6)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'データ作成'
              GOTO ABEND END

/.##１２８→２５６変換##./
CVCS2561:

    ?STEP :=   'CVCS2561'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CVCS256,TOFILE-HDSND256.ONLBLIB
    OVRF      FILE-CVCS128,TOFILE-HDNYKYSF.TOKFLIB
    CALL      PGM-CVCS2561.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '１２８→２５６変換'
              GOTO ABEND END

/.##新ＥＤＩ（ＢＭＳファイルへセット）##./
PCNVFILE:

    ?STEP :=   'PCNVFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-HDSND256.ONLBLIB,TOFILE-NYKJHOND.ONLBLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '新ＥＤＩ（ＢＭＳファイルへセット）'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
/.  ?KEKA2 :=  '確定リストを発行して下さい。'   ./
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


ABEND:

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