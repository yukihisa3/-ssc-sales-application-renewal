/. ***********************************************************  ./
/. *   サカタのタネ　基幹システム（本社システム）            *  ./
/. *   SYSTEM-NAME :    グッデイＥＤＩ　　　　　　　　　　   *  ./
/. *   JOB-NAME    :    出荷送信データ作成リカバリ処理       *  ./
/. *   JOB-ID      :    PSY57960                             *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMES    ,STRING*5,VALUE-'     '       /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY57960'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?JDATE    ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?JTIME    ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?JTORI    ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?SOKO     ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?DSOKO    ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?NDATE    ,STRING*8,VALUE-'00000000'    /.納品日./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'グッデイ出荷送信データ作成リカバリ'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB
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

  /.OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB./
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKO,?DSOKO)
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '倉庫ＣＤ取得'
              GOTO ABEND
    END

/.##出荷送信データリカバリ指示##./
SSY5796I:

    ?STEP :=   'SSY5796I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
  /.OVRF      FILE-GDJOHOL2,TOFILE-GDJOHOL2.TOKKLIB ./
  /.OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB     ./
  /.OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB   ./
    CALL      PGM-SSY5796I.TOKELIBO,
                  PARA-(?JDATE,?JTIME,?JTORI,?SOKO,?NDATE,?DSOKO)
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN PGMEC-?PGMEC
         ELSE
              ?KEKA4 :=  '出荷送信データリカバリ指示'
              GOTO ABEND
         END
    END

/.##ﾊﾟﾗﾒﾀ内容を表示する##./
    ?MSGX :=  '*受信日     = '  && ?JDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*受信時刻   = '  && ?JTIME
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*取引先　   = '  && ?JTORI
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*自倉庫CD   = '  && ?SOKO
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*代表倉庫CD = '  && ?DSOKO
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*納品日     = '  && ?NDATE
    SNDMSG    ?MSGX,TO-XCTL

/.##基本情報ファイル送信済FLG解除##./
SSY5796B:

    ?STEP :=   'SSY7891B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF      FILE-GDJOHOL2,TOFILE-GDJOHOL2.TOKKLIB ./
    CALL      PGM-SSY5796B.TOKELIBO,
                  PARA-(?JDATE,?JTIME,?JTORI,?SOKO,?NDATE)
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'フラグ解除（基本情報）'
              GOTO ABEND
    END

/.##出荷情報ファイル送信済FLG解除##./
SSY5797B:

    ?STEP :=   'SSY5797B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF      FILE-GDSYUKL2,TOFILE-GDSYUKL2.TOKKLIB ./
    CALL      PGM-SSY5797B.TOKELIBO,
                  PARA-(?JDATE,?JTIME,?JTORI,?SOKO,?NDATE)
    ?PGMEC    :=    @PGMEC
    ?PGMES    :=    @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  'フラグ解除（出荷情報）'
              GOTO ABEND
    END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '対象データの出荷確定を実施願います。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
  /.?PGMEC    :=    @PGMEC./
  /.?PGMEM    :=    @PGMEM./
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