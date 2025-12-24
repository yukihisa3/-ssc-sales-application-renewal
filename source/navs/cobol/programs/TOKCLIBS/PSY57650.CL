/. ***********************************************************  ./
/. *   サカタのタネ　基幹システム（本社システム）            *  ./
/. *   SYSTEM-NAME :    グッデイＥＤＩ　　　　　　　　　　   *  ./
/. *   JOB-NAME    :    出荷送信データ作成　　　　　　　　　 *  ./
/. *   JOB-ID      :    PSY57650                             *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSY57650'    /.ﾌﾟﾛｸﾞﾗﾑID./
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

    VAR ?CHK      ,STRING*1,VALUE-'0'           /.初期化FLG  ./

    VAR ?SYORINM1 ,STRING*40
    VAR ?SYORINM2 ,STRING*50
    VAR ?SYORINM3 ,STRING*50
    VAR ?SYORIKN1 ,STRING*7,VALUE-'0000000'     /.処理件数１ ./
    VAR ?SYORIKN2 ,STRING*7,VALUE-'0000000'     /.処理件数２ ./
    VAR ?SYORIKN3 ,STRING*7,VALUE-'0000000'     /.処理件数３ ./
    VAR ?SYORIKN4 ,STRING*7,VALUE-'0000000'     /.処理件数４ ./
    VAR ?SYORIKN5 ,STRING*7,VALUE-'0000000'     /.処理件数５ ./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'グッデイ出荷送信データ作成'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/ONLBLIB/TOKWLIB
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
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKO,?DSOKO)
    ?PGMEC    :=   @PGMEC
    ?PGMES    :=   @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '倉庫ＣＤ取得'
              GOTO ABEND
    END

/.##グッデイ出荷送信データ作成指示##./
SSY5765I:

    ?STEP :=   'SSY5765I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
  /.OVRF      FILE-GDJOHOL2,TOFILE-GDJOHOL2.TOKKLIB ./
  /.OVRF      FILE-GDSYUKL2,TOFILE-GDSYUKL2.TOKKLIB ./
  /.OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB     ./
  /.OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB   ./
    CALL      PGM-SSY5765I.TOKELIBO
             ,PARA-(?JDATE,?JTIME,?JTORI,?SOKO,?NDATE,?DSOKO)
    ?PGMEC    :=   @PGMEC
    ?PGMES    :=   @PGMES
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN PGMEC-?PGMEC
         ELSE
              ?KEKA4 :=  '出荷送信データ作成指示'
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

/.##出荷送信ﾌｧｲﾙ初期化判定##./
SSY5795B:

    ?STEP :=   'SSY5795B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF      FILE-PCCHK5,TOFILE-PCCHK5.ONLBLIB./
    CALL      PGM-SSY5795B.TOKELIBO,PARA-(?CHK)
    ?PGMEC    :=   @PGMEC
    ?PGMES    :=   @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷送信ファイル初期化判定'
              GOTO ABEND END
    /.##判定##./
    ?MSGX :=  '## 初期化FLG判定 = ' && ?CHK && ' ##'
    SNDMSG    ?MSGX,TO-XCTL
    IF        ?CHK = '1'  THEN
               SNDMSG MSG-'##送信ﾌｧｲﾙ前回分初期化##',TO-XCTL
               CLRFILE GDSNDDT.ONLBLIB
               CLRFILE PCCHK5.ONLBLIB
    ELSE
               SNDMSG MSG-'##前回分データが未送信です##',TO-XCTL
               GOTO ABEND
    END

/.##出荷送信データ作成##./
SSY5763B:

    ?STEP :=   'SSY5763B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF      FILE-GDSYUKL2,TOFILE-GDSYUKL2.TOKKLIB  ./
  /.OVRF      FILE-GDJOHOL2,TOFILE-GDJOHOL2.TOKKLIB  ./
  /.OVRF      FILE-GDSNDHD,TOFILE-GDSNDHD.TOKWLIB    ./
  /.OVRF      FILE-GDSNDMS,TOFILE-GDSNDMS.TOKWLIB    ./
    CALL      PGM-SSY5763B.TOKELIBO,
                  PARA-(?JDATE,?JTIME,?JTORI,?SOKO,?NDATE)
    ?PGMEC    :=   @PGMEC
    ?PGMES    :=   @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷送信データ作成'
              GOTO ABEND
    END

/.##出荷送信データ出力##./
SSY5764B:

    ?STEP :=   'SSY5764B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF      FILE-GDSNDHD,TOFILE-GDSNDHD.TOKWLIB    ./
  /.OVRF      FILE-GDSNDMS,TOFILE-GDSNDMS.TOKWLIB    ./
  /.OVRF      FILE-GDSNDDT,TOFILE-GDSNDDT.ONLBLIB    ./
    CALL      PGM-SSY5764B.TOKELIBO,PARA-(?SYORIKN1)
    ?PGMEC    :=   @PGMEC
    ?PGMES    :=   @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '出荷送信データ出力'
              GOTO ABEND
    END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  'グッデイ出荷送信データ作成処理が'
    ?KEKA2 :=  '正常終了しました。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM1 := ?PGNM
    ?SYORINM2 := '正常終了しました。　　　　　　　　　　　　　　'
    ?SYORINM3 := '確定件数を確認して下さい。　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
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

    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM1 := ?PGNM
    ?SYORINM2 := '異常終了しました。'
    ?SYORINM3 := 'ログを採取しＮＡＶへ送信して下さい。'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.?PGMEC    :=    @PGMEC ./
  /.?PGMEM    :=    @PGMEM ./
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