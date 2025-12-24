/. ***********************************************************  ./
/. *     サカタのタネ　　　　　　　　　　　　　　　          *  ./
/. *   SYSTEM-NAME :    ヨドバシ　　　ＥＤＩ　　　　　　　   *  ./
/. *   JOB-ID      :    PSY95500                             *  ./
/. *   JOB-NAME    :    ヨドバシ出荷確定取消処理　　　　　　 *  ./
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
    VAR ?PGMID    ,STRING*8,VALUE-'PSY95500'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8,VALUE-'        '    /.STEP-ID./
    VAR ?BUMON    ,STRING*4,VALUE-'    '        /.部門CD./
    VAR ?TANCD8   ,STRING*8,VALUE-'        '    /.担当者CD./
    VAR ?TANCD    ,STRING*2,VALUE-'  '          /.担当者CD./

    VAR ?TAISYO   ,STRING*1,VALUE-'1'           /.取消対象  ./
    VAR ?JDATE    ,STRING*8,VALUE-'00000000'    /.受信日付  ./
    VAR ?JTIME    ,STRING*4,VALUE-'0000'        /.受信時間  ./
    VAR ?JTORI    ,STRING*8,VALUE-'00000000'    /.受信取引先./
    VAR ?SOKCD    ,STRING*2,VALUE-'00'          /.倉庫(自)  ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'00'          /.倉庫(代表)./
    VAR ?NDATE    ,STRING*8,VALUE-'00000000'    /.納品日./
    VAR ?DENNO    ,STRING*90                    /.伝票番号./

    VAR ?JIKKBN   ,STRING*1,VALUE-'1'           /.実行区分./
    VAR ?JIKDATE  ,STRING*8,VALUE-'00000000'    /.実行日付./
    VAR ?JIKTIME  ,STRING*6,VALUE-'000000'      /.実行時刻./
    VAR ?JIKDATEF ,STRING*8,VALUE-'00000000'    /.実行日付./
    VAR ?JIKDATET ,STRING*8,VALUE-'00000000'    /.実行日付./
    VAR ?JIKTIMEF ,STRING*6,VALUE-'000000'      /.実行時刻./
    VAR ?JIKTIMET ,STRING*6,VALUE-'000000'      /.実行時刻./
    VAR ?KENSU    ,STRING*7,VALUE-'0000000'     /.作成件数./
    VAR ?SYORINM1 ,STRING*40
    VAR ?SYORINM2 ,STRING*50
    VAR ?SYORINM3 ,STRING*50
    VAR ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR ?SYORIKN5 ,STRING*7,VALUE-'0000000'
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ヨドバシ出荷確定取消処理'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKSOLIB/TOKFLIB/TOKKLIB/TOKDTLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.## ログインユーザー情報取得 ##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ログインユーザー情報取得'
              GOTO ABEND
    END
    ?TANCD := ?TANCD8

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4 :=  'ＷＳ－倉庫取得'
              GOTO ABEND
    END

/.##ヨドバシ出荷確定取消処理指示##./
SSY9550I:

    ?STEP :=  'SSY9550I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?DENNO := '000000000' && '000000000' && '000000000' &&
              '000000000' && '000000000' && '000000000' &&
              '000000000' && '000000000' && '000000000' &&
              '000000000'

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKMDLIB
    OVRF      FILE-YODJOHL4,TOFILE-YODJOHL4.TOKDTLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    CALL      PGM-SSY9550I.TOKSOLIB,
              PARA-(?DSOKCD,?TAISYO,?JDATE,?JTIME,?JTORI,?SOKCD,
                    ?NDATE,?DENNO)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  'ヨドバシ出荷確定取消処理指示'
              GOTO ABEND
         END
    END

    ?MSGX :=  '取消対象　　＝' && ?TAISYO
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'バッチ（日）＝' && ?JDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'バッチ（時）＝' && ?JTIME
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'バッチ（取）＝' && ?JTORI
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '倉庫　　　　＝' && ?SOKCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '納品日　　　＝' && ?NDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票番号１　＝' && %SBSTR(?DENNO,1,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票番号２　＝' && %SBSTR(?DENNO,10,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票番号３　＝' && %SBSTR(?DENNO,19,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票番号４　＝' && %SBSTR(?DENNO,28,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票番号５　＝' && %SBSTR(?DENNO,37,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票番号６　＝' && %SBSTR(?DENNO,46,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票番号７　＝' && %SBSTR(?DENNO,55,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票番号８　＝' && %SBSTR(?DENNO,64,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票番号９　＝' && %SBSTR(?DENNO,73,9)
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票番号１０＝' && %SBSTR(?DENNO,82,9)
    SNDMSG    ?MSGX,TO-XCTL

/.##ヨドバシ出荷確定取消処理##./
SSY9551B:

    ?STEP :=   'SSY9551B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-YODSNDL5,TOFILE-YODSNDL5.TOKDTLIB
    OVRF      FILE-YODJOHL4,TOFILE-YODJOHL4.TOKDTLIB
    CALL      PGM-SSY9551B.TOKSOLIB,
              PARA-(?JDATE,?JTIME,?JTORI,?SOKCD,?NDATE,
                    ?DENNO,?TAISYO)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ヨドバシ出荷確定取消処理'
              GOTO ABEND
    END

RTN:
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '処理結果表示'
              GOTO ABEND
    END

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-?PGMEC

ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
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

    RETURN    PGMEC-?PGMEC
