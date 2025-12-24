/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷サブシステム 　       　　　     *  ./
/. *   JOB-ID      :    PSV01000                             *  ./
/. *   JOB-NAME    :    オンラインＤＴＣＳＶデータ作成　　   *  ./
/. *               :    品単ＣＤ追加　Ｖｅｒ                 *  ./
/. *   UPDATE      :    2012/05/07  MIURA                    *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PSV01000'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
/.###PGﾊﾟﾗﾒﾀｴﾘｱ###./
    VAR ?BATNO    ,STRING*20,VALUE-'                    '
    VAR ?SOKOCD   ,STRING*4,VALUE-'    '
    VAR ?HIDUKE   ,STRING*8,VALUE-'        '
    VAR ?JIKAN    ,STRING*4,VALUE-'    '
    VAR ?TOKCD    ,STRING*8,VALUE-'        '
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.      2    ./
    VAR ?OPR3     ,STRING*50                    /.      3    ./
    VAR ?OPR4     ,STRING*50                    /.      4    ./
    VAR ?OPR5     ,STRING*50                    /.      5    ./
/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '発注集計データ（店舗横並一覧）作成'
/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB
/.##資源の獲得##./
ASS:
     ASSIGN FILE-HACYUPPT.TOKKLIB!@XCL
     IF     @PGMEC ^=  0  THEN
       ?OPR1 := '　＃＃　発注集計表ＣＳＶ出力確認　使用状態　＃＃　'
       ?OPR2 := '　＃他の端末にて資源が使用されております。　　＃　'
       ?OPR3 := '　＃確認して下さい。（暫くお待ち下さい。）　　＃　'
       ?OPR4 := '　＃続行＝ＥＮＴＥＲ　終了＝ＰＦ９　　　　　　＃　'
       ?OPR5 := '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
       CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
       GOTO   ASS     END

/.###発注集計表（店舗横並一覧）出力###./
PSV01000:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB

    CALL      PGM-CSV0050I.TOKELIB,PARA-(?BATNO,?SOKOCD,
                                         ?HIDUKE,?JIKAN,?TOKCD)
    ?PGMEC := @PGMEC
    IF        ?PGMEC    ^=   0    THEN
              IF ?PGMEC  =   4001 THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF
                  GOTO   RTN
              ELSE
                  ?KEKA4 :=  '【範囲指定入力】'
                  GOTO   ABEND
              END
    END

    ?MSGX :=  '##日付     = ' && ?HIDUKE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##時間     = ' && ?JIKAN
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##取引先CD = ' && ?TOKCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##倉庫ｺｰﾄﾞ = ' && ?SOKOCD
    SNDMSG    ?MSGX,TO-XCTL

/.###発注集計データ抽出###./
CSV0100B:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-HACYUPPT,TOFILE-HACYUPPT.TOKKLIB

    CALL      PGM-CSV0100B.TOKELIBO,PARA-(?BATNO,?SOKOCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【対象データ抽出】'
              GOTO ABEND END

/.###ファイル初期化１##./
PCLRFIL1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SEQTENF.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【店舗情報データ初期化】'
              GOTO ABEND END

/.###ファイル初期化２##./
PCLRFIL2:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-CSSTBLF.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【発注集計店舗横並初期化】'
              GOTO ABEND END

/.###店舗情報データ抜出##./
CSV0110B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-SEQTENL1,TOFILE-SEQTENL1.TOKKLIB
    CALL      PGM-CSV0110B.TOKELIBO,PARA-(?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【発注集計データより店舗情報抜出】'
              GOTO ABEND END

/.###発注集計データ店舗テーブルセット##./
CSV0120B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CSSTBLL1,TOFILE-CSSTBLL1.TOKKLIB
    OVRF      FILE-HACYUPPT,TOFILE-HACYUPPT.TOKKLIB
    CALL      PGM-CSV0120B.TOKELIBO,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【発注集計店舗横展開データ作成】'
              GOTO ABEND END

/.###ＣＳＶ形式力##./
CSV0130B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-CSSTBLL1,TOFILE-CSSTBLL1.TOKKLIB
    OVRF      FILE-SEQTENL1,TOFILE-SEQTENL1.TOKKLIB
    OVRF      FILE-HACTECSV,TOFILE-HACTECSV.TOKKLIB
    CALL      PGM-CSV0130B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ＣＳＶ形式データ作成】'
              GOTO ABEND END

/.###テキスト出力##./
PFIMPORT:
    /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-HACTECSV.TOKKLIB,PARA-HACTECSV,UNIT-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ＰＣへデータ転送】'
              GOTO ABEND END

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  'ＰＣ上で確認願います。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
