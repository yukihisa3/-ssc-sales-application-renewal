/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    ACOSPG3                           　 *  ./
/. *   JOB-NAME    :    日次計上エラー戻し処理＋　　　　　   *  ./
/. *                    部門間在庫移動処理　　　　　　　　   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'ACOSPG3'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./
    VAR       ?SYSDATE,STRING*13                  /.ｼｽﾃﾑ情報取得./
    VAR       ?DATE   ,STRING*6                   /.ｼｽﾃﾑ日付./
    VAR       ?YOUBI  ,STRING*1                   /.曜日./
    VAR       ?YOUBIN ,STRING*99                  /.曜日名./
    VAR       ?YY     ,STRING*2                   /.年./
    VAR       ?MM     ,STRING*2                   /.月./
    VAR       ?DD     ,STRING*2                   /.日./
    VAR       ?TIME1  ,STRING*2                   /.時刻./
    VAR       ?TIME2  ,STRING*2                   /.時刻./
    VAR       ?TIME3  ,STRING*2                   /.時刻./
    VAR       ?TIME   ,STRING*6                   /.時刻./
    VAR       ?ERRCHK1,STRING*2,VALUE-'  '        /.エラーＦＬＧ./
    VAR       ?ERRCHK2,STRING*2,VALUE-'  '        /.エラーＦＬＧ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＡＣＯＳ計上エラー戻し兼部門間在庫移動'

/.##ﾌﾟﾛｸﾞﾗﾑ開始MSG##./
    /.$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$./
    /.##システム情報取得##./
    ?SYSDATE  :=    @SCDATED               /.ｼｽﾃﾑ情報取得./
    ?DATE     :=    %SBSTR(?SYSDATE,1,6)   /.ｼｽﾃﾑ日付取得./
    ?YOUBI    :=    %SBSTR(?SYSDATE,13,1)  /.曜日取得./
    ?YY       :=    %SBSTR(?DATE,5,2)      /.年取得./
    ?MM       :=    %SBSTR(?DATE,3,2)      /.月取得./
    ?DD       :=    %SBSTR(?DATE,1,2)      /.日取得./
    ?TIME1    :=    %SBSTR(?SYSDATE,7,2)   /.時刻取得./
    ?TIME2    :=    %SBSTR(?SYSDATE,9,2)   /.時刻取得./
    ?TIME3    :=    %SBSTR(?SYSDATE,11,2)  /.時刻取得./
    ?TIME     :=    %SBSTR(?SYSDATE,7,6)   /.時刻取得./
    /.$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$./
/.##開始ＭＳＧ出力##./
    ?MSGX :=  '## '  && ?PGMID && ?PGNM && ' 開始'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX     := '## 開始日時:' && ?YY && '/' && ?MM && '/' && ?DD
               && ' ' && ?TIME1 && ':' && ?TIME2 && ':' && ?TIME3

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKEILBO/TOKWLIB

/.##処理確認画面　　　　　　　　　　　　　　　　　　　　　　　##./
STEP0010:

    ?STEP :=   'STEP0010'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && '処理確認画面'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?OPR1  :=  '　＃＃＃＃＃　日次更新エラー復元処理　＃＃＃＃＃　'
    ?OPR2  :=  '　　日次更新計上エラーデータの受信を行います。'
    ?OPR3  :=  '　　受信後、エラー戻しと部門間在庫移動ＤＴ作成'
    ?OPR4  :=  '　　を行ないます。必ず処理結果確認！！'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ＡＣＯＳ接続確立　　　　　　　　　　　　　　　　　　　　　##./
STEP0020:

    ?STEP :=   'STEP0020'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && 'ＡＣＯＳ接続確立'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNTFTPC RMTNAME-ACOSN,USER-'NEC23',ACCOUNT-'NEC'
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＡＣＯＳ接続確立異常 ##'
              ?KEKA4 := 'ＡＣＯＳ接続確立異常'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ接続確立成功 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.##ＡＣＯＳ受信待ち合せ　　　　　　　　　　　　　　　　　　　##./
STEP0030:

    ?STEP :=   'STEP0030'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && 'ＡＣＯＳ受信待ち合せ'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL TWAIT30.XUCL

/.##ＡＣＯＳ計上エラーＤＴ受信　　　　　　　　　　　　　　　　##./
STEP0040:

    ?STEP :=   'STEP0040'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && 'ＡＣＯＳ計上エラーＤＴ受信'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RCVFTPC FILE-'SSC.TERRF',TOENT-TOKUERR.TOKFLIB,DTYPE-@BINARY
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＡＣＯＳ計上エラーＤＴ受信異常 ##'
              ?KEKA4 := 'ＡＣＯＳ計上エラーＤＴ受信異常'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ計上エラーＤＴ受信成功 ##'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.##ＡＣＯＳ受信完了データ作成　　　　　　　　　　　　　　　　##./
STEP0050:

    ?STEP :=   'STEP0050'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && 'ＡＣＯＳ受信完了データ作成'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-ACOSCHK1,TOFILE-ACOSCHK2.TOKWLIB
    CALL      PGM-SKY3001B.TOKELIB
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＡＣＯＳ受信完了データ作成  異常 ##'
              ?KEKA4 := 'ＡＣＯＳ受信完了データ作成'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ受信完了データ作成　成功 ##'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.##ＡＣＯＳ完了Ｆ送信　　　　　　　　　　　　　　　　　　　　##./
STEP0060:

    ?STEP :=   'STEP0060'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && 'ＡＣＯＳ完了Ｆ送信'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    SNDFTPC ENT-ACOSCHK2.TOKWLIB,TOFILE-'SSC.STOKDM2',DTYPE-@BINARY
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＡＣＯＳ完了Ｆ送信異常 ##'
              ?KEKA4 := 'ＡＣＯＳ完了Ｆ送信異常'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ完了送信成功 ##'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.##ＡＣＯＳ接続切り離し　　　　　　　　　　　　　　　　　　　##./
STEP0070:

    ?STEP :=   'STEP0070'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && 'ＡＣＯＳ接続切り離し'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    DCNTFTPC
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＡＣＯＳ接続切断異常 ##'
              ?KEKA4 := 'ＡＣＯＳ接続切断異常'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ接続切断成功 ##'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.##エラーデータ振分　　　　　　　　　　　　　　　　　　　　　##./
STEP0080:

    ?STEP :=   'STEP0080'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && 'エラーデータ振分'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-TOKUERR,TOFILE-TOKUERR.TOKFLIB
    OVRF      FILE-TOK00,TOFILE-TOKUHOE.TOKFLIB
    OVRF      FILE-TOK01,TOFILE-TOKUFKE.TOKFLIB
    OVRF      FILE-TOK02,TOFILE-TOKUSEE.TOKFLIB
    OVRF      FILE-TOK03,TOFILE-TOK03.TOKFLIB
    OVRF      FILE-TOK04,TOFILE-TOKUOKE.TOKFLIB
    OVRF      FILE-TOK05,TOFILE-TOKUHKE.TOKFLIB
    OVRF      FILE-TOK06,TOFILE-TOK06.TOKFLIB
    OVRF      FILE-TOK07,TOFILE-TOK07.TOKFLIB
    OVRF      FILE-TOK08,TOFILE-TOK08.TOKFLIB
    OVRF      FILE-TOK09,TOFILE-TOKUOSE.TOKFLIB
    CALL      PGM-SKY2201B.TOKELIB
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## エラーデータ振分 異常 ##'
              ?KEKA4 := 'エラーデータ振分'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##エラーデータバックアップ　　　　　　　　　　　　　　　　　##./
STEP0090:

    ?STEP :=   'STEP0090'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && 'エラーデータバックアップ１'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-TOKUERR.TOKFLIB,TOFILE-TOKUERR.TOKBLIB,
            ADD-@NO,BF-1
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## エラーデータバックアップ １異常 ##'
              ?KEKA4 := 'エラーデータ　バックアップ１'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    ?MSGX :=  '## ACOSUP3  :' && 'エラーデータバックアップ３'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL CACOSBK3.TOKCOLIB,PARA-(?ERRCHK1)
    IF        ?ERRCHK1  =   'NG'   THEN
              ?MSGX  := '## エラーデータバックアップ ２異常 ##'
              ?KEKA4 := 'エラーデータ　バックアップ２'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##部門間在庫移動データ作成処理呼び出し　　　　　　　　　　　##./
STEP0100:

    ?STEP :=   'STEP0100'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && '部門間在庫移動ＤＴ作成'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PBZ00100.TOKCOLIB,PARA-(?ERRCHK2)
    IF        ?ERRCHK2  =   'NG'   THEN
              ?MSGX  := '## 部門間在庫移動ＤＴ作成　異常 ##'
              ?KEKA4 := '部門間在庫移動ＤＴ作成'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##日次計上エラーデータ初期化　　　　　　　　　　　　　　　　##./
STEP0110:

    ?STEP :=   'STEP0110'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && '日次計上エラーデータ初期化'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-TOKUERR.TOKFLIB
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 日次計上エラーデータ初期化異常 ##'
              ?KEKA4 := '日次計上エラーデータ初期化'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##日次計上エラーデータ戻し確認　　　　　　　　　　　　　　　##./
STEP0120:

    ?STEP :=   'STEP0120'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && '日次計上エラーＤＴ戻し確認'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?OPR1  :=  '　＃＃＃＃＃　日次更新エラー復元処理　＃＃＃＃＃　'
    ?OPR2  :=  '　　電算室殿より，計上エラーデータの受信処理を'
    ?OPR3  :=  '　　行いました。本社エラー振分件数を確認し，エラー'
    ?OPR4  :=  '　　を戻すか判断して下さい。'
    ?OPR5  :=  '　ＰＦ９：エラー戻し無し，実行：エラー戻し有り　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##売上作成ＦＬＧ初期化　　　　　　　　　　　　　　　　　　　##./
STEP0130:

    ?STEP :=   'STEP0130'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && '売上作成ＦＬＧ初期化'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-DENJNL3,TOFILE-SHTDENL3.TOKFLIB
    OVRF      FILE-NYKFILL1,TOFILE-NYKFILL1.TOKFLIB
    OVRF      FILE-NYSFILL1,TOFILE-NYSFILL1.TOKFLIB
    OVRF      FILE-SGYFILL1,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-TOKU,TOFILE-TOKUHOE.TOKFLIB
    OVRF      FILE-HKYOTU,TOFILE-WORKF.TOKFLIB
    CALL      PGM-TSY8010B.TOKELIB
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 売上作成ＦＬＧ初期化　異常 ##'
              ?KEKA4 := '売上作成ＦＬＧ初期化'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##実績累積Ｆ戻し処理　　　　　　　　　　　　　　　　　　　　##./
STEP0140:

    ?STEP :=   'STEP0140'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && '実績累積Ｆ戻し処理'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-TOKU,TOFILE-TOKUHOE.TOKFLIB
    OVRF      FILE-RUISEKF,TOFILE-RUISEKL1.TOKFLIB
    OVRF      FILE-JISSYUL1,TOFILE-JISSYUL1.TOKFLIB
    CALL      PGM-SJS0090B.TOKELIB
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 実績累積Ｆ戻し処理　異常 ##'
              ?KEKA4 := '実績累積Ｆ戻し処理'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##売上エラーデータ確認リスト発行　　　　　　　　　　　　　　##./
STEP0150:

    ?STEP :=   'STEP0150'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && '売上エラーデータ確認リスト発行'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-HKYOTU,TOFILE-WORKF.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-OSKT200.TOKELIB
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 売上エラー確認Ｌ　異常 ##'
              ?KEKA4 := '売上エラー確認Ｌ'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '日次計上エラー戻し処理（部門間在庫移動'
    ?KEKA2 :=  'ＤＴ作成含む）が正常に終了しました。　'
    ?KEKA3 :=  '戻し後の確認、部門間移動ＤＴの取込依頼'
    ?KEKA4 :=  'を情報ＳＹＳ部様に依頼して下さい。　　'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    /.$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$./
    /.##システム情報取得##./
    ?SYSDATE  :=    @SCDATED               /.ｼｽﾃﾑ情報取得./
    ?DATE     :=    %SBSTR(?SYSDATE,1,6)   /.ｼｽﾃﾑ日付取得./
    ?YOUBI    :=    %SBSTR(?SYSDATE,13,1)  /.曜日取得./
    ?YY       :=    %SBSTR(?DATE,5,2)      /.年取得./
    ?MM       :=    %SBSTR(?DATE,3,2)      /.月取得./
    ?DD       :=    %SBSTR(?DATE,1,2)      /.日取得./
    ?TIME1    :=    %SBSTR(?SYSDATE,7,2)   /.時刻取得./
    ?TIME2    :=    %SBSTR(?SYSDATE,9,2)   /.時刻取得./
    ?TIME3    :=    %SBSTR(?SYSDATE,11,2)  /.時刻取得./
    ?TIME     :=    %SBSTR(?SYSDATE,7,6)   /.時刻取得./
    /.$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$./
/.##開始ＭＳＧ出力##./
    ?MSGX :=  '## '  && ?PGMID && ?PGNM && ' 正常終了'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX     := '## 終了日時:' && ?YY && '/' && ?MM && '/' && ?DD
               && ' ' && ?TIME1 && ':' && ?TIME2 && ':' && ?TIME3
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '日次計上エラー戻し処理（部門間在庫移動'
    ?KEKA2 :=  'ＤＴ作成含む）が異常終了しました。ログ'
    ?KEKA3 :=  'リストを発行しＮＡＶへ連絡して下さい。'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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
    /.$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$./
    /.##システム情報取得##./
    ?SYSDATE  :=    @SCDATED               /.ｼｽﾃﾑ情報取得./
    ?DATE     :=    %SBSTR(?SYSDATE,1,6)   /.ｼｽﾃﾑ日付取得./
    ?YOUBI    :=    %SBSTR(?SYSDATE,13,1)  /.曜日取得./
    ?YY       :=    %SBSTR(?DATE,5,2)      /.年取得./
    ?MM       :=    %SBSTR(?DATE,3,2)      /.月取得./
    ?DD       :=    %SBSTR(?DATE,1,2)      /.日取得./
    ?TIME1    :=    %SBSTR(?SYSDATE,7,2)   /.時刻取得./
    ?TIME2    :=    %SBSTR(?SYSDATE,9,2)   /.時刻取得./
    ?TIME3    :=    %SBSTR(?SYSDATE,11,2)  /.時刻取得./
    ?TIME     :=    %SBSTR(?SYSDATE,7,6)   /.時刻取得./
    /.$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$./
/.##開始ＭＳＧ出力##./
    ?MSGX :=  '## '  && ?PGMID && ?PGNM && ' 異常終了'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX     := '## 終了日時:' && ?YY && '/' && ?MM && '/' && ?DD
               && ' ' && ?TIME1 && ':' && ?TIME2 && ':' && ?TIME3
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC
