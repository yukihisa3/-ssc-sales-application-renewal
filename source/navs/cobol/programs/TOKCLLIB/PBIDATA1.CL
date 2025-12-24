/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＢＩ連携　　　　　　                 *  ./
/. *   JOB-ID      :    PBIDATA1                          　 *  ./
/. *   JOB-NAME    :    ＢＩ連携データ作成　ＢＩフォルダ転送 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PBIDATA1'
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
    VAR       ?KENSU1 ,STRING*6,VALUE-'000000'
    VAR       ?KENSU2 ,STRING*6,VALUE-'000000'
    VAR       ?KENSU3 ,STRING*6,VALUE-'000000'
    VAR       ?KENSU4 ,STRING*6,VALUE-'000000'
    VAR       ?KENSU5 ,STRING*6,VALUE-'000000'
    VAR       ?KENSU6 ,STRING*6,VALUE-'000000'
    VAR       ?KENSU7 ,STRING*6,VALUE-'000000'
    VAR       ?KENSU8 ,STRING*6,VALUE-'000000'
    VAR       ?ERRCHK3,STRING*1,VALUE-' '

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＢＩ連携データ作成　ＢＩフォルダ転送'

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
    DEFLIBL TOKELIB/TOKFLIB/TOKEILBO/TOKWLIB/TOKDLIB/TOKBILIB/TOKKLIB
           /TOKDTLIB

/.##処理確認画面　　　　　　　　　　　　　　　　　　　　　　　##./
STEP0010:

    ?STEP :=   'STEP0010'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1  :' && '処理確認画面'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?OPR1  :=  '　＃＃＃＃＃　ＢＩ連携ＤＴ抽出処理　　＃＃＃＃＃　'
    ?OPR2  :=  '　　前日の日次計上データ、マスタデータのＢＩ用'
    ?OPR3  :=  '　　フォルダーへのデータ作成を開始します。'
    ?OPR4  :=  '　　確認してください！！'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)


/.##売上計上エラーデータバックアップ　　　　　　　　　　　　　##./
STEP0015:

    ?STEP :=   'STEP0015'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && '売上計上エラーデータバックアップ'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  CALL CACOSBK3.TOKCOLIB,PARA-(?ERRCHK3)
./  IF        ?ERRCHK1  =   '1'   THEN
              ?MSGX  := '## 日次計上データバックアップ異常##'
              ?KEKA4 := '売上計上ＥＲＲＤＴバックアップ'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##日次計上ファイルデータセット　　　　　　　　　　　　　　　##./
STEP0020:

    ?STEP :=   'STEP0020'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && '日次計上ファイルデータセット'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-TOKUJ,TOFILE-BITOKUJ.TOKBILIB
    CALL      PGM-SBI0100B.TOKSOLIB,PARA-(?KENSU1)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 日次計上ファイルデータセット異常##'
              ?KEKA4 := '日次計上ＤＴセット処理'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##日次計上ファイルエラーデータチェック　　　　　　　　　　　##./
STEP0030:

    ?STEP :=   'STEP0030'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && '日次計上ﾌｧｲﾙｴﾗｰﾃﾞｰﾀﾁｪｯｸ'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBI0110B.TOKSOLIB
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 日次計上エラーチェック　　異常 ##'
              ?KEKA4 := '日次計上エラーチェック'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##日次計上ファイル連携用ＣＳＶ作成　　　　　　　　　　　　　##./
STEP0040:

    ?STEP :=   'STEP0040'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && '日次計上ﾌｧｲﾙ連携用CSV作成'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBI0120B.TOKSOLIB,PARA-(?KENSU2)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 日次計上エラーチェック　　異常 ##'
              ?KEKA4 := '日次計上エラーチェック'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    ?MSGX :=  'KENSU = ' && ?KENSU1 && '-' && ?KENSU2
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##日次計上データＣＳＶ変換　　　　　　　　　　　　　　　　　##./
STEP0050:

    ?STEP :=   'STEP0050'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && '日次計上データＣＳＶ変換'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBI0130V.TOKSOLIB,PARA-(?KENSU3)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 日次計上データＣＳＶ変換　異常 ##'
              ?KEKA4 := '日次計上データＣＳＶ変換'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##ＡＣＯＳ振替データＣＳＶ変換　　　　　　　　　　　　　　　##./
STEP0060:

    ?STEP :=   'STEP0060'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && 'ＡＣＯＳ振替データＣＳＶ変換'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-BITRNSET,TOFILE-BIFURSET.TOKBILIB
    OVRF FILE-SNDTRNDT,TOFILE-SNDFURDT.TOKBILIB
    CALL      PGM-SBI0130V.TOKSOLIB,PARA-(?KENSU4)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＡＣＯＳ振替データＣＳＶ変換異常 ##'
              ?KEKA4 := 'ＡＣＯＳ振替データＣＳＶ変換'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##売上未計上データＣＳＶ変換　　　　　　　　　　　　　　　　##./
STEP0070:

    ?STEP :=   'STEP0070'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && '売上未計上データＣＳＶ変換'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-BITRNSET,TOFILE-BIMKESET.TOKBILIB
    OVRF FILE-SNDTRNDT,TOFILE-SNDMKEDT.TOKBILIB
    CALL      PGM-SBI0130V.TOKSOLIB,PARA-(?KENSU5)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 売上未計上データＣＳＶ変換異常 ##'
              ?KEKA4 := '売上未計上データＣＳＶ変換'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##商品変換ＴＢＬ　ＣＳＶ変換　　　　　　　　　　　　　　　　##./
STEP0080:

    ?STEP :=   'STEP0080'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && '商品変換ＴＢＬ　ＣＳＶ変換'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBI0170V.TOKSOLIB,PARA-(?KENSU6)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 商品変換ＴＢＬ　ＣＳＶ変換異常 ##'
              ?KEKA4 := '商品変換ＴＢＬ　ＣＳＶ変換'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##商品名称マスタ　ＣＳＶ変換　　　　　　　　　　　　　　　　##./
STEP0090:

    ?STEP :=   'STEP0090'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && '商品名称マスタ　ＣＳＶ変換'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBI0180V.TOKSOLIB,PARA-(?KENSU8)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 商品名称マスタ　ＣＳＶ変換異常 ##'
              ?KEKA4 := '商品名称マスタ　ＣＳＶ変換'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##在庫マスタ　　　ＣＳＶ変換　　　　　　　　　　　　　　　　##./
STEP0100:

    ?STEP :=   'STEP0100'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && '在庫マスタ　　　ＣＳＶ変換'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBI0160V.TOKSOLIB,PARA-(?KENSU7)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## 在庫マスタ　　　ＣＳＶ変換異常 ##'
              ?KEKA4 := '在庫マスタ　　　ＣＳＶ変換'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##ＢＩ連携用データ転送　　　　　　　　　　　　　　　　　　　##./
STEP0110:

    ?STEP :=   'STEP0110'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1 :' && 'ＢＩ連携用データ転送'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    FIMPORT FILE-SNDTRNDT.TOKBILIB,PARA-SNDBI,UNIT-1
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＢＩ連携用データ転送（日次計上） ##'
              ?KEKA4 := 'ＢＩ連携用データ転送（日次計上）'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    FIMPORT FILE-SNDFURDT.TOKBILIB,PARA-SNDBI,UNIT-2
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＢＩ連携用データ転送（ＡＣ振替） ##'
              ?KEKA4 := 'ＢＩ連携用データ転送（ＡＣ振替）'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    FIMPORT FILE-SNDMKEDT.TOKBILIB,PARA-SNDBI,UNIT-3
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＢＩ連携用データ転送（売未計上） ##'
              ?KEKA4 := 'ＢＩ連携用データ転送（売未計上）'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    FIMPORT FILE-SNDSYOTB.TOKBILIB,PARA-SNDBI,UNIT-4
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＢＩ連携用データ転送（商品変換） ##'
              ?KEKA4 := 'ＢＩ連携用データ転送（商品変換）'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    FIMPORT FILE-SNDSHOMS.TOKBILIB,PARA-SNDBI,UNIT-5
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＢＩ連携用データ転送（商品名称） ##'
              ?KEKA4 := 'ＢＩ連携用データ転送（商品名称）'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

    FIMPORT FILE-SNDZAIKO.TOKBILIB,PARA-SNDBI,UNIT-6
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＢＩ連携用データ転送（在庫ＤＴ） ##'
              ?KEKA4 := 'ＢＩ連携用データ転送（在庫ＤＴ）'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

/.##ＢＩ連携件数リスト発行　　　　　　　　　　　　　　　　　　##./
STEP0120:

    ?STEP :=   'STEP0120'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## PBIDATA1  :' && 'ＢＩ連携件数リスト発行'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBI0060L.TOKSOLIB,PARA-('2',?KENSU3,?KENSU4,
              ?KENSU5,?KENSU6,?KENSU8,?KENSU7)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?MSGX  := '## ＢＩ連携件数リスト異常 ##'
              ?KEKA4 := 'ＢＩ連携件数リスト発行'
              SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  'ＢＩ連携用データ作成処理が正常終了しま'
    ?KEKA2 :=  'した。転送内容を確認し、ＢＩ側の取込を'
    ?KEKA3 :=  '実行してください。'
    ?KEKA4 :=  ''
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

    ?KEKA1 :=  'ＢＩ連携用データ作成処理が異常終了しま'
    ?KEKA2 :=  'した。ログリストを採取し、ＮＡＶへ連絡'
    ?KEKA3 :=  'してください。'
    ?KEKA4 :=  ''
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