# ACOSUPR

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/ACOSUPR.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    ACOSUPR                           　 *  ./
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
    VAR       ?PGMID  ,STRING*8,VALUE-'ACOSUPR'
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

```
