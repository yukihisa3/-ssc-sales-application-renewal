# CACOSBK1

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/CACOSBK1.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　ホームガーデン部基幹システム          *  ./
/. *   SYSTEM-NAME :    ＡＣＯＳ計上処理　　　　　　　　　　 *  ./
/. *   JOB-ID      :    CACOSBK1                             *  ./
/. *   JOB-NAME    :    ＨＧ部売上／仕入在庫ＤＴバックアップ *  ./
/. *   UPDATE      :    2018/01/30 NAV TAKAHASHI             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMES  ,STRING*5,VALUE-'     '
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'CACOSBK1'
    VAR       ?STEP   ,STRING*8
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
    VAR       ?TAKAI1 ,STRING*14,VALUE-'/HGNAS/ACOSBAK'
    VAR       ?TAKAI2 ,STRING*99                  /.多階層LIB名./
    VAR       ?MON    ,STRING*5,VALUE-'/1MON'     /.月曜日./
    VAR       ?TUE    ,STRING*5,VALUE-'/2TUE'     /.火曜日./
    VAR       ?WED    ,STRING*5,VALUE-'/3WED'     /.水曜日./
    VAR       ?THU    ,STRING*5,VALUE-'/4THU'     /.木曜日./
    VAR       ?FRI    ,STRING*5,VALUE-'/5FRI'     /.金曜日./
    VAR       ?STA    ,STRING*5,VALUE-'/6STA'     /.金曜日./
    VAR       ?SUN    ,STRING*5,VALUE-'/7SUN'     /.金曜日./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＡＣＯＳ計上ＢＫＵＰ（売上／仕在個別）'

/.##ﾌﾟﾛｸﾞﾗﾑ開始MSG##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ライブラリリスト登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKWLIB

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

/.##曜日変換##./
    CASE  ?YOUBI OF
          # '1' #   ?YOUBIN :=  '月曜日'
                    ?TAKAI2 :=  ?TAKAI1 && ?MON
          # '2' #   ?YOUBIN :=  '火曜日'
                    ?TAKAI2 :=  ?TAKAI1 && ?TUE
          # '3' #   ?YOUBIN :=  '水曜日'
                    ?TAKAI2 :=  ?TAKAI1 && ?WED
          # '4' #   ?YOUBIN :=  '木曜日'
                    ?TAKAI2 :=  ?TAKAI1 && ?THU
          # '5' #   ?YOUBIN :=  '金曜日'
                    ?TAKAI2 :=  ?TAKAI1 && ?FRI
          # '6' #   ?YOUBIN :=  '土曜日'
                    ?TAKAI2 :=  ?TAKAI1 && ?STA
          # '7' #   ?YOUBIN :=  '日曜日'
                    ?TAKAI2 :=  ?TAKAI1 && ?SUN
    ELSE
                    ?YOUBIN :=  '＊'
    END

/.##ＭＳＧ出力##./
    ?MSGX     := '## 実行日付:' && ?YY && '/' && ?MM && '/' && ?DD
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX     := '## 実行時刻:' && ?TIME1 && ':' && ?TIME2 && ':'
                 && ?TIME3
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX     := '## 実行曜日:' && ?YOUBIN
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX     := '## 多階層BK:' && ?TAKAI2
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX     := '## 時刻６桁:' && ?TIME
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    SAVFILE FILE-TOKUWK.TOKWLIB/TOKUZ.TOKFLIB,TODEV-@NONE,
            MODE-@USED,TOPATH-?TAKAI2,REP-@YES,
            COMPRESS-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
            ?KEKA4 := 'ＡＣＯＳ計上ＤＴ多階層退避異常'
            GOTO ABEND END

RTN:

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO
    ?KEKA1 :=  'ＡＣＯＳ計上データバックアップ処理が'
    ?KEKA2 :=  '正常終了しました。'
    ?KEKA3 :=  '（売上／仕入在庫別データ）'
    ?KEKA4 :=  ''
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-?PGMEC

ABEND:

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO
    ?KEKA1 :=  'ＡＣＯＳ計上データバックアップ処理が'
    ?KEKA2 :=  '異常終了しました。ログを採取しＮＡＶ'
    ?KEKA3 :=  'へ連絡して下さい。（売上／仕入在庫）'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-?PGMEC


```
