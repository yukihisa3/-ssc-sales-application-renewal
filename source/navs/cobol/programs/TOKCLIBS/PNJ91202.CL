/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :  受配信管理システム                     *  ./
/. *   JOB-ID      :  PNJ91202                               *  ./
/. *   JOB-NAME    :  起動ＪＯＢ名、回線種別、回線番号取得   *  ./
/. *　　　　　　　　　（パラメタファイル２用）　　　　　　　 *  ./
/. ***********************************************************  ./
    PGM

/.##ワーク定義##./
    VAR  ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR  ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR  ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR  ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR  ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR  ?PGMID    ,STRING*8,VALUE-'PNJ91202'  /.PROGRAM-ID./
    VAR  ?STEP     ,STRING*8                   /.STEP-ID./
    VAR  ?KJOB     ,STRING*8,VALUE-'        '  /.起動ＪＯＢ名./
    VAR  ?KAISHU   ,STRING*1,VALUE-' '         /.回線種別./
    VAR  ?KSNO     ,STRING*1,VALUE-' '         /.回線制御番号./
    VAR  ?SYKEKA   ,STRING*1,VALUE-' '         /.処理結果./
    VAR  ?JOBNM    ,NAME                       /.JOB名./
    VAR  ?LIBNM    ,NAME                       /.ﾗｲﾌﾞﾗﾘ名./
    VAR  ?LIBNM1   ,STRING*08,VALUE-'NAV     ' /.ﾗｲﾌﾞﾗﾘ名１./
    VAR  ?LIBNM2   ,STRING*08,VALUE-'SAKATA1 ' /.ﾗｲﾌﾞﾗﾘ名１./
    VAR  ?LIBNM3   ,STRING*08,VALUE-'SKTPC30 ' /.ﾗｲﾌﾞﾗﾘ名１./
    VAR  ?PARAF    ,NAME!MOD                   /.ﾊﾟﾗﾒｰﾀﾌｧｲﾙ名./
    VAR  ?PARAF1   ,NAME!MOD                   /.ﾊﾟﾗﾒｰﾀﾌｧｲﾙ名./
    VAR  ?PARAF2   ,NAME!MOD                   /.ﾊﾟﾗﾒｰﾀﾌｧｲﾙ名./
    VAR  ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./
    VAR  ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?OPR2     ,STRING*50                  /.      2    ./
    VAR  ?OPR3     ,STRING*50                  /.      3    ./
    VAR  ?OPR4     ,STRING*50                  /.      4    ./
    VAR  ?OPR5     ,STRING*50                  /.      5    ./
    VAR  ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR  ?KEKA1    ,STRING*40                  /.      2    ./
    VAR  ?KEKA2    ,STRING*40                  /.      3    ./
    VAR  ?KEKA3    ,STRING*40                  /.      4    ./
    VAR  ?KEKA4    ,STRING*40                  /.      5    ./

    ACTMSGQ MAXRMSG-50

/.##プログラム開始メッセージ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＪＯＢキャンセル処理⇒２号機：ＩＳＤＮ'

/.##実行確認##./
    ?OPR1  :=  '　＃　オンライン受信状態　解除処理　＃　'
    ?OPR2  :=  '　オンライン受信状態を解除します。'
    ?OPR3  :=  '　受信端末１号機：公衆回線'
    ?OPR4  :=  '　確認して下さい。'
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##パラメタファイルより情報取得##./
SNJ9120B:

    ?STEP := 'SNJ9120B'
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-PARAFIL,TOFILE-PARAFIL2.TOKJLIB
    CALL PGM-SNJ9120B.TOKELIBO,
         PARA-(?KJOB,?KAISHU,?KSNO,?SYKEKA)
    IF  @PGMEC ^= 0 THEN  /.PG実行異常./
        ?KEKA4 :=  '【キャンセル情報取得】'
        GOTO ABEND
    ELSE
        IF  ?SYKEKA =  '9' THEN
            ?KEKA4 := '【パラメタファイルが読めません！】'
             GOTO ABEND
        END
    END
    ?MSGX := '# JOB名 = ' && ?KJOB && ' #'
    SNDMSG   ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##パラメータファイル名編集##./
    ?JOBNM  := %NAME(?KJOB)
    ?LIBNM  := %NAME(?LIBNM1)
    ?PARAF  := %NCAT(?JOBNM,?LIBNM)
    ?FILID  := %STRING(?PARAF)
    ?MSGX := '# ｷｬﾝｾﾙ回線情報 JOB名1 = ' && ?FILID && ' #'
    SNDMSG   ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?LIBNM  := %NAME(?LIBNM2)
    ?PARAF1 := %NCAT(?JOBNM,?LIBNM)
    ?FILID  := %STRING(?PARAF1)
    ?MSGX := '# ｷｬﾝｾﾙ回線情報 JOB名2 = ' && ?FILID && ' #'
    SNDMSG   ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?LIBNM  := %NAME(?LIBNM3)
    ?PARAF2 := %NCAT(?JOBNM,?LIBNM)
    ?FILID  := %STRING(?PARAF2)
    ?MSGX := '# ｷｬﾝｾﾙ回線情報 JOB名3 = ' && ?FILID && ' #'
    SNDMSG   ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ＪＯＢキャンセル##./
PCANJOB:

    ?STEP := 'PCANJOB '
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CANJOB JOB-?PARAF
    CANJOB JOB-?PARAF1
    CANJOB JOB-?PARAF2
/.  IF  @PGMEC ^= 0 THEN
        ?KEKA4 :=  '【ＪＯＢキャンセル】'
        GOTO ABEND
    END
./
    ?MSGX := '# ｷｬﾝｾﾙ回線情報 種別  = ' && ?KAISHU && ' #'
    SNDMSG   ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX := '# ｷｬﾝｾﾙ回線情報 番号  = ' && ?KSNO   && ' #'
    SNDMSG   ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##回線管理マスタ初期化##./
SNJ9080B:

    ?STEP := 'SNJ9080B'
    ?MSGX := '***   '  && ?STEP   &&   '        ***'
    SNDMSG   ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-JSMKAIL1,TOFILE-JSMKAIL1.TOKJLIB
    CALL PGM-SNJ9080B.TOKELIBO,
         PARA-(?KAISHU,?KSNO)
    IF  @PGMEC ^= 0 THEN  /.PG実行異常./
        ?KEKA4 :=  '【回線管理マスタ初期化】'
        GOTO ABEND
    END

RTN:

    ?KEKA1 :=  'キャンセル処理が正常終了しました。'
    ?KEKA2 :=  '確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  'キャンセルが異常終了しました。'
    ?KEKA2 :=  'キャンセルが完了出来ていませんので'
    ?KEKA3 :=  'ＮＡＶへの連絡が必要です。'
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