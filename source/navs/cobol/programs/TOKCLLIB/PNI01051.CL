/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    Ｄ３６５連携　　　　　               *  ./
/. *   JOB-ID      :    PNI01051                             *  ./
/. *   JOB-NAME    :    Ｄ３６５計上データ再作成処理　　　　 * ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PNI01051'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門CD./
    VAR       ?TANTO    ,STRING*8,VALUE-'        ' /.担当者CD./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./
    VAR       ?PARA1  ,STRING*8,VALUE-'00000000'
    VAR       ?PARA2  ,STRING*6,VALUE-'000000'
    VAR       ?PARA3  ,STRING*1,VALUE-' '
    VAR       ?PARA4  ,STRING*1,VALUE-' '
    VAR       ?PARA5  ,STRING*1,VALUE-' '

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'Ｄ３６５計上データ再作成処理（追加）'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL LIBL-TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/TOKDTLIB/
            D365DLIB/TOKWLIB/TOKMDLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

TNI0105I: /.##計上作成　日付、時刻指定画面##./

    ?STEP :=   'TNI0105I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIBO,MEDLIB-TOKMDLIB
    CALL      PGM-TNI0105I.TOKSOLIB,
              PARA-(?PARA1,?PARA2)
    IF        @PGMEC    ^=   0  THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 := '計上作成　日付、時刻指定画面'
              GOTO  ABEND
         END
    END
    ?MSGX :=  '*** DATE     = '  && ?PARA1  &&   ' ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '*** TIME     = '  && ?PARA2  &&   ' ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

PBACKUP:  /.##計上データ再作成#./

    ?STEP :=   'PBACKUP '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.  CNVFILE FILE-SNDNJYTF.D365DLIB,TOFILE-SNDNJYTF.D365WORK,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '受注データバックアップ'
              GOTO  ABEND
    END
    CLRFILE FILE-SNDNJYTF.D365DLIB
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '受注データ初期化'
              GOTO  ABEND
    END

    CNVFILE FILE-SNDURIF.D365DLIB,TOFILE-SNDURIF.D365WORK,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '売上データバックアップ'
              GOTO  ABEND
    END
    CLRFILE FILE-SNDURIF.D365DLIB
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '売上データ初期化'
              GOTO  ABEND
    END

    CNVFILE FILE-SNDJNBKF.D365DLIB,TOFILE-SNDJNBKF.D365WORK,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '受注値引データバックアップ'
              GOTO  ABEND
    END
    CLRFILE FILE-SNDJNBKF.D365DLIB
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '受注値引データ初期化'
              GOTO  ABEND
    END

    CNVFILE FILE-SNDUNBKF.D365DLIB,TOFILE-SNDUNBKF.D365WORK,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '売上値引データバックアップ'
              GOTO  ABEND
    END
    CLRFILE FILE-SNDUNBKF.D365DLIB
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '売上値引データ初期化'
              GOTO  ABEND
    END

    CNVFILE FILE-SNDAKKF.D365DLIB,TOFILE-SNDAKKF.D365WORK,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '売上調整データバックアップ'
              GOTO  ABEND
    END
    CLRFILE FILE-SNDAKKF.D365DLIB
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '売上値引データ初期化'
              GOTO  ABEND
    END
./
TNI0105B:  /.##計上データ再作成#./

    ?STEP :=   'TNI0105B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-SNDUJYTF,TOFILE-SNDNJYTF.D365DLIB
    CALL      PGM-TNI0105B.TOKSOLIB,
              PARA-(?PARA1,?PARA2)
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '計上データ再作成'
              GOTO  ABEND
    END

NVD0600L:  /.##計上データ件数一覧発行##./

    ?STEP :=   'NVD0600L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-NVD0600L.TOKSOLIB,
              PARA-(?PARA1,?PARA2)
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 := '計上データ件数一覧発行'
              GOTO  ABEND
    END

RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '＃＃日次計上データ作成作成処理＃＃'
    ?KEKA2 :=  '　処理が正常終了しました。'
    ?KEKA3 :=  '　件数を確認して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  '＃＃日次計上データ作成作成処理＃＃'
    ?KEKA2 :=  '　処理が異常終了しました。ログを'
    ?KEKA3 :=  '　採取し、管理者に連絡した下さい'
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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC
