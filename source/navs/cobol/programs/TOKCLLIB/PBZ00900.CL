/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    部門間在庫移動機能構築　　　         *  ./
/. *   JOB-ID      :    PBZ00900                             *  ./
/. *   JOB-NAME    :    振替移動実績連携結果監視処理　　　   *  ./
/. *   UPDATE      :                                         *  ./
/. *                                                         *  ./
/. ***********************************************************  ./
    PGM

    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PBZ00900'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?SOKO     ,STRING*2,VALUE-'  '       /.倉庫ＣＤ    ./
    VAR       ?DSOKO    ,STRING*2,VALUE-'  '       /.代表倉庫    ./
    VAR       ?PGNM     ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                 /.      2    ./
    VAR       ?KEKA2    ,STRING*40                 /.      3    ./
    VAR       ?KEKA3    ,STRING*40                 /.      4    ./
    VAR       ?KEKA4    ,STRING*40                 /.      5    ./

    VAR       ?JKEKA    ,STRING*1,VALUE-'0'        /.結果./
    VAR       ?JDATE    ,STRING*8,VALUE-'00000000' /.日付./

/.##ﾌﾟﾛｸﾞﾗﾑﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKWLIB/TOKDTLIB/TOKKLIB/TOKMDLIB

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.----------------------------------------------------------------./
/.##監視処理##./
SBZ0090B:

    ?STEP :=   'SBZ0090B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES


    ?MSGX :=  '##監視中##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    CALL TWAIT30.XUCL

    CALL      PGM-SBZ0090B.TOKSOLIB,PARA-(?JDATE,?JKEKA)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0    THEN
              GOTO   ABEND
    END

    ?MSGX :=  '## 結果 = ' &&  ?JKEKA
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 日付 = ' &&  ?JDATE
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    IF  ?JKEKA  =  ' '  THEN
        ?MSGX :=  '##監視継続##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
        GOTO      SBZ0090B
    END

    IF  ?JKEKA  =  '1'  THEN
        ?MSGX :=  '##結果=OK##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
           /.##下記２０桁目は表示されないので＊等をセット##./
        ?PGNM  :=  '＜ＨＧ振替移動実績連携結果監視結果　＞＊'
        ?KEKA1 :=  'ＡＣＯＳより振替移動実績連携結果を受信＊'
        ?KEKA2 :=  'しました。計上データ全件数正常に計上さ＊'
        ?KEKA3 :=  'れました。　　　　　　　　　　　　　　＊'
        ?KEKA4 :=  'リスト等で計上内容の確認実施して下さい＊'
        OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKMDLIB
        CALL SMG0040L.TOKSOLIB,
             PARA-('4',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
        ?PGMEC := @PGMEC
        ?PGMES := @PGMES
        IF        ?PGMEC    ^=   0    THEN
                  GOTO   ABEND
        END
        GOTO PCLRFILE
    END

    IF  ?JKEKA  =  '2'  THEN
        ?MSGX :=  '##結果=NG##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
           /.##下記２０桁目は表示されないので＊等をセット##./
        ?PGNM  :=  '＜ＨＧ振替移動実績連携結果監視結果　＞＊'
        ?KEKA1 :=  '振替移動実績連携ＤＴ＝＞エラー有り！！＊'
        ?KEKA2 :=  '情報ＳＹＳ部発行のエラーリストを参照し＊'
        ?KEKA3 :=  'データの修正を実施して下さい！！！！！＊'
        ?KEKA4 :=  '（本日分は次処理で再度計上されます。）＊'
        OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKMDLIB
        CALL SMG0040L.TOKSOLIB,
             PARA-('3',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
        ?PGMEC := @PGMEC
        ?PGMES := @PGMES
        IF        ?PGMEC    ^=   0    THEN
                  GOTO   ABEND
        END
    END

/.##ＡＣＯＳ計上済ＦＬＧ戻し処理##./
SBZ0110B:

    ?STEP :=   'SBZ0110B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBZ0110B.TOKSOLIB,PARA-(?JDATE)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##ＡＣＯＳ計上Ｆ初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-ACOSFURI.TOKWLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.----------------------------------------------------------------./
RTN:

    ?MSGX :=  '##振替移動実績連携監視処理終了##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

           /.##下記２０桁目は表示されないので＊等をセット##./
        ?PGNM  :=  '＜ＨＧ振替移動実績連携結果監視処理　＞＊'
        ?KEKA1 :=  '処理が異常終了しました。　　　　　　　＊'
        ?KEKA2 :=  '連携結果の判定更新がされていない可能性＊'
        ?KEKA3 :=  'があります。ログリストを採取し、ＮＡＶ＊'
        ?KEKA4 :=  'へ連絡して下さい！！　　　　　　　　　＊'
        OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKMDLIB
        CALL SMG0040L.TOKSOLIB,
             PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '##振替移動実績連携監視処理終了##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
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
