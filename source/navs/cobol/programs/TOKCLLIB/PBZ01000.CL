/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    部門間在庫移動ＤＴ再計上処理         *  ./
/. *   JOB-ID      :    PBZ01000                             *  ./
/. *   JOB-NAME    :    部門間在庫移動処理　　　　　　　　   *  ./
/. *   UPDATE      :    2018/04/10 NAV TAKAHASHI             *  ./
/. *                                                         *  ./
/. ***********************************************************  ./
    PGM
/.##ﾊﾟﾗﾒﾀ##./
    VAR       ?ERRCHK   ,STRING*2,VALUE-'  '       /.実行結果区分./
/.##WORK##./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMES    ,STRING*5,VALUE-'     '
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PBZ00100'
    VAR       ?STEP     ,STRING*8
/.--------------------------------------------------------------./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
/.--------------------------------------------------------------./
    VAR       ?MSG1     ,STRING*80                 /.開始終了MSG./
    VAR       ?PGNM     ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                 /.      2    ./
    VAR       ?KEKA2    ,STRING*40                 /.      3    ./
    VAR       ?KEKA3    ,STRING*40                 /.      4    ./
    VAR       ?KEKA4    ,STRING*40                 /.      5    ./
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門./
    VAR       ?TANCD8   ,STRING*8,VALUE-'      '   /.部門+担当者./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者./
    VAR       ?SOKO     ,STRING*2,VALUE-'  '       /.倉庫ＣＤ    ./
    VAR       ?DSOKO    ,STRING*2,VALUE-'  '       /.代表倉庫    ./
/.--------------------------------------------------------------./
    VAR       ?JKENSU   ,STRING*7,VALUE-'0000000'  /.処理件数./
    VAR       ?KDATE    ,STRING*8,VALUE-'00000000' /.計上日./
/.--------------------------------------------------------------./
    VAR       ?SYORIKK  ,STRING*1,VALUE-' '        /.監視JOB起動./
/.--------------------------------------------------------------./
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM     :=  '部門間在庫移動ＤＴ再計上処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ライブラリリスト登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKDTLIB/TOKKLIB/TOKMDLIB


/.##処理確認画面　　　　　　　　　　　　　　　　　　　　　　　##./
STEP0010:

    ?STEP :=   'STEP0010'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ACOSUP2  :' && '処理確認画面'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?OPR1  :=  '　＃＃＃＃　部門間在庫移動ＤＴ再計上処理　＃＃＃　'
    ?OPR2  :=  '　　部門間在庫移動ＤＴ再計上処理を行います。'
    ?OPR3  :=  '　　計上データが存在する場合は、情報ＳＹＳ部に'
    ?OPR4  :=  '　　連絡し、取込依頼を行って下さい！！'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##振替移動実績IF作成##./
SBZ0070B:

    ?STEP :=   'SBZ0070B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBZ0070B.TOKSOLIB,PARA-(?BUMON,?TANCD,?JKENSU,
                                          ?KDATE)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '振替移動実績ＩＦ作成　エラー'
              GOTO ABEND
    END
/.##作成件数セット##./
    ?SYORIKN1 :=   ?JKENSU

/.##部門間在庫移動リスト発行##./
SBZ0050L:

    ?STEP :=   'SBZ0050L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBZ0050L.TOKSOLIB,PARA-('1',?KDATE,?KDATE,' ','1')
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '部門間在庫移動リスト発行　エラー'
              GOTO ABEND
    END

/.##処理結果リスト（件数表示用）##./
KEKAMSG1:

    ?STEP :=   'KEKAMSG1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

                 /.１２３４５６７８９０１２３４５６７８９０１２'./
    IF  ?SYORIKN1  =  '0000000'  THEN
     ?SYORINM1 := '振替在庫移動ＩＦ作成処理　計上無し０件！'
     ?SYORINM2 := '再計上データからは部門間移動データが作成'
     ?SYORINM3 := 'されませんでした。情報ＳＹＳ室連絡は不要です'
     ?SYORIKK  := '1'
    ELSE
     ?SYORINM1 :=  '振替在庫移動ＩＦ作成処理　計上有り確認！'
     ?SYORINM2 := '再計上が必要な部門間在庫移動ＤＴが作成されま'
     ?SYORINM3 := 'した。内容確認し情報ＳＹＳ室に連絡して下さい'
     ?SYORIKK  := '2'
    END
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '処理結果リスト（件数表示用）　エラー'
              GOTO ABEND
    END

/.##ＡＣＯＳ連携結果監視ファイル初期化##./
PCLEFIL3:

    ?STEP :=   'PCLRFIL3'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-ACOSCHK4.TOKWLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  'ＡＣＯＳ連携結果監視Ｆ初期化　エラー'
              GOTO ABEND
    END

/.##処理結果リスト（件数表示用）##./
KANSIPGS:

    ?STEP :=   'KANSIPGS'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    IF  ?SYORIKK  =  '2'  THEN
        ?MSGX :=  '## 部門間在庫移動計上結果監視　有 ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
        SBMJOB JOB-BMIDCHK,JOBD-SKTJOBD.XUCL,JOBK-@B,
        PGM-PBZ00900.TOKCOLIB,LOG-@YES!1024,PGMEL-@I/@L/@S/@T
    ELSE
        ?MSGX :=  '## 部門間在庫移動計上結果監視　無 ##'
        SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.----------------------------------------------------------------./
RTN:

    ?ERRCHK := 'OK'
    ?KEKA1 :=  '部門間在庫移動ＤＴ再計上処理が正常'
    ?KEKA2 :=  '終了しました。'
    ?KEKA3 :=  '件数を情報ＳＹＳ部様に連絡して下さい'
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKMDLIB
    CALL SMG0040L.TOKSOLIB,
         PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    ?ERRCHK := 'NG'
    ?KEKA1 :=  '部門間在庫移動ＤＴ再計上処理が異常'
    ?KEKA2 :=  '終了しました。'
    ?KEKA3 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKMDLIB
    CALL SMG0040L.TOKSOLIB,
         PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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