/. ***********************************************************  ./
/. *   サカタのタネ                                          *  ./
/. *   SYSTEM-NAME :    部門間在庫移動機能構築　　　         *  ./
/. *   JOB-ID      :    PBZ00100                             *  ./
/. *   JOB-NAME    :    振替移動実績ＩＦ作成処理　　　　　   *  ./
/. *   UPDATE      :    2018/01/30 NAV TAKAHASHI             *  ./
/. *                                                         *  ./
/. ***********************************************************  ./
    PGM  (P1-?ERRCHK)
/.  PGM ./
/.##ﾊﾟﾗﾒﾀ##./
    PARA      ?ERRCHK   ,STRING*2,OUT,VALUE-'  '   /.実行結果区分./
/.  VAR       ?ERRCHK   ,STRING*2,VALUE-'  '         実行結果区分./
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
    VAR       ?SYSDATE  ,STRING*13                  /.ｼｽﾃﾑ情報取得./
    VAR       ?DATE     ,STRING*6                   /.ｼｽﾃﾑ日付./
    VAR       ?YOUBI    ,STRING*1                   /.曜日./
    VAR       ?YOUBIN   ,STRING*99                  /.曜日名./
    VAR       ?YY       ,STRING*2                   /.年./
    VAR       ?MM       ,STRING*2                   /.月./
    VAR       ?DD       ,STRING*2                   /.日./
    VAR       ?TIME1    ,STRING*2                   /.時刻./
    VAR       ?TIME2    ,STRING*2                   /.時刻./
    VAR       ?TIME3    ,STRING*2                   /.時刻./
    VAR       ?TIME     ,STRING*6                   /.時刻./
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

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM     :=  '振替在庫移動ＩＦ作成処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ライブラリリスト登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKDTLIB/TOKKLIB/TOKMDLIB

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
          # '2' #   ?YOUBIN :=  '火曜日'
          # '3' #   ?YOUBIN :=  '水曜日'
          # '4' #   ?YOUBIN :=  '木曜日'
          # '5' #   ?YOUBIN :=  '金曜日'
          # '6' #   ?YOUBIN :=  '土曜日'
          # '7' #   ?YOUBIN :=  '日曜日'
    ELSE
                    ?YOUBIN :=  '＊'
    END

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKO,?DSOKO)
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD8)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    ?TANCD := %SBSTR(?TANCD8,1,2)

/.##ＭＳＧ出力##./
    ?MSGX     := '## 実行日付:' && ?YY && '/' && ?MM && '/' && ?DD
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX     := '## 実行時刻:' && ?TIME1 && ':' && ?TIME2 && ':'
                 && ?TIME3
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX     := '## 実行曜日:' && ?YOUBIN
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX     := '## 部門:' && ?BUMON && ' 担当者：' && ?TANCD
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##ACOS計上データ（部門間在庫移動用データ）バックアップ##./
PCNVFILE:

    ?STEP :=   'PCNVFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?MSGX :=  '*** ACOSDAT4 => ACOSDAT5(CNVFILE) ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-ACOSDAT4.TOKDTLIB,TOFILE-ACOSDAT5.TOKDTLIB,
            ADD-@NO,BF-1
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4  :=  'バックアップ１エラー'
              GOTO ABEND
    END

    ?MSGX :=  '*** ACOSDAT3 => ACOSDAT4(CNVFILE) ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-ACOSDAT3.TOKDTLIB,TOFILE-ACOSDAT4.TOKDTLIB,
            ADD-@NO,BF-1
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4  :=  'バックアップ２エラー'
              GOTO ABEND
    END

    ?MSGX :=  '*** ACOSDAT2 => ACOSDAT3(CNVFILE) ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-ACOSDAT2.TOKDTLIB,TOFILE-ACOSDAT3.TOKDTLIB,
            ADD-@NO,BF-1
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4  :=  'バックアップ３エラー'
              GOTO ABEND
    END

    ?MSGX :=  '*** ACOSDAT1 => ACOSDAT2(CNVFILE) ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-ACOSDAT1.TOKDTLIB,TOFILE-ACOSDAT2.TOKDTLIB,
            ADD-@NO,BF-1
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4  :=  'バックアップ４エラー'
              GOTO ABEND
    END

    ?MSGX :=  '*** ACOSDATA => ACOSDAT1(CNVFILE) ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-ACOSDATA.TOKDTLIB,TOFILE-ACOSDAT1.TOKDTLIB,
            ADD-@NO,BF-1
    ?PGMEC := @PGMEC
    ?PGMES := @PGMES
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4  :=  'バックアップ５エラー'
              GOTO ABEND
    END

/.##部門間移動抽出F初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-BUMIDTF.TOKDTLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '部門間移動抽出Ｆ初期化　エラー'
              GOTO ABEND
    END

/.##部門間在庫移動DT抽出##./
SBZ0010B:

    ?STEP :=   'SBZ0010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBZ0010B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '部門間在庫移動ＤＴ抽出　エラー'
              GOTO ABEND
    END

/.##ACOS計上エラーデータ削除処理##./
SBZ0130B:

    ?STEP :=   'SBZ0130B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-TOKUERR,TOFILE-TOKUERR.TOKFLIB
    CALL      PGM-SBZ0130B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  'ＡＣＯＳ計上ＥＲＤＴ削除処理　エラー'
              GOTO ABEND
    END

/.##部門間移動集計F初期化##./
PCLRFIL1:

    ?STEP :=   'PCLRFIL1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-BUMSYUF.TOKDTLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '部門間移動集計Ｆ初期化　エラー'
              GOTO ABEND
    END

/.##部門間在庫移動DT集計##./
SBZ0020B:

    ?STEP :=   'SBZ0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBZ0020B.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '部門間在庫移動ＤＴ集計　エラー'
              GOTO ABEND
    END

/.##部門間在庫入出庫DT作成##./
SBZ0030B:

    ?STEP :=   'SBZ0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL      PGM-SBZ0030B.TOKSOLIB,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '部門間在庫入出庫ＤＴ作成　エラー'
              GOTO ABEND
    END

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

/.##部門間在庫移動計上Ｆコピー##./
CNVFILE1:

    ?STEP :=   'CNVFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-FURIACOS.TOKDTLIB,TOFILE-ACOSFURI.TOKWLIB,
            BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '部門間在庫移動計上Ｆコピー　エラー'
              GOTO ABEND
    END

/.##部門間在庫移動用データ　初期化##./
PCLEFIL2:

    ?STEP :=   'PCLRFIL2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-ACOSDATA.TOKDTLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4  :=  '部門間在庫移動用ＤＴ初期化　エラー'
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
     ?SYORINM2 := '本日の計上データからは部門間移動データが作成'
     ?SYORINM3 := 'されませんでした。情報ＳＹＳ室連絡は不要です'
     ?SYORIKK  := '1'
    ELSE
     ?SYORINM1 :=  '振替在庫移動ＩＦ作成処理　計上有り確認！'
     ?SYORINM2 := '計上が必要な部門間在庫移動データが作成されま'
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
    ?KEKA1 :=  '振替移動実績ＩＦ作成処理が正常終了'
    ?KEKA2 :=  'しました。'
    ?KEKA3 :=  '件数を情報ＳＹＳ部様に連絡して下さい'
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKMDLIB
    CALL SMG0040L.TOKSOLIB,
         PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    ?ERRCHK := 'NG'
    ?KEKA1 :=  '振替移動実績ＩＦ作成処理が異常終了'
    ?KEKA2 :=  'しました。'
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