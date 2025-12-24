# RACOSPG2

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/RACOSPG2.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫システム　　　　　　　           *  ./
/. *   JOB-ID      :    RRACOSPG2                            *  ./
/. *   JOB-NAME    :    日次振替                             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMES  ,STRING*5,VALUE-'     '
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'RACOSPG2'
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
    VAR       ?TAKAI1 ,STRING*14,VALUE-'/HGNAS/FURIKAE'
    VAR       ?TAKAI2 ,STRING*99                  /.多階層LIB名./
    VAR       ?MON    ,STRING*4,VALUE-'/MON'      /.月曜日./
    VAR       ?TUE    ,STRING*4,VALUE-'/TUE'      /.火曜日./
    VAR       ?WED    ,STRING*4,VALUE-'/WED'      /.水曜日./
    VAR       ?THU    ,STRING*4,VALUE-'/THU'      /.木曜日./
    VAR       ?FRI    ,STRING*4,VALUE-'/FRI'      /.金曜日./
    VAR       ?HIDUKE ,STRING*8,VALUE-'        '  /.日付./
    VAR       ?JIKAN  ,STRING*4,VALUE-'    '      /.時間./
    VAR       ?BUMON  ,STRING*4,VALUE-'    '      /.部門名./
    VAR       ?TANCD  ,STRING*2,VALUE-'  '        /.担当者CD./
    VAR       ?SOKCD1 ,STRING*2,VALUE-'  '        /.倉庫CD開始./
    VAR       ?SOKCD2 ,STRING*2,VALUE-'99'        /.倉庫CD終了./
    VAR       ?SYUDT1 ,STRING*8,VALUE-'00000000'  /.出荷日開始./
    VAR       ?SYUDT2 ,STRING*8,VALUE-'99999999'  /.出荷日終了./
    VAR       ?SKTCD1 ,STRING*8,VALUE-'        '  /.ｻｶﾀCD 開始./
    VAR       ?SKTCD2 ,STRING*8,VALUE-'99999999'  /.ｻｶﾀCD 終了./
    VAR       ?DENK1  ,STRING*2,VALUE-'70'        /.伝票区分１./
    VAR       ?DENK2  ,STRING*2,VALUE-'  '        /.伝票区分２./
    VAR       ?DENK3  ,STRING*2,VALUE-'  '        /.伝票区分３./
    VAR       ?DENK4  ,STRING*2,VALUE-'  '        /.伝票区分４./
    VAR       ?DENK5  ,STRING*2,VALUE-'  '        /.伝票区分５./
    VAR       ?DENK6  ,STRING*2,VALUE-'  '        /.伝票区分６./
    VAR       ?DENK7  ,STRING*2,VALUE-'  '        /.伝票区分７./
    VAR       ?DENK8  ,STRING*2,VALUE-'  '        /.伝票区分８./
    VAR       ?OUTKBN1,STRING*1,VALUE-' '         /.ＤＴ区分１./
    VAR       ?OUTKBN2,STRING*1,VALUE-'2'         /.ＤＴ区分２./

/.##ライブラリリスト登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB/TOKDTLIB/TOKDLIB
            /TOKMDLIB/TOKSOLIB

/.##ログインユーザー情報取得##./
SIT9000B:

    ?STEP :=   'SIT9000B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ログインユーザー情報取得 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-LOGINUSR,TOFILE-LOGINUSR.@TEMP
    CALL      PGM-SIT9000B.TOKELIBO,PARA-(?BUMON,?TANCD)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 :=  'ログイン情報取得'
              GOTO ABEND
    END

/.##実行日付／時刻取得##./
SJH8399B:

    ?STEP :=   'SJH8399B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 実行日付／時刻取得 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL  SJH8399B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
            ?KEKA4 := '実行日付／時刻取得'
                    GOTO ABEND
    END
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

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＡＣＯＳ振替エラー分再更新'

/.######################./
/.##振替データ受信要求##./
/.######################./


    ?OPR1  :=  '　＃＃＃＃　振替更新処理（再更新）　＃＃＃＃'
    ?OPR2  :=  '　ＡＣＯＳ振替処理でエラーになったデータの'
    ?OPR3  :=  '　再更新処理を行います。マスタへの登録は完'
    ?OPR4  :=  '　しておりますか？'
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ｴﾗｰ振替ﾃﾞｰﾀｺﾋﾟｰ##./
PSETUP:

    ?STEP :=   'PSETUP  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    ?MSGX :=  '## 振替データ初期化     ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-FURIKAF.TOKFLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データ初期化'
              GOTO ABEND END

    ?MSGX :=  '## 振替データコピー2    ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    OVRF      FILE-FURIKAE,TOFILE-FURIERR.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データコピー２（前回エラー分）'
              GOTO ABEND END

/.##振替更新処理##./
SFU0120B:

    ?PGMID := 'SFU0120B'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替データ更新       ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    OVRF FILE-FURIKAE,TOFILE-FURIERR.TOKFLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL PGM-SFU0120N.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
         ?KEKA4 := '振替データ更新'
         GOTO     ABEND
    END

/.##ＡＣＯＳ振替データ累積処理##./
SFU3200B:

    ?PGMID := 'SFU3200B'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## ＡＣＯＳ振替データ累積処理 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SFU3200B.TOKSOLIB,PARA-(?BUMON,?TANCD,?HIDUKE,?TIME)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
         ?KEKA4 := '振替リスト発行'
         GOTO     ABEND
    END

/.## 振替更新ﾃﾞｰﾀﾘｽﾄ発行(ｴﾗｰ以外)##./
SFU3230L:

    ?PGMID := 'SFU3230L'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替更新ﾃﾞｰﾀﾘｽﾄ発行(ｴﾗｰ以外)##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SFU3230L.TOKSOLIB,PARA-(?HIDUKE,?HIDUKE,?SOKCD1,?SOKCD2,
                                     ?SYUDT1,?SYUDT2,?SKTCD1,?SKTCD2,
         ?DENK1,?DENK2,?DENK3,?DENK4,?DENK5,?DENK6,?DENK7,?DENK8,
         ?OUTKBN1)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
         ?KEKA4 := '振替リスト発行（全て）'
         GOTO     ABEND
    END

/.## 振替更新ﾃﾞｰﾀﾘｽﾄ発行(ｴﾗｰ分)##./
SFU3231L:

    ?PGMID := 'SFU3231L'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替更新ﾃﾞｰﾀﾘｽﾄ発行(ｴﾗｰ分)  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL PGM-SFU3231L.TOKSOLIB,PARA-(?HIDUKE,?HIDUKE,?SOKCD1,?SOKCD2,
                                     ?SYUDT1,?SYUDT2,?SKTCD1,?SKTCD2,
         ?DENK1,?DENK2,?DENK3,?DENK4,?DENK5,?DENK6,?DENK7,?DENK8,
         ?OUTKBN2)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
         ?KEKA4 := '振替リスト発行（エラー）'
         GOTO     ABEND
    END

/.2010/02/03 種子販売計画実績管理システム　連携定義追加./
/.##振替ＤＴ→ＰＣ連携用Ｆ作成##./
SFU0500B:

    ?PGMID := 'SFU0500B'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替ＤＴ→ＰＣ連携用Ｆ作成 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF FILE-FURIKAL3,TOFILE-FURIKAL3.TOKFLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF FILE-TOKMS3,TOFILE-TOKMS3.TOKFLIB
    OVRF FILE-PCJISSL1,TOFILE-PCJISSL1.TOKKLIB
    CALL PGM-SFU0500B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
         ?KEKA4 := '振替ＤＴ→ＰＣ連携用Ｆ作成'
         GOTO     ABEND
    END

RTN:

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO
    ?KEKA1 :=  '再ＡＣＯＳ振替処理が正常終了しました。'
    ?KEKA2 :=  '振替リストを確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-?PGMEC

ABEND:

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO
    ?KEKA1 :=  '再ＡＣＯＳ振替処理が異常終了しました。'
    ?KEKA2 :=  'この画面をハードコピーしてＮＡＶへ連絡'
    ?KEKA3 :=  'して下さい。'
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

    RETURN    PGMEC-?PGMEC


```
