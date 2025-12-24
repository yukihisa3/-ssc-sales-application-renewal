# ACOSPG2

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/ACOSPG2.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫システム　　　　　　　           *  ./
/. *   JOB-ID      :    ACOSPG2                             *  ./
/. *   JOB-NAME    :    日次振替                             *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
/. *   UPDATE      :    2017/03/28 TAKAHASHI ACOS振替改善対応*  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMES  ,STRING*5,VALUE-'     '
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'ACOSPG2'
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
           /TOKSOLIB/TOKMDLIB

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
    ?PGNM :=  'ＡＣＯＳ振替データ受信'

/.######################./
/.##振替データ受信要求##./
/.######################./


    ?OPR1  :=  '　＃＃＃＃＃＃＃　振替更新処理　＃＃＃＃＃＃＃＃'
    ?OPR2  :=  '　　振替データの復元処理を開始致します。'
    ?OPR3  :=  '　　（電算室より振替データを受信します。）'
    ?OPR4  :=  '　　確認して下さい。（本社）'
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.####################./
/.##ＡＣＯＳ接続確立##./
/.####################./
    ?MSGX :=  '## ＡＣＯＳ接続確立開始 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNTFTPC RMTNAME-ACOSN,USER-'NEC23',ACCOUNT-'NEC'
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?MSGX :=  '## ＡＣＯＳ接続確立異常 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              ?KEKA4 := 'ＡＣＯＳ接続確立異常'
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ接続確立成功 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.########################./
/.##ＡＣＯＳ受信待ち合せ##./
/.########################./
    ?MSGX :=  '## ＡＣＯＳ受信待ち合せ ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL TWAIT30.XUCL

/.####################./
/.##ＡＣＯＳ振替受信##./
/.####################./
    ?MSGX :=  '## ＡＣＯＳ振替受信開始 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RCVFTPC FILE-'SSC.TOKFRR',TOENT-TOKFRR.TOKFLIB,DTYPE-@BINARY
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?MSGX :=  '## ＡＣＯＳ振替受信異常 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              ?KEKA4 := 'ＡＣＯＳ振替受信異常'
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ振替受信成功 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.########################./
/.##ＡＣＯＳ完了ＤＴ作成##./
/.########################./
    ?MSGX :=  '## ＡＣＯＳ完了Ｆ作成   ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-ACOSCHK1,TOFILE-ACOSCHK3.TOKWLIB
    CALL      PGM-SKY3001B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?MSGX :=  '## ＡＣＯＳ完了Ｆ  異常 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              ?KEKA4 := 'ＡＣＯＳ完了Ｆ　異常'
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ完了Ｆ　成功 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.######################./
/.##ＡＣＯＳ完了Ｆ送信##./
/.######################./
    ?MSGX :=  '## ＡＣＯＳ完了送信開始 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    SNDFTPC ENT-ACOSCHK3.TOKWLIB,TOFILE-'SSC.STOKDM3',DTYPE-@BINARY
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?MSGX :=  '## ＡＣＯＳ完了送信異常 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              ?KEKA4 := 'ＡＣＯＳ完了送信異常'
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ完了送信成功 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.########################./
/.##ＡＣＯＳ接続切り離し##./
/.########################./
    ?MSGX :=  '## ＡＣＯＳ接続切断開始 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    DCNTFTPC
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?MSGX :=  '## ＡＣＯＳ接続切断異常 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
              ?KEKA4 := 'ＡＣＯＳ接続切断異常'
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ接続切断成功 ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    END

/.##振替ｴﾗｰ部門毎振分け##./
SFU0100N:

    ?STEP :=   'SFU0100N'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-TOKU,TOFILE-TOKFRR.TOKFLIB
    OVRF      FILE-HON,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-FUK,TOFILE-TOKFFU.TOKFLIB
    OVRF      FILE-SEN,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-HOK,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-OSA,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-OKA,TOFILE-TOKFOK.TOKFLIB
    OVRF      FILE-BUTOKMF1,TOFILE-BUTOKML1.TOKFLIB
    OVRF      FILE-BUTOKMF2,TOFILE-BUTOKML2.TOKFLIB
    CALL      PGM-SFU0100N.TOKSOLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データ編集／振分　異常'
              GOTO ABEND END

/.##振替ｴﾗｰ部門毎振分ﾘｽﾄ##./
SKY2701L:

    ?STEP :=   'SKY2701L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-HOU,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-FKU,TOFILE-TOKFFU.TOKFLIB
    OVRF      FILE-SEU,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-HKU,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-OSU,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-OKU,TOFILE-TOKFOK.TOKFLIB
    CALL      PGM-SKY2701L.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替ＤＴ　部門別リスト異常'
              GOTO ABEND END

    ?OPR1  :=  '　＃＃＃＃＃＃　バックアップ処理_＃＃＃＃＃＃＃'
    ?OPR2  :=  '　ＡＣＯＳからの振替関係データのバックアップを　'
    ?OPR3  :=  '　を行います。　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  '　確認後、ＥＮＴＥＲを押して下さい。　　　　　　'
    ?OPR5  :=  '　キャンセルの場合は、更新されません！注意！　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    SAVFILE FILE-TOKFRR.TOKFLIB/TOKFHO.TOKFLIB/
            TOKFFU.TOKFLIB/TOKFSE.TOKFLIB/TOKFOK.TOKFLIB/
            TOKFHK.TOKFLIB/TOKFOS.TOKFLIB,TODEV-@NONE,
            MODE-@USED,TOPATH-?TAKAI2,REP-@YES,COMPRESS-@YES
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
            ?KEKA4 := '受信／振分ＤＴ　多階層退避異常'
            GOTO ABEND END

    ?OPR1  :=  '　＃＃＃＃＃＃　バックアップ処理_＃＃＃＃＃＃＃'
    ?OPR2  :=  ''
    ?OPR3  :=  ''
    ?OPR4  :=  ''
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##本社振替ﾃﾞｰﾀｺﾋﾟｰ##./
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

    ?MSGX :=  '## 振替データコピー1    ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    OVRF      FILE-FURIKAE,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データコピー１（本社）'
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

    ?MSGX :=  '## 振替データコピー3    ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    OVRF      FILE-FURIKAE,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データコピー３（仙台）'
              GOTO ABEND END

    ?MSGX :=  '## 振替データコピー4    ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    OVRF      FILE-FURIKAE,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データコピー４（北海道）'
              GOTO ABEND END

    ?MSGX :=  '## 振替データコピー5    ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    OVRF      FILE-FURIKAE,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データコピー５（西日本）'
              GOTO ABEND END

/.## 2012/05/18 九州分もセットするように変更 ##./
    ?MSGX :=  '## 振替データコピー6    ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    OVRF      FILE-FURIKAE,TOFILE-TOKFFU.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データコピー６（九州）'
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

/.##実績累積Ｆ作成（本社分）##./
SJS0020B:

    ?STEP :=   'SJS0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 実績累積Ｆ　累積（本社）    ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-TOKFHO,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-RUISEKF,TOFILE-RUISEKL1.TOKFLIB
    OVRF      FILE-HIDUKEL1,TOFILE-HIDUKEL1.TOKWLIB
    CALL      PGM-SJS0020B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '実績累積Ｆ　累積（本社）'
              GOTO ABEND END

/.##振替データ統合##./
TOUGOU01:

    ?STEP :=   'TOUGOU01'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替データ統合（九州）　　  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-TOKFFU.TOKFLIB,TOFILE-TOKFUALL.TOKWLIB,
            ADD-@NO,BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データ統合（九州）'
              GOTO ABEND END

/.##振替データ統合##./
TOUGOU02:

    ?STEP :=   'TOUGOU02'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替データ統合（仙台）　　  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-TOKFSE.TOKFLIB,TOFILE-TOKFUALL.TOKWLIB,
            ADD-@YES,BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データ統合（仙台）'
              GOTO ABEND END

/.##振替データ統合##./
TOUGOU03:

    ?STEP :=   'TOUGOU03'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替データ統合（北海道）　  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-TOKFHK.TOKFLIB,TOFILE-TOKFUALL.TOKWLIB,
            ADD-@YES,BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データ統合（北海道）'
              GOTO ABEND END

/.##振替データ統合##./
TOUGOU04:

    ?STEP :=   'TOUGOU04'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替データ統合（西日本）　  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-TOKFOS.TOKFLIB,TOFILE-TOKFUALL.TOKWLIB,
            ADD-@YES,BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データ統合（西日本）'
              GOTO ABEND END

/.##振替データ統合##./
TOUGOU05:

    ?STEP :=   'TOUGOU05'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替データ統合（岡山）　　  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CNVFILE FILE-TOKFOK.TOKFLIB,TOFILE-TOKFUALL.TOKWLIB,
            ADD-@YES,BF-1
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データ統合（岡山）'
              GOTO ABEND END

/.##実績累積Ｆ作成（九州／仙台／北海道／西日本／岡山）##./
SJS0021B:

    ?STEP :=   'SJS0021B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 実績累積Ｆ　累積（ＡＬＬ）  ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-TOKFHO,TOFILE-TOKFUALL.TOKWLIB
    OVRF      FILE-RUISEKF,TOFILE-RUISEKL1.TOKFLIB
    OVRF      FILE-HIDUKEL1,TOFILE-HIDUKEL1.TOKWLIB
    CALL      PGM-SJS0020B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '実績累積Ｆ　累積（ＡＬＬ）'
              GOTO ABEND END

/.##実績集計ファイル作成                                        ./
SJS0030B:

    ?STEP :=   'SJS0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 実績集計Ｆ　集計    ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-RUISEKL2,TOFILE-RUISEKL2.TOKFLIB
    OVRF      FILE-JISSYUL1,TOFILE-JISSYUL1.TOKFLIB
    OVRF      FILE-JISSSYU1,TOFILE-JISSSYU1.TOKKLIB  /.20120301 ADD./
    CALL      PGM-SJS0030B.TOKELIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '実績集計Ｆ　集計'
              GOTO ABEND END

/.##振替ｴﾗｰ部門毎振分け##./
SFU0200B:

    ?STEP :=   'SFU0200B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-TOKU,TOFILE-TOKFRR.TOKFLIB
    OVRF      FILE-HON,TOFILE-TOKFHO.TOKKLIB
    OVRF      FILE-FUK,TOFILE-TOKFFU.TOKKLIB
    OVRF      FILE-SEN,TOFILE-TOKFSE.TOKKLIB
    OVRF      FILE-HOK,TOFILE-TOKFHK.TOKKLIB
    OVRF      FILE-OSA,TOFILE-TOKFOS.TOKKLIB
    OVRF      FILE-OKA,TOFILE-TOKFOK.TOKKLIB
    OVRF      FILE-SHO,TOFILE-TOKSHO.TOKKLIB
    OVRF      FILE-SKY,TOFILE-TOKSKY.TOKKLIB
    OVRF      FILE-BUTOKMF1,TOFILE-BUTOKML1.TOKFLIB
    OVRF      FILE-BUTOKMF2,TOFILE-BUTOKML2.TOKFLIB
    CALL      PGM-SFU0200B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '振替データ編集／振分'
              GOTO ABEND END

/.##作業データ作成##./
SFU0210B:

    ?STEP :=   'SFU0210B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 作業実績データ作成（ストック本社）##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-SUTOKUF,TOFILE-TOKSHO.TOKKLIB
    OVRF      FILE-SGYFILF,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-HJYOKEN,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SFU0210B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '作業実績ＤＴ作成（ストック本社）'
              GOTO ABEND END

/.##作業データ作成 2014/01/07追加##./
SFU0210K:

    ?STEP :=   'SFU0210K'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 作業実績データ作成（ストック九州）##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRF      FILE-SUTOKUF,TOFILE-TOKSKY.TOKKLIB
    OVRF      FILE-SGYFILF,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-HJYOKEN,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SFU0210B.TOKELIBO
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '作業実績ＤＴ作成（ストック九州）'
              GOTO ABEND END

/.##九州データコピー##./
STKKYUSY:

    ?STEP :=   'STKKYUSY'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 九州用データバックアップ ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

/.##CNVFILE FILE-TOKSKY.TOKKLIB,TOFILE-TOKSKYS.TOKKLIB,BF-1
##./?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
              ?KEKA4 := '作業実績データ作成（九州分退避）'
              GOTO ABEND END

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

/.2014/01/10 振替データ日付毎バックアップ./
FURIBKUP:

    ?PGMID := 'FURIBKUP'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '##振替ＤＴ→日付毎バックアップ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CALL FURIBKUP.TOKCLIBO

/.##振替更新処理##./
PCLRFILE:

    ?PGMID := 'PCLRFILE'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES
    ?MSGX :=  '## 振替データクリア     ##'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    CLRFILE FILE-TOKFRR.TOKFLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
         ?KEKA4 := 'ＡＣＯＳ受信ＤＴ　初期化'
         GOTO     ABEND
    END
    CLRFILE FILE-TOKFHO.TOKFLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
         ?KEKA4 := '本社分ＤＴ　初期化'
         GOTO     ABEND
    END
    CLRFILE FILE-ZAIFURIK.TOKFLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMES    :=    @PGMES
    IF        ?PGMEC ^=   0    THEN
         ?KEKA4 := '在庫振分ＤＴ　初期化'
         GOTO     ABEND
    END

RTN:

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO
    ?KEKA1 :=  'ＡＣＯＳ振替処理が正常終了しました。'
    ?KEKA2 :=  '振替リストを確認して下さい。'
    ?KEKA3 :=  '九州分のデータがある場合は、九州営業課へ'
    ?KEKA4 :=  '連絡して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    RETURN    PGMEC-?PGMEC

ABEND:

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO
    ?KEKA1 :=  'ＡＣＯＳ振替処理が異常終了しました。'
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
