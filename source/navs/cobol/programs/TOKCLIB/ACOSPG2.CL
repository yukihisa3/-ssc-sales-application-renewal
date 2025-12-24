/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫システム　　　　　　　           *  ./
/. *   JOB-ID      :    ACOSPG2                             *  ./
/. *   JOB-NAME    :    日次振替                             *  ./
/. *   UPDATE      :    2011/11/24 MIURA MOからLTOへ変更     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
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

/.##ライブラリリスト登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB

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
    SNDMSG    ?MSGX,TO-XCTL

    CNTFTPC RMTNAME-ACOSN,USER-'NEC23',ACCOUNT-'NEC'
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '## ＡＣＯＳ接続確立異常 ##'
              SNDMSG    ?MSGX,TO-XCTL
              ?KEKA4 := 'ＡＣＯＳ接続確立異常'
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ接続確立成功 ##'
              SNDMSG    ?MSGX,TO-XCTL
    END

/.########################./
/.##ＡＣＯＳ受信待ち合せ##./
/.########################./
    ?MSGX :=  '## ＡＣＯＳ受信待ち合せ ##'
    SNDMSG    ?MSGX,TO-XCTL

    CALL TWAIT30.XUCL

/.####################./
/.##ＡＣＯＳ振替受信##./
/.####################./
    ?MSGX :=  '## ＡＣＯＳ振替受信開始 ##'
    SNDMSG    ?MSGX,TO-XCTL

    RCVFTPC FILE-'SSC.TOKFRR',TOENT-TOKFRR.TOKFLIB,DTYPE-@BINARY
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '## ＡＣＯＳ振替受信異常 ##'
              SNDMSG    ?MSGX,TO-XCTL
              ?KEKA4 := 'ＡＣＯＳ振替受信異常'
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ振替受信成功 ##'
              SNDMSG    ?MSGX,TO-XCTL
    END

/.########################./
/.##ＡＣＯＳ完了ＤＴ作成##./
/.########################./
    ?MSGX :=  '## ＡＣＯＳ完了Ｆ作成   ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ACOSCHK1,TOFILE-ACOSCHK3.TOKWLIB
    CALL      PGM-SKY3001B.TOKELIB
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '## ＡＣＯＳ完了Ｆ  異常 ##'
              SNDMSG    ?MSGX,TO-XCTL
              ?KEKA4 := 'ＡＣＯＳ完了Ｆ　異常'
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ完了Ｆ　成功 ##'
              SNDMSG    ?MSGX,TO-XCTL
    END

/.######################./
/.##ＡＣＯＳ完了Ｆ送信##./
/.######################./
    ?MSGX :=  '## ＡＣＯＳ完了送信開始 ##'
    SNDMSG    ?MSGX,TO-XCTL

    SNDFTPC ENT-ACOSCHK3.TOKWLIB,TOFILE-'SSC.STOKDM3',DTYPE-@BINARY
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '## ＡＣＯＳ完了送信異常 ##'
              SNDMSG    ?MSGX,TO-XCTL
              ?KEKA4 := 'ＡＣＯＳ完了送信異常'
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ完了送信成功 ##'
              SNDMSG    ?MSGX,TO-XCTL
    END

/.########################./
/.##ＡＣＯＳ接続切り離し##./
/.########################./
    ?MSGX :=  '## ＡＣＯＳ接続切断開始 ##'
    SNDMSG    ?MSGX,TO-XCTL

    DCNTFTPC
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '## ＡＣＯＳ接続切断異常 ##'
              SNDMSG    ?MSGX,TO-XCTL
              ?KEKA4 := 'ＡＣＯＳ接続切断異常'
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ接続切断成功 ##'
              SNDMSG    ?MSGX,TO-XCTL
    END

/.##部門間移動ﾃﾞｰﾀｺﾋﾟｰ##./
BUMNCOPY:
/.  2005/02/03 停止 NAV ./
/.  ?STEP :=   'BUMNCOPY'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 部門間移動ﾃﾞｰﾀｺﾋﾟｰ   ##'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-ZAIFURIK.TOKFLIB,TOFILE-TOKFRR.TOKFLIB,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
./
/.##振替ｴﾗｰ部門毎振分け##./
SFU0100B:

    ?STEP :=   'SFU0100B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKU,TOFILE-TOKFRR.TOKFLIB
    OVRF      FILE-HON,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-FUK,TOFILE-TOKFFU.TOKFLIB
    OVRF      FILE-SEN,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-HOK,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-OSA,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-OKA,TOFILE-TOKFOK.TOKFLIB
    OVRF      FILE-BUTOKMF1,TOFILE-BUTOKML1.TOKFLIB
    OVRF      FILE-BUTOKMF2,TOFILE-BUTOKML2.TOKFLIB
    CALL      PGM-SFU0100B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データ編集／振分　異常'
              GOTO ABEND END

/.##振替ｴﾗｰ部門毎振分ﾘｽﾄ##./
SKY2701L:

    ?STEP :=   'SKY2701L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HOU,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-FKU,TOFILE-TOKFFU.TOKFLIB
    OVRF      FILE-SEU,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-HKU,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-OSU,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-OKU,TOFILE-TOKFOK.TOKFLIB
    CALL      PGM-SKY2701L.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替ＤＴ　部門別リスト異常'
              GOTO ABEND END

    ?OPR1  :=  '　＃＃＃＃＃＃　バックアップ処理　＃＃＃＃＃＃＃'
    ?OPR2  :=  ''
    ?OPR3  :=  ''
    ?OPR4  :=  ''
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    /.##振替ﾃﾞｰﾀ  ##./
/.  SAVFILE FILE-TOKFRR.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END  ./
    /.##本社ﾃﾞｰﾀ  ##./
/.  SAVFILE FILE-TOKFHO.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END  ./
    /.##福岡ﾃﾞｰﾀ  ##./
/.  SAVFILE FILE-TOKFFU.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END  ./
    /.##仙台ﾃﾞｰﾀ  ##./
/.  SAVFILE FILE-TOKFSE.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END  ./
    /.##岡山ﾃﾞｰﾀ  ##./
/.  SAVFILE FILE-TOKFOK.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END  ./
    /.##北海道ﾃﾞｰﾀ##./
/.  SAVFILE FILE-TOKFHK.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END  ./
    /.##大阪ﾃﾞｰﾀ  ##./
/.  SAVFILE FILE-TOKFOS.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END  ./
    SAVFILE FILE-TOKFRR.TOKFLIB/TOKFHO.TOKFLIB/
            TOKFFU.TOKFLIB/TOKFSE.TOKFLIB/TOKFOK.TOKFLIB/
            TOKFHK.TOKFLIB/TOKFOS.TOKFLIB,TODEV-LTO,
            MODE-@USED
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '受信／振分ＤＴ　ＬＴＯ退避異常'
            GOTO ABEND END

/.##本社振替ﾃﾞｰﾀｺﾋﾟｰ##./
PSETUP:

    ?STEP :=   'PSETUP  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ初期化     ##'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-FURIKAF.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.  SETPF FILE-TOKFHO.TOKFLIB,TOFILE-FURIKAF.TOKFLIB,ADD-@NO,
          ACTCHK-@NO ./
    ?MSGX :=  '## 振替データコピー1    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー１（本社）'
              GOTO ABEND END
/.  SETPF FILE-FURIERR.TOKFLIB,TOFILE-FURIKAF.TOKFLIB,ADD-@YES,
          ACTCHK-@NO ./
    ?MSGX :=  '## 振替データコピー2    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-FURIERR.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー２（前回エラー分）'
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー3    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー３（仙台）'
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー4    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー４（北海道）'
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー5    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー５（西日本）'
              GOTO ABEND END
/.## 2012/05/18 九州分もセットするように変更 ##./
    ?MSGX :=  '## 振替データコピー6    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFFU.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー６（九州）'
              GOTO ABEND END

/.2010/02/03 ﾘｽﾄ発行⇒更新の順番を、更新⇒ﾘｽﾄ発行に変更./
/.##振替更新処理##./
SFU0120L:

    ?PGMID := 'SFU0120B'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ更新       ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    OVRF FILE-FURIKAE,TOFILE-FURIERR.TOKFLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL PGM-SFU0120B.TOKELIB
    IF   @PGMEC  ^=  0  THEN
         ?KEKA4 := '振替データ更新'
         GOTO     ABEND
    END

/.##日次振替ﾘｽﾄ発行##./
SFU0110L:

    ?PGMID := 'SFU0110L'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替リスト発行       ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-FURIKAL2,TOFILE-FURIKAL2.TOKFLIB
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF FILE-BUTOKML1,TOFILE-BUTOKML1.TOKFLIB
    OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    CALL PGM-SFU0110L.TOKELIB
    IF   @PGMEC  ^=  0  THEN
         ?KEKA4 := '振替リスト発行'
         GOTO     ABEND
    END

/.##実績累積Ｆ作成（本社分）##./
SJS0020B:

    ?STEP :=   'SJS0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 実績累積Ｆ　累積（本社）    ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKFHO,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-RUISEKF,TOFILE-RUISEKL1.TOKFLIB
    OVRF      FILE-HIDUKEL1,TOFILE-HIDUKEL1.TOKWLIB
    CALL      PGM-SJS0020B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '実績累積Ｆ　累積（本社）'
              GOTO ABEND END

/.##振替データ統合##./
TOUGOU01:

    ?STEP :=   'TOUGOU01'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ統合（九州）　　  ##'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-TOKFFU.TOKFLIB,TOFILE-TOKFUALL.TOKWLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データ統合（九州）'
              GOTO ABEND END

/.##振替データ統合##./
TOUGOU02:

    ?STEP :=   'TOUGOU02'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ統合（仙台）　　  ##'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-TOKFSE.TOKFLIB,TOFILE-TOKFUALL.TOKWLIB,
            ADD-@YES,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データ統合（仙台）'
              GOTO ABEND END

/.##振替データ統合##./
TOUGOU03:

    ?STEP :=   'TOUGOU03'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ統合（北海道）　  ##'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-TOKFHK.TOKFLIB,TOFILE-TOKFUALL.TOKWLIB,
            ADD-@YES,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データ統合（北海道）'
              GOTO ABEND END

/.##振替データ統合##./
TOUGOU04:

    ?STEP :=   'TOUGOU04'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ統合（西日本）　  ##'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-TOKFOS.TOKFLIB,TOFILE-TOKFUALL.TOKWLIB,
            ADD-@YES,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データ統合（西日本）'
              GOTO ABEND END

/.##振替データ統合##./
TOUGOU05:

    ?STEP :=   'TOUGOU05'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ統合（岡山）　　  ##'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-TOKFOK.TOKFLIB,TOFILE-TOKFUALL.TOKWLIB,
            ADD-@YES,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データ統合（岡山）'
              GOTO ABEND END

/.##実績累積Ｆ作成（九州／仙台／北海道／西日本／岡山）##./
SJS0021B:

    ?STEP :=   'SJS0021B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 実績累積Ｆ　累積（ＡＬＬ）  ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKFHO,TOFILE-TOKFUALL.TOKWLIB
    OVRF      FILE-RUISEKF,TOFILE-RUISEKL1.TOKFLIB
    OVRF      FILE-HIDUKEL1,TOFILE-HIDUKEL1.TOKWLIB
    CALL      PGM-SJS0020B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '実績累積Ｆ　累積（ＡＬＬ）'
              GOTO ABEND END

/.##実績集計ファイル作成                                        ./
SJS0030B:

    ?STEP :=   'SJS0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 実績集計Ｆ　集計    ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-RUISEKL2,TOFILE-RUISEKL2.TOKFLIB
    OVRF      FILE-JISSYUL1,TOFILE-JISSYUL1.TOKFLIB
    OVRF      FILE-JISSSYU1,TOFILE-JISSSYU1.TOKKLIB  /.20120301 ADD./
    CALL      PGM-SJS0030B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '実績集計Ｆ　集計'
              GOTO ABEND END

/.##振替ｴﾗｰ部門毎振分け##./
SFU0200B:

    ?STEP :=   'SFU0200B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL

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
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データ編集／振分'
              GOTO ABEND END

/.##作業データ作成##./
SFU0210B:

    ?STEP :=   'SFU0210B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 作業実績データ作成（ストック本社）##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SUTOKUF,TOFILE-TOKSHO.TOKKLIB
    OVRF      FILE-SGYFILF,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-HJYOKEN,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SFU0210B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '作業実績ＤＴ作成（ストック本社）'
              GOTO ABEND END

/.##作業データ作成 2014/01/07追加##./
SFU0210K:

    ?STEP :=   'SFU0210K'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 作業実績データ作成（ストック九州）##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SUTOKUF,TOFILE-TOKSKY.TOKKLIB
    OVRF      FILE-SGYFILF,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-HJYOKEN,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SFU0210B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '作業実績ＤＴ作成（ストック九州）'
              GOTO ABEND END

/.##九州データコピー##./
STKKYUSY:

    ?STEP :=   'STKKYUSY'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 九州用データバックアップ ##'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-TOKSKY.TOKKLIB,TOFILE-TOKSKYS.TOKKLIB,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '作業実績データ作成（九州分退避）'
              GOTO ABEND END

/.2010/02/03 種子販売計画実績管理システム　連携定義追加./
/.##振替ＤＴ→ＰＣ連携用Ｆ作成##./
SFU0500B:

    ?PGMID := 'SFU0500B'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替ＤＴ→ＰＣ連携用Ｆ作成 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-FURIKAL3,TOFILE-FURIKAL3.TOKFLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF FILE-TOKMS3,TOFILE-TOKMS3.TOKFLIB
    OVRF FILE-PCJISSL1,TOFILE-PCJISSL1.TOKKLIB
    CALL PGM-SFU0500B.TOKELIBO
    IF   @PGMEC  ^=  0  THEN
         ?KEKA4 := '振替ＤＴ→ＰＣ連携用Ｆ作成'
         GOTO     ABEND
    END

/.2014/01/10 振替データ日付毎バックアップ./
FURIBKUP:

    ?PGMID := 'FURIBKUP'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '##振替ＤＴ→日付毎バックアップ##'
    SNDMSG    ?MSGX,TO-XCTL

    CALL FURIBKUP.TOKCLIBO

/.##振替更新処理##./
PCLRFILE:

    ?PGMID := 'PCLRFILE'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データクリア     ##'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-TOKFRR.TOKFLIB
    IF   @PGMEC  ^=  0  THEN
         ?KEKA4 := 'ＡＣＯＳ受信ＤＴ　初期化'
         GOTO     ABEND
    END
    CLRFILE FILE-TOKFHO.TOKFLIB
    IF   @PGMEC  ^=  0  THEN
         ?KEKA4 := '本社分ＤＴ　初期化'
         GOTO     ABEND
    END
    CLRFILE FILE-ZAIFURIK.TOKFLIB
    IF   @PGMEC  ^=  0  THEN
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
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

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
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC

