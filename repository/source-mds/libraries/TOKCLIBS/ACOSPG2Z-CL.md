# ACOSPG2Z

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/ACOSPG2Z.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫システム　　　　　　　           *  ./
/. *   JOB-ID      :    ACOSPG2                             *  ./
/. *   JOB-NAME    :    日次振替                             *  ./
/. *               :                                         *  ./
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

    DEFLIBL TOKELIB/TOKFLIB
/.######################./
/.##振替データ受信要求##./
/.######################./


    ?OPR1  :=  '　＃＃＃＃＃＃＃　振替更新処理　＃＃＃＃＃＃＃＃'
    ?OPR2  :=  '　　振替データの復元処理を開始致します。'
    ?OPR3  :=  '　　（電算室より振替データを受信します。）'
    ?OPR4  :=  '　　確認して下さい。（本社）'
    ?OPR5  :=  '　※データバックアップ用ＭＯをセットして下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

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
              GOTO ABEND END

    ?OPR1  :=  '　＃＃＃＃＃＃　バックアップ処理　＃＃＃＃＃＃＃'
    ?OPR2  :=  '　　ＭＯのセットＯＫですか？　　　　　　　　　　'
    ?OPR3  :=  '　　確認して下さい。　　　　　　　　　　　　　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　　　　　　　　　　　　　　　　　　　　　　　　'
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
            TOKFHK.TOKFLIB/TOKFOS.TOKFLIB,TODEV-MO,
            MODE-@USED
    IF        @PGMEC    ^=   0    THEN
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
              GOTO ABEND END
/.  SETPF FILE-FURIERR.TOKFLIB,TOFILE-FURIKAF.TOKFLIB,ADD-@YES,
          ACTCHK-@NO ./
    ?MSGX :=  '## 振替データコピー2    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-FURIERR.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー3    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー4    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー5    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

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
    IF   @PGMEC      ^=  0
       THEN GOTO     ABEND
    END

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
    IF   @PGMEC      ^=  0
       THEN GOTO     ABEND
    END

/.##実績累積Ｆ作成##./
SJS0020B:

    ?STEP :=   'SJS0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 実績累積Ｆ　集計    ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKFHO,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-RUISEKF,TOFILE-RUISEKL1.TOKFLIB
    CALL      PGM-SJS0020B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
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
    CALL      PGM-SJS0030B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
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
              GOTO ABEND END

/.##作業データ作成##./
SFU0210B:

    ?STEP :=   'SFU0210B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SUTOKUF,TOFILE-TOKSHO.TOKKLIB
    OVRF      FILE-SGYFILF,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-HJYOKEN,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SFU0210B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
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
              GOTO ABEND END

/.##振替更新処理##./
PCLRFILE:

    ?PGMID := 'PCLRFILE'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データクリア     ##'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-TOKFRR.TOKFLIB
    IF   @PGMEC      ^=  0
       THEN GOTO     ABEND
    END
    CLRFILE FILE-TOKFHO.TOKFLIB
    IF   @PGMEC      ^=  0
       THEN GOTO     ABEND
    END
    CLRFILE FILE-ZAIFURIK.TOKFLIB
    IF   @PGMEC      ^=  0
       THEN GOTO     ABEND
    END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

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


```
