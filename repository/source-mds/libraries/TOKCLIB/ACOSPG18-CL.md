# ACOSPG18

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/ACOSPG18.CL`

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


    ?OPR1  :=  '　＃（特別処理）　振替更新処理　（特別処理）＃＃'
    ?OPR2  :=  '　　振替データの復元処理を開始致します。'
    ?OPR3  :=  '　　（電算室より振替データを受信します。）'
    ?OPR4  :=  '　　確認して下さい。（本社）'
    ?OPR5  :=  '　※データバックアップ用ＭＯをセットして下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##並べ変え##./
PSORT:

    ?STEP :=   'PSORT   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SORT    INFILE-TOKFRR18.NAVLIB,INRL-256,INBF-1,
            OUTFILE-TOKFRR18.NAVLIB,OUTBF-1,
            KEY-1!26!CA,
            RCDL-@DSP

    IF      @PGMEC    ^=   0    THEN
            CALL SCVMSG1.TOKELIB,PARA-('SORT ERR            ')
            GOTO ABEND
    END

/.##本社振替ﾃﾞｰﾀｺﾋﾟｰ##./
SKY3101B:

    ?STEP :=   'SKY3101B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データコピー     ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ACOS,TOFILE-TOKFRR18.NAVLIB
    OVRF      FILE-ACOSOK,TOFILE-TOKFHOOK.TOKWLIB
    OVRF      FILE-ACOSERR,TOFILE-TOKFHONG.TOKWLIB
/.  CALL      PGM-SKY3101B.TOKELIB     ./
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##振替ｴﾗｰ部門毎振分け##./
SFU0104B:

    ?STEP :=   'SFU0104B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKU,TOFILE-TOKFHOOK.TOKWLIB
    OVRF      FILE-HON,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-FUK,TOFILE-TOKFFU.TOKFLIB
    OVRF      FILE-SEN,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-HOK,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-OSA,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-OKA,TOFILE-TOKFOK.TOKFLIB
    OVRF      FILE-BUTOKMF1,TOFILE-BUTOKML1.TOKFLIB
    OVRF      FILE-BUTOKMF2,TOFILE-BUTOKML2.TOKFLIB
/.  CALL      PGM-SFU0104B.TOKELIB   ./
    IF        @PGMEC    ^=   0    THEN
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
/.  CALL      PGM-SKY2701L.TOKELIB   ./
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
              GOTO ABEND END

    SAVFILE FILE-TOKFHO.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    SAVFILE FILE-TOKFFU.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    SAVFILE FILE-TOKFSE.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    SAVFILE FILE-TOKFOK.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    SAVFILE FILE-TOKFHK.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    SAVFILE FILE-TOKFOS.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END   ./

/.##本社振替ﾃﾞｰﾀｺﾋﾟｰ##./
PSETUP:

    ?STEP :=   'PSETUP  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データコピー     ##'
    SNDMSG    ?MSGX,TO-XCTL

/.  SETPF FILE-TOKFHO.TOKFLIB,TOFILE-FURIKAF.TOKFLIB,ADD-@NO,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    SETPF FILE-FURIERR.TOKFLIB,TOFILE-FURIKAF.TOKFLIB,ADD-@YES,
          ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
  ./
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
/.  CALL PGM-SFU0110L.TOKELIB   ./
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
/.  CALL PGM-SFU0120B.TOKELIB  ./
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
