/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫システム　　　　　　　           *  ./
/. *   JOB-ID      :    PFU02000                             *  ./
/. *   JOB-NAME    :    ストックＮＯ外しデータ作成           *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PFU02000'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    DEFLIBL NAVWORK/TOKELIB/TOKFLIB

    ?OPR1  :=  '　＃＃＃＃＃ストックＮＯ外し処理＃＃＃＃＃'
    ?OPR2  :=  '　振替データからストックＮＯ外しデータを'
    ?OPR3  :=  '　作成します。'
    ?OPR4  :=  ''
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##振替ｴﾗｰ部門毎振分け##./
SFU0200B:

    ?STEP :=   'SFU0200B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKU,TOFILE-TOKFRR.NAVWORK
    OVRF      FILE-HON,TOFILE-TOKFHO.NAVWORK
    OVRF      FILE-FUK,TOFILE-TOKFFU.NAVWORK
    OVRF      FILE-SEN,TOFILE-TOKFSE.NAVWORK
    OVRF      FILE-HOK,TOFILE-TOKFHK.NAVWORK
    OVRF      FILE-OSA,TOFILE-TOKFOS.NAVWORK
    OVRF      FILE-OKA,TOFILE-TOKFOK.NAVWORK
    OVRF      FILE-SHO,TOFILE-TOKSHO.NAVWORK
    OVRF      FILE-SKY,TOFILE-TOKSKY.NAVWORK
    OVRF      FILE-BUTOKMF1,TOFILE-BUTOKML1.TOKFLIB
    OVRF      FILE-BUTOKMF2,TOFILE-BUTOKML2.TOKFLIB
    CALL      PGM-SFU1200B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##作業データ作成##./
SFU0210B:

    ?STEP :=   'SFU0210B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SUTOKUF,TOFILE-TOKSHO.NAVWORK
    OVRF      FILE-SGYFILF,TOFILE-SGYFILL1.NAVWORK
    OVRF      FILE-HJYOKEN,TOFILE-JYOKEN1.NAVWORK
    CALL      PGM-SFU1210B.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##九州データコピー##./
STKKYUSY:

    ?STEP :=   'STKKYUSY'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 九州用データバックアップ ##'
    SNDMSG    ?MSGX,TO-XCTL

/.  CNVFILE FILE-TOKSKY.NAVWORK,TOFILE-TOKSKYS.NAVWORK,BF-1
 ./ IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
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
