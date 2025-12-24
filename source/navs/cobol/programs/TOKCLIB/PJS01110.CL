/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    実績累積管理                         *  ./
/. *   JOB-ID      :    PJS01110                             *  ./
/. *   JOB-NAME    :    実績累積Ｆ削除                       *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PJS01110'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGNM     ,STRING*24!MIXED            /.ＰＧ漢字名 ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    DEFLIBL TOKELIB/TOKFLIB

/.確認画面./
PKAKUNIN:

    ?OPR1  :=  '　＃＃＃＃＃　　受払累積Ｆ削除処理　　＃＃＃＃＃　'
    ?OPR2  :=  '　受払累積Ｆの削除処理を行います。'
    ?OPR3  :=  ''
    ?OPR4  :=  ''
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.実績累積Ｆ保有期間外データ削除./
SJS0110B:

    ?STEP :=   'SJS0110B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃実績累積Ｆ　削除中＃',TO-XCTL

    OVRF FILE-GERUISKF,TOFILE-GERUISKF.TOKBLIB
    OVRF FILE-RUISEKL4,TOFILE-RUISEKL4.TOKFLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  SJS0110B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.実績累積Ｆ保有期間外データ削除./
PCNDPF:

    ?STEP :=   'PCNDPF  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃実績累積Ｆ　圧縮中＃',TO-XCTL

    DACTLF FILE-RUISEKL1.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKL1) ERR ##',TO-XCTL
              GOTO ABEND END
    DACTLF FILE-RUISEKL2.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKL2) ERR ##',TO-XCTL
              GOTO ABEND END
    DACTLF FILE-RUISEKL3.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKL3) ERR ##',TO-XCTL
              GOTO ABEND END
    DACTLF FILE-RUISEKL4.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## DACTLF(RUISEKL4) ERR ##',TO-XCTL
              GOTO ABEND END
    CNDPF FILE-RUISEKF.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## CNDPF(RUISEKFG)  ERR ##',TO-XCTL
              GOTO ABEND END
    ACTLF FILE-RUISEKL1.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKL1)  ERR ##',TO-XCTL
              GOTO ABEND END
    ACTLF FILE-RUISEKL2.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKL2)  ERR ##',TO-XCTL
              GOTO ABEND END
    ACTLF FILE-RUISEKL3.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKL3)  ERR ##',TO-XCTL
              GOTO ABEND END
    ACTLF FILE-RUISEKL4.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              SNDMSG MSG-'## ACTLF(RUISEKL4)  ERR ##',TO-XCTL
              GOTO ABEND END

/.  ?OPR1  :=  '　＃＃＃＃＃　　受払累積Ｆ退避処理　　＃＃＃＃＃　'
    ?OPR2  :=  '　削除した受払累積Ｆデータのバックアップを行いま'
    ?OPR3  :=  '　す。再度，月次前データ退避処理で使用したＭＯを'
    ?OPR4  :=  '　セットして下さい。'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    SAVFILE FILE-GERUISKF.TOKBLIB,TODEV-MO,MODE-@USED
           ./
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