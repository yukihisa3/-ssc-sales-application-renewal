/. ***********************************************************  ./
/. *     サカタのタネ                                        *  ./
/. *   SYSTEM-NAME :                                         *  ./
/. *   JOB-ID      :    PKY18020                             *  ./
/. *   JOB-NAME    :    ユポラベル出力                       *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PKY18020'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    VAR       ?OPR1     ,STRING*50                /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                /.      2    ./
    VAR       ?OPR3     ,STRING*50                /.      3    ./
    VAR       ?OPR4     ,STRING*50                /.      4    ./
    VAR       ?OPR5     ,STRING*50                /.      5    ./
    ?NWKSTN   :=        @ORGWS
    ?WKSTN    :=        %STRING(?NWKSTN)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL   TOKELIB/TOKFLIB

/.##ユポラベル出力##./
SKY1802B:

    ?STEP :=   'SKY1802B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-ZYUPODT1,TOFILE-ZYUPODT1.TOKFLIB
    OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SKY1802B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##データ削除確認画面##./
PKAKUNIN:

    ?STEP :=   'PKAKUNIN'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '　　ユポラベルデータを削除しますか？確認ＯＫ？　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    /.##ユポラベルデータ削除##./
    CLRFILE   FILE-ZYUPODT.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
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