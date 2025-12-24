/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム） 新基幹   *  ./
/. *   SYSTEM-NAME :    出荷管理サブシステム                 *  ./
/. *   JOB-ID      :    PSKT150A                             *  ./
/. *   JOB-NAME    :    伝票_　付番入力                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSKT150A'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1    ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2    ,STRING*50                  /.      2    ./
    VAR       ?OPR3    ,STRING*50                  /.      3    ./
    VAR       ?OPR4    ,STRING*50                  /.      4    ./
    VAR       ?OPR5    ,STRING*50                  /.      5    ./
    DEFLIBL TOKELIB/TOKFLIB
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##伝票_付番入力##./
OSKT150A:
    /.##他端末実行確認##./
/.  ASSIGN FILE-OSKT150A.TOKFLIB!@XCL
    IF        @PGMEC    ^=   0    THEN  GOTO ERR END
            ./
    ?STEP :=   'OSKT150A'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-DENJNL2,TOFILE-SHTDENL2.TOKFLIB
    OVRF      FILE-DENJNL15,TOFILE-SHTDENL5.TOKFLIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-OSKT150.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
              ELSE
              GOTO RTN END

ERR:
    ?OPR1  :=  '＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    ?OPR2  :=  '＃　　　　　　　　　　　　　　　　　　　　　　＃'
    ?OPR3  :=  '＃　　　　　他端末にて使用中です。　　　　　　＃'
    ?OPR4  :=  '＃　　　　　　　　　　　　　　　　　　　　　　＃'
    ?OPR5  :=  '＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    GOTO OSKT150A

RTN:

    RELEASE FILE-JYOKEN1.TOKFLIB!@XCL
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    RELEASE FILE-JYOKEN1.TOKFLIB!@XCL
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