/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    SAVTOKU                              *  ./
/. *   JOB-NAME    :    売り上げデータ再退避                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'SAVTOKU '
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?WKSTN    :=      %STRING(@ORGWS)
    SNDMSG ?WKSTN,TO-XCTL

ASS:


    DEFLIBL TOKFLIB/TOKELIB

    IF     ?WKSTN     ^=    'WKSTN002'
        THEN
            SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
            SNDMSG '＊　　　　　　　　　　　　　　　＊',TO-XCTL
            SNDMSG '＊この端末からは処理できません　＊',TO-XCTL
            SNDMSG '＊　　　　　　　　　　　　　　　＊',TO-XCTL
            SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
            GOTO   RTN   END

    ?OPR1  :=  '　＃＃＃＃＃＃＃　再退避処理　＃＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '　　日次データの再退避処理を行います。　　　　　　'
    ?OPR4  :=  '　　退避用ＦＰＤをセットして下さい。　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.  退避処理（ＴＯＫＵデータ）                                  ./
PTAIHI04:

    ?STEP :=   'PTAIHI04'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'***   TOKUﾃﾞｰﾀ　複写中   ***',TO-XCTL

    CNVFILE FILE-TOKU.TOKFLIB,TOFILE-TOKU,DEV-@WSFPD,ADD-@NO
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