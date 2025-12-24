/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    ZSAV                                 *  ./
/. *   JOB-NAME    :    売上／仕入計上データ退避             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'ZSAV    '
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##実行端末取得##./
    ?WKSTN    :=      %STRING(@ORGWS)
    SNDMSG ?WKSTN,TO-XCTL
/.##実行端末チェック##./
ASS:

    DEFLIBL TOKFLIB/TOKELIB

    IF     ?WKSTN     ^=    'WKSTN001'
        THEN
        IF ?WKSTN ^= 'WKSTN003'
           THEN
            SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
            SNDMSG '＊　　　　　　　　　　　　　　　＊',TO-XCTL
            SNDMSG '＊この端末からは処理できません　＊',TO-XCTL
            SNDMSG '＊　　　　　　　　　　　　　　　＊',TO-XCTL
            SNDMSG '＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊',TO-XCTL
            GOTO   RTN
        END
    END

/.##退避処理（売上／仕入ＤＴ）##./
CNVFILE:

    ?STEP :=   'CNVFILE '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＃＃＃＃＃　日次データ作成　＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '　日次データ退避用のＦＰＤをセットして下さい。　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##############################################################./
    ?MSGX := '## 売上ﾃﾞｰﾀ退避中（本社） ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-TOKUWK.TOKWLIB,TOFILE-TOKU,DEV-@WSFPD,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 仕入ﾃﾞｰﾀ退避中（本社） ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-TOKUZ.TOKFLIB,TOFILE-TOKU,DEV-@WSFPD,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##############################################################./
    ?MSGX := '## 福岡ﾃﾞｰﾀ退避中（退避） ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-TOKUFKU.TOKFLIB,TOFILE-TOKUFKA.TOKFLIB,ADD-@NO,
            BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    CNVFILE FILE-TOKUFKS.TOKFLIB,TOFILE-TOKUFKA.TOKFLIB,ADD-@YES,
            BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 売上ﾃﾞｰﾀ退避中（福岡） ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-TOKUFKU.TOKFLIB,TOFILE-TOKU,DEV-@WSFPD,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 仕入ﾃﾞｰﾀ退避中（福岡） ##'
    SNDMSG MSG-?MSGX,TO-XCTL

/.  CNVFILE FILE-TOKUFKS.TOKFLIB,TOFILE-TOKU,DEV-@WSFPD,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
     ./
/.##############################################################./
    ?MSGX := '## 大阪ﾃﾞｰﾀ退避中（退避） ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-TOKUOSU.TOKFLIB,TOFILE-TOKUOSA.TOKFLIB,ADD-@NO,
            BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    CNVFILE FILE-TOKUOSS.TOKFLIB,TOFILE-TOKUOSA.TOKFLIB,ADD-@YES,
            BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 売上ﾃﾞｰﾀ退避中（大阪） ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-TOKUOSU.TOKFLIB,TOFILE-TOKU,DEV-@WSFPD,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '## 仕入ﾃﾞｰﾀ退避中（大阪） ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-TOKUOSS.TOKFLIB,TOFILE-TOKU,DEV-@WSFPD,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##############################################################./
/.##売上／仕入データ削除##./
    CLRFILE  TOKUWK.TOKWLIB
    CLRFILE  TOKUZ.TOKFLIB
/.##福岡売上／仕入データ削除##./
    CLRFILE  TOKUFKU.TOKFLIB
/.  CLRFILE  TOKUFKS.TOKFLIB  ./
/.##大阪売上／仕入データ削除##./
    CLRFILE  TOKUOSU.TOKFLIB
    CLRFILE  TOKUOSS.TOKFLIB

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