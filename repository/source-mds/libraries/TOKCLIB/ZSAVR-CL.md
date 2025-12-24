# ZSAVR

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/ZSAVR.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    ZSAVR                                *  ./
/. *   JOB-NAME    :    売上／仕入計上データ退避（再）       *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'ZSAVR   '
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##実行端末名称取得##./
    ?WKSTN    :=      %STRING(@ORGWS)
    SNDMSG ?WKSTN,TO-XCTL
/.##実行端末チェック##./
ASS:

    DEFLIBL TOKELIB/TOKFLIB

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


/.##退避処理（売上／仕入計上データ）##./
CNVFILE:

    ?STEP :=   'CNVFILE '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＃＃＃　（再）日次データ作成　＃＃＃＃＃＃　'
    ?OPR2  :=  '　日次データの再作成処理を行います。　　　　　　　'
    ?OPR3  :=  '　日次データ退避用ＦＰＤをセットして下さい。　　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##############################################################./
    ?MSGX := '## 売上／仕入ﾃﾞｰﾀ（再）作成 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    ?MSGX := '## 本社分　　再退避中　　　 ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-TOKU.TOKWLIB,TOFILE-TOKU,DEV-@WSFPD,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##############################################################./
    ?MSGX := '## 売上／仕入ﾃﾞｰﾀ（再）作成 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    ?MSGX := '## 福岡分　　再退避中　　　 ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-TOKUFKA.TOKFLIB,TOFILE-TOKU,DEV-@WSFPD,
            ADD-@YES,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##############################################################./
    ?MSGX := '## 売上／仕入ﾃﾞｰﾀ（再）作成 ##'
    SNDMSG MSG-?MSGX,TO-XCTL
    ?MSGX := '## 大阪分　　再退避中　　　 ##'
    SNDMSG MSG-?MSGX,TO-XCTL

    CNVFILE FILE-TOKUOSA.TOKFLIB,TOFILE-TOKU,DEV-@WSFPD,
            ADD-@YES,BF-1
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

```
