# PSSEK090

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PSSEK090.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    セキチューオンラインシステム         *  ./
/. *   JOB-ID      :    PSSEK090                             *  ./
/. *   JOB-NAME    :    商品台帳発行                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSSEK090'
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  SNDMSG    '***  商品台帳発行処理 ***',TO-XCTL        ./

    DEFLIBL   TOKELIB/TOKFLIB
/.  商品台帳発行　　　　　　　　　                             ./
SSEK090B:

    ?STEP :=   'SSEK090B'
      ?MSGX :=  '***   '  && ?STEP   &&   '        ***'

    OVRF      FILE-SKSHOHIN,TOFILE-SKSHOHIN.TOKFLIB

    OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIB
    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRDSPF   FILE-DSPFILE,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL      PGM-SSEK090B.TOKELIB
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
