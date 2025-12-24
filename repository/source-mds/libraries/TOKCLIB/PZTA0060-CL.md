# PZTA0060

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PZTA0060.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理システム                     *  ./
/. *   JOB-ID      :    PZTA0060                             *  ./
/. *   JOB-NAME    :    実_更新                             *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PZTA0060'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL


/.  在庫管理システム                                           ./
PZTA0055:

    ?OPR1  :=  '　＃＃＃＃＃＃＃　_卸確定更新処理　＃＃＃＃＃＃　'
    ?OPR2  :=  '　_卸確定処理処理を行います。　　　　　　　　　　'
    ?OPR3  :=  '　宜しいですか？　　　　　　　　　　　　　　　　　'
    ?OPR4  :=  '　＜処理続行＝入力／実行　ＰＦ９＝処理中止＞　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?OPR1  :=  '　＃＃＃＃＃＃＃　_卸確定更新処理　＃＃＃＃＃＃　'
    ?OPR2  :=  '　在庫ファイルのバックアップを行います。　　　　　'
    ?OPR3  :=  '　ＭＯをセットして下さい。　　　　　　　　　　　　'
    ?OPR4  :=  '　＜処理続行＝入力／実行　ＰＦ９＝処理中止＞　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    SAVFILE FILE-ZZAIMS.TOKFLIB,TODEV-MO,ADD-@NO

/. ./
PZTA0065:

    ?STEP :=   'ZTA0065B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZZAIMS1,TOFILE-ZZAIMS1.TOKFLIB
    OVRF      FILE-ZZAIMS,TOFILE-ZZAIMS.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-ZTA0065B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?STEP :=   'ZTA0055B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZTANADT2,TOFILE-ZTANADT2.TOKFLIB
    OVRF      FILE-OUTFILE,TOFILE-ZTANAWK.TOKFLIB
    CALL      PGM-ZTA0055B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

PZTA0060:

    ?STEP :=   'ZTA0060B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ZTANADT,TOFILE-ZTANAWK.TOKFLIB
    OVRF      FILE-ZZAIMS1,TOFILE-ZZAIMS1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    CALL      PGM-ZTA0060B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

PZTA0069:

    ?STEP :=   'ZTA0069B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-ZTA0069B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

REC0001:

    DLTOVRF   FILE-*

    CALL      HIKIATE.TOKELIB

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
