# KANRIF1

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/KANRIF1.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　ＣＶＣＳ再創成処理（本社）            *  ./
/. *   SYSTEM-NAME :    ＩＮＳ回線        　　　　           *  ./
/. *   JOB-ID      :    KANRIF1                              *  ./
/. *   JOB-NAME    :    ＣＶＣＳ管理Ｆを再創成する。         *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'KANRIF1 '
    VAR       ?STEP     ,STRING*8

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##運用状況リスト出力##./
SYORI1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG 'ｳﾝﾖｳｼﾞｮｳｷｮｳ ﾘｽﾄ (ISDN)',TO-XCTL

    CALL CLI1010.TOKELIB

/.##ＶＬＤ非活性化##./
SYORI2:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG 'VLD ﾋｶｯｾｲｶ 900.901.902.800.801.802',TO-XCTL

    DACTVLD VLD-900
    DACTVLD VLD-901
    DACTVLD VLD-902
    DACTVLD VLD-800
    DACTVLD VLD-801
    DACTVLD VLD-802

/.監視ＪＯＢキャンセル                                        ./
SYORI3:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG 'ｶﾝｼ ｼﾞｮﾌﾞ STOP (KANSI)',TO-XCTL

    CANJOB RUISEKII.SAKATA1
    CANJOB RUISEKIT.SAKATA1

/.管理ファイル再創成                                          ./
SYORI4:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG 'ｶﾝﾘﾌｧｲﾙ ｻｲｿｳｾｲ  (ISDN)',TO-XCTL

    CALL SKTCVCS1.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.監視ＪＯＢ起動                                              ./
SYORI5:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG 'ｶﾝｼ ｼﾞｮﾌﾞ START (ISDN)',TO-XCTL

    SBMJOB JOB-RUISEKII,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-PVIS1002.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    SBMJOB JOB-RUISEKIT,JOBD-CVCS.XUCL,JOBK-@B,
           PGM-PVTS1002.TOKELIB
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
