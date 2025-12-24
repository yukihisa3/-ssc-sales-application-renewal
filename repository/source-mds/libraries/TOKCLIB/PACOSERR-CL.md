# PACOSERR

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PACOSERR.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PACOSERR                             *  ./
/. *   JOB-NAME    :    売上作成ＦＬＧクリアー               *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PACOSERR'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##売上エラーデータ　コピー##./
    ?OPR1  :=  '　＃＃＃＃＃　日次更新エラー復元処理　＃＃＃＃＃　'
    ?OPR2  :=  '　　日次更新計上エラーデータを復元します。　　　　'
    ?OPR3  :=  '　　エラーデータＦＰＤをセットして下さい。　　　　'
    ?OPR4  :=  '　　確認して下さい。　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    CNVEXTF FILE-TERF,DEV-@WSFPD,TOFILE-TOKUERR.TOKFLIB,ADD-@NO

    DEFLIBL    TOKFLIB/TOKELIB

/.##エラーデータ振分##./
SKY2201B:
    ?STEP :=   'SKY2201B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKUERR,TOFILE-TOKUERR.TOKFLIB
    OVRF      FILE-TOKUHOE,TOFILE-TOKUHOE.TOKFLIB
    OVRF      FILE-TOKUFKE,TOFILE-TOKUFKE.TOKFLIB
    OVRF      FILE-TOKUOSE,TOFILE-TOKUOSE.TOKFLIB
    CALL      PGM-SKY2201B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##売上作成ＦＬＧクリアー##./
OSKT190:
    ?STEP :=   'OSKT190 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DENJNL3,TOFILE-SHTDENL3.TOKFLIB
    OVRF      FILE-TOKU,TOFILE-TOKUHOE.TOKFLIB
    OVRF      FILE-HKYOTU,TOFILE-WORKF.TOKFLIB
    CALL      PGM-OSKT190.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##売上エラーデータ確認リスト発行##./
OSKT200:

    ?STEP :=   'OSKT200 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    OVRF      FILE-HKYOTU,TOFILE-WORKF.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-OSKT200.TOKELIB
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
