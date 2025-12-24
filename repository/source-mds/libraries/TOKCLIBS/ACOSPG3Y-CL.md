# ACOSPG3Y

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/ACOSPG3Y.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    ACOSPG3                             *  ./
/. *   JOB-NAME    :    売上作成ＦＬＧクリアー               *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'ACOSPG3'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.############################./
/.##売上エラーデータ受信要求##./
/.############################./
    ?OPR1  :=  '　＃＃＃＃＃　日次更新エラー復元処理　＃＃＃＃＃　'
    ?OPR2  :=  '　　日次更新計上エラーデータの受信を行います。'
    ?OPR3  :=  '　　（電算室よりエラーデータを受信します。）'
    ?OPR4  :=  '　　確認して下さい。'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.  CNVEXTF FILE-TERF,DEV-@WSFPD,TOFILE-TOKUERR.TOKFLIB,ADD-@NO./

    DEFLIBL    TOKFLIB/TOKELIB

    ?OPR1  :=  '　＃＃＃＃＃　日次更新エラー復元処理　＃＃＃＃＃　'
    ?OPR2  :=  '　　電算室殿より，計上エラーデータの受信処理を'
    ?OPR3  :=  '　　行いました。本社エラー振分件数を確認し，エラー'
    ?OPR4  :=  '　　を戻すか判断して下さい。'
    ?OPR5  :=  '　ＰＦ９：エラー戻し無し，実行：エラー戻し有り　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##売上作成ＦＬＧクリアー##./
TSY8010B:
    ?STEP :=   'TSY8010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-DENJNL3,TOFILE-SHTDENL3.TOKFLIB
    OVRF      FILE-NYKFILL1,TOFILE-NYKFILL1.TOKFLIB
    OVRF      FILE-NYSFILL1,TOFILE-NYSFILL1.TOKFLIB
    OVRF      FILE-SGYFILL1,TOFILE-SGYFILL1.TOKFLIB
    OVRF      FILE-TOKU,TOFILE-TOKUHOE.TOKFLIB
    OVRF      FILE-HKYOTU,TOFILE-WORKF.TOKFLIB
    CALL      PGM-TSY8010B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##実績累積Ｆ戻し処理##./
SJS0090B:
    ?STEP :=   'SJS0090B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKU,TOFILE-TOKUHOE.TOKFLIB
    OVRF      FILE-RUISEKF,TOFILE-RUISEKL1.TOKFLIB
    OVRF      FILE-JISSYUL1,TOFILE-JISSYUL1.TOKFLIB
    CALL      PGM-SJS0090B.TOKELIB
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
