# PSJH8799

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSJH8799.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   ■サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＤＣＭＪＡＰＡＮ　ＷｅｂＥＤＩ対応   *  ./
/. *   JOB-ID      :    PSJH8700                             *  ./
/. *   JOB-NAME    :    ＤＣＭＪＡＰＡＮ　Ｗｅｂデータ取込   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PSJH8700'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK3   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK4   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK5   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK6   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK7   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK8   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK9   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK10  ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?HIDUKE ,STRING*8,VALUE-'        '  /.バッチ日付./
    VAR       ?JIKAN  ,STRING*4,VALUE-'    '      /.バッチ時間./
/.-----------------------------------------------------------./
    VAR       ?CLID   ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./
    VAR       ?PGCHK1 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK2 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK3 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK4 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK5 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK6 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK7 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK8 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK9 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK10,STRING*1,VALUE-' '         /.      5    ./

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＤＣＭＪＡＰＡＮ　取込変換処理'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##受信件数リスト##./
STEP060:

    ?STEP :=   'STEP060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-HCSZHOK,TOFILE-JCAFILE1.ONLBLIB
    OVRF FILE-HCSZTOH,TOFILE-JCAFILE2.ONLBLIB
    OVRF FILE-HCSZKNT,TOFILE-JCAFILE3.ONLBLIB
    OVRF FILE-KAHMASZ,TOFILE-KMSHAC.ONLBLIB
    OVRF FILE-HCSKHOK,TOFILE-JCAFILE5.ONLBLIB
    OVRF FILE-HCSKTOH,TOFILE-JCAFILE6.ONLBLIB
    OVRF FILE-HCSKKNT,TOFILE-JCAFILE7.ONLBLIB
    OVRF FILE-KAHMASK,TOFILE-KSSHAC.ONLBLIB
    OVRF FILE-DAIK1,TOFILE-DAIKI1.ONLBLIB
    OVRF FILE-DAIK2,TOFILE-DAIKI2.ONLBLIB
    OVRF FILE-ERRORF,TOFILE-JCAFILE8.ONLBLIB
    CALL SJH8702L.TOKELIBO,PARA-(?CHK1,?CHK2,?CHK3,?CHK4,?CHK5
                                ,?CHK6,?CHK7,?CHK8,?CHK9,?CHK10)
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '【受信結果リスト】'
                    GOTO ABEND
    END

/.##プログラム正常終了（資源の開放）##./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
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
