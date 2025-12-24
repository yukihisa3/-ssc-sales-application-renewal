# PHOKAI01

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PHOKAI01.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    北海道営業所処理                     *  ./
/. *   JOB-ID      :    PHOKAI01                             *  ./
/. *   JOB-NAME    :    丸日データ変換処理                   *  ./
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

/.##実行端末チェック##./
ASS:

    DEFLIBL TOKFLIB/TOKELIB

    ?OPR1  :=  '　＃＃＃＃　北海道営業所データ変換処理　＃＃＃＃'
    ?OPR2  :=  '　メールにて送られたデータをＫ側に転送します。'
    ?OPR3  :=  '　確認して下さい。'
    ?OPR4  :=  '（メール取込処理、分割処理は終了していますか。）'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##退避処理（売上／仕入ＤＴ）##./
PFEXPORT:

    ?STEP :=   'PFEXPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FEXPORT FILE-MARUNITI.TOKFLIB,MODE-@REP,PARA-HOKKAIDO,UNIT-1
    IF        @PGMEC    ^=   0    THEN
              ?MSGX :=  '## 転送処理　異常終了！！！ ##'
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '## ＮＡＶに連絡して下さい。 ##'
              SNDMSG    ?MSGX,TO-XCTL
              GOTO ABEND
    ELSE
              ?MSGX :=  '## 転送処理　正常終了！！！ ##'
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '## 北海道へ受信連絡！！！　 ##'
              SNDMSG    ?MSGX,TO-XCTL
    END

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
