# LIBSAV

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/LIBSAV.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ライブラリ退避処理                   *  ./
/. *   JOB-ID      :    LIBSAV                               *  ./
/. *   JOB-NAME    :                                         *  ./
/. ***********************************************************  ./
    PGM
/.##ワーク定義##./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'LIBSAV  '
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB

SAVE:

    ?STEP :=  'SAVE    '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　新基幹システムのライブラリのバックアップを　　'
    ?OPR3  :=  '　　開始します。ＭＯをセットして下さい。　　　　　'
    ?OPR4  :=  '　　ＭＯセット後，入力を押して下さい。　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.SKTELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.## 自動受信停止 ##./
    ?MSGX := '## CVCSJIDO CANJOB START ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    CANJOB JOB-CVCSJIDO.SAKATA1

/.## 伝票更新停止 ##./
    ?MSGX := '## DENPYO   CANJOB START ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    CANJOB JOB-DENPYO.SAKATA1

    ?MSGX := '## ﾗｲﾌﾞﾗﾘ 退避処理 START ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF

    ?MSGX := '## ﾗｲﾌﾞﾗﾘ 退避中 TOKSLIB ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVLIB LIB-TOKSLIB,TODEV-MO,ADD-@NO,MODE-@USED

    ?MSGX := '## ﾗｲﾌﾞﾗﾘ 退避中 TOKCLIB ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVLIB LIB-TOKCLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾗｲﾌﾞﾗﾘ 退避中 TOKMLIB ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVLIB LIB-TOKMLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾗｲﾌﾞﾗﾘ 退避中 TOKELIB ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVLIB LIB-TOKELIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾗｲﾌﾞﾗﾘ 退避中 TOKFLIB ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVLIB LIB-TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾗｲﾌﾞﾗﾘ 退避中 TOKWLIB ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVLIB LIB-TOKWLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾗｲﾌﾞﾗﾘ 退避中 TOKPLIB ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVLIB LIB-TOKPLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾗｲﾌﾞﾗﾘ 退避中 ONLBLIB ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVLIB LIB-ONLBLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾗｲﾌﾞﾗﾘ 退避処理 END   ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC



```
