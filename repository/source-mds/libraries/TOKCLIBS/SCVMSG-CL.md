# SCVMSG

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/SCVMSG.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　ＥＤＩ受信システム　                  *  ./
/. *   SYSTEM-NAME :    ＳＮＤＭＳＧ発行                     *  ./
/. *   JOB-ID      :    SCVMSG                               *  ./
/. *   JOB-NAME    :    ＳＮＤＭＳＧ発行                     *  ./
/. ***********************************************************  ./
    PGM (P1-?INMSG)

/.##パラメタ定義##./
    PARA ?INMSG    ,STRING*20,IN,VALUE-'                    '
/.##ワーク定義##./
    VAR  ?MSG      ,STRING*99                    /.ﾒｯｾｰｼﾞ標示用./


/.##SNDMSG出力##./

    ?MSG :=  '## MSG:' && ?INMSG  &&  ' ##'
    SNDMSG ?MSG,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES /.画面に出力./

    RETURN    PGMEC-@PGMEC

```
