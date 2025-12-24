/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ファイル退避処理                     *  ./
/. *   JOB-ID      :    FILSAV                               *  ./
/. *   JOB-NAME    :                                         *  ./
/. ***********************************************************  ./
    PGM
/.##ワーク定義##./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'FILSAV  '
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
    ?OPR2  :=  '　　新基幹システムのファイルの一括バックアップ　　'
    ?OPR3  :=  '　　を行います。ＭＯをセットして下さい。　　　　　'
    ?OPR4  :=  '　　ＭＯセット後，入力を押して下さい。　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX := '## ﾌｧｲﾙ 退避処理 START        ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF

    ?MSGX := '## ﾌｧｲﾙ 退避処理 HTOKMS   1/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-HTOKMS.TOKFLIB,TODEV-MO,ADD-@NO,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 HTENMS   2/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-HTENMS.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 HSHOTBL  3/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-HSHOTBL.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 HMEIMS   4/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-HMEIMS.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 HJYOKEN  5/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-HJYOKEN.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 HFURIMS  6/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-HFURIMS.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 HTENRMS  7/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-HTENRMS.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JANJYOF  8/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JANJYOF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JANKANF  9/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JANKANF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHMEOSF 10/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHMEOSF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHMERRF 11/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHMERRF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHMJIKF 12/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHMJIKF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHMKAIF 13/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHMKAIF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHMKENF 14/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHMKENF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHMKIHF 15/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHMKIHF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHMRUTF 16/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHMRUTF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHMSJSF 17/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHMSJSF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHMTANF 18/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHMTANF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHMTJSF 19/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHMTJSF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 JHSHENF 20/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-JHSHENF.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

    ?MSGX := '## ﾌｧｲﾙ 退避処理 SETGKFA 21/21##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF
    SAVFILE FILE-SETGKFA.TOKFLIB,TODEV-MO,ADD-@YES,MODE-@USED

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC


