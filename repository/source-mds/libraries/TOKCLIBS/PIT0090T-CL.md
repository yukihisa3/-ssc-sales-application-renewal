# PIT0090T

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PIT0090T.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    マスタリスト                         *  ./
/. *   JOB-ID      :    PIT00900                             *  ./
/. *   JOB-NAME    :    商品マスタ保守（ＩＴ統制）           *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PIT00900'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'2920'     /.部門CD./
    VAR       ?TANTO    ,STRING*8,VALUE-'02      ' /.担当者CD./
    VAR       ?UPDT     ,STRING*8,VALUE-'20090319' /.更新日./
    VAR       ?UPTM     ,STRING*6,VALUE-'092153'   /.更新時間./
    VAR       ?UPDTE    ,STRING*8,VALUE-'20090319' /.更新日./
    VAR       ?UPTME    ,STRING*6,VALUE-'113020'   /.更新時間./
    VAR       ?NYUED    ,STRING*8,VALUE-'        ' /.担当者CD./
    VAR       ?PMODE    ,STRING*1,VALUE-' '        /.担当者CD./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?P1       ,STRING*34                 /.帳票出力ﾊﾟﾗﾒﾀ./
    VAR       ?STIME    ,STRING*4,VALUE-'0000'     /.時刻./
    VAR       ?ETIME    ,STRING*4,VALUE-'9999'     /.時刻./

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO/TOKFLIBO

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##商品ﾏｽﾀﾘｽﾄ##./
SIT0100L:

    ?STEP :=   'SIT0100L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    OVRPRTF   FILE-PRTF,TOFILE-PRTF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-MSTLOGF,TOFILE-MSTLOGL3.TOKKLIB
    CALL      PGM-SIT0100L.TOKELIBO,PARA-(?BUMON,?TANTO,?UPDT,?UPTM,
                                          ?UPDTE,?UPTME)
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
