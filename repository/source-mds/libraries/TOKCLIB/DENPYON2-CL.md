# DENPYON2

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/DENPYON2.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    DENPYON                              *  ./
/. *   JOB-NAME    :    伝票更新ＪＯＢ投入                   * ./
/. *               :    （在庫マスタ新バージョン）           *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TORICD)
/.###ﾊﾟﾗﾒﾀｴﾘｱ定義####./
    PARA ?HIDUKE  ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA ?JIKAN   ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA ?TORICD  ,STRING*8,IN,VALUE-'        ' /.受信取引先./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'DENPYON '    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?HENTOK   ,STRING*5,VALUE-'     '       /.ﾍﾝｶﾝﾄｸｲｻｷCD./
    VAR ?ONLJOB   ,STRING*3,VALUE-'ONL'         /.ｼﾞｮﾌﾞﾒｲｼｮｳ./
    VAR ?JOBNAME  ,STRING*8,VALUE-'        '    /.ｼﾞｮﾌﾞﾒｲｼｮｳ./
    VAR ?JOBID    ,NAME                         /.ｼﾞｮﾌﾞﾒｲｼｮｳ名前型./
/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    DEFLIBL TOKELIB/TOKFLIB
/.###ｼﾞｮﾌﾞ名称取得###./
    ?HENTOK   :=    %SBSTR(?TORICD,4,5)
    ?JOBNAME  :=    ?ONLJOB && ?HENTOK
    ?JOBID    :=    %NAME(?JOBNAME)
    ?MSGX     :=    '## JOB名称 = ' && ?JOBNAME && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
/.##在庫引当／売上更新ジョブ投入##./
SBMJOB:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                           /.##各種ﾌｧｲﾙ置換え##./
    SBMJOB JOB-?JOBID,JOBD-SKTJOBD.XUCL,JOBK-@B,
           PGM-CKY01010.TOKELIB,
           LIBL-TOKELIB/TOKFLIB,PARA-(?HIDUKE,
           ?JIKAN,?TORICD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    DSPLOG CLR-@YES,EDT-@JEF

    RETURN    PGMEC-@PGMEC

```
