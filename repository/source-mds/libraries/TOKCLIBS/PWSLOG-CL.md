# PWSLOG

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PWSLOG.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＩＴ統制　　　　　　　　　           *  ./
/. *   JOB-ID      :    PWSLOG                               *  ./
/. *   JOB-NAME    :    ワークステーションログ出力           *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM       (P1-?KBN)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?KBN     ,STRING*1,IN,VALUE-' ' /.空白、1=出力先変更./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'WSLOG'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?WS     ,STRING*8,VALUE-'        '  /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN  ,NAME!MOD                   /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?PRF    ,STRING*8,VALUE-'        '  /.ﾌﾟﾛﾌｨｰﾙ文字./
    VAR       ?PROFL  ,NAME!MOD                   /.ﾌﾟﾛﾌｨｰﾙ名前./
    VAR       ?SYSDATE,STRING*13
    VAR       ?DATE   ,STRING*6
    VAR       ?SPLNM  ,NAME
    VAR       ?SPL    ,STRING*8
    VAR       ?YY     ,STRING*2
    VAR       ?MM     ,STRING*2
    VAR       ?DD     ,STRING*2
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

    ?MSGX    :=  '## WSLOG 発行指示 ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##ｼｽﾃﾑ日付取得##./
    ?SYSDATE  :=    @SCDATED
    ?DATE     :=    %SBSTR(?SYSDATE,1,6)
    ?YY       :=    %SBSTR(?DATE,5,2)
    ?MM       :=    %SBSTR(?DATE,3,2)
    ?DD       :=    %SBSTR(?DATE,1,2)
    ?DATE     :=    ?YY && ?MM && ?DD
    ?SPL      :=    'WS' && ?DATE
    ?SPLNM    :=    %NAME(?SPL)
    ?MSGX    :=  '## ﾛｸﾞ発行日付    = ' && ?SPL
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
/.##ｽﾌﾟｰﾙﾌｧｲﾙ名変更##./
    CHGCMVAR '@OUTDSN',?SPLNM

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?PROFL   :=  @JOBPROF
    ?PRF     :=  %STRING(?PROFL)
    ?MSGX    :=  '## LOGINﾌﾟﾛﾌｨｰﾙ名 = ' && ?PRF
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名    = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES

/.##処理判定##./
INIT000:

    IF      ?KBN = '1'  THEN
            CHGCMVAR '@OUTQN',XXWSLOG
            GOTO      MAIN000
    END

/.##確認画面##./
INIT010:

    ?OPR1  :=  '　＃＃＃＃　ワークステーションログ出力　＃＃＃＃'
    ?OPR2  :=  '　ワークステーションログの出力を行ないます。'
    ?OPR3  :=  '　プリンタは使用可能でしょうか？'
    ?OPR4  :=  '　ＯＫの場合は、ＥＮＴＥＲを押して下さい。'
    ?OPR5  :=  ''
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ワークステーションログ出力##./
MAIN000:

    ?MSGX :=  '##ワークステーションログ出力##'
    SNDMSG    ?MSGX,TO-XCTL

    DMPWSLOG WKSTN-?WKSTN,FROM-@NEWACT,PROF-?PROFL
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '## ﾜｰｸｽﾃｰｼｮﾝﾛｸﾞ出力異常 ##'
              SNDMSG    ?MSGX,,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
              GOTO ABEND
    END

/.##スプールデフォルト戻し##./
END000:

    ?MSGX :=  '## スプール変更 ##'
    SNDMSG    ?MSGX,TO-XCTL

    CHGCMVAR '@OUTQN',XSYSLSTQ
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '## ｽﾌﾟｰﾙ変更異常 ##'
              SNDMSG    ?MSGX,,TO-XCTL.@ORGPROF,JLOG-@YES,SLOG-@YES
              GOTO ABEND
    END

RTN:

/.##ｽﾌﾟｰﾙﾌｧｲﾙ名変更##./
    CHGCMVAR '@OUTDSN',XSYSLIST
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

/.##ｽﾌﾟｰﾙﾌｧｲﾙ名変更##./
    CHGCMVAR '@OUTDSN',XSYSLIST
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
    CHGCMVAR '@OUTQN',XSYSLSTQ

    RETURN    PGMEC-@PGMEC


```
