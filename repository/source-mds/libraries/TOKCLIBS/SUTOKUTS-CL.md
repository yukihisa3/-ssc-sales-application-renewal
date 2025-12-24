# SUTOKUTS

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/SUTOKUTS.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫システム　　　　　　　           *  ./
/. *   JOB-ID      :    SUTOKUTS                             *  ./
/. *   JOB-NAME    :    日次振替                             *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'SUTOKUTS'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./

    DEFLIBL TOKELIB/TOKFLIB
/.######################./
/.##振替データ受信要求##./
/.######################./


    ?OPR1  :=  '　＃＃＃＃＃＃＃　振替更新処理　＃＃＃＃＃＃＃＃'
    ?OPR2  :=  '　　振替データの復元処理を開始致します。'
    ?OPR3  :=  '　　（電算室より振替データを受信します。）'
    ?OPR4  :=  '　　確認して下さい。（本社）'
    ?OPR5  :=  '　※データバックアップ用ＭＯをセットして下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.####################./
/.##ＡＣＯＳ接続確立##./
/.####################./
    ?MSGX :=  '## ＡＣＯＳ接続確立開始 ##'
    SNDMSG    ?MSGX,TO-XCTL

    CNTFTPC RMTNAME-ACOSN,USER-'NEC23',ACCOUNT-'NEC'
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '## ＡＣＯＳ接続確立異常 ##'
              SNDMSG    ?MSGX,TO-XCTL
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ接続確立成功 ##'
              SNDMSG    ?MSGX,TO-XCTL
    END

/.########################./
/.##ＡＣＯＳ受信待ち合せ##./
/.########################./
    ?MSGX :=  '## ＡＣＯＳ受信待ち合せ ##'
    SNDMSG    ?MSGX,TO-XCTL

    CALL TWAIT30.XUCL

/.####################./
/.##ＡＣＯＳ振替受信##./
/.####################./
    ?MSGX :=  '## ＡＣＯＳ振替受信開始 ##'
    SNDMSG    ?MSGX,TO-XCTL

    RCVFTPC FILE-'SSC.TOKFRR2',TOENT-TOKFRR3.NAVLIB,DTYPE-@BINARY
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '## ＡＣＯＳ振替受信異常 ##'
              SNDMSG    ?MSGX,TO-XCTL
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ振替受信成功 ##'
              SNDMSG    ?MSGX,TO-XCTL
    END


/.########################./
/.##ＡＣＯＳ接続切り離し##./
/.########################./
    ?MSGX :=  '## ＡＣＯＳ接続切断開始 ##'
    SNDMSG    ?MSGX,TO-XCTL

    DCNTFTPC
    IF      @PGMEC  ^=  0
        THEN
              ?MSGX :=  '## ＡＣＯＳ接続切断異常 ##'
              SNDMSG    ?MSGX,TO-XCTL
              GOTO ABEND
        ELSE
              ?MSGX :=  '## ＡＣＯＳ接続切断成功 ##'
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
