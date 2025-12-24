/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＣＳＶ連携処理                       *  ./
/. *   JOB-ID      :   POS07710                              *  ./
/. *   JOB-NAME    :   商品名称マスタＣＳＶ出力処理         *  ./
/. ***********************************************************  ./
    PGM

    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'POS00800'
    VAR       ?STEP     ,STRING*8
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?PGNM :=  '（ＣＳＶ）商品名称マスタ出力処理'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

    ?OPR1  :=  '　＃＃＃（ＣＳＶ）商品名称マスタ出力処理　＃＃＃'
    ?OPR2  :=  '　商品名称マスタの全項目をＣＳＶ形式で出力します'
    ?OPR3  :=  '　出力結果は、Ｃ：￥ＭＳＴ＿ＣＳＶ￥内に出力され'
    ?OPR4  :=  '　ますので、出力された内容をＥＸＣＥＬ等で取込み'
    ?OPR5  :=  '　自由に加工して下さい。（ＭＥＩＣＳＶ．ＴＸＴ）'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

SOS0771B: /.##　商品名称マスタ出力　##./

    ?STEP := 'SOS0771B'
    ?MSGX :=  '***   '  && ?STEP  &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF FILE-MEICSV,TOFILE-MEICSV.TOKFLIB
    CALL SOS0771B.TOKELIB
    IF   @PGMEC      ^=  0
          THEN ?KEKA4 :=  '商品名称マスタ出力処理中'
               GOTO     ABEND
    END

PFIMPORT: /.##　抽出結果ＰＣ転送　##./

    ?STEP := 'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP  &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-MEICSV.TOKFLIB,PARA-MST,UNIT-1
    IF   @PGMEC      ^=  0
          THEN ?KEKA4 :=  '抽出結果ＰＣ転送中'
               GOTO     ABEND
    END

RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '出力結果等を確認して下さい。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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
