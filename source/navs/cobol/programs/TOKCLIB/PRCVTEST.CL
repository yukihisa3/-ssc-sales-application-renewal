/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫管理システム                     *  ./
/. *   JOB-ID      :    PRCVTANA                             *  ./
/. *   JOB-NAME    :    _卸ファイル作成                     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PRCVTANA'
    VAR       ?STEP     ,STRING*8
    VAR       ?WKSTN    ,STRING*8
    VAR       ?NWKSTN   ,NAME
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
/.##ＰＧ名称セット##./
    ?PGNM     :=     '_卸確定ＤＴ受信処理'

    ?NWKSTN   :=         @ORGWS
    ?WKSTN    :=         %STRING(?NWKSTN)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＃＃＃＃　_卸Ｆ受信／編集処理　＃＃＃＃＃　'
    ?OPR2  :=  '　電算室より、_卸確定データを受信します。'
    ?OPR3  :=  '　確認して下さい。'
    ?OPR4  :=  '　受信後、確定ＤＴを倉庫別に振分処理を行います。'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    DEFLIBL TOKELIB/TOKFLIB

/.##エラーデータ振分##./
SKY3801B:

    ?STEP :=   'SKY3801B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '_卸確定ＤＴ振分'
    OVRF      FILE-TANADT,TOFILE-ZTANAWK.TOKFLIB
    OVRF      FILE-TOK00,TOFILE-TANAHON.TOKFLIB
    OVRF      FILE-TOK01,TOFILE-TANAFUK.TOKFLIB
    OVRF      FILE-TOK02,TOFILE-TANASEN.TOKFLIB
    OVRF      FILE-TOK03,TOFILE-TANASIR.TOKFLIB
    OVRF      FILE-TOK04,TOFILE-TANAOKA.TOKFLIB
    OVRF      FILE-TOK05,TOFILE-TANAHOK.TOKFLIB
    OVRF      FILE-TOK06,TOFILE-TANANAG.TOKFLIB
    OVRF      FILE-TOK07,TOFILE-TANAKAN.TOKFLIB
    OVRF      FILE-TOK08,TOFILE-TANAYOK.TOKFLIB
    OVRF      FILE-TOK09,TOFILE-TANAOSA.TOKFLIB
    CALL      PGM-SKY3801B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##_卸部門別件数リスト##./
SKY3901L:

    ?STEP :=   'SKY3901L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '_卸確定件数リスト'
    OVRF      FILE-HON,TOFILE-TANAHON.TOKFLIB
    OVRF      FILE-FUK,TOFILE-TANAFUK.TOKFLIB
    OVRF      FILE-OKA,TOFILE-TANAOKA.TOKFLIB
    OVRF      FILE-SEN,TOFILE-TANASEN.TOKFLIB
    OVRF      FILE-HOK,TOFILE-TANAHOK.TOKFLIB
    OVRF      FILE-OSA,TOFILE-TANAOSA.TOKFLIB
    CALL      PGM-SKY3901L.TOKELIB,PARA-('2')
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##確定_卸Ｆ作成##./
ZTANASET:

    ?STEP :=   'ZTANASET'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?KEKA4 := '確定_卸Ｆ作成'
    OVRF      FILE-INFILE,TOFILE-TANAHON.TOKFLIB
    OVRF      FILE-ZTANADT1,TOFILE-ZTANADT1.TOKFLIB
    CALL      PGM-ZTANASET.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
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