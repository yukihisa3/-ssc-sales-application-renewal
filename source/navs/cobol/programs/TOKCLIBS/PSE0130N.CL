/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PSE01300                             *  ./
/. *   JOB-NAME    :    配信ファイル作成（ケーヨー）         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSE01300'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGNM     ,STRING*24!MIXED            /.ＰＧ漢字名 ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?SYORINM1 :=  '送信請求データ作成（ケーヨー）'

/.##ケーヨー請求送信データ作成画面起動##./
STEP00:
    ?JBID    :=   'PSE01300'
    ?CLID    :=   'PSE01300'
    ?MSG1    := '<<< ' && ?CLID && ' START '
               && %SBSTR(@SDATEY,1,2) && '/'
               && %SBSTR(@SDATEY,3,2) && '/'
               && %SBSTR(@SDATEY,5,2) && ' '
               && %SBSTR(@STIME,1,2)  && ':'
               && %SBSTR(@STIME,3,2)  && ':'
               && %SBSTR(@STIME,5,2)  && ' >>>'
     SNDMSG  MSG-?MSG1,TOWS-@ORGWS,JLOG-@YES

    ?OPR1  :=  '　ケーヨー殿請求配信データの作成を行います。　　　'
    ?OPR2  :=  '　確認して下さい。　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '　また，作成完了後，配信処理メニューより、ケーヨー'
    ?OPR4  :=  '　請求データの送信を行って下さい。'
    ?OPR5  :=  '　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##配信データ抽出##./
SSE0130B:

    ?STEP :=   'SSE0130B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SEIGKF2,TOFILE-SETGKFA2.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-WORK01,TOFILE-WORK01.TOKFLIB
    CALL      PGM-SSE0130B.TOKELIB,PARA-(?SYORIKN1)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.##配信ファイル作成##./
SSE0140B:

    ?STEP :=   'SSE0140B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-WORK01,TOFILE-WORK01.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-CVCSKHAI,TOFILE-SEIKEIYO.ONLBLIB
    CALL      PGM-SSE0140B.TOKELIB,PARA-(?SYORIKN2)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',TO-XCTL
    SNDMSG MSG-'＃ケーヨー殿請求配信データの作＃',TO-XCTL
    SNDMSG MSG-'＃成処理が完了しました。　　　＃',TO-XCTL
    SNDMSG MSG-'＃請求データをケーヨーに送付し＃',TO-XCTL
    SNDMSG MSG-'＃て下さい。　　　　　　　　　＃',TO-XCTL
    SNDMSG MSG-'＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃',TO-XCTL

RTN:

    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := '送信用請求データ作成が正常終了しました。　　　　'
    ?SYORINM3 := '件数１：送信請求抽出件数、件数２：送信ＤＴ件数　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
                /.１２３４５６７８９０１２３４５６７８９０１２３４'./
    ?SYORINM2 := '送信用請求データ作成が異常終了しました。　　　　'
    ?SYORINM3 := 'ログを採取して、ＮＡＶへ連絡して下さい。　　　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
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