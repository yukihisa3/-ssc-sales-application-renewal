/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    新受配信サブシステム      　　　     *  ./
/. *   JOB-ID      :    PNJ05000                             *  ./
/. *   JOB-NAME    :    (1)実行制御マスタ更新                *  ./
/. *               :    (2)自動受信時間監視                  *  ./
/. ***********************************************************  ./
    PGM
/.ワークエリア./
    VAR ?RC       ,STRING*1,VALUE-' '           /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PNJ05000'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?CHK      ,STRING*1,VALUE-'1'           /.ﾌﾟﾛｸﾞﾗﾑ引数./
    VAR ?CHK0     ,STRING*1,VALUE-'0'           /.ﾌﾟﾛｸﾞﾗﾑ引数./
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ出力##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKJLIB/TOKELIBO/TOKCLIBO

/.**実行制御マスタ自動受信起動更新”ＯＮ”                 ./
SNJ9010A:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JSMJIKL1,TOFILE-JSMJIKL1.TOKJLIB
    CALL      PGM-SNJ9010B.TOKELIBO,PARA-(?CHK)
    IF  @PGMEC    ^=   0    THEN
        ?RC       :=   '1'
        SNDMSG MSG-'************************************',TO-XCTL
        SNDMSG MSG-'*実行制御マスタ更新時にエラーが発生*',TO-XCTL
        SNDMSG MSG-'*自動受信ジョブの投入を停止しました*',TO-XCTL
        SNDMSG MSG-'*システム管理者へ連絡して下さい。　*',TO-XCTL
        SNDMSG MSG-'************************************',TO-XCTL
        GOTO ABEND
    END

/.**実行制御マスタ自動受信起動更新                         ./
SCV0070B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JSMDAYF,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-JSMKAIF,TOFILE-JSMKAIL1.TOKJLIB
    OVRF      FILE-JSMEDIF,TOFILE-JSMEDIL1.TOKJLIB
    CALL      PGM-SNJ0500B.TOKELIBO
    ?PGMEC  :=  @PGMEC
    ?PGMECX :=  %STRING(?PGMEC)
    SNDMSG ?PGMECX,TO-XCTL.@ORGPROF
    IF  @PGMEC    ^=   0    THEN
        ?RC       :=   '2'
        SNDMSG MSG-'************************************',TO-XCTL
        SNDMSG MSG-'*自動受信処理においてエラーが発生！*',TO-XCTL
        SNDMSG MSG-'*自動受信ジョブの投入を停止しました*',TO-XCTL
        SNDMSG MSG-'*システム管理者へ連絡して下さい。　*',TO-XCTL
        SNDMSG MSG-'************************************',TO-XCTL
        GOTO ABEND
    END

/.**実行制御マスタ自動受信起動更新”ＯＦＦ”               ./
SNJ9010B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JSMJIKL1,TOFILE-JSMJIKL1.TOKJLIB
    CALL      PGM-SNJ9010B.TOKELIBO,PARA-(?CHK0)
    IF  @PGMEC    ^=   0    THEN
        ?RC       :=   '1'
        SNDMSG MSG-'************************************',TO-XCTL
        SNDMSG MSG-'*実行制御マスタ更新時にエラーが発生*',TO-XCTL
        SNDMSG MSG-'*実行管理マスタ内の実行制御ＦＬＧを*',TO-XCTL
        SNDMSG MSG-'*解除して下さい。（ＮＡＶ連絡）　　*',TO-XCTL
        SNDMSG MSG-'************************************',TO-XCTL
        GOTO ABEND
    END

RTN:    /.ﾌﾟﾛｸﾞﾗﾑ正常終了時処理./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

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
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC
