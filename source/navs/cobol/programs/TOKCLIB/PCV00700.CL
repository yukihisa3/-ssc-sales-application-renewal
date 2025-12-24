/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    PCV00700                             *  ./
/. *   JOB-NAME    :    (1)実行制御マスタ更新                *  ./
/. *               :    (2)自動受信時間監視                  *  ./
/. ***********************************************************  ./
/.  PGM (PARA-?RC)   ./
/.パラメタ（空白＝正常、１＝実行制御異常、２＝自動受信異常）./
/.  PARA ?RC      ,STRING*1,OUT,VALUE-' '          ﾟﾛｸﾞﾗﾑID./
    PGM
/.ワークエリア./
    VAR ?RC       ,STRING*1,VALUE-' '           /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PCV00700'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?CHK      ,STRING*1,VALUE-'1'           /.ﾌﾟﾛｸﾞﾗﾑ引数./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    DEFLIBL TOKELIB/TOKFLIB

/.**実行制御マスタ自動受信起動更新                         ./
SCV0030B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JHMJIKF,TOFILE-JHMJIKL1.TOKFLIB
    CALL      PGM-SCV0030B.TOKELIB,PARA-(?CHK)
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

    OVRF      FILE-JHMTJSF,TOFILE-JHMTJSL1.TOKFLIB
    OVRF      FILE-JHMKAIF,TOFILE-JHMKAIL1.TOKFLIB
    OVRF      FILE-JHMEOSF,TOFILE-JHMEOSL1.TOKFLIB
    CALL      PGM-SCV0070B.TOKELIB
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