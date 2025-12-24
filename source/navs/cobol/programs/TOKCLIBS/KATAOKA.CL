/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    共通システム              　　　     *  ./
/. *   JOB-ID      :    PKYSPLQI                             *  ./
/. *   JOB-NAME    :    スプールキュー変更        　　　　  *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?INSPL    ,STRING*8,VALUE-'        '    /.IN ﾌﾟﾘﾝﾀ./
    VAR ?SPL      ,STRING*8,VALUE-'        '    /.IN ﾌﾟﾘﾝﾀ./
    VAR ?CHGSPL   ,STRING*8,VALUE-'        '    /.OUTﾌﾟﾘﾝﾀ./
    VAR ?HENSPL   ,NAME                         /.ﾌﾟﾘﾝﾀ変換用./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PKYSPLQI'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?PRT      ,NAME

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.###ﾗｲﾌﾞﾗﾘﾘｽﾄ定義###./
    DEFLIBL TOKELIB/TOKFLIB

/.###選択中ﾌﾟﾘﾝﾀ###./
    ?HENSPL  :=   @OUTQN
    ?SPL     :=   %STRING(?HENSPL)
    ?INSPL   :=   ?SPL

/.###ﾌﾟﾘﾝﾀｷｭｰ変更###./
SKYSPLQI:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##北上プリンタ切り替え用##./
    OVRF      FILE-SMNPRTL1,TOFILE-SMNPRTL1.KATAOKA
    CALL      PGM-SKYSPLQ1.TOKELIBO,PARA-(?INSPL,?CHGSPL)

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    ?MSGX := '##変更前ﾌﾟﾘﾝﾀ名称 = ' && ?INSPL  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    ?MSGX := '##変更後ﾌﾟﾘﾝﾀ名称 = ' && ?CHGSPL && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌﾟﾘﾝﾀｽﾌﾟｰﾙの切換処理(CHGCMVAR)##./
    ?SPL    :=  ?CHGSPL
    ?PRT    :=  %NAME(?SPL)
    CHGCMVAR CMVAR-'@OUTQN',VALUE-?PRT

/.###ﾌﾟﾛｸﾞﾗﾑ終了###./
RTN:

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