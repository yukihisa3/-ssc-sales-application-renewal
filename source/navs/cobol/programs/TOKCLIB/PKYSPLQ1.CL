/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    共通システム              　　　     *  ./
/. *   JOB-ID      :    PKYSPLQI                             *  ./
/. *   JOB-NAME    :    スプールキュー変更        　　　　  *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
/.###ﾜｰｸｴﾘｱ定義####./
    PGM
    VAR ?INSPL    ,STRING*8,VALUE-'XXXXXXXX'    /.IN ﾌﾟﾘﾝﾀ./
    VAR ?CHGSPL   ,STRING*8,VALUE-'XXXXXXXX'    /.OUTﾌﾟﾘﾝﾀ./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PKYSPLQI'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?PRT      ,NAME*8

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.###ﾗｲﾌﾞﾗﾘﾘｽﾄ定義###./
    DEFLIBL TOKELIB/TOKFLIB

/.###選択中ﾌﾟﾘﾝﾀ###./
    ?INSPL   :=   %STRING(@OUTQN)
    ?CHGSPL  :=   ?INSPL

/.###ﾌﾟﾘﾝﾀｷｭｰ変更###./
SKYSPLQI:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SMNSPLL1,TOFILE-SMNSPLL1.TOKFLIB
    CALL      PGM-SKYSPLQI.TOKELIB,PARA-(?INSPL,?CHGSPL)

    IF        @PGMEC    ^=   0  THEN
              SNDMSG MSG-'**ｴﾗｰ ﾉ ﾀﾒ ﾌﾟﾘﾝﾀ ﾉ ﾍﾝｺｳ ﾃﾞｷﾏｾﾝ**",TO-XCTL
              GOTO ABEND  END

    ?PRT := %NAME(?CHGPRT)
    CHGCMVAR CMVAR-'@OUTQN',VALUE-?PRT
    SNDMSG ?CHGSPL,TO-XCTL

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