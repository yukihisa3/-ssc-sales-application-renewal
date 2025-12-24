/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ロイヤルＨＣ　支払処理　　           *  ./
/. *   JOB-ID      :    PSI78060                             *  ./
/. *   JOB-NAME    :    支払明細書作成（インボイス対応後）   *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSI78060'
    VAR       ?STEP     ,STRING*8
    VAR       ?KBN      ,STRING*1,VALUE-'1'
    VAR       ?SIMEBI   ,STRING*8,VALUE-'        '

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO

/.##締日指定画面##./
SSI7805I:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CALL      PGM-SSI7805I.TOKELIBO,PARA-(?KBN,?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              IF  @PGMEC = 4010   THEN
                  SNDMSG '##取消終了##',TO-XCTL
                  RETURN
              ELSE
                  GOTO ABEND END
    END

/.##支払明細書##./
SSI7806L:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-ROYNHEL1,TOFILE-ROYNHEL1.TOKKLIB
    OVRF      FILE-ROYSMEL1,TOFILE-ROYSMEL1.TOKKLIB
    CALL      PGM-SSI7806L.TOKSOLIB,PARA-(?SIMEBI)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

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
