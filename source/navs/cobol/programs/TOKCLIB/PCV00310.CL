/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    PEOSJIDS                             *  ./
/. *   JOB-NAME    :    実行管理ファイルクリア               *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
   PGM
/.ポインター./
   VAR     ?RC     ,INTEGER                    /.ﾌﾟﾛｸﾞﾗﾑﾘﾀｰﾝｺｰﾄﾞ./
   VAR     ?WS     ,NAME                       /.WKSTN./
   VAR     ?PGMES  ,STRING*5
   VAR     ?CHK    ,STRING*1,VALUE-'0'         /.ｼﾞﾄﾞｳﾃｲｼｸﾌﾞﾝ./
/.メッセージワーク./
   VAR     ?PGMECX ,STRING*11                  /.ｴﾗｰｽﾃｲﾀｽ変換./
   VAR     ?PGMEM  ,STRING*99                  /.ｴﾗｰﾒｯｾｰｼﾞ./
   VAR     ?MSG    ,STRING*99(6)               /.ﾒｯｾｰｼﾞ定義./
   VAR     ?MSGX   ,STRING*99                  /.ﾒｯｾｰｼﾞ定義変換./
   VAR     ?PGMID  ,STRING*8,VALUE-'PJH00040'  /.ﾌﾟﾛｸﾞﾗﾑID./
   VAR     ?STEP   ,STRING*8                   /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./

/.**自動受信停止更新                                        ./
/.**（実行制御マスタの自動受信起動区分を停止する）          ./
JIDOEND:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL

   OVRF FILE-JHMJIKL1,TOFILE-JHMJIKL1.TOKFLIB
   CALL PGM-SCV0030B.TOKELIB,PARA-(?CHK)
   ?RC := @PGMEC
   IF  ?RC ^= 0 THEN
       GOTO   ABEND
   END

END:    /.正常終了時処理./
   RETURN
ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

    ?RC       :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?RC)
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