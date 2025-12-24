/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    CCALEEDI                             *  ./
/. *   JOB-NAME    :    自動受信ＪＯＢ投入処理               *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
   PGM (P1-?KBN)
   PARA    ?KBN    ,STRING*1,IN,VALUE-'0'      /.処理区分./
/.ポインター./
   VAR     ?WS     ,NAME                       /.WKSTN./
   VAR     ?PGMES  ,STRING*5
   VAR     ?RC     ,INTEGER                    /.ﾌﾟﾛｸﾞﾗﾑﾘﾀｰﾝｺｰﾄﾞ./
/.PG実行引数（自動受信プログラム投入時のエラーコード）./
   VAR     ?RETCD  ,STRING*1,VALUE-' '         /.CALLﾘﾀｰﾝｺｰﾄﾞ./
/.メッセージワーク./
   VAR     ?PGMECX ,STRING*11                  /.ｴﾗｰｽﾃｲﾀｽ変換./
   VAR     ?PGMEM  ,STRING*99                  /.ｴﾗｰﾒｯｾｰｼﾞ./
   VAR     ?MSG    ,STRING*99(6)               /.ﾒｯｾｰｼﾞ定義./
   VAR     ?MSGX   ,STRING*99                  /.ﾒｯｾｰｼﾞ定義変換./
   VAR     ?PGMID  ,STRING*8,VALUE-'PJH00040'  /.ﾌﾟﾛｸﾞﾗﾑID./
   VAR     ?STEP   ,STRING*8                   /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
/.確認画面用ワーク./
   VAR     ?MSG1   ,STRING*80                  /.開始終了MSG./
   VAR     ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
   VAR     ?OPR2   ,STRING*50                  /.      2    ./
   VAR     ?OPR3   ,STRING*50                  /.      3    ./
   VAR     ?OPR4   ,STRING*50                  /.      4    ./
   VAR     ?OPR5   ,STRING*50                  /.      5    ./

/.**パラメタチェック ”０”ジョブ起動、”１”手動起動       ./
/.**（ジョブからの起動の場合は、確認画面は表示しない）      ./
KBNCHK:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   IF  ?KBN = '0'      THEN
       GOTO  KYOTU
   END

/.**処理実行確認                                            ./
SYORIKKN:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?OPR1  :=  '　＃＃＃＃＃＃＃　自動受信開始　＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '　　自動受信処理を開始します。確認して下さい。　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL PGM-OHOM0900.XUCL,PARA-(?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.**週間スケジュール作成                                    ./
KYOTU:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   OVRF FILE-JSMKIHL1,TOFILE-JSMKIHL1.TOKJLIB
   OVRF FILE-JSMSPTL1,TOFILE-JSMSPTL1.TOKJLIB
   OVRF FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
   CALL PGM-NCV0080B.TOKELIBO,PARA-('0')
   IF  @PGMEC  ^=  0   THEN
       GOTO  ABEND
   END

END:

   ?MSGX :=  '#週間ｽｹｼﾞｭｰﾙ作成　正常終了#'
   SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

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
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
   ?MSGX :=  '#週間ｽｹｼﾞｭｰﾙ作成　異常終了#'
   SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC
