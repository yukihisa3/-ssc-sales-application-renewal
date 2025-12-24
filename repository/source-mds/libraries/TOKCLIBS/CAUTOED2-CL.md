# CAUTOED2

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/CAUTOED2.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    CAUTOEDI                             *  ./
/. *   JOB-NAME    :    自動受信ＪＯＢ投入処理               *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
   PGM (PARA-?KBN)
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
       GOTO  SNJ9010B
   END

/.**起動端末(WKSTN)の確認                                   ./
/.**（コンソール端末以外はエラー）                          ./
WSCHK:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

                         /.##WKSTN名取得##./
/. ?WS  := @ORGWS
   IF  ?WS ^= WKSTN102 THEN
       MENU MERRWS.TOKMLIB
       RETURN
   END     ./

/.**自動起動済みチェック                                    ./
/.**（実行制御マスタにて自動受信実行中か確認）              ./
JIDOCHK:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   OVRF FILE-JSMJIKL1,TOFILE-JSMJIKL1.TOKJLIB
   CALL PGM-SNJ9000B.TOKELIBO
   ?RC := @PGMEC
   IF  ?RC = 4011 THEN
       MENU MERRJIDS.TOKMLIB
       RETURN PGMEC-?RC
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

/.**自動受信処理起動確認                                    ./
/.**（ジョブ起動の場合に再度、自動受信起動チェックを行なう）./
KYOTU:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   OVRF FILE-JSMJIKL1,TOFILE-JSMJIKL1.TOKJLIB
   CALL PGM-SNJ9000B.TOKELIBO
   ?RC := @PGMEC
   IF  ?RC = 4011 THEN
       MENU MERRJIDS.TOKMLIB
       RETURN PGMEC-?RC
   END

/.**実行制御ファイル更新##./
SNJ9010B:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   OVRF FILE-JSMJIKL1,TOFILE-JSMJIKL1.TOKJLIB
   CALL PGM-SNJ9010B.TOKELIBO,PARA-('1')
   ?RC := @PGMEC
   IF  @PGMEC ^=  0  THEN
       MENU MERRJIDS.TOKMLIB
       RETURN PGMEC-?RC
   END

/.**自動受信ジョブ投入                                      ./
/.**（自動受信プログラム＠Ｂにて起動する）                  ./
JOBSTR:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

   SBMJOB JOB-CVCSJIDO,JOBD-NEWONL2.XUCL,JOBK-@B,
          PGM-PNJ05000.TOKCLIBO,TEMP-2048!256,VSIZE-7,
          RSIZE-32,LOG-@YES!2048,PGMEL-@I/@L/@S/@T,
          LIBL-TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB
   ?RC := @PGMEC
   ?PGMES := @PGMES
   SNDMSG ?PGMES,TOWS-@ORGWS
   IF  ?RC ^=  0   THEN
       GOTO        ABEND
   END

END:

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

    RETURN    PGMEC-@PGMEC

```
