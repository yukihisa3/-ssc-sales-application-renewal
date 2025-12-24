# PEOSJIDE

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PEOSJIDE.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受配信サブシステム        　　　     *  ./
/. *   JOB-ID      :    PEOSJIDS                             *  ./
/. *   JOB-NAME    :    自動受信ＪＯＢ停止処理               *  ./
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
/.確認画面用ワーク./
   VAR     ?MSG1   ,STRING*80                  /.開始終了MSG./
   VAR     ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
   VAR     ?OPR2   ,STRING*50                  /.      2    ./
   VAR     ?OPR3   ,STRING*50                  /.      3    ./
   VAR     ?OPR4   ,STRING*50                  /.      4    ./
   VAR     ?OPR5   ,STRING*50                  /.      5    ./

/.**起動端末(WKSTN)の確認                                   ./
/.**（コンソール端末以外はエラー）                          ./
WSCHK:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL

   ?WS  := @ORGWS
/. IF  ?WS ^= WKSTN102 THEN
       MENU MERRWS.TOKMLIB
       RETURN
   END  ./

/.**自動起動済みチェック                                    ./
/.**（実行制御マスタにて自動受信実行中か確認）              ./
JIDOCHK:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL

   OVRF FILE-JHMJIKL1,TOFILE-JHMJIKL1.TOKFLIB
   CALL PGM-SCV0020B.TOKELIB
   ?RC := @PGMEC
   IF  ?RC = 4010 THEN
       MENU MERRJIDE.TOKMLIB
       RETURN PGMEC-?RC
   END

/.**処理実行確認                                            ./
SYORIKKN:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＃＃＃＃＃　自動受信停止　＃＃＃＃＃＃＃＃　'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '　　自動受信処理を停止します。確認して下さい。　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL PGM-OHOM0900.XUCL,PARA-(?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.**自動受信ジョブ停止                                      ./
/.**（ジョブのキャンセルを行なう）                          ./
JOBCAN:

   ?STEP :=   %LAST(LABEL)
   ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
   SNDMSG    ?MSGX,TO-XCTL

   CANJOB CVCSJIDO.SAKATA1
   CANJOB CVCSJIDO.XSYSOPR

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

```
