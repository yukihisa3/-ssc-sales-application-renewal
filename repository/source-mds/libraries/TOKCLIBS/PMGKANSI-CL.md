# PMGKANSI

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PMGKANSI.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    業務改善　　　                       *  ./
/. *   JOB-ID      :    PMGKANSI                             *  ./
/. *   JOB-NAME    :    出荷状況監視　　　　　               *  ./
/. ***********************************************************  ./
     PGM
     VAR  ?PGMEC ,INTEGER
     VAR  ?PGMECX,STRING*11
     VAR  ?PGMEM ,STRING*99
     VAR  ?MSG   ,STRING*99(6)
     VAR  ?MSGX  ,STRING*99
     VAR  ?PGMID ,STRING*8,VALUE-'PSE00101'
     VAR  ?STEP  ,STRING*8
     VAR  ?SAVEEC,INTEGER
     VAR  ?MSG1,STRING*40
     VAR  ?MSG2,STRING*40
     VAR  ?MSG3,STRING*40
     VAR  ?MSG4,STRING*40
     VAR  ?MSG5,STRING*40
     VAR  ?WSNOI,NAME
     VAR  ?WSNOL,NAME!MOD
     VAR  ?SYSDATE,STRING*13
     VAR  ?DATE   ,STRING*6
     VAR  ?YOUBI  ,STRING*1
     VAR  ?YY     ,STRING*2
     VAR  ?MM     ,STRING*2
     VAR  ?DD     ,STRING*2
     VAR  ?TIME   ,STRING*4
     VAR  ?TIME1  ,STRING*2
     VAR  ?TIME2  ,STRING*2
     VAR  ?TIME3  ,STRING*3
     VAR  ?TIME4  ,STRING*4
     VAR  ?JMSG   ,STRING*50
     VAR  ?PGNM   ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
     VAR  ?KEKA1  ,STRING*40                 /.      2    ./
     VAR  ?KEKA2  ,STRING*40                 /.      3    ./
     VAR  ?KEKA3  ,STRING*40                 /.      4    ./
     VAR  ?KEKA4  ,STRING*40                 /.      5    ./
     VAR  ?PDATE  ,STRING*8,VALUE-'        ' /.ﾊﾟﾗﾒﾀ日付  ./
     VAR  ?PJKBN  ,STRING*1,VALUE-' '        /.ﾊﾟﾗﾒﾀ実行区分./
     VAR  ?PTIME  ,STRING*4,VALUE-'    '     /.ﾊﾟﾗﾒﾀ実行時間./
     VAR  ?PHATY  ,STRING*8,VALUE-'        ' /.ﾊﾟﾗﾒﾀ日次発注日./
     VAR  ?PNOUH  ,STRING*8,VALUE-'        ' /.ﾊﾟﾗﾒﾀ日次納品日./
     VAR  ?PSYUK  ,STRING*8,VALUE-'        ' /.ﾊﾟﾗﾒﾀ日次出荷日./
     VAR  ?PKEKA  ,STRING*1,VALUE-' '        /.ﾊﾟﾗﾒﾀ実行結果./
     VAR  ?PZTIM  ,STRING*2,VALUE-'  '       /.ﾊﾟﾗﾒﾀ前時間./
     VAR  ?PGETU  ,STRING*2,VALUE-'  '       /.ﾊﾟﾗﾒﾀ月次区分./
     VAR  ?SNDT1  ,STRING*2,VALUE-'  '       /.時間./
     VAR  ?SNDT2  ,STRING*2,VALUE-'  '       /.時間./
     VAR  ?PTIME1 ,STRING*3,VALUE-'   '      /.ﾊﾟﾗﾒﾀ実行時間(3ｹﾀ)./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
     DEFLIBL  LIBL-TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '出荷業務監視'

    SNDMSG MSG-'##出荷業務監視　開始##',TO-XCTL

/.##ﾒｯｾｰｼﾞｷｭｰの活性化##./
/.##ﾒｯｾｰｼﾞｷｭｰの時間監視により下記時間で実行される##./
ACT:
    ACTMSGQ   MAXRMSG-200,
              CDR-B01011531!I01010000
                 /B01011545!I01010000
                 /B01011601!I01010000
                 /B01011615!I01010000
                 /B01011631!I01010000
                 /B01011645!I01010000
                 /B01011701!I01010000
                 /B01011715!I01010000
                 /B01011731!I01010000
                 /B01011745!I01010000
                 /B01011801!I01010000
                 /B01011815!I01010000
                 /B01011831!I01010000
                 /B01011845!I01010000
                 /B01011901!I01010000
                 /B01011915!I01010000

/.##ﾒｯｾｰｼﾞｷｭｰ受取待合せ##./
RCV:

    RCVMSG    WAIT-@YES
    SNDMSG    '時間監視中',TO-XCTL
    ?SYSDATE  :=    @SCDATED
    ?DATE     :=    %SBSTR(?SYSDATE,1,6)
    ?YOUBI    :=    %SBSTR(?SYSDATE,13,1)
    ?YY       :=    %SBSTR(?DATE,5,2)
    ?MM       :=    %SBSTR(?DATE,3,2)
    ?DD       :=    %SBSTR(?DATE,1,2)
    ?TIME     :=    %SBSTR(?SYSDATE,7,4)
    ?TIME1    :=    %SBSTR(?TIME,1,2)
    ?TIME2    :=    %SBSTR(?TIME,3,2)
    ?TIME3    :=    %SBSTR(?TIME,1,3)
    ?TIME4    :=    %SBSTR(?TIME,1,4)
    ?SNDT1    :=    %SBSTR(?PTIME,1,2)
    ?SNDT2    :=    %SBSTR(?PTIME,3,2)
    ?PTIME1   :=    %SBSTR(?PTIME,1,3)

    ?JMSG     := '##出荷状況監視中！！ ' &&
                 ?YY && '/' && ?MM && '/' && ?DD &&
                 ' ' &&
                 ?TIME1 && ':' && ?TIME2 && ' ##'
    SNDMSG ?JMSG,TO-XCTL

/.##全端末にメッセージを送信する（＠Ｃ）##./
PROC010:

     SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊'
           ,TOWS-*,LEVEL-@C,SLOG-@YES,JLOG-@YES
     SNDMSG MSG-'＊出荷データ本社送信処理は完了し＊'
           ,TOWS-*,LEVEL-@C,SLOG-@YES,JLOG-@YES
     SNDMSG MSG-'＊ていますか？　確認！！　　　　＊'
           ,TOWS-*,LEVEL-@C,SLOG-@YES,JLOG-@YES
     SNDMSG MSG-'＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊'
           ,TOWS-*,LEVEL-@C,SLOG-@YES,JLOG-@YES
                                  /.##16：00　自動出力##./
    IF   ?TIME4  =   '1601'
          THEN
          SBMJOB JOB-SYUKANSI,JOBD-CVCS.XUCL,JOBK-@B,
                 PGM-PKZ00560.TOKCLIBO,LOG-@YES!1024,
                 PGMEL-@I/@L/@S/@T
    END
                                  /.##17：00　自動出力##./
    IF   ?TIME4  =   '1701'
          THEN
          SBMJOB JOB-SYUKANSI,JOBD-CVCS.XUCL,JOBK-@B,
                 PGM-PKZ00560.TOKCLIBO,LOG-@YES!1024,
                 PGMEL-@I/@L/@S/@T
    END

                                  /.##18：00　自動出力##./
    IF   ?TIME4  =   '1801'
          THEN
          SBMJOB JOB-SYUKANSI,JOBD-CVCS.XUCL,JOBK-@B,
                 PGM-PKZ00560.TOKCLIBO,LOG-@YES!1024,
                 PGMEL-@I/@L/@S/@T
    END

    IF   ?TIME1  ^=   '19'
          THEN
              GOTO RCV
    END

RTN:

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
/.*****  ＴＥＸＴ　ＥＮＤ  ****************************************./

```
