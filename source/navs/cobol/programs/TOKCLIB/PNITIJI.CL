/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ＨＧシステム                         *  ./
/. *   JOB-ID      :    CNI10100                             *  ./
/. *   JOB-NAME    :    日次更新監視ジョブ                   *  ./
/. *   _当日情報取得　　　　　　　　　　　　　　　　　　　　*  ./
/. *   _日次更新日付更新　　　　　　　　　　　　　　　　　　*  ./
/. *   _日次実行時間開始（ＡＣＴＭＳＧＱ）　　　　　　　　　*  ./
/. *   _日次実行時間監視　　　　　　　　　　　　　　　　　　*  ./
/. *   _日次開始前メッセージ表示　　　　　　　　　　　　　　*  ./
/. *   _日次更新処理　　　　　　　　　　　　　　　　　　　　*  ./
/. *   _日次更新結果更新　　　　　　　　　　　　　　　　　　*  ./
/. *   _終了処理　　　　　　　　　　　　　　　　　　　　　　*  ./
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
     DEFLIBL  LIBL-TOKELIB/TOKFLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '日次更新処理監視'

    SNDMSG MSG-'##日次更新処理　開始##',TO-XCTL

STEP010: /.##日次更新条件取得##./

    ?STEP :=  'STEP010 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'##日次更新条件取得##',TO-XCTL

    OVRF      FILE-JHMNITL1,TOFILE-JHMNITL1.TOKFLIB
    CALL      PGM-SJH0130B.TOKELIB,PARA-(?PDATE,?PJKBN,?PTIME
                 ,?PHATY,?PNOUH,?PSYUK,?PKEKA,?PZTIM,?PGETU)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.##日次実行ﾁｪｯｸ##./
    IF        ?PJKBN =  ' '  THEN
              SNDMSG '############################',TO-XCTL
              SNDMSG '# 本日の自動での日次更新は #',TO-XCTL
              SNDMSG '# ありません。手動にて実行 #',TO-XCTL
              SNDMSG '# をお願い致します。       #',TO-XCTL
              SNDMSG '############################',TO-XCTL
              RETURN
    END
    IF        ?PJKBN =  '9'  THEN
              SNDMSG '############################',TO-XCTL
              SNDMSG '# 本日の自動での日次更新は #',TO-XCTL
              SNDMSG '# ありません。手動にて実行 #',TO-XCTL
              SNDMSG '# をお願い致します。       #',TO-XCTL
              SNDMSG '############################',TO-XCTL
              RETURN
    END

STEP020: /.##日次更新条件更新##./

    ?STEP :=  'STEP020 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'##日次更新条件取得##',TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SJH0140B.TOKELIB,PARA-(?PHATY,?PNOUH,?PSYUK)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

    SNDMSG MSG-'##日次更新　監視開始##',TO-XCTL

/.##ﾒｯｾｰｼﾞｷｭｰの活性化##./
/.##ﾒｯｾｰｼﾞｷｭｰの時間監視により下記時間で実行される##./
ACT:
    ACTMSGQ   MAXRMSG-200,
              CDR-B01011401!I01010000
                 /B01011415!I01010000
                 /B01011431!I01010000
                 /B01011445!I01010000
                 /B01011501!I01010000
                 /B01011515!I01010000
                 /B01011531!I01010000
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
                 /B01011931!I01010000
                 /B01011945!I01010000
                 /B01012001!I01010000
                 /B01012015!I01010000
                 /B01012031!I01010000
                 /B01012045!I01010000
                 /B01012101!I01010000
                 /B01012115!I01010000
                 /B01012131!I01010000
                 /B01012145!I01010000
                 /B01012201!I01010000
                 /B01012215!I01010000
                 /B01012231!I01010000
                 /B01012245!I01010000
                 /B01012301!I01010000
                 /B01012315!I01010000
                 /B01012331!I01010000
                 /B01012345!I01010000

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
    ?SNDT1    :=    %SBSTR(?PTIME,1,2)
    ?SNDT2    :=    %SBSTR(?PTIME,3,2)
    ?PTIME1   :=    %SBSTR(?PTIME,1,3)

/.  SNDMSG    ?YY,TO-XCTL
    SNDMSG    ?MM,TO-XCTL
    SNDMSG    ?DD,TO-XCTL
    SNDMSG    ?TIME,TO-XCTL
    SNDMSG    ?YOUBI,TO-XCTL  ./
    ?JMSG     := '##監視中 ' &&
                 ?YY && '/' && ?MM && '/' && ?DD &&
                 ' ' &&
                 ?TIME1 && ':' && ?TIME2 && ' ##'
    SNDMSG ?JMSG,TO-XCTL

/.##起動時間判定処理##./
PROC010:

     IF ?TIME3  ^= ?PTIME1  THEN
        IF  ?PZTIM  =  ?TIME1 THEN
             ?JMSG  := '##日次予告:' && ?SNDT1 && ':' &&
                    ?SNDT2 && ' に日次開始予定##'
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
        END
        IF  ?SNDT1  =  ?TIME1 THEN
             ?JMSG  := '##日次予告:' && ?SNDT1 && ':' &&
                    ?SNDT2 && ' に日次開始予定##'
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
              SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
        END
        GOTO  RCV
     ELSE
              SNDMSG MSG-'##日次更新処理　開始##',TO-XCTL
              /.リモートワークステーションの終了./
              IF  ?PGETU  =  '1'  THEN
                  CALL CNI10000.TOKCLIBO
              ELSE
                  CALL CNI10210.TOKCLIBO
              END
              SNDMSG MSG-'##日次更新処理　終了##',TO-XCTL
     END


RTN:

    ?JMSG  := '##日次更新正常　確認##'
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    ?JMSG  := '##通常業務開始　ＯＫ##'
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?JMSG  := '##日次更新異常　担当者確認##'
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    ?JMSG  := '##異常場所を確認必要？？？##'
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
    SNDMSG ?JMSG,TO-XCTL,TOWS-*,LEVEL-@C,SLOG-@YES
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
