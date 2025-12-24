# IONJYUSN

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/IONJYUSN.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    イオンデータ受信処理    　           *  ./
/. *   JOB-ID      :    IONJYUSN                             *  ./
/. *   JOB-NAME    :    日次振替                             *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'IONJYUSN'
    VAR       ?STEP   ,STRING*8
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?PGNM   ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1  ,STRING*40                  /.      2    ./
    VAR       ?KEKA2  ,STRING*40                  /.      3    ./
    VAR       ?KEKA3  ,STRING*40                  /.      4    ./
    VAR       ?KEKA4  ,STRING*40                  /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'イオン発注データ受信処理'
/.##PG開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ｲｵﾝﾃﾞｰﾀ受信確認##./

    ?OPR1  :=  '　＃＃＃＃＃　イオン発注データ受信　＃＃＃＃＃＃'
    ?OPR2  :=  '　本社よりイオン発注データを受信します。'
    ?OPR3  :=  '　データ退避用のＭＯをセットして下さい。'
    ?OPR4  :=  '　もし、受信エラーの場合は、暫く時間を置き'
    ?OPR5  :=  '　再度、実行して下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##本社ﾃﾞｰﾀ受信##./
PJYUSIN:

    ?STEP :=   'PJYUSIN '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    RCVFILE COM-IFPATH01.IFNODE01,SFILE-IONHACS.ONLBLIB,
            DFILE-'IONHACO',RL-128,BF-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##ﾃﾞｰﾀﾊﾞｯｸｱｯﾌﾟ##./
PBACKUP:

    ?STEP :=   'PBACKUP '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SAVFILE FILE-IONHACS.ONLBLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

RTN:

    ?KEKA1 :=  '【受信処理－正常終了】'
    ?KEKA2 :=  '受信処理が正常終了しました。'
    ?KEKA3 :=  '確認後、発注変換処理を実行して下さい。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '【受信処理－異常終了】'
    ?KEKA2 :=  '受信処理が異常終了しました。'
    ?KEKA3 :=  '本社側で回線使用中の可能性があります。'
    ?KEKA4 :=  '暫く時間をおいてから再度受信して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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

```
