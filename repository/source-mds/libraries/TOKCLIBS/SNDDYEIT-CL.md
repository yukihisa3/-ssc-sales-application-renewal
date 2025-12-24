# SNDDYEIT

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/SNDDYEIT.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　ﾀﾞｲﾕｰｴｲﾄ新ＥＤＩシステム　　          *  ./
/. *   SYSTEM-NAME :    PC送信処理（データバックアップ）     *  ./
/. *   JOB-ID      :    SNDDYEIT                             *  ./
/. *   JOB-NAME    :    PC送信処理（データバックアップ）     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'SNDDYEIT'
    VAR       ?STEP     ,STRING*99
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

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'ダイユーエイト納品予定データ送信処理'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   TOKELIB/TOKFLIB/ONLBLIB

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【ダイユーエイト　納品予定データＰＣ送信】'
    ?OPR2  :=  'ダイユーエイト納品予定データをＰＣ上に転送'
    ?OPR3  :=  'します。正常終了しましたら、Ｗｅｂより送信'
    ?OPR4  :=  'を開始して下さい。　　　　　　　　　　　　'
    ?OPR5  :=  '　　　　　　　　　　　　　　　　　　　　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.  前々回データバックアップ                                     ./
SNDDYDT3:

    ?STEP :=   '前々回データバックアップ'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-SNDDYDT2.ONLBLIB,TOFILE-SNDDYDT3.ONLBLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'バックアップ１'
              GOTO ABEND END

/.  前回データバックアップ                                     ./
SNDDYDT2:

    ?STEP :=  '前回データバックアップ'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-SNDDYDT1.ONLBLIB,TOFILE-SNDDYDT2.ONLBLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'バックアップ２'
              GOTO ABEND END

/.  今回データバックアップ                                     ./
SNDDYDT1:

    ?STEP :=  '今回データバックアップ'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-SNDDYDT.ONLBLIB,TOFILE-SNDDYDT1.ONLBLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'バックアップ３'
              GOTO ABEND END

/.  納品予定データPC送信                                     ./
FIMPORT:

    ?STEP :=  '納品予定データPC送信'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-SNDDYDT.ONLBLIB,PARA-DAIYU8,UNIT-3

    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  'ＰＣへデータ転送'
              GOTO ABEND END

/.  納品予定データPC送信                                     ./
PCLRFILE:

    ?STEP :=  '納品予定データ初期化'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-SNDDYDT.ONLBLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '送信データ初期化'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  'ＰＣに移りダイユーエイトに送付して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
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
