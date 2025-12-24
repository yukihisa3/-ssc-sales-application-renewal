# SNDHOMAC

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/SNDHOMAC.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ホーマックオンライン　　　           *  ./
/. *   JOB-ID      :    SNDHOMAC                             *  ./
/. *   JOB-NAME    :   出荷データ送信         　　　　　　  *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'SNDHOMAC'
    VAR       ?STEP     ,STRING*8
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '納品予定データ＝＞ＰＣ送信'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ＧＰ6000_ＰＣ##./
SNDDATA:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'ＧＰ_ＰＣデータ転送中',TO-XCTL

    FIMPORT FILE-SNDHOMAC.TOKKLIB,PARA-HORMAC,UNIT-3
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【ＰＣへ送信中】'
              GOTO ABEND END

/.##データバックアップ1##./
DTBAK1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'バックアップ中です_',TO-XCTL

    CNVFILE FILE-SNDHOMA1.TOKKLIB,TOFILE-SNDHOMA2.TOKKLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【前回分をバックアップ】'
              GOTO ABEND END

/.##データバックアップ2##./
DTBAK2:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'バックアップ中です_',TO-XCTL

    CNVFILE FILE-SNDHOMAC.TOKKLIB,TOFILE-SNDHOMA1.TOKKLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【今回分をバックアップ】'
              GOTO ABEND END

/.##ホスト送信ＤＴ初期化##./
PCLRFILE:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'ホスト側ファイル初期化',TO-XCTL

    CLRFILE SNDHOMAC.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【ホスト送信ＤＴ初期化】'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  'ホーマック納品予定データをＰＣ上に転送'
    ?KEKA2 :=  'しました。ＰＣより送信処理を行なって下'
    ?KEKA3 :=  'さい。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  'ホーマック納品予定データＰＣ送信に失敗'
    ?KEKA2 :=  'しました。ログ採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
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
