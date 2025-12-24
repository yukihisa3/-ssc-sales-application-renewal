# PNSYSND

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PNSYSND.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＤＣＭ仕入先統合　　　　　　　　　　 *  ./
/. *   JOB-ID      :    PNSYSND                              *  ./
/. *   JOB-NAME    :    納品予定データ送信（ＰＣへ送信）　  *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PNSYSND'
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
    ?PGNM :=  '【ＤＣＭ納品予定ＤＴ→ＰＣ送信処理】'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
SHOM0900:

    ?OPR1  :=  '【ＤＣＭ納品予定ＤＴ→ＰＣ送信処理】'
    ?OPR2  :=  '納品予定データ作成にて作成された送信ＤＴを'
    ?OPR3  :=  '送信用のパソコンに送信します。'
    ?OPR4  :=  '準備ＯＫの場合は、ＥＮＴＥＲを押して下さい。'
    ?OPR5  :=  'キャンセルの場合は、Ｆ９を押して下さい。　　'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ＧＰ6000_ＰＣ（資材データ）##./
SNDDATA1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'ＧＰ_ＰＣ資材データ転送中',TO-XCTL

    FIMPORT FILE-SNDDCMSZ.TOKKLIB,PARA-DCMJAPAN,UNIT-3
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【ＰＣへ送信中（資材）】'
              GOTO ABEND END

/.##データバックアップ1##./
DTBAK1:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'資材データバックアップ中です_',TO-XCTL

    CNVFILE FILE-SNDDCMZ1.TOKKLIB,TOFILE-SNDDCMZ2.TOKKLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【前回分をバックアップ（資材）】'
              GOTO ABEND END

/.##データバックアップ2##./
DTBAK2:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'資材データバックアップ中です_',TO-XCTL

    CNVFILE FILE-SNDDCMSZ.TOKKLIB,TOFILE-SNDDCMZ1.TOKKLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【今回分をバックアップ（資材）】'
              GOTO ABEND END

/.##ＧＰ6000_ＰＣ（植物データ）##./
SNDDATA2:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'ＧＰ_ＰＣ植物データ転送中',TO-XCTL

    FIMPORT FILE-SNDDCMSK.TOKKLIB,PARA-DCMJAPAN,UNIT-15
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【ＰＣへ送信中（植物）】'
              GOTO ABEND END

/.##データバックアップ3##./
DTBAK3:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'植物データバックアップ中です_',TO-XCTL

    CNVFILE FILE-SNDDCMK1.TOKKLIB,TOFILE-SNDDCMK2.TOKKLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【前回分をバックアップ（植物）】'
              GOTO ABEND END

/.##データバックアップ4##./
DTBAK4:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'植物データバックアップ中です_',TO-XCTL

    CNVFILE FILE-SNDDCMSK.TOKKLIB,TOFILE-SNDDCMK1.TOKKLIB,
            ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【今回分をバックアップ（植物）】'
              GOTO ABEND END

/.##ホスト送信ＤＴ初期化##./
PCLRFILE:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'ホスト側ファイル初期化',TO-XCTL

    CLRFILE SNDDCMSZ.TOKKLIB
    CLRFILE SNDDCMSK.TOKKLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '【ホスト送信ＤＴ初期化】'
              GOTO ABEND END
RTN:

    ?KEKA1 :=  'ＤＣＭ納品予定ＤＴをパソコン上に転送'
    ?KEKA2 :=  'しました。ＰＣより送信処理を行なって下'
    ?KEKA3 :=  'さい。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  'ＤＣＭ納品予定ＤＴをパソコン上に転送に'
    ?KEKA2 :=  '失敗しました。ログを採取しＮＡＶへ連絡'
    ?KEKA3 :=  'して下さい。'
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
