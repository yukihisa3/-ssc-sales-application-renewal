# ACOSPG2A

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/ACOSPG2A.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    在庫システム　　　　　　　           *  ./
/. *   JOB-ID      :    ACOSPG2                             *  ./
/. *   JOB-NAME    :    日次振替                             *  ./
/. *               :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'ACOSPG2'
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

/.##ライブラリリスト登録##./
    DEFLIBL TOKELIB/TOKFLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＡＣＯＳ振替データ受信'

/.######################./
/.##振替データ受信要求##./
/.######################./


/.##振替ｴﾗｰ部門毎振分け##./
SFU0100B:

    ?STEP :=   'SFU0100B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TOKU,TOFILE-TOKFRR.TOKFLIB
    OVRF      FILE-HON,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-FUK,TOFILE-TOKFFU.TOKFLIB
    OVRF      FILE-SEN,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-HOK,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-OSA,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-OKA,TOFILE-TOKFOK.TOKFLIB
    OVRF      FILE-BUTOKMF1,TOFILE-BUTOKML1.TOKFLIB
    OVRF      FILE-BUTOKMF2,TOFILE-BUTOKML2.TOKFLIB
    CALL      PGM-SFU0100B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データ編集／振分　異常'
              GOTO ABEND END

/.##振替ｴﾗｰ部門毎振分ﾘｽﾄ##./
SKY2701L:

    ?STEP :=   'SKY2701L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ編集／振分 ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-HOU,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-FKU,TOFILE-TOKFFU.TOKFLIB
    OVRF      FILE-SEU,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-HKU,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-OSU,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-OKU,TOFILE-TOKFOK.TOKFLIB
    CALL      PGM-SKY2701L.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替ＤＴ　部門別リスト異常'
              GOTO ABEND END

    ?OPR1  :=  '　＃＃＃＃＃＃　バックアップ処理　＃＃＃＃＃＃＃'
    ?OPR2  :=  '　　ＭＯのセットＯＫですか？　　　　　　　　　　'
    ?OPR3  :=  '　　確認して下さい。　　　　　　　　　　　　　　'
    ?OPR4  :=  '　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR5  :=  '　　　　　　　　　　　　　　　　　　　　　　　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    /.##振替ﾃﾞｰﾀ  ##./
    SAVFILE FILE-TOKFRR.TOKFLIB/TOKFHO.TOKFLIB/
            TOKFFU.TOKFLIB/TOKFSE.TOKFLIB/TOKFOK.TOKFLIB/
            TOKFHK.TOKFLIB/TOKFOS.TOKFLIB,TODEV-MO,
            MODE-@USED
    IF      @PGMEC    ^=   0    THEN
            ?KEKA4 := '受信／振分ＤＴ　ＭＯ退避異常'
            GOTO ABEND END

/.##本社振替ﾃﾞｰﾀｺﾋﾟｰ##./
PSETUP:

    ?STEP :=   'PSETUP  '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替データ初期化     ##'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-FURIKAF.TOKFLIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー1    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFHO.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー１（本社）'
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー2    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-FURIERR.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー２（前回エラー分）'
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー3    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFSE.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー３（仙台）'
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー4    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFHK.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー４（北海道）'
              GOTO ABEND END
    ?MSGX :=  '## 振替データコピー5    ##'
    SNDMSG    ?MSGX,TO-XCTL
    OVRF      FILE-FURIKAE,TOFILE-TOKFOS.TOKFLIB
    OVRF      FILE-FURIKAL1,TOFILE-FURIKAL1.TOKFLIB
    CALL      PGM-SFU9999B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 := '振替データコピー５（西日本）'
              GOTO ABEND END

/.##日次振替ﾘｽﾄ発行##./
SFU0110L:

    ?PGMID := 'SFU0110L'
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '## 振替リスト発行       ##'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF FILE-FURIKAL2,TOFILE-FURIKAL2.TOKFLIB
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF FILE-ZSHIMS1,TOFILE-ZSHIMS1.TOKFLIB
    OVRF FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF FILE-BUTOKML1,TOFILE-BUTOKML1.TOKFLIB
    OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL
    CALL PGM-SFU0110L.TOKELIB
    IF   @PGMEC  ^=  0  THEN
         ?KEKA4 := '振替リスト発行'
         GOTO     ABEND
    END


RTN:

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO
    ?KEKA1 :=  'ＡＣＯＳ振替処理が正常終了しました。'
    ?KEKA2 :=  '振替リストを確認して下さい。'
    ?KEKA3 :=  '九州分のデータがある場合は、九州営業課へ'
    ?KEKA4 :=  '連絡して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO
    ?KEKA1 :=  'ＡＣＯＳ振替処理が異常終了しました。'
    ?KEKA2 :=  'この画面をハードコピーしてＮＡＶへ連絡'
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
