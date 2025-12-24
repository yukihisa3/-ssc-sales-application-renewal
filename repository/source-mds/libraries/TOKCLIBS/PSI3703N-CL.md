# PSI3703N

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSI3703N.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *   _サカタのタネ　ホームガーデン部　　　　　　　　      *  ./
/. *   SYSTEM-NAME :    ナフコマスタメンテ　　　　　　　　   *  ./
/. *   JOB-ID      :    PSI3703N                             *  ./
/. *   JOB-NAME    :    不照合リスト発行 　　　　　　　　   *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC  ,INTEGER
    VAR       ?PGMECX ,STRING*11
    VAR       ?PGMEM  ,STRING*99
    VAR       ?MSG    ,STRING*99(6)
    VAR       ?MSGX   ,STRING*99
    VAR       ?PGMID  ,STRING*8,VALUE-'PSI3703N'
    VAR       ?STEP   ,STRING*8
    VAR       ?WKSTN  ,STRING*8
    VAR       ?JBNM   ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID   ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID   ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?CHK    ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK1   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?CHK2   ,STRING*1,VALUE-'0'         /.ﾊﾟﾗﾒﾀﾁｪｯｸ./
    VAR       ?HIDUKE ,STRING*8,VALUE-'        '  /.バッチ日付./
    VAR       ?JIKAN  ,STRING*4,VALUE-'    '      /.バッチ時間./
/.-----------------------------------------------------------./
    VAR       ?CLID   ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1   ,STRING*80                  /.開始終了MSG./
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
    VAR       ?PGCHK1 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK2 ,STRING*1,VALUE-' '         /.      5    ./
    VAR       ?PGCHK3 ,STRING*1,VALUE-' '         /.      5    ./

    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ナフコ不照合ＣＳＶ'

/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
STEP000:
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##確認画面##./
STEP010:

    ?OPR1  :=  '【ナフコ不照合ＣＳＶ出力】'
    ?OPR2  :=  'ナフコ支払データとサカタ請求を突き合せ'
    ?OPR3  :=  '不照合ＣＳＶを出力します。'
    ?OPR4  :=  ''
    ?OPR5  :=  '不照合内容の確認を行なって下さい。'
    CALL      OHOM0900.XUCL,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
STEP020:
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB

/.##ソート（取引先＋店舗＋伝票番号順）##./
STEP030:

    ?STEP :=   'STEP030 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃　ソート処理　　開始　＃＃',TO-XCTL
    SORT       INFILE-SITGKF90.TOKKLIB,INRL-200,INBF-20,
              OUTFILE-SITGKF90.TOKKLIB,OUTBF-20,
              WORKFILE-NAFUSORT.TOKKLIB,
              KEY-1!8!CA,KEY1-17!5!CA,KEY2-24!9!CA,
              RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ソート処理】'
              GOTO ABEND END
    SNDMSG MSG-'＃＃　ソート処理　　終了　＃＃',TO-XCTL

/.##ナフコ不照合リスト出力##./
STEP040:

    ?STEP :=   'STEP040 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SNDMSG MSG-'＃＃　不照合リスト　開始　＃＃',TO-XCTL
    OVRF FILE-SITGKF90,TOFILE-SITGKF90.TOKKLIB
    OVRF FILE-SETGK904,TOFILE-SETGK904.TOKKLIB
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF FILE-NFNUNMWK,TOFILE-NFNUNMWK.TOKKLIB
    OVRF FILE-NFNUNMW1,TOFILE-NFNUNMW1.TOKKLIB
    CALL SSI3703N.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ナフコ不照合ＣＳＶ】'
              GOTO ABEND END
    SNDMSG MSG-'＃＃　不照合リスト　終了　＃＃',TO-XCTL

/.## ＰＣへ転送 ##./
PFIMPORT:

    ?STEP :=   'PFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-NFNUNMWK.TOKKLIB,PARA-NFNUNMWK,UNIT-2

    IF   @PGMEC    ^=   0    THEN
           ?KEKA4 := '【ＰＣ転送処理】'
         GOTO ABEND
    END

/.## ＰＣへ転送 ##./
PFIMPOR1:

    ?STEP :=   'PFIMPOR1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-NFNUNMW1.TOKKLIB,PARA-NFNUNMWK,UNIT-3

    IF   @PGMEC    ^=   0    THEN
           ?KEKA4 := '【ＰＣ転送処理】'
         GOTO ABEND
    END

/.##プログラム正常終了（資源の開放）##./
RTN:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコ不照合ＣＳＶ発行が正常終了しました　'
    ?KEKA2 :=  '出力したＣＳＶを確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

/.##プログラム異常終了（資源の開放－＞ログリスト出力）##./
ABEND:

    OVRDSPF FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIB
    ?KEKA1 :=  'ナフコ不照合ＣＳＶ出力が異常終了しました'
    ?KEKA2 :=  'ログ採取し，ＮＡＶへ連絡して下さい。'
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
