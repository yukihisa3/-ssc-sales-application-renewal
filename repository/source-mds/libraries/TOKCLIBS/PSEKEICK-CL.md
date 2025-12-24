# PSEKEICK

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PSEKEICK.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ケーヨー請求送信データ確認           *  ./
/. *   JOB-ID      :    PSEKEICK                             *  ./
/. *   JOB-NAME    :    ケーヨー請求送信データ確認リスト発行 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSEKEICK'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGNM     ,STRING*24!MIXED            /.ＰＧ漢字名 ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
    VAR       ?SYORINM1 ,STRING*40
    VAR       ?SYORINM2 ,STRING*50
    VAR       ?SYORINM3 ,STRING*50
    VAR       ?SYORIKN1 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN2 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN3 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN4 ,STRING*7,VALUE-'0000000'
    VAR       ?SYORIKN5 ,STRING*7,VALUE-'0000000'
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNAME   ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNAME :=  'ケーヨー請求送信データ確認リスト'

/.##確認画面#./
STEP00:
    ?JBID    :=   'PSEKEICK'
    ?CLID    :=   'PSEKEICK'
    ?MSG1    := '<<< ' && ?CLID && ' START '
               && %SBSTR(@SDATEY,1,2) && '/'
               && %SBSTR(@SDATEY,3,2) && '/'
               && %SBSTR(@SDATEY,5,2) && ' '
               && %SBSTR(@STIME,1,2)  && ':'
               && %SBSTR(@STIME,3,2)  && ':'
               && %SBSTR(@STIME,5,2)  && ' >>>'
     SNDMSG  MSG-?MSG1,TOWS-@ORGWS,JLOG-@YES

    ?OPR1  :=  '　ケーヨー送信データ内容を確認します。'
    ?OPR2  :=  '　送信対象のデータの一部が出力されますので、'
    ?OPR3  :=  '　検収日等が処理を送信する月の請求データで'
    ?OPR4  :=  '　有るか確認をお願い致します。'
    ?OPR5  :=  '　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

    CHGCMVAR '@OUTQN',XXLWQ

/.##送信データコピー##./
PSETPF:

    SETPF FILE-SEIKEIYO.ONLBLIB,TOFILE-KEIYOSEI.TOKWLIB,
          ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ケーヨー送信ＤＴコピー】'
              GOTO ABEND END

/.##配信ファイル作成##./
KEIYOSEI:

    ?STEP :=   'KEIYOSEI'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    STRDPSD OBJ-KEIYOSEL.TOKELIBO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ケーヨー請求送信データ確認リスト】'
              GOTO ABEND END

RTN:

    CHGCMVAR '@OUTQN',XSYSLSTQ

    ?KEKA1 :=  'ケーヨー殿請求送信確認リストを発行'
    ?KEKA2 :=  'しました。'
    ?KEKA3 :=  'データ内容が、請求月のデータか確認'
    ?KEKA4 :=  'をお願い致します。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNAME,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    CHGCMVAR '@OUTQN',XSYSLSTQ

    ?KEKA1 :=  'ケーヨー殿請求送信確認リスト発行が'
    ?KEKA2 :=  '異常終了しました。'
    ?KEKA3 :=  'ＮＡＶへ連絡して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNAME,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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
