# PSIKEIB1

**種別**: JCL  
**ライブラリ**: TOKCLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIB/PSIKEIB1.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    支払受信ＤＴ→ＦＰＤ退避             *  ./
/. *   JOB-ID      :    PSIKEIB1                             *  ./
/. *   JOB-NAME    :                                         *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PSIKEIB1'
    VAR       ?STEP     ,STRING*8
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ケーヨー支払データ送信前処理'

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ケーヨー支払データ退避処理##./
SAVE:

    ?STEP :=  'SAVE    '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '　＃＃＃＃　支払ＤＴ→電算室渡しＤＴ　＃＃＃＃＃　'
    ?OPR2  :=  '　支払ＤＴをＤＩＳＫ上に転送します。　　　　　　　'
    ?OPR3  :=  '　転送完了後、電算室担当者へメールにて送付願います'
    ?OPR4  :=  '　＜処理続行＝入力／実行　ＰＦ９＝処理中止＞　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##テキスト形式退避処理##./
TFIMPORT:

    ?STEP :=  'TFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-SHIFPD.TOKFLIB,PARA-KEIYOS,UNIT-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【テキスト形式】'
              GOTO ABEND END

/.##バイナリ形式退避処理##./
BFIMPORT:

    ?STEP :=  'BFIMPORT'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-SHIFPD.TOKFLIB,PARA-KEIYOS,UNIT-2
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【バイナリ形式】'
              GOTO ABEND END

RTN:

    ?KEKA1 :=  '電算室へ渡すケーヨー支払ＤＴが'
    ?KEKA2 :=  '正常に作成されました。'
    ?KEKA3 :=  'メールにて担当者様へ送付願います。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '請求締処理が異常終了しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-@PGMEC

```
