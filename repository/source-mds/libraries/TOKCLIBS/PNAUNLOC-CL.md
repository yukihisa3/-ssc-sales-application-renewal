# PNAUNLOC

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNAUNLOC.CL`

## ソースコード

```jcl
/. *********************************************************** ./
/. *     サカタのタネ　特販システム（本社システム）          * ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   * ./
/. *   JOB-ID      :    PNAUNLOC                             * ./
/. *   JOB-NAME    :    排他状態強制解除　　　　　　　　　　 * ./
/. *          ※通常使用しない（非常時に必要に応じて）       * ./
/. *********************************************************** ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS      ,STRING*08,VALUE-'        '      /.ﾜｰｸｽﾃｰｼｮﾝ文字  ./
    VAR ?WKSTN   ,NAME                            /.ﾜｰｸｽﾃｰｼｮﾝ名前  ./
    VAR ?PGMEC   ,INTEGER                         /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ ./
    VAR ?PGMECX  ,STRING*11                       /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ    ./
    VAR ?PGMEM   ,STRING*99                       /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ  ./
    VAR ?MSG     ,STRING*99(6)                    /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX    ,STRING*99                         /.SNDMSG表示用 ./
    VAR ?CLID    ,STRING*08,VALUE-'PNAUNLOC'        /.CLID         ./
    VAR ?STEP    ,STRING*08                         /.STEP-ID      ./
    VAR ?CLNM    ,STRING*40                         /.CL名称       ./
    VAR ?KEKA1   ,STRING*40                         /.ﾒｯｾｰｼﾞ1      ./
    VAR ?KEKA2   ,STRING*40                         /.ﾒｯｾｰｼﾞ2      ./
    VAR ?KEKA3   ,STRING*40                         /.ﾒｯｾｰｼﾞ3      ./
    VAR ?KEKA4   ,STRING*40                         /.ﾒｯｾｰｼﾞ4      ./

    VAR ?BUMON   ,STRING*04,VALUE-'    '            /.部門ＣＤ　 ./
    VAR ?TANCD   ,STRING*02,VALUE-'  '              /.担当者ＣＤ ./
    VAR ?OPR1    ,STRING*50                         /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2    ,STRING*50                         /.      2    ./
    VAR ?OPR3    ,STRING*50                         /.      3    ./
    VAR ?OPR4    ,STRING*50                         /.      4    ./
    VAR ?OPR5    ,STRING*50                         /.      5    ./
   /.##パラメタ（Ｂ）##./
    VAR ?PB01    ,STRING*06,VALUE-'000000'          /.部門・担当者 ./
    VAR ?PB02    ,STRING*01,VALUE-'0'               /.ｵﾝﾗｲﾝ手書種別./
    VAR ?PB03    ,STRING*09,VALUE-'000000000'       /.連携_(FROM) ./
    VAR ?PB04    ,STRING*09,VALUE-'000000000'       /.連携_(TO)   ./
    VAR ?PB05    ,STRING*15,VALUE-'000.000.000.000' /.起動元ＩＰ   ./

/.------------------------------------------------------------------./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := '排他状態強制解除'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/HULOLIB/HULFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.##実行確認##./
    ?OPR1  :=  '　苗業務システムとの連携排他状態を、'
    ?OPR2  :=  '　強制的に解除する処理です。'
    ?OPR3  :=  '　この処理を平常時に行うと、現在連携中である場合'
    ?OPR4  :=  '　データに不整合が発生する可能性があります！'
    ?OPR5  :=  '　本当に続行してよいですか？？？'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##排他状態強制解除##./
CLRFILE1:

    ?STEP :=   'CLRFILE1'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE   FILE-NARLOCF.TOKKLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '排他状態強制解除'
              GOTO ABEND
    END

CLRFILE2:

    ?STEP :=   'CLRFILE2'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE   FILE-NARLOCF2.TOKKLIB
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
              ?KEKA4 :=  '排他状態強制解除'
              GOTO ABEND
    END

/.------------------------------------------------------------------./

/.##正常終了処理##./
RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  ''
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?CLID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC
/.------------------------------------------------------------------./

/.##異常終了処理##./
ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?CLNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?CLID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC

```
