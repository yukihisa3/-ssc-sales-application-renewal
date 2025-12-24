# PNACLRNO

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNACLRNO.CL`

## ソースコード

```jcl
/. *********************************************************** ./
/. *     サカタのタネ　特販システム（本社システム）          * ./
/. *   SYSTEM-NAME :    ＨＧ基幹システム　　　　　　　　　   * ./
/. *   JOB-ID      :    PNACLRNO                             * ./
/. *   JOB-NAME    :    売上伝票ファイル　　　　　　　　　　 * ./
/. *                    連携ＮＯ強制クリア　　　　　　　　　 * ./
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
    VAR ?CLID    ,STRING*08,VALUE-'PNACLRNO'        /.CLID         ./
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
    VAR ?MSG1    ,STRING*72,VALUE-''            /.連携_指定ﾒｯｾｰｼﾞ ./
    VAR ?MSG2    ,STRING*72,VALUE-''
    VAR ?MSG3    ,STRING*72,VALUE-''
    VAR ?MSG4    ,STRING*72,VALUE-''
    VAR ?MSG5    ,STRING*72,VALUE-''
    VAR ?MSG1A,STRING*38,VALUE-'売上伝票ファイルからクリアする連携Ｎ'
    VAR ?MSG1B,STRING*38,VALUE-'Ｏを指定してください。　　　　　　　'

    VAR ?MSG2A,STRING*38,VALUE-'　　　　　　　　　　　　　　　　　　'
    VAR ?MSG2B,STRING*38,VALUE-'　　　　　　　　　　　　　　　　　　'

    VAR ?MSG3A,STRING*38,VALUE-'連携ＮＯ（開始）のみ有効です。　　　'
    VAR ?MSG3B,STRING*38,VALUE-'　　　　　　　　　　　　　　　　　　'

    VAR ?MSG4A,STRING*38,VALUE-'　　　　　　　　　　　　　　　　　　'
    VAR ?MSG4B,STRING*38,VALUE-'　　　　　　　　　　　　　　　　　　'

    VAR ?MSG5A,STRING*38,VALUE-'　　　　　　　　　　　　　　　　　　'
    VAR ?MSG5B,STRING*38,VALUE-'　　　　　　　　　　　　　　　　　　'

   /.##パラメタ（Ｂ）##./
    VAR ?PB01    ,STRING*06,VALUE-'000000'          /.部門・担当者 ./
    VAR ?PB02    ,STRING*01,VALUE-'0'               /.ｵﾝﾗｲﾝ手書種別./
    VAR ?PB03    ,STRING*09,VALUE-'000000000'       /.連携_(FROM) ./
    VAR ?PB04    ,STRING*09,VALUE-'000000000'       /.連携_(TO)   ./
    VAR ?PB05    ,STRING*15,VALUE-'000.000.000.000' /.起動元ＩＰ   ./


/.------------------------------------------------------------------./

/.##実行PG名称ｾｯﾄ##./
    ?CLNM := '売上伝票ファイル連携ＮＯ強制クリア'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKELIBO/TOKFLIB/TOKKLIB/HULOLIB/HULFLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?CLID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL,SLOG-@YES,JLOG-@YES

/.##実行確認##./
    ?OPR1  :=  '　売上伝票ファイルに更新されている、連携ＮＯを'
    ?OPR2  :=  '　強制的にクリアする処理です。'
    ?OPR3  :=  ''
    ?OPR4  :=  '　この後、クリアする連携ＮＯを指定します。'
    ?OPR5  :=  '　続行しますか？'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

/.##連携_指定（共通）##./
SNA0090I:

    ?STEP :=   'SNA0090I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?MSG1  := %SBSTR(?MSG1A,2,36) && %SBSTR(?MSG1B,2,36)
    ?MSG2  := %SBSTR(?MSG2A,2,36) && %SBSTR(?MSG2B,2,36)
    ?MSG3  := %SBSTR(?MSG3A,2,36) && %SBSTR(?MSG3B,2,36)
    ?MSG4  := %SBSTR(?MSG4A,2,36) && %SBSTR(?MSG4B,2,36)
    ?MSG5  := %SBSTR(?MSG5A,2,36) && %SBSTR(?MSG5B,2,36)

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.TOKELIB,MEDLIB-TOKELIBO
    OVRF      FILE-NARKANL1,TOFILE-NARKANL1.TOKKLIB
    CALL      PGM-SNA0090I.TOKELIBO,
              PARA-(?BUMON,?TANCD,
                    ?MSG1,?MSG2,?MSG3,?MSG4,?MSG5,
                    ?PB01,?PB02,?PB03,?PB04,?PB05)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0   THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN
         ELSE
              ?KEKA4 :=  '連携_指定（共通）'
              GOTO ABEND
         END
    END
    ?MSGX := '# 指定連携_(開始) = ' && ?PB03 && ' #'
    SNDMSG    ?MSGX,TO-XCTL
/.  ?MSGX := '# 指定連携_(終了) = ' && ?PB04 && ' #'
    SNDMSG    ?MSGX,TO-XCTL      ./

/.##連携ＮＯ強制クリア　##./
SNACLRNO:

    ?STEP :=   'SNACLRNO'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-SHTDENLE,TOFILE-SHTDENLE.TOKFLIB
    CALL      PGM-SNACLRNO.TOKELIBO,PARA-(?PB03)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '売上伝票ファイル連携ＮＯ強制クリア'
              GOTO ABEND
    END
    RETURN    PGMEC-@PGMEC

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
