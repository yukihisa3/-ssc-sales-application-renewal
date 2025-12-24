/. *************************************************************  ./
/. *   サカタのタネ                                            *  ./
/. *   SYS-NAME    :    受領返品　　　　　　                   *  ./
/. *   JOB-ID      :    PJR01400                               *  ./
/. *   JOB-NAME    :    共通変換（ジュンテンドー）　　　　　　　 ./
/. *   CREATE      :    2017/10/11 S11270540                   *  ./
/. *   UPDATE      :                                           *  ./
/. *************************************************************  ./
    PGM

/.##ﾊﾟﾗﾒﾀ定義##./
/.  PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        '   受信日付./
/.  PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '       受信時間./
/.  PARA      ?TOKCD    ,STRING*8,IN,VALUE-'        '   受信取引先./
/.  PARA      ?LINE     ,STRING*1,IN,VALUE-' '          回線./
/.  PARA      ?YUSEN    ,STRING*1,IN,VALUE-' '          回線優先./
/.  PARA      ?LIBNM    ,STRING*8,IN,VALUE-'        '   集信LIB./
/.  PARA      ?FILNM    ,STRING*8,IN,VALUE-'        '   集信FILE./
/.  PARA      ?JKEKA    ,STRING*4,OUT,VALUE-'    '      結果./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?HIDUKE   ,STRING*8,VALUE-'        '  /. 受信日付./
    VAR       ?JIKAN    ,STRING*4,VALUE-'    '      /. 受信時間./
    VAR       ?TOKCD    ,STRING*8,VALUE-'00005116'  /. 受信取引先./
    VAR       ?JKEKA    ,STRING*4,VALUE-'    '      /.  結果./
    VAR       ?PGMEC    ,INTEGER                    /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PJR01400'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
/.##ﾃﾞｰﾀ変換PG用ﾊﾟﾗﾒﾀ##./
    VAR       ?PARA     ,STRING*14,VALUE-'              '
/.##結果FLG用##./
    VAR       ?KEKA     ,STRING*4,VALUE-'    '      /.結果FLGﾊﾟﾗﾒﾀ./
/.##ﾌｧｲﾙ変換ﾜｰｸ##./
    VAR       ?LIBN     ,NAME                       /.ﾗｲﾌﾞﾗﾘ名前型./
    VAR       ?FILN     ,NAME                       /.ﾌｧｲﾙ名前型./
    VAR       ?FILLIB   ,NAME!MOD                   /.ﾌｧｲﾙ拡張用./
    VAR       ?FILID    ,STRING*17                  /.ﾌｧｲﾙ名表示用./

/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/ONLBLIB/
            TOKDTLIB/TOKDLIB/TOKKLIB/TOKWLIB/TOKSOLIB

/.##ＣＳＶデータ転送##./
FEXPORT:

    ?STEP :=   'FEXPORT '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    FEXPORT   FILE-JUNJYRW1.TOKDTLIB,
              TYPE-@FILE,
              MODE-@REP,
              PARA-JUNJYR,
              UNIT-1
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受領ＣＳＶデータ結合##./
SJR0140B:

    ?STEP :=   'SJR0140B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0140B.TOKSOLIB,PARA-(',"                ",')
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##ＣＳＶデータ→多階層へ##./
FIMPORT:

    ?STEP :=   'FIMPORT '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    FIMPORT   FILE-JUNJYRW2.TOKDTLIB,
              TYPE-@FILE,
              PARA-JUNJYR,
              UNIT-2
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##多階層より→ＣＳＶデータ##./
CNVDF:

    ?STEP :=   'CNVDF   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CNVDF     FILE-JUNJYRF.TOKDTLIB,
              /.↓社内テスト用
              PATH-'/NAVPC/JUNJYRW2.CSV', ./
              /.↓現地実機用./
              PATH-'/HGNAS/JUNJYRW2.CSV',
              MODE-@IMP,
              ADD-@NO,
              ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##バッチ_採番処理##./
SJH8399B:

    ?STEP :=   'SJH8399B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJH8399B.TOKELIBO,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受領データ共通変換##./
SJR0141B:

    ?STEP :=   'SJR0141B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0141B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信エラーリスト（受領・返品）##./
SJR0220L:

    ?STEP :=   'SJR0220L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0220L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##受信状況リスト（受領・返品）##./
SJR0210L:

    ?STEP :=   'SJR0210L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0210L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##返品データ抽出##./
SJR0280B:

    ?STEP :=   'SJR0280B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0280B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##変換なしの為、エラー区分の正常を更新##./
SNJ0730B:

    ?STEP :=   'SNJ0730B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKJLIB/TOKKLIB

              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
    ?KEKA := 'K523'
    CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K548'
              CALL PGM-SNJ0730B.TOKELIBO,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?JKEKA := ?KEKA
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

/.##異常終了時##./
ABEND:

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END
    ?JKEKA := ?KEKA

    RETURN    PGMEC-@PGMEC
