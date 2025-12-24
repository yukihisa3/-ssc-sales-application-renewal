# PNJH781J

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH781J.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　ＮＡＶＳ基幹システム　　　　　        *  ./
/. *   SYSTEM-NAME :    変換伝票データ作成（ＪＸ手順）　　   *  ./
/. *   JOB-ID      :    PNJH7815                             *  ./
/. *   JOB-NAME    :    ロイヤルＨＣ　オンラインシステム　　 *  ./
/. ***********************************************************  ./
/.  PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?LINE,P5-?YUSEN,
         P6-?LIBNM,P7-?FILNM,P8-?JKEKA)
./  PGM
/.##ﾊﾟﾗﾒﾀ定義##./
    VAR       ?HIDUKE   ,STRING*8,VALUE-'20230330' /.受信日付./
    VAR       ?JIKAN    ,STRING*4,VALUE-'0800'     /.受信時間./
    VAR       ?TOKCD    ,STRING*8,VALUE-'00051649' /.受信取引先./
    VAR       ?LINE     ,STRING*1,VALUE-'I'        /.回線./
    VAR       ?YUSEN    ,STRING*1,VALUE-'1'        /.回線優先./
    VAR       ?LIBNM    ,STRING*8,VALUE-'ONLBLIB ' /.集信LIB./
    VAR       ?FILNM    ,STRING*8,VALUE-'ROYALJYR' /.集信FILE./
    VAR       ?JKEKA    ,STRING*4,VALUE-'0000'     /.結果./
/.##ﾜｰｸﾃｲｷﾞ##./
    VAR       ?PGMEC    ,INTEGER                   /.ﾘﾀｰﾝｺｰﾄﾞ./
    VAR       ?PGMECX   ,STRING*11                  /.ﾘﾀｰﾝｺｰﾄﾞ変換./
    VAR       ?PGMEM    ,STRING*99                  /.ﾘﾀｰﾝ名称./
    VAR       ?MSG      ,STRING*99(6)               /.ﾒｯｾｰｼﾞ退避ﾜｰｸ./
    VAR       ?MSGX     ,STRING*99                  /.ﾒｯｾｰｼﾞ表示用./
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH7815'  /.PROGRAM-ID./
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
    /.エラーメール発行用ワーク./
    VAR ?ERKBN    ,STRING*03,VALUE-'   '       /.区分./
    VAR ?ERDATE   ,STRING*08,VALUE-'00000000'  /.バッチ日付./
    VAR ?ERTIME   ,STRING*04,VALUE-'0000'      /.バッチ時刻./
    VAR ?ERTKCD   ,STRING*08,VALUE-'00000000'  /.バッチ取引先./
/.##受信ＪＸ手順変更用ﾜｰｸ##./
    VAR ?KENSU1   ,STRING*08,VALUE-'00000000'  /.全件数./
    VAR ?KENSU2   ,STRING*08,VALUE-'00000000'  /.発注件数./
    VAR ?KENSU3   ,STRING*08,VALUE-'00000000'  /.受領件数./
    VAR ?KENSU4   ,STRING*08,VALUE-'00000000'  /.支払件数./
    VAR ?KENSU5   ,STRING*08,VALUE-'00000000'  /.欠品集計件数./
    VAR ?KENSU6   ,STRING*08,VALUE-'00000000'  /.対象外件数./


/.##ﾌﾟﾛｸﾞﾗﾑｶｲｼﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾃﾞｰﾀ変換PGﾍのﾊﾟﾗﾒﾀ作成##./
    ?PARA :=   ?HIDUKE && ?JIKAN && ?LINE && ?YUSEN
    SNDMSG ?PARA,TO-XCTL.@ORGPROF,JLOG-@YES

/.##受信ﾌｧｲﾙノ編集##./
    ?FILN   :=  %NAME(?FILNM)
    ?LIBN   :=  %NAME(?LIBNM)
    ?FILLIB :=  %NCAT(?FILN,?LIBN)
    ?FILID  :=  %STRING(?FILLIB)
    SNDMSG ?FILID,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/ONLBLIB/
            TOKDLIB/TOKDTLIB/TOKKLIB/TOKWLIB/TOKSOLIB


/.##受領データ処理###############################################./

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/ONLBLIB/
            TOKDLIB/TOKDTLIB/TOKKLIB/TOKWLIB/TOKSOLIB

/.##受領データ共通変換##./
SJR0060B:

    ?STEP :=   'SJR0060B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0060B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
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


/.##ﾌﾟﾛｸﾞﾗﾑ正常終了##./
RTN:

    ?JKEKA := ?KEKA
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    RETURN    PGMEC-@PGMEC

/.##異常終了時##./
ABEND:

  /.## エラーメール用データ出力　セット 開始  ##./
    ?ERKBN   :=  '002'
    ?ERDATE  :=  ?HIDUKE
    ?ERTIME  :=  ?JIKAN
    ?ERTKCD  :=  ?TOKCD
    CALL ERR0010B.TOKSOLIB,PARA-(?ERKBN,?ERDATE,?ERTIME,?ERTKCD)
  /.## エラーメール用データ出力　セット 終了  ##./

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

```
