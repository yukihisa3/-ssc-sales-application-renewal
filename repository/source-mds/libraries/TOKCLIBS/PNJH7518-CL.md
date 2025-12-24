# PNJH7518

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH7518.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　ＮＡＶＳ基幹システム　　　　　        *  ./
/. *   SYSTEM-NAME :    変換伝票データ作成（ＪＸ手順）　　   *  ./
/. *   JOB-ID      :    PNJH7815                             *  ./
/. *   JOB-NAME    :    ロイヤルＨＣ　オンラインシステム　　 *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?LINE,P5-?YUSEN,
         P6-?LIBNM,P7-?FILNM,P8-?JKEKA)
/.##ﾊﾟﾗﾒﾀ定義##./
    PARA      ?HIDUKE   ,STRING*8,IN,VALUE-'        ' /.受信日付./
    PARA      ?JIKAN    ,STRING*4,IN,VALUE-'    '     /.受信時間./
    PARA      ?TOKCD    ,STRING*8,IN,VALUE-'        ' /.受信取引先./
    PARA      ?LINE     ,STRING*1,IN,VALUE-' '        /.回線./
    PARA      ?YUSEN    ,STRING*1,IN,VALUE-' '        /.回線優先./
    PARA      ?LIBNM    ,STRING*8,IN,VALUE-'        ' /.集信LIB./
    PARA      ?FILNM    ,STRING*8,IN,VALUE-'        ' /.集信FILE./
    PARA      ?JKEKA    ,STRING*4,OUT,VALUE-'    '     /.結果./
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

/.##受信ＤＴ種別毎振分##./
SSY7810B:

    ?STEP :=   'SSY7810B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES


    OVRF      FILE-ONLNWROY,TOFILE-?FILLIB
    CALL      PGM-SSY7810B.TOKSOLIB,PARA-(?KENSU1,?KENSU2,?KENSU3,
                                    ?KENSU4,?KENSU5,?KENSU6)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K624'
              CALL PGM-SNJ0730B.TOKELIB,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    ELSE
              ?MSGX :=  '## 全件数     = ' && ?KENSU1 && ' ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX :=  '## 発注ＤＴ   = ' && ?KENSU2 && ' ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX :=  '## 受領ＤＴ   = ' && ?KENSU3 && ' ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX :=  '## 支払ＤＴ   = ' && ?KENSU4 && ' ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX :=  '## 欠品修正Ｄ = ' && ?KENSU5 && ' ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
              ?MSGX :=  '## 対象外ＤＴ = ' && ?KENSU6 && ' ##'
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

/.##受信ＤＴ種別毎振分リスト##./
SSY7811L:

    ?STEP :=   'SSY7811L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SSY7811L.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD,
              ?KENSU1,?KENSU2,?KENSU3,?KENSU4,?KENSU5,?KENSU6)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K625'
              CALL PGM-SNJ0730B.TOKELIB,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    END

/.##受領データ実行判定##./
HANTEI01:

    ?STEP :=   'HANTEI01'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF   ?KENSU3 =  '00000000' THEN
         GOTO  HANTEI02
    ELSE
         ?MSGX :=  '## 受領データ有→変換開始 ##'
         SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END


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

/.##発注データ実行判定##./
HANTEI02:

    ?STEP :=   'HANTEI02'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF  ?KENSU2 =  '00000000' THEN
         /.##ABENDｺｰﾄﾞｾｯﾄ##./
         ?KEKA := 'K526'   /.##データ無セット##./
         CALL PGM-SNJ0730B.TOKELIB,
              PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
         GOTO RTN
    ELSE
         ?MSGX :=  '## 発注データ有→変換開始 ##'
         SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

/.##発注データ処理###############################################./
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/ONLBLIB/
            TOKDLIB/TOKDTLIB/TOKKLIB/TOKWLIB/TOKSOLIB

/.##データ変換##./
NJH7801B:

    ?STEP :=   'NJH7801B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-CVCSG001,TOFILE-ONLROYAL.ONLBLIB
    OVRF      FILE-JSMKENL1,TOFILE-JSMKENL1.TOKJLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-JHMRUTL1,TOFILE-JHMRUTL1.TOKFLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-JHSHENL1,TOFILE-JHSHENL1.TOKFLIB
    OVRF      FILE-RHJOHOL1,TOFILE-RHJOHOL1.TOKKLIB
/.  OVRF      FILE-VLD500,TOFILE-LD500.TOKFLIB   ./
    OVRVLDF   FILE-VLD500,TOFILE-LD500.XUCL
    CALL      PGM-NJH7810B.TOKELIBO,PARA-(?PARA)
    IF        @PGMEC    ^=   0    THEN
              /.##ABENDｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K525'
              CALL PGM-SNJ0730B.TOKELIB,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
              GOTO ABEND
    ELSE
              /.##正常終了ｺｰﾄﾞｾｯﾄ##./
              ?KEKA := 'K522'
              CALL PGM-SNJ0730B.TOKELIB,
                   PARA-(?HIDUKE,?JIKAN,?TOKCD,?KEKA)
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
