# PNJH5751

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PNJH5751.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    受領データ変換処理                   *  ./
/. *   JOB-ID      :    PNJH5751                             *  ./
/. *   JOB-NAME    :    グッデイオンラインシステム      　　 *  ./
/. *   UPDATE      :    2017.10.11 S11270540                 *  ./
/. *   UPDATE      :    2021.11.08 S22470210 返品自動計上    *  ./
/. ***********************************************************  ./
    PGM (P1-?HIDUKE,P2-?JIKAN,P3-?TOKCD,P4-?LINE,P5-?YUSEN,
         P6-?LIBNM,P7-?FILNM,P8-?JKEKA)
/.  PGM ./
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
    VAR       ?PGMID    ,STRING*8,VALUE-'PNJH5751'  /.PROGRAM-ID./
    VAR       ?STEP     ,STRING*8                   /.STEP-ID./
/.##返品自動計上パラメタ##./
    VAR       ?JKBN     ,STRING*1,VALUE-' '         /.実行区分./
    VAR       ?BUMCD    ,STRING*4,VALUE-'2920'      /.部門ＣＤ./
    VAR       ?TANCD    ,STRING*2,VALUE-'98'        /.担当者ＣＤ./
    VAR       ?HTOKCD   ,STRING*8,VALUE-'00000000'  /.取引先ＣＤ./
    VAR       ?NDATE    ,STRING*8,VALUE-'        '  /.入力日./
    VAR       ?UPTIME   ,STRING*6,VALUE-'      '    /.更新日./
    VAR       ?DENCNT   ,STRING*7,VALUE-'0000000'   /.売伝追加件数./
    /. リスト出力用パラメタ ./
    VAR       ?KEIKBN   ,STRING*1,VALUE-'3'         /. 計上区分 ./
    VAR       ?TANST    ,STRING*2,VALUE-'98'        /. 担当者開始 ./
    VAR       ?TANED    ,STRING*2,VALUE-'98'        /. 担当者終了 ./
    VAR       ?DKBN     ,STRING*1,VALUE-'2'         /. 日付区分　 ./
    VAR       ?AFROM    ,STRING*8,VALUE-'00000000'  /. 検収日開始 ./
    VAR       ?ATO      ,STRING*8,VALUE-'00000000'  /. 検収日終了 ./
    VAR       ?NFROM    ,STRING*8,VALUE-'00000000'  /. 入力日開始 ./
    VAR       ?NTO      ,STRING*8,VALUE-'00000000'  /. 入力日終了 ./
    VAR       ?KFROM    ,STRING*8,VALUE-'00000000'  /. 計上日開始 ./
    VAR       ?KTO      ,STRING*8,VALUE-'00000000'  /. 計上日終了 ./
    VAR       ?SFROM    ,STRING*8,VALUE-'00000000'  /. 作成日開始 ./
    VAR       ?STO      ,STRING*8,VALUE-'00000000'  /. 作成日終了 ./
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
    DEFLIBL   TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKJLIB/
              TOKDTLIB/TOKSOLIB/TOKDLIB/TOKWLIB/ONLBLIB

/.##制御ＣＤ削除##./
SSY5705B:

    ?STEP :=   'SSY5705B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-HACGODYN,TOFILE-?FILLIB
    OVRF      FILE-HACGODYW,TOFILE-JYUGODYW.ONLBLIB
    CALL      PGM-SSY5705B.TOKELIBO
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

/.##データ変換##./
NJH5751B:

    ?STEP :=   'NJH5751B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    OVRF      FILE-CVCSG001,TOFILE-JYUGODYW.ONLBLIB
    OVRF      FILE-JSMDAYL1,TOFILE-JSMDAYL1.TOKJLIB
    OVRF      FILE-GDJYURL1,TOFILE-GDJYURL1.TOKKLIB
    CALL      PGM-NJH5751B.TOKELIBO,PARA-(?PARA)
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

/.##受領データ共通変換##./
SJR0120B:

    ?STEP :=   'SJR0120B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    CALL      PGM-SJR0120B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
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

/.  CALL      PGM-SJR0280B.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
./  CALL      PGM-SJR0280C.TOKSOLIB,PARA-(?HIDUKE,?JIKAN,?TOKCD)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.↓ 以下追加 2021.11.08 ./
/.##返品自動計上判定処理（通常）##./
SJR0430B:

    ?STEP :=   'SJR0430B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    ?HTOKCD := ?TOKCD /.1取引先CDであるため一意で処理./

    IF        ?JKBN  =  '1'  THEN
              OVRF  FILE-COMRHEL2,TOFILE-COMRMHL7.TOKDTLIB
    ELSE
              OVRF  FILE-COMRHEL2,TOFILE-COMRHEL2.TOKDTLIB
    END

/.##CALL      PGM-SJR0430B.TOKSOLIB,
              PARA-(?JKBN,?BUMCD,?TANCD,?HIDUKE,?JIKAN,?HTOKCD,
                    ?NDATE)##./  /.計上対象の入力日をOUT./
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##返品自動計上処理（通常）##./
SJR0440B:

    ?STEP :=   'SJR0440B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    IF        ?JKBN = ' '  THEN
              OVRF  FILE-COMRHELA,TOFILE-COMRHELA.TOKDTLIB
    ELSE
              OVRF  FILE-COMRHELA,TOFILE-COMRMHLA.TOKDTLIB
    END
/.##CALL      PGM-SJR0440B.TOKSOLIB,
              PARA-(?BUMCD,?TANCD,?HIDUKE,?JIKAN,?HTOKCD,?NDATE,
                    ?UPTIME,?DENCNT)   計上時刻・件数をOUT##./
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END
    IF        ?DENCNT    =   '0000000'    THEN
              GOTO RTN
    END

/.## 返品自動計上確認データ抽出 ##./
SJR0360A:

    ?STEP :=   'SJR0360A'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    IF  ?JKBN = ' '  THEN
        OVRF FILE-COMRHEL4,TOFILE-COMRHEL4.TOKDTLIB
        OVRF FILE-COMRHEL5,TOFILE-COMRHEL5.TOKDTLIB
        OVRF FILE-COMRHEL8,TOFILE-COMRHEL8.TOKDTLIB
        OVRF FILE-COMRHEL9,TOFILE-COMRHEL9.TOKDTLIB
    ELSE
        OVRF FILE-COMRHEL4,TOFILE-COMRMHL4.TOKDTLIB
        OVRF FILE-COMRHEL5,TOFILE-COMRMHL5.TOKDTLIB
        OVRF FILE-COMRHEL8,TOFILE-COMRMHL8.TOKDTLIB
        OVRF FILE-COMRHEL9,TOFILE-COMRMHL9.TOKDTLIB
    END
    OVRF FILE-COMWHEL1,TOFILE-AUTWHEL1.TOKDTLIB

    ?NFROM  :=  ?NDATE   /.判定処理からの計上対象入力日./
    ?NTO    :=  ?NDATE   /.判定処理からの計上対象入力日./

/.##CALL PGM-SJR0360A.TOKSOLIB,PARA-(?KEIKBN,?HTOKCD,?TANST,?TANED,
                                     ?DKBN,?AFROM,?ATO,?NFROM,?NTO,
                                     ?KFROM,?KTO,?SFROM,?STO,
                                     ?UPTIME)   計上更新日も条件##./
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF        ?PGMEC    ^=   0    THEN
        IF ?PGMEC = 4001  THEN
           GOTO  RTN
        ELSE
           GOTO  ABEND
        END
    END

/.## 返品自動計上確認リスト発行 ##./
SJR0460L:

    ?STEP :=   %LAST(LABEL)      /.##ﾌﾟﾛｸﾞﾗﾑｽﾀｰﾄﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG ?MSGX,TO-XCTL.@ORGPROF,SLOG-@YES,JLOG-@YES

    OVRPRTF FILE-XU04LP,TOFILE-XU04LP.XUCL

/.##CALL PGM-SJR0460L.TOKSOLIB,PARA-(?JKBN,?KEIKBN,?HTOKCD,?TANST,
             ?TANED,?DKBN,?AFROM,?ATO,?NFROM,?NTO,?KFROM,?KTO,
             ?SFROM,?STO)
##./?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    IF  ?PGMEC ^= 0  THEN
        GOTO  ABEND
    ELSE
        GOTO  RTN
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

```
